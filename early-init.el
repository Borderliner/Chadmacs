;;; early-init.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-
;;;
;;; chadmacs --- Chadmacs Emacs config files
;;;
;;; Commentary:
;;; Borderliner's Chadmacs Emacs config files, with clean visuals and fast startups
;;;
;;; Code:

;;;;;;;;;; Core ;;;;;;;;;;;;
;; Enable debugging for better error messages. Will disable when Chadmacs is stable enough
;; (setq debug-on-error t)

;; Disable Emacs's built-in package manager, package.el
(setq package-enable-at-startup nil)

;; Disable file-name handlers during startup
(defvar my/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist my/file-name-handler-alist)))
;; Increase GC threshold. Decrease if you don't have sufficient RAM.I have 32GiB so 300MiB is nothing.
(setq gc-cons-threshold (* 300 1024 1024)) ;; 300MiB

;;;;;;;;; UI ;;;;;;;;;;;;;;;

;; Disable GUI chrome *before* the initial frame is created
;; (no flash, works reliably even with themes/packages)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; Also set frame parameters (extra safety for the very first frame)
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))

;; Prevent any resizing flash during font/theme loading
(setq frame-inhibit-implied-resize t)

;; Startup messages (safe to keep here)
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil
      inhibit-compacting-font-caches t)

;;;;;;;;;; Paths ;;;;;;;;;;;;

;; Base dirs
(setq user-emacs-directory (expand-file-name "~/.emacs.d/"))
(defconst my/var-dir (expand-file-name "var/" user-emacs-directory))

;; Make sure it exists
(make-directory my/var-dir t)

;; ELPA's user dir
(setq package-user-dir (expand-file-name "elpa" my/var-dir))
(setq package-check-signature nil
      package-archives nil)
(with-eval-after-load 'package
  (setq package-gnupg-directory (locate-user-emacs-file "var/elpa/gnupg")))

;; Native compilation cache (Emacs 28+)
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache
   (expand-file-name "eln/" my/var-dir)))

;; Custom file (DO NOT pollute init)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;;;;;;;;;; Elpaca ;;;;;;;;;;;;

;; Elpaca version
(defvar elpaca-installer-version 0.11)

;; 🔽 Move ALL elpaca stuff under var/
(defvar elpaca-directory (expand-file-name "elpaca/" my/var-dir))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))

;; Elpaca installation, don't mess with it unless you're absolutely sure what you're doing
(defvar elpaca-order
  '(elpaca
       :repo "https://github.com/progfolio/elpaca.git"
       :ref nil
       :depth 1
       :inherit ignore
       :files (:defaults "elpaca-test.el" (:exclude "extensions"))
       :build (:not elpaca--activate-package)))

(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28)
      (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop
                    (apply #'call-process
                           `("git" nil ,buffer t "clone"
                             ,@(when-let* ((depth (plist-get order :depth)))
                                 (list (format "--depth=%d" depth)
                                       "--no-single-branch"))
                             ,(plist-get order :repo)
                             ,repo))))
                  ((zerop
                    (call-process "git" nil buffer t "checkout"
                                  (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop
                    (call-process emacs nil buffer nil
                                  "-Q" "-L" "." "--batch"
                                  "--eval"
                                  "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn
              (message "%s" (buffer-string))
              (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      (error
       (warn "%s" err)
       (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil))
      (load "./elpaca-autoloads"))))

(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; use-package support
(elpaca elpaca-use-package
  (elpaca-use-package-mode))

(provide 'early-init)
;;; early-init.el ends here
