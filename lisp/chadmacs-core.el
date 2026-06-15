;;; chadmacs-core.el --- Sane defaults, persistence, optimization -*- lexical-binding: t; -*-
;;
;; Commentary:
;;   Core defaults that should hold before any UI or package is initialised:
;;   coding system, GUI chrome, modern editing knobs, persistence redirects
;;   (everything under ~/.emacs.d/var/), and the small set of optimisation
;;   packages (gcmh, exec-path-from-shell, no-littering).
;;
;;; Code:

;; ---------------------------------------------------------------- Defaults --

;; Initial buffer behaviour
(setq initial-buffer-choice nil)
(setq frame-title-format nil)

;; No GUI dialogs
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq pop-up-windows nil)

;; Minor visual cleanups
(setq indicate-empty-lines nil)
(setq cursor-in-non-selected-windows nil)

;; Fill / editing
(setq fill-column 80)
(setq confirm-nonexistent-file-or-buffer nil)

;; Org
(setq org-return-follows-link t)

;; Mouse in terminal
(unless (display-graphic-p)
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

;; Disable all GUI chrome
(tool-bar-mode -1)
(menu-bar-mode -1)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; y/n instead of yes/no
(setq use-short-answers t)
(setq confirm-kill-emacs 'y-or-n-p)

;; Tabs -> spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Modern editing defaults
(setq mouse-yank-at-point t)
(setq sentence-end-double-space nil)
(setq require-final-newline t)
(setq vc-follow-symlinks t)
(setq recenter-positions '(middle top bottom))

;; Auto-revert buffers when files change on disk
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; Highlight current line
(global-hl-line-mode 1)

;; Remember window layout changes (C-c <left> / C-c <right>)
(winner-mode 1)

;; Temporary buffers
(temp-buffer-resize-mode 1)
(setq temp-buffer-max-height 8)

;; Minimum window height
(setq window-min-height 1)

;; UTF-8 everywhere
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment   'utf-8)

;; Unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse
      uniquify-separator " • "
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

;; Kill terminal buffer when the process exits
(defun my/term-sentinel-kill-buffer (orig-fn proc msg)
  "Kill terminal buffer when the process exits."
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        (funcall orig-fn proc msg)
        (kill-buffer buffer))
    (funcall orig-fn proc msg)))
(advice-add 'term-sentinel :around #'my/term-sentinel-kill-buffer)

;; Kill current buffer (instead of asking first buffer name)
(global-set-key (kbd "C-x k") 'kill-current-buffer)

;; M-n for new frame (M-n is unbound in vanilla emacs)
(defun new-frame ()
  "\\<M-n> for new frame (\\<M-n> is unbound in vanilla Emacs)."
  (interactive)
  (select-frame (make-frame))
  (switch-to-buffer "*scratch*"))
(global-set-key (kbd "M-n") 'new-frame)
(global-set-key (kbd "M-`") 'other-frame)
(global-set-key (kbd "<M-return>") #'toggle-frame-fullscreen)

;; Open recent files
(global-set-key (kbd "C-x C-r") 'consult-recent-file)

;; Smart find-file: project-wide flat recursive list when inside a project,
;; regular find-file otherwise.
(defun my/smart-find-file ()
  "Find file in current Projectile project (flat fuzzy list) or fall back to `find-file'."
  (interactive)
  (if (and (fboundp 'projectile-project-p) (projectile-project-p))
      (call-interactively #'consult-projectile-find-file)
    (call-interactively #'find-file)))
(global-set-key (kbd "C-x C-f") #'my/smart-find-file)

;; Common keybindings
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "M-/") 'completion-at-point)

;; Grab keyboard focus when the first frame is created. Some window managers
;; (especially under tiling Wayland/X compositors) leave the new Emacs frame
;; unfocused after launch.
(add-hook 'window-setup-hook
          (lambda ()
            (when (display-graphic-p)
              (select-frame-set-input-focus (selected-frame)))))

;; Print init time + GC count when Emacs finishes starting. Lets you spot
;; regressions after editing config (typical: ~1.5-3s with cold cache).
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Chadmacs ready in %s with %d GCs"
                     (emacs-init-time) gcs-done)))

;; ------------------------------------------------------------ Persistence --

;; Backups (~)
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backup/" my/var-dir))))

;; Auto-saves (#)
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save/" my/var-dir) t)))
(setq auto-save-list-file-prefix
      (expand-file-name "auto-save/.saves-" my/var-dir))

;; Savehist (minibuffer history)
(setq savehist-file (expand-file-name "history" my/var-dir))
(savehist-mode 1)

;; Recentf (recent files/places)
(setq recentf-save-file (expand-file-name "recentf" my/var-dir))
(recentf-mode 1)

;; Save-place (cursor positions in files)
(setq save-place-file (expand-file-name "places" my/var-dir))
(save-place-mode 1)

;; Bookmarks
(setq bookmark-default-file (expand-file-name "bookmarks" my/var-dir))
(setq bookmark-save-flag 1)

;; Tramp (remote connections)
(setq tramp-persistency-file-name (expand-file-name "tramp" my/var-dir))
(setq tramp-histfile-override (expand-file-name "tramp-history" my/var-dir))

;; Project list
(setq project-list-file (expand-file-name "projects" my/var-dir))

;; undo-fu session
(setq undo-fu-session-directory
      (expand-file-name "undo-fu-session/" my/var-dir))

;; Eshell
(setq eshell-directory-name (expand-file-name "eshell/" my/var-dir))

;; Transient persistence files
(let ((transient-dir (expand-file-name "transient/" my/var-dir)))
  (setq transient-history-file (expand-file-name "history.el" transient-dir)
        transient-levels-file  (expand-file-name "levels.el" transient-dir)
        transient-values-file  (expand-file-name "values.el" transient-dir)))

;; Projectile state
(setq projectile-cache-file
      (expand-file-name "projectile.cache" my/var-dir))
(setq projectile-known-projects-file
      (expand-file-name "projectile-bookmarks.eld" my/var-dir))

;; Yasnippet user-snippet dir (consumed by chadmacs-editing.el)
(defvar my/yas-user-snippet-dir (expand-file-name "snippets" my/var-dir))

;; Catch-all redirects for other state files that some packages write to
;; ~/.emacs.d/ by default. Setting them up front prevents future leaks.
(setq url-configuration-directory (expand-file-name "url/" my/var-dir))
(setq nsm-settings-file           (expand-file-name "network-security.data" my/var-dir))
(setq package-quickstart-file     (expand-file-name "package-quickstart.el" my/var-dir))
(with-eval-after-load 'request
  (setq request-storage-directory (expand-file-name "request/" my/var-dir)))

;; Backup retention policy
(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Make the extensions directory loadable (language-specific files)
(add-to-list 'load-path (expand-file-name "extensions" user-emacs-directory))

(defun create-or-load-custom-file ()
  "Load the custom.el file (gitignored) if it exists or create if it doesn't."
  (unless (file-exists-p custom-file)
    (with-temp-file custom-file
      (insert ";;; custom.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-
;;;
;;; chadmacs --- Your own config files
;;;
;;; Commentary:
;;; Instead of polluting init.el and early-init.el, it's better that you add your stuff here
;;; and let git handle updates from Chadmacs by pulling from git
;;;
;;; Code:
")))
  (load custom-file t t))

;; ----------------------------------------------------------- Optimization --

;; Skip theme-load confirmation prompt
;; (Elpaca's own elpaca-use-package-by-default is set in early-init.el to
;; default every use-package form to ensure-via-Elpaca.)
(setq custom-safe-themes t)

;; Adaptive GC - defers garbage collection to idle, smoother typing
(use-package gcmh
  :ensure t
  :demand t
  :config
  (gcmh-mode 1))

;; Inherit shell PATH so LSPs/formatters launched from GUI Emacs work
;; (Mostly useful on macOS - Linux users typically get PATH from shell rc.)
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :defer 1
  :config
  (exec-path-from-shell-initialize))

;; Standardize package state paths under var/ (used by future packages)
(use-package no-littering
  :ensure t
  :demand t
  :init
  (setq no-littering-etc-directory (expand-file-name "etc/" my/var-dir)
        no-littering-var-directory my/var-dir))

(provide 'chadmacs-core)
;;; chadmacs-core.el ends here
