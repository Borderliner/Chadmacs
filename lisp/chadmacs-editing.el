;;; chadmacs-editing.el --- Editing enhancements -*- lexical-binding: t; -*-
;;
;; Commentary:
;;   Undo, sessions, snippets, paren handling, jumping, multi-cursor, line
;;   movement, editable grep results, and similar interactive-editing tools.
;;
;;; Code:

;; -------------------------------------------------------------- Undo --

(use-package undo-fu
  :ensure t)

(use-package undo-fu-session
  :ensure t
  :after undo-fu
  :config
  (undo-fu-session-global-mode))

;; Visual undo tree on top of undo-fu
(use-package vundo
  :ensure t
  :commands vundo
  :bind ("C-x u" . vundo))

;; ------------------------------------------------------------ Session --

(use-package easysession
  :ensure t
  :demand t
  :custom
  (easysession-directory (expand-file-name "easysession/" my/var-dir))
  (easysession-save-interval (* 10 60))
  (easysession-switch-to-save-session t)
  (easysession-switch-to-exclude-current nil)
  (easysession-save-mode-lighter-show-session-name t)
  :config
  ;; Session keybindings under `C-c s' as a real prefix-keymap. Using a
  ;; named map (with (description . command) values) makes which-key
  ;; show "load / save / rename / ..." instead of bare command names.
  (defvar my/easysession-map (make-sparse-keymap)
    "Prefix-keymap for easysession commands under `C-c s'.")
  (define-key my/easysession-map (kbd "l") '("load"       . easysession-switch-to))
  (define-key my/easysession-map (kbd "L") '("load+geom"  . easysession-switch-to-and-restore-geometry))
  (define-key my/easysession-map (kbd "s") '("save"       . easysession-save))
  (define-key my/easysession-map (kbd "r") '("rename"     . easysession-rename))
  (define-key my/easysession-map (kbd "R") '("reset"      . easysession-reset))
  (define-key my/easysession-map (kbd "u") '("unload"     . easysession-unload))
  (define-key my/easysession-map (kbd "d") '("delete"     . easysession-delete))
  (global-set-key (kbd "C-c s") my/easysession-map)

  ;; Auto-load last session on startup...
  (setq easysession-setup-load-session t)

  ;; ...but only for a "bare" `emacs' launch. When Emacs is started with a
  ;; file argument (e.g. `emacs -nw foo.txt'), restoring the previous
  ;; session's saved window layout buries that file in a tiny one-line split
  ;; under *scratch* / the last session buffer. Skip the restore in that case
  ;; so the file you asked for owns the frame.
  ;;
  ;; Detection must use `command-line-args', NOT the buffer list: under Elpaca,
  ;; easysession restores on `elpaca-after-init-hook', which fires *before*
  ;; `command-line-1' visits the file, so at predicate time no file-visiting
  ;; buffer exists yet. `command-line-args' still holds the filename here
  ;; (`-nw' and friends are already stripped by C), so a non-option argument
  ;; means "a file was passed -> don't restore". Bare `emacs' has no such
  ;; argument and still restores the full session.
  (setq easysession-setup-load-predicate
        (lambda ()
          (not (seq-some (lambda (arg) (not (string-prefix-p "-" arg)))
                         (cdr command-line-args)))))

  (with-eval-after-load 'easysession
    (require 'easysession-scratch)
    (easysession-scratch-mode 1))
  (with-eval-after-load 'easysession
    (require 'easysession-magit)
    (easysession-magit-mode 1))
  (easysession-setup)

  ;; When easysession restores a real session, suppress dashboard so the
  ;; restored buffers stay visible instead of being shadowed by the splash.
  (add-hook 'easysession-after-load-hook
            (lambda ()
              (when-let ((db (get-buffer "*dashboard*")))
                (kill-buffer db)))))

;; ------------------------------------------------------------ Snippets --

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(use-package yasnippet
  :ensure t
  :commands (yas-minor-mode yas-global-mode)
  :hook (after-init . yas-global-mode)
  :custom
  (yas-also-auto-indent-first-line t)
  (yas-also-indent-empty-lines t)
  (yas-snippet-revival nil)
  (yas-wrap-around-region nil)
  :init
  (setq yas-verbosity 0)
  :config
  ;; Prepend user dir so user snippets take precedence over yasnippet-snippets
  (add-to-list 'yas-snippet-dirs my/yas-user-snippet-dir))

;; -------------------------------------------------- Whitespace & indent --

;; Strip trailing whitespace + final blank lines on save
(use-package stripspace
  :ensure t
  :commands stripspace-local-mode
  :hook
  (prog-mode . stripspace-local-mode)
  (text-mode . stripspace-local-mode)
  (conf-mode . stripspace-local-mode)
  :custom
  (stripspace-only-if-initially-clean nil)
  (stripspace-restore-column t))

;; Auto-indent code as you type (lisp only - aggressive elsewhere)
(use-package aggressive-indent
  :ensure t
  :commands aggressive-indent-mode
  :hook (lisp-data-mode . aggressive-indent-mode))

;; Structural editing.
;;
;; smartparens replaces paredit (paredit is unmaintained, ships obsolete
;; API). smartparens-strict-mode in lisp matches paredit's "never break
;; balance" guarantee; plain smartparens-mode elsewhere gives auto-pair
;; for braces, brackets, quotes, etc. across all prog modes.
;;
;; `sp-use-paredit-bindings' installs paredit's keymap onto smartparens
;; so muscle memory carries over (C-) / C-( for slurp, C-} / C-{ for
;; barf, M-( wrap, M-s splice, M-r raise, C-k smart kill, M-S split,
;; M-J join, M-? convolute, etc).
(use-package smartparens
  :ensure t
  :hook
  (prog-mode      . smartparens-mode)
  (lisp-data-mode . smartparens-strict-mode)
  :config
  ;; Sensible default pair definitions for HTML / Markdown / Lisp / C / etc.
  (require 'smartparens-config)
  (sp-use-paredit-bindings)
  ;; Show matching pair in echo area instead of blinking the cursor
  (setq sp-show-pair-from-inside t)
  ;; Don't pair single quotes inside Lisp (apostrophes break it)
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'lisp-data-mode  "'" nil :actions nil)
  (sp-local-pair 'lisp-mode       "'" nil :actions nil)
  (sp-local-pair 'scheme-mode     "'" nil :actions nil)
  (sp-local-pair 'clojure-mode    "'" nil :actions nil))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; ------------------------------------------------- Selection & jumping --

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))

(use-package avy
  :ensure t
  :bind (("M-g j" . avy-goto-char-timer)
         ("M-g w" . avy-goto-word-1)
         ("M-g l" . avy-goto-line)))

(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window))
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-scope 'frame))

;; ----------------------------------------------- grep / line / cursors --

;; Editable grep results buffer
(use-package wgrep
  :ensure t
  :commands (wgrep-change-to-wgrep-mode)
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

;; Move line/region with M-up / M-down
(use-package move-text
  :ensure t
  :bind (("M-<up>"   . move-text-up)
         ("M-<down>" . move-text-down)))

;; QoL: smart C-a, duplicate-line, open-line, kill-line-backwards, file ops
(use-package crux
  :ensure t
  :bind (("C-a"           . crux-move-beginning-of-line)
         ("C-c d"         . crux-duplicate-current-line-or-region)
         ("C-c D"         . crux-delete-file-and-buffer)
         ("C-c r"         . crux-rename-file-and-buffer)
         ("C-c o"         . crux-smart-open-line)
         ("C-c O"         . crux-smart-open-line-above)
         ("C-S-k"         . crux-kill-whole-line)
         ("C-<backspace>" . crux-kill-line-backwards)))

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)))

(provide 'chadmacs-editing)
;;; chadmacs-editing.el ends here
