;;; chadmacs-editing.el --- Editing enhancements -*- lexical-binding: t; -*-
;;
;; Commentary:
;;   Undo, sessions, snippets, paren handling, jumping, multi-cursor, line
;;   movement, editable grep results, and similar interactive-editing tools.
;;
;;; Code:

;; -------------------------------------------------------------- Undo --

;; Undo / redo are stock Emacs (C-/ undo, C-? / C-M-_ undo-redo).
;; undo-fu-session persists each file's undo history across restarts.
(use-package undo-fu-session
  :ensure t
  :config
  (undo-fu-session-global-mode))

;; Visual undo tree
(use-package vundo
  :ensure t
  :commands vundo
  :bind ("C-x u" . vundo))

;; ------------------------------------------------------------ Session --

;; Frames easysession may restore the saved layout into. Under a daemon the
;; invisible initial frame is one of the candidates, and for text-terminal
;; frames frameset picks it first: the session went into that hidden frame
;; and the new `emacsclient -t' frame was deleted as unused, so the client
;; exited the moment it connected. Never reuse the daemon's initial frame.
(defun my/easysession-reusable-frame-p (frame)
  "Return non-nil if easysession may restore a saved frame into FRAME."
  (not (equal (terminal-name (frame-terminal frame)) "initial_terminal")))

(use-package easysession
  :ensure t
  :demand t
  :custom
  (easysession-frameset-restore-reuse-frames #'my/easysession-reusable-frame-p)
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
  ;; buffer exists yet. `my/command-line-files-p' (chadmacs-core) parses the
  ;; arguments, so `emacs --eval ...' or `emacs -f ...' still restore the
  ;; session; only an actual file argument skips it.
  (setq easysession-setup-load-predicate
        (lambda () (not (my/command-line-files-p))))

  (with-eval-after-load 'easysession
    (require 'easysession-scratch)
    (easysession-scratch-mode 1))
  (with-eval-after-load 'easysession
    (require 'easysession-magit)
    (easysession-magit-mode 1))
  (easysession-setup)

  ;; Outside a daemon, easysession-setup restores the session from
  ;; `elpaca-after-init-hook', which fires inside `after-init-hook' - before
  ;; Emacs has set up a text terminal or applied the frame parameters - and
  ;; the saved window layout was lost there (every `emacs -nw' came back as
  ;; a single window). Restore from `window-setup-hook' instead, the last
  ;; startup hook, once the frame is fully initialized. The daemon path
  ;; (server-after-make-frame-hook) is left as is.
  (unless (daemonp)
    (remove-hook 'elpaca-after-init-hook #'easysession--setup-load-session)
    (add-hook 'window-setup-hook #'easysession--setup-load-session))

  ;; When easysession restores a real session, suppress dashboard so the
  ;; restored buffers stay visible instead of being shadowed by the splash.
  (add-hook 'easysession-after-load-hook
            (lambda ()
              (when-let* ((db (get-buffer "*dashboard*")))
                (kill-buffer db)))))

;; ------------------------------------------------------------ Snippets --

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(use-package yasnippet
  :ensure t
  :commands (yas-minor-mode yas-global-mode)
  ;; `elpaca-after-init', not `after-init': Elpaca evaluates this form after
  ;; `after-init-hook' has already run, so that hook would never fire.
  :hook (elpaca-after-init . yas-global-mode)
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
;; Paredit's keys (C-) / C-( slurp, C-} / C-{ barf, C-<right> / C-<left>
;; slurp / barf, M-( wrap, M-r raise, M-S split, M-j join, ...) carry
;; over for muscle memory, but only in Lisp buffers: they go into
;; `smartparens-strict-mode-map', which is active exactly where strict
;; mode is. `sp-use-paredit-bindings' would put them in
;; `smartparens-mode-map' and so hijack every prog-mode buffer (C-<left>
;; / C-<right> stop moving by word, M-s search prefix gone, ...).
;;
;; Even in Lisp, the paredit keys that shadow keys Chadmacs documents
;; globally are left out: M-s (the search prefix - M-s l consult-line,
;; M-s r ripgrep, ...), M-? (xref-find-references) and M-<up> / M-<down>
;; (move-text). Their commands stay available through M-x.
(defconst my/sp-paredit-skipped-keys '("M-s" "M-?" "M-<up>" "M-<down>")
  "Keys from `sp-paredit-bindings' that are not bound in Lisp buffers.")

(use-package smartparens
  :ensure t
  :hook
  (prog-mode      . smartparens-mode)
  (lisp-data-mode . smartparens-strict-mode)
  :config
  ;; Sensible default pair definitions for HTML / Markdown / Lisp / C / etc.
  (require 'smartparens-config)
  (pcase-dolist (`(,key . ,command) sp-paredit-bindings)
    (unless (member key my/sp-paredit-skipped-keys)
      (define-key smartparens-strict-mode-map (kbd key) command)))
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

;; Contract with `-' right after C-= (expand-region's repeat keys: `=' grows,
;; `-' shrinks, `0' resets) or with C-- C-=. `C--' itself stays
;; `negative-argument', as in stock Emacs.
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

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

;; QoL: smart C-a, duplicate-line, open-line, file ops. C-<backspace> stays
;; stock `backward-kill-word'; kill to line start with C-0 C-k.
(use-package crux
  :ensure t
  :bind (("C-a"           . crux-move-beginning-of-line)
         ("C-c d"         . crux-duplicate-current-line-or-region)
         ("C-c D"         . crux-delete-file-and-buffer)
         ("C-c r"         . crux-rename-file-and-buffer)
         ("C-c o"         . crux-smart-open-line)
         ("C-c O"         . crux-smart-open-line-above)
         ("C-S-k"         . crux-kill-whole-line)))

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)))

(provide 'chadmacs-editing)
;;; chadmacs-editing.el ends here
