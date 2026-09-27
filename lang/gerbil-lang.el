;;; gerbil-lang.el --- Gerbil Scheme support -*- lexical-binding: t; -*-
;;
;; Gerbil ships its Emacs support (gerbil-mode.el, gambit.el) inside the
;; Gerbil installation instead of on a package archive, so nothing is
;; installed here: the modes are loaded from `*gerbil-path*'. The whole
;; module is a no-op when that directory doesn't exist.
;;
;; Installed somewhere other than /opt/gerbil? Set the path in custom.el
;; *above* the (require 'gerbil-lang) line, e.g.
;;   (defvar *gerbil-path* "~/.local/gerbil")
;;
;; `C-c C-g' turns the current buffer into a Gerbil buffer with a gxi REPL
;; next to it.

(defvar *gerbil-path* "/opt/gerbil"
  "Root of the Gerbil installation (holds bin/gxi and share/emacs/site-lisp).")

(use-package gerbil-mode
  :ensure nil
  :when (file-directory-p *gerbil-path*)
  :preface
  (defun gerbil-setup-buffers ()
    "Change current buffer mode to gerbil-mode and start a REPL"
    (interactive)
    (gerbil-mode)
    (split-window-right)
    (shrink-window-horizontally 2)
    (let ((buf (buffer-name)))
      (other-window 1)
      (run-scheme scheme-program-name)
      (switch-to-buffer-other-window "*scheme*" nil)
      (switch-to-buffer buf)))
  (defun clear-comint-buffer ()
    (interactive)
    (with-current-buffer "*scheme*"
      (let ((comint-buffer-maximum-size 0))
        (comint-truncate-buffer))))
  :mode (("\\.ss\\'"  . gerbil-mode)
         ("\\.pkg\\'" . gerbil-mode))
  :bind (:map comint-mode-map
              (("C-S-n" . comint-next-input)
               ("C-S-p" . comint-previous-input)
               ("C-S-l" . clear-comint-buffer))
              :map gerbil-mode-map
              (("C-S-l" . clear-comint-buffer)))
  :init
  (add-to-list 'load-path
               (expand-file-name "share/emacs/site-lisp" *gerbil-path*))
  (autoload 'gerbil-mode "gerbil-mode"
    "Gerbil editing mode." t)
  ;; gambit-inferior-mode lives in gambit.el, not gerbil-mode.el; without
  ;; this the :hook below would autoload it from the wrong file when a
  ;; Scheme REPL starts before any Gerbil buffer was opened.
  (autoload 'gambit-inferior-mode "gambit"
    "Gambit inferior REPL mode." t)
  (global-set-key (kbd "C-c C-g") 'gerbil-setup-buffers)
  (add-to-list 'exec-path (expand-file-name "bin" *gerbil-path*))
  :hook
  (inferior-scheme-mode . gambit-inferior-mode)
  :config
  (require 'gambit
           (expand-file-name "share/emacs/site-lisp/gambit.el" *gerbil-path*))
  (setf scheme-program-name (expand-file-name "bin/gxi" *gerbil-path*))
  (let ((tags (locate-dominating-file default-directory "TAGS")))
    (when tags (visit-tags-table tags)))
  (let ((tags (expand-file-name "src/TAGS" *gerbil-path*)))
    (when (file-exists-p tags) (visit-tags-table tags))))

(provide 'gerbil-lang)
;;; gerbil-lang.el ends here
