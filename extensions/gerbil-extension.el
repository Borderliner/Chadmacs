
;; (defvar *gerbil-path* "/opt/gerbil")

;; (use-package gerbil-mode
;;   :when (file-directory-p *gerbil-path*)
;;   :preface
;;   (defun gerbil-setup-buffers ()
;;     "Change current buffer mode to gerbil-mode and start a REPL"
;;     (interactive)
;;     (gerbil-mode)
;;     (split-window-right)
;;     (shrink-window-horizontally 2)
;;     (let ((buf (buffer-name)))
;;       (other-window 1)
;;       (run-scheme scheme-program-name)
;;       (switch-to-buffer-other-window "*scheme*" nil)
;;       (switch-to-buffer buf)))
;;   (defun clear-comint-buffer ()
;;     (interactive)
;;     (with-current-buffer "*scheme*"
;;       (let ((comint-buffer-maximum-size 0))
;;         (comint-truncate-buffer))))
;;   :mode (("\\.ss\\'"  . gerbil-mode)
;;          ("\\.pkg\\'" . gerbil-mode))
;;   :bind (:map comint-mode-map
;;               (("C-S-n" . comint-next-input)
;;                ("C-S-p" . comint-previous-input)
;;                ("C-S-l" . clear-comint-buffer))
;;               :map gerbil-mode-map
;;               (("C-S-l" . clear-comint-buffer)))
;;   :init
;;   (add-to-list 'load-path
;;                (expand-file-name "share/emacs/site-lisp" *gerbil-path*))
;;   (autoload 'gerbil-mode "gerbil-mode"
;;     "Gerbil editing mode." t)
;;   (global-set-key (kbd "C-c C-g") 'gerbil-setup-buffers)
;;   (add-to-list 'exec-path (expand-file-name "bin" *gerbil-path*))
;;   :hook
;;   (inferior-scheme-mode . gambit-inferior-mode)
;;   :config
;;   (require 'gambit
;;            (expand-file-name "share/emacs/site-lisp/gambit.el" *gerbil-path*))
;;   (setf scheme-program-name (expand-file-name "bin/gxi" *gerbil-path*))
;;   (let ((tags (locate-dominating-file default-directory "TAGS")))
;;     (when tags (visit-tags-table tags)))
;;   (let ((tags (expand-file-name "src/TAGS" *gerbil-path*)))
;;     (when (file-exists-p tags) (visit-tags-table tags))))

;; (provide 'gerbil-extension)
