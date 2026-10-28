;;; python-lang.el --- Python support -*- lexical-binding: t; -*-
;;
;; LSP binary (pick one):
;;   pip install python-lsp-server     ;; pylsp (default fallback)
;;   pip install pyright                ;; preferred by many
;;   pip install python-lsp-ruff        ;; super-fast linter plugin for pylsp
;;
;; Tree-sitter: python grammar via treesit-auto on first open.

;; No `:mode' here: Emacs already sends .py / .pyi / .pyw to
;; `python-mode', and treesit-auto switches that to `python-ts-mode'
;; once the grammar is installed. Mapping the files to `python-ts-mode'
;; directly left them with no highlighting at all whenever the grammar
;; was missing, instead of falling back to `python-mode'.
(use-package python
  :ensure nil
  :hook
  (python-mode    . eglot-ensure)
  (python-ts-mode . eglot-ensure)
  (python-mode    . subword-mode)
  (python-ts-mode . subword-mode)
  :custom
  (python-indent-offset 4)
  (python-shell-interpreter "python3"))

;; Optional: pyvenv for virtualenv management. Activate per project with
;;   M-x pyvenv-activate
;; or set `pyvenv-workon' in .dir-locals.el. Uncomment to enable:
;;
;; (use-package pyvenv
;;   :ensure t
;;   :hook (python-ts-mode . pyvenv-mode))

(provide 'python-lang)
;;; python-lang.el ends here
