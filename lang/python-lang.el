;;; python-lang.el --- Python support -*- lexical-binding: t; -*-
;;
;; LSP binary (pick one):
;;   pip install python-lsp-server     ;; pylsp (default fallback)
;;   pip install pyright                ;; preferred by many
;;   pip install python-lsp-ruff        ;; super-fast linter plugin for pylsp
;;
;; Tree-sitter: python grammar via treesit-auto on first open.

(use-package python
  :ensure nil
  :mode (("\\.py\\'"  . python-ts-mode)
         ("\\.pyi\\'" . python-ts-mode)
         ("\\.pyw\\'" . python-ts-mode))
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
