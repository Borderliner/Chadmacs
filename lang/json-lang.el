;;; json-lang.el --- JSON / JSONC support -*- lexical-binding: t; -*-
;;
;; LSP binary:
;;   npm install -g vscode-langservers-extracted   ;; provides vscode-json-language-server
;;
;; Tree-sitter: json grammar via treesit-auto.

(use-package json-ts-mode
  :ensure nil
  :mode (("\\.json\\'"   . json-ts-mode)
         ("\\.jsonc\\'"  . json-ts-mode))
  :hook (json-ts-mode . eglot-ensure)
  :custom
  (json-ts-mode-indent-offset 2))

(provide 'json-lang)
;;; json-lang.el ends here
