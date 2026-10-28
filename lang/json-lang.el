;;; json-lang.el --- JSON / JSONC support -*- lexical-binding: t; -*-
;;
;; LSP binary:
;;   npm install -g vscode-langservers-extracted   ;; provides vscode-json-language-server
;;
;; Tree-sitter: json grammar via treesit-auto.

;; Files go to the classic `js-json-mode' (Emacs's own mapping for .json;
;; added here for .jsonc), and treesit-auto switches them to
;; `json-ts-mode' once the grammar is installed. Without a grammar they
;; keep `js-json-mode' highlighting instead of none.
(use-package json-ts-mode
  :ensure nil
  :mode ("\\.jsonc\\'" . js-json-mode)
  :hook
  (json-ts-mode . eglot-ensure)
  (js-json-mode . eglot-ensure)
  :custom
  (json-ts-mode-indent-offset 2))

(provide 'json-lang)
;;; json-lang.el ends here
