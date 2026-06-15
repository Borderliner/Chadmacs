;;; yaml-extension.el --- YAML support -*- lexical-binding: t; -*-
;;
;; LSP binary:
;;   npm install -g yaml-language-server
;;
;; Tree-sitter: yaml grammar via treesit-auto.

(use-package yaml-ts-mode
  :ensure nil
  :mode (("\\.ya?ml\\'"   . yaml-ts-mode)
         ("\\.eyaml\\'"   . yaml-ts-mode))
  :hook (yaml-ts-mode . eglot-ensure))

(provide 'yaml-extension)
;;; yaml-extension.el ends here
