;;; go-extension.el --- Go support -*- lexical-binding: t; -*-
;;
;; LSP binary: gopls
;;   go install golang.org/x/tools/gopls@latest
;;
;; Tree-sitter: go + gomod grammars via treesit-auto.

(use-package go-ts-mode
  :ensure nil
  :mode (("\\.go\\'"     . go-ts-mode)
         ("go\\.mod\\'"  . go-mod-ts-mode))
  :hook
  (go-ts-mode  . eglot-ensure)
  (go-ts-mode  . subword-mode)
  :custom
  (go-ts-mode-indent-offset 4))

(provide 'go-extension)
;;; go-extension.el ends here
