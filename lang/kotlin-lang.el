;;; kotlin-lang.el --- Kotlin support -*- lexical-binding: t; -*-
;;
;; LSP binary: kotlin-language-server
;;   download: https://github.com/fwcd/kotlin-language-server/releases
;;
;; Tree-sitter: kotlin grammar via treesit-auto.

(use-package kotlin-ts-mode
  :ensure t
  :mode (("\\.kt\\'"  . kotlin-ts-mode)
         ("\\.kts\\'" . kotlin-ts-mode))
  :hook (kotlin-ts-mode . eglot-ensure))

(provide 'kotlin-lang)
;;; kotlin-lang.el ends here
