;;; scala-lang.el --- Scala support -*- lexical-binding: t; -*-
;;
;; LSP binary: metals
;;   cs install metals
;;   or download from https://scalameta.org/metals/

(use-package scala-mode
  :ensure t
  :mode (("\\.scala\\'" . scala-mode)
         ("\\.sbt\\'"   . scala-mode)
         ("\\.sc\\'"    . scala-mode))
  :hook
  (scala-mode . eglot-ensure)
  (scala-mode . subword-mode))

(provide 'scala-lang)
;;; scala-lang.el ends here
