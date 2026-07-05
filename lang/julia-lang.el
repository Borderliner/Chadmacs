;;; julia-lang.el --- Julia support -*- lexical-binding: t; -*-
;;
;; LSP binary: LanguageServer.jl
;;   Set up in your Julia env:
;;     using Pkg
;;     Pkg.add("LanguageServer")
;;     Pkg.add("SymbolServer")
;;
;; eglot-jl wires eglot to LanguageServer.jl.

(use-package julia-mode
  :ensure t
  :mode "\\.jl\\'")

(use-package eglot-jl
  :ensure t
  :after julia-mode
  :hook (julia-mode . (lambda () (eglot-jl-init) (eglot-ensure))))

(provide 'julia-lang)
;;; julia-lang.el ends here
