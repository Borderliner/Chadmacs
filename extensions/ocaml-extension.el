;;; ocaml-extension.el --- OCaml / Reason support -*- lexical-binding: t; -*-
;;
;; LSP binary: ocaml-lsp
;;   opam install ocaml-lsp-server

(use-package tuareg
  :ensure t
  :mode (("\\.ml[ily]?\\'" . tuareg-mode)
         ("\\.dune\\'"     . tuareg-mode))
  :hook (tuareg-mode . eglot-ensure))

(use-package dune
  :ensure t
  :after tuareg)

(provide 'ocaml-extension)
;;; ocaml-extension.el ends here
