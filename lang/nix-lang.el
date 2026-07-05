;;; nix-lang.el --- Nix language support -*- lexical-binding: t; -*-
;;
;; LSP binary (pick one):
;;   nix profile install nixpkgs#nil     ;; nil — community LSP
;;   nix profile install nixpkgs#nixd    ;; nixd — modern alternative

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'"
  :hook (nix-mode . eglot-ensure))

(provide 'nix-lang)
;;; nix-lang.el ends here
