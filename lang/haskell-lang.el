;;; haskell-lang.el --- Haskell support -*- lexical-binding: t; -*-
;;
;; LSP binary: haskell-language-server (HLS)
;;   ghcup install hls
;;   or: nix-shell -p haskell-language-server

(use-package haskell-mode
  :ensure t
  :mode (("\\.hs\\'"    . haskell-mode)
         ("\\.lhs\\'"   . literate-haskell-mode)
         ("\\.cabal\\'" . haskell-cabal-mode))
  :hook
  (haskell-mode . eglot-ensure)
  (haskell-mode . interactive-haskell-mode)
  (haskell-mode . subword-mode)
  :custom
  (haskell-indentation-electric-flag t))

(provide 'haskell-lang)
;;; haskell-lang.el ends here
