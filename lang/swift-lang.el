;;; swift-lang.el --- Swift support -*- lexical-binding: t; -*-
;;
;; LSP binary: sourcekit-lsp (bundled with Swift toolchain on macOS,
;; install Swift toolchain on Linux).

(use-package swift-mode
  :ensure t
  :mode "\\.swift\\'"
  :hook
  (swift-mode . eglot-ensure)
  (swift-mode . subword-mode))

(provide 'swift-lang)
;;; swift-lang.el ends here
