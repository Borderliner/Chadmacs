;;; zig-lang.el --- Zig support -*- lexical-binding: t; -*-
;;
;; LSP binary: zls (Zig Language Server)
;;   Download from https://github.com/zigtools/zls/releases
;;   or build: git clone https://github.com/zigtools/zls && cd zls && zig build

(use-package zig-mode
  :ensure t
  :mode "\\.zig\\'"
  :hook (zig-mode . eglot-ensure)
  :custom
  (zig-format-on-save nil))

(provide 'zig-lang)
;;; zig-lang.el ends here
