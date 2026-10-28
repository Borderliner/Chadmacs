;;; rust-lang.el --- Rust support -*- lexical-binding: t; -*-
;;
;; LSP binary: rust-analyzer
;;   rustup component add rust-analyzer
;;
;; rustic builds on rust-mode: cargo commands, test runner, and eglot
;; wiring (`rustic-lsp-client').

(use-package rust-mode
  :ensure t
  :defer t
  :init
  (setq rust-mode-treesitter-derive t))

(use-package rustic
  :ensure t
  :after rust-mode
  :mode ("\\.rs\\'" . rustic-mode)
  :custom
  (rustic-cargo-use-last-stored-arguments t)
  :config
  ;; apheleia runs rustfmt on save (see chadmacs-tools.el)
  (setq rustic-format-on-save nil)
  ;; Diagnostics go to flycheck: `flycheck-eglot-mode' (chadmacs-tools)
  ;; already turns flymake off in eglot buffers, Rust included.
  (setq rustic-lsp-client 'eglot))

(provide 'rust-lang)
;;; rust-lang.el ends here
