;;; elixir-extension.el --- Elixir / Phoenix support -*- lexical-binding: t; -*-
;;
;; LSP binary: elixir-ls (or lexical for newer alternative)
;;   git clone https://github.com/elixir-lsp/elixir-ls
;;   cd elixir-ls && mix deps.get && mix elixir_ls.release2 -o /opt/elixir-ls
;;   Then add /opt/elixir-ls/language_server.sh (or .bat) to PATH.
;;
;; Tree-sitter: elixir + heex grammars auto-install via treesit-auto.
;;
;; elixir-ts-mode and heex-ts-mode are built in to Emacs 30+.

(use-package elixir-ts-mode
  :ensure nil
  :mode (("\\.exs?\\'"    . elixir-ts-mode)
         ("mix\\.lock\\'" . elixir-ts-mode))
  :hook
  (elixir-ts-mode . eglot-ensure)
  (elixir-ts-mode . subword-mode))

(use-package heex-ts-mode
  :ensure nil
  :mode "\\.heex\\'"
  :hook (heex-ts-mode . eglot-ensure))

;; Optional: inf-elixir for IEx REPL integration. Uncomment to enable:
;;
;; (use-package inf-elixir
;;   :ensure t
;;   :bind (:map elixir-ts-mode-map
;;               ("C-c C-z" . inf-elixir)
;;               ("C-c C-l" . inf-elixir-send-buffer)
;;               ("C-c C-r" . inf-elixir-send-region)))

(provide 'elixir-extension)
;;; elixir-extension.el ends here
