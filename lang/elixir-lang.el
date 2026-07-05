;;; elixir-lang.el --- Elixir / Phoenix support -*- lexical-binding: t; -*-
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

;; HEEx (Phoenix component templates).
;;
;; phoenixframework/tree-sitter-heex's v0.9.0 release (Mar 2026) bumped
;; tree-sitter-cli to 0.26.6, which emits ABI 15. Emacs 30.x only supports
;; ABI 13/14, so on Emacs 30 the grammar fails to load with
;; "version-mismatch: 15".
;;
;; Pin via `:abi14-revision' (treesit-auto's blessed field for this exact
;; case). On Emacs <= 30 treesit-auto clones that commit; on Emacs >= 31
;; (which supports ABI 15) it falls back to `:revision' / upstream HEAD
;; automatically - no maintenance when Emacs gets bumped.
;;
;; If you already installed the broken ABI-15 grammar, remove it first:
;;   rm -f ~/.emacs.d/var/tree-sitter/libtree-sitter-heex.*
;; then `M-x treesit-install-language-grammar RET heex RET' (or just open
;; a .heex file - treesit-auto will offer to install).

(use-package heex-ts-mode
  :ensure nil
  :mode "\\.heex\\'"
  :hook (heex-ts-mode . eglot-ensure)
  :init
  (with-eval-after-load 'treesit-auto
    (setq treesit-auto-recipe-list
          (cl-remove-if (lambda (r) (eq (treesit-auto-recipe-lang r) 'heex))
                        treesit-auto-recipe-list))
    (add-to-list 'treesit-auto-recipe-list
                 (make-treesit-auto-recipe
                  :lang 'heex
                  :ts-mode 'heex-ts-mode
                  :url "https://github.com/phoenixframework/tree-sitter-heex"
                  :revision "main"
                  :abi14-revision "248ced3030bb257567cba6074088f25a5dada32d"
                  :source-dir "src"
                  :ext "\\.heex\\'"))))

;; Optional: inf-elixir for IEx REPL integration. Uncomment to enable:
;;
;; (use-package inf-elixir
;;   :ensure t
;;   :bind (:map elixir-ts-mode-map
;;               ("C-c C-z" . inf-elixir)
;;               ("C-c C-l" . inf-elixir-send-buffer)
;;               ("C-c C-r" . inf-elixir-send-region)))

(provide 'elixir-lang)
;;; elixir-lang.el ends here
