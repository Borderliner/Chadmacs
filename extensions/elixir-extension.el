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

;; HEEx (Phoenix component templates).
;;
;; Phoenix's tree-sitter-heex grammar repo currently builds with the new
;; tree-sitter CLI (>= 0.24) which emits ABI 15. Emacs 30.x only supports
;; ABI 13 / 14, so loading the grammar produces:
;;
;;   Cannot activate tree-sitter, because language grammar for heex is
;;   unavailable (version-mismatch): 15
;;
;; Until Emacs 31 (ABI 15) lands or upstream ships an ABI 14 build, we:
;;   1. Skip activating heex-ts-mode when the grammar is unusable.
;;   2. Tell treesit-auto to stop auto-installing it (silences the
;;      install prompt on every .heex visit).
;;   3. Fall back to web-mode (if loaded via web-extension) or html-mode
;;      so .heex files still get reasonable syntax highlighting.
;;
;; If you want full heex-ts-mode support, install an older grammar
;; revision manually:
;;   M-x treesit-install-language-grammar RET heex RET
;; and enter a commit hash from before the tree-sitter CLI 0.24 bump.

(defun chadmacs--heex-grammar-usable-p ()
  "Non-nil when the heex grammar is installed AND at a supported ABI."
  (and (fboundp 'treesit-language-available-p)
       (treesit-language-available-p 'heex)))

(defun chadmacs--heex-fallback-mode ()
  "Pick web-mode if loaded, else html-mode for .heex files."
  (cond
   ((fboundp 'web-mode)  (web-mode))
   (t                    (html-mode))))

(if (chadmacs--heex-grammar-usable-p)
    (use-package heex-ts-mode
      :ensure nil
      :mode "\\.heex\\'"
      :hook (heex-ts-mode . eglot-ensure))
  (add-to-list 'auto-mode-alist
               '("\\.heex\\'" . chadmacs--heex-fallback-mode))
  (with-eval-after-load 'treesit-auto
    (setq treesit-auto-langs (delq 'heex treesit-auto-langs))))

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
