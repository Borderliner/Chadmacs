;;; markdown-lang.el --- Markdown writing setup -*- lexical-binding: t; -*-
;;
;; LSP binary (optional, useful for note-taking with cross-links):
;;   cargo install marksman
;;   or: brew install marksman / pacman -S marksman
;;
;; markdown-mode is already installed by chadmacs-tools.el (eldoc / eldoc-box
;; uses it to render LSP doc strings). We use `:ensure nil' here so Elpaca
;; doesn't queue it a second time (which raises a "Duplicate item ID queued"
;; warning) - just attach extra modes and hooks.

(use-package markdown-mode
  :ensure nil
  :mode (("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :hook
  (markdown-mode . visual-line-mode)
  (markdown-mode . eglot-ensure)
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-header-scaling t)
  (markdown-asymmetric-header t))

(provide 'markdown-lang)
;;; markdown-lang.el ends here
