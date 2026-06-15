;;; markdown-extension.el --- Markdown writing setup -*- lexical-binding: t; -*-
;;
;; LSP binary (optional, useful for note-taking with cross-links):
;;   cargo install marksman
;;   or: brew install marksman / pacman -S marksman
;;
;; markdown-mode is already installed by chadmacs-tools.el (needed by eldoc
;; rendering). This file adds writing-friendly hooks and the eglot binding.

(use-package markdown-mode
  :ensure t
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

(provide 'markdown-extension)
;;; markdown-extension.el ends here
