;;; ruby-lang.el --- Ruby / Rails support -*- lexical-binding: t; -*-
;;
;; LSP binary (pick one):
;;   gem install solargraph                          ;; classic
;;   gem install ruby-lsp                            ;; Shopify's, faster
;;   gem install rubocop ruby-lsp ruby-lsp-rails    ;; for Rails projects
;;
;; Tree-sitter: ruby grammar via treesit-auto.

(use-package ruby-ts-mode
  :ensure nil
  :mode (("\\.rb\\'"      . ruby-ts-mode)
         ("\\.rake\\'"    . ruby-ts-mode)
         ("Gemfile\\'"    . ruby-ts-mode)
         ("Rakefile\\'"   . ruby-ts-mode)
         ("\\.gemspec\\'" . ruby-ts-mode))
  :hook
  (ruby-ts-mode . eglot-ensure)
  (ruby-ts-mode . subword-mode)
  :custom
  (ruby-indent-level 2))

(provide 'ruby-lang)
;;; ruby-lang.el ends here
