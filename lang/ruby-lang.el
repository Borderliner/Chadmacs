;;; ruby-lang.el --- Ruby / Rails support -*- lexical-binding: t; -*-
;;
;; LSP binary (pick one):
;;   gem install solargraph                          ;; classic
;;   gem install ruby-lsp                            ;; Shopify's, faster
;;   gem install rubocop ruby-lsp ruby-lsp-rails    ;; for Rails projects
;;
;; Tree-sitter: ruby grammar via treesit-auto.

;; No `:mode' here: Emacs already sends .rb / .rake / .gemspec /
;; Gemfile / Rakefile to `ruby-mode', and treesit-auto switches that to
;; `ruby-ts-mode' once the grammar is installed. Without a grammar the
;; files keep `ruby-mode' highlighting instead of none.
(use-package ruby-ts-mode
  :ensure nil
  :hook
  (ruby-mode    . eglot-ensure)
  (ruby-ts-mode . eglot-ensure)
  (ruby-mode    . subword-mode)
  (ruby-ts-mode . subword-mode)
  :custom
  (ruby-indent-level 2))

(provide 'ruby-lang)
;;; ruby-lang.el ends here
