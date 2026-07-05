;;; docker-lang.el --- Dockerfile / docker-compose support -*- lexical-binding: t; -*-
;;
;; LSP binary:
;;   npm install -g dockerfile-language-server-nodejs
;;
;; Tree-sitter: dockerfile grammar via treesit-auto.

(use-package dockerfile-ts-mode
  :ensure nil
  :mode (("Dockerfile\\'" . dockerfile-ts-mode)
         ("\\.dockerfile\\'" . dockerfile-ts-mode))
  :hook (dockerfile-ts-mode . eglot-ensure))

;; docker-compose.yml files are picked up by yaml-lang; activate that
;; together with this one for a full Docker workflow.

;; Optional: full docker management UI (containers, images, networks, volumes).
;; Uncomment to enable:
;;
;; (use-package docker
;;   :ensure t
;;   :bind ("C-c C-d" . docker))

(provide 'docker-lang)
;;; docker-lang.el ends here
