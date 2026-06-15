;;; typescript-extension.el --- TypeScript / TSX / JavaScript support -*- lexical-binding: t; -*-
;;
;; LSP binary:
;;   npm install -g typescript typescript-language-server
;;
;; Tree-sitter grammars (typescript, tsx, javascript) auto-install on first
;; open via treesit-auto. Accept the prompt or run
;;   M-x treesit-install-language-grammar RET typescript / tsx / javascript
;;
;; Eglot's `eglot-server-programs' already wires every *-ts-mode here to
;; typescript-language-server.

(use-package typescript-ts-mode
  :ensure nil
  :mode (("\\.ts\\'"  . typescript-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.cts\\'" . typescript-ts-mode))
  :hook
  (typescript-ts-mode . eglot-ensure)
  (typescript-ts-mode . subword-mode))

(use-package tsx-ts-mode
  :ensure nil
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode))
  :hook
  (tsx-ts-mode . eglot-ensure)
  (tsx-ts-mode . subword-mode))

(use-package js-ts-mode
  :ensure nil
  :mode (("\\.js\\'"  . js-ts-mode)
         ("\\.mjs\\'" . js-ts-mode)
         ("\\.cjs\\'" . js-ts-mode))
  :hook
  (js-ts-mode . eglot-ensure)
  (js-ts-mode . subword-mode))

(provide 'typescript-extension)
;;; typescript-extension.el ends here
