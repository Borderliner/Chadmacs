;;; typescript-lang.el --- TypeScript / TSX / JavaScript support -*- lexical-binding: t; -*-
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

;; JavaScript goes to the classic `js-mode' (Emacs's own mapping for .js;
;; added here for .mjs / .cjs), and treesit-auto switches it to
;; `js-ts-mode' once the grammar is installed. Mapping the files to
;; `js-ts-mode' directly left them with no highlighting at all whenever
;; the grammar was missing. TypeScript / TSX have no classic mode in
;; Emacs to fall back to, so they stay mapped to their ts modes above.
(use-package js-ts-mode
  :ensure nil
  :mode (("\\.mjs\\'" . js-mode)
         ("\\.cjs\\'" . js-mode))
  :hook
  (js-mode    . eglot-ensure)
  (js-ts-mode . eglot-ensure)
  (js-mode    . subword-mode)
  (js-ts-mode . subword-mode))

(provide 'typescript-lang)
;;; typescript-lang.el ends here
