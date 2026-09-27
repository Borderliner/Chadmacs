;;; web-lang.el --- HTML, CSS, SCSS, Vue support -*- lexical-binding: t; -*-
;;
;; LSP binaries:
;;   npm install -g vscode-langservers-extracted  ;; HTML / CSS / JSON
;;   npm install -g @vue/language-server          ;; Vue (volar)
;;   npm install -g emmet-ls                      ;; emmet expansion via LSP
;;
;; Tree-sitter: html, css grammars auto-install.

(use-package html-ts-mode
  :ensure nil
  :mode (("\\.html?\\'" . html-ts-mode))
  :hook (html-ts-mode . eglot-ensure))

(use-package css-ts-mode
  :ensure nil
  :mode "\\.css\\'"
  :hook (css-ts-mode . eglot-ensure))

;; SCSS - `scss-mode' is built in (css-mode.el, derived from css-mode). The
;; old MELPA package of the same name would shadow it, so don't install it.
(use-package css-mode
  :ensure nil
  :mode ("\\.scss\\'" . scss-mode)
  :hook (scss-mode . eglot-ensure))

;; web-mode for templating files (.erb, .hbs, .vue, mixed-content .html)
(use-package web-mode
  :ensure t
  :mode (("\\.vue\\'"      . web-mode)
         ("\\.erb\\'"      . web-mode)
         ("\\.hbs\\'"      . web-mode)
         ("\\.handlebars\\'" . web-mode)
         ("\\.ejs\\'"      . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.svelte\\'"   . web-mode))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-enable-current-element-highlight t)
  (web-mode-enable-auto-pairing t)
  :hook (web-mode . eglot-ensure))

;; Emmet: HTML/CSS abbreviation expansion (Cmd-style: div>ul>li*3 TAB)
(use-package emmet-mode
  :ensure t
  :hook
  ((html-ts-mode css-ts-mode scss-mode web-mode) . emmet-mode))

(provide 'web-lang)
;;; web-lang.el ends here
