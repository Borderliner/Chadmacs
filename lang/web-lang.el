;;; web-lang.el --- HTML, CSS, SCSS, Vue support -*- lexical-binding: t; -*-
;;
;; LSP binaries:
;;   npm install -g vscode-langservers-extracted  ;; HTML / CSS / JSON
;;   npm install -g @vue/language-server          ;; Vue (volar) - opt-in, see web-mode below
;;   npm install -g emmet-ls                      ;; emmet expansion via LSP
;;
;; Tree-sitter: html, css grammars auto-install.

;; No `:mode' for HTML or CSS: Emacs already sends .html / .htm to
;; `mhtml-mode' and .css to `css-mode', and treesit-auto switches those
;; to `html-ts-mode' / `css-ts-mode' once the grammars are installed.
;; Mapping the files to the ts modes directly left them with no
;; highlighting at all whenever a grammar was missing.
(use-package html-ts-mode
  :ensure nil
  :hook
  (mhtml-mode   . eglot-ensure)
  (html-ts-mode . eglot-ensure))

(use-package css-ts-mode
  :ensure nil
  :hook (css-ts-mode . eglot-ensure))

;; SCSS - `scss-mode' is built in (css-mode.el, derived from css-mode), so
;; the `css-mode' hook covers SCSS buffers too. The old MELPA package of
;; the same name would shadow it, so don't install it.
(use-package css-mode
  :ensure nil
  :mode ("\\.scss\\'" . scss-mode)
  :hook (css-mode . eglot-ensure))

;; web-mode for templating files (.erb, .hbs, .vue, mixed-content .html).
;; No `eglot-ensure' hook: eglot has no server mapping for web-mode (one
;; mode for many template languages), so it could only fail on every
;; file. For Vue, map web-mode to the Vue server in `eglot-server-programs'
;; in custom.el and hook eglot-ensure there.
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
  (web-mode-enable-auto-pairing t))

;; Emmet: HTML/CSS abbreviation expansion (Cmd-style: div>ul>li*3 TAB).
;; `css-mode' covers `scss-mode', which derives from it.
(use-package emmet-mode
  :ensure t
  :hook
  ((html-ts-mode mhtml-mode css-ts-mode css-mode web-mode) . emmet-mode))

(provide 'web-lang)
;;; web-lang.el ends here
