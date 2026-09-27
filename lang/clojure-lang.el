;;; clojure-lang.el --- Clojure support -*- lexical-binding: t; -*-
;;
;; No LSP hook here: CIDER talks to a running nREPL for completion,
;; evaluation and docs. Install the Clojure CLI or Leiningen, then run
;; M-x cider-jack-in from inside a project.

(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode))
  :init
  (add-hook 'clojure-mode-hook #'yas-minor-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  ;; Structural editing via smartparens strict mode, as in every other
  ;; Lisp buffer (paredit-style keys come with it - see
  ;; chadmacs-editing.el). Running paredit-mode on top of the
  ;; prog-mode-wide smartparens-mode left two pair managers fighting.
  (add-hook 'clojure-mode-hook #'smartparens-strict-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook #'eldoc-mode)
  (add-hook 'clojure-mode-hook #'idle-highlight-mode)
  )

(use-package cider
  :ensure t
  :config
  (setq nrepl-log-messages t
        cider-repl-display-in-current-window t
        cider-repl-use-clojure-font-lock t
        cider-prompt-save-file-on-load 'always-save
        cider-font-lock-dynamically '(macro core function var)
        nrepl-hide-special-buffers t
        cider-overlays-use-font-lock t
        ;; Set explicitly: `cider-repl-toggle-pretty-printing' flips the
        ;; current value, and since CIDER already defaults to t, calling it
        ;; here turned pretty-printing *off*.
        cider-repl-use-pretty-printing t))

(use-package cider-eval-sexp-fu
  :ensure t
  :after (cider clojure-mode))

(use-package clj-refactor
  :ensure t
  :after (cider clojure-mode)
  :config (cljr-add-keybindings-with-prefix "C-c C-m"))

;; Enabled from clojure-mode-hook above only. It used to hook every
;; prog-mode and text-mode buffer, so turning on Clojure support changed
;; all other languages too.
(use-package idle-highlight-mode
  :ensure t
  :defer t
  :config (setq idle-highlight-idle-time 0.2))

(provide 'clojure-lang)
;;; clojure-lang.el ends here
