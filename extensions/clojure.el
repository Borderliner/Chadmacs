
(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode))
  :init
  (add-hook 'clojure-mode-hook #'yas-minor-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'paredit-mode)
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
        cider-overlays-use-font-lock t)
  (cider-repl-toggle-pretty-printing))

(use-package cider-eval-sexp-fu
  :ensure t
  :after (cider clojure-mode))

(use-package clj-refactor
  :ensure t
  :after (cider clojure-mode)
  :config (cljr-add-keybindings-with-prefix "C-c C-m"))

(use-package idle-highlight-mode
  :ensure t
  :config (setq idle-highlight-idle-time 0.2)

  :hook ((prog-mode text-mode) . idle-highlight-mode))

(provide 'clojure-extension)
