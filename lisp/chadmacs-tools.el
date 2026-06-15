;;; chadmacs-tools.el --- Programming & development tools -*- lexical-binding: t; -*-
;;
;; Commentary:
;;   Projectile, magit, flycheck, eglot (LSP), envrc, apheleia (formatters),
;;   tree-sitter, eldoc-box, markdown-mode, and a few lisp helpers.
;;
;;; Code:

;; -------------------------------------------------------- Project mgmt --

(use-package projectile
  :ensure t
  :init
  ;; Optional: auto-discover projects in common directories
  ;; (setq projectile-project-search-path '("~/code/" "~/work/" "~/projects/"))
  :config
  (projectile-mode +1)

  (setq projectile-completion-system 'default)
  ;; Sort projects by recent activity (works nicely with recentf and dashboard)
  (setq projectile-sort-order 'recentf)
  ;; Faster indexing on large repos (uses external git / fd / find)
  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching t)

  ;; Standard key prefix
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

  ;; Integrate Projectile with project.el so tools that depend on project.el
  ;; (eglot, consult-project-buffer, ...) see Projectile-managed roots.
  (defun my/projectile-project-find-function (dir)
    "Identify Projectile-managed projects for project.el.
Return (cons \\='transient ROOT) if DIR is part of a known Projectile project."
    (when-let ((root (projectile-project-root dir)))
      (cons 'transient root)))

  (add-to-list 'project-find-functions #'my/projectile-project-find-function))

;; -------------------------------------------------------------- Git --

(use-package transient
  :ensure t)

(use-package pinentry
  :ensure t
  :config
  (pinentry-start))

(use-package magit
  :ensure t)

;; ----------------------------------------------------------- Flycheck --

(use-package flycheck
  :ensure t
  :config
  (setq flycheck-idle-change-delay 5)
  (setq flycheck-idle-buffer-switch-delay 5)
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (global-flycheck-mode))

(use-package flycheck-posframe
  :ensure t
  :after flycheck
  :if (display-graphic-p)
  :init
  (setq flycheck-posframe-border-width 16)
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode)
  (flycheck-posframe-configure-pretty-defaults))

;; --------------------------------------------------------------- LSP --

;; Eglot ships with `eglot-server-programs' covering most common LSPs
;; (pylsp/pyright, typescript-language-server, gopls, bash-language-server,
;; yaml-language-server, marksman, etc). You need the binaries on PATH -
;; install via the language's own toolchain (pip install python-lsp-server,
;; npm install -g typescript-language-server, go install golang.org/x/tools/gopls,
;; pacman/brew install bash-language-server / yaml-language-server / marksman).
(use-package eglot
  :ensure nil
  :commands (eglot-ensure eglot-rename eglot-format-buffer)
  :bind
  (:map eglot-mode-map
        ("C-c l r" . eglot-rename)
        ("C-c l f" . eglot-format-buffer)
        ("C-c l a" . eglot-code-actions)
        ("C-c l h" . eldoc))
  :hook
  ;; C family
  (c-mode             . eglot-ensure)
  (c++-mode           . eglot-ensure)
  (csharp-ts-mode     . eglot-ensure)
  ;; Python
  (python-mode        . eglot-ensure)
  (python-ts-mode     . eglot-ensure)
  ;; JS / TS
  (js-mode            . eglot-ensure)
  (js-ts-mode         . eglot-ensure)
  (typescript-ts-mode . eglot-ensure)
  (tsx-ts-mode        . eglot-ensure)
  ;; Go
  (go-mode            . eglot-ensure)
  (go-ts-mode         . eglot-ensure)
  ;; Shell
  (sh-mode            . eglot-ensure)
  (bash-ts-mode       . eglot-ensure)
  ;; YAML
  (yaml-mode          . eglot-ensure)
  (yaml-ts-mode       . eglot-ensure)
  ;; Markdown (marksman) - opt in by uncommenting
  ;; (markdown-mode   . eglot-ensure)
  )

;; Bridge eglot diagnostics into flycheck (since flycheck is globally enabled)
(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

;; ----------------------------------------------------- direnv (envrc) --

;; Auto-load .envrc / direnv environments per project. Eglot, flycheck, and
;; compile commands then see the correct PATH / env vars (rust-toolchain,
;; nix-shell, devbox, asdf shims, etc). Requires the `direnv' binary on PATH.
;; envrc MUST initialize after other minor modes so its env-injection applies
;; everywhere - that's why it hooks `after-init' globally rather than per-mode.
(use-package envrc
  :ensure t
  :hook (after-init . envrc-global-mode)
  :bind (:map envrc-mode-map
              ("C-c E" . envrc-command-map)))

;; ----------------------------------------------------------- Format --

;; Apheleia runs code formatters (shfmt, black, prettier, rustfmt, ...) async
;; on save without disrupting the cursor position.
(use-package apheleia
  :ensure t
  :commands (apheleia-mode apheleia-global-mode)
  :hook (prog-mode . apheleia-mode))

;; ----------------------------------------------------- Elisp helpers --

(use-package highlight-defined
  :ensure t
  :commands highlight-defined-mode
  :hook (lisp-data-mode . highlight-defined-mode))

(use-package elisp-refs
  :ensure t
  :commands (elisp-refs-function
             elisp-refs-macro
             elisp-refs-variable
             elisp-refs-special
             elisp-refs-symbol))

;; --------------------------------------------------------- Tree-sitter --

;; Tree-sitter in Emacs is an incremental parsing system providing precise,
;; high-performance syntax highlighting. Supports Bash, C, C++, C#, CMake,
;; CSS, Dockerfile, Go, Java, JavaScript, JSON, Python, Rust, TOML,
;; TypeScript, YAML, Elisp, Lua, Markdown, and many others.
(use-package treesit-auto
  :ensure t
  :init
  (setq treesit-auto-install 'prompt)
  (setq treesit-auto-add-to-auto-mode-alist 'all)
  :config
  (defvar my/treesit-dir (expand-file-name "tree-sitter/" my/var-dir))

  (defun my/treesit-install-redirect (orig-fn &rest args)
    "Redirect tree-sitter grammar installs under `my/var-dir'."
    (let ((user-emacs-directory my/var-dir))
      (apply orig-fn args)))

  (advice-add 'treesit-install-language-grammar
              :around #'my/treesit-install-redirect)

  (setq treesit-extra-load-path (list my/treesit-dir))

  ;; Custom C# tree-sitter recipe (the default points at the wrong subdir)
  (setq my/csharp-ts-config
        (make-treesit-auto-recipe
         :lang 'c-sharp
         :ts-mode 'csharp-ts-mode
         :remap '(csharp-mode)
         :url "https://github.com/tree-sitter/tree-sitter-c-sharp"
         :revision "master"
         :source-dir "src"
         :ext "\\.cs\\'"))
  (add-to-list 'treesit-auto-recipe-list my/csharp-ts-config)
  (global-treesit-auto-mode))

;; ----------------------------------------------------- Misc language --

;; Markdown major mode (needed for eldoc / docs rendering even if you don't
;; edit markdown files).
(use-package markdown-mode
  :ensure t)

;; -------------------------------------------------------- Eldoc UI --

(use-package eldoc-box
  :ensure t
  :if (display-graphic-p)
  :bind
  (("C-<prior>" . eldoc-box-scroll-down)
   ("C-<next>"  . eldoc-box-scroll-up))
  :hook (prog-mode . eldoc-box-hover-mode)
  :config
  (when (fboundp 'doom-color)
    (set-face-attribute 'eldoc-box-border nil
                        :background (doom-color 'yellow)))

  ;; Place the childframe at the top-right of the entire frame (ignores
  ;; treemacs / window splits).
  (defun my/eldoc-box--always-frame-top-right (width _height)
    "Place the childframe at the top-right corner of the entire Emacs frame."
    (pcase-let ((`(,_left ,right ,top) eldoc-box-offset))
      (cons (- (frame-outer-width) width right)
            top)))
  (setq eldoc-box-position-function #'my/eldoc-box--always-frame-top-right))

(provide 'chadmacs-tools)
;;; chadmacs-tools.el ends here
