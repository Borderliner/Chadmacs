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

  ;; On Debian / Ubuntu the fd binary is called `fdfind' (the `fd' name is
  ;; reserved by an unrelated package). Point projectile at whichever exists.
  (setq projectile-fd-executable (or (executable-find "fd")
                                     (executable-find "fdfind")))

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

;; Eglot ships with `eglot-server-programs' covering most common LSPs. Each
;; language's eglot hook lives in its dedicated extensions/<lang>-extension.el
;; so users opt in per-language by uncommenting (require 'lang-extension) in
;; init.el. The hooks below are the always-on baseline (sh + bash always work).
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
  (sh-mode      . eglot-ensure)
  (bash-ts-mode . eglot-ensure))

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
;; Maximum font-lock decoration for tree-sitter modes.
;;
;; Emacs treesit feature groups are layered: level 1 = comment/definition,
;; level 2 += keyword/string/type, level 3 += assignment/builtin/constant
;; (default), level 4 += function (calls), variable (references), bracket,
;; delimiter, operator, property.
;;
;; At level 3, function *definitions* get colored but function *calls* and
;; variable *references* don't. Bumping to 4 colors them like every other
;; modern editor expects. Must be set before any treesit major mode loads,
;; hence here in :init of the treesit-auto block.
(setq treesit-font-lock-level 4)

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

  ;; Custom C# tree-sitter recipe (the default points at the wrong subdir).
  ;; Pinned to v0.23.5 via :abi14-revision so Emacs 30 (ABI 14 only) gets
  ;; a loadable grammar; Emacs 31+ (ABI 15) will pick :revision automatically.
  (setq my/csharp-ts-config
        (make-treesit-auto-recipe
         :lang 'c-sharp
         :ts-mode 'csharp-ts-mode
         :remap '(csharp-mode)
         :url "https://github.com/tree-sitter/tree-sitter-c-sharp"
         :revision "master"
         :abi14-revision "v0.23.5"
         :source-dir "src"
         :ext "\\.cs\\'"))
  (add-to-list 'treesit-auto-recipe-list my/csharp-ts-config)

  ;; --- ABI 14 pin set for Emacs <= 30 ----------------------------------
  ;;
  ;; Many grammars' `main' branches now build with tree-sitter CLI 0.24+,
  ;; which emits ABI 15. Emacs 30.x only loads ABI 13/14. Treesit-auto
  ;; ships :abi14-revision pins for some grammars (python, go, gomod,
  ;; yaml, bash) but not for all; we add the rest below.
  ;;
  ;; `:abi14-revision' is the forward-compatible knob: on Emacs 30 the
  ;; pinned tag is cloned; on Emacs 31+ treesit-auto uses :revision / HEAD
  ;; automatically. No maintenance needed when Emacs gets bumped.
  ;;
  ;; Each tag below is the most recent v0.23.x (last release before the
  ;; upstream CLI bump). If you find a missing language, file a PR with
  ;; treesit-auto so the pin lands upstream for everyone.
  (let ((abi14-pins
         '(;; --- mainstream popular ---
           (c          . "v0.23.6")
           (cpp        . "v0.23.4")
           (typescript . "v0.23.2")
           (tsx        . "v0.23.2")
           (javascript . "v0.23.1")
           (rust       . "v0.23.3")
           (json       . "v0.23.0")
           (html       . "v0.23.2")
           (css        . "v0.23.2")
           (ruby       . "v0.23.1")
           (markdown   . "v0.4.1")
           (julia      . "v0.23.1")
           (lua        . "v0.2.0")
           (php        . "v0.23.12")
           (scala      . "v0.23.4")
           (sql        . "v0.3.8")
           ;; --- per-extension set ---
           (haskell    . "v0.23.1")
           (ocaml      . "v0.23.2")
           (elixir     . "v0.3.0")
           (dockerfile . "v0.2.0")
           (kotlin     . "0.3.8")
           (zig        . "v1.0.2")
           (swift      . "0.7.1")
           (nix        . "v0.0.2")
           (vue        . "v0.2.1")
           ;; --- niche, but functional with the right pin ---
           (hyprlang   . "v3.0.0")   ; Feb 2024 release, ABI 14
           (dart       . "d4d8f3e")  ; Oct 4 2025 commit, last before the
                                     ; "tree-sitter.json for CLI 0.25" bump
           (make       . "main"))))  ; alemuller/tree-sitter-make stable since 2022
    (dolist (recipe treesit-auto-recipe-list)
      (when-let ((pin (alist-get (treesit-auto-recipe-lang recipe)
                                 abi14-pins)))
        (setf (treesit-auto-recipe-abi14-revision recipe) pin))))

  ;; The remaining warning-list languages stay out of `treesit-auto-langs'.
  ;; Each has a hard upstream problem - listing them again with pinning
  ;; would only convert "version-mismatch" into a different error class:
  ;;
  ;;   bibtex      : upstream repo has no tagged releases AND its main
  ;;                 branch builds with an incompatible CLI; no known
  ;;                 ABI-14-clean commit to pin to.
  ;;   cobol       : treesit-auto's recipe builds a .so that doesn't
  ;;                 export `tree_sitter_cobol' (broken parser-generation
  ;;                 in the upstream grammar). Pinning to older commits
  ;;                 reproduces the same undefined-symbol error.
  ;;   commonlisp  : repo intermittently unreachable from cloning. Not a
  ;;                 pin problem; treesit-auto fails the clone itself.
  ;;   gitcommit   : the-mikedavis/tree-sitter-gitcommit was archived
  ;;                 read-only in July 2025; no fork has taken over.
  ;;   magik       : no canonical grammar repo exists at the URL
  ;;                 treesit-auto's recipe expects.
  ;;   nu          : no release tags AND main branch builds with CLI
  ;;                 0.24+; no easily-pinnable commit.
  ;;   perl        : the perl-lang grammar publishes its first tagged
  ;;                 release (v1.0.0, Mar 2026) AFTER the CLI 0.24 bump;
  ;;                 every tag is ABI 15.
  ;;   solidity    : main branch and recent tags all build at ABI 15;
  ;;                 every older commit chosen in testing failed to build
  ;;                 against the recipe's `:source-dir' assumption.
  ;;   typespec    : the recipe's grammar repo has no parser.c at the
  ;;                 expected path; the build step itself dies.
  ;;   verilog     : treesit-auto's verilog recipe builds a .so that
  ;;                 doesn't export `tree_sitter_verilog'. The
  ;;                 tree-sitter-systemverilog fork works but uses a
  ;;                 different language symbol; not a drop-in pin.
  ;;
  ;; If you actually need one of these, add it back in custom.el:
  ;;
  ;;   (with-eval-after-load 'treesit-auto
  ;;     (add-to-list 'treesit-auto-langs 'perl))
  ;;
  ;; and override its recipe with `:url' / `:revision' / `:source-dir'
  ;; as needed.
  (setq treesit-auto-langs
        (cl-set-difference
         treesit-auto-langs
         '(bibtex cobol commonlisp gitcommit magik nu
                  perl solidity typespec verilog)))

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
