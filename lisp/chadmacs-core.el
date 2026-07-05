;;; chadmacs-core.el --- Sane defaults, persistence, optimization -*- lexical-binding: t; -*-
;;
;; Commentary:
;;   Core defaults that should hold before any UI or package is initialised:
;;   coding system, GUI chrome, modern editing knobs, persistence redirects
;;   (everything under ~/.emacs.d/var/), and the small set of optimisation
;;   packages (gcmh, exec-path-from-shell, no-littering).
;;
;;; Code:

;; ---------------------------------------------------------------- Defaults --

;; Initial buffer behaviour
(setq initial-buffer-choice nil)
(setq frame-title-format nil)

;; No GUI dialogs
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq pop-up-windows nil)

;; Minor visual cleanups
(setq indicate-empty-lines nil)
(setq cursor-in-non-selected-windows nil)

;; Fill / editing
(setq fill-column 80)
(setq confirm-nonexistent-file-or-buffer nil)

;; Org
(setq org-return-follows-link t)

;; Mouse in terminal
(unless (display-graphic-p)
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

;; Disable all GUI chrome
(tool-bar-mode -1)
(menu-bar-mode -1)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; y/n instead of yes/no
(setq use-short-answers t)
(setq confirm-kill-emacs 'y-or-n-p)

;; Tabs -> spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Modern editing defaults
(setq mouse-yank-at-point t)
(setq sentence-end-double-space nil)
(setq require-final-newline t)
(setq vc-follow-symlinks t)
(setq recenter-positions '(middle top bottom))

;; Auto-revert buffers when files change on disk
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; Highlight current line
(global-hl-line-mode 1)

;; Remember window layout changes (C-c <left> / C-c <right>)
(winner-mode 1)

;; Temporary buffers
(temp-buffer-resize-mode 1)
(setq temp-buffer-max-height 8)

;; Minimum window height
(setq window-min-height 1)

;; Safeguard: a file opened on the command line should own the whole frame.
;; Startup can pop up a second window before the file is shown - a warning or
;; error buffer (a broken custom.el, a package's `display-warning', ...), a
;; restored session, and so on - which shoves the file you launched into a
;; tiny split under *scratch*. Once startup settles, if Emacs was started with
;; a file argument, give that file the whole frame. This does NOT suppress the
;; warning: it still lands in *Warnings* for you to read.
(defun my/cli-file-take-full-frame ()
  "Give a file opened on the command line the entire frame.
No-op for a bare `emacs' launch (no file argument) and under a daemon.
Detection uses `command-line-args' (which still holds the filename here)
rather than the buffer list, since this may run before the file is visited."
  (when (and (not (daemonp))
             (seq-some (lambda (arg) (not (string-prefix-p "-" arg)))
                       (cdr command-line-args)))
    (let ((file-windows (seq-filter (lambda (w) (buffer-file-name (window-buffer w)))
                                    (window-list))))
      ;; Only act when exactly one file window is showing: that is the
      ;; tell-tale of an unwanted split (file + *scratch* / *Warnings* / ...).
      ;; Two or more file windows means an intentional multi-file open
      ;; (`emacs a.txt b.txt') - leave that arrangement alone.
      (when (= (length file-windows) 1)
        ;; Hide side windows first (popup managers like popper, treemacs, ...
        ;; display in side windows, which `delete-other-windows' refuses to
        ;; collapse - "Cannot make side window the only window"). Then make the
        ;; file's buffer the sole normal window. Grab the buffer before toggling
        ;; side windows, since that can invalidate the window object.
        (let ((buf (window-buffer (car file-windows))))
          (ignore-errors (window-toggle-side-windows))
          (switch-to-buffer buf)
          (ignore-errors (delete-other-windows)))))))

;; Run on a short idle timer so it fires after session restore, asynchronous
;; package `:config' blocks, and any warning popups have settled. Trigger from
;; both `emacs-startup-hook' (always fires, right after command-line files are
;; visited) and `elpaca-after-init-hook' (a backstop for anything Elpaca pops
;; up later). Running twice is harmless - it's a no-op once the file is alone.
(defun my/cli-file-take-full-frame-soon ()
  "Schedule `my/cli-file-take-full-frame' once Emacs goes idle."
  (run-with-idle-timer 0.3 nil #'my/cli-file-take-full-frame))
(add-hook 'emacs-startup-hook     #'my/cli-file-take-full-frame-soon)
(add-hook 'elpaca-after-init-hook #'my/cli-file-take-full-frame-soon)

;; UTF-8 everywhere
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment   'utf-8)

;; Unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse
      uniquify-separator " • "
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

;; Kill terminal buffer when the process exits
(defun my/term-sentinel-kill-buffer (orig-fn proc msg)
  "Kill terminal buffer when the process exits."
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        (funcall orig-fn proc msg)
        (kill-buffer buffer))
    (funcall orig-fn proc msg)))
(advice-add 'term-sentinel :around #'my/term-sentinel-kill-buffer)

;; Kill current buffer (instead of asking first buffer name)
(global-set-key (kbd "C-x k") 'kill-current-buffer)

;; M-n for new frame (M-n is unbound in vanilla emacs)
(defun new-frame ()
  "\\<M-n> for new frame (\\<M-n> is unbound in vanilla Emacs)."
  (interactive)
  (select-frame (make-frame))
  (switch-to-buffer "*scratch*"))
(global-set-key (kbd "M-n") 'new-frame)
(global-set-key (kbd "M-`") 'other-frame)
(global-set-key (kbd "<M-return>") #'toggle-frame-fullscreen)

;; Open recent files
(global-set-key (kbd "C-x C-r") 'consult-recent-file)

;; Smart find-file:
;; - In a project (detected by walking up for .projectile, .git, or any
;;   Projectile root): flat fuzzy list of every file via
;;   `consult-projectile-find-file'. Type "index.html" -> matches
;;   "client/index.html".
;; - Outside a project: regular `find-file' so you can browse the
;;   filesystem directory by directory.
(defun my/smart-find-file (&optional arg)
  "Find file: project-wide flat fuzzy list inside a project, `find-file' otherwise.
With a prefix argument ARG (\\[universal-argument]), always use plain
`find-file' so you can escape the project context and browse the
filesystem freely."
  (interactive "P")
  (let* ((dir (or default-directory (expand-file-name "~/")))
         (root (or (and (fboundp 'projectile-project-root)
                        (ignore-errors (projectile-project-root dir)))
                   (locate-dominating-file dir ".projectile")
                   (locate-dominating-file dir ".git"))))
    (if (and (not arg) root (fboundp 'consult-projectile-find-file))
        (let ((default-directory (expand-file-name root)))
          (call-interactively #'consult-projectile-find-file))
      (call-interactively #'find-file))))
(global-set-key (kbd "C-x C-f") #'my/smart-find-file)

;; Pure-Emacs / canonical-third-party keybindings.
;;
;; - C-x g / C-x M-g     Magit: status / dispatch (magit's documented binds)
;; - C-x C-b             ibuffer instead of vanilla list-buffers (de facto
;;                       pure-Emacs upgrade since forever)
;; - C-c a / C-c c / C-c l   Org triad: agenda, capture, store-link
;;                       (https://orgmode.org/manual/Activation.html)
;; - M-/                 completion-at-point (so corfu/cape capfs hit it
;;                       from a stock-feeling key)
(global-set-key (kbd "C-x g")   'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c a")   'org-agenda)
(global-set-key (kbd "C-c c")   'org-capture)
(global-set-key (kbd "C-c l")   'org-store-link)
(global-set-key (kbd "M-/")     'completion-at-point)

;; Grab keyboard focus when the first frame is created. Some window managers
;; (especially under tiling Wayland/X compositors) leave the new Emacs frame
;; unfocused after launch.
(add-hook 'window-setup-hook
          (lambda ()
            (when (display-graphic-p)
              (select-frame-set-input-focus (selected-frame)))))

;; Print init time + GC count when Emacs finishes starting. Lets you spot
;; regressions after editing config (typical: ~1.5-3s with cold cache).
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Chadmacs ready in %s with %d GCs"
                     (emacs-init-time) gcs-done)))

;; ------------------------------------------------------------ Persistence --

;; Backups (~)
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backup/" my/var-dir))))

;; Auto-saves (#)
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save/" my/var-dir) t)))
(setq auto-save-list-file-prefix
      (expand-file-name "auto-save/.saves-" my/var-dir))

;; Savehist (minibuffer history)
(setq savehist-file (expand-file-name "history" my/var-dir))
(savehist-mode 1)

;; Recentf (recent files/places)
(setq recentf-save-file (expand-file-name "recentf" my/var-dir))
(recentf-mode 1)

;; Save-place (cursor positions in files)
(setq save-place-file (expand-file-name "places" my/var-dir))
(save-place-mode 1)

;; Bookmarks
(setq bookmark-default-file (expand-file-name "bookmarks" my/var-dir))
(setq bookmark-save-flag 1)

;; Tramp (remote connections)
(setq tramp-persistency-file-name (expand-file-name "tramp" my/var-dir))
(setq tramp-histfile-override (expand-file-name "tramp-history" my/var-dir))

;; Project list
(setq project-list-file (expand-file-name "projects" my/var-dir))

;; undo-fu session
(setq undo-fu-session-directory
      (expand-file-name "undo-fu-session/" my/var-dir))

;; Eshell
(setq eshell-directory-name (expand-file-name "eshell/" my/var-dir))

;; Transient persistence files
(let ((transient-dir (expand-file-name "transient/" my/var-dir)))
  (setq transient-history-file (expand-file-name "history.el" transient-dir)
        transient-levels-file  (expand-file-name "levels.el" transient-dir)
        transient-values-file  (expand-file-name "values.el" transient-dir)))

;; Projectile state
(setq projectile-cache-file
      (expand-file-name "projectile.cache" my/var-dir))
(setq projectile-known-projects-file
      (expand-file-name "projectile-bookmarks.eld" my/var-dir))

;; Yasnippet user-snippet dir (consumed by chadmacs-editing.el)
(defvar my/yas-user-snippet-dir (expand-file-name "snippets" my/var-dir))

;; Catch-all redirects for other state files that some packages write to
;; ~/.emacs.d/ by default. Setting them up front prevents future leaks.
(setq url-configuration-directory (expand-file-name "url/" my/var-dir))
(setq nsm-settings-file           (expand-file-name "network-security.data" my/var-dir))
(setq package-quickstart-file     (expand-file-name "package-quickstart.el" my/var-dir))
(with-eval-after-load 'request
  (setq request-storage-directory (expand-file-name "request/" my/var-dir)))

;; Backup retention policy
(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Make the lang directory loadable (language-specific files)
(add-to-list 'load-path (expand-file-name "lang" user-emacs-directory))

(defconst my/custom-file-template
  ";;; custom.el --- Your Chadmacs overrides -*- no-byte-compile: t; lexical-binding: t; -*-
;;;
;;; chadmacs --- Your own config files
;;;
;;; Commentary:
;;; This file is GITIGNORED. Put your personal settings, package overrides,
;;; and the list of language modules you want here. Pulling new Chadmacs
;;; versions will never touch it -> no merge conflicts.
;;;
;;; To re-generate from scratch: delete this file and restart Emacs (or
;;; run `chadmacs doctor --reset-custom').
;;;
;;; Code:

;; ============================================================
;; Language modules
;; ============================================================
;; Each line maps to lang/<name>-lang.el. Uncomment to enable
;; the language. Each module's header comment lists the LSP binary you
;; need to install on your system for completion / errors / rename to work.

;; --- Web stack ---
;; (require 'typescript-lang)     ;; TS / TSX / JS / JSX
;; (require 'web-lang)            ;; HTML / CSS / SCSS / Vue / Svelte / Emmet
;; (require 'json-lang)           ;; JSON / JSONC
;; (require 'yaml-lang)           ;; YAML

;; --- Backend / scripting ---
;; (require 'python-lang)         ;; Python (+ optional pyvenv)
;; (require 'go-lang)             ;; Go (+ go.mod)
;; (require 'ruby-lang)           ;; Ruby / Rails / Gemfile / Rakefile
;; (require 'elixir-lang)         ;; Elixir + HEEx (Phoenix templates)
;; (require 'lua-lang)            ;; Lua

;; --- Systems / native ---
;; (require 'rust-lang)           ;; Rust + rustic + cargo
;; (require 'c-cpp-lang)          ;; C / C++ + meson + cmake
;; (require 'csharp-lang)         ;; C# / .NET
;; (require 'zig-lang)            ;; Zig
;; (require 'swift-lang)          ;; Swift

;; --- JVM ---
;; (require 'scala-lang)          ;; Scala / sbt
;; (require 'kotlin-lang)         ;; Kotlin

;; --- Functional ---
;; (require 'haskell-lang)        ;; Haskell + Cabal
;; (require 'ocaml-lang)          ;; OCaml + Dune
;; (require 'clojure-lang)        ;; Clojure + CIDER + clj-refactor

;; --- Scientific ---
;; (require 'julia-lang)          ;; Julia + LanguageServer.jl

;; --- DevOps / infra ---
;; (require 'docker-lang)         ;; Dockerfile (compose via yaml)
;; (require 'terraform-lang)      ;; Terraform / HCL
;; (require 'nix-lang)            ;; Nix

;; --- Docs / writing ---
;; (require 'markdown-lang)       ;; Markdown / GFM (+ marksman)

;; --- Niche ---
;; (require 'gerbil-lang)         ;; Gerbil Scheme

;; ============================================================
;; Optional feature toggles
;; ============================================================

;; Ghostel-IME: only needed if you use an Emacs Lisp input method
;; (Korean Hangul via `M-x set-input-method', etc). OS-level IMEs
;; (fcitx / ibus / macOS / Windows) work in Ghostel without this.
;; (setq my/enable-ghostel-ime t)

;; ============================================================
;; Your personal overrides go below
;; ============================================================
;; e.g. (setq user-full-name \"Jane Doe\")

;; --- Font ---
;; set-face-attribute's 2nd arg is the FRAME (nil = all frames); the font
;; itself goes through :font, and :height is 1/10 pt (140 = 14pt). Point it
;; at a font installed on your system, then uncomment:
;; (set-face-attribute 'default nil :font \"JetBrainsMono Nerd Font\" :height 140)

;; --- Theme ---
;; Chadmacs enables doom-monokai-pro. To use a different theme, load it AFTER
;; Elpaca finishes init - otherwise the default loads afterwards (Elpaca is
;; asynchronous) and overwrites yours. Disable any enabled theme first for a
;; clean swap. Every doom-theme ships with Chadmacs (e.g. doom-one):
;; (add-hook 'elpaca-after-init-hook
;;           (lambda ()
;;             (mapc #'disable-theme custom-enabled-themes)
;;             (load-theme 'doom-one t)))

"
  "Bootstrap content for custom.el on first launch.")

(defun create-or-load-custom-file ()
  "Load the custom.el file (gitignored) if it exists or create if it doesn't.
Custom.el is seeded with the language-module activation template so users
can enable per-language support without touching upstream files."
  (unless (file-exists-p custom-file)
    (with-temp-file custom-file
      (insert my/custom-file-template)))
  (load custom-file t t))

;; ----------------------------------------------------------- Optimization --

;; Skip theme-load confirmation prompt
;; (Elpaca's own elpaca-use-package-by-default is set in early-init.el to
;; default every use-package form to ensure-via-Elpaca.)
(setq custom-safe-themes t)

;; Adaptive GC - defers garbage collection to idle, smoother typing
(use-package gcmh
  :ensure t
  :demand t
  :config
  (gcmh-mode 1))

;; Inherit shell PATH into Emacs.
;;
;; GUI Emacs launched from a .desktop file (Linux X / Wayland) or from
;; Finder (macOS) doesn't source ~/.bashrc / ~/.zshrc / config.fish, so
;; per-user tool directories (~/.bun/bin, ~/.cargo/bin, ~/.local/bin,
;; nvm shims, asdf shims, ...) are missing from `exec-path'. Without
;; this, eglot reports "No such file or directory" for LSPs that the
;; shell can see fine. Daemon Emacs has the same issue.
(use-package exec-path-from-shell
  :ensure t
  :if (or (memq window-system '(mac ns x pgtk)) (daemonp))
  :defer 1
  :config
  (exec-path-from-shell-initialize))

;; Standardize package state paths under var/ (used by future packages)
(use-package no-littering
  :ensure t
  :demand t
  :init
  (setq no-littering-etc-directory (expand-file-name "etc/" my/var-dir)
        no-littering-var-directory my/var-dir))

(provide 'chadmacs-core)
;;; chadmacs-core.el ends here
