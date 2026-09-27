;;; chadmacs-core.el --- Sane defaults, persistence, optimization -*- lexical-binding: t; -*-
;;
;; Commentary:
;;   Core defaults that should hold before any UI or package is initialised:
;;   coding system, GUI chrome, modern editing knobs, persistence (every
;;   state file under var/, via no-littering), and the small set of
;;   optimisation packages (gcmh, exec-path-from-shell).
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

;; Graphical-only setup. A daemon runs its init files before any frame
;; exists, so `display-graphic-p' checked at load time is always nil there
;; and GUI-only setup never reached `emacsclient -c' frames. Route such
;; setup through this instead.
(defun my/when-graphical (fn)
  "Call FN now if the selected frame is graphical, else on the first one.
Under a daemon, init runs without any graphical frame; FN then runs from
`server-after-make-frame-hook' once the first graphical client frame
exists."
  (if (display-graphic-p)
      (funcall fn)
    (letrec ((once (lambda ()
                     (when (display-graphic-p)
                       (remove-hook 'server-after-make-frame-hook once)
                       (funcall fn)))))
      (add-hook 'server-after-make-frame-hook once))))

;; Mouse in terminal frames. `xterm-mouse-mode' only acts on text
;; terminals, so it is enabled unconditionally - a load-time
;; `display-graphic-p' check skipped it under a daemon, leaving
;; `emacsclient -t' frames without mouse support. The wheel needs no extra
;; bindings: mwheel handles it (xterm-mouse reports wheel-up / wheel-down).
(xterm-mouse-mode 1)

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

;; Was Emacs started with a file to visit? `command-line-args' still holds
;; the original arguments late in startup (C already stripped -nw, -Q, ...),
;; but a bare "is there an argument without a leading dash" test is wrong:
;; `emacs --eval EXPR', `emacs -l init.el' or `emacs -f magit-status' pass
;; a non-dash argument too. Walk the list like `command-line-1' does.
(defconst my/command-line-options-with-arg
  '("f" "funcall" "e" "eval" "execute" "L" "directory" "l" "load"
    "scriptload" "scripteval" "insert" "dump-file" "seccomp")
  "Command-line options (without leading dashes) whose next argument is a
value, not a file.")

(defun my/command-line-files-p ()
  "Return non-nil if Emacs was started with a file to visit.
Skips the values of options such as --eval, -l and -f."
  (let ((args (cdr command-line-args))
        found)
    (while (and args (not found))
      (let ((arg (pop args)))
        (cond
         ;; After `--' every argument is a file.
         ((equal arg "--") (setq found (and args t)))
         ;; --file / --find-file / --visit FILE
         ((string-match-p "\\`--?\\(?:find-file\\|file\\|visit\\)\\(=\\|\\'\\)" arg)
          (setq found (or (string-search "=" arg) (and args t))))
         ((string-prefix-p "-" arg)
          ;; --opt=VALUE carries its value inline; otherwise skip the next
          ;; argument when the option takes one.
          (when (and (not (string-search "=" arg))
                     (member (string-trim-left arg "-+")
                             my/command-line-options-with-arg))
            (pop args)))
         ;; +LINE[:COLUMN] positions the file that follows.
         ((string-match-p "\\`\\+[0-9]" arg))
         (t (setq found t)))))
    found))

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
Detection uses `my/command-line-files-p' rather than the buffer list,
since this may run before the file is visited."
  (when (and (not (daemonp))
             (my/command-line-files-p))
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
;;   `projectile-find-file'. Type "index.html" -> matches
;;   "client/index.html". Input that matches no file visits it as a new
;;   file relative to the project root, so C-x C-f still creates files.
;;   (`consult-projectile-find-file' silently ignores such input.)
;; - Outside a project: regular `find-file' so you can browse the
;;   filesystem directory by directory.
(defun my/smart-find-file (&optional arg)
  "Find file: project-wide flat fuzzy list inside a project, `find-file' otherwise.
Inside a project, input that matches no existing file creates that file
relative to the project root.  With a prefix argument ARG
(\\[universal-argument]), always use plain `find-file' so you can escape
the project context and browse the filesystem freely."
  (interactive "P")
  (let* ((dir (or default-directory (expand-file-name "~/")))
         (root (or (and (fboundp 'projectile-project-root)
                        (ignore-errors (projectile-project-root dir)))
                   (locate-dominating-file dir ".projectile")
                   (locate-dominating-file dir ".git"))))
    (if (and (not arg) root (fboundp 'projectile-find-file))
        (let ((default-directory (expand-file-name root)))
          (call-interactively #'projectile-find-file))
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

;; no-littering decides where every package keeps its state (minibuffer
;; history, recentf, save-place, bookmarks, tramp, transient, projectile,
;; eshell, url, ...) - all under var/. It `setq's those paths the moment
;; it loads, so it must be loaded *before* any mode reads its file below.
;; Under Elpaca a plain `use-package' only loads after init, which let
;; savehist/recentf read one file and later save to another: every
;; session started with empty history. `:ensure (:wait t)' makes Elpaca
;; install and load it right here instead.
(use-package no-littering
  :ensure (:wait t)
  :demand t
  :init
  (setq no-littering-etc-directory (expand-file-name "etc/" my/var-dir)
        no-littering-var-directory my/var-dir))

;; Backups (~) and auto-saves (#) - no-littering leaves these alone
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backup/" my/var-dir))))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save/" my/var-dir) t)))

;; Backup retention policy
(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Minibuffer history, recent files, cursor positions (files under var/,
;; placed by no-littering above)
(savehist-mode 1)
(save-place-mode 1)

;; Keep Chadmacs' own state files (treemacs persist, bookmarks, ...) out of
;; the recent-files list. Added before the mode starts so its startup
;; cleanup also drops entries recorded by earlier sessions.
(require 'recentf)
(add-to-list 'recentf-exclude
             (concat "\\`" (regexp-quote (recentf-expand-file-name my/var-dir))))
(recentf-mode 1)

;; Save bookmarks on every change
(setq bookmark-save-flag 1)

;; Tramp's remote shell history (no-littering doesn't redirect this one)
(setq tramp-histfile-override (expand-file-name "tramp-history" my/var-dir))

;; Yasnippet user-snippet dir (consumed by chadmacs-editing.el)
(defvar my/yas-user-snippet-dir (expand-file-name "snippets" my/var-dir))

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
;; Chadmacs applies these after this file loads (a plain set-face-attribute
;; here would be overridden). :height is 1/10 pt (140 = 14pt). With
;; my/font left nil, the first installed JetBrainsMono Nerd Font variant
;; is used. Point it at a font installed on your system, then uncomment:
;; (setq my/font \"JetBrainsMono Nerd Font\"
;;       my/font-height 140)

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

(provide 'chadmacs-core)
;;; chadmacs-core.el ends here
