;;; chadmacs-navigation.el --- File explorers, popups, terminal -*- lexical-binding: t; -*-
;;
;; Commentary:
;;   File / project navigation tools and the popup/terminal trifecta:
;;     - Treemacs (sidebar tree)
;;     - Dirvish (dired replacement)
;;     - Popper (popup window manager)
;;     - Ghostel (ghostty-vt terminal)
;;
;;; Code:

;; --------------------------------------------------------- Treemacs --

(use-package treemacs
  :ensure t
  :commands (treemacs
             treemacs-select-window
             treemacs-delete-other-windows
             treemacs-select-directory
             treemacs-bookmark
             treemacs-find-file
             treemacs-find-tag)
  ;; Treemacs prefix moved to C-c T to avoid clobbering tab-bar's C-x t map
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-c T 1"   . treemacs-delete-other-windows)
        ("C-c T t"   . treemacs)
        ("C-c T d"   . treemacs-select-directory)
        ("C-c T B"   . treemacs-bookmark)
        ("C-c T C-t" . treemacs-find-file)
        ("C-c T M-t" . treemacs-find-tag))
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
        treemacs-deferred-git-apply-delay        0.5
        treemacs-directory-name-transformer      #'identity
        treemacs-display-in-side-window          t
        treemacs-eldoc-display                   'simple
        treemacs-file-event-delay                2000
        treemacs-file-extension-regex            treemacs-last-period-regex-value
        treemacs-file-follow-delay               0.2
        treemacs-file-name-transformer           #'identity
        treemacs-follow-after-init               t
        treemacs-expand-after-init               t
        treemacs-find-workspace-method           'find-for-file-or-pick-first
        treemacs-git-command-pipe                ""
        treemacs-goto-tag-strategy               'refetch-index
        treemacs-header-scroll-indicators        '(nil . "^^^^^^")
        treemacs-hide-dot-git-directory          t
        treemacs-indentation                     2
        treemacs-indentation-string              " "
        treemacs-is-never-other-window           nil
        treemacs-max-git-entries                 5000
        treemacs-missing-project-action          'ask
        treemacs-move-files-by-mouse-dragging    t
        treemacs-move-forward-on-expand          nil
        treemacs-no-png-images                   nil
        treemacs-no-delete-other-windows         t
        treemacs-project-follow-cleanup          nil
        treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" my/var-dir)
        treemacs-position                        'left
        treemacs-read-string-input               'from-child-frame
        treemacs-recenter-distance               0.1
        treemacs-recenter-after-file-follow      nil
        treemacs-recenter-after-tag-follow       nil
        treemacs-recenter-after-project-jump     'always
        treemacs-recenter-after-project-expand   'on-distance
        treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
        treemacs-project-follow-into-home        nil
        treemacs-show-cursor                     nil
        treemacs-show-hidden-files               t
        treemacs-silent-filewatch                nil
        treemacs-silent-refresh                  nil
        treemacs-sorting                         'alphabetic-asc
        treemacs-select-when-already-in-treemacs 'move-back
        treemacs-space-between-root-nodes        t
        treemacs-tag-follow-cleanup              t
        treemacs-tag-follow-delay                1.5
        treemacs-text-scale                      nil
        treemacs-user-mode-line-format           nil
        treemacs-user-header-line-format         nil
        treemacs-wide-toggle-width               70
        treemacs-width                           35
        treemacs-width-increment                 1
        treemacs-width-is-initially-locked       t
        treemacs-workspace-switch-cleanup        nil)

  (treemacs-follow-mode t)
  (treemacs-project-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)

  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t) (treemacs-git-mode 'deferred))
    (`(t . _) (treemacs-git-mode 'simple)))

  (treemacs-hide-gitignored-files-mode t))

(use-package treemacs-magit
  :ensure t)

(use-package treemacs-icons-dired
  :ensure t
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile))

;; -------------------------------------------------------- Dirvish --

;; Dirvish - Dired with sidebar, preview, git status, and quick-access.
;; `dirvish-override-dired-mode' makes every dired session use Dirvish.
(use-package dirvish
  :ensure t
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")
     ("c" "~/.config/"                  "Config")
     ("e" "~/.emacs.d/"                 "Emacs config")))
  (dirvish-mode-line-format
   '(:left (sort symlink) :right (omit yank index)))
  (dirvish-attributes
   '(nerd-icons file-time file-size collapse subtree-state vc-state git-msg))
  (delete-by-moving-to-trash t)
  (dired-listing-switches
   "-l --almost-all --human-readable --group-directories-first --no-group")
  :bind
  ;; `dirvish-override-dired-mode' above makes every `dired' session a
  ;; Dirvish session, so the canonical `C-x d' already opens it - no
  ;; extra top-level bind needed.
  (:map dirvish-mode-map
        ("a"   . dirvish-quick-access)
        ("f"   . dirvish-file-info-menu)
        ("y"   . dirvish-yank-menu)
        ("N"   . dirvish-narrow)
        ("^"   . dirvish-history-last)
        ("h"   . dirvish-history-jump)
        ("s"   . dirvish-quicksort)
        ("v"   . dirvish-vc-menu)
        ("TAB" . dirvish-subtree-toggle)
        ("M-f" . dirvish-history-go-forward)
        ("M-b" . dirvish-history-go-backward)
        ("M-l" . dirvish-ls-switches-menu)
        ("M-m" . dirvish-mark-menu)
        ("M-t" . dirvish-layout-toggle)
        ("M-s" . dirvish-setup-menu)
        ("M-e" . dirvish-emerge-menu)
        ("M-j" . dirvish-fd-jump)))

;; -------------------------------------------------------- Popper --

;; Popper turns *Help*, compile, eldoc, *Messages*, async-shell, eshell, etc.
;; into toggleable popups so they stop hijacking your main windows.
(use-package popper
  :ensure t
  ;; `M-`' is already bound to `other-frame' in chadmacs-core, so popper-cycle
  ;; gets `C-M-<` instead of the upstream default.
  :bind (("C-`"   . popper-toggle)
         ("C-M-<" . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*compilation\\*"
          "\\*Warnings\\*"
          "\\*Backtrace\\*"
          "\\*eldoc\\*"
          "\\*ghostel"          ; matches *ghostel*, *ghostel-compile*, *ghostel: DIR*
          help-mode
          helpful-mode
          compilation-mode
          eshell-mode
          shell-mode
          ghostel-mode))
  (popper-mode 1)
  (popper-echo-mode 1))

;; --------------------------------------------------------- Ghostel --

;; Ghostel is a terminal emulator backed by libghostty-vt (the VT engine
;; from the Ghostty terminal). Replaces vterm. Advantages over vterm for
;; this config:
;;
;;   - Native module is a prebuilt binary that auto-downloads on first
;;     `M-x ghostel'. No cmake, libvterm-dev, or 3-minute compile step.
;;   - True color, Kitty keyboard + graphics protocols (image previews
;;     in yazi/tui apps work), hyperlinks, desktop notifications, sync
;;     output - all out of the box.
;;   - Shell integration (directory tracking, prompt navigation) works
;;     for bash / zsh / fish / nushell without any dotfile changes.
;;   - Multiple input modes (semi-char default, char, line, emacs, copy)
;;     so switching between "typing in the terminal" and "reading it
;;     like a normal Emacs buffer" is fluid: `C-c C-t' freezes for copy,
;;     `C-c C-j' back to interactive.
;;
;; Requires Emacs 28.1+ with dynamic module support, on macOS / Linux /
;; FreeBSD. Auto-disabled everywhere else.
(use-package ghostel
  :ensure t
  :commands (ghostel
             ghostel-project
             ghostel-next
             ghostel-previous
             ghostel-list-buffers
             ghostel-project-next
             ghostel-project-previous
             ghostel-project-list-buffers)
  :bind (("C-c v" . ghostel)
         ("C-c V" . my/ghostel-other-window)
         ;; Emacs 28+ project.el hook: `C-x p m' opens a Ghostel buffer
         ;; scoped to the current project; `C-x p M' lists project
         ;; ghostel buffers for switching.
         :map project-prefix-map
         ("m" . ghostel-project)
         ("M" . ghostel-project-list-buffers))
  :init
  (defun my/ghostel-other-window ()
    "Open a Ghostel terminal in another window (splits if there's only one)."
    (interactive)
    (unless (> (length (window-list)) 1)
      (split-window-right))
    (other-window 1)
    (call-interactively #'ghostel))
  :config
  ;; Surface Ghostel entries in the `C-x p p' project-switch dispatcher.
  (with-eval-after-load 'project
    (add-to-list 'project-switch-commands
                 '(ghostel-project "Ghostel") t)
    (add-to-list 'project-switch-commands
                 '(ghostel-project-list-buffers "Ghostel buffers") t)))

;; --- Ghostel extensions (ship inside the ghostel package) ---
;;
;; These are separate use-package forms with `:ensure nil' because
;; installing `ghostel' above already pulls their .el files onto the
;; load-path. Each mode has an autoload cookie, so `:hook' is enough
;; to defer loading until the hook fires.

;; Route `eshell-visual-commands' (tmux, htop, less, ...) into a
;; Ghostel buffer instead of eshell's raw terminfo emulation.
(use-package ghostel-eshell
  :ensure nil
  :after ghostel
  :hook (eshell-load . ghostel-eshell-visual-command-mode))

;; Run `M-x compile' and every compile-command inside a Ghostel buffer.
;; Colored output, escape codes, and progress bars work.
(use-package ghostel-compile
  :ensure nil
  :after ghostel
  :hook (after-init . ghostel-compile-global-mode))

;; Replace comint's `ansi-color-process-output' with Ghostel's VT parser,
;; so every comint buffer (shell, gud, python-shell, sql-*, ...) gets
;; true color + escape handling.
(use-package ghostel-comint
  :ensure nil
  :after ghostel
  :hook (after-init . ghostel-comint-global-mode))

;; --- Ghostel IME (opt-in) ---
;;
;; `ghostel-ime' adds support for Emacs Lisp input methods (Korean
;; Hangul via quail, etc.) inside Ghostel buffers. It has zero effect
;; for users on OS-level IMEs (fcitx / ibus / macOS input source /
;; Windows IME) - those work with Ghostel unconditionally. So we gate
;; it behind an opt-in defvar to keep the default lean.
;;
;; To enable, put in `custom.el':
;;   (setq my/enable-ghostel-ime t)
;; and restart Emacs.
(defvar my/enable-ghostel-ime nil
  "Non-nil to enable `ghostel-ime-mode' in every Ghostel buffer.
Only useful when you use an Emacs Lisp input method (e.g. Korean
Hangul via `M-x set-input-method'). Ghostel handles OS-level IMEs
(fcitx, ibus, macOS input sources, Windows IME) without this.")

(use-package ghostel-ime
  :ensure nil
  :if my/enable-ghostel-ime
  :after ghostel
  :hook (ghostel-mode . ghostel-ime-mode))

(provide 'chadmacs-navigation)
;;; chadmacs-navigation.el ends here
