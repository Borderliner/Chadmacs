;;; chadmacs-ui.el --- Frame, theme, modeline, dashboard -*- lexical-binding: t; -*-
;;
;; Commentary:
;;   Visual layer: frame parameters and padding, fallback glyphs, divider,
;;   nano-modeline, doom-themes (Monokai Pro default), dashboard, smooth
;;   scrolling, nerd-icons, diff-hl, page-break-lines, which-key.
;;
;;; Code:

;; --------------------------------------------------------- Frame & layout --

;; Apply to all new frames (including the initial one if in early-init.el)
(add-to-list 'default-frame-alist '(min-height            . 1))
(add-to-list 'default-frame-alist '(height                . 45))
(add-to-list 'default-frame-alist '(min-width             . 1))
(add-to-list 'default-frame-alist '(width                 . 100))
(add-to-list 'default-frame-alist '(internal-border-width . 18))
(add-to-list 'default-frame-alist '(left-fringe           . 1))
(add-to-list 'default-frame-alist '(right-fringe          . 1))

;; Slim fringes
(setq-default left-fringe-width  1)
(setq-default right-fringe-width 1)

;; Fallback glyph for truncation/wrap (clean look). Family is intentionally
;; left unset; the actual JetBrainsMono variant is detected at theme-load
;; time below and applied to the default face. Inheriting from
;; font-lock-comment-face is enough to render the glyph in the right tone.
(require 'disp-table)
(defface my-fallback-glyph
  '((t :inherit font-lock-comment-face))
  "Fallback face for truncation and wrap glyphs.")

(when (display-graphic-p)
  (set-display-table-slot standard-display-table 'truncation
                          (make-glyph-code ?… 'my-fallback-glyph))
  (set-display-table-slot standard-display-table 'wrap
                          (make-glyph-code ?↩ 'my-fallback-glyph)))

;; No ugly widget buttons (nice with custom themes)
(setq widget-image-enable nil)

;; Hide org emphasis markers in buffers (clean reading)
(setq org-hide-emphasis-markers t)

;; Display line numbers globally
(global-display-line-numbers-mode 1)

;; Slim vertical divider on right only
(setq window-divider-default-right-width 1)
(setq window-divider-default-places 'right-only)
(window-divider-mode 1)

;; ------------------------------------------------------------- Modeline --

;; doom-modeline replaces the default modeline (and nano-modeline before
;; it). Cleaner layout, nerd-icons integration, projectile / VC / LSP /
;; flycheck segments out of the box, and no "function not known to be
;; defined" noise for optional integrations.
;;
;; `:demand t' + explicit `(doom-modeline-mode 1)' in :config is required
;; with Elpaca: hooking onto `after-init-hook' is too late because
;; Elpaca processes its install queue at that hook, so on a fresh boot
;; doom-modeline isn't loaded yet when after-init fires - the modeline
;; stays the default until the *next* restart. Activating in :config
;; runs after Elpaca finishes installing on the same boot.
(use-package doom-modeline
  :ensure t
  :demand t
  :custom
  (doom-modeline-height 28)
  (doom-modeline-bar-width 3)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count nil)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-vcs-max-length 12)
  (doom-modeline-workspace-name nil)
  (doom-modeline-lsp t)
  (doom-modeline-time nil)
  (doom-modeline-percent-position nil)
  (doom-modeline-buffer-encoding nil)
  :config
  ;; Safety: if nano-modeline still lingers from a previous install,
  ;; turn it off so it doesn't fight doom-modeline for the header line.
  (when (fboundp 'nano-modeline-mode)
    (nano-modeline-mode -1))
  (doom-modeline-mode 1))

;; --------------------------------------------------------------- Theme --

(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-monokai-pro t)
  (doom-themes-treemacs-config)
  (doom-themes-org-config)
  ;; Pick the first installed JetBrainsMono variant. Different packagers
  ;; use different family names - nerd-fonts upstream calls it
  ;; "JetBrainsMono Nerd Font", but Ubuntu's apt package and Homebrew's
  ;; tap label it "JetBrainsMono NF", and a few packagers strip spaces.
  (when (display-graphic-p)
    (let ((font (seq-find (lambda (f) (find-font (font-spec :name f)))
                          '("JetBrainsMono Nerd Font"
                            "JetBrainsMono NF"
                            "JetBrainsMonoNerdFont"
                            "JetBrains Mono Nerd Font"
                            "JetBrainsMono Nerd Font Mono"))))
      (when font
        (set-face-attribute 'default nil :font font :height 110)))))

;; ----------------------------------------------------------- Dashboard --

(use-package dashboard
  :ensure t
  :after projectile
  :init
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-banner-logo-title (concat "Welcome, " (user-full-name) "!"))
  (setq dashboard-startup-banner 2)
  (setq dashboard-center-content t)
  (setq dashboard-vertically-center-content t)
  (setq dashboard-navigation-cycle t)
  (setq dashboard-show-shortcuts t)
  (setq dashboard-heading-shorcut-format " [shortcut: %s]")
  (setq dashboard-display-icons-p t)
  (setq dashboard-icon-type 'nerd-icons)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-startupify-list '(dashboard-insert-banner
                                    dashboard-insert-newline
                                    dashboard-insert-banner-title
                                    dashboard-insert-newline
                                    dashboard-insert-navigator
                                    dashboard-insert-newline
                                    dashboard-insert-init-info
                                    dashboard-insert-items
                                    dashboard-insert-newline))
  (setq dashboard-items '((recents   . 5)
                          (bookmarks . 5)
                          (projects  . 5)
                          (agenda    . 5)
                          (registers . 5)))
  (setq dashboard-item-shortcuts '((recents   . "r")
                                   (bookmarks . "m")
                                   (projects  . "p")
                                   (agenda    . "a")
                                   (registers . "e")))
  :config
  (dashboard-setup-startup-hook))

;; --------------------------------------------------- Scrolling & icons --

;; ultra-scroll: precise pixel-by-pixel scrolling for trackpads and mouse
;; wheels. Lives on GitHub only (not MELPA), so the recipe is pinned
;; explicitly. Internally drives `pixel-scroll-precision-mode' - do NOT
;; enable that mode separately or the two will fight each other.
;;
;; `scroll-conservatively' MUST be > 100 for ultra-scroll to feel right:
;; any value <= 100 lets Emacs recenter the buffer when point scrolls
;; off-screen, which produces a jarring snap on top of the smooth pixel
;; scroll. The upstream README flags this as the #1 gotcha. We set 101
;; (the canonical "disable recentering" sentinel).
;;
;; ultra-scroll auto-disables on TTY (no graphics) so it's safe to enable
;; unconditionally; the mode is a no-op in terminal Emacs.
(use-package ultra-scroll
  :ensure (ultra-scroll :host github :repo "jdtsmith/ultra-scroll")
  :init
  (setq scroll-conservatively 101
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

;; Nerd icons in completion (looks great with nano)
(use-package nerd-icons-completion
  :ensure t
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)

  ;; nerd-icons-completion has hardcoded icon names (e.g. `nf-oct-folder_cache')
  ;; that drift when upstream nerd-icons renames or removes glyphs. Without
  ;; this advice, opening `find-file' in a directory containing `.cache/'
  ;; (and a few other names) raises and breaks vertico. Swallow the lookup
  ;; error and return a blank string so completion still renders.
  (dolist (fn '(nerd-icons-octicon
                nerd-icons-faicon
                nerd-icons-codicon
                nerd-icons-mdicon
                nerd-icons-devicon
                nerd-icons-sucicon
                nerd-icons-flicon
                nerd-icons-wicon
                nerd-icons-pomicon
                nerd-icons-ipsicon))
    (when (fboundp fn)
      (advice-add fn :around
                  (lambda (orig &rest args)
                    (condition-case nil
                        (apply orig args)
                      (error "")))))))

;; ---------------------------------------------------- Diff-hl & breaks --

;; Git diff showing in the fringe
(use-package diff-hl
  :ensure t
  :commands (diff-hl-mode global-diff-hl-mode)
  :hook
  (prog-mode . diff-hl-mode)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :init
  (setq diff-hl-flydiff-delay 0.4)
  (setq diff-hl-show-staged-changes nil)
  (setq diff-hl-update-async t)
  (setq diff-hl-global-modes '(not pdf-view-mode image-mode)))

;; Visible indicators for page breaks
(use-package page-break-lines
  :ensure t
  :commands (page-break-lines-mode global-page-break-lines-mode)
  :hook
  (lisp-data-mode . page-break-lines-mode))

;; ----------------------------------------------------------- which-key --

;; Popup showing key completions after a prefix (built-in on Emacs 30+)
(use-package which-key
  :ensure t
  :demand t
  :custom
  (which-key-idle-delay 0.4)
  (which-key-popup-type 'minibuffer)
  :config
  (which-key-mode 1))

(provide 'chadmacs-ui)
;;; chadmacs-ui.el ends here
