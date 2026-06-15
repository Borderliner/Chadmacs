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

;; Fallback glyph for truncation/wrap (clean look)
(require 'disp-table)
(defface my-fallback-glyph
  '((t :family "JetBrainsMono Nerd Font"
       :inherit font-lock-comment-face))
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

(use-package nano-modeline
  :ensure t
  :config
  (add-hook 'prog-mode-hook            #'nano-modeline-prog-mode)
  (add-hook 'text-mode-hook            #'nano-modeline-text-mode)
  (add-hook 'org-mode-hook             #'nano-modeline-org-mode)
  (add-hook 'xwidget-webkit-mode-hook  #'nano-modeline-xwidget-mode)
  (add-hook 'messages-buffer-mode-hook #'nano-modeline-message-mode)
  (add-hook 'org-capture-mode-hook     #'nano-modeline-org-capture-mode)
  (add-hook 'org-agenda-mode-hook      #'nano-modeline-org-agenda-mode)
  (setq nano-modeline-position 'nano-modeline-header))

;; --------------------------------------------------------------- Theme --

(use-package doom-themes
  :ensure t
  :after nano-modeline
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-monokai-pro t)
  (doom-themes-treemacs-config)
  (doom-themes-org-config)
  (when (and (display-graphic-p)
             (find-font (font-spec :name "JetBrainsMono Nerd Font")))
    (set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 110))

  ;; Sync nano-modeline faces with current doom-theme (better contrast for
  ;; doom-monokai-pro).
  (defun my/nano-modeline-sync-simple ()
    "Sync nano-modeline faces with current doom-theme."
    (when (fboundp 'doom-color)
      (let ((bg-main    (doom-color 'bg))
            (bg-modeline (doom-color 'bg-alt))
            (fg-main     (doom-color 'fg))
            (fg-dim      (doom-color 'grey))
            (accent      (doom-color 'yellow))
            (status-bg   (doom-darken (doom-color 'bg-alt) 0.12)))
        (custom-set-faces
         `(nano-modeline-active
           ((t (:background ,bg-modeline
                            :foreground ,accent
                            :box (:line-width 1 :color ,bg-main)))))
         `(nano-modeline-inactive
           ((t (:background ,(doom-darken bg-modeline 0.08)
                            :foreground ,fg-dim
                            :box (:line-width 1 :color ,bg-main)))))
         `(nano-modeline-status
           ((t (:foreground ,accent
                            :background ,status-bg
                            :weight bold
                            :box (:line-width 1 :color ,status-bg)))))))))

  (add-hook 'doom-load-theme-hook #'my/nano-modeline-sync-simple)
  (when (fboundp 'doom-color)
    (my/nano-modeline-sync-simple)))

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

(use-package ultra-scroll
  :ensure t
  :init
  (setq scroll-conservatively 3
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
