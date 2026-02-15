;;; init.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-
;;;
;;; chadmacs --- Chadmacs Emacs config files
;;;
;;; Commentary:
;;; Borderliner's Chadmacs Emacs config files, with clean visuals and fast startups
;;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;; Sane Defaults ;;;;;;;;;;;;;;;;;;;;;;;;;
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
(set-scroll-bar-mode nil)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'y-or-n-p)

;; Tabs → spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Temporary buffers
(temp-buffer-resize-mode 1)
(setq temp-buffer-max-height 8)

;; Minimum window height
(setq window-min-height 1)

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
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  "Kill terminal buffer when the process exits."
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

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

;;;;;;;;;;;;;;;;;;;;;;;;;; Persistence and File Management ;;;;;;;;;;;;;;;;;;;;;;;;;

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
(savehist-mode 1)  ; Enable if not already active

;; Recentf (recent files/places)
(setq recentf-save-file (expand-file-name "recentf" my/var-dir))
(recentf-mode 1)  ; Enable if you use recentf

;; Save-place (cursor positions in files)
(setq save-place-file (expand-file-name "places" my/var-dir))
(save-place-mode 1)  ; Enable if you use save-place

;; Bookmarks
(setq bookmark-default-file (expand-file-name "bookmarks" my/var-dir))
(setq bookmark-save-flag 1)

;; Tramp (remote connections)
(setq tramp-persistency-file-name (expand-file-name "tramp" my/var-dir))
(setq tramp-histfile-override (expand-file-name "tramp-history" my/var-dir))  ; For Tramp history specifically

(setq project-list-file (expand-file-name "projects" my/var-dir))

(setq undo-fu-session-directory
      (expand-file-name "undo-fu-session/" my/var-dir))

;; Redirect Eshell to var directory
(setq eshell-directory-name (expand-file-name "eshell/" my/var-dir))

;; Redirect Transient persistence files to var directory
(let ((transient-dir (expand-file-name "transient/" my/var-dir)))
  (setq transient-history-file (expand-file-name "history.el" transient-dir)
        transient-levels-file  (expand-file-name "levels.el" transient-dir)
        transient-values-file  (expand-file-name "values.el" transient-dir)))

(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Optimization ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; compile-angel exclusions — same list
(use-package compile-angel
  :demand t
  :ensure t
  :custom
  (compile-angel-verbose t)

  :config
  (push "/init.el" compile-angel-excluded-files)
  (push "/early-init.el" compile-angel-excluded-files)

  ;; (dolist (file my/nano-files)
  ;;   (unless (member file compile-angel-excluded-files)
  ;;     (push file compile-angel-excluded-files)))

  (compile-angel-on-load-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;; Themes and UI ;;;;;;;;;;;;;;;;;;;;;;;;;
;; Apply to all new frames (including the initial one if in early-init.el)
(add-to-list 'default-frame-alist '(min-height          . 1))
(add-to-list 'default-frame-alist '(height             . 45))   ; adjust to taste
(add-to-list 'default-frame-alist '(min-width           . 1))
(add-to-list 'default-frame-alist '(width               . 100))   ; adjust to taste
(add-to-list 'default-frame-alist '(internal-border-width . 18)) ; ← main padding!
(add-to-list 'default-frame-alist '(left-fringe         . 1))
(add-to-list 'default-frame-alist '(right-fringe        . 1))

;; Optional: if you like the very slim fringes look
(setq-default left-fringe-width  1)
(setq-default right-fringe-width 1)

;; Optional: fallback glyph for truncation/wrap (clean look)
(require 'disp-table)
(defface my-fallback-glyph
  '((t :family "JetBrainsMono Nerd Font"  ; or Fira Code / whatever you use
       :inherit font-lock-comment-face))   ; or any faded face
  "Fallback face for truncation and wrap glyphs.")

(set-display-table-slot standard-display-table 'truncation
                        (make-glyph-code ?… 'my-fallback-glyph))
(set-display-table-slot standard-display-table 'wrap
                        (make-glyph-code ?↩ 'my-fallback-glyph))  ; or ? or ?…

;; =====================================
;; Optional extras you might want
;; =====================================

;; No ugly widget buttons (nice with custom themes)
(setq widget-image-enable nil)

;; Hide org emphasis markers in buffers (clean reading)
(setq org-hide-emphasis-markers t)

;; Vertical divider only on right (if you like it slim)
(setq window-divider-default-right-width 1)   ; was 24 in nano → too thick
(setq window-divider-default-places 'right-only)
(window-divider-mode 1)   ; comment out if you don't want any divider

(use-package nano-modeline
  :ensure t
  :config
  ;; Activate it where it makes sense
  (add-hook 'prog-mode-hook            #'nano-modeline-prog-mode)
  (add-hook 'text-mode-hook            #'nano-modeline-text-mode)
  (add-hook 'org-mode-hook             #'nano-modeline-org-mode)
  (add-hook 'xwidget-webkit-mode-hook  #'nano-modeline-xwidget-mode)
  (add-hook 'messages-buffer-mode-hook #'nano-modeline-message-mode)
  (add-hook 'org-capture-mode-hook     #'nano-modeline-org-capture-mode)
  (add-hook 'org-agenda-mode-hook      #'nano-modeline-org-agenda-mode)

  (setq nano-modeline-position 'nano-modeline-header) ;; or footer
  (nano-modeline-text-mode t)
  )

(use-package doom-themes
  :ensure t
  :after nano-modeline
  :custom
  ;; Optional: Global theme settings for better integration
  (doom-themes-enable-bold t) ; Enable bold fonts
  (doom-themes-enable-italic t) ; Enable italics
  :config
  ;; Load the specific theme (doom-monokai-pro)
  (load-theme 'doom-monokai-pro t)
  ;; Optional: Treemacs integration if you use it later
  (doom-themes-treemacs-config)
  ;; Optional: Org-mode visual tweaks (from doom-themes)
  (doom-themes-org-config)  (set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 110)
  (global-display-line-numbers-mode 1)
  ;; Simple sync with doom-theme colors
  (defun my/nano-modeline-sync-simple ()
    "Sync nano-modeline faces with current doom-theme (better contrast for doom-monokai-pro)."
    (when (fboundp 'doom-color)
      (let ((bg-main    (doom-color 'bg))           ; very dark buffer background
            (bg-modeline (doom-color 'bg-alt))      ; usually a touch lighter/different
            (fg-main     (doom-color 'fg))          ; default text
            (fg-dim      (doom-color 'grey))        ; comments/dimmed text
            (accent      (doom-color 'yellow))      ; bright yellow/orange in Monokai Pro
            (status-bg   (doom-darken (doom-color 'bg-alt) 0.12))) ; darker for status contrast

        (custom-set-faces
         ;; Active header-line: slightly distinct bg + bright text
         `(nano-modeline-active
           ((t (:background ,bg-modeline
                            :foreground ,accent           ; ← yellow/orange accent for text
                            :box (:line-width 1 :color ,bg-main)))))

         ;; Inactive: dim and subdued
         `(nano-modeline-inactive
           ((t (:background ,(doom-darken bg-modeline 0.08)
                            :foreground ,fg-dim
                            :box (:line-width 1 :color ,bg-main)))))

         ;; Status indicators (RO / ** / RW) — make them pop
         `(nano-modeline-status
           ((t (:foreground ,accent              ; yellow text
                            :background ,status-bg
                            :weight bold
                            :box (:line-width 1 :color ,status-bg)))))))))

  ;; Hook & initial run (same as before)
  (add-hook 'doom-load-theme-hook #'my/nano-modeline-sync-simple)
  (when (fboundp 'doom-color)
    (my/nano-modeline-sync-simple))
  )

(use-package dashboard
  :ensure t
  ;; :hook
  ;; (after-init . dashboard-insert-startupify-lists)
  ;; (after-init . dashboard-initialize)
  :init
  (setq dashboard-projects-backend 'project-el)
  (setq dashboard-banner-logo-title (concat "Welcome, " (user-full-name) "!"))
  (setq dashboard-startup-banner 2)
  (setq dashboard-center-content t)
  (setq dashboard-vertically-center-content t)
  (setq dashboard-navigation-cycle t)
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
                                    dashboard-insert-newline
                                    ;; dashboard-insert-footer
                                    ))
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
  (add-hook 'after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'after-init-hook #'dashboard-initialize)
  (dashboard-setup-startup-hook)
  )

(use-package ultra-scroll
  :ensure t
  ;;:vc (:url "https://github.com/jdtsmith/ultra-scroll") ; if desired (emacs>=v30)
  :init
  (setq scroll-conservatively 3 ; or whatever value you prefer, since v0.4
        scroll-margin 0)        ; important: scroll-margin>0 not yet supported
  :config
  (ultra-scroll-mode 1))

;; Nerd icons in completion (looks great with nano)
(use-package nerd-icons-completion
  :ensure t
  :config
  (nerd-icons-completion-mode)
  ;; Don't change this into :hook
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)
  )

;; Git diff showing
(use-package diff-hl
  :ensure t
  :commands (diff-hl-mode
             global-diff-hl-mode)
  :hook
  (prog-mode . diff-hl-mode)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :init
  (setq diff-hl-flydiff-delay 0.4)  ; Faster
  (setq diff-hl-show-staged-changes nil)  ; Realtime feedback
  (setq diff-hl-update-async t)  ; Do not block Emacs
  (setq diff-hl-global-modes '(not pdf-view-mode image-mode))
  )

;; Displays visible indicators for page breaks
(use-package page-break-lines
  :ensure t
  :commands (page-break-lines-mode
             global-page-break-lines-mode)
  :hook
  (lisp-data-mode . page-break-lines-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;; Completion and Minibuffer ;;;;;;;;;;;;;;;;;;;;;;;;;

;; Better completion framework
(use-package vertico
  :ensure t
  :init
  (setq vertico-count 15
        ;; vertico-resize t
        vertico-cycle t)
  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t)
  :config
  (vertico-mode)
  (vertico-mouse-mode))

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-posframe
  :ensure t
  :init
  (setq vertico-posframe-parameters
        '((left-fringe . 12)
          (right-fringe . 12)))
  (setq vertico-posframe-border-width 1)
  :config
  ;; ────── Doom Monokai Pro colors (exactly like your nano-modeline sync) ──────
  (defun my/vertico-posframe-sync ()
    "Sync vertico-posframe faces with current Doom theme."
    (when (fboundp 'doom-color)
      (let* ((bg-alt   (doom-color 'bg-alt))
             (fg-main  (doom-color 'fg))
             (accent   (doom-color 'yellow))
             (border   (doom-darken accent 0.2))
             (popup-bg (doom-darken bg-alt 0.08)))

        (custom-set-faces
         `(vertico-posframe
           ((t (:background ,popup-bg :foreground ,fg-main))))
         `(vertico-posframe-border
           ((t (:background ,border))))
         `(vertico-posframe-border-2
           ((t (:background ,border))))
         `(vertico-posframe-border-3
           ((t (:background ,border))))
         `(vertico-posframe-border-4
           ((t (:background ,border))))
         `(vertico-posframe-border-fallback
           ((t (:background ,border))))))))
  ;; Run now + on every theme change
  (add-hook 'doom-load-theme-hook #'my/vertico-posframe-sync)
  (my/vertico-posframe-sync)

  ;; Optional: fallback to buffer display if posframe fails (e.g. terminal)
  (setq vertico-posframe-fallback-mode #'vertico-buffer-mode)
  (vertico-posframe-mode))

(use-package orderless
  :ensure t
  :custom
  ;; (orderless-style-dispatchers '(orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil) ;; Disable defaults, use our settings
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring

;; Marginalia allows Embark to offer you preconfigured actions in more contexts.
;; In addition to that, Marginalia also enhances Vertico by adding rich
;; annotations to the completion candidates displayed in Vertico's interface.
(use-package marginalia
  :ensure t
  :commands (marginalia-mode marginalia-cycle)
  :config
  (marginalia-mode)
  )

;; Embark integrates with Consult and Vertico to provide context-sensitive
;; actions and quick access to commands based on the current selection, further
;; improving user efficiency and workflow within Emacs. Together, they create a
;; cohesive and powerful environment for managing completions and interactions.
(use-package embark
  ;; Embark is an Emacs package that acts like a context menu, allowing
  ;; users to perform context-sensitive actions on selected items
  ;; directly from the completion interface.
  :ensure t
  :commands (embark-act
             embark-dwim
             embark-export
             embark-collect
             embark-bindings
             embark-prefix-help-command)
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult-flycheck
  :ensure t)

;; Rich completion commands (M-x, find-file, switch-buffer, etc.)
(use-package consult
  :ensure t
  :hook
  (completion-list-mode . vertico-posframe-mode)
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g r" . consult-grep-match)
         ("M-g f" . consult-flycheck)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  )

;; Corfu enhances in-buffer completion by displaying a compact popup with
;; current candidates, positioned either below or above the point. Candidates
;; can be selected by navigating up or down.
(use-package corfu
  :ensure t

  :hook
  (prog-mode . corfu-mode)
  (shell-mode . corfu-mode)
  (eshell-mode . corfu-mode)

  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-trigger ".")
  (corfu-quit-no-match 'separator)

  ;; Enable Corfu
  :config
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  )

(use-package emacs
  :custom
  ;;;;;;;;; CORFU ;;;;;;;;
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p)

  ;;;;;;;;; VERTICO ;;;;;;;;;

  ;; Enable context menu. `vertico-multiform-mode' adds a menu in the minibuffer
  ;; to switch display modes.
  (context-menu-mode t)
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  )

;; Cape, or Completion At Point Extensions, extends the capabilities of
;; in-buffer completion. It integrates with Corfu or the default completion UI,
;; by providing additional backends through completion-at-point-functions.
(use-package cape
  :ensure t
  ;; :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind ("C-c p" . cape-prefix-map)
  :hook
  (completion-at-point-functions . cape-dabbrev)
  (completion-at-point-functions . cape-file)
  (completion-at-point-functions . cape-elisp-block)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;; Editing Enhancements ;;;;;;;;;;;;;;;;;;;;;;;;;

;; The undo-fu package is a lightweight wrapper around Emacs' built-in undo
;; system, providing more convenient undo/redo functionality.
(use-package undo-fu
  :ensure t)

;; The undo-fu-session package complements undo-fu by enabling the saving
;; and restoration of undo history across Emacs sessions, even after restarting.
(use-package undo-fu-session
  :ensure t
  :after undo-fu
  :config
  (undo-fu-session-global-mode))

(use-package easysession
  :ensure t
  :demand t
  :custom
  (easysession-save-interval (* 10 60))  ; Save every 10 minutes

  ;; Save the current session when using `easysession-switch-to'
  (easysession-switch-to-save-session t)

  ;; Do not exclude the current session when switching sessions
  (easysession-switch-to-exclude-current nil)

  ;; Display the active session name in the mode-line lighter.
  (easysession-save-mode-lighter-show-session-name t)

  ;; Optionally, the session name can be shown in the modeline info area:
  ;; (easysession-mode-line-misc-info t)

  :config
  ;; Key mappings
  (global-set-key (kbd "C-c sl") #'easysession-switch-to) ; Load session
  (global-set-key (kbd "C-c ss") #'easysession-save) ; Save session
  (global-set-key (kbd "C-c sL") #'easysession-switch-to-and-restore-geometry)
  (global-set-key (kbd "C-c sr") #'easysession-rename)
  (global-set-key (kbd "C-c sR") #'easysession-reset)
  (global-set-key (kbd "C-c su") #'easysession-unload)
  (global-set-key (kbd "C-c sd") #'easysession-delete)

  ;; non-nil: Make `easysession-setup' load the session automatically.
  ;; (nil: session is not loaded automatically; the user can load it manually.)
  (setq easysession-setup-load-session t)

  ;; The `easysession-setup' function adds hooks:
  ;; - To enable automatic session loading during `emacs-startup-hook', or
  ;;   `server-after-make-frame-hook' when running in daemon mode.
  ;; - To save the session at regular intervals, and when Emacs exits.
  (with-eval-after-load 'easysession
    (require 'easysession-scratch)
    (easysession-scratch-mode 1))
  (with-eval-after-load 'easysession
    (require 'easysession-magit)
    (easysession-magit-mode 1))
  (easysession-setup))

;; The official collection of snippets for yasnippet.
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;; YASnippet is a template system designed that enhances text editing by
;; enabling users to define and use snippets. When a user types a short
;; abbreviation, YASnippet automatically expands it into a full template, which
;; can include placeholders, fields, and dynamic content.
(use-package yasnippet
  :ensure t
  :commands (yas-minor-mode
             yas-global-mode)

  :hook
  (after-init . yas-global-mode)

  :custom
  (yas-also-auto-indent-first-line t)  ; Indent first line of snippet
  (yas-also-indent-empty-lines t)
  (yas-snippet-revival nil)  ; Setting this to t causes issues with undo
  (yas-wrap-around-region nil) ; Do not wrap region when expanding snippets
  ;; (yas-triggers-in-field nil)  ; Disable nested snippet expansion
  ;; (yas-indent-line 'fixed) ; Do not auto-indent snippet content
  ;; (yas-prompt-functions '(yas-no-prompt))  ; No prompt for snippet choices

  :init
  ;; Suppress verbose messages
  (setq yas-verbosity 0))

;; The stripspace Emacs package provides stripspace-local-mode, a minor mode
;; that automatically removes trailing whitespace and blank lines at the end of
;; the buffer when saving.
(use-package stripspace
  :ensure t
  :commands stripspace-local-mode

  ;; Enable for prog-mode-hook, text-mode-hook, conf-mode-hook
  :hook
  (prog-mode . stripspace-local-mode)
  (text-mode . stripspace-local-mode)
  (conf-mode . stripspace-local-mode)

  :custom
  ;; The `stripspace-only-if-initially-clean' option:
  ;; - nil to always delete trailing whitespace.
  ;; - Non-nil to only delete whitespace when the buffer is clean initially.
  ;; (The initial cleanliness check is performed when `stripspace-local-mode'
  ;; is enabled.)
  (stripspace-only-if-initially-clean nil)

  ;; Enabling `stripspace-restore-column' preserves the cursor's column position
  ;; even after stripping spaces. This is useful in scenarios where you add
  ;; extra spaces and then save the file. Although the spaces are removed in the
  ;; saved file, the cursor remains in the same position, ensuring a consistent
  ;; editing experience without affecting cursor placement.
  (stripspace-restore-column t))

;; Enables automatic indentation of code while typing
(use-package aggressive-indent
  :ensure t
  :commands aggressive-indent-mode
  :hook
  (lisp-data-mode . aggressive-indent-mode)
  )

;; Prevent parenthesis imbalance
(use-package paredit
  :ensure t
  :commands paredit-mode
  :hook
  (lisp-data-mode . paredit-mode)
  :config
  (define-key paredit-mode-map (kbd "RET") nil)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;; Programming and Development Tools ;;;;;;;;;;;;;;;;;;;;;;;;;

;; Project management
(use-package project
  :ensure t)

;; Dependency for magit
(use-package transient
  :ensure t)

(use-package pinentry
  :ensure t
  :config
  ;; start pinentry-emacs
  (pinentry-start))

;; Git management
(use-package magit
  :ensure t)

;; Dependency for eglot
(use-package flycheck
  :ensure t
  :config
  ;; Delay Flycheck by 5 seconds after you stop typing
  (setq flycheck-idle-change-delay 5)

  ;; Optional: even longer delay when switching buffers
  (setq flycheck-idle-buffer-switch-delay 5)

  ;; Optional: don't check on every new line (only on idle/save)
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (global-flycheck-mode)
  )

(use-package flycheck-posframe
  :ensure t
  :after flycheck
  :init
  (setq flycheck-posframe-border-width 16)
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode)
  (flycheck-posframe-configure-pretty-defaults)
  )

;; Set up the Language Server Protocol (LSP) servers using Eglot.
(use-package eglot
  :ensure nil
  :commands (eglot-ensure
             eglot-rename
             eglot-format-buffer)
  :hook
  (csharp-ts-mode . eglot-ensure)
  (c-mode . eglot-ensure)
  (c++-mode . eglot-ensure)
  )

;; Apheleia is an Emacs package designed to run code formatters (e.g., Shfmt,
;; Black and Prettier) asynchronously without disrupting the cursor position.
(use-package apheleia
  :ensure t
  :commands (apheleia-mode
             apheleia-global-mode)
  :hook
  (prog-mode . apheleia-mode)
  )

;; Highlights function and variable definitions in Emacs Lisp mode
(use-package highlight-defined
  :ensure t
  :commands highlight-defined-mode
  :hook
  (lisp-data-mode . highlight-defined-mode)
  )

;; Provides functions to find references to functions, macros, variables,
;; special forms, and symbols in Emacs Lisp
(use-package elisp-refs
  :ensure t
  :commands (elisp-refs-function
             elisp-refs-macro
             elisp-refs-variable
             elisp-refs-special
             elisp-refs-symbol))

;; Tree-sitter in Emacs is an incremental parsing system introduced in Emacs 29
;; that provides precise, high-performance syntax highlighting. It supports a
;; broad set of programming languages, including Bash, C, C++, C#, CMake, CSS,
;; Dockerfile, Go, Java, JavaScript, JSON, Python, Rust, TOML, TypeScript, YAML,
;; Elisp, Lua, Markdown, and many others.

(use-package treesit-auto
  :ensure t
  :init
  (setq treesit-auto-install 'prompt)
  (setq treesit-auto-add-to-auto-mode-alist 'all)
  :config
  (defvar my/treesit-dir
    (expand-file-name "tree-sitter/" my/var-dir))

  (defun my/treesit-install-redirect (orig-fn &rest args)
    (let ((user-emacs-directory my/var-dir))
      (apply orig-fn args)))

  (advice-add 'treesit-install-language-grammar
              :around #'my/treesit-install-redirect)

  (setq treesit-extra-load-path (list my/treesit-dir))

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

;; Markdown mode, useful for eldoc
(use-package markdown-mode
  :ensure t)

(use-package eldoc-box
  :ensure t
  :bind
  (("C-<prior>" . eldoc-box-scroll-down)
   ("C-<next>" . eldoc-box-scroll-up))
  :hook
  (prog-mode . eldoc-box-hover-mode)
  :config
  (set-face-attribute 'eldoc-box-border nil
                      :background (doom-color 'yellow))
  ;; ────── ALWAYS top-right of the WHOLE FRAME (ignores Treemacs/splits) ──────
  (defun my/eldoc-box--always-frame-top-right (width _height)
    "Place the childframe at the top-right corner of the entire Emacs frame."
    (pcase-let ((`(,_left ,right ,top) eldoc-box-offset))
      (cons (- (frame-outer-width) width right)   ; right edge of frame
            top)))                                ; top edge of frame

  (setq eldoc-box-position-function #'my/eldoc-box--always-frame-top-right)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;; Org Mode and Productivity ;;;;;;;;;;;;;;;;;;;;;;;;;

;; org mode is a major mode designed for organizing notes, planning, task
;; management, and authoring documents using plain text with a simple and
;; expressive markup syntax. It supports hierarchical outlines, TODO lists,
;; scheduling, deadlines, time tracking, and exporting to multiple formats
;; including HTML, LaTeX, PDF, and Markdown.
(use-package org
  :ensure t
  :commands (org-mode org-version)
  :mode
  ("\\.org\\'" . org-mode)
  :custom
  (org-hide-leading-stars t)
  (org-startup-indented t)
  (org-adapt-indentation nil)
  (org-edit-src-content-indentation 0)
  ;; (org-fontify-done-headline t)
  ;; (org-fontify-todo-headline t)
  ;; (org-fontify-whole-heading-line t)
  ;; (org-fontify-quote-and-verse-blocks t)
  (org-startup-truncated t))

(use-package buffer-terminator
  :ensure t
  :custom
  ;; Enable/Disable verbose mode to log buffer cleanup events
  (buffer-terminator-verbose nil)

  ;; Set the inactivity timeout (in seconds) after which buffers are considered
  ;; inactive (default is 30 minutes):
  (buffer-terminator-inactivity-timeout (* 30 60)) ; 30 minutes

  ;; Define how frequently the cleanup process should run (default is every 10
  ;; minutes):
  (buffer-terminator-interval (* 10 60)) ; 10 minutes

  :config
  (buffer-terminator-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;; File and Project Navigation ;;;;;;;;;;;;;;;;;;;;;;;;;

;; A file and project explorer for Emacs that displays a structured tree
;; layout, similar to file browsers in modern IDEs. It functions as a sidebar
;; in the left window, providing a persistent view of files, projects, and
;; other elements.
(use-package treemacs
  :ensure t
  :commands (treemacs
             treemacs-select-window
             treemacs-delete-other-windows
             treemacs-select-directory
             treemacs-bookmark
             treemacs-find-file
             treemacs-find-tag)

  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag))

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

  ;; The default width and height of the icons is 22 pixels. If you are
  ;; using a Hi-DPI display, uncomment this to double the icon size.
  ;; (treemacs-resize-icons 44)

  (treemacs-follow-mode t)
  (treemacs-project-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)

  ;;(when treemacs-python-executable
  ;;  (treemacs-git-commit-diff-mode t))

  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))

  (treemacs-hide-gitignored-files-mode t)

  (treemacs-start-on-boot t))

(use-package treemacs-magit
  :ensure t)

(use-package treemacs-icons-dired
  :ensure t
  :hook
  (dired-mode . treemacs-icons-dired-enable-once)
  )

;;
;; (use-package treemacs-tab-bar  ; treemacs-tab-bar if you use tab-bar-mode
;;   :after (treemacs)
;;   :ensure t
;;   :config (treemacs-set-scope-type 'Tabs))
;;

;;;;;;;;;;;;;;;;;;;;;;;;;; Help and Documentation ;;;;;;;;;;;;;;;;;;;;;;;;;

;; Helpful is an alternative to the built-in Emacs help that provides much more
;; contextual information.
(use-package helpful
  :ensure t
  :commands (helpful-callable
             helpful-variable
             helpful-key
             helpful-command
             helpful-at-point
             helpful-function)
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  :custom
  (helpful-max-buffers 7))

(provide 'init)
;;; init.el ends here
