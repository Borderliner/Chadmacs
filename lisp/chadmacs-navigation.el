;;; chadmacs-navigation.el --- File explorers, popups, terminal -*- lexical-binding: t; -*-
;;
;; Commentary:
;;   File / project navigation tools and the popup/terminal trifecta:
;;     - Treemacs (sidebar tree)
;;     - Dirvish (dired replacement)
;;     - Popper (popup window manager)
;;     - vterm (libvterm terminal)
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
  (("C-c f d" . dirvish)
   :map dirvish-mode-map
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
          help-mode
          helpful-mode
          compilation-mode
          eshell-mode
          shell-mode
          vterm-mode))
  (popper-mode 1)
  (popper-echo-mode 1))

;; ---------------------------------------------------------- vterm --

;; Fast libvterm-backed terminal. Needs libvterm + cmake at build time; if
;; install fails, install libvterm-devel / libvterm-dev on your system and
;; run M-x elpaca-rebuild vterm.
(use-package vterm
  :ensure t
  :commands (vterm vterm-other-window)
  :bind (("C-c v" . vterm)
         ("C-c V" . vterm-other-window))
  :custom
  (vterm-max-scrollback 10000)
  (vterm-buffer-name-string "vterm: %s"))

(provide 'chadmacs-navigation)
;;; chadmacs-navigation.el ends here
