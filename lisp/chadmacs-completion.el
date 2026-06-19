;;; chadmacs-completion.el --- Minibuffer + in-buffer completion -*- lexical-binding: t; -*-
;;
;; Commentary:
;;   The completion stack:
;;     - Minibuffer: vertico, orderless, marginalia, embark, consult
;;     - In-buffer: corfu (+ corfu-terminal for TTY), cape
;;
;;; Code:

;; ----------------------------------------------------------- Minibuffer --

(use-package vertico
  :ensure t
  :init
  (setq vertico-count 15
        vertico-cycle t)
  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t)
  :config
  (vertico-mode)
  (vertico-mouse-mode))

;; Vertico's directory-navigation extension (ships with vertico, no install)
(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("RET"   . vertico-directory-enter)
              ("DEL"   . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil)
  (completion-pcm-leading-wildcard t))

;; Marginalia: rich annotations in completion candidates
(use-package marginalia
  :ensure t
  :commands (marginalia-mode marginalia-cycle)
  :config
  (marginalia-mode))

;; Embark: context-sensitive actions / right-click menu
(use-package embark
  :ensure t
  :commands (embark-act
             embark-dwim
             embark-export
             embark-collect
             embark-bindings
             embark-prefix-help-command)
  :bind
  (("C-."   . embark-act)
   ("C-;"   . embark-dwim)
   ("C-h B" . embark-bindings))
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

;; Flycheck navigation via consult
(use-package consult-flycheck
  :ensure t)

;; Rich completion commands (M-x, find-file, switch-buffer, etc.)
(use-package consult
  :ensure t
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h"   . consult-history)
         ("C-c k"   . consult-kmacro)
         ("C-c m"   . consult-man)
         ("C-c i"   . consult-info)
         ;; `C-c s' is owned by easysession (C-c s l, C-c s s, ...), so
         ;; ripgrep gets `C-c /' instead. Mnemonic: "/" = search across.
         ("C-c /"   . consult-ripgrep)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)
         ("C-x b"   . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; Registers
         ("M-#"   . consult-register-load)
         ("M-'"   . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Yank
         ("M-y" . consult-yank-pop)
         ;; M-g bindings in `goto-map'
         ("M-g e"   . consult-compile-error)
         ("M-g f"   . consult-flycheck)
         ("M-g g"   . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o"   . consult-outline)
         ("M-g m"   . consult-mark)
         ("M-g k"   . consult-global-mark)
         ("M-g i"   . consult-imenu)
         ("M-g I"   . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
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
         ("M-e"   . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))
  :init
  ;; Improve register preview (formatting, thin separators, sorting)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)
  :config
  ;; Consult-powered xref preview
  (setq xref-show-xrefs-function       #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (setq consult-preview-key 'any)
  ;; Disable the async split character (default '#'). With `nil', the whole
  ;; minibuffer input is sent to the async backend (ripgrep, grep, find...)
  ;; verbatim - no split, no client-side filter pass, no leading-`#' hint
  ;; in the prompt. Trade-off: you lose `pattern1#pattern2' two-phase
  ;; narrowing, but in practice orderless + rg's own regex flags cover the
  ;; same ground without surprising new users.
  (setq consult-async-split-style nil))

(defun my/consult-ripgrep-project ()
  "Run `consult-ripgrep' at the projectile project root.
Falls back to `default-directory' when not inside a project.

Exists because `consult-projectile-ripgrep' has a stale-autoload bug
on some Elpaca builds (\"failed to define function ...\")."
  (interactive)
  (let ((dir (or (and (fboundp 'projectile-project-root)
                      (ignore-errors (projectile-project-root)))
                 default-directory)))
    (consult-ripgrep dir)))

(use-package consult-projectile
  :ensure t
  :after (consult projectile)
  :bind (("C-c p p" . consult-projectile-switch-project)
         ("C-c p f" . consult-projectile-find-file)
         ("C-c p g" . my/consult-ripgrep-project)))

;; Backend for `projectile-ripgrep' / `M-x rg' / `M-x rg-dwim'. Projectile's
;; ripgrep command delegates to whichever of the `rg' or `ripgrep' packages
;; is installed; without one it errors with "Packages 'ripgrep' and 'rg'
;; are not available". `rg' is the actively maintained one.
(use-package rg
  :ensure t
  :commands (rg rg-dwim rg-project rg-menu))

;; ----------------------------------------------------------- In-buffer --

;; Corfu - compact popup completion at point
(use-package corfu
  :ensure t
  :hook
  (prog-mode   . corfu-mode)
  (shell-mode  . corfu-mode)
  (eshell-mode . corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-quit-no-match 'separator)
  :config
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode))

;; Corfu uses child-frames for its popup, which only render under a GUI.
;; corfu-terminal swaps in an overlay-popup for TTY Emacs. Its dependency
;; `popon' is shipped as a tar release on NonGNU ELPA; Elpaca's tar
;; handler currently fails on it ("Unable to find main elisp file"), so
;; both packages are pinned to their Codeberg git remotes directly.
(use-package popon
  :ensure (popon :host nil
                 :repo "https://codeberg.org/akib/emacs-popon.git"))

(use-package corfu-terminal
  :ensure (corfu-terminal :host nil
                          :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
  :after (corfu popon)
  :unless (display-graphic-p)
  :config
  (corfu-terminal-mode 1))

;; Built-in-emacs hook & custom block (corfu / vertico shared knobs).
;; Pair handling is owned by `smartparens-mode' globally (see
;; chadmacs-editing.el), so no electric-pair-mode wiring here.
(use-package emacs
  :custom
  ;; Corfu: TAB completes when no indent
  (tab-always-indent 'complete)
  ;; Emacs 30+: skip Ispell from completion-at-point
  (text-mode-ispell-word-completion nil)
  ;; Hide commands in M-x which don't apply to current mode
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Minibuffer
  (context-menu-mode t)
  (enable-recursive-minibuffers t)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

;; Cape: extra completion-at-point sources (dabbrev, file path, elisp block).
;; Loaded buffer-locally per prog/text-mode so it composes with eglot.
(use-package cape
  :ensure t
  :bind ("C-c e" . cape-prefix-map)
  :init
  (defun my/cape-setup ()
    "Register cape capfs buffer-locally."
    (add-hook 'completion-at-point-functions #'cape-dabbrev nil t)
    (add-hook 'completion-at-point-functions #'cape-file nil t)
    (add-hook 'completion-at-point-functions #'cape-elisp-block nil t))
  (add-hook 'prog-mode-hook #'my/cape-setup)
  (add-hook 'text-mode-hook #'my/cape-setup))

(provide 'chadmacs-completion)
;;; chadmacs-completion.el ends here
