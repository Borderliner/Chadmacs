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

;; Marginalia: rich annotations in completion candidates. Enabled in :init -
;; with `:commands' + `:config' the package only loaded once one of its
;; commands ran, and the only call to one was that very :config, so it
;; never loaded at all.
(use-package marginalia
  :ensure t
  :init
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

;; Bound inside `projectile-command-map' (the `C-c p' prefix), not the
;; global map: `projectile-mode-map' binds `C-c p' to that prefix map and
;; minor-mode maps take precedence, so global `C-c p p/f/g' bindings were
;; always shadowed by projectile's own commands.
(use-package consult-projectile
  :ensure t
  :after projectile
  :bind (:map projectile-command-map
              ("p" . consult-projectile-switch-project)
              ("f" . consult-projectile-find-file)
              ("g" . my/consult-ripgrep-project)))

;; Backend for `projectile-ripgrep' / `M-x rg' / `M-x rg-dwim'. Projectile's
;; ripgrep command delegates to whichever of the `rg' or `ripgrep' packages
;; is installed; without one it errors with "Packages 'ripgrep' and 'rg'
;; are not available". `rg' is the actively maintained one.
(use-package rg
  :ensure t
  :commands (rg rg-dwim rg-project rg-menu))

;; ----------------------------------------------------------- In-buffer --

;; Corfu - compact popup completion at point, in every buffer from startup.
;; It used to load from prog / shell / eshell hooks and only then turn on
;; `global-corfu-mode', so text and Org buffers had no popup until the
;; first code buffer was opened - and then they all got one at once.
(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-quit-no-match 'separator)
  :config
  (global-corfu-mode)
  (corfu-history-mode)
  ;; corfu-history ranks recently chosen candidates first; savehist
  ;; (chadmacs-core) only keeps it across restarts when asked to.
  (add-to-list 'savehist-additional-variables 'corfu-history)
  (corfu-popupinfo-mode))

;; Corfu uses child-frames for its popup, which before Emacs 31 only render
;; under a GUI. corfu-terminal swaps in an overlay-popup for TTY Emacs. Its
;; dependency `popon' is shipped as a tar release on NonGNU ELPA; Elpaca's
;; tar handler currently fails on it ("Unable to find main elisp file"), so
;; both packages are pinned to their Codeberg git remotes directly.
;;
;; Only needed on TTY Emacs *older than 31*. Emacs 31 renders child frames
;; natively in the terminal, so corfu's own popup already works there, and
;; neither package is installed or loaded. Loading corfu-terminal on Emacs
;; 31 just emits a `display-warning' ("`corfu-terminal' is not needed on
;; Emacs 31"), which pops up *Warnings* during startup. That popup splits
;; the initial frame, so a file opened with `emacs -nw FILE' lands in a
;; tiny split under *scratch* instead of owning the frame. (Elpaca's
;; :ensure evaluates a backquoted value: nil -> don't install.)
(use-package popon
  :ensure `,(and (< emacs-major-version 31)
                 '(popon :host nil
                         :repo "https://codeberg.org/akib/emacs-popon.git"))
  :unless (>= emacs-major-version 31))

(use-package corfu-terminal
  :ensure `,(and (< emacs-major-version 31)
                 '(corfu-terminal
                   :host nil
                   :repo "https://codeberg.org/akib/emacs-corfu-terminal.git"))
  :after (corfu popon)
  :unless (or (display-graphic-p) (>= emacs-major-version 31))
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
;; Added to the *global* value of `completion-at-point-functions', which a
;; buffer only consults after its own capfs (the `t' entry of the local
;; list). The mode's real completion - eglot, elisp symbols, python, ... -
;; therefore always goes first and cape fills in where it has nothing.
;; Prepending them buffer-locally let `cape-dabbrev' shadow e.g.
;; `elisp-completion-at-point' with plain buffer words.
(use-package cape
  :ensure t
  :bind ("C-c e" . cape-prefix-map)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(provide 'chadmacs-completion)
;;; chadmacs-completion.el ends here
