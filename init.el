;;; init.el --- Chadmacs main loader -*- no-byte-compile: t; lexical-binding: t; -*-
;;
;; Author: Borderliner
;; URL: https://github.com/Borderliner/Chadmacs
;;
;;; Commentary:
;;
;; Thin orchestrator. Real configuration lives in lisp/chadmacs-*.el modules.
;; Bootstrap order:
;;
;;   1. early-init.el  -> GC tuning, Elpaca bootstrap, paths under var/
;;   2. chadmacs-core         -> sane defaults, persistence, optimisation
;;   3. chadmacs-ui           -> frame, theme, modeline, dashboard, which-key
;;   4. chadmacs-completion   -> vertico / consult / corfu / cape / embark
;;   5. chadmacs-editing      -> undo, sessions, snippets, paredit, vundo, ...
;;   6. chadmacs-tools        -> projectile, magit, flycheck, eglot, envrc, ...
;;   7. chadmacs-org          -> org, buffer-terminator
;;   8. chadmacs-navigation   -> treemacs, dirvish, popper, vterm
;;   9. chadmacs-help         -> helpful
;;  10. <extensions/*>        -> language-specific (rust, etc.)
;;  11. chadmacs-leader       -> Doom-style C-c SPC menu (last - references
;;                                commands from every module above)
;;  12. custom.el             -> user-local overrides (gitignored)
;;
;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "extensions" user-emacs-directory))

(require 'chadmacs-core)
(require 'chadmacs-ui)
(require 'chadmacs-completion)
(require 'chadmacs-editing)
(require 'chadmacs-tools)
(require 'chadmacs-org)
(require 'chadmacs-navigation)
(require 'chadmacs-help)

;; Language extensions are enabled per-user in custom.el (gitignored).
;; A pre-populated template with every available language is installed
;; on first launch by `create-or-load-custom-file' (see chadmacs-core).
;; This keeps the upstream init.el free of personal selections, so
;; pulling new Chadmacs versions never produces merge conflicts.

;; Leader key loaded last so all referenced commands are autoloaded
(require 'chadmacs-leader)

(create-or-load-custom-file)

(provide 'init)
;;; init.el ends here
