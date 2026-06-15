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

;; -----------------------------------------------------------------------
;; Language extensions
;; -----------------------------------------------------------------------
;; Each file in extensions/ bundles a major mode + LSP hook + minor modes
;; for a single language. Each file's header comment lists the system
;; binary / package you need to install for LSP to work. Uncomment to
;; activate; comment again to disable. No conflict between them.
;;
;; Tip: put your enabled extensions in custom.el instead of here if you
;; want to keep your enabled-language list outside the upstream config.

;; --- Always-on baseline ---
(require 'rust-extension)

;; --- Web stack ---
;; (require 'typescript-extension)     ;; TypeScript / TSX / JavaScript / JSX
;; (require 'web-extension)            ;; HTML / CSS / SCSS / Vue / Svelte / Emmet
;; (require 'json-extension)           ;; JSON / JSONC
;; (require 'yaml-extension)           ;; YAML

;; --- Backend / scripting ---
;; (require 'python-extension)         ;; Python (+ optional pyvenv)
;; (require 'go-extension)             ;; Go (+ go.mod)
;; (require 'ruby-extension)           ;; Ruby / Rails / Gemfile / Rakefile
;; (require 'elixir-extension)         ;; Elixir + HEEx (Phoenix templates)
;; (require 'lua-extension)            ;; Lua

;; --- Systems / native ---
;; (require 'c-cpp-extension)          ;; C / C++ + meson + cmake
;; (require 'csharp-extension)         ;; C# / .NET
;; (require 'zig-extension)            ;; Zig
;; (require 'swift-extension)          ;; Swift

;; --- JVM ---
;; (require 'scala-extension)          ;; Scala / sbt
;; (require 'kotlin-extension)         ;; Kotlin

;; --- Functional ---
;; (require 'haskell-extension)        ;; Haskell + Cabal
;; (require 'ocaml-extension)          ;; OCaml + Dune
;; (require 'clojure-extension)        ;; Clojure + CIDER + clj-refactor

;; --- Scientific ---
;; (require 'julia-extension)          ;; Julia + LanguageServer.jl

;; --- DevOps / infra ---
;; (require 'docker-extension)         ;; Dockerfile (compose via yaml)
;; (require 'terraform-extension)      ;; Terraform / HCL
;; (require 'nix-extension)            ;; Nix

;; --- Docs / writing ---
;; (require 'markdown-extension)       ;; Markdown / GFM (+ marksman)

;; --- Niche ---
;; (require 'gerbil-extension)         ;; Gerbil Scheme (kept for legacy)

;; Leader key loaded last so all referenced commands are autoloaded
(require 'chadmacs-leader)

(create-or-load-custom-file)

(provide 'init)
;;; init.el ends here
