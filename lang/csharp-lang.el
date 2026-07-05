;;; csharp-lang.el --- C# / .NET support -*- lexical-binding: t; -*-
;;
;; LSP binary (pick one):
;;   csharp-ls   — dotnet tool install -g csharp-ls
;;   omnisharp   — bundled with vscode; install from
;;                  https://github.com/OmniSharp/omnisharp-roslyn/releases
;;
;; Tree-sitter: the custom c-sharp recipe in chadmacs-tools.el clones
;; tree-sitter-c-sharp at first install.

(use-package csharp-mode
  :ensure nil
  :mode "\\.cs\\'"
  :hook
  (csharp-mode    . eglot-ensure)
  (csharp-ts-mode . eglot-ensure)
  (csharp-mode    . subword-mode)
  (csharp-ts-mode . subword-mode))

(provide 'csharp-lang)
;;; csharp-lang.el ends here
