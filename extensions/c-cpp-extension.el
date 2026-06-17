;;; c-cpp-extension.el --- C / C++ support -*- lexical-binding: t; -*-
;;
;; LSP binary: clangd
;;   pacman -S clang        ;; Arch (clang package ships clangd)
;;   apt install clangd     ;; Debian / Ubuntu
;;   brew install llvm      ;; macOS (then add to PATH)
;;
;; Generate a compile_commands.json (with cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON
;; or `bear -- make') so clangd indexes properly.

(use-package cc-mode
  :ensure nil
  :hook
  (c-mode      . eglot-ensure)
  (c++-mode    . eglot-ensure)
  (c-ts-mode   . eglot-ensure)
  (c++-ts-mode . eglot-ensure)
  (c-mode      . subword-mode)
  (c++-mode    . subword-mode))

;; Meson build files
(use-package meson-mode
  :ensure t
  :mode "meson\\.build\\'")

;; CMake mode + cmake-language-server.
;;
;; MELPA's official recipe clones the entire CMake source tree (hundreds
;; of MB) and looks for the major mode under Auxiliary/. Elpaca's main-
;; file resolver doesn't descend into Auxiliary/, so the build fails with
;; "Unable to find main elisp file for cmake-mode". Use the emacsmirror
;; single-file mirror instead (a tiny repo with cmake-mode.el at root).
(use-package cmake-mode
  :ensure (cmake-mode :host github :repo "emacsmirror/cmake-mode")
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'"         . cmake-mode))
  :hook (cmake-mode . eglot-ensure))

(provide 'c-cpp-extension)
;;; c-cpp-extension.el ends here
