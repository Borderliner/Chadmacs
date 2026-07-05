;;; c-cpp-lang.el --- C / C++ support -*- lexical-binding: t; -*-
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
;;
;; `:inherit nil' is required so Elpaca does NOT merge our recipe with
;; MELPA's - otherwise MELPA's :files ("Auxiliary/*.el") spec leaks in
;; and tries to find Auxiliary/ in the emacsmirror clone (which has no
;; such directory), producing the same "Unable to find main elisp file"
;; error you'd get on the MELPA recipe.
(use-package cmake-mode
  :ensure (cmake-mode :type git
                      :host github
                      :repo "emacsmirror/cmake-mode"
                      :files (:defaults)
                      :inherit nil)
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'"         . cmake-mode))
  :hook (cmake-mode . eglot-ensure))

(provide 'c-cpp-lang)
;;; c-cpp-lang.el ends here
