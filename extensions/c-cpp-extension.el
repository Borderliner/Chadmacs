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

;; Optional: CMake mode + cmake-language-server
(use-package cmake-mode
  :ensure t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'"         . cmake-mode))
  :hook (cmake-mode . eglot-ensure))

(provide 'c-cpp-extension)
;;; c-cpp-extension.el ends here
