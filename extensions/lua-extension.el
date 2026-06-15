;;; lua-extension.el --- Lua support -*- lexical-binding: t; -*-
;;
;; LSP binary:
;;   pacman -S lua-language-server     ;; Arch
;;   brew install lua-language-server  ;; macOS
;;   or download from https://github.com/LuaLS/lua-language-server/releases

(use-package lua-mode
  :ensure t
  :mode ("\\.lua\\'" . lua-mode)
  :hook (lua-mode . eglot-ensure)
  :custom
  (lua-indent-level 2))

(provide 'lua-extension)
;;; lua-extension.el ends here
