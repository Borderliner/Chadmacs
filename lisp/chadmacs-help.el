;;; chadmacs-help.el --- Better describe-* commands -*- lexical-binding: t; -*-
;;
;; Commentary:
;;   Replaces built-in describe-function / variable / key / command / symbol
;;   with their `helpful' equivalents, which provide more context, source
;;   links, and example calls.
;;
;;; Code:

(use-package helpful
  :ensure t
  :commands (helpful-callable
             helpful-variable
             helpful-key
             helpful-command
             helpful-at-point
             helpful-function)
  :bind
  ([remap describe-command]  . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-key]      . helpful-key)
  ([remap describe-symbol]   . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  :custom
  (helpful-max-buffers 7))

(provide 'chadmacs-help)
;;; chadmacs-help.el ends here
