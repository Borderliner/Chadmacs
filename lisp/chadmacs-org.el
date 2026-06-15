;;; chadmacs-org.el --- Org-mode + buffer hygiene -*- lexical-binding: t; -*-
;;
;; Commentary:
;;   Org mode visual defaults and `buffer-terminator' for trimming long-idle
;;   buffers that accumulate during big work sessions.
;;
;;; Code:

;; org-mode: organising notes, planning, task management, document authoring
;; with plain-text markup. Supports hierarchical outlines, TODO lists,
;; scheduling, deadlines, time tracking, and export to HTML/LaTeX/PDF/Markdown.
(use-package org
  :ensure t
  :commands (org-mode org-version)
  :mode ("\\.org\\'" . org-mode)
  :custom
  (org-startup-indented t)
  (org-adapt-indentation nil)
  (org-edit-src-content-indentation 0)
  (org-startup-truncated t))

;; Kill buffers that have been idle for a while (default: 30 min). Keeps the
;; buffer list manageable in long-running Emacs sessions.
(use-package buffer-terminator
  :ensure t
  :custom
  (buffer-terminator-verbose nil)
  (buffer-terminator-inactivity-timeout (* 30 60))
  (buffer-terminator-interval (* 10 60))
  :config
  (buffer-terminator-mode 1))

(provide 'chadmacs-org)
;;; chadmacs-org.el ends here
