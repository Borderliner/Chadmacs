;;; chadmacs-leader.el --- Doom-style leader keys -*- lexical-binding: t; -*-
;;
;; Commentary:
;;   Vanilla-Emacs leader-key setup. C-c SPC opens a tree of operations
;;   grouped by domain (buffer, file, git, search, window, ...). Combined
;;   with which-key this gives Doom/Spacemacs-style discoverability without
;;   needing evil or general.el. Each binding uses the (description . command)
;;   cons form so the which-key popup shows the human label.
;;
;;   This module is required LAST, after every other chadmacs-* module, so
;;   that all referenced commands (consult-ripgrep, magit-status, vterm,
;;   easysession-*, dashboard-open, ...) are autoloaded by the time the
;;   leader-map is built.
;;
;;; Code:

;; -------------------------------------------------------- Helpers --

(defun my/kill-other-buffers ()
  "Kill all buffers except the current one."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun my/copy-file-name ()
  "Copy current buffer's file path to kill-ring."
  (interactive)
  (when-let ((name (or buffer-file-name default-directory)))
    (kill-new name)
    (message "Copied: %s" name)))

(defun my/consult-ripgrep-here ()
  "Run consult-ripgrep starting at current `default-directory'."
  (interactive)
  (consult-ripgrep default-directory))

(defun my/consult-ripgrep-project ()
  "Run consult-ripgrep at the projectile project root.
Falls back to `default-directory' when not inside a project.
This is a thin wrapper that exists because `consult-projectile-ripgrep'
has an Elpaca autoload regression on some builds."
  (interactive)
  (let ((dir (or (and (fboundp 'projectile-project-root)
                      (ignore-errors (projectile-project-root)))
                 default-directory)))
    (consult-ripgrep dir)))

;; -------------------------------------------------- Top-level map --

(define-prefix-command 'my/leader-map)
(global-set-key (kbd "C-c SPC") 'my/leader-map)

;; Top-level leader entries
(define-key my/leader-map (kbd "SPC") '("M-x" . execute-extended-command))
(define-key my/leader-map (kbd "/")   '("search project" . consult-ripgrep))
(define-key my/leader-map (kbd ".")   '("find file" . my/smart-find-file))
(define-key my/leader-map (kbd ",")   '("switch buffer" . consult-buffer))
(define-key my/leader-map (kbd ":")   '("eval expression" . eval-expression))
(define-key my/leader-map (kbd "x")   '("scratch" . scratch-buffer))

;; --------------------------------------------------------- Buffer --

(define-prefix-command 'my/leader-buffer-map)
(define-key my/leader-map        (kbd "b") '("buffer..." . my/leader-buffer-map))
(define-key my/leader-buffer-map (kbd "b") '("switch" . consult-buffer))
(define-key my/leader-buffer-map (kbd "d") '("kill" . kill-current-buffer))
(define-key my/leader-buffer-map (kbd "k") '("kill" . kill-current-buffer))
(define-key my/leader-buffer-map (kbd "K") '("kill others" . my/kill-other-buffers))
(define-key my/leader-buffer-map (kbd "r") '("revert" . revert-buffer))
(define-key my/leader-buffer-map (kbd "s") '("save" . save-buffer))
(define-key my/leader-buffer-map (kbd "S") '("save all" . save-some-buffers))
(define-key my/leader-buffer-map (kbd "n") '("next" . next-buffer))
(define-key my/leader-buffer-map (kbd "p") '("prev" . previous-buffer))
(define-key my/leader-buffer-map (kbd "i") '("ibuffer" . ibuffer))

;; ---------------------------------------------------------- File --

(define-prefix-command 'my/leader-file-map)
(define-key my/leader-map      (kbd "f") '("file..." . my/leader-file-map))
(define-key my/leader-file-map (kbd "f") '("find" . my/smart-find-file))
(define-key my/leader-file-map (kbd "F") '("find (no project)" . find-file))
(define-key my/leader-file-map (kbd "r") '("recent" . consult-recent-file))
(define-key my/leader-file-map (kbd "s") '("save" . save-buffer))
(define-key my/leader-file-map (kbd "S") '("save as" . write-file))
(define-key my/leader-file-map (kbd "d") '("dirvish" . dirvish))
(define-key my/leader-file-map (kbd "D") '("delete file+buffer" . crux-delete-file-and-buffer))
(define-key my/leader-file-map (kbd "R") '("rename file+buffer" . crux-rename-file-and-buffer))
(define-key my/leader-file-map (kbd "y") '("copy filename" . my/copy-file-name))

;; ----------------------------------------------------------- Git --

(define-prefix-command 'my/leader-git-map)
(define-key my/leader-map     (kbd "g") '("git..." . my/leader-git-map))
(define-key my/leader-git-map (kbd "g") '("status" . magit-status))
(define-key my/leader-git-map (kbd "b") '("blame" . magit-blame))
(define-key my/leader-git-map (kbd "l") '("log buffer" . magit-log-buffer-file))
(define-key my/leader-git-map (kbd "L") '("log all" . magit-log))
(define-key my/leader-git-map (kbd "d") '("dispatch" . magit-dispatch))
(define-key my/leader-git-map (kbd "f") '("file dispatch" . magit-file-dispatch))

;; -------------------------------------------------------- Project --

(define-prefix-command 'my/leader-project-map)
(define-key my/leader-map         (kbd "p") '("project..." . my/leader-project-map))
(define-key my/leader-project-map (kbd "p") '("switch project" . consult-projectile-switch-project))
(define-key my/leader-project-map (kbd "f") '("find file" . consult-projectile-find-file))
(define-key my/leader-project-map (kbd "g") '("ripgrep" . my/consult-ripgrep-project))
(define-key my/leader-project-map (kbd "b") '("project buffer" . consult-project-buffer))
(define-key my/leader-project-map (kbd "c") '("compile" . projectile-compile-project))
(define-key my/leader-project-map (kbd "r") '("run" . projectile-run-project))
(define-key my/leader-project-map (kbd "t") '("test" . projectile-test-project))
(define-key my/leader-project-map (kbd "k") '("kill buffers" . projectile-kill-buffers))
(define-key my/leader-project-map (kbd "!") '("shell cmd" . projectile-run-shell-command-in-root))

;; --------------------------------------------------------- Search --

(define-prefix-command 'my/leader-search-map)
(define-key my/leader-map        (kbd "s") '("search..." . my/leader-search-map))
(define-key my/leader-search-map (kbd "s") '("buffer (consult-line)" . consult-line))
(define-key my/leader-search-map (kbd "p") '("project (rg)" . consult-ripgrep))
(define-key my/leader-search-map (kbd "d") '("directory (rg)" . my/consult-ripgrep-here))
(define-key my/leader-search-map (kbd "i") '("imenu" . consult-imenu))
(define-key my/leader-search-map (kbd "f") '("find file (fd)" . consult-find))
(define-key my/leader-search-map (kbd "j") '("avy char" . avy-goto-char-timer))
(define-key my/leader-search-map (kbd "l") '("avy line" . avy-goto-line))

;; --------------------------------------------------------- Window --

(define-prefix-command 'my/leader-window-map)
(define-key my/leader-map        (kbd "w") '("window..." . my/leader-window-map))
(define-key my/leader-window-map (kbd "w") '("other" . ace-window))
(define-key my/leader-window-map (kbd "o") '("other" . other-window))
(define-key my/leader-window-map (kbd "d") '("delete" . delete-window))
(define-key my/leader-window-map (kbd "D") '("delete other" . delete-other-windows))
(define-key my/leader-window-map (kbd "s") '("split below" . split-window-below))
(define-key my/leader-window-map (kbd "v") '("split right" . split-window-right))
(define-key my/leader-window-map (kbd "u") '("winner undo" . winner-undo))
(define-key my/leader-window-map (kbd "U") '("winner redo" . winner-redo))
(define-key my/leader-window-map (kbd "=") '("balance" . balance-windows))

;; --------------------------------------------------------- Toggle --

(define-prefix-command 'my/leader-toggle-map)
(define-key my/leader-map        (kbd "t") '("toggle..." . my/leader-toggle-map))
(define-key my/leader-toggle-map (kbd "l") '("line numbers" . display-line-numbers-mode))
(define-key my/leader-toggle-map (kbd "w") '("word wrap" . visual-line-mode))
(define-key my/leader-toggle-map (kbd "t") '("treemacs" . treemacs))
(define-key my/leader-toggle-map (kbd "T") '("theme" . consult-theme))
(define-key my/leader-toggle-map (kbd "f") '("flycheck" . flycheck-mode))
(define-key my/leader-toggle-map (kbd "v") '("vterm" . vterm))
(define-key my/leader-toggle-map (kbd "F") '("fullscreen" . toggle-frame-fullscreen))
(define-key my/leader-toggle-map (kbd "p") '("popper" . popper-toggle))

;; ----------------------------------------------------------- Help --

(define-prefix-command 'my/leader-help-map)
(define-key my/leader-map      (kbd "h") '("help..." . my/leader-help-map))
(define-key my/leader-help-map (kbd "f") '("function" . helpful-callable))
(define-key my/leader-help-map (kbd "v") '("variable" . helpful-variable))
(define-key my/leader-help-map (kbd "k") '("key" . helpful-key))
(define-key my/leader-help-map (kbd "s") '("symbol" . helpful-symbol))
(define-key my/leader-help-map (kbd "p") '("package" . describe-package))
(define-key my/leader-help-map (kbd "m") '("mode" . describe-mode))

;; ----------------------------------------------------------- Open --

(define-prefix-command 'my/leader-open-map)
(define-key my/leader-map      (kbd "o") '("open..." . my/leader-open-map))
(define-key my/leader-open-map (kbd "a") '("agenda" . org-agenda))
(define-key my/leader-open-map (kbd "c") '("capture" . org-capture))
(define-key my/leader-open-map (kbd "d") '("dashboard" . dashboard-open))
(define-key my/leader-open-map (kbd "t") '("treemacs" . treemacs))
(define-key my/leader-open-map (kbd "v") '("vterm" . vterm))
(define-key my/leader-open-map (kbd "e") '("eshell" . eshell))

;; -------------------------------------------------------- Session --

(define-prefix-command 'my/leader-session-map)
(define-key my/leader-map         (kbd "S") '("session..." . my/leader-session-map))
(define-key my/leader-session-map (kbd "s") '("save" . easysession-save))
(define-key my/leader-session-map (kbd "l") '("load" . easysession-switch-to))
(define-key my/leader-session-map (kbd "L") '("load+geom" . easysession-switch-to-and-restore-geometry))
(define-key my/leader-session-map (kbd "r") '("rename" . easysession-rename))
(define-key my/leader-session-map (kbd "u") '("unload" . easysession-unload))
(define-key my/leader-session-map (kbd "d") '("delete" . easysession-delete))

;; ----------------------------------------------------------- Quit --

(define-prefix-command 'my/leader-quit-map)
(define-key my/leader-map      (kbd "q") '("quit..." . my/leader-quit-map))
(define-key my/leader-quit-map (kbd "q") '("quit Emacs" . save-buffers-kill-terminal))
(define-key my/leader-quit-map (kbd "Q") '("force quit" . kill-emacs))
(define-key my/leader-quit-map (kbd "r") '("restart" . restart-emacs))
(define-key my/leader-quit-map (kbd "f") '("close frame" . delete-frame))

(provide 'chadmacs-leader)
;;; chadmacs-leader.el ends here
