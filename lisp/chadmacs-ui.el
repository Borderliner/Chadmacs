;;; chadmacs-ui.el --- Frame, theme, modeline, dashboard -*- lexical-binding: t; -*-
;;
;; Commentary:
;;   Visual layer: frame parameters and padding, fallback glyphs, divider,
;;   nano-modeline, doom-themes (Monokai Pro default), dashboard, smooth
;;   scrolling, nerd-icons, diff-hl, page-break-lines, which-key.
;;
;;; Code:

;; --------------------------------------------------------- Frame & layout --

;; Apply to all new frames (including the initial one if in early-init.el)
(add-to-list 'default-frame-alist '(min-height            . 1))
(add-to-list 'default-frame-alist '(height                . 45))
(add-to-list 'default-frame-alist '(min-width             . 1))
(add-to-list 'default-frame-alist '(width                 . 100))
(add-to-list 'default-frame-alist '(internal-border-width . 18))
(add-to-list 'default-frame-alist '(left-fringe           . 1))
(add-to-list 'default-frame-alist '(right-fringe          . 1))

;; Slim fringes
(setq-default left-fringe-width  1)
(setq-default right-fringe-width 1)

;; Fallback glyph for truncation/wrap (clean look). Family is intentionally
;; left unset: the glyph uses the default face's font, which is chosen by
;; `my/apply-font' below. Inheriting from font-lock-comment-face is enough
;; to render the glyph in the right tone.
(require 'disp-table)
(defface my-fallback-glyph
  '((t :inherit font-lock-comment-face))
  "Fallback face for truncation and wrap glyphs.")

(my/when-graphical
 (lambda ()
   (set-display-table-slot standard-display-table 'truncation
                           (make-glyph-code ?… 'my-fallback-glyph))
   (set-display-table-slot standard-display-table 'wrap
                           (make-glyph-code ?↩ 'my-fallback-glyph))))

;; No ugly widget buttons (nice with custom themes)
(setq widget-image-enable nil)

;; Hide org emphasis markers in buffers (clean reading)
(setq org-hide-emphasis-markers t)

;; Display line numbers globally
(global-display-line-numbers-mode 1)

;; Slim vertical divider on right only
(setq window-divider-default-right-width 1)
(setq window-divider-default-places 'right-only)
(window-divider-mode 1)

;; ------------------------------------------------------------- Modeline --

;; doom-modeline replaces the default modeline (and nano-modeline before
;; it). Cleaner layout, nerd-icons integration, projectile / VC / LSP /
;; flycheck segments out of the box, and no "function not known to be
;; defined" noise for optional integrations.
;;
;; `:demand t' + explicit `(doom-modeline-mode 1)' in :config is required
;; with Elpaca: hooking onto `after-init-hook' is too late because
;; Elpaca processes its install queue at that hook, so on a fresh boot
;; doom-modeline isn't loaded yet when after-init fires - the modeline
;; stays the default until the *next* restart. Activating in :config
;; runs after Elpaca finishes installing on the same boot.
(use-package doom-modeline
  :ensure t
  :demand t
  :custom
  (doom-modeline-height 28)
  (doom-modeline-bar-width 3)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count nil)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-vcs-max-length 12)
  (doom-modeline-workspace-name nil)
  (doom-modeline-lsp t)
  (doom-modeline-time nil)
  (doom-modeline-percent-position nil)
  (doom-modeline-buffer-encoding nil)
  :config
  ;; Safety: if nano-modeline still lingers from a previous install,
  ;; turn it off so it doesn't fight doom-modeline for the header line.
  (when (fboundp 'nano-modeline-mode)
    (nano-modeline-mode -1))
  (doom-modeline-mode 1))

;; ---------------------------------------------------------------- Font --

;; The default font is applied from `after-init-hook', i.e. after custom.el
;; (loaded at the end of init.el), so the two variables below can be set
;; there. It used to be set from doom-themes' :config, which Elpaca runs
;; after init - silently overriding any font chosen in custom.el.
(defvar my/font nil
  "Default font family, e.g. \"Iosevka\".
When nil, use the first installed variant in `my/font-candidates'.
Set it in custom.el.")

(defvar my/font-height 110
  "Height of the default face in 1/10 pt (110 = 11pt).
Set it in custom.el.")

;; Different packagers use different family names for the same font -
;; nerd-fonts upstream calls it "JetBrainsMono Nerd Font", but Ubuntu's apt
;; package and Homebrew's tap label it "JetBrainsMono NF", and a few
;; packagers strip spaces.
(defconst my/font-candidates
  '("JetBrainsMono Nerd Font"
    "JetBrainsMono NF"
    "JetBrainsMonoNerdFont"
    "JetBrains Mono Nerd Font"
    "JetBrainsMono Nerd Font Mono")
  "Font families tried in order when `my/font' is nil.")

;; When the font isn't installed, `my/font-height' still applies to the
;; font Emacs falls back to. It used to be dropped along with the family,
;; leaving Emacs's stock ~10pt with no hint why.
(defun my/available-font ()
  "Return `my/font', or the first installed candidate, if it's installed.
Needs a graphical frame selected: font lookup fails on a text terminal."
  (seq-find (lambda (f) (find-font (font-spec :name f)))
            (if my/font (list my/font) my/font-candidates)))

(defun my/apply-font ()
  "Set the default face to `my/font' (or a candidate) at `my/font-height'.
If no such font is installed, apply the height to the current font and
say so. Return the font family applied, or nil."
  (let ((font (my/available-font)))
    (if font
        (set-face-attribute 'default nil :font font :height my/font-height)
      (set-face-attribute 'default nil :height my/font-height)
      (message "Chadmacs: %s is not installed; using %s"
               (or my/font (car my/font-candidates))
               (face-attribute 'default :family)))
    font))

;; Missing JetBrainsMono Nerd Font is downloaded from the nerd-fonts
;; release into the user's font directory (no root needed) - in the
;; background, so startup doesn't wait on the network, and only the
;; regular family (16 files, ~40MB unpacked, not all 98). Only when
;; `my/font' is unset: a font you picked yourself is never replaced.
(defvar my/install-missing-fonts t
  "Non-nil to download the fonts Chadmacs uses when they're missing.
Set it to nil in custom.el to install fonts yourself.")

(defconst my/jetbrains-mono-url
  "https://github.com/ryanoasis/nerd-fonts/releases/latest/download/JetBrainsMono.tar.xz"
  "Nerd-fonts release archive holding JetBrainsMono Nerd Font.")

(defun my/jetbrains-mono-directory ()
  "Return the directory to install JetBrainsMono Nerd Font into, or nil.
nil means this system has no per-user font directory Chadmacs knows of."
  (pcase system-type
    ((or 'gnu 'gnu/linux 'gnu/kfreebsd 'berkeley-unix)
     (expand-file-name "fonts/JetBrainsMonoNerdFont/"
                       (or (getenv "XDG_DATA_HOME") "~/.local/share")))
    ('darwin (expand-file-name "~/Library/Fonts/"))))

;; $1 = archive URL, $2 = destination directory.
(defconst my/jetbrains-mono-install-script
  "set -e
tmp=$(mktemp -d)
trap 'rm -rf \"$tmp\"' EXIT
curl -fsSL --retry 2 -o \"$tmp/font.tar.xz\" \"$1\"
tar -xJf \"$tmp/font.tar.xz\" -C \"$tmp\" \\
  $(tar -tJf \"$tmp/font.tar.xz\" | grep '^JetBrainsMonoNerdFont-.*\\.ttf$')
mkdir -p \"$2\"
cp \"$tmp\"/JetBrainsMonoNerdFont-*.ttf \"$2\"
if command -v fc-cache >/dev/null 2>&1; then fc-cache -f \"$2\"; fi"
  "Shell script that downloads JetBrainsMono Nerd Font and installs it.")

(defun my/graphical-frame ()
  "Return a live graphical frame, preferring the selected one, or nil."
  (if (display-graphic-p)
      (selected-frame)
    (seq-find #'display-graphic-p (frame-list))))

(defun my/install-jetbrains-mono ()
  "Download and install JetBrainsMono Nerd Font in the background.
Apply it once installed; if Emacs can't see it yet, ask for a restart."
  (let ((dir (my/jetbrains-mono-directory))
        (log "*Chadmacs font install*"))
    (cond
     ((get-process "chadmacs-font-install"))
     ((not dir)
      (message "Chadmacs: install JetBrainsMono Nerd Font from %s"
               "https://www.nerdfonts.com/font-downloads"))
     ((not (and (executable-find "curl") (executable-find "tar")))
      (message "Chadmacs: curl and tar are needed to download JetBrainsMono Nerd Font"))
     (t
      (message "Chadmacs: downloading JetBrainsMono Nerd Font...")
      (make-process
       :name "chadmacs-font-install"
       :buffer (with-current-buffer (get-buffer-create log)
                 (erase-buffer)
                 (current-buffer))
       :command (list "sh" "-c" my/jetbrains-mono-install-script
                      "sh" my/jetbrains-mono-url dir)
       :sentinel
       (lambda (proc _event)
         (unless (process-live-p proc)
           (if (/= (process-exit-status proc) 0)
               (message "Chadmacs: JetBrainsMono Nerd Font download failed; see %s"
                        log)
             (kill-buffer log)
             (if-let* ((frame (my/graphical-frame))
                       ((with-selected-frame frame (my/available-font))))
                 (progn
                   (with-selected-frame frame (my/apply-font))
                   (message "Chadmacs: installed JetBrainsMono Nerd Font in %s" dir))
               (message "Chadmacs: installed JetBrainsMono Nerd Font in %s; restart Emacs to use it"
                        dir))))))))))

;; nerd-icons draws the modeline, dashboard, completion and Dirvish icons
;; from its own symbol font (`nerd-icons-font-family'); without it they
;; render as boxes. Run its installer when that font is missing - from an
;; idle timer, because `nerd-icons-install-fonts' downloads synchronously.
;; It only knows where to install on Linux and macOS (elsewhere it asks
;; for a directory), so other systems are left alone.
(defun my/install-nerd-icons-font-maybe ()
  "Run `nerd-icons-install-fonts' if the nerd-icons font is missing."
  (when-let* ((frame (my/graphical-frame)))
    (when (and (memq system-type '(gnu gnu/linux gnu/kfreebsd darwin))
               (require 'nerd-icons nil t)
               (not (with-selected-frame frame
                      (find-font (font-spec :name nerd-icons-font-family)))))
      (condition-case err
          (progn
            (nerd-icons-install-fonts t)
            (message "Chadmacs: installed the nerd-icons font; restart Emacs to see the icons"))
        (error
         (message "Chadmacs: couldn't install the nerd-icons font: %s"
                  (error-message-string err)))))))

(defun my/setup-fonts ()
  "Apply the default font, downloading missing fonts if allowed.
See `my/install-missing-fonts'."
  (unless (or (my/apply-font) my/font (not my/install-missing-fonts))
    (my/install-jetbrains-mono))
  (when my/install-missing-fonts
    (run-with-idle-timer 2 nil #'my/install-nerd-icons-font-maybe)))

(add-hook 'after-init-hook
          (lambda () (my/when-graphical #'my/setup-fonts)))

;; --------------------------------------------------------------- Theme --

(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-monokai-pro t)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;; ----------------------------------------------------------- Dashboard --

;; Dashboard: startup screen with a custom ASCII banner, action navigator,
;; and lists of recents / projects / bookmarks. Designed to work identically
;; on GUI and TTY - the banner is an embedded UTF-8 string (block characters
;; from the Unicode Box Drawing set + one dot / dash decoration line), no
;; PNG or SVG anywhere. Nerd-icon glyphs in the navigator degrade to plain
;; text if the terminal lacks a Nerd Font.
(defconst my/dashboard-banner
  "
 ██████╗██╗  ██╗ █████╗ ██████╗ ███╗   ███╗ █████╗  ██████╗███████╗
██╔════╝██║  ██║██╔══██╗██╔══██╗████╗ ████║██╔══██╗██╔════╝██╔════╝
██║     ███████║███████║██║  ██║██╔████╔██║███████║██║     ███████╗
██║     ██╔══██║██╔══██║██║  ██║██║╚██╔╝██║██╔══██║██║     ╚════██║
╚██████╗██║  ██║██║  ██║██████╔╝██║ ╚═╝ ██║██║  ██║╚██████╗███████║
 ╚═════╝╚═╝  ╚═╝╚═╝  ╚═╝╚═════╝ ╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝

              ── minimal  ·  fast  ·  intentional ──

"
  "Chadmacs' ASCII dashboard banner (block-character CHADMACS wordmark).
Fed to `dashboard' as its startup banner via `dashboard-banner-ascii'.")

(use-package dashboard
  :ensure t
  :after (projectile nerd-icons)
  :init
  ;; --- Data / layout ---
  (setq dashboard-projects-backend         'projectile
        dashboard-startup-banner           'ascii  ; use the embedded string below
        dashboard-banner-ascii             my/dashboard-banner
        dashboard-banner-logo-title        nil     ; tagline is in the banner
        dashboard-center-content           t
        dashboard-vertically-center-content t
        dashboard-navigation-cycle         t
        dashboard-show-shortcuts           t
        dashboard-heading-shorcut-format   "  %s"
        dashboard-display-icons-p          t
        dashboard-icon-type                'nerd-icons
        ;; Which sections appear (init info, navigator, footer, ...) is
        ;; decided by `dashboard-startupify-list' below.
        dashboard-set-heading-icons        t
        dashboard-set-file-icons           t)

  ;; --- Init info line: "◆ ready in 1.42s · 3 GCs" ---
  (setq dashboard-init-info
        (lambda ()
          (format "◆ ready in %.2fs  ·  %d GCs"
                  (my/startup-seconds)
                  gcs-done)))

  ;; --- Footer: one message picked at random each startup ---
  ;; The original Chadmacs taglines plus a large pool of deep zen, strategy,
  ;; and stoic quotes. NOTE: the banner already carries the "minimal · fast ·
  ;; intentional" tagline, so the pool skips it to avoid restating the theme.
  (setq dashboard-footer-icon "◆")
  (setq dashboard-footer-messages
        '(;; --- Chadmacs ---
          "built to last  ·  tuned to be sharp"
          "sharp tools for sharp minds"
          "the config other configs look up to"
          "you are not your framework"
          "sharp edges, no bloat"
          "ultra clean  ·  ultra fast  ·  ultra chad"
          ;; --- Zen & Tao ---
          "The obstacle is the path.  — Zen proverb"
          "Let go, or be dragged.  — Zen proverb"
          "When walking, walk. When eating, eat.  — Zen proverb"
          "No snowflake ever falls in the wrong place.  — Zen proverb"
          "Knock on the sky and listen to the sound.  — Zen proverb"
          "When the student is ready, the teacher appears.  — Zen proverb"
          "Sitting quietly, doing nothing, spring comes and the grass grows by itself.  — Zen proverb"
          "Muddy water is best cleared by leaving it alone.  — Alan Watts"
          "The quieter you become, the more you can hear.  — Ram Dass"
          "Fall seven times, stand up eight.  — Japanese proverb"
          "The bamboo that bends is stronger than the oak that resists.  — Japanese proverb"
          "Nature does not hurry, yet everything is accomplished.  — Lao Tzu"
          "To the mind that is still, the whole universe surrenders.  — Lao Tzu"
          "Mastering others is strength; mastering yourself is true power.  — Lao Tzu"
          "Silence is a source of great strength.  — Lao Tzu"
          "A good traveler has no fixed plans and is not intent on arriving.  — Lao Tzu"
          "Do not seek to follow the wise; seek what they sought.  — Bashō"
          ;; --- War & strategy ---
          "The supreme art of war is to subdue the enemy without fighting.  — Sun Tzu"
          "In the midst of chaos, there is also opportunity.  — Sun Tzu"
          "Victorious warriors win first and then go to war.  — Sun Tzu"
          "The greatest victory is that which requires no battle.  — Sun Tzu"
          "Know yourself and you will win all battles.  — Sun Tzu"
          "Opportunities multiply as they are seized.  — Sun Tzu"
          "Move swift as the wind, stay silent as the forest.  — Sun Tzu"
          "The wise warrior avoids the battle.  — Sun Tzu"
          "Do nothing which is of no use.  — Miyamoto Musashi"
          "Perceive that which cannot be seen with the eye.  — Miyamoto Musashi"
          "Today is victory over yourself of yesterday.  — Miyamoto Musashi"
          "There is more than one path to the top of the mountain.  — Miyamoto Musashi"
          "It is not the mountain we conquer, but ourselves.  — Edmund Hillary"
          "Amateurs talk strategy; professionals talk logistics.  — military maxim"
          "No plan survives contact with the enemy.  — Helmuth von Moltke"
          "He who defends everything defends nothing.  — Frederick the Great"
          "Discipline equals freedom.  — Jocko Willink"
          ;; --- Stoic & philosophical ---
          "What stands in the way becomes the way.  — Marcus Aurelius"
          "You have power over your mind, not outside events.  — Marcus Aurelius"
          "Waste no more time arguing what a good man should be. Be one.  — Marcus Aurelius"
          "The best revenge is to be unlike him who performed the injury.  — Marcus Aurelius"
          "We suffer more often in imagination than in reality.  — Seneca"
          "Luck is what happens when preparation meets opportunity.  — Seneca"
          "It is not that we have a short time to live, but that we waste much of it.  — Seneca"
          "The man who moves a mountain begins by carrying away small stones.  — Confucius"
          "It does not matter how slowly you go, so long as you do not stop.  — Confucius"
          "The journey of a thousand miles begins with a single step.  — Lao Tzu"
          "He who has a why to live can bear almost any how.  — Nietzsche"
          "That which does not kill us makes us stronger.  — Nietzsche"
          "Those who were seen dancing were thought insane by those who could not hear the music.  — Nietzsche"
          "The cave you fear to enter holds the treasure you seek.  — Joseph Campbell"
          "Out of the mountain of despair, a stone of hope.  — Martin Luther King Jr."
          "First we make our habits, then our habits make us.  — John Dryden"))

  ;; --- Section list & shortcut letters ---
  (setq dashboard-items '((recents   . 5)
                          (projects  . 5)
                          (bookmarks . 3)))
  (setq dashboard-item-shortcuts '((recents   . "r")
                                   (projects  . "p")
                                   (bookmarks . "m")))

  ;; --- Assembly order: banner → info → navigator → items → footer ---
  (setq dashboard-startupify-list
        '(dashboard-insert-banner
          dashboard-insert-newline
          dashboard-insert-init-info
          dashboard-insert-newline
          dashboard-insert-navigator
          dashboard-insert-newline
          dashboard-insert-items
          dashboard-insert-newline
          dashboard-insert-footer))

  :config
  ;; Navigator is defined here (not :init) so nerd-icons is guaranteed
  ;; loaded - dashboard :after already pulled it, but the icon glyphs
  ;; are evaluated eagerly and would nil-out at :init if we lost the race.
  (require 'nerd-icons)
  (let ((icon (lambda (name)
                (nerd-icons-octicon name :face 'font-lock-keyword-face))))
    (setq dashboard-navigator-buttons
          `(((,(funcall icon "nf-oct-file_directory")
              "Files" "Open a file manager (dirvish)"
              (lambda (&rest _) (dirvish)))
             (,(funcall icon "nf-oct-search")
              "Search" "Ripgrep in the current project"
              (lambda (&rest _) (call-interactively #'consult-ripgrep)))
             (,(funcall icon "nf-oct-terminal")
              "Terminal" "Open a Ghostel terminal"
              (lambda (&rest _) (call-interactively #'ghostel)))
             (,(funcall icon "nf-oct-repo")
              "Projects" "Switch to another project"
              (lambda (&rest _)
                (call-interactively #'projectile-switch-project)))))))

  (dashboard-setup-startup-hook))

;; --------------------------------------------------- Scrolling & icons --

;; ultra-scroll: precise pixel-by-pixel scrolling for trackpads and mouse
;; wheels. Lives on GitHub only (not MELPA), so the recipe is pinned
;; explicitly. Internally drives `pixel-scroll-precision-mode' - do NOT
;; enable that mode separately or the two will fight each other.
;;
;; `scroll-conservatively' MUST be > 100 for ultra-scroll to feel right:
;; any value <= 100 lets Emacs recenter the buffer when point scrolls
;; off-screen, which produces a jarring snap on top of the smooth pixel
;; scroll. The upstream README flags this as the #1 gotcha. We set 101
;; (the canonical "disable recentering" sentinel).
;;
;; ultra-scroll auto-disables on TTY (no graphics) so it's safe to enable
;; unconditionally; the mode is a no-op in terminal Emacs.
(use-package ultra-scroll
  :ensure (ultra-scroll :host github :repo "jdtsmith/ultra-scroll")
  :init
  (setq scroll-conservatively 101
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

;; Third-of-a-page scroll instead of full-page for `C-v' / `M-v' (and any
;; other key bound to scroll-up-command / scroll-down-command, including
;; <next> and <prior>). Stock Emacs scrolls (window-height -
;; next-screen-context-lines) lines per command - almost a full screen -
;; which makes it easy to lose orientation in deep code. A 1/3 jump stays
;; close enough that the surrounding context stays familiar.
;;
;; Using [remap ...] (instead of `(kbd "C-v")') makes the wrappers follow
;; the commands wherever they're bound, including keys other modes attach
;; them to. The `max 1' guard avoids a zero-scroll in tiny minibuffers.
;; A prefix argument is passed through untouched, so `C-u 5 C-v' still
;; scrolls five lines, and the "^P" spec keeps shift-selection working.
(defun my/scroll-third-page-down (&optional arg)
  "Scroll forward 1/3 of a window (like `C-v', but a third of the distance).
With a prefix argument ARG, scroll ARG lines, as `scroll-up-command' does."
  (interactive "^P")
  (scroll-up-command (or arg (max 1 (/ (window-body-height) 3)))))

(defun my/scroll-third-page-up (&optional arg)
  "Scroll backward 1/3 of a window (like `M-v', but a third of the distance).
With a prefix argument ARG, scroll ARG lines, as `scroll-down-command' does."
  (interactive "^P")
  (scroll-down-command (or arg (max 1 (/ (window-body-height) 3)))))

(global-set-key [remap scroll-up-command]   #'my/scroll-third-page-down)
(global-set-key [remap scroll-down-command] #'my/scroll-third-page-up)

;; Nerd icons in completion (looks great with nano)
(use-package nerd-icons-completion
  :ensure t
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)

  ;; nerd-icons-completion has hardcoded icon names (e.g. `nf-oct-folder_cache')
  ;; that drift when upstream nerd-icons renames or removes glyphs. Without
  ;; this advice, opening `find-file' in a directory containing `.cache/'
  ;; (and a few other names) raises and breaks vertico. Swallow the lookup
  ;; error and return a blank string so completion still renders.
  (dolist (fn '(nerd-icons-octicon
                nerd-icons-faicon
                nerd-icons-codicon
                nerd-icons-mdicon
                nerd-icons-devicon
                nerd-icons-sucicon
                nerd-icons-flicon
                nerd-icons-wicon
                nerd-icons-pomicon
                nerd-icons-ipsicon))
    (when (fboundp fn)
      (advice-add fn :around
                  (lambda (orig &rest args)
                    (condition-case nil
                        (apply orig args)
                      (error "")))))))

;; ---------------------------------------------------- Diff-hl & breaks --

;; Git diff showing in the fringe
(use-package diff-hl
  :ensure t
  :commands (diff-hl-mode global-diff-hl-mode)
  :hook
  (prog-mode . diff-hl-mode)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :init
  (setq diff-hl-show-staged-changes nil)
  (setq diff-hl-update-async t))

;; Visible indicators for page breaks
(use-package page-break-lines
  :ensure t
  :commands (page-break-lines-mode global-page-break-lines-mode)
  :hook
  (lisp-data-mode . page-break-lines-mode))

;; ----------------------------------------------------------- which-key --

;; Popup showing key completions after a prefix. Built in since Emacs 30,
;; so only fetched on older Emacs; installing it anyway shadowed the
;; built-in copy with a second one. (Elpaca's :ensure evaluates a
;; backquoted value: t -> install, nil -> use what ships with Emacs.)
(use-package which-key
  :ensure `,(< emacs-major-version 30)
  :demand t
  :custom
  (which-key-idle-delay 0.4)
  (which-key-popup-type 'minibuffer)
  ;; `C-h' after a prefix belongs to Embark (`embark-prefix-help-command',
  ;; chadmacs-completion): a searchable list of the prefix's bindings.
  ;; Without this, which-key claimed `prefix-help-command' too, and which
  ;; one won depended only on which module happened to load last.
  (which-key-use-C-h-commands nil)
  :config
  (which-key-mode 1))

(provide 'chadmacs-ui)
;;; chadmacs-ui.el ends here
