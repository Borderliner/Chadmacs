# 🗿 Chadmacs

> Borderliner's **ultra-clean, ultra-fast, ultra-chad Emacs setup**.

Minimal chrome. Smooth UI. Modern completion. LSP ready. Tree-sitter powered.

![Chadmacs](https://i.imgur.com/IKNrO3u.png)

---

## ⚡ Philosophy

Chadmacs is built around a few non-negotiables:

* 🚀 Fast startup
* 🧼 Clean visuals (no visual noise)
* 📦 Reproducible package management (Elpaca)
* 🧠 Modern completion stack (Vertico + Orderless + Consult + Embark + Corfu)
* 🌲 Tree-sitter first
* 💾 Proper persistence (everything in `~/.emacs.d/var/`)

No spaghetti. No distro bloat. Just Emacs — refined.

---

# 🏗 Architecture

```
~/.emacs.d/
├── early-init.el          # GC, Elpaca bootstrap, paths
├── init.el                # thin loader; just (require 'chadmacs-*)
├── lisp/                  # config modules
│   ├── chadmacs-core.el         # sane defaults, persistence, optimisation
│   ├── chadmacs-ui.el           # frame, theme, modeline, dashboard
│   ├── chadmacs-completion.el   # vertico/consult/corfu/cape/embark
│   ├── chadmacs-editing.el      # undo, sessions, snippets, paredit, etc.
│   ├── chadmacs-tools.el        # projectile/magit/flycheck/eglot/envrc
│   ├── chadmacs-org.el          # org, buffer-terminator
│   ├── chadmacs-navigation.el   # treemacs, dirvish, popper, vterm
│   ├── chadmacs-help.el         # helpful
│   └── chadmacs-leader.el       # Doom-style C-c SPC menu
├── extensions/            # language-specific modules
│   └── rust-extension.el
├── custom.el              # gitignored — your local overrides
└── var/                   # every auto-generated file lives here
    ├── elpaca/
    ├── eln/
    ├── backup/
    ├── auto-save/
    ├── easysession/
    ├── tree-sitter/
    ├── etc/
    ├── url/
    ├── history, recentf, places, bookmarks, projects
    └── ...
```

All generated state lives inside `var/`. The root stays clean. The
modular `lisp/` layout makes it easy to fork or disable individual
subsystems without touching anything else.

---

# 🚀 Features

## 🧼 Clean UI

* No menu bar
* No tool bar
* No scroll bars
* Internal frame padding
* Slim fringes
* Nano modeline
* Doom Monokai Pro theme
* JetBrainsMono Nerd Font
* Clean truncation glyphs (…)

## 🧠 Completion Stack

| Tool       | Purpose                                  |
| ---------- | ---------------------------------------- |
| Vertico    | Vertical completion UI                   |
| Orderless  | Flexible matching                        |
| Consult    | Rich commands (M-x, grep, buffers, etc.) |
| Embark     | Context actions (`C-.`)                  |
| Marginalia | Annotations                              |
| Corfu      | In-buffer completion                     |
| Cape       | Extra completion sources                 |

Modern. Fast. Cohesive.

## 🧑‍💻 Dev Setup

* Eglot (LSP) — auto-starts for C/C++/C#, Python, JS/TS/TSX, Go, Bash, YAML, Rust
* Flycheck + posframe + flycheck-eglot (diagnostics from LSP into flycheck)
* Apheleia (async format-on-save)
* Tree-sitter auto install (prompts on first open)
* Magit
* Diff-hl
* envrc (per-project `.envrc` / direnv — auto PATH/env per repo)
* dirvish (dired with sidebar + preview + git-state)
* Treemacs (sidebar file tree)
* vterm (libvterm terminal)
* popper (toggleable popup windows for *Help*, compile, eldoc, …)

## 📝 Editing Enhancements

* undo-fu + session persistence
* vundo (visual undo tree)
* Yasnippet
* Strip trailing whitespace
* Aggressive indent (Lisp)
* Paredit
* expand-region, avy, ace-window, crux
* multiple-cursors
* move-text (M-up/M-down)
* wgrep (edit grep results in place)
* which-key (key discoverability)

## 💾 Session & Persistence

* easysession (workspace restore)
* savehist
* save-place
* recentf
* bookmark persistence
* tramp history

Everything survives restarts.

---

# 🔧 Installation

### 1️⃣ Backup old config

```bash
mv ~/.emacs.d ~/.emacs.d.backup
```

### 2️⃣ Clone Chadmacs

```bash
git clone https://github.com/Borderliner/Chadmacs.git ~/.emacs.d
```

### 3️⃣ Launch Emacs

🪄 First launch bootstraps **Elpaca** automatically.

🍳 Sit back. Let it cook.

⌛ After **Elpaca** installation, check **async-compilation** buffer (C-x b), and wait for everything to be compiled.

🔁 Once done, ignore warnings, **Restart** Emacs.

---

# 🧭 Keybindings Cheat Sheet

## 🎯 Leader Key (Doom-style)

`C-c SPC` opens a tree of operations grouped by domain. Combined with
`which-key` you get Doom/Spacemacs-style discoverability without evil.

| Key              | Action                          |
| ---------------- | ------------------------------- |
| `C-c SPC SPC`    | M-x (extended command)          |
| `C-c SPC /`      | Ripgrep in project              |
| `C-c SPC .`      | Find file (project-aware)       |
| `C-c SPC ,`      | Switch buffer                   |
| `C-c SPC b`      | Buffer submap                   |
| `C-c SPC f`      | File submap                     |
| `C-c SPC g`      | Git (magit) submap              |
| `C-c SPC p`      | Project submap                  |
| `C-c SPC s`      | Search submap                   |
| `C-c SPC w`      | Window submap                   |
| `C-c SPC t`      | Toggle submap                   |
| `C-c SPC h`      | Help submap (helpful)           |
| `C-c SPC o`      | Open submap                     |
| `C-c SPC S`      | Session (easysession) submap    |
| `C-c SPC q`      | Quit / restart submap           |

Hit any prefix and pause; `which-key` lists every binding with labels.

## 🪟 Frames / Windows

| Key             | Action                          |
| --------------- | ------------------------------- |
| `M-n`           | New frame                       |
| `` M-` ``       | Switch frame                    |
| `M-RET`         | Toggle fullscreen               |
| `M-o`           | Switch window (ace-window)      |
| `C-c <left>`    | Undo window layout (winner)     |
| `C-c <right>`   | Redo window layout (winner)     |
| `` C-` ``       | Toggle latest popup (popper)    |
| `C-M-<`         | Cycle popups                    |
| `` C-M-` ``     | Convert window <-> popup        |

## 🗂 Files / Buffers

| Key       | Action         |
| --------- | -------------- |
| `C-x b`   | Consult buffer |
| `C-x C-f` | Smart find-file (project-aware) |
| `C-x C-r` | Recent files   |
| `C-x g`   | Magit status   |
| `M-s d`   | Find files     |
| `M-s r`   | Ripgrep        |
| `M-y`     | Yank pop       |

## 🎯 Navigation

| Key      | Action                |
| -------- | --------------------- |
| `M-g j`  | Avy jump to char      |
| `M-g w`  | Avy jump to word      |
| `M-g l`  | Avy jump to line      |
| `M-g g`  | Goto line             |
| `M-g i`  | Imenu                 |
| `M-g f`  | Flycheck errors       |

## ✏️ Editing

| Key            | Action                          |
| -------------- | ------------------------------- |
| `C-=`          | Expand region                   |
| `C--`          | Contract region                 |
| `C-a`          | Smart beginning of line (crux)  |
| `M-<up>/<down>`| Move line / region              |
| `C-c d`        | Duplicate line / region         |
| `C-c o`        | Open line below                 |
| `C-c O`        | Open line above                 |
| `C-S-k`        | Kill whole line                 |
| `C-x u`        | Visual undo tree (vundo)        |
| `M-/`          | Completion-at-point             |
| `C->`          | Mark next like this (mc)        |
| `C-<`          | Mark previous like this (mc)    |
| `C-S-c C-S-c`  | Edit lines as multiple cursors  |

## ⚡ Embark (right-click menu)

Embark turns any minibuffer candidate or symbol-at-point into a context
menu. Try selecting a file in `consult-buffer` and pressing `C-.` — you
get rename / copy-path / open-other-window / etc. without leaving the
completion UI.

| Key     | Action                                       |
| ------- | -------------------------------------------- |
| `C-.`   | Embark act (context menu at point)           |
| `C-;`   | Embark dwim (apply default action)           |
| `C-h B` | Show all keybindings (embark-bindings)       |

Inside a minibuffer:
* `C-.` — list actions on the current candidate
* `C-c C-e` — export results to an editable buffer (combine with `wgrep`
  to mass-edit grep matches)

## 🌳 Treemacs (prefix `C-c T`)

| Key            | Action                  |
| -------------- | ----------------------- |
| `M-0`          | Select treemacs window  |
| `C-c T t`      | Toggle treemacs         |
| `C-c T 1`      | Delete other windows    |
| `C-c T d`      | Select directory        |
| `C-c T B`      | Treemacs bookmark       |
| `C-c T C-t`    | Find current file       |
| `C-c T M-t`    | Find tag                |

## 📅 Org

| Key      | Action          |
| -------- | --------------- |
| `C-c a`  | Org agenda      |
| `C-c c`  | Org capture     |

## 🧠 LSP (prefix `C-c l`)

Eglot auto-starts in the following major modes. You must install the
language server binary yourself (Chadmacs doesn't bundle them):

| Language       | Mode                                       | Server binary                                 |
| -------------- | ------------------------------------------ | --------------------------------------------- |
| C / C++        | `c-mode`, `c++-mode`                       | clangd                                        |
| C#             | `csharp-ts-mode`                           | omnisharp / csharp-ls                         |
| Python         | `python-mode`, `python-ts-mode`            | pylsp / pyright / ruff                        |
| JS / TS / TSX  | `js-mode`, `typescript-ts-mode`, `tsx-...` | typescript-language-server                    |
| Go             | `go-mode`, `go-ts-mode`                    | gopls                                         |
| Bash / Shell   | `sh-mode`, `bash-ts-mode`                  | bash-language-server                          |
| YAML           | `yaml-mode`, `yaml-ts-mode`                | yaml-language-server                          |
| Rust           | `rustic-mode`                              | rust-analyzer                                 |
| Markdown (opt) | `markdown-mode` (uncomment hook)           | marksman                                      |

| Key       | Action            |
| --------- | ----------------- |
| `C-c l r` | Rename symbol     |
| `C-c l f` | Format buffer     |
| `C-c l a` | Code actions      |
| `C-c l h` | Eldoc at point    |

Eglot diagnostics flow into Flycheck via `flycheck-eglot`.

## 📁 Dirvish (dired replacement)

`dirvish-override-dired-mode` is enabled — every `dired` session is a
Dirvish session. `C-c f d` (or `C-c SPC f d`) opens it from anywhere.

| Key        | Action                              |
| ---------- | ----------------------------------- |
| `C-c f d`  | Open dirvish                        |
| `a`        | Quick-access entries menu           |
| `TAB`      | Toggle subtree                      |
| `s`        | Sort menu                           |
| `v`        | Version-control menu                |
| `y`        | Yank menu (copy / paste / link)     |
| `f`        | File info menu                      |
| `^`        | Jump to last directory              |
| `h`        | Jump to a recent directory          |
| `N`        | Narrow visible entries              |
| `M-t`      | Toggle layout (preview pane on/off) |
| `M-j`      | `fd` jump (fuzzy directory finder)  |

## 🪟 Popper popups

Bound buffers (`*Help*`, compile, `*Warnings*`, eldoc, eshell, vterm, …)
are managed as toggleable popups instead of hijacking your windows.

| Key            | Action                          |
| -------------- | ------------------------------- |
| `` C-` ``      | Toggle latest popup             |
| `C-M-<`        | Cycle through popups            |
| `` C-M-` ``    | Promote window to popup (or vice versa) |

## 🖥️ Terminal (vterm)

| Key      | Action                                |
| -------- | ------------------------------------- |
| `C-c v`  | Open vterm                            |
| `C-c V`  | Open vterm in other window            |

Requires `libvterm` + `cmake` at build time. On error during install:
`sudo pacman -S libvterm cmake` (or equivalent) then `M-x elpaca-rebuild vterm`.

## 🌿 envrc (per-project direnv)

When a project has a `.envrc`, envrc injects its environment into
Emacs's PATH / env so eglot, flycheck, and compile commands see the
correct toolchain.

| Key      | Action                                |
| -------- | ------------------------------------- |
| `C-c E`  | envrc command map (allow / reload / …) |

Requires `direnv` on PATH.

## 🎨 Themes

| Key        | Action                  |
| ---------- | ----------------------- |
| `C-c t T`  | Live theme switcher (consult-theme) |

## 🔌 Completion (cape prefix `C-c e`)

| Key      | Action                |
| -------- | --------------------- |
| `C-c e`  | Cape prefix map       |

## 📦 Sessions (easysession)

| Key       | Action            |
| --------- | ----------------- |
| `C-c sl`  | Load session      |
| `C-c ss`  | Save session      |
| `C-c sr`  | Rename session    |
| `C-c sd`  | Delete session    |

---

# 🌲 Tree-sitter

Grammars auto-install (prompt mode).

Stored in:

```
~/.emacs.d/var/tree-sitter/
```

Custom C# recipe included.

---

# 🎨 UI Customization

### Change Font

Inside `lisp/chadmacs-ui.el`:

```elisp
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 110)
```

### Change Theme

Replace:

```elisp
(load-theme 'doom-monokai-pro t)
```

With another Doom theme — or use `C-c t T` to preview interactively.

---

# 🧪 Performance Tricks

* 🚀 GC threshold raised during startup (gcmh adapts after)
* 🚀 File-name handlers disabled during init
* 🚀 Native compilation cache redirected
* 🚀 Startup time printed on emacs-startup-hook

Startup is snappy.

---

# 🧹 Minimalism Rules

Chadmacs avoids:

* Massive framework abstractions
* Doom/Spacemacs layers
* Excessive magic macros
* Hidden side effects

You own your config.
You understand your config.
You debug your config.

---

# 🗿 Why Chadmacs?

Because you don't want:

> "A distribution that configures Emacs for you."

You want:

> "A sharp and clean tool that respects your time."

---

# 📌 Requirements

* Emacs 29+
* Nerd Font installed
* Git
* Tree-sitter support enabled

Optional:

* ripgrep
* fd
* direnv (for envrc)
* libvterm + cmake (for vterm)
* language servers (per-language)

---

# 🛠 Troubleshooting

### Elpaca issues

Delete:

```
~/.emacs.d/var/elpaca/
```

Restart Emacs.

---

### Tree-sitter grammar issues

Delete:

```
~/.emacs.d/var/tree-sitter/
```

Reinstall grammar.

---

# 🧬 Future Improvements

* Better dashboard visuals
* More LSP integrations
* Optional Wayland tweaks
* Smarter modeline modules

---

# 👑 Final Words

Chadmacs is not a distro.
It's a mindset.

Minimal.
Fast.
Intentional.

Enjoy your editor. 🗿🔥
