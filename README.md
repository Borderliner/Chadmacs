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
│   ├── chadmacs-editing.el      # undo, sessions, snippets, smartparens, ...
│   ├── chadmacs-tools.el        # projectile/magit/flycheck/eglot/envrc
│   ├── chadmacs-org.el          # org, buffer-terminator
│   ├── chadmacs-navigation.el   # treemacs, dirvish, popper, ghostel
│   └── chadmacs-help.el         # helpful
├── extensions/            # opt-in language modules, one per language
├── bin/chadmacs           # CLI: update / doctor / treesit-clean / ...
├── install.sh             # one-shot installer (used by curl-install)
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
* Doom modeline
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
* ghostel (libghostty-vt terminal — replaces vterm; prebuilt native module, no cmake step)
* popper (toggleable popup windows for *Help*, compile, eldoc, …)

## 📝 Editing Enhancements

* undo-fu + session persistence
* vundo (visual undo tree)
* Yasnippet
* Strip trailing whitespace
* Aggressive indent (Lisp)
* smartparens (with paredit-style keybindings via `sp-use-paredit-bindings`)
* expand-region, avy, ace-window, crux
* multiple-cursors
* move-text (M-up/M-down)
* wgrep (edit grep results in place)
* which-key (key discoverability for every prefix)

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

### One-liner (recommended)

```bash
curl -fsSL https://raw.githubusercontent.com/Borderliner/Chadmacs/master/install.sh | bash
```

What it does:

1. Backs up any existing `~/.emacs.d` to `~/.emacs.d.backup-YYYYMMDD-HHMMSS`.
2. Clones Chadmacs into `~/.emacs.d`.
3. Adds `~/.emacs.d/bin` to `$PATH` for bash, zsh, and fish (idempotent).
4. Prints next steps.

### Manual

```bash
mv ~/.emacs.d ~/.emacs.d.backup
git clone https://github.com/Borderliner/Chadmacs.git ~/.emacs.d
~/.emacs.d/install.sh         # same wiring as the curl path
```

### Launch

🪄 First launch bootstraps **Elpaca** automatically.

🍳 Sit back. Let it cook.

⌛ After **Elpaca** installation, check `*elpaca-log*` buffer (`C-x b`), and wait for everything to be compiled.

🔁 Once done, ignore warnings, **Restart** Emacs.

---

# 🛠 The `chadmacs` CLI

Installed under `~/.emacs.d/bin/`. After installation the installer wires
this directory into your shell PATH. Available subcommands:

| Command              | Action                                                      |
| -------------------- | ----------------------------------------------------------- |
| `chadmacs help`      | Show usage.                                                 |
| `chadmacs update`    | `git pull` Chadmacs + run Elpaca fetch for installed pkgs.  |
| `chadmacs doctor`    | Check Emacs version, fonts, fd/rg/direnv/libvterm, LSP bins.|
| `chadmacs uninstall` | Remove `~/.emacs.d` after backing up `var/` to `$HOME`.     |

Set `CHADMACS_DIR` to override the install location.

---

# 🌍 Enabling Languages

Language support is opt-in. Each language has a self-contained file in
`extensions/<lang>-extension.el` listing its LSP binary in the header
comment. To enable one, edit `~/.emacs.d/custom.el` (auto-generated on
first launch, **gitignored** — your enabled list survives upstream pulls):

```elisp
;; --- Backend / scripting ---
(require 'python-extension)         ;; Python (+ pylsp / pyright)
(require 'go-extension)             ;; Go + gopls
(require 'elixir-extension)         ;; Elixir + HEEx + elixir-ls

;; --- Web stack ---
(require 'typescript-extension)     ;; TS / TSX / JS / JSX
(require 'web-extension)            ;; HTML / CSS / SCSS / Emmet
```

Full language list available out of the box: Rust, C/C++, C#, Python, Go,
TypeScript, JavaScript, Elixir, Ruby, Lua, Haskell, OCaml, Clojure, Zig,
Swift, Scala, Kotlin, Julia, Web (HTML/CSS/SCSS), YAML, JSON, Markdown,
Docker, Terraform, Nix, Gerbil.

---

# 🧭 Keybindings

Chadmacs uses **pure-Emacs conventions**. No leader key, no evil-style
chord trees. Every binding lives where its owning package lives, on one
of the standard prefixes:

| Prefix     | Owner                                      |
| ---------- | ------------------------------------------ |
| `C-x`      | Emacs core: files, buffers, windows, quit  |
| `C-c <letter>` | Package / user commands (Emacs convention) |
| `C-h`      | Help system (helpful overrides describe-*) |
| `M-g`      | "goto" family (lines, errors, marks, jumps)|
| `M-s`      | "search" family (line, ripgrep, grep, find)|
| `C-c p …`  | Projectile                                 |
| `C-c s …`  | easysession                                |
| `C-c T …`  | Treemacs                                   |
| `C-c l …`  | Eglot (mode-local in eglot buffers)        |
| `C-c e …`  | Cape extra completion sources              |

> 💡 **Discoverability.** `which-key` is on by default. Press any prefix
> (`C-x`, `C-c`, `C-h`, `M-g`, `M-s`, …) and pause — a popup lists every
> binding under that prefix with a human-readable description. Use this
> instead of memorizing a leader tree.

## Files & buffers (`C-x` — Emacs defaults, with smart upgrades)

| Key       | Action                                                  |
| --------- | ------------------------------------------------------- |
| `C-x C-f` | Smart find-file (project-aware fuzzy, plain `find-file` otherwise) |
| `C-x C-s` | Save                                                    |
| `C-x C-w` | Save as                                                 |
| `C-x C-r` | Recent files (consult)                                  |
| `C-x b`   | Switch buffer (consult-buffer)                          |
| `C-x k`   | Kill current buffer                                     |
| `C-x C-b` | ibuffer                                                 |
| `C-x d`   | Dirvish (replaces dired via `dirvish-override-dired-mode`) |
| `C-x u`   | Visual undo tree (vundo)                                |
| `C-x g`   | Magit status                                            |
| `C-x M-g` | Magit dispatch                                          |
| `M-y`     | Yank pop (consult)                                      |

## Search & jump (`M-s`, `M-g` — Emacs convention)

Consult's standard binds. `M-s` is "search across", `M-g` is "go to".

| Key         | Action                                                  |
| ----------- | ------------------------------------------------------- |
| `C-s` `C-r` | isearch forward / backward (Emacs default)              |
| `M-%`       | Query-replace (Emacs default)                           |
| `M-s l`     | consult-line (jump-to-line by content in current buffer)|
| `M-s L`     | consult-line-multi (across all buffers)                 |
| `M-s r`     | consult-ripgrep                                         |
| `M-s g`     | consult-grep                                            |
| `M-s G`     | consult-git-grep                                        |
| `M-s d`     | consult-find (fd file finder)                           |
| `M-s u`     | consult-focus-lines                                     |
| `M-s k`     | consult-keep-lines                                      |
| `M-s e`     | consult-isearch-history                                 |
| `M-g g`     | consult-goto-line                                       |
| `M-g i`     | consult-imenu                                           |
| `M-g I`     | consult-imenu-multi                                     |
| `M-g o`     | consult-outline                                         |
| `M-g m`     | consult-mark                                            |
| `M-g k`     | consult-global-mark                                     |
| `M-g f`     | consult-flycheck (errors)                               |
| `M-g e`     | consult-compile-error                                   |
| `M-g j`     | avy-goto-char-timer                                     |
| `M-g w`     | avy-goto-word-1                                         |
| `M-g l`     | avy-goto-line                                           |
| `C-c /`     | consult-ripgrep (top-level shortcut)                    |

## Help (`C-h` — `helpful` overrides describe-*)

| Key     | Action                                            |
| ------- | ------------------------------------------------- |
| `C-h f` | helpful-callable (functions + commands + macros)  |
| `C-h v` | helpful-variable                                  |
| `C-h k` | helpful-key                                       |
| `C-h o` | helpful-symbol                                    |
| `C-h B` | embark-bindings (every binding currently active)  |
| `C-h ?` | Help menu (the original Emacs help system stays)  |

## Editing

| Key             | Action                                          |
| --------------- | ----------------------------------------------- |
| `C-a`           | Smart beginning of line (crux)                  |
| `C-/` `C-z`     | Undo (Emacs default)                            |
| `M-/`           | completion-at-point                             |
| `M-;`           | comment line / region (Emacs default)           |
| `C-=`           | Expand region                                   |
| `C--`           | Contract region                                 |
| `M-<up>` `M-<down>` | Move line / region                          |
| `C-S-k`         | Kill whole line (crux)                          |
| `C-<backspace>` | Kill line backwards (crux)                      |
| `C-c d`         | Duplicate line / region (crux)                  |
| `C-c o`         | Open line below (crux)                          |
| `C-c O`         | Open line above (crux)                          |
| `C-c r`         | Rename file & buffer (crux)                     |
| `C-c D`         | Delete file & buffer (crux)                     |
| `C->`           | Multiple-cursors: mark next like this           |
| `C-<`           | Multiple-cursors: mark previous like this       |
| `C-c C-<`       | Multiple-cursors: mark all like this            |
| `C-S-c C-S-c`   | Multiple-cursors: edit lines                    |

Lisp buffers also get the full `smartparens` paredit-style keymap
(`C-)` / `C-(` slurp, `C-}` / `C-{` barf, `M-(` wrap, `M-s` splice,
`M-r` raise, `C-k` smart kill, etc).

## Windows & frames

Stock `C-x 0/1/2/3` window ops + a few canonical additions.

| Key           | Action                                  |
| ------------- | --------------------------------------- |
| `C-x 0/1/2/3` | Delete / only / split-below / split-right |
| `M-o`         | ace-window (jump to window by letter)   |
| `C-c <left>`  | Undo window layout (winner-mode)        |
| `C-c <right>` | Redo window layout                      |
| `M-n`         | New frame                               |
| `` M-` ``     | other-frame                             |
| `M-RET`       | toggle-frame-fullscreen                 |

## Popups (`popper`)

| Key          | Action                                         |
| ------------ | ---------------------------------------------- |
| `` C-` ``    | Toggle latest popup                            |
| `C-M-<`      | Cycle popups                                   |
| `` C-M-` ``  | Promote window ⇄ popup                         |

Targets: `*Help*`, `*Warnings*`, compile, eldoc, eshell, ghostel, …

## Terminal (`ghostel`)

Ghostel is a libghostty-vt-backed terminal that replaces vterm. First
`M-x ghostel` triggers an auto-download of a small prebuilt native
module (no cmake, no libvterm-dev). Everything after that just works —
true color, Kitty keyboard + graphics protocols (image previews in
tools like `yazi` work), hyperlinks, and shell integration for bash /
zsh / fish / nushell without any dotfile changes.

### Opening

| Key       | Action                                          |
| --------- | ----------------------------------------------- |
| `C-c v`   | Open a Ghostel terminal                         |
| `C-c V`   | Open a Ghostel terminal in another window       |
| `C-x p m` | Open Ghostel in the current project's root      |
| `C-x p M` | List Ghostel buffers in the current project     |

Also surfaces in the `C-x p p` project-switch dispatcher menu.

### Input modes (eat.el-style)

Ghostel offers five input modes. You'll spend most of your time in
`semi-char` (the default). Switch inside a Ghostel buffer:

| Key       | Mode        | What it does                                  |
| --------- | ----------- | --------------------------------------------- |
| `C-c C-j` | semi-char   | Default. Almost all keys go to the terminal.  |
| `C-c M-d` | char        | *All* keys go to the terminal. `M-RET` exits. |
| `C-c C-l` | line        | Emacs edits the line; ENTER sends it whole.   |
| `C-c C-e` | emacs       | Read-only Emacs nav over live output.         |
| `C-c C-t` | copy        | Freezes the terminal for selecting / copying. |

The read-only modes auto-exit on any key that would send input, so
"see something → `C-c C-t` → copy → keep typing" is one flow.

### Extensions (all on by default)

- **`ghostel-eshell-visual-command-mode`** — visual commands (`tmux`,
  `htop`, `less`, …) launched from eshell run inside a Ghostel buffer.
- **`ghostel-compile-global-mode`** — `M-x compile` and every derived
  compile command runs in a Ghostel buffer, so ANSI colors, escape
  codes, and progress bars render correctly.
- **`ghostel-comint-global-mode`** — replaces comint's built-in
  `ansi-color-process-output` with Ghostel's VT parser, so every
  comint-based shell (`M-x shell`, `M-x sql-*`, `M-x python-shell`,
  gud, …) gets true-color / escape handling.

### IME (opt-in)

Ghostel handles OS-level input methods (fcitx, ibus, macOS input
sources, Windows IME) natively. If you use an **Emacs Lisp** input
method (e.g. Korean Hangul via `M-x set-input-method`), turn on
`ghostel-ime-mode` too. In `custom.el`:

```elisp
(setq my/enable-ghostel-ime t)
```

then restart. See the `M-x describe-variable my/enable-ghostel-ime`
docstring for details.

### Shell integration

Directory tracking and prompt navigation are on automatically for
local bash / zsh / fish / nushell sessions. To call Emacs commands
from inside the terminal (e.g. `e file.txt` to open in Emacs), see
[ghostel's shell integration docs](https://dakra.github.io/ghostel/#shell-integration)
for the small shell function block to add to your rc file.

## Projects (`C-c p` — projectile prefix)

| Key       | Action                                              |
| --------- | --------------------------------------------------- |
| `C-c p p` | Switch project (consult-projectile)                 |
| `C-c p f` | Find file in project (consult-projectile)           |
| `C-c p g` | Ripgrep in project root                             |
| `C-c p …` | Everything else from projectile's command map       |

Press `C-c p` and pause — which-key shows the full projectile menu.

## Git (`C-x g` — magit canonical)

| Key       | Action          |
| --------- | --------------- |
| `C-x g`   | Magit status    |
| `C-x M-g` | Magit dispatch  |

Inside magit, `?` shows every magit binding.

## Org (`C-c a/c/l` — Org's documented triad)

| Key     | Action          |
| ------- | --------------- |
| `C-c a` | Org agenda      |
| `C-c c` | Org capture     |
| `C-c l` | Org store-link  |

## Sessions (`C-c s` — easysession)

| Key       | Action                              |
| --------- | ----------------------------------- |
| `C-c s l` | Load session                        |
| `C-c s L` | Load session + restore geometry     |
| `C-c s s` | Save session                        |
| `C-c s r` | Rename session                      |
| `C-c s R` | Reset session                       |
| `C-c s u` | Unload session                      |
| `C-c s d` | Delete session                      |

## LSP (`C-c l` — eglot, mode-local)

`C-c l` is also Org's `store-link` globally; in eglot-managed buffers
it becomes the eglot prefix instead.

| Key       | Action            |
| --------- | ----------------- |
| `C-c l r` | Rename symbol     |
| `C-c l f` | Format buffer     |
| `C-c l a` | Code actions      |
| `C-c l h` | Eldoc at point    |

Eglot auto-starts in core modes Chadmacs ships (C / C++ / C# / Bash).
Per-language extensions in `extensions/` enable Eglot for their
language when you `(require '<lang>-extension)` in `custom.el`. You
must install each language server binary yourself.

| Language       | Mode                                  | Server                       |
| -------------- | ------------------------------------- | ---------------------------- |
| C / C++        | `c-mode`, `c++-mode`                  | clangd                       |
| C#             | `csharp-ts-mode`                      | omnisharp / csharp-ls        |
| Python         | `python-ts-mode`                      | pylsp / pyright / ruff       |
| JS / TS / TSX  | `typescript-ts-mode`, `tsx-ts-mode`   | typescript-language-server   |
| Go             | `go-ts-mode`                          | gopls                        |
| Rust           | `rustic-mode`                         | rust-analyzer                |
| Bash / Shell   | `sh-mode`, `bash-ts-mode`             | bash-language-server         |
| YAML           | `yaml-ts-mode`                        | yaml-language-server         |
| Markdown       | `markdown-mode`                       | marksman                     |

Eglot diagnostics flow into Flycheck via `flycheck-eglot`.

## File explorer

### Dirvish (in dired buffers)

`C-x d` opens it. Inside:

| Key   | Action                              |
| ----- | ----------------------------------- |
| `a`   | Quick-access entries menu           |
| `TAB` | Toggle subtree                      |
| `s`   | Sort menu                           |
| `v`   | Version-control menu                |
| `y`   | Yank menu (copy / paste / link)     |
| `f`   | File info menu                      |
| `^`   | Jump to last directory              |
| `h`   | Jump to a recent directory          |
| `N`   | Narrow visible entries              |
| `M-t` | Toggle layout (preview pane)        |
| `M-j` | `fd` jump (fuzzy directory finder)  |

### Treemacs (sidebar — prefix `C-c T`)

| Key         | Action                  |
| ----------- | ----------------------- |
| `M-0`       | Select treemacs window  |
| `C-c T t`   | Toggle treemacs         |
| `C-c T 1`   | Delete other windows    |
| `C-c T d`   | Select directory        |
| `C-c T B`   | Treemacs bookmark       |
| `C-c T C-t` | Find current file       |
| `C-c T M-t` | Find tag                |

## Embark — action menu on anything

Embark turns any minibuffer candidate or symbol-at-point into a context
menu. Pick a file in `consult-buffer` and press `C-.` — you get rename
/ copy-path / open-other-window without leaving the completion UI.

| Key     | Action                                       |
| ------- | -------------------------------------------- |
| `C-.`   | embark-act (context menu at point)           |
| `C-;`   | embark-dwim (apply default action)           |
| `C-h B` | embark-bindings                              |

Inside a minibuffer:
* `C-.` — list actions on the current candidate
* `C-c C-e` — export results to an editable buffer (combine with `wgrep`
  to mass-edit grep matches)

## In-buffer completion (`C-c e` — cape extra sources)

| Key     | Action          |
| ------- | --------------- |
| `C-c e` | Cape prefix map |

`corfu` shows a popup automatically as you type; `cape` adds extra
completion-at-point sources (dabbrev, file paths, elisp blocks, …).

## envrc (per-project direnv)

When a project has a `.envrc`, envrc injects its environment into
Emacs's PATH / env so eglot, flycheck, and compile commands see the
correct toolchain.

| Key     | Action                                |
| ------- | ------------------------------------- |
| `C-c E` | envrc command map (allow / reload / …)|

Requires `direnv` on `PATH`.

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
* Emacs built with `--with-modules` (for ghostel — auto-checked by `chadmacs doctor`)
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
