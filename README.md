# 🗿 Chadmacs

> Borderliner’s **ultra-clean, ultra-fast, ultra-chad Emacs setup**.

Minimal chrome. Smooth UI. Modern completion. LSP ready. Tree-sitter powered.

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
├── early-init.el
├── init.el
└── var/
    ├── elpaca/
    ├── eln/
    ├── backup/
    ├── auto-save/
    ├── history
    ├── recentf
    ├── places
    ├── tree-sitter/
    └── ...
```

All generated state lives inside `var/`.

Your config stays clean.
Your git repo stays clean.
Your soul stays clean.

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

* Eglot (LSP)
* Flycheck + posframe
* Apheleia (formatting)
* Tree-sitter auto install
* Magit
* Diff-hl
* Treemacs

## 📝 Editing Enhancements

* undo-fu + session persistence
* Yasnippet
* Strip trailing whitespace
* Aggressive indent (Lisp)
* Paredit

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

## 🪟 Frames

| Key     | Action            |
| ------- | ----------------- |
| `M-n`   | New frame         |
| `M-``   | Switch frame      |
| `M-RET` | Toggle fullscreen |

## 🗂 Files / Buffers

| Key       | Action         |
| --------- | -------------- |
| `C-x b`   | Consult buffer |
| `C-x C-r` | Recent files   |
| `M-s d`   | Find files     |
| `M-s r`   | Ripgrep        |

## ⚡ Actions

| Key   | Action      |
| ----- | ----------- |
| `C-.` | Embark act  |
| `C-;` | Embark dwim |

## 🧠 LSP

Auto-starts in:

* `c-mode`
* `c++-mode`
* `csharp-ts-mode`

Common commands:

| Command               | What it does  |
| --------------------- | ------------- |
| `M-g i`               | Imenu         |
| `M-g r`               | References    |
| `eglot-rename`        | Rename symbol |
| `eglot-format-buffer` | Format buffer |

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

Inside `init.el`:

```elisp
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 110)
```

### Change Theme

Replace:

```elisp
(load-theme 'doom-monokai-pro t)
```

With another Doom theme.

---

# 🧪 Performance Tricks

* 🚀 GC threshold raised during startup
* 🚀 File-name handlers disabled during init
* 🚀 Native compilation cache redirected
* 🚀 compile-angel auto-compiles

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

Because you don’t want:

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
* language servers

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
It’s a mindset.

Minimal.
Fast.
Intentional.

Enjoy your editor. 🗿🔥
