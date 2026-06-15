#!/usr/bin/env bash
# install.sh - Chadmacs one-line installer.
#
# Usage:
#   curl -fsSL https://raw.githubusercontent.com/Borderliner/Chadmacs/master/install.sh | bash
#
# Or from a local checkout:
#   bash install.sh
#
# What it does:
#   1. Backs up an existing ~/.emacs.d to ~/.emacs.d.backup-YYYYMMDD-HHMMSS
#   2. Clones Borderliner/Chadmacs into ~/.emacs.d
#   3. Adds ~/.emacs.d/bin to PATH in bash / zsh / fish rc files (idempotent)
#   4. Prints next steps.

set -euo pipefail

CHADMACS_REPO="${CHADMACS_REPO:-https://github.com/Borderliner/Chadmacs.git}"
CHADMACS_BRANCH="${CHADMACS_BRANCH:-master}"
CHADMACS_DIR="${CHADMACS_DIR:-$HOME/.emacs.d}"

# ---- colors -----------------------------------------------------------------
if [ -t 1 ] && command -v tput >/dev/null 2>&1 && [ "$(tput colors 2>/dev/null || echo 0)" -ge 8 ]; then
  C_RESET="$(tput sgr0)"
  C_BOLD="$(tput bold)"
  C_GREEN="$(tput setaf 2)"
  C_YELLOW="$(tput setaf 3)"
  C_RED="$(tput setaf 1)"
  C_BLUE="$(tput setaf 4)"
else
  C_RESET= C_BOLD= C_GREEN= C_YELLOW= C_RED= C_BLUE=
fi

say()  { printf '%s•%s %s\n' "$C_BLUE" "$C_RESET" "$*"; }
ok()   { printf '%s✓%s %s\n' "$C_GREEN" "$C_RESET" "$*"; }
warn() { printf '%s!%s %s\n' "$C_YELLOW" "$C_RESET" "$*"; }
die()  { printf '%s✗%s %s\n' "$C_RED" "$C_RESET" "$*" >&2; exit 1; }
hdr()  { printf '\n%s%s%s\n' "$C_BOLD" "$*" "$C_RESET"; }

have() { command -v "$1" >/dev/null 2>&1; }

# ---- preflight --------------------------------------------------------------
have git || die "git is required. Install git and re-run."

# ---- header -----------------------------------------------------------------
cat <<'BANNER'

   ▄████████    ▄█    █▄       ▄████████ ████████▄    ▄▄▄▄███▄▄▄▄      ▄████████  ▄████████    ▄████████
  ███    ███   ███    ███     ███    ███ ███   ▀███ ▄██▀▀▀███▀▀▀██▄   ███    ███ ███    ███   ███    ███
  ███    █▀    ███    ███     ███    ███ ███    ███ ███   ███   ███   ███    ███ ███    █▀    ███    █▀
  ███         ▄███▄▄▄▄███▄▄   ███    ███ ███    ███ ███   ███   ███   ███    ███ ███          ███
  ███        ▀▀███▀▀▀▀███▀  ▀███████████ ███    ███ ███   ███   ███ ▀███████████ ███        ▀███████████
  ███    █▄    ███    ███     ███    ███ ███    ███ ███   ███   ███   ███    ███ ███    █▄           ███
  ███    ███   ███    ███     ███    ███ ███   ▄███ ███   ███   ███   ███    ███ ███    ███    ▄█    ███
  ████████▀    ███    █▀      ███    █▀  ████████▀   ▀█   ███   █▀    ███    █▀  ████████▀   ▄████████▀

   Ultra-clean, ultra-fast Emacs config.

BANNER

# ---- backup existing --------------------------------------------------------
if [ -e "$CHADMACS_DIR" ]; then
  BACKUP_DIR="${CHADMACS_DIR}.backup-$(date +%Y%m%d-%H%M%S)"
  hdr "Backing up existing $CHADMACS_DIR"
  mv "$CHADMACS_DIR" "$BACKUP_DIR"
  ok "Moved to $BACKUP_DIR"
fi

# ---- clone ------------------------------------------------------------------
hdr "Cloning Chadmacs"
say "git clone --depth=1 -b $CHADMACS_BRANCH $CHADMACS_REPO -> $CHADMACS_DIR"
git clone --depth=1 -b "$CHADMACS_BRANCH" "$CHADMACS_REPO" "$CHADMACS_DIR"
ok "Cloned."

# ---- chmod bin --------------------------------------------------------------
if [ -f "$CHADMACS_DIR/bin/chadmacs" ]; then
  chmod +x "$CHADMACS_DIR/bin/chadmacs"
  ok "Made $CHADMACS_DIR/bin/chadmacs executable."
fi

# ---- shell PATH wiring ------------------------------------------------------
hdr "Wiring \$PATH for shells"

BIN_DIR="$CHADMACS_DIR/bin"
MARKER="# >>> chadmacs PATH >>>"
MARKER_END="# <<< chadmacs PATH <<<"

# bash
wire_bash_zsh() {
  local rc="$1"
  if [ -f "$rc" ] && grep -q "$MARKER" "$rc"; then
    ok "$rc already wired."
    return
  fi
  cat >>"$rc" <<EOF

$MARKER
export PATH="$BIN_DIR:\$PATH"
$MARKER_END
EOF
  ok "Appended PATH export to $rc"
}

# bash + zsh share the same form
if [ -f "$HOME/.bashrc" ]    || have bash; then wire_bash_zsh "$HOME/.bashrc"; fi
if [ -f "$HOME/.bash_profile" ] && ! grep -q "$MARKER" "$HOME/.bash_profile" 2>/dev/null; then
  wire_bash_zsh "$HOME/.bash_profile"
fi
if [ -f "$HOME/.zshrc" ]     || have zsh;  then wire_bash_zsh "$HOME/.zshrc";  fi

# fish (different syntax + fish_add_path is preferred)
if have fish; then
  FISH_CONF="$HOME/.config/fish/conf.d"
  mkdir -p "$FISH_CONF"
  FISH_FILE="$FISH_CONF/chadmacs.fish"
  if [ -f "$FISH_FILE" ]; then
    ok "$FISH_FILE already present."
  else
    cat >"$FISH_FILE" <<EOF
# Chadmacs PATH wiring
if test -d "$BIN_DIR"
    fish_add_path -gP "$BIN_DIR"
end
EOF
    ok "Created $FISH_FILE"
  fi
fi

# ---- preflight inside emacs -------------------------------------------------
hdr "Smoke test"
if have emacs; then
  EV=$(emacs --batch --eval '(princ emacs-version)' 2>/dev/null || echo "?")
  MAJOR=$(printf '%s' "$EV" | cut -d. -f1)
  if [ "$MAJOR" -ge 29 ] 2>/dev/null; then
    ok "Emacs $EV detected — meets the 29+ requirement."
  else
    warn "Emacs $EV is older than 29 — install a newer Emacs before launching Chadmacs."
  fi
else
  warn "Emacs not installed. Install it before launching Chadmacs."
fi

# ---- next steps -------------------------------------------------------------
hdr "Done"
cat <<EOF
${C_BOLD}Next steps${C_RESET}

  1. Open a new shell (or source your rc) so the chadmacs CLI is on \$PATH:
       ${C_BLUE}exec \$SHELL${C_RESET}

  2. Run the doctor to see what's installed:
       ${C_BLUE}chadmacs doctor${C_RESET}

  3. Launch Emacs. Elpaca will bootstrap and build every package on
     first run (a few minutes). Watch ${C_BLUE}*elpaca-log*${C_RESET} in Emacs.
       ${C_BLUE}emacs${C_RESET}

  4. Enable the languages you use by editing:
       ${C_BLUE}~/.emacs.d/custom.el${C_RESET}
     (Auto-generated on first launch, gitignored — safe from updates.)

  5. Updating:
       ${C_BLUE}chadmacs update${C_RESET}

  6. Removing:
       ${C_BLUE}chadmacs uninstall${C_RESET}    (your var/ will be backed up)

Enjoy your editor. 🗿🔥
EOF
