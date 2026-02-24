#!/bin/bash
#
# Omarchy theme-set hook: generates and loads an Emacs theme from the current
# system palette. Installed to ~/.config/omarchy/hooks/theme-set.d/ via install.sh.
#
# The hook system exports color variables (primary_background, normal_red, etc.)
# only for themes that ship a colors.toml. Older/override themes (e.g. ayu-light)
# instead ship a fully-rendered alacritty.toml, so we fall back to parsing that.

new_emacs_file="$HOME/.config/omarchy/current/theme/omarchy-doom-theme.el"

# Generates omarchy-doom-theme.el from the exported color variables.
create_dynamic_theme() {
    cat > "$new_emacs_file" << EOF
(autothemer-deftheme
 omarchy-doom "A theme for Omarchy Linux"
 ((((class color) (min-colors #xFFFFFF)))

   ;; Color palette from system theme
   (bg          "#${primary_background}")
   (fg          "#${primary_foreground}")
   (cursor-fg   "#${primary_background}")
   (cursor-bg   "#${cursor_color}")
   (sel-fg      "#${selection_foreground}")
   (sel-bg      "#${selection_background}")
   (black       "#${normal_black}")
   (red         "#${normal_red}")
   (green       "#${normal_green}")
   (yellow      "#${normal_yellow}")
   (blue        "#${normal_blue}")
   (magenta     "#${normal_magenta}")
   (cyan        "#${normal_cyan}")
   (white       "#${normal_white}")
   (br-black    "#${bright_black}")
   (br-red      "#${bright_red}")
   (br-green    "#${bright_green}")
   (br-yellow   "#${bright_yellow}")
   (br-blue     "#${bright_blue}")
   (br-magenta  "#${bright_magenta}")
   (br-cyan     "#${bright_cyan}")
   (br-white    "#${bright_white}")
   )

 ;; Face mappings (aligned with Zed/VSCode/Neovim mappings)
 (
  ;; Core faces
  (default                          (:foreground fg :background bg))
  (cursor                           (:foreground cursor-fg :background cursor-bg))
  (region                           (:foreground sel-fg :background sel-bg))
  (highlight                        (:background sel-bg))
  (shadow                           (:foreground br-black))
  (minibuffer-prompt                (:foreground blue :bold t))
  (link                             (:foreground blue :underline t))
  (link-visited                     (:foreground magenta :underline t))

  ;; Line numbers
  (line-number                      (:foreground br-black))
  (line-number-current-line         (:foreground br-red))

  ;; Search / match
  (isearch                          (:foreground bg :background yellow))
  (lazy-highlight                   (:foreground bg :background br-yellow))
  (match                            (:foreground bg :background blue))

  ;; Syntax highlighting
  (font-lock-keyword-face           (:foreground magenta))
  (font-lock-function-name-face     (:foreground blue))
  (font-lock-function-call-face     (:foreground blue))
  (font-lock-variable-name-face     (:foreground red))
  (font-lock-variable-use-face      (:foreground red))
  (font-lock-string-face            (:foreground green))
  (font-lock-doc-face               (:foreground green :italic t))
  (font-lock-comment-face           (:foreground br-black :italic t))
  (font-lock-comment-delimiter-face (:foreground br-black :italic t))
  (font-lock-constant-face          (:foreground br-red))
  (font-lock-number-face            (:foreground br-red))
  (font-lock-type-face              (:foreground yellow))
  (font-lock-builtin-face           (:foreground cyan))
  (font-lock-preprocessor-face      (:foreground cyan))
  (font-lock-negation-char-face     (:foreground red))
  (font-lock-warning-face           (:foreground yellow :bold t))
  (font-lock-regexp-grouping-construct (:foreground cyan))
  (font-lock-regexp-grouping-backslash (:foreground cyan))

  ;; Mode line
  (mode-line                        (:foreground fg :background black))
  (mode-line-inactive               (:foreground br-black :background black))
  (mode-line-emphasis               (:foreground blue :bold t))
  (mode-line-buffer-id              (:foreground fg :bold t))

  ;; Errors / warnings
  (error                            (:foreground red))
  (warning                          (:foreground yellow))
  (success                          (:foreground green))

  ;; Diff / version control
  (diff-added                       (:foreground green))
  (diff-removed                     (:foreground red))
  (diff-changed                     (:foreground magenta))
  (diff-header                      (:foreground blue :bold t))

  ;; Parens
  (show-paren-match                 (:foreground bg :background blue :bold t))
  (show-paren-mismatch              (:foreground bg :background red :bold t))

  ;; Whitespace
  (trailing-whitespace              (:background red))

  ;; Fringe and UI
  (fringe                           (:foreground br-black :background bg))
  (vertical-border                  (:foreground black))
 ))

(provide-theme 'omarchy-doom)
EOF
}

if ! command -v emacs >/dev/null 2>&1; then
    skipped "Emacs"
    exit 0
fi

# Themes with colors.toml have their palette exported as shell variables by the
# hook system before this script runs. Older/override themes skip that step, so
# we detect the gap and parse alacritty.toml ourselves instead.
if [ -z "$primary_background" ]; then
    alacritty_file="$HOME/.config/omarchy/current/theme/alacritty.toml"
    if [ ! -f "$alacritty_file" ]; then
        emacsclient -e "(display-warning 'omarchy-themer \"Theme has no colors.toml or alacritty.toml; Emacs theme not updated\" :warning)" 2>/dev/null
        exit 0
    fi

    # Extract a hex color (without leading #) from a section/key in alacritty.toml.
    # alacritty.toml uses nested TOML sections: [colors.primary], [colors.normal], etc.
    extract_alacritty_color() {
        local section="$1" key="$2"
        awk -v sec="[${section}]" -v k="$key" '
            /^\[/ { in_sec = ($0 == sec) }
            in_sec && /=/ {
                split($0, a, "=")
                gsub(/[[:space:]]/, "", a[1])
                if (a[1] == k) {
                    match(a[2], /#[0-9a-fA-F]{6}/)
                    if (RSTART) { print substr(a[2], RSTART+1, 6); exit }
                }
            }
        ' "$alacritty_file"
    }

    primary_background=$(extract_alacritty_color "colors.primary" "background")
    primary_foreground=$(extract_alacritty_color "colors.primary" "foreground")
    cursor_color=$(extract_alacritty_color "colors.cursor" "cursor")
    selection_foreground=$(extract_alacritty_color "colors.selection" "text")
    selection_background=$(extract_alacritty_color "colors.selection" "background")
    normal_black=$(extract_alacritty_color "colors.normal" "black")
    normal_red=$(extract_alacritty_color "colors.normal" "red")
    normal_green=$(extract_alacritty_color "colors.normal" "green")
    normal_yellow=$(extract_alacritty_color "colors.normal" "yellow")
    normal_blue=$(extract_alacritty_color "colors.normal" "blue")
    normal_magenta=$(extract_alacritty_color "colors.normal" "magenta")
    normal_cyan=$(extract_alacritty_color "colors.normal" "cyan")
    normal_white=$(extract_alacritty_color "colors.normal" "white")
    bright_black=$(extract_alacritty_color "colors.bright" "black")
    bright_red=$(extract_alacritty_color "colors.bright" "red")
    bright_green=$(extract_alacritty_color "colors.bright" "green")
    bright_yellow=$(extract_alacritty_color "colors.bright" "yellow")
    bright_blue=$(extract_alacritty_color "colors.bright" "blue")
    bright_magenta=$(extract_alacritty_color "colors.bright" "magenta")
    bright_cyan=$(extract_alacritty_color "colors.bright" "cyan")
    bright_white=$(extract_alacritty_color "colors.bright" "white")
fi

# Guard against a malformed alacritty.toml that yielded no colors.
if [ -z "$primary_background" ]; then
    emacsclient -e "(display-warning 'omarchy-themer \"Could not extract colors from alacritty.toml; Emacs theme not updated\" :warning)" 2>/dev/null
    exit 0
fi

# The theme directory may not exist for newly-installed themes; create it first.
mkdir -p "$(dirname "$new_emacs_file")"
create_dynamic_theme

emacsclient -e "(omarchy-themer-install-and-load \"$new_emacs_file\")"

success "Emacs theme updated!"
exit 0
