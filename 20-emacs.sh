#!/bin/bash

new_emacs_file="$HOME/.config/omarchy/current/theme/omarchy-doom-theme.el"

create_dynamic_theme() {
    cat > "$new_emacs_file" << EOF
(autothemer-deftheme
 omarchy-doom "A theme for Omarchy Linux"
 ((((class color) (min-colors #xFFFFFF)))

   ;; Define our color palette
   (omarchy-background "#${primary_background}")
   (omarchy-foreground "#${primary_foreground}")
   (omarchy-orange "orange1")
   (omarchy-cursor "#${normal_white}")
   (omarchy-cursor-bg "#eb6123")
   (omarchy-highlight "#${normal_yellow}")
   (omarchy-purple "MediumPurple2")
   (omarchy-green "LightGreen")

   )

 ((default (:foreground omarchy-foreground
            :background omarchy-background))
  (cursor  (:foreground omarchy-cursor
            :background omarchy-cursor-bg))
  (region  (:background omarchy-purple))
  (font-lock-keyword-face (:foreground omarchy-green))
  (font-lock-constant-face (:foreground omarchy-highlight))
  (font-lock-string-face (:foreground omarchy-orange))
 ))


(provide-theme 'omarchy-doom)
EOF
}

if ! command -v emacs >/dev/null 2>&1; then
    skipped "Emacs"
fi

if [ ! -f "$new_emacs_file" ]; then
    create_dynamic_theme
fi

emacsclient -e "(omarchy-themer-install-and-load \"$new_emacs_file\")"

success "Emacs theme updated!"
exit 0
