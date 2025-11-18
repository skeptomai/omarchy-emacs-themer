#!/bin/bash
#
# Installation script for omarchy-emacs-themer
# Copies the theme hook script to the Omarchy Linux hooks directory

set -e

HOOK_SCRIPT="20-emacs.sh"
TARGET_DIR="$HOME/.config/omarchy/hooks/theme-set.d"

# Check if the target directory exists
if [ ! -d "$TARGET_DIR" ]; then
    echo "Error: Target directory '$TARGET_DIR' does not exist."
    echo "Please ensure Omarchy Linux theme system is properly installed."
    exit 1
fi

# Check if the hook script exists in the current directory
if [ ! -f "$HOOK_SCRIPT" ]; then
    echo "Error: Hook script '$HOOK_SCRIPT' not found in current directory."
    exit 1
fi

# Copy the hook script to the target directory
cp "$HOOK_SCRIPT" "$TARGET_DIR/"

# Make the script executable
chmod +x "$TARGET_DIR/$HOOK_SCRIPT"

echo "Successfully installed $HOOK_SCRIPT to $TARGET_DIR"
echo "The script is now executable and will run when Omarchy Linux changes themes."
