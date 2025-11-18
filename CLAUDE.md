# CLAUDE.md - Project Context

## Project Purpose

This repository contains **omarchy-themer**, an Emacs Lisp package that integrates Doom Emacs with Omarchy Linux's system-wide theme management. The package enables automatic synchronization of Emacs themes with the Omarchy Linux dynamic theming system.

## What This Package Does

When you change your system theme in Omarchy Linux, this package ensures that Doom Emacs updates to match, providing a consistent visual experience across your entire desktop environment.

### Key Components

1. **omarchy-themer.el** - The main Emacs Lisp package providing:
   - `omarchy-themer-install-and-load` - Copies theme files and loads them into Emacs
   - `omarchy-themer-add-theme-directory` - Manages theme directory paths
   - Built on the autothemer package for robust theme definitions

2. **20-emacs.sh** - Hook script that:
   - Generates dynamic theme files from Omarchy Linux system colors
   - Communicates with running Emacs instances via emacsclient
   - Should be installed to `~/.config/omarchy/hooks/theme-set.d/`

## How It Works

1. User changes system theme in Omarchy Linux
2. Omarchy triggers all scripts in `~/.config/omarchy/hooks/theme-set.d/`
3. The `20-emacs.sh` script generates a new theme file with current system colors
4. Script calls `omarchy-themer-install-and-load` via emacsclient
5. Package copies the theme file to the theme directory and loads it
6. All running Emacs sessions update to the new theme

## Installation

Users should:
1. Add the package to Doom Emacs via `packages.el`
2. Run the provided `install.sh` to install the hook script
3. Run `doom sync` to complete installation

## Dependencies

- Emacs 27.1+
- Doom Emacs
- autothemer package (0.2.2+)
- Omarchy Linux theme system

## Author

Christopher Brown <cb@skeptomai.com>
