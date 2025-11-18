# omarchy-emacs-themer

An Emacs Lisp package that integrates Doom Emacs with Omarchy Linux's system-wide theme management.

## Overview

`omarchy-themer` automatically synchronizes your Emacs theme with Omarchy Linux's dynamic theming system. When you change your system theme in Omarchy Linux, this package ensures that Doom Emacs updates to match, providing a consistent visual experience across your entire desktop environment.

## Features

- **Automatic theme synchronization** - Emacs themes update automatically when Omarchy Linux theme changes
- **Built on autothemer** - Leverages the powerful autothemer package for theme definitions
- **Seamless integration** - Works with Omarchy's hook-based theme mechanism
- **Dynamic theme loading** - Themes are installed and activated on-the-fly

## Installation

### Prerequisites

- Emacs 27.1 or later
- Doom Emacs
- autothemer package (version 0.2.2+)
- Omarchy Linux theme system

### Setup

1. Clone or copy this package to your system
2. Add to your Doom Emacs `packages.el`:

```elisp
(package! omarchy-themer :recipe (:local-repo "/path/to/omarchy-emacs-themer"))
```

3. Install the hook script to integrate with Omarchy's theme system:

```bash
cp 20-emacs.sh ~/.config/omarchy/hooks/theme-set.d/
```

4. Reload Doom Emacs packages:

```bash
doom sync
```

## Usage

The package works automatically once installed. When Omarchy Linux changes themes, the `20-emacs.sh` hook script generates a new theme file with colors from your system theme and loads it into any running Emacs instances.

### Manual Functions

You can also use these functions directly:

- `omarchy-themer-install-and-load` - Install and load a theme file
- `omarchy-themer-add-theme-directory` - Add the theme directory to Emacs' load path
- `omarchy-themer-version` - Display package version

## How It Works

1. Omarchy Linux triggers theme hooks when the system theme changes
2. The `20-emacs.sh` script generates a new theme file with system colors
3. The script calls `omarchy-themer-install-and-load` via `emacsclient`
4. The package copies the theme file and loads it into Emacs
5. Your Emacs session updates to match your system theme

## License

MIT License - See LICENSE file for details

## Author

Christopher Brown <cb@skeptomai.com>

## Links

- GitHub: https://github.com/skeptomai/omarchy-emacs-themer
