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

### Quick Install

Clone the repository and run the installer:

```bash
git clone https://github.com/skeptomai/omarchy-emacs-themer.git
cd omarchy-emacs-themer
./install.sh
```

Then add to your Doom Emacs configuration (see below).

---

## Configuration

### Doom Emacs

#### Option 1: Local Development/Testing

Add to `~/.doom.d/packages.el`:

```elisp
;; Using local path
(package! omarchy-themer
  :recipe (:local-repo "/path/to/omarchy-emacs-themer"))
```

#### Option 2: Install from GitHub (Recommended)

Add to `~/.doom.d/packages.el`:

```elisp
;; Install directly from GitHub
(package! omarchy-themer
  :recipe (:host github
           :repo "skeptomai/omarchy-emacs-themer"
           :files ("*.el")))
```

#### Configuration in config.el

Add to `~/.doom.d/config.el`:

```elisp
;; Load the package
(use-package! omarchy-themer
  :config
  ;; Optional: customize theme directory (default is ~/.emacs.d/themes/)
  (setq omarchy-themer-theme-directory
        (expand-file-name "themes/" doom-user-dir))

  ;; Ensure theme directory is in load path on startup
  (omarchy-themer-add-theme-directory)

  ;; Sync with current system theme on startup
  (omarchy-themer-sync-on-startup))
```

Then sync:

```bash
doom sync
```

---

### Vanilla Emacs / Straight.el

Add to your `init.el`:

```elisp
;; Using straight.el
(straight-use-package
 '(omarchy-themer :type git
                  :host github
                  :repo "skeptomai/omarchy-emacs-themer"))

(require 'omarchy-themer)
(omarchy-themer-add-theme-directory)
(omarchy-themer-sync-on-startup)
```

---

### use-package + quelpa

Add to your `init.el`:

```elisp
(use-package omarchy-themer
  :quelpa (omarchy-themer :fetcher github
                          :repo "skeptomai/omarchy-emacs-themer")
  :config
  (omarchy-themer-add-theme-directory)
  (omarchy-themer-sync-on-startup))
```

---

### Manual Installation

1. Clone the repository:

```bash
git clone https://github.com/skeptomai/omarchy-emacs-themer.git ~/.emacs.d/site-lisp/omarchy-emacs-themer
```

2. Run the installer for the hook script:

```bash
cd ~/.emacs.d/site-lisp/omarchy-emacs-themer
./install.sh
```

3. Add to your `init.el`:

```elisp
(add-to-list 'load-path "~/.emacs.d/site-lisp/omarchy-emacs-themer")
(require 'omarchy-themer)
(omarchy-themer-add-theme-directory)
(omarchy-themer-sync-on-startup)
```

## Usage

### Automatic Theme Syncing

Once installed, the package works automatically. When you change your Omarchy Linux theme:

1. Omarchy triggers the `20-emacs.sh` hook script
2. Script generates `~/.config/omarchy/current/theme/omarchy-doom-theme.el` with system colors
3. Script calls `emacsclient` to load the new theme
4. All running Emacs sessions update instantly

**Note**: You must have Emacs server running for automatic updates. Start it with `M-x server-start` or add to your config:

```elisp
;; In ~/.doom.d/config.el or init.el
(server-start)
```

### Manual Functions

You can also use these functions interactively:

#### `omarchy-themer-install-and-load`
Install and load a theme file from anywhere on your system.

```elisp
M-x omarchy-themer-install-and-load RET /path/to/my-theme.el RET
```

Or in Lisp:
```elisp
(omarchy-themer-install-and-load "/path/to/my-theme.el")
(omarchy-themer-install-and-load "/path/to/my-theme.el" 'my-custom-name)
```

#### `omarchy-themer-add-theme-directory`
Manually add the theme directory to Emacs' theme search path.

```elisp
M-x omarchy-themer-add-theme-directory RET
```

#### `omarchy-themer-sync-on-startup`
Sync Emacs with the current system theme. Loads the theme file at `~/.config/omarchy/current/theme/omarchy-doom-theme.el`.

This should be called in your config to ensure Emacs starts with the current system theme, even if the theme changed while Emacs was not running.

```elisp
M-x omarchy-themer-sync-on-startup RET
```

Or in your config:
```elisp
(omarchy-themer-sync-on-startup)
```

#### `omarchy-themer-version`
Display the current package version.

```elisp
M-x omarchy-themer-version RET
```

### Advanced Configuration

#### Custom Theme Directory

By default, themes are stored in `~/.emacs.d/themes/`. To use a different location:

```elisp
(setq omarchy-themer-theme-directory "~/my/custom/theme/path/")
```

#### Post-Theme Load Hook

If you want to customize the theme after it loads, you can use Emacs' built-in `load-theme` advice:

```elisp
(defun my-omarchy-theme-customizations ()
  "Custom face adjustments after Omarchy theme loads."
  (when (string-prefix-p "omarchy" (symbol-name (car custom-enabled-themes)))
    (set-face-attribute 'font-lock-comment-face nil :slant 'italic)))

(advice-add 'load-theme :after
            (lambda (&rest _) (my-omarchy-theme-customizations)))
```

## How It Works

### During Theme Changes (Push)
1. Omarchy Linux triggers theme hooks when the system theme changes
2. The `20-emacs.sh` script generates a new theme file with system colors
3. The script calls `omarchy-themer-install-and-load` via `emacsclient`
4. The package copies the theme file and loads it into Emacs
5. Your Emacs session updates to match your system theme

### On Emacs Startup (Pull)
1. `omarchy-themer-sync-on-startup` runs during Emacs initialization
2. It checks for the current theme at `~/.config/omarchy/current/theme/omarchy-doom-theme.el`
3. If found, it loads the theme ensuring Emacs starts with the current system theme
4. This prevents Emacs from using stale theme data from previous sessions

## Troubleshooting

### Theme doesn't update automatically

**Check if Emacs server is running:**
```elisp
M-x server-start
```

Add to your config to start automatically:
```elisp
;; Doom Emacs: ~/.doom.d/config.el
;; Vanilla Emacs: ~/.emacs.d/init.el
(unless (server-running-p)
  (server-start))
```

**Verify the hook script is installed:**
```bash
ls -la ~/.config/omarchy/hooks/theme-set.d/20-emacs.sh
```

If missing, run `./install.sh` from the package directory.

**Test the hook manually:**
```bash
bash ~/.config/omarchy/hooks/theme-set.d/20-emacs.sh
```

### Theme file not found

Check that the theme directory exists:
```elisp
M-x describe-variable RET omarchy-themer-theme-directory RET
```

Verify it's in the load path:
```elisp
M-x describe-variable RET custom-theme-load-path RET
```

If not listed, run:
```elisp
M-x omarchy-themer-add-theme-directory
```

### `emacsclient` command not found

Ensure Emacs is installed with client support. On most systems:
```bash
which emacsclient
```

For Doom Emacs, it should be in your PATH automatically.

### Package not loading in Doom Emacs

1. Verify it's in `packages.el`
2. Run `doom sync`
3. Restart Emacs
4. Check for errors: `M-x doom/version` and review `*Messages*` buffer

### Colors look wrong

The hook script uses Omarchy color variables. Ensure they're exported before the script runs. Check `/etc/omarchy/themes/` or `~/.config/omarchy/current/theme/` for color definitions.

## Development

### Testing Changes

After modifying `omarchy-themer.el`:

```bash
# Doom Emacs
doom sync
```

Or reload manually:
```elisp
M-x load-file RET /path/to/omarchy-themer.el RET
```

### Hook Script Development

Test hook script changes:
```bash
# Source Omarchy colors first
source ~/.config/omarchy/current/theme/colors.sh

# Run the hook
bash 20-emacs.sh
```

## License

MIT License - See LICENSE file for details

## Author

Christopher Brown <cb@skeptomai.com>

## Links

- GitHub: https://github.com/skeptomai/omarchy-emacs-themer
- Issues: https://github.com/skeptomai/omarchy-emacs-themer/issues
