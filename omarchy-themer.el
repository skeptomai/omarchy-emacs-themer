;;; omarchy-themer.el --- Theme management for Omarchy Linux -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Christopher Brown <cb@skeptomai.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (autothemer "0.2.2"))
;; Keywords: faces, themes
;; URL: https://github.com/skeptomai/omarchy-emacs-themer

;; This file is not part of GNU Emacs.

;;; Commentary:

;; omarchy-themer provides theme management utilities for Doom Emacs
;; that integrate with Omarchy Linux's system-wide theme management.
;; Built on top of the autothemer package.
;;
;; This package enables bidirectional theme synchronization:
;;
;; - PUSH: System theme changes trigger updates to running Emacs instances
;;   via the 20-emacs.sh hook script that calls omarchy-themer-install-and-load
;;
;; - PULL: Emacs startup checks current system theme via
;;   omarchy-themer-sync-on-startup to ensure initial state matches the system
;;
;; Usage:
;;
;;   (use-package! omarchy-themer
;;     :config
;;     (omarchy-themer-add-theme-directory)
;;     (omarchy-themer-sync-on-startup))
;;

;;; Code:

(require 'autothemer)

;;; Customization

(defgroup omarchy-themer nil
  "Theme management for Doom Emacs."
  :group 'faces
  :prefix "omarchy-themer-")

(defcustom omarchy-themer-theme-directory
  (expand-file-name "themes/" user-emacs-directory)
  "Directory where omarchy-themer looks for theme files."
  :type 'directory
  :group 'omarchy-themer)

;;; Public Functions

;;;###autoload
(defun omarchy-themer-add-theme-directory ()
  "Add `omarchy-themer-theme-directory' to `custom-theme-load-path'.
Only adds it as the first element if it's not already present in the list."
  (interactive)
  (let ((theme-dir (file-name-as-directory
                    (expand-file-name omarchy-themer-theme-directory))))
    (unless (member theme-dir custom-theme-load-path)
      (push theme-dir custom-theme-load-path)
      (message "Added %s to custom-theme-load-path" theme-dir))))

;;;###autoload
(defun omarchy-themer-install-and-load (theme-file &optional theme-name)
  "Copy THEME-FILE to `omarchy-themer-theme-directory' and load the theme.
If THEME-NAME is provided, use that; otherwise derive from filename by
removing -theme.el suffix (e.g., foo-theme.el -> foo)."
  (interactive "fTheme file: ")
  (let* ((theme-dir (file-name-as-directory
                     (expand-file-name omarchy-themer-theme-directory)))
         (filename (file-name-nondirectory theme-file))
         (dest-file (expand-file-name filename theme-dir))
         (theme-name (or theme-name
                         (intern (replace-regexp-in-string
                                  "-theme$" ""
                                  (file-name-sans-extension filename))))))
    ;; Ensure directory exists
    (unless (file-exists-p theme-dir)
      (make-directory theme-dir t))
    ;; Copy the file
    (copy-file theme-file dest-file t)
    (message "Copied %s to %s" filename theme-dir)
    ;; Ensure theme directory is in load path
    (omarchy-themer-add-theme-directory)
    ;; Disable all active themes first so their face definitions don't bleed
    ;; into the new theme.  Without this, load-theme stacks on top of the
    ;; previous theme and any faces we don't explicitly define (e.g. Doom's
    ;; internal echo-area / minibuffer faces) retain the old theme's values.
    (mapc #'disable-theme custom-enabled-themes)
    ;; Load the theme (automatically finds and loads the file)
    (load-theme theme-name t)
    (message "Loaded theme: %s" theme-name)))

;;; lsp-ui-doc integration

(defun omarchy-themer--apply-lsp-ui-doc-colors (frame &rest _)
  "Apply lsp-ui-doc-background face colors to the child FRAME.
lsp-ui-doc-frame-hook calls this every time the popup frame is
created, which is the only reliable point at which the frame exists
and accepts face attribute changes."
  (let ((bg (face-background 'lsp-ui-doc-background nil t))
        (fg (face-foreground 'lsp-ui-doc-background nil t)))
    (when bg (set-face-background 'default bg frame))
    (when fg (set-face-foreground 'default fg frame))))

;; Wire up the hook when lsp-ui-doc is loaded.  Using with-eval-after-load
;; means this is a no-op when lsp-ui is not installed.
(with-eval-after-load 'lsp-ui-doc
  (add-hook 'lsp-ui-doc-frame-hook #'omarchy-themer--apply-lsp-ui-doc-colors))

;;;###autoload
(defun omarchy-themer-sync-on-startup ()
  "Load current Omarchy theme when Emacs starts.
Checks for theme file at ~/.config/omarchy/current/theme/omarchy-doom-theme.el
and loads it if present. This ensures Emacs starts with the current system theme
even if the theme changed while Emacs was not running."
  (interactive)
  (let ((current-theme (expand-file-name
                        "~/.config/omarchy/current/theme/omarchy-doom-theme.el")))
    (when (file-exists-p current-theme)
      (omarchy-themer-install-and-load current-theme))))

;;;###autoload
(defun omarchy-themer-version ()
  "Display the current version of omarchy-themer."
  (interactive)
  (message "omarchy-themer version 0.1.0"))

(provide 'omarchy-themer)
;;; omarchy-themer.el ends here
