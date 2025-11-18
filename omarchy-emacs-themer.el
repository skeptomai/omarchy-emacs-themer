;;; omarchy-emacs-themer.el --- Theme management for Doom Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Christopher Brown <cb@skeptomai.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (autothemer "0.2.2"))
;; Keywords: faces, themes
;; URL: https://github.com/skeptomai/omarchy-emacs-themer

;; This file is not part of GNU Emacs.

;;; Commentary:

;; omarchy-emacs-themer provides theme management utilities for Doom Emacs
;; built on top of the autothemer package.
;;
;; Usage:
;;
;;   (require 'omarchy-emacs-themer)
;;

;;; Code:

(require 'autothemer)

(defgroup omarchy-emacs-themer nil
  "Theme management for Doom Emacs."
  :group 'faces
  :prefix "omarchy-emacs-themer-")

(defcustom omarchy-emacs-themer-theme-directory
  (expand-file-name "themes/" user-emacs-directory)
  "Directory where omarchy-emacs-themer looks for theme files."
  :type 'directory
  :group 'omarchy-emacs-themer)

;;;###autoload
(defun omarchy-emacs-themer-add-theme-directory ()
  "Add `omarchy-emacs-themer-theme-directory' to `custom-theme-load-path'.
Only adds it as the first element if it's not already present in the list."
  (interactive)
  (let ((theme-dir (file-name-as-directory
                    (expand-file-name omarchy-emacs-themer-theme-directory))))
    (unless (member theme-dir custom-theme-load-path)
      (push theme-dir custom-theme-load-path)
      (message "Added %s to custom-theme-load-path" theme-dir))))

;;;###autoload
(defun omarchy-emacs-themer-install-and-load (theme-file &optional theme-name)
  "Copy THEME-FILE to `omarchy-emacs-themer-theme-directory' and load the theme.
If THEME-NAME is provided, use that; otherwise derive from filename by
removing -theme.el suffix (e.g., foo-theme.el -> foo)."
  (interactive "fTheme file: ")
  (let* ((theme-dir (file-name-as-directory
                     (expand-file-name omarchy-emacs-themer-theme-directory)))
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
    (omarchy-emacs-themer-add-theme-directory)
    ;; Load the theme (automatically finds and loads the file)
    (load-theme theme-name t)
    (message "Loaded theme: %s" theme-name)))

;;;###autoload
(defun omarchy-emacs-themer-version ()
  "Display the current version of omarchy-emacs-themer."
  (interactive)
  (message "omarchy-emacs-themer version 0.1.0"))

(provide 'omarchy-emacs-themer)
;;; omarchy-emacs-themer.el ends here
