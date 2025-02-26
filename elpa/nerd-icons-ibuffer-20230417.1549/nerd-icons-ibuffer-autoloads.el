;;; nerd-icons-ibuffer-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from nerd-icons-ibuffer.el

(autoload 'nerd-icons-ibuffer-mode "nerd-icons-ibuffer" "\
Display icons for all buffers in ibuffer.

This is a minor mode.  If called interactively, toggle the
`Nerd-Icons-Ibuffer mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable the
mode if ARG is nil, omitted, or is a positive number.  Disable the mode
if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate the variable `nerd-icons-ibuffer-mode'.

The mode's hook is called both when the mode is enabled and when it is
disabled.

(fn &optional ARG)" t)
(register-definition-prefixes "nerd-icons-ibuffer" '("nerd-icons-ibuffer-"))

;;; End of scraped data

(provide 'nerd-icons-ibuffer-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; nerd-icons-ibuffer-autoloads.el ends here
