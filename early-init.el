;;; early-init.el --- LEmacs (Lionyx Emacs)  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; Early init configuration for LEmacs
;;
;;; Code:

;;; Performance HACKs
;; --- Max GC Buffer while starting Emacs
;; --- Quickier filename handling
;; --- Both reseted after loading

;; GC Defaults
(setq read-process-output-max (* 1024 1024 3)) ;; 3mb

(setq gc-cons-threshold most-positive-fixnum   ;; 2^61 bytes
      gc-cons-percentage 0.6)

;; Empty file-name-handler-alist while booting
(defvar lemacs/file-name-handler-alist-cache file-name-handler-alist)

(setq file-name-handler-alist nil)

(defun lemacs/restore-post-init-settings ()
  "Restore file-name-handler after Emacs is booted."
  (setq gc-cons-threshold 16777216 ; 16mb
        gc-cons-percentage 0.1)
  (setq file-name-handler-alist lemacs/file-name-handler-alist-cache))

(add-hook 'emacs-startup-hook #'lemacs/restore-post-init-settings)

;; Defer GC to "after" using the minibuffer, avoiding it to be unresponsive
(defun lemacs/defer-gc ()
  "Defer GC."
  (setq gc-cons-threshold most-positive-fixnum))
(defun lemacs/-do-restore-gc ()
  "Restore gc."
  (setq gc-cons-threshold 16777216))
(defun lemacs/restore-gc ()
  "Restore gc after timer."
  (run-at-time 1 nil #'lemacs/-do-restore-gc))

(add-hook 'minibuffer-setup #'lemacs/defer-gc)
(add-hook 'minibuffer-exit #'lemacs/restore-gc)

;; Starts with the most fundamental mode
(setq initial-major-mode 'fundamental-mode)

;; Ensure Emacs loads the most recent byte-compiled files.
(setq load-prefer-newer t)


;;; Native Compile Settings
;;  I use a <emacs-default-dir>/eln-cache-<machine-hostname>/ dir to store
;;  my different machines eln compiled code. So the same config (and dirs)
;;  can be used on several machines.
;;  This is on early init to avoid Emacs on start compiling the native
;;  packages in eln-cache and than having another eln-cache-debian incomplete,
;;  causing several unwanted recompilations.
(when (featurep 'native-compile)
  ;; Set the right directory to store the native compilation cache
  (let ((path (expand-file-name (concat "eln-cache-" (system-name) "/") user-emacs-directory)))
    (setq-default native-comp-eln-load-path       (list path)
                  native-compile-target-directory path)
    (startup-redirect-eln-cache path)
  (setq-default native-comp-async-report-warnings-errors nil  ;; Silence compiler warnings as they can be pretty disruptive
                native-comp-jit-compilation              t    ;; Make native compilation happens asynchronously
                package-native-compile                   t)))  ;; Compile installed packages

;; Ensure that quitting only occurs once Emacs finishes native compiling,
;; preventing incomplete or leftover compilation files in `/tmp`.
(setq native-comp-async-query-on-exit t)
(setq confirm-kill-processes t)

;;; LSP Flag to use PLISTS
(setenv "LSP_USE_PLISTS" "true")

;;; Hack to avoid being flashbanged
(defun lemacs/avoid-initial-flash-of-light ()
  "Avoid flash of light when starting Emacs."
  (setq mode-line-format nil)
  ;; These colors should match your selected theme for maximum effect
  ;; Note that for catppuccin whenever we create a new frame or open it on terminal
  ;; it is necessary to reload the theme.
  (set-face-attribute 'default nil :background "#1E1E2D" :foreground "white")
  (set-face-attribute 'mode-line nil :background "#1E1E2D" :foreground "white" :box 'unspecified))

(lemacs/avoid-initial-flash-of-light)

;;; Better Window Management handling
(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format '("Emacs"))

(setq inhibit-compacting-font-caches t)

;;; Disables unused UI Elements
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

(provide 'early-init)
;;; early-init.el ends here
