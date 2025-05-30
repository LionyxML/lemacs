;;; lemacs-init.el --- LEmacs (Lionyx Emacs) -*- lexical-binding: t; -*-
;;
;; Author: Rahul M. Juliato <rahul.juliato@gmail.com>
;; URL: https://github.com/LionyxML/lemacs
;; Keywords: config, emacs, init
;; Version: 0.2.3
;; Package-Requires: ((emacs "30"))

;;; Commentary:
;; Early init configuration for LEmacs.

;;; License:
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;; We are going to use the straight package manager
(setq package-enable-at-startup nil)

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
(let ((path (expand-file-name "eln-cache/" user-emacs-directory))) ;; This is the default, I'm leaving this here so if
    (setq-default native-comp-eln-load-path       (list path)      ;; we want to change it, it is easier.
                  native-compile-target-directory path)
    (startup-redirect-eln-cache path))
(setq-default native-comp-async-report-warnings-errors nil  ;; Silence compiler warnings as they can be pretty disruptive
              native-comp-jit-compilation              t    ;; Make native compilation happens asynchronously
              package-native-compile                   nil) ;; We want straight to compile packages for us

;; Ensure that quitting only occurs once Emacs finishes native compiling,
;; preventing incomplete or leftover compilation files in `/tmp`.
(setq native-comp-async-query-on-exit t)
(setq confirm-kill-processes t)

(setq native-comp-async-jobs-number 1) ;; Slower but also quieter

;;; LSP Flag to use PLISTS
(setenv "LSP_USE_PLISTS" "true")

;;; Hack to avoid being flashbanged
(defun lemacs/avoid-initial-flash-of-light ()
  "Avoid flash of light when starting Emacs."
  (setq mode-line-format nil)
  ;; These colors should match your selected theme for maximum effect
  ;; Note that for catppuccin whenever we create a new frame or open it on terminal
  ;; it is necessary to reload the theme.
  (set-face-attribute 'default nil :background "#292D3E" :foreground "white")
  (set-face-attribute 'mode-line nil :background "#292D3E" :foreground "white" :box 'unspecified))

(lemacs/avoid-initial-flash-of-light)

;;; Better Window Management handling
(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format '("Emacs"))

;;; Don´t compact font caches during GC
(setq inhibit-compacting-font-caches t)

;; Avoid raising the *Messages* buffer if anything is still without
;; lexical bindings
(setq warning-minimum-level :error)
(setq warning-suppress-types '((lexical-binding)))

;;; Disables unused UI Elements
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

(provide 'early-init)
;;; early-init.el ends here
