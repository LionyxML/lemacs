;;; init.el --- LEmacs (Lionyx Emacs) -*- lexical-binding: t; -*-
;; Author: Rahul M. Juliato <rahul.juliato@gmail.com>
;; URL: https://github.com/LionyxML/lemacs
;; Keywords: config, emacs, init
;; Version: 0.2.1
;; Package-Requires: ((emacs "30"))

;;; Commentary:
;; My personal always evolving Emacs config file.

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

;;; --------------------------------- PERFORMANCE HACKS
;; --- Max GC Buffer while starting Emacs
;; --- Quickier filename handling
;; --- Both reseted after loading
(setq read-process-output-max (* 1024 1024 3)) ;; 3mb

(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
  gc-cons-percentage 0.6)

(defvar lemacs/file-name-handler-alist-cache file-name-handler-alist)

(setq file-name-handler-alist nil)

(defun lemacs/restore-post-init-settings ()
  (setq gc-cons-threshold 16777216 ; 16mb
        gc-cons-percentage 0.1)
  (setq file-name-handler-alist lemacs/file-name-handler-alist-cache))

(add-hook 'emacs-startup-hook #'lemacs/restore-post-init-settings)

;; --- Defer GC to "after" using the minibuffer, avoiding it to be unresponsive
(defun lemacs/defer-gc ()
  (setq gc-cons-threshold most-positive-fixnum))
(defun lemacs/-do-restore-gc ()
  (setq gc-cons-threshold 16777216))
(defun lemacs/restore-gc ()
  (run-at-time 1 nil #'lemacs/-do-restore-gc))

(add-hook 'minibuffer-setup #'lemacs/defer-gc)
(add-hook 'minibuffer-exit #'lemacs/restore-gc)

;;--- Starts with the most funcamental mode
(setq initial-major-mode 'fundamental-mode)

;;; --------------------------------- NATIVE COMP SETTINGS
(when (featurep 'native-compile)
  ;; Set the right directory to store the native compilation cache
  (let ((path (expand-file-name "eln-cache/" user-emacs-directory)))
    (setq-default native-comp-eln-load-path       (list path)
                  native-compile-target-directory path)
    (when (fboundp 'startup-redirect-eln-cache)
      (startup-redirect-eln-cache path)))
  (setq-default native-comp-async-report-warnings-errors nil  ;; Silence compiler warnings as they can be pretty disruptive
                native-comp-deferred-compilation         t    ;; Make native compilation happens asynchronously
                package-native-compile                   t))  ;; Compile installed packages

;;; --------------------------------- USE-PACKAGE INIT
;; Package sources
(eval-when-compile
  (require 'use-package))

(require 'package)
(setq package-check-signature nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(add-to-list 'load-path "~/.emacs.d/site-lisp")

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))


;;; --------------------------------- LEMACS INSTALLER
(defun lemacs/first-install ()
  "Install tree-sitter grammars and nerd-icons fonts on the first run."
  (interactive)
  (switch-to-buffer "*Messages*")

  (message ">>> All required packages installed.")
  (message ">>> Configuring LEmacs...")

  (message ">>> Configuring Tree Sitter parsers...")
  (require 'treesit-auto)
  (treesit-auto-install-all)

  (message ">>> Configuring Nerd Fonts...")
  (require 'nerd-icons)
  (nerd-icons-install-fonts)

  (message ">>> Native compile 3rd-party packages...")
  (native-compile-prune-cache)
  (dolist (dir (directory-files package-user-dir t "^[^.]" t))
    (when (file-directory-p dir)
      (byte-recompile-directory dir 0 t)
      (native-compile-async dir 'recursively)))
 
  (message ">>> LEmacs installed!!! Presss any key to close the installer and open Emacs normally.")
  (read-key)
  (kill-emacs))

;;; --------------------------------- LEMACS CUSTOM OPTIONS
(defcustom lemacs-input-mode 'evil
  "The LSP implementation to use."
  :type '(choice
           (const :tag "evil" evil)
           (const :tag "emacs" emacs))
  :group 'lemacs)

(defcustom lemacs-lsp-client 'lsp-mode
  "The LSP implementation to use."
  :type '(choice
           (const :tag "eglot" eglot)
           (const :tag "lsp-mode" lsp-mode)
           (const :tag "none" nil))
  :group 'lemacs)

(defcustom lemacs-in-buffer-completion 'corfu
  "The in-buffer completion to use."
  :type '(choice
           (const :tag "corfu" corfu)
           (const :tag "none" nil))
  :group 'lemacs)

(defcustom lemacs-polymode 'off
  "Enables polymode, like to styled-components on style[s|d].[t|j]sx? files.
Notice this is a bit messy."
  :type '(choice
           (const :tag "on" 1)
           (const :tag "off" nil))
  :group 'lemacs)

(defcustom lemacs-ligatures 'on
  "Enables fonts ligatures."
  :type '(choice
           (const :tag "on" 1)
           (const :tag "off" nil))
  :group 'lemacs)

(defcustom lemacs-docker-executable 'podman
  "The executable to be used with docker-mode."
  :type '(choice
           (const :tag "docker" docker)
           (const :tag "podman" podman))
  :group 'lemacs)

(defcustom lemacs-nerd-icons 't
  "Enables Nerd Icons provided by Nerd Fonts."
  :type '(choice
           (const :tag "t" t)
           (const :tag "nil" nil))
  :group 'lemacs)

(defcustom lemacs-ascii-art 't
  "Enables ASCII art on GUI Emacs."
  :type '(choice
           (const :tag "t" t)
           (const :tag "nil" nil))
  :group 'lemacs)

(defvar lemacs-art "
          ████████  ▄▄▄▄▄███▄▄▄▄▄    ████████  ████████ █████████
█       ██    ███ ██▀▀▀███▀▀▀██  ██    ███ ██    ███ ███    ███
███       ███    ██ ███   ███   ███  ███    ███ ███    ██ ███    ██
███      ███▄▄▄     ███   ███   ███  ███    ███ ███        ███
███       ███▀▀▀     ███   ███   ███ ██████████ ███       ██████████
███       ███    ██ ███   ███   ███  ███    ███ ███    ██        ███
███     ███    ███ ███   ███   ███  ███    ███ ███    ███ ██    ███
█████████ ██████████ ██   ███   ██  ███    ██ ██████████████████

")

(defcustom lemacs-start-transparent 't
  "Makes Emacs use Transparency when loaded."
  :type '(choice
           (const :tag "t" t)
           (const :tag "nil" nil))
  :group 'lemacs)

(defcustom lemacs-default-terminal-emulator 'eat
  "Default terminal `emulator/shell' for lemacs.
Possible values are `eshell' or `eat'.  Yes, I known,
eshell is not a term emulator, but on broader terms,
it is a shell inside a window, hence I'm threading
both as options to ~when I need to run a term~."
  :type 'symbol
  :group 'lemacs)

(defcustom lemacs-default-projects-folder "~/Projects"
  "Default place to search for projects with `lemacs/find-projects-and-switch'."
  :type 'string
  :group 'lemacs)

(defcustom lemacs-default-theme 'catppuccin
  "Default LEmacs Theme.  Change it to nil to set your own."
  :type '(choice
           (const :tag "catppuccin" "catppuccin")
           (const :tag "modus" "modus")
           (const :tag "nil" nil))
  :group 'lemacs)

(defcustom lemacs-default-initial-buffer 'dashboard
  "Default LEmacs initial buffer."
  :type '(choice
           (const :tag "scratch" "scratch")
           (const :tag "dashboard" "dashboard")
           (const :tag "terminal" "terminal"))
  :group 'lemacs)

(defcustom lemacs-codeium-scope 'nil
  "Default Codeium (IA assist) scope."
  :type '(choice
           (const :tag "everywhere" "everywhere")
           (const :tag "prog-mode" "prog-mode")
           (const :tag "nil" nil))
  :group 'lemacs)

;;; --------------------------------- EMACS
(use-package emacs
  :custom
  ;; PLEASE DO NOT FORGET!!!!
  ;; Each system uses gnupg with a diferent agent.
  ;; From TUI, maybe it is enough to set something like (bash)
  ;;   export GPG_TTY=$(tty)
  ;; On the other hand for GUI, you need to have something
  ;; on the ~/.gnupg/gpg-agent.conf, like for macos,
  ;;   brew install pinentry-mac
  ;;   echo "pinentry-program /opt/homebrew/bin/pinentry-mac" > ~/.gnupg/gpg-agent.conf
  ;; on Linux it might be something like:
  ;;   pinentry-program /usr/bin/pinentry-gtk-2
  ;; or... pinentry, pinentry-gnome3, pinentry-x11, etc.
  (auth-sources
   (list (expand-file-name ".authinfo.gpg" user-emacs-directory)))
  (undo-limit 67108864) ; 64mb.
  (undo-strong-limit 100663296) ; 96mb.
  (undo-outer-limit 1006632960) ; 960mb.
  (auto-save-default nil)
  (create-lockfiles nil)
  (delete-by-moving-to-trash t)
  (display-line-numbers-type 'relative)
  (enable-recursive-minibuffers t)
  (ibuffer-show-empty-filter-groups nil)
  (indent-tabs-mode nil)
  (inhibit-splash-screen t)
  (inhibit-startup-buffer-menu t)
  (inhibit-startup-echo-area-message user-login-name)
  (inhibit-startup-screen t)
  (inhibit-x-resources t)
  (initial-scratch-message "")
  (ispell-dictionary "en_US")
  (line-spacing 1)
  (make-backup-files nil)
  (native-comp-async-report-warnings-errors 'silent)
  (org-babel-load-languages '((emacs-lisp . t) (python . t) (ruby . t) (shell . t)))
  (pixel-scroll-precision-use-momentum nil)
  (ring-bell-function 'ignore)
  (shr-use-colors nil)
  (split-width-threshold 300)
  (switch-to-buffer-obey-display-actions t)
  (tab-always-indent 'complete) ;; TAB serves as M-TAB to completion
  (tab-width 4)
  (treesit-font-lock-level 4)
  (truncate-lines t)
  (tsx-ts-mode-indent-offset 4)
  (typescript-ts-mode-indent-offset 4)
  (use-dialog-box nil)
  (use-file-dialog nil)
  (use-short-answers t)
  (warning-minimum-level :emergency)
  (window-combination-resize t)
  (grep-find-ignored-directories
   '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "node_modules" "build" "dist"))
  :hook
  (prog-mode . display-line-numbers-mode)
  :config
  ;; Settings per OS
  (set-face-attribute 'default nil :family "JetBrainsMono Nerd Font" :height 100)
  (when (eq system-type 'darwin)
    (setq insert-directory-program "gls")
    (setq mac-option-key-is-meta nil
          mac-option-modifier nil
          mac-command-key-is-meta t
          mac-command-modifier 'meta)
    (set-face-attribute 'default nil :family "JetBrainsMono Nerd Font" :height 130))

  ;; Modeline fonts ajustments per OS
  (unless (eq system-type 'darwin)
    (if (facep 'mode-line-active)
        (set-face-attribute 'mode-line-active nil
                            :family "JetBrainsMono Nerd Font"
                            :height 100) ; For 29+
      (set-face-attribute 'mode-line nil
                          :family "JetBrainsMono Nerd Font"
                          :height 100))
    (set-face-attribute 'mode-line-inactive nil
                        :family "JetBrainsMono Nerd Font"
                        :height 100))

  (when (eq system-type 'darwin)
    (if (facep 'mode-line-active)
        (set-face-attribute 'mode-line-active nil
                            :family "JetBrainsMono Nerd Font"
                            :height 130) ; For 29+
      (set-face-attribute 'mode-line nil
                          :family "JetBrainsMono Nerd Font"
                          :height 130))
    (set-face-attribute 'mode-line-inactive nil
                        :family "JetBrainsMono Nerd Font"
                        :height 130))

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (defun lemacs/find-projects-and-switch (&optional directory)
    "Find and switch to a project directory from ~/Projects."
    (interactive)
    (let* ((d (or directory lemacs-default-projects-folder))
           (find-command (concat "find " d " -mindepth 1 -maxdepth 4 -type d"))
           (project-list (split-string (shell-command-to-string find-command) "\n" t))
           (selected-project (completing-read "Select project: " project-list)))
      (when (and selected-project (file-directory-p selected-project))
        (project-switch-project selected-project))))

  (defun lemacs/transparency-set ()
    "Set frame transparency (Graphical Mode)."
    (interactive)
    (set-frame-parameter (selected-frame) 'alpha '(90 90)))

  (defun lemacs/transparency-unset ()
    "Unset frame transparency (Graphical Mode)."
    (interactive)
    (set-frame-parameter (selected-frame) 'alpha '(100 100)))

  ;; Apply transparency
  (when lemacs-start-transparent
    (lemacs/transparency-set))

  (defun lemacs/rename-buffer-and-move-to-new-window ()
    "Promotes a side buffer to a new window."
    (interactive)
    (let ((temp-name (make-temp-name "temp-buffer-")))
      (rename-buffer temp-name t)
      (delete-window)
      (split-window-right)
      (switch-to-buffer temp-name)))
  (global-set-key (kbd "C-x x x") 'lemacs/rename-buffer-and-move-to-new-window)

  (defun lemacs/all-available-fonts ()
    "Create and visit a buffer containing a sorted list of available fonts."
    (interactive)
    (let ((font-list (sort (x-list-fonts "*") #'string<))
          (font-buffer (generate-new-buffer "*Font List*")))
      (with-current-buffer font-buffer
        (dolist (font font-list)
          (let* ((font-family (nth 2 (split-string font "-"))))
            (insert (format "%s\n" (propertize font 'face `(:family ,font-family :height 110))))))
        (goto-char (point-min))
        (setq buffer-read-only t))
      (pop-to-buffer font-buffer)))

  ;; Enable indent-tabs-mode (no tabs) for all prog-modes
  (defun lemacs/prefer-tabs ()
    "Disables indent-tabs-mode, and prefer spaces over tabs."
    (interactive)
    (indent-tabs-mode -1))
  (add-hook 'prog-mode-hook #'lemacs/prefer-tabs)


  ;; Play media from eww through MPV
  (defun lemacs/eww-play-media ()
    "Play the current media link in MPV."
    (interactive)
    (eww-copy-page-url)
    (let ((url (current-kill 0)))
      (message (concat ">>> Sent to mpv: " url))
      (start-process "mpv" nil "mpv" "--cache=yes" "--force-window=yes" url)))

  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Unbinds C-z to (suspend-frame)
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z"))

  ;; Page down and center
  (global-set-key (kbd "C-v") (lambda ()
								(interactive)
								(scroll-up-command)
								(recenter)
								))


  ;; Page up and center if not on beginning of buffer
  (global-set-key (kbd "M-v") (lambda ()
								(interactive)
								(scroll-down-command)
								(unless (= (window-start) (point-min))
								  (recenter))
								(when (= (window-start) (point-min))
								  (let ((midpoint (/ (window-height) 2)))
									(goto-char (window-start))
									(forward-line midpoint)
									(recenter midpoint)))))


  (defun lemacs/outline-set-buffer-local-ellipsis (ellipsis)
    "Apply the ellipsis ELLIPSIS to outline mode locally to a buffer."
    (let* ((display-table (or buffer-display-table (make-display-table)))
           (face-offset (* (face-id 'shadow) (ash 1 22)))
           (value (vconcat (mapcar (lambda (c) (+ face-offset c)) ellipsis))))
      (set-display-table-slot display-table 'selective-display value)
      (setq buffer-display-table display-table)))
  (add-hook 'outline-minor-mode-hook
            #'(lambda() (lemacs/outline-set-buffer-local-ellipsis " ▼ ")))

  ;; Starts elisp with outline collapse
  (defun lemacs/elisp-mode-hook ()
    (interactive)
    (outline-minor-mode 1)
    (outline-hide-sublevels 1))
  ;; (add-hook 'emacs-lisp-mode-hook #'lemacs/elisp-mode-hook)


  ;; Save manual customizations to other file than init.el
  (setq custom-file (locate-user-emacs-file "custom-vars.el"))
  (load custom-file 'noerror 'nomessage)

  ;; Duplicate line
  (defun lemacs/duplicate-line-or-region (&optional n)
    "Duplicate current line, or region if active.
With argument N, make N copies.
negative N, comment out original line and use the absolute value."
    (interactive "*p")
    (let ((use-region (use-region-p)))
      (save-excursion
        (let ((text (if use-region                 ;Get region if active, otherwise line
                        (buffer-substring (region-beginning) (region-end))
                      (prog1 (thing-at-point 'line)
                        (end-of-line)
                        (if (< 0 (forward-line 1)) ;Go to beginning of next line, or make a new one
                            (newline))))))
          (dotimes (_ (abs (or n 1)))               ;Insert N times, or once if not specified
            (insert text))))
      (if use-region nil                                   ;Only if we're working with a line (not a region)
        (let ((pos (- (point) (line-beginning-position)))) ;Save column
          (if (> 0 n)                                      ;Comment out original with negative arg
              (comment-region (line-beginning-position) (line-end-position)))
          (forward-line 1)
          (forward-char pos)))))

  ;; Welcome to LEmacs
  (add-hook 'emacs-startup-hook
            (lambda ()
              (message "Emacs has fully loaded. This code runs after startup.")

              ;; (profiler-report)
              ;; (profiler-stop)

              (with-current-buffer (get-buffer-create "*scratch*")
                (insert (format "%s

    Loading time : %s
    Packages     : %s
"
                                lemacs-art
                                (emacs-init-time)
                                (number-to-string (length package-activated-list)))))))

  ;; LEmacs default starting buffer if no arguments or file
  (add-hook 'emacs-startup-hook
            (lambda ()
              (let* ((filtered-args (seq-filter
                                     (lambda (arg)
                                       (not (member arg '("-Q" "-nw" "--eval"))))
                                     command-line-args)))
                (when (= (length filtered-args) 1)
                  (ignore-errors
                    (pcase lemacs-default-initial-buffer
                      ('scratch (scratch-buffer))
                      ('dashboard (dashboard-open))
                      ('terminal (lemacs/open-term))))))))

  ;; Runs 'private.el' after Emacs inits
  (add-hook 'after-init-hook
            (lambda ()
              (let ((private-file (expand-file-name "private.el" user-emacs-directory)))
                (when (file-exists-p private-file)
                  (load private-file)))))

  :bind
  (("C-x C-b" . 'ibuffer))
  :init
  (when (eq lemacs-default-theme 'catppuccin)
    (ignore-errors
      (load-theme 'catppuccin :no-confirm)))

  ;; Makes everything accept utf-8 as default, so buffers with tsx and so
  ;; won't ask for encoding (because undecided-unix) every single keystroke
  (modify-coding-system-alist 'file "" 'utf-8)

  ;; Emacs frame starts focused
  (select-frame-set-input-focus (selected-frame))

  ;; Emacs frame starts maximized
  (toggle-frame-maximized)

  (delete-selection-mode 1)
  (blink-cursor-mode -1)
  (desktop-save-mode -1)
  (file-name-shadow-mode 1)
  (global-auto-revert-mode 1)
  (indent-tabs-mode -1)
  (pixel-scroll-precision-mode 1)
  (save-place-mode 1)
  (savehist-mode 1)
  (winner-mode 1)
  (xterm-mouse-mode 1))

;;; --------------------------------- DIRED
(use-package dired
  :ensure nil
  :defer t
  :custom
  (dired-dwim-target t)
  (dired-guess-shell-alist-user
   '(("\\.\\(png\\|jpe?g\\|tiff\\)" "feh" "xdg-open" "open")
     ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv" "xdg-open" "open")
     (".*" "xdg-open")))
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-listing-switches "-lah --group-directories-first")
  :init
  (defun dired-get-size ()
    "On hitting ? gets the selected or under cursor file/dir size."
    (interactive)
    (let ((files (dired-get-marked-files)))
      (with-temp-buffer
        (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
        (message "Size of all marked files: %s"
                 (progn
                   (re-search-backward "\\(^[0-9.,]+[A-Za-z]+\\).*total$")
                   (match-string 1)))))))

;;; --------------------------------- ISEARCH
(use-package isearch
  :ensure nil
  :defer t
  :config
  (setq isearch-lazy-count t)
  (setq lazy-count-prefix-format "(%s/%s) ")
  (setq lazy-count-suffix-format nil)
  (setq search-whitespace-regexp ".*?"))


;;; --------------------------------- GNUS
(use-package gnus
  :ensure nil
  :defer t
  :custom
  (gnus-init-file (expand-file-name "gnus/.gnus.el" user-emacs-directory)))

;;; --------------------------------- ERC
(use-package erc
  :ensure nil
  :defer t
  :init
  (with-eval-after-load 'erc
    (add-to-list 'erc-modules 'sasl))
  
  (setopt erc-sasl-mechanism 'external)

  (defun erc-liberachat ()
    (interactive)
    (erc-tls :server "irc.libera.chat"
             :port 6697
             :user ""
             :password ""
             :client-certificate
             (list
              ;; Put your certificate on ~/.emacs.d/erc/ or change this
              (expand-file-name "erc/cert.pem" user-emacs-directory)
              (expand-file-name "erc/cert.pem" user-emacs-directory))))

  (defun lemacs-erc-enable-flyspell ()
    "Enable Flyspell mode in ERC buffers."
    (flyspell-mode 1))
  (add-hook 'erc-join-hook 'lemacs-erc-enable-flyspell)

  :custom
  (erc-join-buffer 'window)
  (erc-buffer-display 'window)
  ;; (erc-interactive-display ...) ;; this option will be available on next ERC release (5.6)
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-timestamp-format "[%H:%M]")
  (erc-autojoin-channels-alist '((".*\\.libera\\.chat" "#emacs" "#systemcrafters"))))

;;; --------------------------------- ESHELL
(use-package eshell
  :ensure nil
  :defer nil
  :config
  (setq eshell-hist-ignoredups t)
  (setq eshell-cmpl-cycle-completions nil)
  (setq eshell-cmpl-ignore-case t)
  (setq eshell-ask-to-save-history (quote always))

  (defun eshell/x (&rest args)
    "Run a command in a vertical split `eat` buffer."
    (let ((command (car args))
          (arguments (cdr args)))
      (split-window-right)
      (other-window 1)
      (apply 'eat command arguments)))

  (defun lemacs/open-term  ()
    "Open the default terminal emulator based on lemacs-default-terminal-emulator."
    (interactive)
    (pcase lemacs-default-terminal-emulator
      ('eshell (eshell 'N))
      ('eat (eat nil 'N))
      (_ (error "Unknown terminal emulator: %s" lemacs-default-terminal-emulator))))

  (defun lemacs/close-term ()
    "Closes the eshell (or any buffer). If it is the last window, close the current tab instead of deleting the window."
    (interactive)
    (let ((current-tab (tab-bar--current-tab)))
      (kill-buffer (current-buffer))       ; Kill the current buffer
      (if (one-window-p)
          (tab-bar-close-tab current-tab)  ; Close the tab if it's the last window
        (delete-window))))                 ; Otherwise, just delete the window

  (defun lemacs/split-term-vertical ()
    "Split the window vertically and open a new instance of eshell."
    (interactive)
    (split-window-right)
    (other-window 1)
    (lemacs/open-term))

  (defun lemacs/split-term-horizontal ()
    "Split the window horizontally and open a new instance of eshell."
    (interactive)
    (split-window-below)
    (other-window 1)
    (lemacs/open-term))

  (defun lemacs/open-term-new-tab ()
    "Open eshell in a new tab."
    (interactive)
    (let ((new-tab (generate-new-buffer-name
                    (pcase lemacs-default-terminal-emulator
                      ('eshell "*eshell*")
                      ('eat "*eat*")
                      (_ (error "Unknown terminal emulator: %s" lemacs-default-terminal-emulator))))))
      (tab-new)
      (lemacs/open-term)
      (rename-buffer new-tab)))

  (defun lemacs/kill-all-shell-buffers ()
    "Kill all *eshell* buffers."
    (interactive)
    (let ((eshell-buffers
           (cl-remove-if-not
            (lambda (buffer)
              (string-prefix-p
               (pcase lemacs-default-terminal-emulator
                 ('eshell "*eshell*")
                 ('eat "*eat*")
                 (_ (error "Unknown terminal emulator: %s" lemacs-default-terminal-emulator)))
               (buffer-name buffer)))
            (buffer-list))))

      (if eshell-buffers
          (progn
            (message "Killing *eshell* buffers:")
            (dolist (buffer eshell-buffers)
              (message "  %s" (buffer-name buffer))
              (kill-buffer buffer)))
        (message "No *eshell* buffers to kill."))))

  (global-set-key (kbd "C-c e e") 'lemacs/open-term )
  (global-set-key (kbd "C-c e v") 'lemacs/split-term-vertical)
  (global-set-key (kbd "C-c e \\") 'lemacs/split-term-vertical)
  (global-set-key (kbd "C-c e |") 'lemacs/split-term-vertical)

  (global-set-key (kbd "C-c e h") 'lemacs/split-term-horizontal)
  (global-set-key (kbd "C-c e -") 'lemacs/split-term-horizontal)
  (global-set-key (kbd "C-c e s") 'lemacs/split-term-horizontal)

  (global-set-key (kbd "C-c e k") 'lemacs/kill-all-shell-buffers)
  (global-set-key (kbd "C-c e t") 'lemacs/open-term-new-tab)

  (global-set-key (kbd "C-c e x") 'lemacs/close-term)

  (add-hook 'eshell-mode-hook
			(lambda ()
              (progn
               (define-key eshell-mode-map "\C-a" 'eshell-bol)
               (define-key eshell-mode-map "\C-r" 'consult-history)
               (define-key eshell-mode-map [up] 'previous-line)
               (define-key eshell-mode-map [down] 'next-line))
              (local-set-key (kbd "C-l")
							 (lambda ()
                               (interactive)
                               (eshell/clear 1)
							   (eshell-send-input)
							   ))))

  (setq eshell-prompt-function
        (lambda ()
          (concat
           "┌─("
           (if (> eshell-last-command-status 0)
               (nerd-icons-faicon "nf-fa-close")
             (nerd-icons-faicon "nf-fa-check"))
           " "
           (number-to-string eshell-last-command-status)
           ")──("
           (nerd-icons-faicon "nf-fa-user")
           " "
           (user-login-name)
           ")──("
           (nerd-icons-mdicon "nf-md-clock")
           " "
           (format-time-string "%H:%M:%S" (current-time))
           ")──("
           (nerd-icons-faicon "nf-fa-folder")
           " "
           (concat (if (>= (length (eshell/pwd)) 40)
                       (concat "..." (car (last (butlast (split-string (eshell/pwd) "/") 0))))
                     (abbreviate-file-name (eshell/pwd))))
           ")\n"
           (when (and (fboundp 'vc-git-root) (vc-git-root default-directory))
             (concat
              "├─("
              (nerd-icons-devicon "nf-dev-git_branch")
              " "
              (car (vc-git-branches))
              (let* ((branch (car (vc-git-branches)))
                     (behind (string-to-number
                              (shell-command-to-string
                               (concat "git rev-list --count HEAD..origin/" branch)))))
                (if (> behind 0)
                    (concat "  " (nerd-icons-faicon "nf-fa-cloud_download") " " (number-to-string behind))
                  ""))

              (let ((modified (length (split-string
                                       (shell-command-to-string
                                        "git ls-files --modified") "\n" t)))
                    (untracked (length (split-string
                                        (shell-command-to-string
                                         "git ls-files --others --exclude-standard") "\n" t))))
                (concat
                 (if (> modified 0)
                     (concat "  " (nerd-icons-octicon "nf-oct-file_diff") " "
                             (number-to-string modified)))
                 (if (> untracked 0)
                     (concat "  " (nerd-icons-faicon "nf-fa-question_circle") " "
                             (number-to-string untracked)))))

              ")\n"
              ))
           "└─➜ ")))

  (setq eshell-prompt-regexp "└─➜ ")

  (add-hook 'eshell-mode-hook (lambda () (setenv "TERM" "xterm-256color")))

  (setq eshell-visual-commands
		'("vi" "screen" "top"  "htop" "btm" "less" "more" "lynx" "ncftp" "pine" "tin" "trn"
		  "elm" "irssi" "nmtui-connect" "nethack" "vim" "alsamixer" "nvim" "w3m"
		   "ncmpcpp" "newsbeuter" "nethack" "mutt" "yarn" "pnpm" "sudo" "mpv" "cava")))

;;; --------------------------------- VC
(use-package vc
  :ensure nil
  :defer t
  :config
  (setq vc-annotate-color-map
        '((20 . "#f5e0dc")
          (40 . "#f2cdcd")
          (60 . "#f5c2e7")
          (80 . "#cba6f7")
          (100 . "#f38ba8")
          (120 . "#eba0ac")
          (140 . "#fab387")
          (160 . "#f9e2af")
          (180 . "#a6e3a1")
          (200 . "#94e2d5")
          (220 . "#89dceb")
          (240 . "#74c7ec")
          (260 . "#89b4fa")
          (280 . "#b4befe"))))

;;;--------------------------------- WINDOW
(use-package window
  :ensure nil
  :defer t
  :custom
  (display-buffer-alist
   '(("\\*.*-e?shell\\*"  ;; we only want <project_name>-eshell to follow this rule
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . -1))
     ("\\*\\(Backtrace\\|Warnings?\\|Compile-Log\\|Messages\\|Bookmark List\\|Ibuffer\\|Occur\\|eldoc\\|sh\\|env\\|python3\\|sudo\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 0))
     ("\\*\\(Flymake diagnostics\\|prettier er\\|xref\\|EGLOT\\|Org-Babel Er\\|Completions\\)"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 1))
     ("\\*\\([Hh]elp\\)\\*"
      (display-buffer-in-side-window)
      (window-width . 75)
      (side . right)
      (slot . 0))
     ("\\*\\(undo-tree\\)\\*"
      (display-buffer-in-side-window)
      (window-width . 50)
      (side . right)
      (slot . 1)))))

;;; --------------------------------- TAB-BAR
(use-package tab-bar
  :ensure nil
  :defer t
  :custom
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  :init
  ;; This aims to substitute tmux (or gnu/screen) with Emacs
  ;; Tabs are our tmux windows (new one with C-x t 2)
  ;; Windows are emacs windows (new one with C-x 5 2)
  ;; Or even better Windows are Perspectives of `persp-mode' (persp-switch)

  (setq tab-bar-position t)

  (setq tab-bar-auto-width t)
  (setq tab-bar-auto-width-min '(10 4))
  (setq tab-bar-auto-width-max '(40 5))


  (defun lemacs/renumber-tabs (&optional include-file-name)
    "Renumber all tabs according to their position.
If INCLUDE-FILE-NAME is non-nil, include the file name in the tab name."
    (interactive "P")
    (let ((tabs (tab-bar-tabs)))
      (dotimes (i (length tabs))
        (let* ((tab (nth i tabs))
                (old-name (alist-get 'name tab))
                (file-name (if include-file-name
                             (replace-regexp-in-string "^\\([0-9]+) \\)" "" old-name)
                             ""))
                (new-name (format " »%d« %s" (1+ i) file-name)))
          (tab-bar-select-tab (1+ i)) ; Select the tab by its 1-based index
          (tab-bar-rename-tab new-name)))))


  ;; Whenever we modify tabs, we want it renumbered
  (advice-add 'tab-close :after #'lemacs/renumber-tabs)
  (advice-add 'tab-close-other :after #'lemacs/renumber-tabs)
  (advice-add 'tab-new :after #'lemacs/renumber-tabs)


  (defun lemacs/switch-tab-or-tab-bar ()
    "Switch between 2 tabs or choose if > 2 tabs are present."
    (interactive)
    (if (= (length (tab-bar-tabs)) 2)
      (tab-next)
      (call-interactively 'tab-bar-switch-to-tab)
      ))

  (global-set-key (kbd "M-r") 'lemacs/switch-tab-or-tab-bar))

;;; --------------------------------- DEFFERED BUILTINS
(use-package org
  :ensure nil
  :defer t
  :mode ("\\.org\\'" . org-mode))

(use-package recentf
  :ensure nil
  :defer t
  :hook
  ;; (after-init . recentf-mode)
  )

(use-package column-number
  :ensure nil
  :defer t
  :hook
  (after-init . column-number-mode))

;;; --------------------------------- EXTERNAL PACKAGES
(use-package evil
  :ensure t
  :defer t
  :hook
  (after-init . evil-mode)
  :init
  (setq evil-want-integration t)      ;; Integrate `evil' with other Emacs features (optional as it's true by default).
  (setq evil-want-keybinding nil)     ;; Disable default keybinding to set custom ones.
  (setq evil-want-C-u-scroll t)       ;; Makes C-u scroll
  (setq evil-want-C-u-delete t)       ;; Makes C-u delete on insert mode
  (setq evil-want-minibuffer t)       ;; Makes mini-buffer evil (so you can edit it, paste, etc.)
  :config
  (evil-set-undo-system 'undo-tree)   ;; Uses the undo-tree package as the default undo system

  ;; Set the leader key to space for easier access to custom commands. (setq evil-want-leader t)
  (setq evil-leader/in-all-states t)  ;; Make the leader key available in all states.
  (setq evil-want-fine-undo t)        ;; Evil uses finer grain undoing steps

  ;; Define the leader key as Space
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-set-leader 'visual (kbd "SPC"))

  ;; Keybindings for searching and finding files.
  (evil-define-key 'normal 'global (kbd "<leader> s f") 'consult-find)
  (evil-define-key 'normal 'global (kbd "<leader> s g") 'consult-grep)
  (evil-define-key 'normal 'global (kbd "<leader> s G") 'consult-git-grep)
  (evil-define-key 'normal 'global (kbd "<leader> s r") 'consult-ripgrep)
  (evil-define-key 'normal 'global (kbd "<leader> s h") 'consult-info)
  (evil-define-key 'normal 'global (kbd "<leader> /") 'consult-line)

  ;; Flymake navigation
  (evil-define-key 'normal 'global (kbd "<leader> x x") 'consult-flymake);; Gives you something like `trouble.nvim'
  (evil-define-key 'normal 'global (kbd "] d") 'flymake-goto-next-error) ;; Go to next Flymake error
  (evil-define-key 'normal 'global (kbd "[ d") 'flymake-goto-prev-error) ;; Go to previous Flymake error

  ;; Dired commands for file management
  (evil-define-key 'normal 'global (kbd "<leader> x d") 'dired)
  (evil-define-key 'normal 'global (kbd "<leader> x j") 'dired-jump)
  (evil-define-key 'normal 'global (kbd "<leader> x f") 'find-file)

  ;; Diff-HL navigation for version control
  (evil-define-key 'normal 'global (kbd "] c") 'diff-hl-next-hunk) ;; Next diff hunk
  (evil-define-key 'normal 'global (kbd "[ c") 'diff-hl-previous-hunk) ;; Previous diff hunk


  ;; Magit keybindings for Git integration
  (evil-define-key 'normal 'global (kbd "<leader> g g") 'magit-status)      ;; Open Magit status
  (evil-define-key 'normal 'global (kbd "<leader> g l") 'magit-log-current) ;; Show current log
  (evil-define-key 'normal 'global (kbd "<leader> g d") 'magit-diff-buffer-file) ;; Show diff for the current file
  (evil-define-key 'normal 'global (kbd "<leader> g D") 'diff-hl-show-hunk) ;; Show diff for a hunk
  (evil-define-key 'normal 'global (kbd "<leader> g b") 'vc-annotate)       ;; Annotate buffer with version control info

  ;; Buffer management keybindings
  (evil-define-key 'normal 'global (kbd "] b") 'switch-to-next-buffer) ;; Switch to next buffer
  (evil-define-key 'normal 'global (kbd "[ b") 'switch-to-prev-buffer) ;; Switch to previous buffer
  (evil-define-key 'normal 'global (kbd "<leader> b i") 'consult-buffer) ;; Open consult buffer list
  (evil-define-key 'normal 'global (kbd "<leader> b b") 'ibuffer) ;; Open Ibuffer
  (evil-define-key 'normal 'global (kbd "<leader> b d") 'kill-current-buffer) ;; Kill current buffer
  (evil-define-key 'normal 'global (kbd "<leader> b k") 'kill-current-buffer) ;; Kill current buffer
  (evil-define-key 'normal 'global (kbd "<leader> b x") 'kill-current-buffer) ;; Kill current buffer
  (evil-define-key 'normal 'global (kbd "<leader> b s") 'save-buffer) ;; Save buffer
  (evil-define-key 'normal 'global (kbd "<leader> b l") 'consult-buffer) ;; Consult buffer

  ;; Treemacs keybindings
  (evil-define-key 'normal 'global (kbd "<leader> e f") 'treemacs)
  (evil-define-key 'normal 'global (kbd "<leader> e e") 'treemacs)

  ;; Eshell/Eat management keybindings
  (evil-define-key 'normal 'global (kbd "<leader> t e") 'lemacs/open-term )
  (evil-define-key 'normal 'global (kbd "<leader> t v") 'lemacs/split-term-vertical)
  (evil-define-key 'normal 'global (kbd "<leader> t \\") 'lemacs/split-term-vertical)
  (evil-define-key 'normal 'global (kbd "<leader> t |") 'lemacs/split-term-vertical)

  (evil-define-key 'normal 'global (kbd "<leader> t h") 'lemacs/split-term-horizontal)
  (evil-define-key 'normal 'global (kbd "<leader> t -") 'lemacs/split-term-horizontal)
  (evil-define-key 'normal 'global (kbd "<leader> t s") 'lemacs/split-term-horizontal)

  (evil-define-key 'normal 'global (kbd "<leader> t k") 'lemacs/kill-all-shell-buffers)
  (evil-define-key 'normal 'global (kbd "<leader> t t") 'lemacs/open-term-new-tab)

  (evil-define-key 'normal 'global (kbd "<leader> t x") 'lemacs/close-term)

  ;; Managing tabs
  ;; ]t and [t are already set
  (evil-define-key 'normal 'global (kbd "<leader> t n") 'tab-new)
  (evil-define-key 'normal 'global (kbd "<leader> t c") 'tab-close)
  (evil-define-key 'normal 'global (kbd "<leader> t l") 'lemacs/switch-tab-or-tab-bar)

  ;; Project management keybindings
  (evil-define-key 'normal 'global (kbd "<leader> p b") 'consult-project-buffer) ;; Consult project buffer
  (evil-define-key 'normal 'global (kbd "<leader> p p") 'lemacs/find-projects-and-switch ) ;; Find projects
  (evil-define-key 'normal 'global (kbd "<leader> p f") 'project-find-file) ;; Find file in project
  (evil-define-key 'normal 'global (kbd "<leader> p g") 'project-find-regexp) ;; Find regexp in project
  (evil-define-key 'normal 'global (kbd "<leader> p k") 'project-kill-buffers) ;; Kill project buffers
  (evil-define-key 'normal 'global (kbd "<leader> p D") 'project-dired) ;; Dired for project

  ;; Perspective keybindings
  (evil-define-key 'normal 'global (kbd "<leader> p a") 'persp-add-buffer)

  (evil-define-key 'normal 'global (kbd "<leader> p s") 'persp-switch)
  (defun lemacs/consult-or-persp-buffer ()
    "Use `persp-switch-to-buffer` if `persp-mode` is active, otherwise `consult-buffer`."
    (interactive)
    (if (bound-and-true-p persp-mode)
        (call-interactively #'persp-switch-to-buffer)
      (consult-buffer)))
  (evil-define-key 'normal 'global (kbd "<leader>SPC") 'lemacs/consult-or-persp-buffer )

  ;; Yank from kill ring
  (evil-define-key 'normal 'global (kbd "P") 'consult-yank-from-kill-ring)
  (evil-define-key 'normal 'global (kbd "<leader> P") 'consult-yank-from-kill-ring)

  ;; Undo tree visualization
  (evil-define-key 'normal 'global (kbd "<leader> u") 'undo-tree-visualize)

  ;; Help keybindings
  (evil-define-key 'normal 'global (kbd "<leader> h m") 'describe-mode) ;; Describe current mode
  (evil-define-key 'normal 'global (kbd "<leader> h f") 'describe-function) ;; Describe function
  (evil-define-key 'normal 'global (kbd "<leader> h v") 'describe-variable) ;; Describe variable
  (evil-define-key 'normal 'global (kbd "<leader> h k") 'describe-key) ;; Describe key

  ;; Tab navigation
  (evil-define-key 'normal 'global (kbd "] t") 'tab-next) ;; Go to next tab
  (evil-define-key 'normal 'global (kbd "[ t") 'tab-previous) ;; Go to previous tab


  ;; Custom example. Formatting with prettier tool.
  (evil-define-key 'normal 'global (kbd "<leader> m p")
    (lambda ()
      (interactive)
      (shell-command (concat "prettier --write " (shell-quote-argument (buffer-file-name))))
      (revert-buffer t t t)))

  ;; LSP commands keybindings
  (evil-define-key 'normal lsp-mode-map
    ;; (kbd "gd") 'lsp-find-definition                ;; evil-collection already provides gd
    (kbd "gr") 'lsp-find-references                   ;; Finds LSP references
    (kbd "<leader> c a") 'lsp-execute-code-action     ;; Execute code actions
    (kbd "<leader> r n") 'lsp-rename                  ;; Rename symbol
    (kbd "gI") 'lsp-find-implementation               ;; Find implementation
    (kbd "<leader> l f") 'lsp-format-buffer)          ;; Format buffer via lsp


  (defun ek/lsp-describe-and-jump ()
	"Show hover documentation and jump to *lsp-help* buffer."
	(interactive)
	(lsp-describe-thing-at-point)
	(let ((help-buffer "*lsp-help*"))
      (when (get-buffer help-buffer)
		(switch-to-buffer-other-window help-buffer))))
  ;; Open hover documentation
  (evil-define-key 'normal 'global (kbd "K") 'ek/lsp-describe-and-jump)
  ;; Yeah, on terminals, Emacs doesn't support (YET), the use of floating windows,
  ;; thus, this will open a small buffer bellow your window.
  ;; This floating frames are called "child frames" and some recent effort is being put
  ;; into having a translation of those marvelous GUI stuff to terminal. Let's hope
  ;; we add this to Emacs Kick soom :)

  ;; Commenting functionality for single and multiple lines
  (evil-define-key 'normal 'global (kbd "gcc")
    (lambda ()
      (interactive)
      (if (not (use-region-p))
          (comment-or-uncomment-region (line-beginning-position) (line-end-position)))))
  
  (evil-define-key 'visual 'global (kbd "gc")
    (lambda ()
      (interactive)
      (if (use-region-p)
          (comment-or-uncomment-region (region-beginning) (region-end)))))


  (defun lemacs-open-eldoc ()
    "Toggle the Eldoc documentation buffer. Enable Eldoc if not already enabled."
    (interactive)
    ;; Ensure eldoc-mode is active
    (unless (bound-and-true-p eldoc-mode)
      (eldoc-mode 1))
    ;; Open or toggle the eldoc documentation buffer
    (let ((eldoc-buf (eldoc-doc-buffer)))
      (if (get-buffer-window eldoc-buf)
          (quit-window nil (get-buffer-window eldoc-buf))  ;; Close if visible
        (display-buffer eldoc-buf))))                      ;; Open if not visible

  (if (display-graphic-p)
      (evil-define-key 'normal 'global (kbd "K") #'eldoc-box-help-at-point)
    (evil-define-key 'normal 'global (kbd "K") #'lemacs-open-eldoc)
    (global-set-key (kbd "C-h C-.") #'eldoc-box-help-at-point))


  ;; On minibuffer, makes C-p C-n work with selections on vertico
  (eval-after-load "evil-maps"
    (dolist (map '(evil-motion-state-map
                   evil-insert-state-map
                   evil-emacs-state-map))
      (define-key (eval map) "\C-n" nil)
      (define-key (eval map) "\C-p" nil)))

  (evil-mode 1))

(use-package evil-collection
  :defer t
  :ensure t
  :custom
  (evil-collection-want-find-usages-bindings t)
  :hook
  (evil-mode . evil-collection-init))

(use-package evil-surround
  :ensure t
  :after evil-collection
  :config
  (global-evil-surround-mode 1))

(use-package evil-matchit
  :ensure t
  :after evil-collection
  :config
  (global-evil-matchit-mode 1))

(use-package 0x0
  :ensure t
  :defer t)

(use-package add-node-modules-path
  :ensure t
  :defer t
  :custom
  ;; Makes sure you are using the local bin for your
  ;; node project. Local eslint, typescript server...
  (eval-after-load 'typescript-ts-mode
	'(add-hook 'typescript-ts-mode-hook #'add-node-modules-path))
  (eval-after-load 'tsx-ts-mode
	'(add-hook 'tsx-ts-mode-hook #'add-node-modules-path))
  (eval-after-load 'typescriptreact-mode
	'(add-hook 'typescriptreact-mode-hook #'add-node-modules-path))
  (eval-after-load 'js-mode
	'(add-hook 'js-mode-hook #'add-node-modules-path)))

(use-package ace-window
  :ensure t
  :defer t
  :bind
  ("M-O" . ace-window)
  ("M-o" . ace-window))

(use-package add-node-modules-path
  :defer t
  :ensure t
  :config)

(use-package breadcrumb
  :defer t
  :ensure t
  :hook
  (eglot-connect . breadcrumb-mode)
  :config)

(use-package dashboard
  :ensure t
  :defer t
  :config
  ;; (setq dashboard-startup-banner (".....logo.png" . ".....logo.txt"))
  ;; (setq dashboard-startup-banner 'logo)
  (when lemacs-ascii-art
    (setq dashboard-startup-banner
          (expand-file-name "assets/lemacs_logo.txt" user-emacs-directory)))

  (unless lemacs-ascii-art
    (setq dashboard-startup-banner
		  (cons (expand-file-name "assets/lemacs_logo.png" user-emacs-directory)
                (expand-file-name "assets/lemacs_logo.txt" user-emacs-directory))))

  (setq dashboard-center-content t)
  (setq dashboard-vertically-center-content nil)
  (setq dashboard-display-icons-p t)

  (when lemacs-nerd-icons
    (setq dashboard-icon-type 'nerd-icons))


  (setq dashboard-show-shortcuts nil)
  (setq dashboard-set-heading-icons nil)

  (setq dashboard-startupify-list '(dashboard-insert-newline
				                    dashboard-insert-banner
				                    dashboard-insert-newline
				                    dashboard-insert-init-info
				                    dashboard-insert-newline
				                    dashboard-insert-newline
				                    dashboard-insert-navigator
				                    dashboard-insert-newline
				                    dashboard-insert-newline
                                    dashboard-insert-items
				                    ))

  (setq dashboard-items '(
                          ;; (recents   . 5)
                          ;; (bookmarks . 5)
                          ;; (projects  . 5)
                          ;; (agenda    . 5)
                          ;; (registers . 5)
						  ))

  (setq dashboard-navigator-buttons ;; format: icon title help action face prefix suffix`.
        `((
           (,(nerd-icons-faicon "nf-fa-envelope") "gnus" "" (lambda (&rest _) (gnus)))
           (,(nerd-icons-faicon "nf-fa-rss") "elfeed" "" (lambda (&rest _) (elfeed)))
           (,(nerd-icons-faicon "nf-fa-firefox") "eww" "" (lambda (&rest _) (call-interactively 'eww)))
           (,(nerd-icons-faicon "nf-fa-music") "emms" "" (lambda (&rest _) (emms-browser)))
           (,(nerd-icons-faicon "nf-fa-youtube_play") "yeetube" "" (lambda (&rest _) (call-interactively 'yeetube-search)))
           (,(nerd-icons-faicon "nf-fa-hashtag") "erc" "" (lambda (&rest _) (call-interactively 'erc-tls)))
           (,(nerd-icons-faicon "nf-fa-cog") "config" "" (lambda (&rest _) (call-interactively (customize-group 'lemacs))))
           )))

  (dashboard-setup-startup-hook))

(use-package diff-hl
  :defer t
  :ensure t
  :hook
  (find-file . (lambda ()
                 (global-diff-hl-mode)           ;; Enable Diff-HL mode for all files.
                 (diff-hl-flydiff-mode)          ;; Automatically refresh diffs.
                 (diff-hl-margin-mode)))         ;; Show diff indicators in the margin.
  :custom
  (diff-hl-side 'left)                           ;; Set the side for diff indicators.
  (diff-hl-margin-symbols-alist '((insert . "│") ;; Customize symbols for each change type.
                                   (delete . "-")
                                   (change . "│")
                                   (unknown . "?")
                                   (ignored . "i")))
  :config
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode))

(use-package async
  :defer t
  :ensure t
  :hook
  ((dired-mode . dired-async-mode)
   (after-init . async-bytecomp-package-mode)))

(use-package diredfl
  :defer t
  :ensure t
  :hook
  (dired-mode . diredfl-global-mode))

(use-package docker
  :defer t
  :ensure t
  :bind ("C-c d" . docker)
  :config
  (when (eq lemacs-docker-executable 'docker)
    (setq docker-command "docker")
    (setq docker-compose-command "docker-compose"))

  (when (eq lemacs-docker-executable 'podman)
    (setq docker-command "podman")
    (setq docker-compose-command "podman-compose")))

(use-package dockerfile-mode
  :defer t
  :ensure t
  :mode "\\Dockerfile\\'"
  :config)

(use-package doom-modeline
  :defer t
  :ensure t
  :hook
  (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-buffer-file-name-style 'buffer-name)
  (doom-modeline-project-detection 'project)
  (doom-modeline-buffer-name t)
  (doom-modeline-vcs-max-length 25)
  (doom-modeline-icon lemacs-nerd-icons)
  :config
  (setq inhibit-compacting-font-caches t) ;; Don´t compact font caches during GC
)

(use-package dotenv-mode
  :defer t
  :ensure t
  :config)

(use-package eat
  :defer t
  :ensure t
  :config
  ;; Add to your .bashrc
  ;; [ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
  ;; source "$EAT_SHELL_INTEGRATION_DIR/bash"

  ;; Makes eat quicker
  (setq process-adaptive-read-buffering nil)

  (setq eat-term-name "xterm-256color")

  ;; Runs not compatible eshell term stuff with eat on the same buffer
  (add-hook 'eshell-load-hook #'eat-eshell-mode)

  ;; Runs listed 'visual-mode' eshell stuff with eat on separated buffer
  ;; (takes precedence over the above setting)
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)

  ;; Disabled line/number minor modes on modeline
  (add-hook 'eat-mode-hook (lambda ()
                             (setq-local line-number-mode nil)
                             (setq-local column-number-mode nil))))

(use-package elfeed
  :defer t
  :ensure t
  :config
  (define-advice elfeed-search--header (:around (oldfun &rest args))
  (if elfeed-db
      (apply oldfun args)
    "No database loaded yet"))

  ;; NOTE: set your feeds like these
  ;; (setq elfeed-feeds
  ;;       '(
  ;;         "https://planet.emacslife.com/atom.xml"
  ;;         "https://www.rahuljuliato.com/rss.xml"
  ;;         "https://www.youtube.com/feeds/videos.xml?channel_id=UCAiiOTio8Yu69c3XnR7nQBQ"
  ;;         ))

  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-url "youtube\\.com"
                                :add '(video youtube)))

  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :before "2 weeks ago"
                                :remove 'unread))

(defun lemacs/elfeed-play-enclosure ()
  "Play elfeed enclosure file (for podcasts)."
  (interactive)
  (require 'mpv)
  (let ((entry elfeed-show-entry))
    (if entry
        (let ((entry elfeed-show-entry)
              (url (car (elt (elfeed-entry-enclosures entry)  0 ))))
          (message (concat ">>> Loading: " url))
          (mpv-play-url url))))))

(use-package elfeed-tube
  :ensure t
  :defer t
  :hook
  (elfeed-search-update . elfeed-tube-setup)
  :config
  ;; (setq elfeed-tube-auto-save-p nil) ; default value
  ;; (setq elfeed-tube-auto-fetch-p t)  ; default value
  )

(use-package elfeed-tube-mpv
  ;; :defer t
  :ensure t
  :after elfeed-tube)

(use-package emms
  :defer t
  :ensure t
  :config
  (require 'emms-setup)
  (emms-all)
  (emms-default-players)
  (setq-default
   emms-source-playlist-default-format 'm3u
   emms-playlist-mode-center-when-go t
   emms-playlist-default-major-mode 'emms-playlist-mode
   emms-show-format "NP: %s"

   emms-player-list '(emms-player-mpv)
   emms-player-mpv-environment '("PULSE_PROP_media.role=music")
   emms-player-mpv-parameters '("--quiet" "--really-quiet" "--no-video" "--no-audio-display" "--force-window=no" "--vo=null"))
  (setq emms-player-mpv-update-metadata t)

  ;; The tinytag python package is a dependency
  ;; Install it with: python3 -m pip install tinytag
  (setq emms-info-functions '(emms-info-tinytag))

  ;; Load cover images
  (setq emms-browser-covers 'emms-browser-cache-thumbnail-async)

  (defun pad-string (str len)
    "Return a string of length LEN starting with STR, truncating or padding as necessary."
    (let* ((str-len (length str))
           (extra-len (- len str-len)))
      (if (>= extra-len 0)
          (concat str (make-string extra-len ? ))
        (concat (substring str 0 (- len 3)) "..."))))

  (defun my-emms-track-description-function (track)
    "Detailed track listing for TRACK."
    (let ((type (emms-track-get track 'type))
	      (name (emms-track-get track 'name))
	      (artist (emms-track-get track 'info-artist))
	      (album (emms-track-get track 'info-album))
	      (title (emms-track-get track 'info-title))
	      (tracknumber (emms-track-get track 'info-tracknumber))
	      (year (emms-track-get-year track))
	      (timet (emms-track-get track 'info-playing-time)))
	  (cond ((eq type 'file)
	         ;; If it has a minimum of metadata
	         (if (and artist title)
		         (concat
		          " "
		          (pad-string
		           (if title
			           (if tracknumber
			               (concat "["
				                   (format "%02d" (string-to-number tracknumber))
				                   "] "
				                   title)
			             title)
		             "Unknown Title")
		           33)
		          "  "
		          (pad-string (if timet
				                  (format "%02d:%02d" (/ timet 60) (% timet 60))
				                "")
				              5)
		          "  "
		          (pad-string (or artist "Unknown Artist") 18)
		          "  "
		          (pad-string (if album
				                  (if year
					                  
					                  album)
				                "Unknown Album")
				              25)
		          "  "
		          (pad-string (or year "")
				              4))
		       name))
	        ((eq 'url type)
             (emms-format-url-track-name name))
	        ;; E.g. playlists
	        (t (concat (symbol-name type) ":" name)))))
  
  (setq emms-track-description-function 'my-emms-track-description-function))

(use-package erc-hl-nicks
  :defer t
  :ensure t
  :config
  :after (:all erc))

(use-package exec-path-from-shell
  :defer t
  :ensure t
  :hook
  (after-init . (lambda ()
                  (when (memq window-system '(mac ns x))
                    (exec-path-from-shell-initialize)))))

(use-package eglot
  :if (eq lemacs-lsp-client 'eglot)
  :ensure t
  :defer t
  :hook
  (python-ts-mode . eglot-ensure)
  (js-ts-mode . eglot-ensure)
  (typescript-ts-mode . eglot-ensure)
  (typescriptreact-mode . eglot-ensure)
  (tsx-ts-mode . eglot-ensure)
  (rust-ts-mode . eglot-ensure)
  (css-mode . eglot-ensure)
  (sass-mode . eglot-ensure)
  (web-mode . eglot-ensure)
  (prisma-mode . eglot-ensure)
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  (eglot-sync-connect nil)
  (eglot-connect-timeout nil)
  :config
  (when (eq lemacs-lsp-client 'eglot)

    (fset #'jsonrpc--log-event #'ignore)

    (progn
	    (bind-keys :map eglot-mode-map
				("C-c l a" . eglot-code-actions)
				("C-c l o" . eglot-code-action-organize-imports)
				("C-c l r" . eglot-rename)
				("C-c l f" . eglot-format))))
  
  (cl-delete-duplicates (nconc eglot-server-programs
                          '((((js-mode :language-id "javascript")
                               (js-ts-mode :language-id "javascript")
                               (tsx-ts-mode :language-id "typescriptreact")
                               (typescript-ts-mode :language-id "typescript")
                               (typescript-mode :language-id "typescript"))
                              .
                              ("typescript-language-server" "--stdio"
                                :initializationOptions
                                (:preferences
                                  (:includeInlayEnumMemberValueHints t
                                    :includeInlayFunctionLikeReturnTypeHints t
                                    :includeInlayFunctionParameterTypeHints t
                                    :includeInlayParameterNameHints "all"
                                    :includeInlayParameterNameHintsWhenArgumentMatchesName t
                                    :includeInlayPropertyDeclarationTypeHints t
                                    :includeInlayVariableTypeHints t
                                    :includeInlayVariableTypeHintsWhenTypeMatchesName t
                                    :completeFunctionCalls t))))))
    :test #'(lambda (element _)
              (if (listp (car element))
                (if (listp (caar element))
                  (memq 'js-mode (caar element))
                  (memq 'js-mode (car element)))
                (eq 'js-mode element))))

  (setq-default eglot-workspace-configuration
    '(:completions
       (:completeFunctionCalls t))))

(use-package eldoc
  :defer t
  :ensure t
  :config
  (setq eldoc-echo-area-use-multiline-p nil)
  (setq eldoc-echo-area-display-truncation-message nil)
  
  (if (display-graphic-p)
      (global-set-key (kbd "C-h C-.") #'eldoc-box-help-at-point)
    (global-set-key (kbd "C-h C-.") #'eldoc-doc-buffer)))

(use-package eldoc-box
  :if (window-system)
  :defer t
  :ensure t
  :after (:all eldoc)
  :custom-face
  ;; (eldoc-box-border ((t (:background "#333"))))
  :config
  (setq eldoc-box-frame-parameters
      '((left . -1)
        (top . -1)
        (width  . 0)
        (height  . 0)
        (no-accept-focus . t)
        (no-focus-on-map . t)
        (min-width  . 0)
        (min-height  . 0)
        (internal-border-width . 2)
        (vertical-scroll-bars . nil)
        (horizontal-scroll-bars . nil)
        (right-fringe . 10)
        (left-fringe . 3)
        (menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (line-spacing . 0)
        (unsplittable . t)
        (undecorated . t)
        (visibility . nil)
        (mouse-wheel-frame . nil)
        (no-other-frame . t)
        (cursor-type . nil)
        (inhibit-double-buffering . t)
        (drag-internal-border . t)
        (no-special-glyphs . t)
        (desktop-dont-save . t)
        (tab-bar-lines . 0)
        (tab-bar-lines-keep-state . 1))))

(use-package expand-region
  :defer t
  :ensure t
  :bind
  (("M-1" . my/expand-region-wrapper))
  :config
  ;; This extends expand-region to also expand from treesit nodes
  (add-to-list 'load-path "~/.emacs.d/site-lisp/treesit-er-expansions")
  (when (and (functionp 'treesit-available-p)
			 (treesit-available-p))
	(require 'treesit-er-expansions))

  (defun my/expand-region-wrapper ()
	"Wrapper function for expand-region in Tree-sitter mode."
	(interactive)
	(condition-case nil
		(er/treesit-er-parent-node)
      (error
       (er/expand-region 1)))))

(use-package geiser-guile
  :defer t
  :ensure t
  :config)

(use-package gh-md
  :defer t
  :ensure t
  :config)

(use-package git-timemachine
  :defer t
  :ensure t
  :bind ("M-g t" . git-timemachine-toggle))

(use-package volatile-highlights
  :defer t
  :ensure t
  :hook ((prog-mode text-mode) . volatile-highlights-mode)
  :config)

(use-package kkp
  :if (not window-system)
  :ensure t
  :defer t
  :config
  ;; (setq kkp-alt-modifier 'alt) ;; use this if you want to map the Alt keyboard modifier to Alt in Emacs (and not to Meta)
  (global-kkp-mode +1))

(use-package handlebars-mode
  :defer t
  :ensure t
  :config)

(use-package hl-todo
  :defer t
  :ensure t
  :hook
  (prog-mode . hl-todo-mode)
  :config)

(use-package indent-guide
  :defer t
  :ensure t
  :hook
  (prog-mode . indent-guide-mode)
  :config
  (setq indent-guide-char "¦"))

(use-package ligature
  :if (eq lemacs-ligatures 'on)
  :defer t
  :ensure t
  :hook (after-init . global-ligature-mode)
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://")))

(use-package magit
  :defer t
  :ensure t
  :config
  (setq ediff-custom-diff-options "-u")
  (setq ediff-diff-options "")
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq magit-diff-refine-hunk 'all)
  (setq magit-diff-use-overlays nil)
  (setq magit-ediff-dwim-show-on-hunks t)
  (setq magit-executable "/usr/local/bin/git"))

(use-package magit-stats
  :defer t
  :ensure t
  :config)

(use-package markdown-mode
  :ensure t
  :defer t
  :bind
  (:map markdown-mode-map
        ("C-c C-e" . markdown-do))
  :mode ("README\\.md\\'" . gfm-mode)
  :custom
  (setq markdown-command "multimarkdown"))

(use-package nerd-icons-completion
  :if lemacs-nerd-icons
  :ensure t
  ;; :defer t
  :after (:all nerd-icons marginalia)
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-dired
  ;; :defer t
  :if lemacs-nerd-icons
  :ensure t
  :after (:all nerd-icons dired)
  :config
  (add-hook 'dired-mode-hook #'nerd-icons-dired-mode))

(use-package nerd-icons-ibuffer
  ;; :defer t
  :if lemacs-nerd-icons
  :ensure t
  :after (:any nerd-icons)
  :config
  (add-hook 'ibuffer-mode-hook #'nerd-icons-ibuffer-mode))

(use-package ibuffer-project
  :ensure t
  :config
  (add-hook
   'ibuffer-hook
   (lambda ()
     (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
     (unless (eq ibuffer-sorting-mode 'project-file-relative)
       (ibuffer-do-sort-by-project-file-relative)))))

(use-package org-ros
  :defer t
  :ensure t
  :bind
  (("C-S-p" . org-ros))
  :config)

(use-package olivetti
  :defer t
  :ensure t
  :init
  (defun lemacs/center-visual-fill-on ()
    (interactive)
    (visual-line-mode 1)
    (setq olivetti-body-width 100)
    (olivetti-mode 1))

  (defun lemacs/center-visual-fill-off ()
    (interactive)
    (visual-line-mode 0)
    (kill-local-variable 'olivetti-body-width)
    (olivetti-mode 0))

  (add-hook 'elfeed-show-mode-hook 'lemacs/center-visual-fill-on)
  (add-hook 'gnus-article-mode-hook 'lemacs/center-visual-fill-on))

(use-package package-lint
  :ensure t
  :defer t
  :config)

(use-package proced-narrow
  :ensure t
  :defer t
  :after proced)

(use-package package-lint
  :ensure t
  :defer t
  :config)

(use-package prisma-mode
  :defer t
  :mode "\\.prisma?\\'"
  :load-path "site-lisp/prisma-mode/")

(use-package lsp-prisma
  :defer t
  :after (:any prisma-mode)
  :load-path "site-lisp/prisma-mode/")

(use-package lsp-tailwindcss
  :ensure t
  :defer t
  :config
  (add-to-list 'lsp-language-id-configuration '(".*\\.erb$" . "html"))
  :init
  (setq lsp-tailwindcss-add-on-mode t))

(use-package pulsar
  :defer t
  :ensure t
  :hook
  (after-init . pulsar-global-mode)
  :config
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.025)
  (setq pulsar-iterations 10)
  (setq pulsar-face 'evil-ex-lazy-highlight)

  (add-to-list 'pulsar-pulse-functions 'evil-scroll-down)
  (add-to-list 'pulsar-pulse-functions 'flymake-goto-next-error)
  (add-to-list 'pulsar-pulse-functions 'flymake-goto-prev-error)
  (add-to-list 'pulsar-pulse-functions 'evil-yank)
  (add-to-list 'pulsar-pulse-functions 'evil-yank-line)
  (add-to-list 'pulsar-pulse-functions 'evil-delete)
  (add-to-list 'pulsar-pulse-functions 'evil-delete-line)
  (add-to-list 'pulsar-pulse-functions 'evil-jump-item)
  (add-to-list 'pulsar-pulse-functions 'diff-hl-next-hunk)
  (add-to-list 'pulsar-pulse-functions 'diff-hl-previous-hunk))


(use-package persp-mode
  :defer t
  :ensure t
  :custom
  (persp-auto-save-opt 0)
  (persp-auto-resume-time 0)
  :hook
  (after-init . persp-mode)
  :config
  ;; Makes tab-bar-tabs also restorable via persp-mode
  (add-hook 'persp-before-deactivate-functions
            (defun +workspaces-save-tab-bar-data-h (_)
              (when (get-current-persp)
                (set-persp-parameter
                 'tab-bar-tabs (tab-bar-tabs)))))

  (add-hook 'persp-activated-functions
            (defun +workspaces-load-tab-bar-data-h (_)
              (tab-bar-tabs-set (persp-parameter 'tab-bar-tabs))
              (tab-bar--update-tab-bar-lines t))))

(use-package persp-mode-project-bridge
  :defer t
  :ensure t
  :hook
  (persp-mode-project-bridge-mode . (lambda ()
                                      (if persp-mode-project-bridge-mode
                                          (persp-mode-project-bridge-find-perspectives-for-all-buffers)
                                        (persp-mode-project-bridge-kill-perspectives))))
  (persp-mode . persp-mode-project-bridge-mode))


(use-package pyvenv
  :defer t
  :ensure t
  :after (:any python-ts-mode)
  :config)

(use-package rainbow-delimiters
  :defer t
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode)
  :config)

(use-package sly
  :ensure t
  :defer t
  :init
  ;; 1.) Install sbcl systemwide (brew install sbcl | apt install sbcl)
  ;; 2.) Install the quicklisp package manager
  ;; $ curl -o /tmp/ql.lisp http://beta.quicklisp.org/quicklisp.lisp
  ;; $ sbcl --no-sysinit --no-userinit --load /tmp/ql.lisp \
  ;;        --eval '(quicklisp-quickstart:install :path "~/.quicklisp")' \
  ;;        --eval '(ql:add-to-init-file)' \
  ;;        --quit
  (setq inferior-lisp-program "sbcl"))

(use-package sass-mode
  :defer t
  :ensure t
  :config)

(use-package scss-mode
  :defer t
  :ensure t
  :config)

(use-package smartparens
  :defer t
  :ensure t
  :hook
  (prog-mode . smartparens-mode)
  :config)

(use-package typescript-mode
  :defer t
  :ensure t
  :config)

(use-package transpose-frame
  :defer t
  :ensure t
  :bind
  (("C-x 4 t" . transpose-frame)
   ("C-x 4 r" . rotate-frame-clockwise))
  :config)

(use-package transmission
  :defer t
  :ensure t
  :config)

(use-package treesit-auto
  :ensure t
  :defer t
  :custom
  (treesit-auto-install 'prompt)
  :hook
  (after-init . global-treesit-auto-mode)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all))

(use-package treemacs
  :defer t
  :ensure t
  :bind
  (("M-i" . treemacs))
  :config
  (setq treemacs-show-hidden-files t)
  ;; (setq treemacs-resize-icons 44)
  (setq treemacs-no-png-images nil)
  (setq treemacs-width 40)
  (setq treemacs-filewatch-mode t)
  (setq treemacs-icons nil)
  (unless lemacs-nerd-icons
    (setq treemacs-no-png-images 't))
  (setq treemacs-file-event-delay 100)
  (setq treemacs-silent-refresh t)
  (setq treemacs--project-follow-delay 0.05)
  (treemacs-project-follow-mode +1))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-icons-dired
  :defer t
  :ensure t
  :config)

(use-package treemacs-magit
  :defer t
  :ensure t
  :after (:all treemacs)
  :config)

(use-package treemacs-nerd-icons
  :if lemacs-nerd-icons
  :ensure t
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package undo-tree
  :defer t
  :ensure t
  :hook
  (after-init . global-undo-tree-mode)
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/.cache/undo"))))

(use-package vc-msg
  :defer t
  :ensure t
  :bind
  (("M-2" . 'vc-msg-show))
  :config
  (setq-default pos-tip-background-color "#2A2A39")
  (setq-default pos-tip-foreground-color "#FFFFEF")
  (setq vc-msg-show-at-line-beginning-p nil))

(use-package which-key
  :defer t
  :ensure t
  :hook
  (after-init . which-key-mode)
  :config)

(use-package xclip
  :defer t
  :ensure t
  :config
  (when (eq system-type 'gnu/linux)
    (xclip-mode 1)))

(use-package xterm-color
  :defer t
  :ensure t
  :config)

(use-package yaml-mode
  :defer t
  :ensure t
  :mode
  ("\\.yaml\\'" "\\.yml\\'")
  :custom-face
  (font-lock-variable-name-face ((t (:foreground "#cba6f7"))))
  :config)

(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'left)
  :init
  (marginalia-mode))

(use-package vertico
  :ensure t
  :hook
  (after-init . vertico-mode)
  :custom
  (vertico-count 10)                    ; Number of candidates to display
  (vertico-resize nil)
  (vertico-cycle nil)                   ; Go from last to first candidate and first to last (cycle)?
  :config
  ;; Prefix the current candidate with “» ”. From
  ;; https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
  (advice-add #'vertico--format-candidate :around
    (lambda (orig cand prefix suffix index _start)
      (setq cand (funcall orig cand prefix suffix index _start))
      (concat
        (if (= vertico--index index)
          (propertize "» " 'face '(:foreground "#80adf0" :weight bold))
          "  ")
        cand))))

(use-package orderless
  :ensure t
  :defer t
  :after vertico
  :init
   (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package cape
  :ensure t
  :config
  (defun lemacs/eglot-capf ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'eglot-completion-at-point
                       #'tempel-expand
                       #'cape-file))))
  (add-hook 'eglot-managed-mode-hook #'lemacs/eglot-capf))

(use-package corfu
  :if (eq lemacs-in-buffer-completion 'corfu)
  :defer t
  :ensure t
  :custom-face
  ;; (corfu-border ((t (:background  "#333"))))
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                    ;; Enable auto completion
  (corfu-auto-delay 0)
  (corfu-auto-prefix 3)
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-quit-no-match t)
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-scroll-margin 5)           ;; Use scroll margin
  (corfu-max-width 50)
  (corfu-min-width 50)
  (corfu-popupinfo-delay 0)
  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.

  :config
  (when lemacs-nerd-icons
	(add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

  (setq corfu--frame-parameters
        '((no-accept-focus . t)
          (no-focus-on-map . t)
          (min-width . t)
          (min-height . t)
          (border-width . 0)
          (outer-border-width . 0)
          (internal-border-width . 1)
          (child-frame-border-width . 2)
          (left-fringe . 0)
          (right-fringe . 0)
          (vertical-scroll-bars)
          (horizontal-scroll-bars)
          (menu-bar-lines . 0)
          (tool-bar-lines . 0)
          (tab-bar-lines . 0)
          (no-other-frame . t)
          (unsplittable . t)
          (undecorated . t)
          (cursor-type)
          (no-special-glyphs . t)
          (desktop-dont-save . t)))
  

  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode t)

  (when (not window-system)
	(add-to-list 'load-path "~/.emacs.d/site-lisp/corfu-terminal/")
	(require 'corfu-terminal)
	(corfu-terminal-mode)))

(use-package nerd-icons-corfu
  :if lemacs-nerd-icons
	:ensure t
  :defer t
	:after (:all corfu)
	:config)

(use-package consult
  :ensure t
  :defer t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
		     ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.

  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )

(use-package embark
  :ensure t
  :defer t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)  ;; alternative for `describe-bindings'
   ("C-h K" . embark-export))   ;; export candidates buffer
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  (defun embark-which-key-indicator ()
	"An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
	(lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
		(which-key--show-keymap
		 (if (eq (plist-get (car targets) :type) 'embark-become)
			 "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
		 (if prefix
			 (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
		 nil nil t (lambda (binding)
					 (not (string-suffix-p "-argument" (cdr binding))))))))

  (setq embark-indicators
		'(embark-which-key-indicator
		  embark-highlight-indicator
		  embark-isearch-highlight-indicator))

  (defun embark-hide-which-key-indicator (fn &rest args)
	"Hide the which-key indicator immediately when using the completing-read prompter."
	(which-key--hide-popup-ignore-command)
	(let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep
  :defer t
  :ensure t
  :config
  (wgrep-enable-key "e")
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))


(use-package flymake-eslint
  :ensure t
  :config
  ;; If Emacs is compiled with JSON support
  (setq flymake-eslint-prefer-json-diagnostics t)
    
  (defun lemacs/use-local-eslint ()
    "Set project's `node_modules' binary eslint as first priority.
If nothing is found, keep the default value flymake-eslint set or
your override of `flymake-eslint-executable-name.'"
    (interactive)
    (let* ((root (locate-dominating-file (buffer-file-name) "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/.bin/eslint"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flymake-eslint-executable-name eslint)
        (message (format "Found local ESLINT! Setting: %s" eslint))
        (flymake-eslint-enable))))


  (defun lemacs/configure-eslint-with-flymake ()
	(when (or (eq major-mode 'tsx-ts-mode)
			  (eq major-mode 'typescript-ts-mode)
			  (eq major-mode 'typescriptreact-mode))
      (lemacs/use-local-eslint)))

  (add-hook 'eglot-managed-mode-hook #'lemacs/use-local-eslint)
  (add-hook 'lsp-mode-hook #'lemacs/use-local-eslint)

  ;; With older projects without LSP or if eglot fails
  ;; you can call interactivelly M-x lemacs/use-local-eslint RET
  ;; or add a hook like:
  (add-hook 'js-ts-mode-hook #'lemacs/use-local-eslint))


(use-package flymake
  :defer t
  :ensure t
  :hook
  (prog-mode . flymake-mode)
  :config
  (set-window-margins nil 2 2)
  (set-window-fringes nil 0 0)

  (bind-keys :map flymake-mode-map
			 ;; ("C-c ! l" . flymake-show-buffer-diagnostics)
			 ("C-c ! l" . consult-flymake)
			 ("C-c ! P" . flymake-show-project-diagnostics)
			 ("C-c ! n" . flymake-goto-next-error)
			 ("C-c ! p" . flymake-goto-prev-error)
             ("M-7" . flymake-goto-prev-error)
             ("M-8" . flymake-goto-next-error)))

(use-package flymake-popon
  :ensure t
  :defer t
  :config
  (custom-set-faces
   '(flymake-popon ((t (:inherit default :background "#2a2a39"))))
   '(flymake-popon-posframe-border ((t nil))))
  :hook
  (flymake-mode . global-flymake-popon-mode))

;; This is ugly but the only way I managed to make it work, manual hooks didn't do the trick :/
(when (eq lemacs-lsp-client 'lsp-mode)
  (use-package lsp-mode
	:if (eq lemacs-lsp-client 'lsp-mode)
	:defer t
    :hook ((lsp-mode . lsp-diagnostics-mode)
           (lsp-mode . lsp-enable-which-key-integration)
           ((tsx-ts-mode
             typescript-ts-mode
             css-mode
             rust-ts-mode
             python-ts
             web-mode
             prisma-mode
             js-ts-mode) . lsp))
	:ensure t
    :custom
	(lsp-keymap-prefix "C-c l")
    (lsp-inlay-hint-enable t)
    (lsp-completion-provider :none)
    (lsp-session-file (locate-user-emacs-file ".lsp-session"))
    (lsp-log-io nil) ;; for speed
    (lsp-idle-delay 0) ;; debouncing, if needed 0.5
    (lsp-keep-workspace-alive nil)
    ;; core
    (lsp-enable-xref t)
    (lsp-auto-configure t)
    (lsp-enable-links nil)
    (lsp-eldoc-enable-hover t)
    (lsp-enable-dap-auto-configure t)
    (lsp-enable-file-watchers nil)
    (lsp-enable-folding nil)
    (lsp-enable-imenu t)
    (lsp-enable-indentation nil)
    (lsp-enable-on-type-formatting nil)
    (lsp-enable-suggest-server-download t)
    (lsp-enable-symbol-highlighting t)
    (lsp-enable-text-document-color nil)
    ;; modeline
    (lsp-modeline-code-actions-enable nil) ; Modeline should be relatively clean
    (lsp-modeline-diagnostics-enable nil)  ; Already supported through `flycheck'/ `flymake'
    (lsp-modeline-workspace-status-enable nil) ; Modeline displays "LSP" when lsp-mode is enabled
    (lsp-signature-doc-lines 1)                ; Don't raise the echo area. It's distracting
    (lsp-eldoc-render-all t)
    ;; completion
    (lsp-completion-enable t)
    (lsp-completion-enable-additional-text-edit t) ; Ex: auto-insert an import for a completion candidate
    (lsp-enable-snippet nil)                         ; Important to provide full JSX completion
    (lsp-completion-show-kind t)                   ; Optional
    ;; lens
    (lsp-lens-enable t)
    ;; headerline
    (lsp-headerline-breadcrumb-enable-symbol-numbers t)
    (lsp-headerline-arrow "▶")
    (lsp-headerline-breadcrumb-enable-diagnostics nil)
    (lsp-headerline-breadcrumb-icons-enable nil)
    ;; semantic
    (lsp-semantic-tokens-enable nil)
    
    :init
    (setq lsp-use-plists t)
	;; (lsp-inlay-hints-mode)
    ))

(use-package tempel
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")

  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))

  :init
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
  )

(use-package tempel-collection
  :defer t
  :ensure t
  :after tempel)

(use-package polymode
  :if (eq lemacs-polymode 'on)
  :ensure t
  :defer t
  :config
  ;; React.JS styled-components "integration"
  (define-hostmode poly-typescript-hostmode nil
    "Typescript hostmode."
    :mode 'typescript-ts-mode)
  (define-innermode poly-typescript-cssinjs-innermode nil
    :mode 'css-mode
    :head-matcher "\\(styled\\|css\\|\\.attrs<[^>]+>\\([^)]+\\)\\)?[.()<>[:alnum:]]?+`"
    :tail-matcher "\`"
    :head-mode 'host
    :tail-mode 'host)
  (define-polymode poly-typescript-mode
    :hostmode 'poly-typescript-hostmode
    :innermodes '(poly-typescript-cssinjs-innermode))

  ;; I do not want this to proliferate to all  .[j|t]sx? files, so
  ;; I am limiting it to the styled? filenames
  (add-to-list 'auto-mode-alist '("\\(styled\\|style[sd]\\).[tj]sx?\\'" . poly-typescript-mode)))

(use-package verb
  :ensure t
  :defer t)

(use-package yeetube
  :ensure t
  :defer t
  :init (define-prefix-command 'my/yeetube-map)
  :config
  (setf yeetube-mpv-disable-video t) ;; Disable video output
  :bind (("C-c y" . 'my/yeetube-map)
          :map my/yeetube-map
		  ("s" . 'yeetube-search)
		  ("b" . 'yeetube-play-saved-video)
		  ("d" . 'yeetube-download-videos)
		  ("p" . 'yeetube-mpv-toggle-pause)
		  ("v" . 'yeetube-mpv-toggle-video)
		  ("V" . 'yeetube-mpv-toggle-no-video-flag)
		  ("k" . 'yeetube-remove-saved-video)))


;;; --------------------------------- PIM
(use-package vdirel
  :ensure t
  :defer t)

(use-package khalel
  :ensure t
  :after org
  :config
  ;; (setq khalel-khal-command "~/.local/bin/khal")
  (setq org-agenda-files  (list (concat org-directory "/" "calendar.org")))
  (setq khalel-import-org-file (concat org-directory "/" "calendar.org"))
  (setq khalel-vdirsyncer-command "vdirsyncer")
  (setq khalel-capture-key "e")
  (setq khalel-import-org-file-confirm-overwrite nil)
  (setq khalel-import-end-date "+30d")

  (defun lemacs/calendar-sync ()
    (interactive)
    (khalel-run-vdirsyncer)
    (khalel-import-events))

  (khalel-add-capture-template))

;;; --------------------------------- AI Assistant
(use-package codeium
  :if (not (eq lemacs-codeium-scope 'nil))
  :load-path "site-lisp/codeium/"
  :config
  ;; First time loading this package, you need to set up your API key:
  ;; (setq codeium/metadata/api_key "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")
  ;;
  ;; You can do this from within Emacs by running: M-x codeium-install
  (when lemacs-codeium-scope
    (pcase lemacs-codeium-scope
      ('everywhere (add-to-list 'completion-at-point-functions #'codeium-completion-at-point))
      ('prog-mode  (add-hook 'prog-mode-hook
                             (lambda ()
                               (require 'cape)
                               (setq-local completion-at-point-functions
                                           (list (cape-capf-super #'codeium-completion-at-point #'lsp-completion-at-point)))))))
    (codeium-init)))

(use-package ellama
  :defer t
  :ensure t
  :init
  (setopt ellama-language "English")
  (require 'llm-ollama)
  (setopt ellama-provider
          (make-llm-ollama
            :chat-model "codellama" :embedding-model "codellama")))


;;; --------------------------------- THEMES
(use-package catppuccin-theme
  :if (eq lemacs-default-theme 'catppuccin)
  :defer t
  :ensure t
  :config
  ;; NOTE reloading catppuccin "undoes" what early init does to screen NOT to flash on GUI boot
  (defun lemacs/catppuccin-hack (_)
    "A catppuccin hack to make sure everything is loaded"
    (catppuccin-reload))
  (add-hook 'after-init-hook (lambda ()
                               (run-with-timer 0.3 nil
                                               (lambda ()(lemacs/catppuccin-hack nil)))))
  (add-hook 'after-make-frame-functions 'lemacs/catppuccin-hack)
  
  ;; Custom diff-hl colors
  (custom-set-faces
   `(diff-hl-change ((t (:background unspecified :foreground ,(catppuccin-get-color 'blue))))))
  (custom-set-faces
   `(diff-hl-delete ((t (:background unspecified :foreground ,(catppuccin-get-color 'red))))))
  (custom-set-faces
   `(diff-hl-insert ((t (:background unspecified :foreground ,(catppuccin-get-color 'green))))))

  ;; Custom vhl/default-face
  (custom-set-faces `(vhl/default-face ((t (:background ,(catppuccin-get-color 'surface2)))))))


(use-package modus-themes
  :if (eq lemacs-default-theme 'modus)
  :ensure t
  :defer t
  :init
  (load-theme 'modus-vivendi t)
  :config

  ;; Regular modus options
  (setq modus-themes-italic-constructs t
	    modus-themes-bold-constructs t
	    modus-themes-mixed-fonts t
        modus-themes-prompts '(bold intense))

  (customize-set-variable
   'modus-themes-common-palette-overrides
   `(
     ;; Make the mode-line borderless and stand out less
     (bg-mode-line-active bg-main)
     (fg-mode-line-active fg-main)
     (bg-mode-line-inactive bg-main)
     (fg-mode-line-inactive fg-dim)
     (border-mode-line-active bg-transparent)
     (border-mode-line-inactive bg-transparent)))

  (modus-themes-with-colors
    (custom-set-faces
     ;; Treemacs faces with Modus color variables
     `(treemacs-async-loading-face ((,c :foreground ,fg-main)))
     `(treemacs-directory-face ((,c :foreground ,blue)))
     `(treemacs-directory-collapsed-face ((,c :foreground ,blue)))
     `(treemacs-file-face ((,c :foreground ,fg-main)))
     `(treemacs-fringe-indicator-face ((,c :foreground ,fg-main)))
     `(treemacs-git-added-face ((,c :inherit success)))
     `(treemacs-git-commit-diff-face ((,c :foreground ,green)))
     `(treemacs-git-conflict-face ((,c :inherit error)))
     `(treemacs-git-ignored-face ((,c :inherit shadow)))
     `(treemacs-git-modified-face ((,c :inherit warning)))
     `(treemacs-git-renamed-face ((,c :inherit italic)))
     `(treemacs-git-unmodified-face ((,c :foreground ,fg-main)))
     `(treemacs-git-untracked-face ((,c :inherit shadow)))
     `(treemacs-header-button-face ((,c :foreground ,fg-main)))
     `(treemacs-help-column-face ((,c :foreground ,blue)))
     `(treemacs-help-title-face ((,c :foreground ,fg-main)))
     `(treemacs-hl-line-face ((,c :background ,bg-hl-line :extend t)))
     `(treemacs-marked-file-face ((,c :foreground ,red)))
     `(treemacs-nerd-icons-face ((,c :foreground ,blue)))
     `(treemacs-on-failure-pulse-face ((,c :foreground ,fg-main)))
     `(treemacs-on-success-pulse-face ((,c :foreground ,fg-main)))
     `(treemacs-peek-mode-indicator-face ((,c :foreground ,fg-main)))
     `(treemacs-remote-face ((,c :foreground ,fg-main)))
     `(treemacs-root-face ((,c :foreground ,blue :background ,bg-main)))
     `(treemacs-root-remote-disconnected-face ((,c :foreground ,yellow)))
     `(treemacs-root-remote-unreadable-face ((,c :foreground ,yellow)))
     `(treemacs-root-unreadable-face ((,c :foreground ,red)))
     `(treemacs-tags-face ((,c :foreground ,fg-main)))
     `(treemacs-term-node-face ((,c :foreground ,blue)))
     `(treemacs-window-background-face ((,c :background ,bg-main)))
     `(treemacs-nerd-icons-root-face ((,c :foreground ,blue)))
     `(treemacs-nerd-icons-file-face ((,c :foreground ,blue)))

     ;; Custom diff-hl colors
     `(diff-hl-change ((,c :foreground ,blue :background unspecified)))
     `(diff-hl-delete ((,c :foreground ,red :background unspecified)))
     `(diff-hl-insert ((,c :foreground ,green :background unspecified)))

     ;; Other faces
     `(fringe ((,c
                :background ,bg-main
                :box nil)))
     `(line-number ((,c
                     :background ,bg-main
                     :box nil)))
     `(line-number-current-line ((,c
                                  :background ,bg-main
                                  :box nil)))
     `(tab-bar ((,c
                 ;; :height 0.8
                 :background ,bg-main
                 :box nil)))
     `(tab-bar-tab ((,c
                     :background ,bg-main
                     :underline (:color ,blue-intense :style line)
                     :box (:line-width 2 :style flat-button))))
     `(tab-bar-tab-inactive ((,c
                              :background ,bg-main
                              :box (:line-width 2 :style flat-button)))))))



;;; -------------------------------- INIT/PROVIDE THIS CONFIG
(provide 'init)
;;; init.el ends here
