;; init.el --- LEmacs (Lionyx Emacs)
;; Author: Rahul M. Juliato <rahul.juliato@gmail.com>
;; URL: https://github.com/LionyxML/lemacs
;; Keywords: config, emacs, init
;; Version: 0.1.46
;; Package-Requires: ((emacs "29"))

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
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(add-to-list 'load-path "~/.emacs.d/lisp")

(package-initialize)

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

  (defun extract-use-package-packages ()
    (let ((packages '())
          (init-file (expand-file-name "init.el" user-emacs-directory)))
      (with-temp-buffer
        (insert-file-contents init-file)
        (goto-char (point-min))
        (while (re-search-forward "^\\(\\s-*\\)(use-package\\s-+\\(\\(\\sw\\|\\s_\\)+\\)" nil t)
          (push (intern (match-string 2)) packages)))
      (reverse packages)))

  (defun install-use-package-packages (packages-builtin packages-external)
    (interactive)
    (let ((to-install (cl-set-difference packages-external packages-builtin)))
      (dolist (pkg to-install)
        (unless (package-installed-p pkg)
          (package-install pkg)))))

  (condition-case err
      (let ((lemacs--emacs-builtin-packages '(dired window eshell erc eldoc emacs isearch prisma-mode lsp-prisma))
            (lemacs--install-packages (extract-use-package-packages)))

        (switch-to-buffer "*Messages*")
        (message (format "%s" lemacs-art))
        (message "\n\n\nInstalling LEmacs...\n\n\n\n")

        (package-refresh-contents)
        (install-use-package-packages lemacs--emacs-builtin-packages lemacs--install-packages)

        (require 'tree-sitter)
        (require 'nerd-icons)

        (call-interactively 'tree-sitter-langs-install-grammars)
        (call-interactively 'nerd-icons-install-fonts)
        (kill-emacs))

    (error
     (message "LEmacs failed to install, run 'emacs -nw --debug-init'"))))

;;; --------------------------------- LEMACS CUSTOM OPTIONS
(defcustom lemacs-lsp-client 'eglot
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
           (const :tag "company" company)
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

(defcustom lemacs-docker-executable 'docker
  "The executable to be used with docker-mode."
  :type '(choice
           (const :tag "docker" docker)
           (const :tag "podman" podman))
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
;;; --------------------------------- EMACS
(use-package emacs
  :custom
  (auto-save-default nil)
  (column-number-mode t)
  (create-lockfiles nil)
  (delete-by-moving-to-trash t)
  (enable-recursive-minibuffers t)
  (frame-inhibit-implied-resize t)
  (frame-resize-pixelwise t)
  (gnus-init-file "~/.gnus.el")
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
  (pixel-scroll-precision-mode t)
  (pixel-scroll-precision-use-momentum nil)
  (ring-bell-function 'ignore)
  (shr-use-colors nil)
  (split-width-threshold 300)
  (switch-to-buffer-obey-display-actions t)
  (tab-always-indent 'complete) ;; TAB serves as M-TAB to completion
  (tab-width 4)
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  (treesit-font-lock-level 4)
  (truncate-lines t)
  (tsx-ts-mode-indent-offset 4)
  (typescript-ts-mode-indent-offset 4)
  (use-dialog-box nil)
  (use-file-dialog nil)
  (use-short-answers t)
  (warning-minimum-level :emergency)
  (window-combination-resize t)
  (xterm-mouse-mode t)
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

  ;; Set exec path from shell
  (defun lemacs/set-exec-path-from-shell-PATH ()
    "Set up Emacs' `exec-path' and PATH environment the same as user Shell."
    (interactive)
    (let ((path-from-shell (replace-regexp-in-string
                             "[ \t\n]*$" "" (shell-command-to-string
                                              "$SHELL --login -c 'echo $PATH'"
                                              ))))
      (setenv "PATH" path-from-shell)
      (setq exec-path (split-string path-from-shell path-separator))))
  (lemacs/set-exec-path-from-shell-PATH)

  (defun lemacs/rename-buffer-and-move-to-new-window ()
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


  ;; Starts elisp with outline collapsed
  (defun lemacs/elisp-mode-hook ()
    (interactive)
    (outline-minor-mode 1)
    (outline-hide-sublevels 1))
  (add-hook 'emacs-lisp-mode-hook #'lemacs/elisp-mode-hook)


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
          (dotimes (i (abs (or n 1)))               ;Insert N times, or once if not specified
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
  :bind
  (("M-D" . 'my-duplicate-line-or-region)
   ("C-x C-b" . 'ibuffer))
  :init
  (load-theme 'catppuccin :no-confirm)

  ;; Emacs frame starts focused
  (select-frame-set-input-focus (selected-frame))

  ;; Emacs frame starts maximized
  (toggle-frame-maximized)

  (winner-mode 1)
  (global-auto-revert-mode 1)
  (indent-tabs-mode -1)
  (recentf-mode 1)
  (savehist-mode 1)
  (save-place-mode 1)
  (desktop-save-mode 1)
  (when (eq system-type 'gnu/linux)
    (xclip-mode 1))
  (file-name-shadow-mode 1)
  (delete-selection-mode 1)
  (diff-hl-flydiff-mode 1))


;;; --------------------------------- DIRED
(use-package dired
  :ensure nil
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
  :config
  (setq isearch-lazy-count t)
  (setq lazy-count-prefix-format "(%s/%s) ")
  (setq lazy-count-suffix-format nil)
  (setq search-whitespace-regexp ".*?"))

;;; --------------------------------- ERC
(use-package erc
  :ensure nil
  :defer t
  :custom

  (defun lemacs-erc-enable-flyspell ()
    "Enable Flyspell mode in ERC buffers."
    (flyspell-mode 1))

  (add-hook 'erc-join-hook 'lemacs-erc-enable-flyspell)

  (erc-join-buffer 'window)
  ;; (erc-interactive-display ...) ;; this option will be available on next ERC release (5.6)
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-timestamp-format "[%H:%M]")
  (erc-autojoin-channels-alist '((".*\\.libera\\.chat" "#emacs" "#systemcrafters"))))

;;; --------------------------------- ESHELL
(use-package eshell
  :config
  (setq eshell-hist-ignoredups t)
  (setq eshell-cmpl-cycle-completions nil)
  (setq eshell-cmpl-ignore-case t)
  (setq eshell-ask-to-save-history (quote always))

  (defun lemacs/open-eshell ()
    "Open a new instance of eshell."
    (interactive)
    (eshell 'N))

  (defun lemacs/close-eshell ()
    "Closes the eshell (or any buffer), killing the window."
    (interactive)
    (kill-buffer (current-buffer))
    (delete-window))

  (defun lemacs/split-eshell-vertical ()
    "Split the window vertically and open a new instance of eshell."
    (interactive)
    (split-window-right)
    (other-window 1)
    (lemacs/open-eshell))

  (defun lemacs/split-eshell-horizontal ()
    "Split the window horizontally and open a new instance of eshell."
    (interactive)
    (split-window-below)
    (other-window 1)
    (lemacs/open-eshell))

  (defun lemacs/open-eshell-new-tab ()
    "Open eshell in a new tab."
    (interactive)
    (let ((new-tab (generate-new-buffer-name "*eshell*")))
      (tab-new)
      (eshell)
      (rename-buffer new-tab)))

  (defun lemacs/kill-all-eshell-buffers ()
    "Kill all *eshell* buffers."
    (interactive)
    (let ((eshell-buffers
           (cl-remove-if-not
            (lambda (buffer)
              (string-prefix-p "*eshell*" (buffer-name buffer)))
            (buffer-list))))

      (if eshell-buffers
          (progn
            (message "Killing *eshell* buffers:")
            (dolist (buffer eshell-buffers)
              (message "  %s" (buffer-name buffer))
              (kill-buffer buffer)))
        (message "No *eshell* buffers to kill."))))

  (global-set-key (kbd "C-c e e") 'lemacs/open-eshell)
  (global-set-key (kbd "C-c e v") 'lemacs/split-eshell-vertical)
  (global-set-key (kbd "C-c e \\") 'lemacs/split-eshell-vertical)
  (global-set-key (kbd "C-c e |") 'lemacs/split-eshell-vertical)

  (global-set-key (kbd "C-c e h") 'lemacs/split-eshell-horizontal)
  (global-set-key (kbd "C-c e -") 'lemacs/split-eshell-horizontal)

  (global-set-key (kbd "C-c e k") 'lemacs/kill-all-eshell-buffers)
  (global-set-key (kbd "C-c e t") 'lemacs/open-eshell-new-tab)

  (global-set-key (kbd "C-c e x") 'lemacs/close-eshell)

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
              ")\n"
              ))
           "└─➜ ")))

  (setq eshell-prompt-regexp "└─➜ ")

  (add-hook 'eshell-mode-hook (lambda () (setenv "TERM" "xterm-256color")))
  (add-hook 'eshell-mode-hook (lambda () (company-mode -1)) 'append)

  (setq eshell-visual-commands
		'("vi" "screen" "top"  "htop" "btm" "less" "more" "lynx" "ncftp" "pine" "tin" "trn"
		  "elm" "irssi" "nmtui-connect" "nethack" "vim" "alsamixer" "nvim" "w3m"
		   "ncmpcpp" "newsbeuter" "nethack" "mutt" "yarn" "pnpm" "sudo")))

;;; --------------------------------- VC
(use-package vc
  :ensure t
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

;;; --------------------------------- WINDOW
(use-package window
  :ensure nil
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
  :init
  ;; This aims to substitute tmux (or gnu/screen) with Emacs
  ;; Tabs are our tmux windows (new one with C-x t 2)
  ;; Windows are emacs windows (new one with C-x 5 2)

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

  (global-set-key (kbd "M-l") 'lemacs/switch-tab-or-tab-bar))


;;; --------------------------------- EXTERNAL PACKAGES
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
  ("M-o" . ace-window))

(use-package add-node-modules-path
  :defer t
  :ensure t
  :config)

(use-package catppuccin-theme
  :defer t
  :ensure t
  :config

  (defun lemacs/catppuccin-hack (_)
    "A catppuccin hack to make sure everything is loaded"
    (catppuccin-reload)
    (set-face-attribute 'diff-hl-change nil :background "#89b4fa")
    (set-face-attribute 'diff-hl-delete nil :background "#f38ba8")
    (set-face-attribute 'diff-hl-insert nil :background "#a6e3a1"))


  ;; Run hack on Terminal mode on loading
  (when (not window-system)
    (add-hook 'after-init-hook (lambda ()
                                 (run-with-timer 0.3 nil
                                   (lambda ()(lemacs/catppuccin-hack nil))))))

  ;; Run hack after a new frame is open
  (add-hook 'after-make-frame-functions 'lemacs/catppuccin-hack))

(use-package breadcrumb
  :defer t
  :ensure t
  :hook
  (eglot-connect . breadcrumb-mode)
  :config)

(use-package dashboard
  :ensure t
  :config
  (setq dashboard-banner-logo-title "Welcome to LEMACS")
  ;; (setq dashboard-startup-banner (".....logo.png" . ".....logo.txt"))
  ;; (setq dashboard-startup-banner 'logo)
  ;; (setq dashboard-startup-banner
  ;;   (expand-file-name "assets/lemacs_logo.txt" user-emacs-directory))
  (setq dashboard-startup-banner
		(cons (expand-file-name "assets/lemacs_logo.png" user-emacs-directory)
      (expand-file-name "assets/lemacs_logo.txt" user-emacs-directory)))

  (setq dashboard-center-content t)
  (setq dashboard-vertically-center-content nil)
  (setq dashboard-display-icons-p t)
  (setq dashboard-icon-type 'nerd-icons)

  (setq dashboard-items '(
                        ;; (recents   . 5)
                        ;; (bookmarks . 5)
                        ;; (projects  . 5)
                        ;; (agenda    . 5)
                        ;; (registers . 5)
						))

  (dashboard-setup-startup-hook))

(use-package diff-hl
  :defer t
  :ensure t
  :hook
  (after-init . global-diff-hl-mode)
  :custom
  (diff-hl-margin-mode t)
  (diff-hl-side 'left)
  (diff-hl-margin-symbols-alist
   '((insert . " ")
	 (delete . " ")
	 (change . " ")
	 (unknown . " ")
	 (ignored . " ")))
  :bind
  (("M-9" . 'diff-hl-previous-hunk)
   ("M-0" . 'diff-hl-next-hunk))
  :config
  (set-face-attribute 'diff-hl-change nil :background "#89b4fa")
  (set-face-attribute 'diff-hl-delete nil :background "#f38ba8")
  (set-face-attribute 'diff-hl-insert nil :background "#a6e3a1"))

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
  (doom-modeline-icon t)
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
  (setq eat-term-name "xterm-256color"))

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
  (setq emms-info-functions '(emms-info-tinytag)) ;; When using Tinytag
  ;; Load cover images
  (setq emms-browser-covers 'emms-browser-cache-thumbnail-async))

(use-package erc-hl-nicks
  :defer t
  :ensure t
  :config
  :after (:all erc))

(use-package eglot
  :if (eq lemacs-lsp-client 'eglot)
  :ensure t
  :preface
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  :config
  (when (eq lemacs-lsp-client 'eglot)
    (progn
      (add-hook 'python-ts-mode-hook #'eglot-ensure)
      (add-hook 'js-ts-mode-hook #'eglot-ensure)
      (add-hook 'typescript-ts-mode-hook #'eglot-ensure)
      (add-hook 'typescriptreact-mode-hook #'eglot-ensure)
      (add-hook 'tsx-ts-mode-hook #'eglot-ensure)
      (add-hook 'rust-ts-mode-hook #'eglot-ensure)
      (add-hook 'css-mode-hook #'eglot-ensure)
      (add-hook 'sass-mode-hook #'eglot-ensure)
      (add-hook 'web-mode-hook #'eglot-ensure)
      (add-hook 'prisma-mode-hook #'eglot-ensure)

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
       (:completeFunctionCalls t)))
  
  
  (defun my-enable-flymake-eslint ()
	"Enable eslint if typescript mode"
	(when (or (eq major-mode 'tsx-ts-mode)
			  (eq major-mode 'typescript-ts-mode)
			  (eq major-mode 'js-ts-mode)
			  (eq major-mode 'typescriptreact-mode))
	  (flymake-eslint-enable)))

  (add-hook 'eglot-managed-mode-hook #'my-enable-flymake-eslint))

(use-package eldoc
  :defer t
  :ensure t
  :config
  (if (display-graphic-p)
    (global-set-key (kbd "C-h C-.") #'eldoc-box-help-at-point)
  (global-set-key (kbd "C-h C-.") #'eldoc-doc-buffer))

  (setq eldoc-echo-area-use-multiline-p nil))

(use-package eldoc-box
  :if (window-system)
  :defer t
  :ensure t
  :after (:all eldoc)
  :config)

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

(use-package git-timemachine
  :defer t
  :ensure t
  :bind ("M-g t" . git-timemachine-toggle))

(use-package gh-md
  :defer t
  :ensure t
  :config)

(use-package kkp
  :ensure t
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
  (setq indent-guide-char "│"))

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
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-dired
  ;; :defer t
  :ensure t
  :after (:all nerd-icons dired)
  :config
  (add-hook 'dired-mode-hook #'nerd-icons-dired-mode))

(use-package nerd-icons-ibuffer
  ;; :defer t
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

(use-package package-lint
  :ensure t
  :defer t
  :config)

(use-package prettier
  :ensure t
  :defer t
  :hook
  (prog-mode . prettier-mode)
  :config)

(use-package prisma-mode
  :defer t
  :mode "\\.prisma?\\'"
  :load-path "site-lisp/prisma-mode/")

(use-package lsp-prisma
  :defer t
  :after (:any prisma-mode)
  :load-path "site-lisp/prisma-mode/")

(use-package python-black
  :defer t
  :ensure t
  :config
  (python-black-extra-args '("--line-length" "79")))

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

(use-package restclient
  :defer t
  :ensure t
  :config)

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

;; NOTE:
;;   Tree-sitter is still a bit messy on Emacs.
;;   We basically still install the 3rd party package just to have the nicer
;;  more colorful sintax higlighting.
;;   Problem is when grepping or peeking a file, such as with lsp peek or
;;  consult-grep, sintax higlighting is not loaded by default. For that we use
;;  the internal treesit mode, configured via treesit-auto.
;;   If we had nicier syntax highlighting (.tsx is the benchmark here), we could
;;  drop tree-sitter and tree-sitter-langs. But still, not the time to do so.
(use-package treesit-auto
  :ensure t
  :defer t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package tree-sitter
  :ensure t
  :defer t
  :hook
  (after-init . global-tree-sitter-mode)
  :config

  (setq tree-sitter-load-path (list (expand-file-name "tree-sitter" user-emacs-directory)))

  (setq major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (js-mode . js-ts-mode)
          (javascript-mode . js-ts-mode)
          (js2-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)))

  ;; we choose this instead of tsx-mode so that eglot / lsp-mode can automatically figure out language for server
  ;; see https://github.com/joaotavora/eglot/issues/624 and https://github.com/joaotavora/eglot#handling-quirky-servers
  (define-derived-mode typescriptreact-mode tsx-ts-mode
    "TypeScript TSX")
  ;; use our derived mode for tsx files
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  ;; by default, typescript-mode is mapped to the treesitter typescript parser
  ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx))

  (add-hook 'tree-sitter-after-on-hook 'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :defer t
  :ensure t
  :config
  (setq-default tree-sitter-langs-grammar-dir (expand-file-name "tree-sitter" user-emacs-directory))

  (defun create-tree-sitter-links ()
	"Create links from .emacs.d/tree-sitter/bin* to .emacs.d/tree-sitter/* files.
Since tree-sitter-mode uses the format provided by /bin and the built-in
uses the files with the prefix libtree-sitter-."
  (interactive)
  (let ((bin-dir (expand-file-name "tree-sitter/bin" user-emacs-directory))
        (lib-dir (expand-file-name "tree-sitter" user-emacs-directory)))
    (dolist (file (directory-files bin-dir nil "\\.so$"))
      (let ((link-name (concat lib-dir "/libtree-sitter-" (file-name-nondirectory file))))
        (unless (file-exists-p link-name)
          (make-symbolic-link (concat bin-dir "/" file) link-name t))))))

  (create-tree-sitter-links))

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
  (setq treemacs-file-event-delay 100)
  (setq treemacs-silent-refresh t)
  (setq treemacs--project-follow-delay 0.05)
  (treemacs-project-follow-mode +1))

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
  (setq-default pos-tip-background-color "#4F4F4F")
  (setq-default pos-tip-foreground-color "#FFFFEF")
  (setq vc-msg-show-at-line-beginning-p nil))

(use-package which-key
  :defer t
  :ensure t
  :hook
  (after-init . which-key-mode)
  :config
  (with-eval-after-load 'lsp-mode
    (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)))

(use-package xclip
  :defer t
  :ensure t
  :config)

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
  :custom
  (vertico-count 10)                    ; Number of candidates to display
  (vertico-resize nil)
  (vertico-cycle nil)                   ; Go from last to first candidate and first to last (cycle)?
  :config
  (vertico-mode)

  ;; Prefix the current candidate with “» ”. From
  ;; https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
  (advice-add #'vertico--format-candidate :around
    (lambda (orig cand prefix suffix index _start)
      (setq cand (funcall orig cand prefix suffix index _start))
      (concat
        (if (= vertico--index index)
          (propertize "» " 'face 'vertico-current)
          "  ")
        cand))))

(use-package orderless
  :ensure t
  :init
   (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package corfu
  :if (eq lemacs-in-buffer-completion 'corfu)
  :defer t
  :ensure t
	;; Optional customizations
	:custom
	;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
	(corfu-auto t)                 ;; Enable auto completion
	(corfu-auto-delay 0)
	(corfu-auto-prefix 3)
	;; (corfu-separator ?\s)          ;; Orderless field separator
	;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
	;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
	(corfu-quit-no-match t)
	;; (corfu-preview-current nil)    ;; Disable current candidate preview
	;; (corfu-preselect 'prompt)      ;; Preselect the prompt
	;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
	(corfu-scroll-margin 5)        ;; Use scroll margin
	(corfu-max-width 50)
	(corfu-popupinfo-mode t)
	(corfu-popupinfo-delay 0)

	;; Enable Corfu only for certain modes.
	;; :hook ((prog-mode . corfu-mode)
	;;        (shell-mode . corfu-mode)
	;;        (eshell-mode . corfu-mode))

	;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
	;; be used globally (M-/).  See also the customization variable
	;; `global-corfu-modes' to exclude certain modes.

	:config
	(add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)

	:init
	(global-corfu-mode))


(when (not window-system)
	(add-to-list 'load-path "~/.emacs.d/site-lisp/corfu-terminal/")
	(require 'corfu-terminal)
	(corfu-terminal-mode))

(use-package nerd-icons-corfu
	:ensure t
  :defer t
	:after (:all corfu)
	:config)


;; NOTE:
;;   We use company-quickhelp + company-quickhelp-terminal on CLI
;;   And company-box on GUI (company quickhelp on GUI is toolkit dependent
;;   and altough it works ok with Emacs Lucid, it does not with GTK and macOS).
(use-package company
  :if (eq lemacs-in-buffer-completion 'company)
  :disabled (not (eq lemacs-in-buffer-completion 'company))
	:defer t
	:ensure t
	:bind
	("C-j" . company-complete)
	:config
	(setq company-tooltip-maximum-width 50)
	(setq company-tooltip-align-annotations t)
	(setq company-minimum-prefix-length 2)
	(setq company-idle-delay 0.1)
  :init
  (global-company-mode))

(use-package company-quickhelp
	:if (and (eq lemacs-in-buffer-completion 'company) (eq window-system nil))
	:defer t
	:ensure t
  :after (:all company)
	:custom
	(company-quickhelp-use-propertized-text nil)
	:config
	(eval-after-load 'company
	  '(define-key company-active-map (kbd "C-h") #'company-quickhelp-manual-begin)))

(use-package company-quickhelp-terminal
	:if (and (eq lemacs-in-buffer-completion 'company) (eq window-system nil))
	:defer t
	:ensure t
  :after (:all company)
	:custom
	(company-quickhelp-use-propertized-text nil)
	:config
	(with-eval-after-load 'company-quickhelp
    (company-quickhelp-terminal-mode 1)))


(use-package company-box
	:if (and (eq lemacs-in-buffer-completion 'company) (window-system))
	:defer t
	:ensure t
  :after (:all company)
	:hook (company-mode . company-box-mode)
	:config
	(setq company-box-scrollbar nil))


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
  :defer t
  :ensure t
  :config)

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
             ("M-8" . flymake-goto-next-error))

  ;; Magic in order to display markings on margin, not the fringe
  ;; Probably when this becomes ready, we wont need it to display
  ;; flymake on the margin:
  ;; https://mail.gnu.org/archive/html/emacs-devel/2024-03/msg00715.html
  (advice-add #'flymake--fringe-overlay-spec :override
	  (lambda (bitmap &optional recursed)
		(set-window-margins nil 2 2)
		(set-window-fringes nil 0 0)

      (if (and (symbolp bitmap)
            (boundp bitmap)
            (not recursed))
        (flymake--fringe-overlay-spec
          (symbol-value bitmap) t)
        (and flymake-fringe-indicator-position
          bitmap
          (propertize "!" 'display
            `((margin left-margin)
               ,bitmap))))))

  (put 'flymake-error 'flymake-bitmap (propertize "»" 'face `(:inherit (error default) :underline nil)))
  (put 'flymake-warning 'flymake-bitmap (propertize "»" 'face `(:inherit (warning default) :underline nil)))
  (put 'flymake-note 'flymake-bitmap (propertize "»" 'face `(:inherit (success default) :underline nil)))
  )

;; This is ugly but the only way I managed to make it work, manual hooks didn't do the trick :/
(when (eq lemacs-lsp-client 'lsp-mode)
  (use-package lsp-mode
	:if (eq lemacs-lsp-client 'lsp-mode)
	:defer t
	:hook
	((python-ts-mode . lsp)
	 (js-ts-mode . lsp)
	 (typescript-ts-mode . lsp)
	 (rust-ts-mode . lsp)
	 (tsx-ts-mode . lsp)
	 (css-mode . lsp)
	 (sass-mode . lsp)
	 (web-mode . lsp)
	 (prisma-mode . lsp))
	:ensure t
	:config
	(lsp-inlay-hints-mode)
  (setq lsp-inlay-hint-enable t)

  (setq lsp-completion-provider :none)

	(setq lsp-enable-links nil)
	(setq lsp-eldoc-enable-hover t)
    (setq lsp-eldoc-render-all t)
	(setq lsp-python-ms-python-executable "/usr/bin/python3")

	(setq lsp-headerline-breadcrumb-enable-symbol-numbers t)
	(setq lsp-headerline-arrow "▶")
	(setq lsp-headerline-breadcrumb-enable-diagnostics nil)
	(setq lsp-headerline-breadcrumb-icons-enable nil)

	(setq lsp-log-io nil)   ;; Don't log everything = speed
	(setq lsp-idle-delay 0) ;; If needed, increase to 0.5...
	(setq lsp-keep-workspace-alive nil)
	(setq lsp-keymap-prefix "C-c l")
	(define-key lsp-mode-map (kbd "C-c l") lsp-command-map)

	;; ESLINT is hell...
	;; Install it globally (taking in consideration node is the same version as above)
	(setq lsp-eslint-server-command '("vscode-eslint-language-server" "--stdio"))
	;; (setq lsp-eslint-server-command '("~/.nvm/versions/node/v16.15.0/bin/vscode-eslint-language-server" "--stdio"))
	;; (setq lsp-eslint-server-command '("vscode-eslint-language-server" "--stdio"))

	;; LSP Custom for: Prisma
	(add-to-list 'load-path "~/.emacs.d/site-lisp/prisma-mode/")
	(require 'prisma-mode)
	(require 'lsp-prisma)

	(add-hook 'before-save-hook #'(lambda () (when (eq major-mode 'prisma-mode)
                                               (lsp-format-buffer))))
	(add-hook 'prisma-mode-hook #'lsp-deferred)

	;; LSP requirements on the server
	;; sudo npm i -g typescript-language-server; sudo npm i -g typescript

	;; LSP Mapping on what mode uses what LSP server
	(setq lsp-language-id-configuration '((java-mode . "java")
                                          (python-mode . "python")
										  (python-ts-mode . "python")
                                          (gfm-view-mode . "markdown")
                                          (rust-mode . "rust")
                                          (rustic-mode . "rust")
                                          (rust-ts-mode . "rust")
                                          (css-mode . "css")
                                          (sass-mode . "sass")
                                          (xml-mode . "xml")
                                          (c-mode . "c")
                                          (c++-mode . "cpp")
                                          (objc-mode . "objective-c")
                                          (web-mode . "html")
                                          (html-mode . "html")
                                          (sgml-mode . "html")
                                          (mhtml-mode . "html")
                                          (go-mode . "go")
                                          (haskell-mode . "haskell")
                                          (php-mode . "php")
                                          (json-mode . "json")
                                          (js-ts-mode . "javascript")
                                          (js-mode . "javascript")
                                          (rjsx-mode . "javascript")
                                          (javascript-mode . "javascript")
                                          (typescript-mode . "typescript")
                                          (typescript-ts-mode . "typescript")
                                          (tsx-ts-mode . "typescriptreact")
                                          (prisma-mode . "prisma")
                                          (typescriptreact-mode . "typescriptreact")
                                          (ruby-mode . "ruby")
										  (emacs-lisp-mode . nil)
                                          ))
	;; LSP debugging
	;; (setq lsp-print-io t)
	;; (setq lsp-trace t)
	;; (setq lsp-print-performance t)

	))

(use-package ellama
  :defer t
  :ensure t
  :init
  (setopt ellama-language "English")
  (require 'llm-ollama)
  (setopt ellama-provider
          (make-llm-ollama
            :chat-model "codellama" :embedding-model "codellama")))

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

(use-package yasnippet
  :ensure t
  :defer t
  :diminish yas-minor-mode
  :custom (yas-keymap-disable-hook
           (lambda () (and (frame-live-p corfu--frame)
                           (frame-visible-p corfu--frame))))
  :hook (after-init . yas-global-mode))

(use-package yasnippet-snippets
  :ensure t
  :defer t
  :after (:all yasnippet)
  :config)

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

;;; -------------------------------- INIT/PROVIDE THIS CONFIG

(provide 'init)
;;; init.el ends here
