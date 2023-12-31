;; init.el --- LEmacs (Lionyx Emacs)
;; Author: Rahul M. Juliato <rahul.juliato@gmail.com>
;; URL: https://github.com/LionyxML/lemacs
;; Keywords: config, emacs, init
;; Version: 0.1.4
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

;;; --------------------------------- USE-PACKAGE INIT


;; Performance Hack 01 --- GC buffer before all
(setq gc-cons-threshold #x40000000)

;; Performance Hack 02 --- Quickier filename handling, resetted after load
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Performance Hack 03 --- The basic fundamental mode for begin with
(setq initial-major-mode 'fundamental-mode)

;; (profiler-start 'cpu)

;; Removes the titlebar from GUI Emacs
;;(add-to-list 'default-frame-alist '(undecorated . t))
(menu-bar-mode -1)
(tool-bar-mode -1)


;; Native compilation settings
(when (featurep 'native-compile)
  ;; Set the right directory to store the native compilation cache
  (let ((path (expand-file-name "eln-cache/" user-emacs-directory)))
    (setq-default native-comp-eln-load-path       (list path)
                  native-compile-target-directory path)
    (when (fboundp 'startup-redirect-eln-cache)
      (startup-redirect-eln-cache path)))
  (setq-default native-comp-async-report-warnings-errors nil  ;; Silence compiler warnings as they can be pretty disruptive
                native-comp-deferred-compilation         t    ;; Make native compilation happens asynchronously
                package-native-compile                   t)   ;; Compile installed packages
  )


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

;;; --------------------------------- CUSTOM SET VARIABLES
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#4F4F4F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3"
	"#6F6F6F"])
 '(column-number-mode t)
 '(cursor-type '(bar . 3))
 '(custom-enabled-themes '(catppuccin))
 '(custom-safe-themes
   '("6cff32351bcb1726accf9dcf9c400367971eaa8bb1d163409b78ea9c9a6ae8d0"
	 "714394050e703db8a773ed350ca6f9cb6636d4bf2e348514804a48929aafc762"
	 "8390abb2cc504d44f0c9dfdaf79d4e943f0328a933e20ceec74c74d17d65834f"
	 "2cc1ac47eed7ac51d79d1aaf6218d52ec84d9c6eb8a448f221f592bddfe51550"
	 "dc2e1b0abb9a5d2033f6d881618932dcdb9af7633d8fa44336f9c9a3484379bd"
	 "eb0f822891b90a730f3331959311439f01bb39da3cdf998b5693ecec877858d0"
	 "e1990eeea39781f009b7f4634ca52a770d05bb7ce423a8fbbcd8a4f327efb626"
	 "4f6dc03105f64cd7e5a3f555ea7c6bac7d9447141473ef9ff3c23b63858066da"
	 "1b8df5c4f3364ebfbe9c0d3d859f6c31ab652ba518612ec27b12e462ce677731"
	 "82b43e48862ecc7e3af29838ed843227e331b187865828dc4915021c5a74baa1"
	 "51f3fb81f9233280cb28ee3023e43e82c9307d59d158626881ca14f964d2abeb"
	 "242f33ba517c05f45e075d8ed3d13c0a7b7d1392e0c95d66830029e561607085"
	 "45e409674661674c12070af5f8ef71741599eeb9fccd84557f1b822509f3b100"
	 "e6b0ec96166bb3bb2843d83e56c0292308aab10ee5b79fb921d16ad2dbea5d5f"
	 "38457f8afb329ce87e1a41d31e155acb4dcdf5ee6a1ea703d401f2042747a69f"
	 "2459d6e7e96aefaed9cebaf7fde590f64e76c96f48632d8310cfea5d10ec2bb1"
	 "50bb891011dfe0c30cd463c65e898523788d4ac4e6df141eed75030a33da1135"
	 "7f34e5ab75ec580aff579b3b0f40379d280f8441e424b7a04322524ed7f348b6"
	 "31804a8ea314e76b68f8b1c454212c3d9710c4294b8cfbaa008dd338c8d91773"
	 "0018c218377a0f234066cd01eb9b636d3739b0b614c7b2c0b8e37a306b7bf8ef"
	 "e871f44a640f98523876f77dccdbf0e20747ca7e111f9f147fe23c9d5f4937c1"
	 "406d7c11a38d7b0e6c305ea91515cbd0c89cd73c55d041da9545338df98f1db4"
	 "2fcd2b44646836f0f4acbd42a13fa85123dac744628f0105a5e9f0f7dbbc936a"
	 "80214de566132bf2c844b9dee3ec0599f65c5a1f2d6ff21a2c8309e6e70f9242"
	 "d23073a9616156a16aecbd3d38e1c3a1f006fc5d920e3fbcb681411e35d2a096"
	 "c191ad8745b348656877bb8fd54cf8398911add379c7d0fdb235f755123c8c15"
	 "801c56b8fb127b8b8ce20b31b493690f86fb13e7bd51ad911f5bb1a0f4310c14"
	 "35f1be3b2bda0b91473107f455c54cf5ff74a8a9371e13a11a0a75d8d06825a6"
	 "46aa01ed69cef28b48aaa49053a6f987f9c12c06cf9f88a028b249dcc5a48157"
	 "0527c20293f587f79fc1544a2472c8171abcc0fa767074a0d3ebac74793ab117"
	 default))
 '(dired-kill-when-opening-new-dired-buffer t)
 '(dired-listing-switches "-lh")
 '(doc-view-continuous t)
 '(emms-mode-line-icon-image-cache
   '(image :type xpm :ascent center :data
		   "/* XPM */\12static char *note[] = {\12/* width height num_colors chars_per_pixel */\12\"    10   11        2            1\",\12/* colors */\12\". c #358d8d\",\12\"# c None s None\",\12/* pixels */\12\"###...####\",\12\"###.#...##\",\12\"###.###...\",\12\"###.#####.\",\12\"###.#####.\",\12\"#...#####.\",\12\"....#####.\",\12\"#..######.\",\12\"#######...\",\12\"######....\",\12\"#######..#\" };") t)
 '(eshell-visual-commands
   '("vi" "screen" "top" "less" "more" "lynx" "ncftp" "pine" "tin" "trn"
	 "elm" "irssi" "nmtui-connect" "nethack" "vim" "alsamixer"
	 "ncmpcpp" "newsbeuter" "nethack" "mutt"))
 '(exec-path
   '("/bin" "/usr/bin" "/usr/local/bin" "/usr/local/sbin" "/usr/sbin"))
 '(flycheck-checker-error-threshold nil)
 '(flycheck-checkers
   '(rustic-clippy lsp ada-gnat asciidoctor asciidoc awk-gawk
				   bazel-build-buildifier bazel-module-buildifier
				   bazel-starlark-buildifier
				   bazel-workspace-buildifier c/c++-clang c/c++-gcc
				   c/c++-cppcheck cfengine chef-foodcritic coffee
				   coffee-coffeelint coq css-csslint css-stylelint
				   cuda-nvcc cwl d-dmd dockerfile-hadolint
				   elixir-credo emacs-lisp emacs-lisp-checkdoc
				   ember-template erlang-rebar3 erlang eruby-erubis
				   eruby-ruumba fortran-gfortran go-gofmt go-golint
				   go-vet go-build go-test go-errcheck go-unconvert
				   go-staticcheck groovy haml handlebars
				   haskell-stack-ghc haskell-ghc haskell-hlint
				   html-tidy javascript-eslint javascript-jshint
				   javascript-standard json-jsonlint json-python-json
				   json-jq jsonnet less less-stylelint llvm-llc
				   lua-luacheck lua markdown-markdownlint-cli
				   markdown-mdl nix nix-linter opam perl
				   perl-perlcritic php php-phpmd php-phpcs processing
				   proselint protobuf-protoc protobuf-prototool pug
				   puppet-parser puppet-lint python-flake8
				   python-pylint python-pycompile python-pyright
				   python-mypy r-lintr racket rpm-rpmlint rst-sphinx
				   rst ruby-rubocop ruby-standard ruby-reek
				   ruby-rubylint ruby ruby-jruby rust-cargo rust
				   rust-clippy scala scala-scalastyle scheme-chicken
				   scss-lint scss-stylelint sass/scss-sass-lint sass
				   scss sh-bash sh-posix-dash sh-posix-bash sh-zsh
				   sh-shellcheck slim slim-lint sql-sqlint
				   systemd-analyze tcl-nagelfar terraform
				   terraform-tflint tex-chktex tex-lacheck texinfo
				   textlint typescript-tslint verilog-verilator
				   vhdl-ghdl xml-xmlstarlet xml-xmllint yaml-jsyaml
				   yaml-ruby yaml-yamllint))
 '(flycheck-indication-mode-line-symbol '<)
 '(grep-find-ignored-directories
   '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN"
	 "_darcs" "{arch}" "node_modules" "build" "dist"))
 '(hkey-init nil)
 '(inhibit-startup-buffer-menu nil)
 '(ispell-dictionary "pt_BR")
 '(jdee-db-active-breakpoint-face-colors (cons "#0d0d0d" "#5DD8FF"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#0d0d0d" "#67B7A4"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#0d0d0d" "#6C7986"))
 '(magit-diff-use-overlays nil)
 '(magit-ediff-dwim-show-on-hunks t)
 '(make-backup-files nil)
 '(mm-text-html-renderer 'shr)
 '(native-comp-async-report-warnings-errors 'silent)
 '(nnreddit-python-command "python3")
 '(nrepl-message-colors
   '("#9d0006" "#af3a03" "#b57614" "#747400" "#c6c148" "#004858"
	 "#689d6a" "#d3869b" "#8f3f71"))
 '(objed-cursor-color "#FC6A5D")
 '(org-agenda-files '("/Users/rmj/notas/gaveta/gcal.org"))
 '(org-babel-load-languages '((emacs-lisp . t) (python . t) (ruby . t) (shell . t)))
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(org-fontify-done-headline nil)
 '(org-fontify-todo-headline nil)
 '(org-safe-remote-resources
   '("\\`https://fniessen\\.github\\.io/org-html-themes/org/theme-readtheorg\\.setup\\'"))
 '(package-selected-packages
   '(add-node-modules-path catppuccin-theme company consult
						   consult-flycheck corfu-terminal
						   css-in-js-mode diff-hl docker
						   dockerfile-mode doom-modeline dotenv-mode
						   ef-themes eldoc-box ellama
						   emacs-ibuffer-project embark embark-consult
						   emms erc-hl-nicks exec-path-from-shell
						   expand-region flycheck gh-md
						   gnu-elpa-keyring-update handlebars-mode
						   hl-indent hl-todo ibuffer-project
						   indent-guide kkp lsp-mode lsp-ui magit
						   magit-stats maple-minibuffer marginalia
						   markdown-mode mmm-mode multi-vterm
						   nerd-icons-completion nerd-icons-corfu
						   nerd-icons-dired nerd-icons-ibuffer
						   orderless org-ros package-lint prettier
						   python-black pyvenv rainbow-delimiters
						   restclient rust-mode rustic sass-mode
						   scss-mode smartparens transmission
						   transpose-frame tree-sitter
						   tree-sitter-langs treemacs
						   treemacs-icons-dired treemacs-magit
						   treemacs-nerd-icons undo-tree vc-msg
						   vertico wgrep which-key xclip yaml-mode))
 '(pdf-view-midnight-colors '("#b2b2b2" . "#292b2e"))
 '(pos-tip-background-color "#4F4F4F")
 '(pos-tip-foreground-color "#FFFFEF")
 '(python-black-extra-args '("--line-length" "79"))
 '(python-guess-indent nil)
 '(python-indent-def-block-scale 4)
 '(python-indent-guess-indent-offset nil)
 '(python-indent-offset 4)
 '(python-shell-interpreter "/usr/bin/python3")
 '(scroll-bar-mode nil)
 '(scss-output-directory "../css/")
 '(scss-sass-options '("--no-source-map"))
 '(send-mail-function 'smtpmail-send-it)
 '(tab-width 4)
 '(tree-sitter-major-mode-language-alist
   '((typescriptreact-mode . tsx) (agda2-mode . agda) (sh-mode . bash)
	 (c-mode . c) (caml-mode . ocaml) (clojure-mode . clojure)
	 (csharp-mode . c-sharp) (c++-mode . cpp) (d-mode . d)
	 (css-mode . css) (elm-mode . elm) (elixir-mode . elixir)
	 (erlang-mode . erlang) (ess-r-mode . r) (fennel-mode . fennel)
	 (go-mode . go) (haskell-mode . haskell) (hcl-mode . hcl)
	 (terraform-mode . hcl) (html-mode . html)
	 (markdown-mode . markdown) (mhtml-mode . html) (nix-mode . nix)
	 (java-mode . java) (javascript-mode . javascript)
	 (js-mode . javascript) (js2-mode . javascript)
	 (js3-mode . javascript) (js-ts-mode . javascript)
	 (json-mode . json) (jsonc-mode . json) (julia-mode . julia)
	 (lua-mode . lua) (meson-mode . meson) (ocaml-mode . ocaml)
	 (perl-mode . perl) (php-mode . php) (prisma-mode . prisma)
	 (python-mode . python) (pygn-mode . pgn) (rjsx-mode . javascript)
	 (ruby-mode . ruby) (rust-mode . rust) (rustic-mode . rust)
	 (scala-mode . scala) (scheme-mode . scheme) (swift-mode . swift)
	 (toml-mode . toml) (tuareg-mode . ocaml)
	 (typescript-ts-mode . typescript) (tsx-ts-mode . typescript)
	 (tsx-js-mode . typescript) (typescript-mode . typescript)
	 (verilog-mode . verilog) (yaml-mode . yaml) (zig-mode . zig)))
 '(treemacs-no-png-images nil)
 '(treemacs-width 40)
 '(treesit-font-lock-level 4)
 '(truncate-lines t)
 '(w3m-default-display-inline-images t)
 '(xterm-mouse-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 130 :family "Hack Nerd Font" :embolden true))))
 '(term ((t (:background "black" :foreground "gray100")))))

;;; --------------------------------- GENERAL FACES
(when (eq system-type 'darwin)
(custom-set-faces
 '(default ((t (:height 130 :family "Hack Nerd Font" :embolden true))))
  '(term ((t (:background "black" :foreground "gray100"))))))

(unless (eq system-type 'darwin)
(custom-set-faces
 '(default ((t (:height 110 :family "Hack Nerd Font" :embolden true))))
  '(term ((t (:background "black" :foreground "gray100"))))))


;;; --------------------------------- EXTERNAL PACKAGES
(use-package add-node-modules-path
  :defer t
  :ensure t
  :config)

(use-package catppuccin-theme
  :defer t
  :ensure t
  :config)

(use-package diff-hl
  :defer t
  :ensure t
  :custom
  (diff-hl-margin-mode t)
  (diff-hl-margin-symbols-alist
   '((insert . " ") (delete . " ") (change . " ") (unknown . " ")
	 (ignored . " ")))
  :bind
  (("M-9" . 'diff-hl-previous-hunk)
   ("M-0" . 'diff-hl-next-hunk))
  :config)

(use-package docker
  :defer t
  :ensure t
  :config)

(use-package dockerfile-mode
  :defer t
  :ensure t
  :config)

(use-package doom-modeline
  :defer t
  :ensure t
  :custom
  (doom-modeline-buffer-file-name-style 'truncate-upto-root)
  (doom-modeline-buffer-name t)
  (doom-modeline-vcs-max-length 25)
  (doom-modeline-icon t)
  ()
  :config
  (setq inhibit-compacting-font-caches t) ;; Don´t compact font caches during GC
  
  (unless (eq system-type 'darwin)
    (if (facep 'mode-line-active)
        (set-face-attribute 'mode-line-active nil
                            :family "Hack Nerd Font"
                            :height 110) ; For 29+
      (set-face-attribute 'mode-line nil
                          :family "Hack Nerd Font"
                          :height 110))
    (set-face-attribute 'mode-line-inactive nil
                        :family "Hack Nerd Font"
                        :height 110))

  (when (eq system-type 'darwin)
    (if (facep 'mode-line-active)
        (set-face-attribute 'mode-line-active nil
                            :family "Hack Nerd Font"
                            :height 130) ; For 29+
      (set-face-attribute 'mode-line nil
                          :family "Hack Nerd Font"
                          :height 130))
    (set-face-attribute 'mode-line-inactive nil
                        :family "Hack Nerd Font"
                        :height 130)))

(use-package dotenv-mode
  :defer t
  :ensure t
  :config)

(use-package emms
  :defer t
  :ensure t
  :config
  (require 'emms-setup)
  (emms-all)
  (emms-default-players)
  (setq-default
   emms-source-file-default-directory "~/work_music/"

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

(use-package exec-path-from-shell
  :defer t
  :ensure t
  :config)

(use-package expand-region
  :defer t
  :ensure t
  :bind
  (("M-1" . 'er/expand-region))
  :config)

(use-package gh-md
  :defer t
  :ensure t
  :config)

(use-package gnu-elpa-keyring-update
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

(use-package hl-indent
  :defer t
  :ensure t
  :config)

(use-package hl-todo
  :defer t
  :ensure t
  :config)

(use-package indent-guide
  :defer t
  :ensure t
  :config
  (setq indent-guide-char "│"))

(use-package magit
  :defer t
  :ensure t
  :config
  (setq magit-executable "/usr/local/bin/git")
  (setq magit-diff-refine-hunk 'all)
  (setq ediff-diff-options "")
  (setq ediff-custom-diff-options "-u")
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally))

(use-package magit-stats
  :defer t
  :ensure t
  :config)

(use-package markdown-mode
  :defer t
  :ensure t
  :config)

(use-package multi-vterm
  :defer t
  :ensure t
  :config)

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
  :config)

(use-package package-lint
  :defer t
  :ensure t
  :config)

(use-package prettier
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

(use-package python-black
  :defer t
  :ensure t
  :config)

(use-package pyvenv
  :defer t
  :ensure t
  :after (:any python-ts-mode)
  :config)

(use-package rainbow-delimiters
  :defer t
  :ensure t
  :config)

(use-package restclient
  :defer t
  :ensure t
  :config)

(use-package rust-mode
  :defer t
  :ensure t
  :config)

(use-package rustic
  :defer t
  :ensure t
  :config
  (setq rustic-analyzer-command '("~/.cargo/bin/rust-analyzer"))
  (setq rustic-format-on-save t))

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

(use-package tree-sitter
  :ensure t
  :config
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

  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook 'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :defer t
  :ensure t
  :config)

(use-package treemacs
  :defer t
  :ensure t
  :bind
  (("M-i" . treemacs))
  :config
  (setq treemacs-show-hidden-files t)
  ;; (setq treemacs-resize-icons 44)
  (setq treemacs-filewatch-mode t)
  (setq treemacs-icons nil)
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
  ;; :defer t
  :ensure t
  :after (:all treemacs nerd-icons)
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package undo-tree
  :defer t
  :ensure t
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/.cache/undo")))
  )

(use-package vc-msg
  :defer t
  :ensure t
  :bind
  (("M-2" . 'vc-msg-show))
  :config
  (setq vc-msg-show-at-line-beginning-p nil))

(use-package vterm
  :defer t
  :ensure t
  :bind
  (("M-t" . 'my-toggle-vterm-buffer))
  :config
  (add-hook 'vterm-mode-hook (lambda ()
                               (define-key vterm-mode-map (kbd "M-t") #'my-toggle-vterm-buffer)
                               (setq-local global-hl-line-mode
                                           nil))))

(use-package which-key
  :defer t
  :ensure t
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
  :config)

(use-package marginalia
  ;; :general
  ;; (:keymaps 'minibuffer-local-map
  ;;  "M-A" 'marginalia-cycle)
  :ensure t
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

(use-package company
  :defer t
  :ensure t
  :config
  (setq company-tooltip-align-annotations t))

;; NOTE TO SELF: Corfu is not yet mature, meaning it needs A LOT of effort to make
;;               it work on both TUI and GUI, and auto doc for TUI is now broken...
;;               getting back to good old company-mode...

(use-package consult-flycheck
  :defer t
  :ensure t)

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :ensure t
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
         ("M-g f" . consult-flycheck)               ;; Alternative: consult-flycheck
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
         ;; ("M-s l" . consult-line)
         ("M-s l" . my-consult-line-current-selection-or-word)
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
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
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
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep
  :ensure t)

(use-package emacs
  :bind
  (("M-D" . 'my-duplicate-line-or-region)
   ("C-x C-b" . 'ibuffer))
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)

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

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)

  ;; Remap scrolling to always center
  (global-set-key (kbd "C-v") (lambda ()
								(interactive)
								(scroll-up-command)
								(recenter)
								))
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

  ;; Inits everything in the right order
  (when (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode 1))
  (setq pixel-scroll-precision-use-momentum nil)

  ;; Activate global on init
  (global-undo-tree-mode)

  ;; General customizations
  ;; (my-setup-outline-mode-elisp)
  (set-default 'truncate-lines t)
  (desktop-save-mode 1)

  (setq ring-bell-function 'ignore)
  (setq-default line-spacing 1)
  (setq-default ident-tabs-mode nil)
  (setq initial-scratch-message "")

  (setq ibuffer-show-empty-filter-groups nil)

  (setq gnus-init-file "~/.gnus.el")

  (setq warning-minimum-level :emergency)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq make-backup-files nil)
  (setq auto-save-default nil)
  (setq create-lockfiles nil)
  (setq window-combination-resize t
        split-width-threshold 300)

  (doom-modeline-mode 1))

(use-package flycheck
  :defer t
  :ensure t
  :config
  (with-eval-after-load 'flycheck
    (custom-set-variables
     '(flycheck-indication-mode-line-symbol (quote <))))

  (setq-default flycheck-indication-mode 'left-margin)
  (add-hook 'flycheck-mode-hook #'flycheck-set-indication-mode)

  (defun my-set-flycheck-margins ()
    (interactive)
    (setq left-fringe-width 1 right-fringe-width 1
          left-margin-width 1 right-margin-width 1)
    (flycheck-refresh-fringes-and-margins))

  (add-hook 'flycheck-mode-hook #'my-set-flycheck-margins))

(use-package lsp-mode
  :defer t
  :hook
  ((python-ts-mode . lsp)
   (js-ts-mode . lsp)
   (typescript-ts-mode . lsp)
   (tsx-ts-mode . lsp)
   (css-mode . lsp)
   (sass-mode . lsp)
   (web-mode . lsp)
   (prisma-mode . lsp))
  :ensure t
  :config
  (lsp-inlay-hints-mode)
  (setq lsp-inlay-hint-enable t)

  (setq lsp-enable-links nil)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-python-ms-python-executable "/usr/bin/python3")

  (setq lsp-headerline-breadcrumb-enable-symbol-numbers t)
  (setq lsp-headerline-arrow "▶")
  (setq lsp-headerline-breadcrumb-enable-diagnostics nil)
  (setq lsp-headerline-breadcrumb-icons-enable nil)
  
  (setq lsp-log-io nil) ;; Don't log everything = speed
  (setq lsp-idle-delay 0.5)
  (setq lsp-keep-workspace-alive nil)
  (setq lsp-keymap-prefix "C-c l")
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)


  ;; ESLINT is hell...
  ;; Install it globally (taking in consideration node is the same version as above)
  (setq lsp-eslint-server-command '("vscode-eslint-language-server" "--stdio"))
  ;; (setq lsp-eslint-server-command '("/Users/rmj/.nvm/versions/node/v16.15.0/bin/vscode-eslint-language-server" "--stdio"))
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
                                        (tsx-ts-mode . "typescript")
                                        (prisma-mode . "prisma")
                                        (typescriptreact-mode . "typescriptreact")
                                        (ruby-mode . "ruby")
                    (emacs-lisp-mode . nil)
                                        ))
  ;; LSP debugging
  ;;(setq lsp-print-io t)
  ;;(setq lsp-trace t)
  ;;(setq lsp-print-performance t)

  )

(use-package lsp-ui
  :defer t
  :ensure t
  :after (:all lsp)
  :custom
  (lsp-ui-doc-max-width 100)
  (lsp-ui-peek-always-show nil)
  (lsp-ui-doc-alignment 'window)
  (lsp-ui-doc-position 'top) ;; 'at-point 'top 'bottom
  ;; (lsp-ui-doc-show-with-cursor t)
  ;; (lsp-ui-doc-enable t)
  ;; (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-diagnostic-max-line-length 100)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions nil)
  :bind
  (("M-3" . 'lsp-ui-peek-find-implementation)
   ("M-4" . 'lsp-ui-peek-find-references)
   ("M-5" . 'lsp-ui-doc-toggle))
  :config)

(use-package ellama
  :defer t
  :ensure t
  :init
  (setopt ellama-language "English")
  (require 'llm-ollama)
  (setopt ellama-provider
          (make-llm-ollama
            :chat-model "codellama" :embedding-model "codellama")))

(exec-path-from-shell-initialize)

;;; --------------------------------- DIRED
(defun dired-get-size ()
  "On hitting ? gets the selected or under cursor file/dir size."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message "Size of all marked files: %s"
               (progn
                 (re-search-backward "\\(^[0-9.,]+[A-Za-z]+\\).*total$")
                 (match-string 1))))))

(define-key dired-mode-map (kbd "?") 'dired-get-size)

;;; --------------------------------- I-SEARCH
(setq isearch-lazy-count t)
(setq lazy-count-prefix-format "(%s/%s) ")
(setq lazy-count-suffix-format nil)
(setq search-whitespace-regexp ".*?")

;;; --------------------------------- MY-FUNCTIONS
(defun my-erc-global-configs ()
  "Set ERC overwall configs."
  (interactive)
  (setq erc-hide-list '("JOIN" "PART" "QUIT"))
  (setq erc-timestamp-format "[%H:%M]")
  (add-hook 'window-configuration-change-hook
            (lambda ()
              (setq erc-fill-column (- (window-width) 2))))
  (setq erc-autojoin-channels-alist '((".*\\.libera\\.chat" "#emacs"))))

(defun my-toggle-vterm-buffer ()
  "Toggle vterm buffer on the bottom part of the screen."
  (interactive)
  (if (get-buffer-window "*vterm*" 'visible)
      (delete-window (get-buffer-window "*vterm*"))
    (let ((window-height (truncate (* 0.2 (window-height))))
          (buf (get-buffer-create "*vterm*")))
      (split-window-vertically (- window-height))
      (balance-windows)
      (set-window-buffer (window-in-direction 'below) buf)
      (select-window (window-in-direction 'below))
      (vterm)
      (select-window (window-in-direction 'above))
      (enlarge-window window-height)
      (select-window (window-in-direction 'below))
      )))

(defun my-duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active.
With argument N, make N copies.
negative N, comment out original line and use the absolute value."
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region        ;Get region if active, otherwise line
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      (if (< 0 (forward-line 1)) ;Go to beginning of next line, or make a new one
                          (newline))))))
        (dotimes (i (abs (or n 1)))     ;Insert N times, or once if not specified
          (insert text))))
    (if use-region nil                  ;Only if we're working with a line (not a region)
      (let ((pos (- (point) (line-beginning-position)))) ;Save column
        (if (> 0 n)                             ;Comment out original with negative arg
            (comment-region (line-beginning-position) (line-end-position)))
        (forward-line 1)
        (forward-char pos)))))

(defun my-set-terminal-transparency ()
  "Set transparency on terminal.
Also terminal emulator must be already configured to support it."
  (interactive)
  (set-face-background 'default "unspecified-bg" (selected-frame))
  (set-face-background 'line-number "unspecified-bg" (selected-frame)))

(defun my-transparency-set ()
  "Set frame transparency (Graphical Mode)."
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(85 50)))

(defun my-transparency-unset ()
  "Unset frame transparency (Graphical Mode)."
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(100 100)))

(defun my-setup-outline-mode-elisp ()
  "Automatically load outline mode on elisp files."
  (defun my-elisp-mode-hook ()
    (interactive)
    (outline-minor-mode 1)
    (outline-hide-sublevels 1))
  (add-hook 'emacs-lisp-mode-hook #'my-elisp-mode-hook))

(defun my-first-install ()
  "Install tree-sitter grammars and nerd-icons fonts on the first run."
  (interactive)
  (condition-case err
      (progn
        ;; Load necessary packages
        (require 'tree-sitter)
        (require 'nerd-icons)

        ;; Install tree-sitter grammars
        (call-interactively 'tree-sitter-langs-install-grammars)

        ;; Install nerd-icons fonts
        (call-interactively 'nerd-icons-install-fonts)

        ;; Close Emacs
        (kill-emacs))

    (error
     ;; Display error message and suggest running with debugging
     (message "LEmacs failed to install, run 'emacs -nw --debug-init'"))))


;;; --------------------------------- AFTER INIT HOOK
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs has fully loaded. This code runs after startup.")

            ;; Performance Hack 02.1 --- Quickier filename handling, resetted after load
            (setq file-name-handler-alist default-file-name-handler-alist)

            ;; macOS specials
            (when (eq system-type 'darwin)
              (setq mac-option-key-is-meta nil)
              (setq mac-option-modifier nil)
              (setq mac-command-key-is-meta t)
              (setq mac-command-modifier 'meta))

            (xclip-mode 1)
            (delete-selection-mode 1)
            ;; (global-company-mode)
            (global-diff-hl-mode)
            (diff-hl-flydiff-mode)
            (my-setup-outline-mode-elisp)

            (add-hook 'prog-mode-hook 'hl-todo-mode +1)
            (add-hook 'prog-mode-hook 'smartparens-mode +1)
            (add-hook 'prog-mode-hook 'indent-guide-mode +1)
            (add-hook 'prog-mode-hook 'display-line-numbers-mode)
            (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

            (treemacs)
            (treemacs)
            (which-key-mode)
            
      ;; (profiler-report)
      ;; (profiler-stop)

      (with-current-buffer (get-buffer-create "*scratch*")
        (insert (format "

  ██╗     ███████╗███╗   ███╗ █████╗  ██████╗███████╗
  ██║     ██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝
  ██║     █████╗  ██╔████╔██║███████║██║     ███████╗
  ██║     ██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║
  ███████╗███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║
  ╚══════╝╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝

    Loading time : %s
    Packages     : %s
"
        (emacs-init-time)
        (number-to-string (length package-activated-list)))))
    ))



;;; --------------------------------- INIT/PROVIDE THIS CONFIG

(provide 'init)
;;; init.el ends here
