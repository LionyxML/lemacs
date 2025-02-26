;;; eat.el --- Emulate A Terminal, in a region, in a buffer and in Eshell -*- lexical-binding: t; -*-

;; Copyright (C) 2022, 2023 Akib Azmain Turja.

;; Author: Akib Azmain Turja <akib@disroot.org>
;; Created: 2022-08-15
;; Version: 0.9.4
;; Package-Requires: ((emacs "26.1") (compat "29.1"))
;; Keywords: terminals processes
;; Homepage: https://codeberg.org/akib/emacs-eat

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Eat's name self-explanatory, it stands for "Emulate A Terminal".
;; Eat is a terminal emulator.  It can run most (if not all)
;; full-screen terminal programs, including Emacs.

;; It is pretty fast, more than three times faster than Term, despite
;; being implemented entirely in Emacs Lisp.  So fast that you can
;; comfortably run Emacs inside Eat, or even use your Emacs as a
;; terminal multiplexer.

;; It has many feature that other Emacs terminal emulator still don't
;; have, for example complete mouse support.

;; It flickers less than other Emacs terminal emulator, so you get
;; more performance and a smooth experience.

;; To start Eat, run M-x eat.  Eat has three keybinding modes:

;;   * "semi-char" mode: This is the default keybinding mode.  Most
;;     keys are bound to send the key to the terminal, except the
;;     following keys: `C-\', `C-c', `C-x', `C-g', `C-h', `C-M-c',
;;     `C-u', `M-x', `M-:', `M-!', `M-&' and some other keys (see the
;;     user option `eat-semi-char-non-bound-keys' for the complete
;;     list).  The following special keybinding are available:

;;       * `C-q': Send next key to the terminal.
;;       * `C-y': Like `yank', but send the text to the terminal.
;;       * `M-y': Like `yank-pop', but send the text to the terminal.
;;       * `C-c' `C-k': Kill process.
;;       * `C-c' `C-e': Switch to "emacs" keybinding mode.
;;       * `C-c' `M-d': Switch to "char" keybinding mode.

;;   * "emacs" mode: No special keybinding, except the following:

;;       * `C-c' `C-j': Switch to "semi-char" keybinding mode.
;;       * `C-c' `M-d': Switch to "char" keybinding mode.
;;       * `C-c' `C-k': Kill process.

;;   * "char" mode: All supported keys are bound to send the key to
;;     the terminal, except `C-M-m' or `M-RET', which is bound to
;;     switch to "semi-char" keybinding mode.

;; If you like Eshell, then there is a good news for you.  Eat
;; integrates with Eshell.  Eat has two global minor modes for Eshell:

;;   * `eat-eshell-visual-command-mode': Run visual commands with Eat
;;     instead of Term.

;;   * `eat-eshell-mode': Run Eat inside Eshell.  After enabling this,
;;     you can run full-screen terminal programs directly in Eshell.
;;     You have three keybinding modes here too, except that `C-c'
;;     `C-k' is not special (i.e. not bound by Eat) in "emacs" mode
;;     and "line" mode.

;;; Code:

(require 'compat)
(require 'subr-x)
(require 'cl-lib)
(require 'ansi-color)
(require 'color)
(require 'shell)
(require 'term)
(require 'url)

;; Needed by `eat-reload'.
(defvar eat--being-loaded nil
  "Non-nil means Eat is being loaded.")

(setq eat--being-loaded t)


;;;; User Options.

(defgroup eat nil
  "Emulate A Terminal."
  :group 'processes
  :group 'terminals
  :link '(url-link "https://codeberg.org/akib/emacs-eat"))

(defgroup eat-term nil
  "Eat terminal emulator."
  :group 'eat)

(defgroup eat-ui nil
  "Eat user interface."
  :group 'eat)

(defgroup eat-eshell nil
  "Eat Eshell integration."
  :group 'eat)

(defcustom eat-buffer-name "*eat*"
  "The basename used for Eat buffers.

This is the default name used when running Eat."
  :type 'string
  :group 'eat-ui)

(defcustom eat-kill-buffer-on-exit nil
  "Non-nil means automatically kill Eat buffer when process exits."
  :type 'boolean
  :group 'eat-ui)

(defcustom eat-term-scrollback-size 131072 ; 128 K
  "Size of scrollback area in characters.  nil means unlimited."
  :type '(choice natnum (const nil))
  :group 'eat-term
  :group 'eat-ui)

(defcustom eat-enable-kill-from-terminal t
  "Non-nil means allow terminal program to add text to `kill-ring'.

When non-nil, terminal program can send special escape sequence to add
some text to `kill-ring'."
  :type 'boolean
  :group 'eat-ui
  :group 'eat-eshell)

(defcustom eat-enable-yank-to-terminal nil
  "Non-nil means allow terminal program to get text from `kill-ring'.

When non-nil, terminal program can get killed text from `kill-ring'.
This is left disabled for security reasons."
  :type 'boolean
  :group 'eat-ui
  :group 'eat-eshell)

(defcustom eat-query-before-killing-running-terminal 'auto
  "Whether to query before killing running terminal.

If the value is t, always query.  If the value is nil, never query.
If the value is `auto', query if a shell command is running (shell
integration needs to be enabled to use this properly)."
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil)
                 (const :tag "If a shell command is running" auto))
  :group 'eat-ui)

(defcustom eat-eshell-fallback-if-stty-not-available 'ask
  "What to do if `stty' is unavailable.

`stty' is a dependency to setup terminal.  If `stty' is unavailable,
Eat won't be able to setup terminal, so any input won't be visible.

The value should be any of the following:

nil           Do nothing.
t             Fallback to plain Eshell if `stty' is not available.
`ask'           Ask what to do.
FUNCTION      Call FUNCTION with the command and arguments (using
                `apply') and fallback to plain Eshell if it returns
                nil."
  :type '(radio (const :tag "Do nothing" nil)
                (const :tag "Fallback to plain Eshell" t)
                (const :tag "Ask interactively" ask)
                (function :tag "Function"))
  :group 'eat-eshell)

(defcustom eat-sixel-scale 1.0
  "Scale Sixel images by this amount."
  :type 'number
  :group 'eat-ui
  :group 'eat-eshell)

(defcustom eat-sixel-aspect-ratio 1.0
  "Aspect ratio of Sixel images.

The value is a positive number specifying the ratio of the width and
height of a Sixel pixel.  For example, the value of 1.5 means the
aspect ratio of 3:2."
  :type 'number
  :group 'eat-ui
  :group 'eat-eshell)

(defcustom eat-sixel-render-formats
  '(xpm svg half-block background none)
  "List of formats to render Sixel, in order of preference."
  :type '(repeat (choice (const :tag "XPM Image" xpm)
                         (const :tag "SVG Image" svg)
                         (const :tag "UTF-8 half block" half-block)
                         (const :tag "Background color" background)
                         (const :tag "None" none)))
  :group 'eat-ui
  :group 'eat-eshell)

(defcustom eat-line-input-ring-size 1000
  "Number of input history items to keep."
  :type 'natnum
  :group 'eat-ui)

(defcustom eat-line-auto-move-to-input t
  "Non-nil means move to input line when inserting characters."
  :type 'boolean
  :group 'eat-ui)

(defcustom eat-line-move-point-for-matching-input 'after-input
  "Controls where to place point after matching input.

\\<eat-line-mode-map>This influences the commands \
\\[eat-line-previous-matching-input-from-input] and \
\\[eat-line-next-matching-input-from-input].
If `after-input', point will be positioned after the input typed
by the user, but before the rest of the history entry that has
been inserted.  If `end-of-line', point will be positioned at the
end of the current logical (not visual) line after insertion."
  :type '(radio (const :tag "Stay after input" after-input)
                (const :tag "Move to end of line" end-of-line))
  :group 'eat-ui)

(defcustom eat-line-input-history-isearch nil
  "Non-nil to Isearch in input history only, not in the terminal.

If t, usual Isearch keys like \\[isearch-backward] and \
\\[isearch-backward-regexp] in Eat buffer search in
the input history.  If `dwim', Isearch keys search in the input
history only when initial point position is on input line.  When
starting Isearch from other parts of the Eat buffer, they search in
the Eat buffer.  If nil, Isearch operates on the whole Eat buffer."
  :type '(choice (const :tag "Don't search in input history" nil)
                 (const :tag "When point is on input line initially, \
search history"
                        dwim)
                 (const :tag "Always search in input history" t))
  :group 'eat-ui)

(defcustom eat-line-input-send-function #'eat-line-send-default
  "Function to send the shell prompt input.

The function is called without any argument.  The buffer is narrowed
to the input.  The function may modify the input but mustn't modify
the buffer restrictions.  It should call
`eat-line-send-default' to send the final output."
  :type 'function
  :group 'eat-ui)

(defcustom eat-semi-char-non-bound-keys
  '([?\C-x] [?\C-\\] [?\C-q] [?\C-g] [?\C-h] [?\e ?\C-c] [?\C-u]
    [?\e ?x] [?\e ?:] [?\e ?!] [?\e ?&]
    [C-insert] [M-insert] [S-insert] [C-M-insert]
    [C-S-insert] [M-S-insert] [C-M-S-insert]
    [C-delete] [M-delete] [S-delete] [C-M-delete]
    [C-S-delete] [M-S-delete] [C-M-S-delete]
    [C-deletechar] [M-deletechar]
    [S-deletechar] [C-M-deletechar] [C-S-deletechar]
    [M-S-deletechar] [C-M-S-deletechar]
    [C-up] [C-down] [C-right] [C-left]
    [M-up] [M-down] [M-right] [M-left]
    [S-up] [S-down] [S-right] [S-left]
    [C-M-up] [C-M-down] [C-M-right] [C-M-left]
    [C-S-up] [C-S-down] [C-S-right] [C-S-left]
    [M-S-up] [M-S-down] [M-S-right] [M-S-left]
    [C-M-S-up] [C-M-S-down] [C-M-S-right] [C-M-S-left]
    [C-home] [M-home] [S-home] [C-M-home] [C-S-home]
    [M-S-home] [C-M-S-home]
    [C-end] [M-end] [S-end] [C-M-end] [C-S-end]
    [M-S-end] [C-M-S-end]
    [C-prior] [M-prior] [S-prior] [C-M-prior]
    [C-S-prior] [M-S-prior] [C-M-S-prior]
    [C-next] [M-next] [S-next] [C-M-next] [C-S-next]
    [M-S-next] [C-M-S-next])
  "List of keys not bound in Eat \"semi-char\" mode.

Keys appearing in this list are not bound to send the key to terminal.
Eat might still bound them to do something else (for example, changing
keybinding mode).

Each element is a vector of form [KEY] or [?\\e KEY], meaning KEY or
M-KEY shouldn't be bound.  KEY shouldn't contain meta (Alt) modifier.

When changing this from Lisp, make sure to call
`eat-update-semi-char-mode-map' to update the keymap and reload Eat to
make the changes effective."
  :type '(repeat sexp)
  :set (lambda (sym val)
         (set-default-toplevel-value sym val)
         (when (and (not eat--being-loaded)
                    (boundp 'eat-semi-char-mode-map))
           (eat-update-semi-char-mode-map)
           (let ((after-load-alist nil)
                 (after-load-functions nil))
             (eat-reload))))
  :group 'eat-ui)

(defcustom eat-eshell-semi-char-non-bound-keys
  '([?\C-\\] [?\C-x] [?\C-g] [?\C-h] [?\e ?\C-c] [?\C-u] [?\C-q]
    [?\e ?x] [?\e ?:] [?\e ?!] [?\e ?&]
    [C-insert] [M-insert] [S-insert] [C-M-insert]
    [C-S-insert] [M-S-insert] [C-M-S-insert]
    [C-delete] [M-delete] [S-delete] [C-M-delete]
    [C-S-delete] [M-S-delete] [C-M-S-delete]
    [C-deletechar] [M-deletechar]
    [S-deletechar] [C-M-deletechar] [C-S-deletechar]
    [M-S-deletechar] [C-M-S-deletechar]
    [C-up] [C-down] [C-right] [C-left]
    [M-up] [M-down] [M-right] [M-left]
    [S-up] [S-down] [S-right] [S-left]
    [C-M-up] [C-M-down] [C-M-right] [C-M-left]
    [C-S-up] [C-S-down] [C-S-right] [C-S-left]
    [M-S-up] [M-S-down] [M-S-right] [M-S-left]
    [C-M-S-up] [C-M-S-down] [C-M-S-right] [C-M-S-left]
    [C-home] [M-home] [S-home] [C-M-home] [C-S-home]
    [M-S-home] [C-M-S-home]
    [C-end] [M-end] [S-end] [C-M-end] [C-S-end]
    [M-S-end] [C-M-S-end]
    [C-prior] [M-prior] [S-prior] [C-M-prior]
    [C-S-prior] [M-S-prior] [C-M-S-prior]
    [C-next] [M-next] [S-next] [C-M-next] [C-S-next]
    [M-S-next] [C-M-S-next])
  "List of keys not bound in Eat-Eshell \"semi-char\" mode.

Keys appearing in this list are not bound to send the key to terminal.
Eat might still bound them to do something else (for example, changing
keybinding mode).

Each element is a vector of form [KEY] or [?\\e KEY], meaning KEY or
M-KEY shouldn't be bound.  KEY shouldn't contain meta (Alt) modifier.

When changing this from Lisp, make sure to call
`eat-eshell-update-semi-char-mode-map' to update the keymap and reload
Eat to make the changes effective."
  :type '(repeat sexp)
  :set (lambda (sym val)
         (set-default-toplevel-value sym val)
         (when (and (not eat--being-loaded)
                    (boundp 'eat-eshell-semi-char-mode-map))
           (eat-eshell-update-semi-char-mode-map)
           (let ((after-load-alist nil)
                 (after-load-functions nil))
             (eat-reload))))
  :group 'eat-eshell)

(defcustom eat-enable-directory-tracking t
  "Non-nil means do directory tracking.

When non-nil, Eat will track the working directory of program.  You
need to configure the program to send current working directory
information.  See Info node `(eat)Directory Tracking' for instructions
to setup your shell."
  :type 'boolean
  :group 'eat-ui
  :group 'eat-eshell)

(defcustom eat-enable-shell-command-history t
  "Non-nil means add shell commands to Emacs history.

When non-nil, any command you run in your shell will also appear in
the history of commands like `eat', `shell-command' and
`async-shell-command'."
  :type 'boolean
  :group 'eat-ui
  :group 'eat-eshell)

(defcustom eat-message-handler-alist nil
  "Alist of message handler name and its handler function.

The keys are the names of message handlers, and the values are their
respective handler functions.

Shells can send Eat messages, as defined in this user option.  If an
appropiate message handler is defined, it's called with the other
arguments, otherwise it's ignored."
  :type '(alist :key-type string
                :value-type function)
  :group 'eat-ui
  :group 'eat-eshell)

(defcustom eat-enable-auto-line-mode nil
  "Non-nil means switch to line mode automatically on shell prompt."
  :type 'boolean
  :group 'eat-ui)

(defcustom eat-enable-shell-prompt-annotation t
  "Non-nil means annotate shell prompt with the status of command.

When non-nil, display a mark in front of shell prompt describing the
status of the command executed in that prompt."
  :type 'boolean
  :group 'eat-ui)

(defcustom eat-shell-prompt-annotation-position 'left-margin
  "The position where to display shell prompt annotation.

The value can be one of the following:

`left-margin'   Use the left margin.
`right-margin'  Use the right margin."
  :type '(choice (const :tag "Left margin" left-margin)
                 (const :tag "Right margin" right-margin))
  :group 'eat-ui)

(defcustom eat-shell-prompt-annotation-running-margin-indicator "-"
  "String in margin annotation to indicate the command is running."
  :type 'string
  :group 'eat-ui)

(defface eat-shell-prompt-annotation-running
  '((t :inherit compilation-info))
  "Face used in annotation to indicate the command is running."
  :group 'eat-ui)

(defcustom eat-shell-prompt-annotation-success-margin-indicator "0"
  "String in margin annotation to indicate the command has succeeded."
  :type 'string
  :group 'eat-ui)

(defface eat-shell-prompt-annotation-success
  '((t :inherit success))
  "Face used in annotation to indicate the command has succeeded."
  :group 'eat-ui)

(defcustom eat-shell-prompt-annotation-failure-margin-indicator "X"
  "String in margin annotation to indicate the command has failed."
  :type 'string
  :group 'eat-ui)

(defface eat-shell-prompt-annotation-failure
  '((t :inherit error))
  "Face used in annotation to indicate the command has failed."
  :group 'eat-ui)

(defcustom eat-shell-prompt-annotation-correction-delay 0.1
  "Seconds to wait before correcting shell prompt annotations.

Wait this many second after terminal update before correcting shell
prompt annotation."
  :type 'number
  :group 'eat-ui)

(defcustom eat-exec-hook nil
  "Hook run after `eat' executes a commamnd.

The hook is run with the process run in the terminal as the only
argument."
  :type 'hook
  :group 'eat-ui)

(defcustom eat-update-hook nil
  "Hook run after the terminal in a Eat buffer is updated."
  :type 'hook
  :group 'eat-ui)

(defcustom eat-exit-hook nil
  "Hook run after the command executed by `eat' exits.

The hook is run with the process that just exited as the only
argument."
  :type 'hook
  :group 'eat-ui)

(defcustom eat-eshell-exec-hook nil
  "Hook run after a terminal is created in Eshell."
  :type 'hook
  :group 'eat-eshell)

(defcustom eat-eshell-update-hook nil
  "Hook run after the terminal in a Eshell buffer is updated."
  :type 'hook
  :group 'eat-eshell)

(defcustom eat-eshell-exit-hook nil
  "Hook run after the terminal in Eshell is deleted."
  :type 'hook
  :group 'eat-eshell)

(defconst eat--cursor-type-value-type
  (let ((cur-type
         '(choice
           (const :tag "Frame default" t)
           (const :tag "Filled box" box)
           (cons :tag "Box with specified size" (const box) integer)
           (const :tag "Hollow cursor" hollow)
           (const :tag "Vertical bar" bar)
           (cons :tag "Vertical bar with specified height" (const bar)
                 integer)
           (const :tag "Horizontal bar" hbar)
           (cons :tag "Horizontal bar with specified width"
                 (const hbar) integer)
           (const :tag "None " nil))))
    `(list
      ,cur-type
      (choice
       (const :tag "No blinking" nil)
       (number :tag "Blinking frequency"))
      ,cur-type))
  "Custom type specification for Eat's cursor type variables.")

(defcustom eat-invisible-cursor-type '(nil nil nil)
  "Type of cursor to use as invisible cursor in Eat buffer.

The value is a list of form (CURSOR-ON BLINKING-FREQUENCY CURSOR-OFF).

When the cursor is on, CURSOR-ON is used as `cursor-type', which see.
BLINKING-FREQUENCY is the blinking frequency of cursor's blinking.
When the cursor is off, CURSOR-OFF is used as `cursor-type'.  This
should be nil when cursor is not blinking."
  :type eat--cursor-type-value-type
  :group 'eat-ui
  :group 'eat-eshell)

(defcustom eat-default-cursor-type
  `(,(default-value 'cursor-type) nil nil)
  "Cursor to use in Eat buffer.

The value is a list of form (CURSOR-ON BLINKING-FREQUENCY CURSOR-OFF).

When the cursor is on, CURSOR-ON is used as `cursor-type', which see.
BLINKING-FREQUENCY is the blinking frequency of cursor's blinking.
When the cursor is off, CURSOR-OFF is used as `cursor-type'.  This
should be nil when cursor is not blinking."
  :type eat--cursor-type-value-type
  :group 'eat-ui
  :group 'eat-eshell)

(defcustom eat-very-visible-cursor-type
  `(,(default-value 'cursor-type) 2 hollow)
  "Very visible cursor to use in Eat buffer.

The value is a list of form (CURSOR-ON BLINKING-FREQUENCY CURSOR-OFF).

When the cursor is on, CURSOR-ON is used as `cursor-type', which see.
BLINKING-FREQUENCY is the blinking frequency of cursor's blinking.
When the cursor is off, CURSOR-OFF is used as `cursor-type'.  This
should be nil when cursor is not blinking."
  :type eat--cursor-type-value-type
  :group 'eat-ui
  :group 'eat-eshell)

(defcustom eat-vertical-bar-cursor-type '(bar nil nil)
  "Vertical bar cursor to use in Eat buffer.

The value is a list of form (CURSOR-ON BLINKING-FREQUENCY CURSOR-OFF).

When the cursor is on, CURSOR-ON is used as `cursor-type', which see.
BLINKING-FREQUENCY is the blinking frequency of cursor's blinking.
When the cursor is off, CURSOR-OFF is used as `cursor-type'.  This
should be nil when cursor is not blinking."
  :type eat--cursor-type-value-type
  :group 'eat-ui
  :group 'eat-eshell)

(defcustom eat-very-visible-vertical-bar-cursor-type '(bar 2 nil)
  "Very visible vertical bar cursor to use in Eat buffer.

The value is a list of form (CURSOR-ON BLINKING-FREQUENCY CURSOR-OFF).

When the cursor is on, CURSOR-ON is used as `cursor-type', which see.
BLINKING-FREQUENCY is the blinking frequency of cursor's blinking.
When the cursor is off, CURSOR-OFF is used as `cursor-type'.  This
should be nil when cursor is not blinking."
  :type eat--cursor-type-value-type
  :group 'eat-ui
  :group 'eat-eshell)

(defcustom eat-horizontal-bar-cursor-type '(hbar nil nil)
  "Horizontal bar cursor to use in Eat buffer.

The value is a list of form (CURSOR-ON BLINKING-FREQUENCY CURSOR-OFF).

When the cursor is on, CURSOR-ON is used as `cursor-type', which see.
BLINKING-FREQUENCY is the blinking frequency of cursor's blinking.
When the cursor is off, CURSOR-OFF is used as `cursor-type'.  This
should be nil when cursor is not blinking."
  :type eat--cursor-type-value-type
  :group 'eat-ui
  :group 'eat-eshell)

(defcustom eat-very-visible-horizontal-bar-cursor-type '(hbar 2 nil)
  "Very visible horizontal bar cursor to use in Eat buffer.

The value is a list of form (CURSOR-ON BLINKING-FREQUENCY CURSOR-OFF).

When the cursor is on, CURSOR-ON is used as `cursor-type', which see.
BLINKING-FREQUENCY is the blinking frequency of cursor's blinking.
When the cursor is off, CURSOR-OFF is used as `cursor-type'.  This
should be nil when cursor is not blinking."
  :type eat--cursor-type-value-type
  :group 'eat-ui
  :group 'eat-eshell)

(defcustom eat-minimum-latency 0.008
  "Minimum display latency in seconds.

Lowering it too much may cause (or increase) flickering and decrease
performance due to too many redisplays.  Increasing it too much will
cause the terminal to feel less responsive.  Try to increase this
value if the terminal flickers."
  :type 'number
  :group 'eat-ui
  :group 'eat-eshell)

(defcustom eat-maximum-latency 0.033
  "Minimum display latency in seconds.

Increasing it too much may make the terminal feel less responsive in
case of huge burst of output.  Try to increase this value if the
terminal flickers.  Try to lower the value if the terminal feels less
responsive."
  :type 'number
  :group 'eat-ui
  :group 'eat-eshell)

(defcustom eat-term-name #'eat-term-get-suitable-term-name
  "Value for the `TERM' environment variable.

The value can also be a function.  In that case, the function is
called without any argument and the return value is used as the value.
For example, this can set to `eat-term-get-suitable-term-name' to set
the value according to the number of colors supported by the current
display.

This value is used by terminal programs to identify the terminal."
  :type '(choice
          (string :tag "Value")
          (const :tag "Automatic" eat-term-get-suitable-term-name)
          (function :tag "Function"))
  :group 'eat-term)

;; Upgrading Eat causes `eat-term-terminfo-directory' and
;; `eat-term-shell-integration-directory' to be outdated, so update it
;; if not modified by user (or something else).
(defvar eat--load-file-path nil
  "Path to currently loaded Eat.")

(defvar eat--install-path nil
  "Path to directory where Eat is installed.")

(defvar eat--terminfo-path nil
  "Path to directory where Terminfo databases are installed.")

(defvar eat--shell-integration-path nil
  "Path to directory where shell integration scripts are installed.")

(setq eat--load-file-path (or load-file-name buffer-file-name))

(setq eat--install-path
      (copy-sequence (file-name-directory eat--load-file-path)))

(defvar eat-term-terminfo-directory)
(defvar eat-term-shell-integration-directory)
(let ((old-terminfo-path eat--terminfo-path)
      (old-shell-integration-path eat--shell-integration-path))
  (setq eat--terminfo-path
        (expand-file-name "terminfo" eat--install-path))
  (setq eat--shell-integration-path
        (expand-file-name "integration" eat--install-path))

  (defcustom eat-term-terminfo-directory eat--terminfo-path
    "Directory where required terminfo databases can be found.

This value is used by terminal programs to find the terminfo databases
that describe the capabilities of the terminal."
    :type 'directory
    :group 'eat-term)

  (defcustom eat-term-shell-integration-directory
    eat--shell-integration-path
    "Directory where Eat shell integration scripts can be found.

This value is exposed to terminal programs as
`EAT_SHELL_INTEGRATION_DIR' environment variable."
    :type 'directory
    :group 'eat-ui
    :group 'eat-eshell)

  (when (eq eat-term-terminfo-directory old-terminfo-path)
    (setq eat-term-terminfo-directory eat--terminfo-path))
  (when (eq eat-term-shell-integration-directory
            old-shell-integration-path)
    (setq eat-term-shell-integration-directory
          eat--shell-integration-path)))

(defcustom eat-term-inside-emacs (format "%s,eat" emacs-version)
  "Value for the `INSIDE_EMACS' environment variable."
  :type 'string
  :group 'eat-term)

(defcustom eat-enable-blinking-text nil
  "Non-nil means enable blinking of text with blink attribute.

When non-nil, enable `eat-blink-mode' to enable blinking of text with
blink attribute by default.  You manually toggle `eat-blink-mode' to
toggle this behavior buffer-locally."
  :type 'boolean
  :group 'eat-ui
  :group 'eat-eshell)

(defcustom eat-slow-blink-frequency 2
  "Frequency of blinking of slowly text.

This has an effect only if `eat-blink-mode' is enabled."
  :type 'number
  :group 'eat-ui)

(defcustom eat-fast-blink-frequency 3
  "Frequency of blinking of rapidly text.

This has an effect only if `eat-blink-mode' is enabled."
  :type 'number
  :group 'eat-ui)

(defcustom eat-enable-alternative-display t
  "Non-nil means enable alternative display.

Full screen programs often use alternative display to keep old
contents on display unaltered."
  :type 'boolean
  :group 'eat-term)

(make-obsolete-variable 'eat-enable-alternative-display
                        "don't use it." "0.9" 'set)

(defcustom eat-enable-mouse t
  "Non-nil means enable mouse support.

When non-nil, terminal programs can receive mouse events from Emacs."
  :type 'boolean
  :group 'eat-ui)

(defcustom eat-input-chunk-size 1024
  "Maximum size of chunk of data send at once.

Long inputs send to Eat processes are broken up into chunks of this
size.

If your process is choking on big inputs, try lowering the value."
  :type 'integer
  :group 'eat-ui)

(defface eat-term-bold '((t :inherit bold))
  "Face used to render bold text."
  :group 'eat-term)

(defface eat-term-faint '((t :weight light))
  "Face used to render faint text."
  :group 'eat-term)

(defface eat-term-italic '((t :inherit italic))
  "Face used to render italic text."
  :group 'eat-term)

(defface eat-term-slow-blink '((t :inverse-video t))
  "Face used to render slowly blinking text."
  :group 'eat-term)

(defface eat-term-fast-blink '((t :inverse-video t))
  "Face used to render rapidly blinking text."
  :group 'eat-term)

;; Define color faces.
(let ((face-counter 0))
  (let ((colors '("black" "red" "green" "yellow" "blue" "magenta"
                  "cyan" "white")))
    ;; Basic colors.
    (dolist (color colors)
      (let ((face (intern (format "eat-term-color-%i" face-counter))))
        (custom-declare-face
         face `((t :inherit
                   ,(intern (format (if (eval-when-compile
                                          (>= emacs-major-version 28))
                                        "ansi-color-%s"
                                      "term-color-%s")
                                    color))))
         (format "Face used to render %s color text." color)
         :group 'eat-term)
        (put (intern (format "eat-term-color-%s" color))
             'face-alias face))
      (cl-incf face-counter))
    ;; Bright colors.
    (dolist (color colors)
      (let ((face (intern (format "eat-term-color-%i" face-counter))))
        (custom-declare-face
         face `((t :inherit
                   ,(intern (format (if (eval-when-compile
                                          (>= emacs-major-version 28))
                                        "ansi-color-bright-%s"
                                      "term-color-%s")
                                    color))))
         (format "Face used to render bright %s color text." color)
         :group 'eat-term)
        (put (intern (format "eat-term-color-bright-%s" color))
             'face-alias face))
      (cl-incf face-counter)))
  ;; 256-colors.
  (while (< face-counter 256)
    (let ((color
           (if (>= face-counter 232)
               (format "#%06X"
                       (* #x010101
                          (+ 8 (* 10 (- face-counter 232)))))
             (let ((col (- face-counter 16))
                   (res 0)
                   (frac (* 6 6)))
               (while (<= 1 frac)
                 (setq res (* res #x000100))
                 (let ((color-num (mod (/ col frac) 6)))
                   (unless (zerop color-num)
                     (setq res (+ res #x37 (* #x28 color-num)))))
                 (setq frac (/ frac 6)))
               (format "#%06X" res)))))
      (custom-declare-face
       (intern (format "eat-term-color-%i" face-counter))
       `((t :foreground ,color :background ,color))
       (format "Face used to render text with %i%s color of 256 color\
 palette."
               face-counter
               (or (and (not (<= 11 (% face-counter 100) 13))
                        (nth (% face-counter 10)
                             '(nil "st" "nd" "rd")))
                   "th"))
       :group 'eat-term))
    (cl-incf face-counter)))

(defface eat-term-font-0 '((t))
  "Default font."
  :group 'eat-term)

(put 'eat-term-font-default 'face-alias 'eat-term-font-0)

;; Font faces, 1 to 9 (inclusive).
(cl-loop for counter from 1 to 9
         do (custom-declare-face
             (intern (format "eat-term-font-%i" counter)) '((t))
             (format "Alternative font %i." counter)
             :group 'eat-term))


;;;; Utility Functions.

(defun eat--t-goto-bol (&optional n)
  "Go to the beginning of current line.

With optional argument N, go to the beginning of Nth next line if N is
positive, otherwise go to the beginning of -Nth previous line.  If the
specified position is before `point-min' or after `point-max', go to
that point.

Return the number of lines moved.

Treat LINE FEED (?\\n) as the line delimiter."
  ;; TODO: Comment.
  (setq n (or n 0))
  (cond ((> n 0)
         (let ((moved 0))
           (while (and (< (point) (point-max))
                       (< moved n))
             (and (search-forward "\n" nil 'move)
                  (cl-incf moved)))
           moved))
        ((<= n 0)
         (let ((moved 1))
           (while (and (or (= moved 1)
                           (< (point-min) (point)))
                       (< n moved))
             (cl-decf moved)
             (and (search-backward "\n" nil 'move)
                  (= moved n)
                  (goto-char (match-end 0))))
           moved))))

(defun eat--t-goto-eol (&optional n)
  "Go to the end of current line.

With optional argument N, go to the end of Nth next line if N is
positive, otherwise go to the end of -Nth previous line.  If the
specified position is before `point-min' or after `point-max', go to
that point.

Return the number of lines moved.

Treat LINE FEED (?\\n) as the line delimiter."
  ;; TODO: Comment.
  (setq n (or n 0))
  (cond ((>= n 0)
         (let ((moved -1))
           (while (and (or (= moved -1)
                           (< (point) (point-max)))
                       (< moved n))
             (cl-incf moved)
             (and (search-forward "\n" nil 'move)
                  (= moved n)
                  (goto-char (match-beginning 0))))
           moved))
        ((< n 0)
         (let ((moved 0))
           (while (and (< (point-min) (point))
                       (< n moved))
             (and (search-backward "\n" nil 'move)
                  (cl-decf moved)))
           moved))))

(defun eat--t-bol (&optional n)
  "Return the beginning of current line.

With optional argument N, return a cons cell whose car is the
beginning of Nth next line and cdr is N, if N is positive, otherwise
return a cons cell whose car is the beginning of -Nth previous line
and cdr is N.  If the specified position is before `point-min' or
after `point-max', return a cons cell whose car is that point and cdr
is number of lines that point is away from current line.

Treat LINE FEED (?\\n) as the line delimiter."
  ;; Move to the beginning of line, record the point, and return that
  ;; point and the distance of that point from current line in lines.
  (save-excursion
    ;; `let' is neccessary, we need to evaluate (point) after going to
    ;; `(eat--t-goto-bol N)'.
    (let ((moved (eat--t-goto-bol n)))
      (cons (point) moved))))

(defun eat--t-eol (&optional n)
  "Return the end of current line.

With optional argument N, return a cons cell whose car the end of Nth
next line and cdr is N, if N is positive, otherwise return a cons cell
whose car is the end of -Nth previous line and cdr in N.  If the
specified position is before `point-min' or after `point-max', return
a cons cell whose car is that point and cdr is number of lines that
point is away from current line.

Treat LINE FEED (?\\n) as the line delimiter."
  ;; Move to the beginning of line, record the point, and return that
  ;; point and the distance of that point from current line in lines.
  (save-excursion
    ;; `let' is neccessary, we need to evaluate (point) after going to
    ;; (eat--t-goto-eol N).
    (let ((moved (eat--t-goto-eol n)))
      (cons (point) moved))))

(defun eat--t-col-motion (n)
  "Move to Nth next column.

Go to Nth next column if N is positive, otherwise go to -Nth previous
column.  If the specified position is before `point-min' or after
`point-max', go to that point.

Return the number of columns moved.

Assume all characters occupy a single column."
  ;; Record the current position.
  (let ((start-pos (point)))
    ;; Move to the new position.
    (cond ((> n 0)
           (let ((eol (car (eat--t-eol)))
                 (pos (+ (point) n)))
             (goto-char (min pos eol))))
          ((< n 0)
           (let ((bol (car (eat--t-bol)))
                 (pos (+ (point) n)))
             (goto-char (max pos bol)))))
    ;; Return the distance from the previous position.
    (- (point) start-pos)))

(defun eat--t-current-col ()
  "Return the current column.

Assume all characters occupy a single column."
  ;; We assume that that all characters occupy a single column, so a
  ;; subtraction should work.  For multi-column characters, we add
  ;; extra invisible spaces before the character to make it occupy as
  ;; many character is its width.
  (- (point) (car (eat--t-bol))))

(defun eat--t-goto-col (n)
  "Go to column N.

Return the current column after moving point.

Assume all characters occupy a single column."
  ;; Move to column 0.
  (eat--t-goto-bol)
  ;; Now the target column is N characters away.
  (eat--t-col-motion n))

(defun eat--t-repeated-insert (c n &optional face)
  "Insert character C, N times, using face FACE, if given."
  (insert (if face
              (let ((str (make-string n c)))
                (put-text-property 0 n 'face face str)
                (put-text-property 0 n 'font-lock-face face str)
                str)
            ;; Avoid the `let'.
            (make-string n c))))

(defun eat--t-join-long-line (&optional limit)
  "Join long line once, but don't try to go beyond LIMIT.

For example: \"*foo\\nbar\\nbaz\" is converted to \"foo*bar\\nbaz\",
where `*' indicates point."
  ;; Are we already at the end a part of a long line?
  (unless (get-text-property (point) 'eat--t-wrap-line)
    ;; Find the next end of a part of a long line.
    (goto-char (or (next-single-property-change
                    (point) 'eat--t-wrap-line nil limit)
                   limit (point-max))))
  ;; Remove the newline.
  (when (< (point) (or limit (point-max)))
    (1value (cl-assert (1value (= (1value (char-after)) ?\n))))
    (delete-char 1)))

(defun eat--t-break-long-line (threshold)
  "Break a line longer than THRESHOLD once.

For example: when THRESHOLD is 3, \"*foobarbaz\" is converted to
\"foo\\n*barbaz\", where `*' indicates point."
  (let ((loop t))
    ;; Find a too long line.
    (while (and loop (< (point) (point-max)))
      ;; Go to the threshold column.
      (eat--t-goto-col threshold)
      ;; Are we at the end of line?
      (if (eq (char-after) ?\n)
          ;; We are already at the end of line, so move to the next
          ;; line and start from the beginning.
          (forward-char)
        ;; The next character is not a newline, so we must be at a
        ;; long line, or we are the end of the accessible part of the
        ;; buffer.  Whatever the case, we break the loop, and if it is
        ;; a long line, we break the line.
        (setq loop nil)
        (unless (= (point) (point-max))
          (insert-before-markers
           #("\n" 0 1 (eat--t-wrap-line t))))))))


;;;; Emulator.

(cl-defstruct (eat--t-cur
               (:constructor eat--t-make-cur)
               (:copier eat--t-copy-cur))
  "Structure describing cursor position."
  (position nil :documentation "Position of cursor.")
  (y 1 :documentation "Y coordinate of cursor.")
  (x 1 :documentation "X coordinate of cursor.")
  (sixel-x 0 :documentation "X coordinate of Sixel cursor.")
  (sixel-y 0 :documentation "Y coordinate of Sixel cursor.")
  (sixel-beg nil :documentation "Cons cell of current sixel line."))

(cl-defstruct (eat--t-disp
               (:constructor eat--t-make-disp)
               (:copier eat--t-copy-disp))
  "Structure describing the display."
  (begin nil :documentation "Beginning of visible display.")
  (width 80 :documentation "Width of display.")
  (height 24 :documentation "Height of display.")
  (cursor nil :documentation "Cursor.")
  (saved-cursor
   (1value (eat--t-make-cur))
   :documentation "Saved cursor.")
  (old-begin
   nil
   :documentation
   "Beginning of visible display during last Eat redisplay."))

(cl-defstruct (eat--t-face
               (:constructor eat--t-make-face)
               (:copier eat--t-copy-face))
  "Structure describing the display attributes to use."
  (face nil :documentation "Face to use.")
  (fg nil :documentation "Foreground color.")
  (bg nil :documentation "Background color.")
  (intensity nil :documentation "Intensity face, or nil.")
  (italic nil :documentation "Non-nil means use italic face.")
  (underline nil :documentation "Non-nil means underline text.")
  (underline-color nil :documentation "Underline color.")
  (crossed nil :documentation "Non-nil means strike-through text.")
  (conceal nil :documentation "Non-nil means invisible text.")
  (inverse nil :documentation "Non-nil means inverse colors.")
  (blink nil :documentation "Blink face, or nil.")
  (font 'eat-term-font-0 :documentation "Current font face."))

(cl-defstruct (eat--t-term
               (:constructor eat--t-make-term)
               (:copier eat--t-copy-term))
  "Structure describing a terminal."
  (buffer nil :documentation "The buffer of terminal.")
  (begin nil :documentation "Beginning of terminal.")
  (end nil :documentation "End of terminal area.")
  (title "" :documentation "The title of the terminal.")
  (bell-fn
   (1value #'ignore)
   :documentation "Function to ring the bell.")
  (input-fn
   (1value #'ignore)
   :documentation "Function to send input.")
  (set-cursor-fn
   (1value #'ignore)
   :documentation "Function to set cursor.")
  (manipulate-selection-fn
   (1value #'ignore)
   :documentation "Function to manipulate selection.")
  (grab-mouse-fn
   (1value #'ignore)
   :documentation "Function to grab mouse.")
  (set-focus-ev-mode-fn
   (1value #'ignore)
   :documentation "Function to set focus event mode.")
  (set-title-fn
   (1value #'ignore)
   :documentation "Function to set the title.")
  (set-cwd-fn
   (1value #'ignore)
   :documentation "Function to set the current working directory.")
  (ui-cmd-fn
   (1value #'ignore)
   :documentation "Function to handle UI command sequence.")
  (parser-state nil :documentation "State of parser.")
  (scroll-begin 1 :documentation "First line of scroll region.")
  (scroll-end 24 :documentation "Last line of scroll region.")
  (display nil :documentation "The display.")
  (main-display nil :documentation "Main display.

Nil when not in alternative display mode.")
  (face
   (1value (eat--t-make-face))
   :documentation "Display attributes.")
  (auto-margin t :documentation "State of auto margin mode.")
  (ins-mode nil :documentation "State of insert mode.")
  (charset
   (copy-tree '(g0 . ((g0 . us-ascii)
                      (g1 . us-ascii)
                      (g2 . us-ascii)
                      (g3 . us-ascii))))
   :documentation "Current character set.")
  (cur-state :block :documentation "Current state of cursor.")
  (cur-visible-p t :documentation "Is the cursor visible?")
  (saved-face
   (1value (eat--t-make-face))
   :documentation "Saved SGR attributes.")
  (bracketed-yank nil :documentation "State of bracketed yank mode.")
  (keypad-mode nil :documentation "State of keypad mode.")
  (mouse-mode nil :documentation "Current mouse mode.")
  (mouse-pressed nil :documentation "Pressed mouse buttons.")
  (mouse-encoding nil :documentation "Current mouse event encoding.")
  (focus-event-mode nil :documentation "Whether to send focus event.")
  (cut-buffers
   (1value (make-vector 8 nil))
   :documentation "Cut buffers.")
  (sixel-buffer
   (let ((pair (cons (cons 0 (make-vector 1000 nil)) nil)))
     (setf (cdr pair) (cons pair pair))
     pair)
   :documentation "Buffer to hold Sixel data.")
  (sixel-buffer-size 1 :documentation "Line count in Sixel buffer.")
  (sixel-palette
   (copy-sequence (make-vector 256 nil))
   :documentation "Sixel color registers.")
  (sixel-color 0 :documentation "Current Sixel color register.")
  (sixel-render-format
   'background
   :documentation "Format to render Sixel images in.")
  (sixel-image-extra-props
   nil
   :documentation "Extra properties of images used to display Sixel.")
  (sixel-scroll-mode t :documentation "Whether to auto-scroll.")
  (sixel-initial-cursor-pos
   '(1 . 1)
   :documentation "Initial position of cursor before entering Sixel.")
  (char-width 1 :documentation "Width of each character in pixel.")
  (char-height 1 :documentation "Height of each character in pixel.")
  ;; NOTE: Change the default value of parameters when changing this.
  (bold-face 'eat-term-bold :documentation "Face for bold text.")
  (faint-face 'eat-term-faint :documentation "Face for faint text.")
  (italic-face 'eat-term-italic :documentation "Face for slant text.")
  (slow-blink-face 'eat-term-slow-blink :documentation "Slow blink.")
  (fast-blink-face 'eat-term-fast-blink :documentation "Fast blink.")
  (color-faces
   (copy-sequence
    (eval-when-compile
      (vconcat
       (cl-loop for i from 0 to 255
                collect (intern (format "eat-term-color-%i" i))))))
   :documentation "Faces for colors.")
  (font-faces
   (copy-sequence
    (eval-when-compile
      (vconcat
       (cl-loop for i from 0 to 9
                collect (intern (format "eat-term-font-%i" i))))))
   :documentation "Faces for fonts.")
  (params
   (copy-hash-table
    (eval-when-compile
      (let ((tbl (make-hash-table :test 'eq)))
        (puthash 'input-function #'ignore tbl)
        (puthash 'ring-bell-function #'ignore tbl)
        (puthash 'set-cursor-function #'ignore tbl)
        (puthash 'grab-mouse-function #'ignore tbl)
        (puthash 'grab-focus-events-function #'ignore tbl)
        (puthash 'manipulate-selection-function #'ignore tbl)
        (puthash 'set-title-function #'ignore tbl)
        (puthash 'set-cwd-function #'ignore tbl)
        (puthash 'ui-command-function #'ignore tbl)
        (puthash 'char-dimensions '(1 . 1) tbl)
        (puthash 'sixel-render-format 'background tbl)
        (puthash 'bold-face 'eat-term-bold tbl)
        (puthash 'faint-face 'eat-term-faint tbl)
        (puthash 'italic-face 'eat-term-italic tbl)
        (puthash 'slow-blink-face 'eat-term-slow-blink tbl)
        (puthash 'fast-blink-face 'eat-term-fast-blink tbl)
        (cl-loop
         for i from 0 to 255
         do (puthash (intern (format "color-%i-face" i))
                     (intern (format "eat-term-color-%i" i)) tbl))
        (cl-loop
         for i from 0 to 9
         do (puthash (intern (format "font-%i-face" i))
                     (intern (format "eat-term-font-%i" i)) tbl))
        tbl)))
   :documentation "Alist of terminal parameters."))

(defvar eat--t-term nil
  "The current terminal.

Don't `set' it, bind it to a value with `let'.")

(defun eat--t-reset ()
  "Reset terminal."
  (let ((disp (eat--t-term-display eat--t-term)))
    ;; Reset most of the things to their respective default values.
    (setf (eat--t-term-parser-state eat--t-term) nil)
    (setf (eat--t-disp-begin disp) (point-min-marker))
    (setf (eat--t-disp-old-begin disp) (point-min-marker))
    (setf (eat--t-disp-cursor disp)
          (eat--t-make-cur :position (point-min-marker)))
    (setf (eat--t-disp-saved-cursor disp) (eat--t-make-cur))
    (setf (eat--t-term-scroll-begin eat--t-term) 1)
    (setf (eat--t-term-scroll-end eat--t-term)
          (eat--t-disp-height disp))
    (setf (eat--t-term-main-display eat--t-term) nil)
    (setf (eat--t-term-face eat--t-term) (eat--t-make-face))
    (setf (eat--t-term-auto-margin eat--t-term) t)
    (setf (eat--t-term-ins-mode eat--t-term) nil)
    (setf (eat--t-term-charset eat--t-term)
          '(g0 (g0 . us-ascii)
               (g1 . dec-line-drawing)
               (g2 . dec-line-drawing)
               (g3 . dec-line-drawing)))
    (setf (eat--t-term-saved-face eat--t-term) (eat--t-make-face))
    (setf (eat--t-term-bracketed-yank eat--t-term) nil)
    (setf (eat--t-term-cur-state eat--t-term) :block)
    (setf (eat--t-term-cur-visible-p eat--t-term) t)
    (setf (eat--t-term-title eat--t-term) "")
    (setf (eat--t-term-keypad-mode eat--t-term) nil)
    (setf (eat--t-term-mouse-mode eat--t-term) nil)
    (setf (eat--t-term-mouse-encoding eat--t-term) nil)
    (setf (eat--t-term-focus-event-mode eat--t-term) nil)
    (setf (eat--t-term-sixel-scroll-mode eat--t-term) t)
    ;; Clear everything.
    (delete-region (point-min) (point-max))
    ;; Inform the UI about our new state.
    (funcall (eat--t-term-grab-mouse-fn eat--t-term) eat--t-term nil)
    (funcall (eat--t-term-set-focus-ev-mode-fn eat--t-term)
             eat--t-term nil)
    (funcall (eat--t-term-set-title-fn eat--t-term) eat--t-term "")
    (funcall (eat--t-term-set-cursor-fn eat--t-term) eat--t-term
             :block)))

(defun eat--t-cur-right (&optional n)
  "Move cursor N columns right.

N default to 1.  If N is out of range, place cursor at the edge of
display."
  (let* ((disp (eat--t-term-display eat--t-term))
         (cursor (eat--t-disp-cursor disp)))
    ;; If N is less than 1, set N to 1.  If N is more than the number
    ;; of available columns on the right side, set N to the maximum
    ;; possible value.
    (setq n (min (- (eat--t-disp-width disp) (eat--t-cur-x cursor))
                 (max (or n 1) 1)))
    ;; N is non-zero in most cases, except at the edge of display.
    (unless (zerop n)
      ;; Move to the Nth next column, use spaces to reach that column
      ;; if needed.
      (eat--t-repeated-insert ?\s (- n (eat--t-col-motion n)))
      (cl-incf (eat--t-cur-x cursor) n))))

(defun eat--t-cur-left (&optional n)
  "Move cursor N columns left.

N default to 1.  If N is out of range, place cursor at the edge of
display."
  (let* ((disp (eat--t-term-display eat--t-term))
         (cursor (eat--t-disp-cursor disp)))
    ;; If N is less than 1, set N to 1.  If N is more than the number
    ;; of available columns on the left side, set N to the maximum
    ;; possible value.
    (setq n (min (1- (eat--t-cur-x cursor)) (max (or n 1) 1)))
    ;; N is non-zero in most cases, except at the edge of display.
    (unless (zerop n)
      ;; Move to the Nth previous column.
      (cl-assert (1value (>= (eat--t-current-col) n)))
      (backward-char n)
      (cl-decf (eat--t-cur-x cursor) n))))

(defun eat--t-cur-horizontal-abs (&optional n)
  "Move cursor to Nth column on current line.

N default to 1.  If N is out of range, place cursor at the edge of
display."
  (let* ((disp (eat--t-term-display eat--t-term))
         (cursor (eat--t-disp-cursor disp)))
    ;; If N is out of range, bring it within the bounds of range.
    (setq n (min (max (or n 1) 1) (eat--t-disp-width disp)))
    ;; Depending on the current position of cursor, move right or
    ;; left.
    (cond ((< (eat--t-cur-x cursor) n)
           (eat--t-cur-right (- n (eat--t-cur-x cursor))))
          ((< n (eat--t-cur-x cursor))
           (eat--t-cur-left (- (eat--t-cur-x cursor) n))))))

(defun eat--t-beg-of-next-line (n)
  "Move to beginning of Nth next line."
  (let* ((disp (eat--t-term-display eat--t-term))
         (cursor (eat--t-disp-cursor disp)))
    ;; If N is less than 1, set N to 1.  If N is more than the number
    ;; of available lines below, set N to the maximum possible value.
    (setq n (min (- (eat--t-disp-height disp) (eat--t-cur-y cursor))
                 (max (or n 1) 1)))
    ;; N is non-zero in most cases, except at the edge of display.
    ;; Whatever the case, we move to the beginning of line.
    (if (zerop n)
        (1value (eat--t-goto-bol))
      ;; Move to the Nth next line, use newlines to reach that line if
      ;; needed.
      (eat--t-repeated-insert ?\n (- n (eat--t-goto-bol n)))
      (cl-incf (eat--t-cur-y cursor) n))
    (1value (setf (eat--t-cur-x cursor) 1))))

(defun eat--t-beg-of-prev-line (n)
  "Move to beginning of Nth previous line."
  (let* ((disp (eat--t-term-display eat--t-term))
         (cursor (eat--t-disp-cursor disp)))
    ;; If N is less than 1, set N to 1.  If N is more than the number
    ;; of available lines above, set N to the maximum possible value.
    (setq n (min (1- (eat--t-cur-y cursor)) (max (or n 1) 1)))
    ;; Move to the beginning Nth previous line.  Even if there are no
    ;; line above, move to the beginning of the line.
    (eat--t-goto-bol (- n))
    (cl-decf (eat--t-cur-y cursor) n)
    (1value (setf (eat--t-cur-x cursor) 1))))

(defun eat--t-cur-down (&optional n)
  "Move cursor N lines down.

N default to 1.  If N is out of range, place cursor at the edge of
display."
  (let ((x (eat--t-cur-x (eat--t-disp-cursor
                          (eat--t-term-display eat--t-term)))))
    ;; Move to the beginning of target line.
    (eat--t-beg-of-next-line n)
    ;; If the cursor wasn't at column one, move the cursor to the
    ;; cursor to that column.
    (unless (= x 1)
      (eat--t-cur-right (1- x)))))

(defun eat--t-cur-up (&optional n)
  "Move cursor N lines up.

N default to 1.  If N is out of range, place cursor at the edge of
display."
  (let ((x (eat--t-cur-x (eat--t-disp-cursor
                          (eat--t-term-display eat--t-term)))))
    ;; Move to the beginning of target line.
    (eat--t-beg-of-prev-line n)
    ;; If the cursor wasn't at column one, move the cursor to the
    ;; cursor to that column.
    (unless (= x 1)
      (eat--t-cur-right (1- x)))))

(defun eat--t-cur-vertical-abs (&optional n)
  "Move cursor to Nth line on display.

N default to 1.  If N is out of range, place cursor at the edge of
display."
  (let* ((disp (eat--t-term-display eat--t-term))
         (cursor (eat--t-disp-cursor disp)))
    ;; If N is out of range, bring it within the bounds of range.
    (setq n (min (max (or n 1) 1) (eat--t-disp-height disp)))
    ;; Depending on the current position of cursor, move downward or
    ;; upward.
    (cond ((< (eat--t-cur-y cursor) n)
           (eat--t-cur-down (- n (eat--t-cur-y cursor))))
          ((< n (eat--t-cur-y cursor))
           (eat--t-cur-up (- (eat--t-cur-y cursor) n))))))

(defun eat--t-scroll-up (&optional n as-side-effect)
  "Scroll up N lines, preserving cursor position.

N default to 1.  By default, don't change current line and current
column, but if AS-SIDE-EFFECT is given and non-nil, assume that
scrolling is triggered as a side effect of some other control function
and don't move the point relative to the text and change current line
accordingly."
  (let ((disp (eat--t-term-display eat--t-term))
        (scroll-begin (eat--t-term-scroll-begin eat--t-term))
        (scroll-end (eat--t-term-scroll-end eat--t-term)))
    ;; N shouldn't be more more than the number of lines in scroll
    ;; region.
    (setq n (min (max (or n 1) 0) (1+ (- scroll-end scroll-begin))))
    ;; Make sure that N is positive.
    (unless (zerop n)
      ;; Try to not point relative to the text.
      (save-excursion
        (goto-char (eat--t-disp-begin disp))
        ;; Move to the beginning of scroll region.
        (eat--t-goto-bol (1- scroll-begin))
        ;; If the first line on display isn't in scroll region or
        ;; if this is the alternative display, delete text.
        (if (or (eat--t-term-main-display eat--t-term)
                (> scroll-begin 1))
            (delete-region (point) (car (eat--t-bol n)))
          ;; Otherwise, send the text to the scrollback area by
          ;; advancing the display beginning marker.
          (eat--t-goto-bol n)
          ;; Make sure we're at the beginning of a line, because we
          ;; might be at `point-max'.
          (unless (or (= (point) (point-min))
                      (= (char-before) ?\n))
            (insert ?\n))
          (set-marker (eat--t-disp-begin disp) (point)))
        ;; Is the last line on display in scroll region?
        (when (< scroll-end (eat--t-disp-width disp))
          ;; No, it isn't.
          ;; Go to the end of scroll region (before deleting or moving
          ;; texts).
          (eat--t-goto-bol (- (1+ (- scroll-end scroll-begin)) n))
          ;; If there is anything after the scroll region, insert
          ;; newlines to keep that text unmoved.
          (when (< (point) (point-max))
            (eat--t-repeated-insert ?\n n))))
      ;; Recalculate point if needed.
      (let* ((cursor (eat--t-disp-cursor disp))
             (recalc-point
              (<= scroll-begin (eat--t-cur-y cursor) scroll-end)))
        ;; If recalc-point is non-nil, and AS-SIDE-EFFECT is non-nil,
        ;; update cursor position so that it is unmoved relative to
        ;; surrounding text and reconsider point recalculation.
        (when (and recalc-point as-side-effect)
          (setq recalc-point (< (- (eat--t-cur-y cursor) n)
                                scroll-begin))
          (setf (eat--t-cur-y cursor)
                (max (- (eat--t-cur-y cursor) n) scroll-begin)))
        (when recalc-point
          ;; Recalculate point.
          (let ((y (eat--t-cur-y cursor))
                (x (eat--t-cur-x cursor)))
            (eat--t-goto 1 1)
            (eat--t-goto y x)))))))

(defun eat--t-scroll-down (&optional n)
  "Scroll down N lines, preserving cursor position.

N defaults to 1."
  (let ((disp (eat--t-term-display eat--t-term))
        (scroll-begin (eat--t-term-scroll-begin eat--t-term))
        (scroll-end (eat--t-term-scroll-end eat--t-term)))
    ;; N shouldn't be more more than the number of lines in scroll
    ;; region.
    (setq n (min (max (or n 1) 0) (1+ (- scroll-end scroll-begin))))
    ;; Make sure that N is positive.
    (unless (zerop n)
      ;; Move to the beginning of scroll region.
      (goto-char (eat--t-disp-begin disp))
      (eat--t-goto-bol (1- scroll-begin))
      ;; Insert newlines to push text downwards.
      (eat--t-repeated-insert ?\n n)
      ;; Go to the end scroll region (after inserting newlines).
      (eat--t-goto-eol (- (1+ (- scroll-end scroll-begin)) (1+ n)))
      ;; Delete the text that was pushed out of scroll region.
      (when (< (point) (point-max))
        (delete-region (point) (car (eat--t-eol n))))
      ;; The cursor mustn't move, so we have to recalculate point.
      (let* ((cursor (eat--t-disp-cursor disp))
             (y (eat--t-cur-y cursor))
             (x (eat--t-cur-x cursor)))
        (eat--t-goto 1 1)
        (eat--t-goto y x)))))

(defun eat--t-goto (&optional y x)
  "Go to Xth column of Yth line of display.

Y and X default to 1.  Y and X are one-based.  If Y and/or X are out
of range, place cursor at the edge of display."
  ;; Important special case: if Y and X are both one, move to the
  ;; display beginning.
  (if (and (or (not y) (eql y 1))
           (or (not x) (eql x 1)))
      (let* ((disp (eat--t-term-display eat--t-term))
             (cursor (eat--t-disp-cursor disp)))
        (goto-char (eat--t-disp-begin disp))
        (1value (setf (eat--t-cur-y cursor) 1
                      (eat--t-cur-x cursor) 1)))
    ;; Move to column one, go to Yth line and move to Xth column.
    ;; REVIEW: We move relative to cursor position, which faster for
    ;; positions near the point (usually the case), but slower for
    ;; positions far away from the point.  There are only two cursor
    ;; positions whose exact position is known beforehand, the cursor
    ;; (whose position is (point)) and (1, 1) (the display beginning).
    ;; There are almost always some points which are at more distance
    ;; from current position than from the display beginning (the only
    ;; exception is when the cursor is at the display beginning).  So
    ;; first moving to the display beginning and then moving to those
    ;; point will be faster than moving from cursor (except a tiny
    ;; (perhaps negligible) overhead of `goto-char').  What we don't
    ;; have is a formula the calculate the distance between two
    ;; positions.
    (eat--t-cur-horizontal-abs 1)
    (eat--t-cur-vertical-abs y)
    (eat--t-cur-horizontal-abs x)))

(defun eat--t-enable-auto-margin ()
  "Enable automatic margin."
  ;; Set the automatic margin flag to t, the rest of code will take
  ;; care of the effects.
  (1value (setf (eat--t-term-auto-margin eat--t-term) t)))

(defun eat--t-disable-auto-margin ()
  "Disable automatic margin."
  ;; Set the automatic margin flag to nil, the rest of code will take
  ;; care of the effects.
  (1value (setf (eat--t-term-auto-margin eat--t-term) nil)))

(defun eat--t-set-charset (slot charset)
  "SLOT's character set to CHARSET."
  (setf (alist-get slot (cdr (eat--t-term-charset eat--t-term)))
        charset))

(defun eat--t-change-charset (charset)
  "Change character set to CHARSET.

CHARSET should be one of `g0', `g1', `g2' and `g3'."
  (cl-assert (memq charset '(g0 g1 g2 g3)))
  (setf (car (eat--t-term-charset eat--t-term)) charset))

(defun eat--t-move-before-to-safe ()
  "Move to a safe position before point.  Return how much moved.

If the current position is safe, do nothing and return 0.

Safe position is the position that's not on a multi-column wide
character or its the internal invisible spaces."
  (if (and (not (bobp))
           ;; Is the current position unsafe?
           (get-text-property (1- (point)) 'eat--t-invisible-space))
      (let ((start-pos (point)))
        ;; Move to the safe position.
        (goto-char (or (previous-single-property-change
                        (point) 'eat--t-invisible-space)
                       (point-min)))
        (cl-assert
         (1value (or (bobp)
                     (null (get-text-property
                            (1- (point)) 'eat--t-invisible-space)))))
        (- start-pos (point)))
    0))

(defun eat--t-make-pos-safe ()
  "If the position isn't safe, make it safe by replacing with spaces."
  (let ((moved (eat--t-move-before-to-safe)))
    (unless (zerop moved)
      (let ((width (get-text-property
                    (point) 'eat--t-char-width)))
        (cl-assert width)
        (delete-region (point) (+ (point) width))
        (eat--t-repeated-insert
         ?\s width (eat--t-face-face
                    (eat--t-term-face eat--t-term)))
        (backward-char (- width moved))))))

(defun eat--t-fix-partial-multi-col-char (&optional preserve-face)
  "Replace any partial multi-column character with spaces.

If PRESERVE-FACE is non-nil, preserve original face."
  (let ((face (if preserve-face
                  (get-char-property (point) 'face)
                (eat--t-face-face
                 (eat--t-term-face eat--t-term)))))
    (if (get-text-property (point) 'eat--t-invisible-space)
        (let ((start-pos (point))
              (count nil))
          (goto-char (or (next-single-property-change
                          (point) 'eat--t-invisible-space)
                         (point-max)))
          (setq count (- (1+ (point)) start-pos))
          ;; Make sure we really overwrote the character
          ;; partially.
          (when (< count (get-text-property
                          (point) 'eat--t-char-width))
            (delete-region start-pos (1+ (point)))
            (eat--t-repeated-insert ?\s count face))
          (goto-char start-pos))
      ;; Detect the case where we have deleted all the invisible
      ;; spaces before, but not the multi-column character itself.
      (when-let* (((not (eobp)))
                  (w (get-text-property (point) 'eat--t-char-width))
                  ((> w 1)))
        ;; `delete-char' also works, but it does more checks, so
        ;; hopefully this will be faster.
        (delete-region (point) (1+ (point)))
        (insert (propertize " " 'face face 'font-lock-face face))
        (backward-char)))))

(defconst eat--t-dec-line-drawing-chars
  (eval-and-compile
    (let ((alist '((?+ . ?→)
                   (?, . ?←)
                   (?- . ?↑)
                   (?. . ?↓)
                   (?0 . ?█)
                   (?\` . ?�)
                   (?a . ?▒)
                   (?b . ?␉)
                   (?c . ?␌)
                   (?d . ?␍)
                   (?e . ?␊)
                   (?f . ?°)
                   (?g . ?±)
                   (?h . ?░)
                   (?i . ?#)
                   (?j . ?┘)
                   (?k . ?┐)
                   (?l . ?┌)
                   (?m . ?└)
                   (?n . ?┼)
                   (?o . ?⎺)
                   (?p . ?⎻)
                   (?q . ?─)
                   (?r . ?⎼)
                   (?s . ?⎽)
                   (?t . ?├)
                   (?u . ?┤)
                   (?v . ?┴)
                   (?w . ?┬)
                   (?x . ?│)
                   (?y . ?≤)
                   (?z . ?≥)
                   (?{ . ?π)
                   (?| . ?≠)
                   (?} . ?£)
                   (?~ . ?•)))
          (table (make-hash-table :purecopy t)))
      (dolist (pair alist)
        (puthash (car pair) (cdr pair) table))
      table))
  "Hash table for DEC Line Drawing charset.

The key is the output character from client, and value of the
character to actually show.")

(defun eat--t-write (str &optional beg end)
  "Write STR from BEG to END on display."
  (setq beg (or beg 0))
  (setq end (or end (length str)))
  (let* ((disp (eat--t-term-display eat--t-term))
         (cursor (eat--t-disp-cursor disp))
         (scroll-end (eat--t-term-scroll-end eat--t-term))
         (charset
          (alist-get (car (eat--t-term-charset eat--t-term))
                     (cdr (eat--t-term-charset eat--t-term))))
         (face (eat--t-face-face (eat--t-term-face eat--t-term)))
         ;; Alist of indices and width of multi-column characters.
         (multi-col-char-indices nil)
         (inserted-till beg))
    (cl-assert charset)
    ;; Find all the multi-column wide characters in ST; hopefully it
    ;; won't slow down showing plain ASCII.
    (setq multi-col-char-indices
          (cl-loop for i from beg to (1- end)
                   when (/= (char-width (aref str i)) 1)
                   collect (cons i (char-width (aref str i)))))
    ;; If the position isn't safe, replace the multi-column
    ;; character with spaces to make it safe.
    (eat--t-make-pos-safe)
    ;; TODO: Comment.
    ;; REVIEW: This probably needs to be updated.
    (while (< inserted-till end)
      ;; Insert STR, and record the width of STR inserted
      ;; successfully.
      (let ((ins-count
             (named-let write
                 ((max (min (- (eat--t-disp-width disp)
                               (1- (eat--t-cur-x cursor)))
                            (+ (- end inserted-till)
                               (cl-loop
                                for p in multi-col-char-indices
                                sum (1- (cdr p))))))
                  (written 0))
               (let* ((next-multi-col (car multi-col-char-indices))
                      (end (+ inserted-till max))
                      (e (if next-multi-col
                             ;; Exclude the multi-column character.
                             (min (car next-multi-col) end)
                           end))
                      (wrote (- e inserted-till)))
                 (cl-assert (>= wrote 0))
                 (let ((s (substring str inserted-till e)))
                   ;; Convert STR to Unicode according to the
                   ;; current character set.
                   (pcase-exhaustive charset
                     ;; For `us-ascii', the default, no conversion
                     ;; is necessary.
                     ('us-ascii)
                     ;; `dec-line-drawing' contains various
                     ;; characters useful for drawing line diagram,
                     ;; so it is a must.  This is also possible
                     ;; with `us-ascii', thanks to Unicode, but the
                     ;; character set `dec-line-drawing' is usually
                     ;; less expensive in terms of bytes needed to
                     ;; transfer than `us-ascii'.
                     ('dec-line-drawing
                      (dotimes (i (length s))
                        (when-let*
                            ((r (gethash
                                 (aref s i)
                                 eat--t-dec-line-drawing-chars)))
                          (aset s i r)))))
                   ;; Add face.
                   (put-text-property 0 (length s) 'face face s)
                   (put-text-property
                    0 (length s) 'font-lock-face face s)
                   (insert s))
                 (setq inserted-till e)
                 (if (or (null next-multi-col)
                         (< (- end e) (cdr next-multi-col)))
                     ;; Either everything is done, or we reached
                     ;; the limit.
                     (+ written wrote)
                   ;; There are many characters which are too
                   ;; narrow for `char-width' to return 1.  XTerm,
                   ;; Kitty and St seems to ignore them, so we too.
                   (if (zerop (cdr next-multi-col))
                       (cl-incf inserted-till)
                     (insert
                      ;; Make sure the multi-column character
                      ;; occupies the same number of characters as
                      ;; its width.
                      (propertize
                       (make-string (1- (cdr next-multi-col)) ?\s)
                       'invisible t 'face face 'font-lock-face face
                       'eat--t-invisible-space t
                       'eat--t-char-width (cdr next-multi-col))
                      ;; Now insert the multi-column character.
                      (propertize
                       (substring str inserted-till
                                  (cl-incf inserted-till))
                       'face face 'font-lock-face face
                       'eat--t-char-width (cdr next-multi-col))))
                   (setf multi-col-char-indices
                         (cdr multi-col-char-indices))
                   (write (- max wrote (cdr next-multi-col))
                          (+ written wrote
                             (cdr next-multi-col))))))))
        (cl-incf (eat--t-cur-x cursor) ins-count)
        (if (eat--t-term-ins-mode eat--t-term)
            (delete-region
             (save-excursion
               (eat--t-col-motion (- (eat--t-disp-width disp)
                                     (1- (eat--t-cur-x cursor))))
               ;; Make sure the point is safe.
               (eat--t-move-before-to-safe)
               (point))
             (car (eat--t-eol)))
          (delete-region (point) (min (+ ins-count (point))
                                      (car (eat--t-eol))))
          ;; Replace any partially-overwritten character with
          ;; spaces.
          (eat--t-fix-partial-multi-col-char))
        (when (> (eat--t-cur-x cursor) (eat--t-disp-width disp))
          (if (not (eat--t-term-auto-margin eat--t-term))
              (eat--t-cur-left 1)
            (when (< inserted-till end)
              (when (= (eat--t-cur-y cursor) scroll-end)
                (eat--t-scroll-up 1 'as-side-effect))
              (if (= (eat--t-cur-y cursor) scroll-end)
                  (eat--t-carriage-return)
                (if (= (point) (point-max))
                    (insert #("\n" 0 1 (eat--t-wrap-line t)))
                  (put-text-property (point) (1+ (point))
                                     'eat--t-wrap-line t)
                  (forward-char))
                (1value (setf (eat--t-cur-x cursor) 1))
                (cl-incf (eat--t-cur-y cursor))))))))))

(defun eat--t-horizontal-tab (&optional n)
  "Go to the Nth next tabulation stop.

N default to 1."
  ;; N must be positive.
  (setq n (max (or n 1) 1))
  (let* ((disp (eat--t-term-display eat--t-term))
         (cursor (eat--t-disp-cursor disp)))
    ;; Do some math calculate the distance of the Nth next tabulation
    ;; stop from cursor, and go there.
    (eat--t-cur-right (+ (- 8 (mod (1- (eat--t-cur-x cursor)) 8))
                         (* (1- n) 8)))))

(defun eat--t-horizontal-backtab (&optional n)
  "Go to the Nth previous tabulation stop.

N default to 1."
  ;; N must be positive.
  (setq n (max (or n 1) 1))
  (let* ((disp (eat--t-term-display eat--t-term))
         (cursor (eat--t-disp-cursor disp)))
    ;; Do some math calculate the distance of the Nth next tabulation
    ;; stop from cursor, and go there.
    (eat--t-cur-left (+ (1+ (mod (- (eat--t-cur-x cursor) 2) 8))
                        (* (1- n) 8)))))

(defun eat--t-index ()
  "Go to the next line preserving column, scrolling if necessary."
  (let* ((disp (eat--t-term-display eat--t-term))
         (cursor (eat--t-disp-cursor disp))
         (scroll-end (eat--t-term-scroll-end eat--t-term))
         ;; Are we inside scroll region?
         (in-scroll-region (<= (eat--t-cur-y cursor) scroll-end)))
    ;; If this is the last line (of the scroll region or the display),
    ;; scroll up, otherwise move cursor downward.
    (if (= (if in-scroll-region scroll-end (eat--t-disp-height disp))
           (eat--t-cur-y cursor))
        (eat--t-scroll-up 1)
      (eat--t-cur-down 1))))

(defun eat--t-carriage-return ()
  "Go to column one."
  (eat--t-cur-horizontal-abs 1))

(defun eat--t-line-feed ()
  "Go to the first column of the next line, scrolling if necessary."
  (let* ((disp (eat--t-term-display eat--t-term))
         (cursor (eat--t-disp-cursor disp))
         (scroll-end (eat--t-term-scroll-end eat--t-term))
         ;; Are we inside scroll region?
         (in-scroll-region (<= (eat--t-cur-y cursor) scroll-end)))
    ;; If we are at the very end of the terminal, we might have some
    ;; optimizations.
    (if (= (point) (point-max))
        ;; If the cursor is above the last line of the scroll region
        ;; (or the display, if we are outside the scroll region), we
        ;; can simply insert a newline and update the cursor position.
        (if (/= (if in-scroll-region
                    scroll-end
                  (eat--t-disp-height disp))
                (eat--t-cur-y cursor))
            (progn
              (insert ?\n)
              (setf (eat--t-cur-x cursor) 1)
              (cl-incf (eat--t-cur-y cursor)))
          ;; This is the last line.  We need to scroll up.
          (eat--t-scroll-up 1 'as-side-effect)
          ;; If we're still at the last line (only happens when the
          ;; display has only a single line), go to column one of it.
          (if (= (if in-scroll-region
                     scroll-end
                   (eat--t-disp-height disp))
                 (eat--t-cur-y cursor))
              (eat--t-carriage-return)
            ;; If we are somehow moved from the end of terminal,
            ;; `eat--t-beg-of-next-line' is the best option.
            (if (/= (point) (point-max))
                (eat--t-beg-of-next-line 1)
              ;; We are still at the end!  We can can simply insert a
              ;; newline and update the cursor position.
              (insert ?\n)
              (setf (eat--t-cur-x cursor) 1)
              (cl-incf (eat--t-cur-y cursor)))))
      ;; We are not at the end of terminal.  But we still have a last
      ;; chance.  `eat--t-beg-of-next-line' is usually faster than
      ;; `eat--t-carriage-return' followed by `eat--t-index', so if
      ;; there is at least a single line (in the scroll region, if the
      ;; cursor in the scroll region, otherwise in the display)
      ;; underneath the cursor, we can use `eat--t-beg-of-next-line'.
      (if (/= (if in-scroll-region
                  scroll-end
                (eat--t-disp-height disp))
              (eat--t-cur-y cursor))
          (eat--t-beg-of-next-line 1)
        ;; We don't have any other option, so we must use the most
        ;; time-expensive option.
        (eat--t-carriage-return)
        (eat--t-index)))))

(defun eat--t-reverse-index ()
  "Go to the previous line preserving column, scrolling if needed."
  (let* ((cursor (eat--t-disp-cursor
                  (eat--t-term-display eat--t-term)))
         (scroll-begin (eat--t-term-scroll-begin eat--t-term))
         ;; Are we in the scroll region?
         (in-scroll-region (<= scroll-begin (eat--t-cur-y cursor))))
    ;; If this is the first line (of the scroll region or the
    ;; display), scroll down, otherwise move cursor upward.
    (if (= (if in-scroll-region scroll-begin 1)
           (eat--t-cur-y cursor))
        (eat--t-scroll-down 1)
      (eat--t-cur-up 1))))

(defun eat--t-bell ()
  "Ring the bell."
  ;; Call the UI's bell handler.
  (funcall (eat--t-term-bell-fn eat--t-term) eat--t-term))

(defun eat--t-form-feed ()
  "Insert a vertical tab."
  ;; Form feed is same as `eat--t-index'.
  (eat--t-index))

(defun eat--t-save-cur ()
  "Save current cursor position."
  (let ((disp (eat--t-term-display eat--t-term))
        (saved-face (eat--t-copy-face
                     (eat--t-term-face eat--t-term))))
    ;; Save cursor position.
    (setf (eat--t-disp-saved-cursor disp)
          (eat--t-copy-cur (eat--t-disp-cursor disp)))
    ;; Save SGR attributes.
    (setf (eat--t-term-saved-face eat--t-term) saved-face)
    ;; We use side-effects, so make sure the saved face doesn't share
    ;; structure with the current face.
    (setf (eat--t-face-face saved-face)
          (copy-tree (eat--t-face-face saved-face)))
    (setf (eat--t-face-underline-color saved-face)
          (copy-tree (eat--t-face-underline-color saved-face)))))

(defun eat--t-restore-cur ()
  "Restore previously save cursor position."
  (let ((saved (eat--t-disp-saved-cursor
                (eat--t-term-display eat--t-term))))
    ;; Restore cursor position.
    (eat--t-goto (eat--t-cur-y saved) (eat--t-cur-x saved))
    ;; Restore SGR attributes.
    (setf (eat--t-term-face eat--t-term)
          (copy-tree (eat--t-term-saved-face eat--t-term)))
    (setf (eat--t-face-underline-color (eat--t-term-face eat--t-term))
          (copy-tree (eat--t-face-underline-color
                      (eat--t-term-face eat--t-term))))))

(defun eat--t-erase-in-line (&optional n)
  "Erase part of current line, but don't move cursor.

N defaults to 0.  When N is 0, erase cursor to end of line.  When N is
1, erase beginning of line to cursor.  When N is 2, erase whole line."
  (let ((face (eat--t-term-face eat--t-term)))
    (pcase-exhaustive n
      ((or 0 'nil (pred (< 2)))
       ;; Delete cursor position (inclusive) to end of line.
       (delete-region (point) (car (eat--t-eol)))
       ;; If the SGR background attribute is set, we need to fill the
       ;; erased area with that background.
       (when (eat--t-face-bg face)
         (save-excursion
           (let* ((disp (eat--t-term-display eat--t-term))
                  (cursor (eat--t-disp-cursor disp)))
             (eat--t-repeated-insert
              ?\s (1+ (- (eat--t-disp-width disp)
                         (eat--t-cur-x cursor)))
              (and (eat--t-face-bg face)
                   (eat--t-face-face face)))))))
      (1
       ;; Delete beginning of line to cursor position (inclusive).
       (delete-region (car (eat--t-bol))
                      (if (or (= (point) (point-max))
                              (= (char-after) ?\n))
                          (point)
                        (1+ (point))))
       ;; Fill the region with spaces, use SGR background attribute
       ;; if set.
       (let ((cursor (eat--t-disp-cursor
                      (eat--t-term-display eat--t-term))))
         (eat--t-repeated-insert ?\s (eat--t-cur-x cursor)
                                 (and (eat--t-face-bg face)
                                      (eat--t-face-face face))))
       ;; We erased the character at the cursor position, so after
       ;; fill with spaces we are still off by one column; so move a
       ;; column backward.
       (backward-char))
      (2
       ;; Delete whole line.
       (delete-region (car (eat--t-bol)) (car (eat--t-eol)))
       (let* ((disp (eat--t-term-display eat--t-term))
              (cursor (eat--t-disp-cursor disp)))
         ;; Fill the region before cursor position with spaces, use
         ;; SGR background attribute if set.
         (eat--t-repeated-insert ?\s (1- (eat--t-cur-x cursor))
                                 (and (eat--t-face-bg face)
                                      (eat--t-face-face face)))
         ;; If the SGR background attribute is set, we need to fill
         ;; the erased area including and after cursor position with
         ;; that background.
         (when (eat--t-face-bg face)
           (save-excursion
             (eat--t-repeated-insert
              ?\s (1+ (- (eat--t-disp-width disp)
                         (eat--t-cur-x cursor)))
              (and (eat--t-face-bg face)
                   (eat--t-face-face face))))))))))

(defun eat--t-erase-in-disp (&optional n)
  "Erase part of display.

N defaults to 0.  When N is 0, erase cursor to end of display.  When N
is 1, erase beginning of display to cursor.  In both on the previous
cases, don't move cursor.  When N is 2, erase display and reset cursor
to (1, 1).  When N is 3, also erase the scrollback."
  (let ((face (eat--t-term-face eat--t-term)))
    (pcase-exhaustive n
      ((or 0 'nil (pred (< 3)))
       ;; Delete from cursor position (inclusive) to end of terminal.
       (delete-region (point) (point-max))
       ;; If the SGR background attribute is set, we need to fill the
       ;; erased area with that background.
       (when (eat--t-face-bg face)
         ;; `save-excursion' probably uses marker to save point, which
         ;; doesn't work in this case.  So we the store the point as a
         ;; integer.
         (let* ((pos (point))
                (disp (eat--t-term-display eat--t-term))
                (cursor (eat--t-disp-cursor disp)))
           ;; Fill current line.
           (eat--t-repeated-insert ?\s (1+ (- (eat--t-disp-width disp)
                                              (eat--t-cur-x cursor)))
                                   (eat--t-face-face face))
           ;; Fill the following lines.
           (dotimes (_ (- (eat--t-disp-height disp)
                          (eat--t-cur-y cursor)))
             (insert ?\n)
             (eat--t-repeated-insert ?\s (eat--t-disp-width disp)
                                     (eat--t-face-face face)))
           ;; Restore position.
           (goto-char pos))))
      (1
       (let* ((disp (eat--t-term-display eat--t-term))
              (cursor (eat--t-disp-cursor disp))
              (y (eat--t-cur-y cursor))
              (x (eat--t-cur-x cursor))
              ;; Should we erase including the cursor position?
              (incl-point (/= (point) (point-max))))
         ;; Delete the region to be erased.
         (delete-region (eat--t-disp-begin disp)
                        (if incl-point (1+ (point)) (point)))
         ;; If the SGR background attribute isn't set, insert
         ;; newlines, otherwise fill the erased area above the current
         ;; line with background color.
         (if (not (eat--t-face-bg face))
             (eat--t-repeated-insert ?\n (1- y))
           (dotimes (_ (1- y))
             (eat--t-repeated-insert ?\s (eat--t-disp-width disp)
                                     (eat--t-face-face face))
             (insert ?\n)))
         ;; Fill the current line to keep the cursor unmoved.  Use
         ;; background if the corresponding SGR attribute is set.
         (eat--t-repeated-insert ?\s x (and (eat--t-face-bg face)
                                            (eat--t-face-face face)))
         ;; We are off by one column; so move a column backward.
         (when incl-point
           (backward-char))))
      ((or 2 3)
       ;; Move to the display beginning.
       (eat--t-goto 1 1)
       ;; Delete everything in the display, and if N is 3, also delete
       ;; everything in the scrollback area.
       (delete-region (if (= n 2) (point) (point-min))
                      (point-max))
       ;; If the SGR background attribute is set, fill the display
       ;; with that background.
       (when (eat--t-face-bg face)
         ;; `save-excursion' probably uses marker to save point, which
         ;; doesn't work in this case.  So we the store the point as a
         ;; integer.
         (let ((pos (point))
               (disp (eat--t-term-display eat--t-term)))
           (dotimes (i (eat--t-disp-height disp))
             (unless (zerop i)
               (insert ?\n))
             (eat--t-repeated-insert ?\s (eat--t-disp-width disp)
                                     (eat--t-face-face face)))
           ;; Restore point.
           (goto-char pos)))))))

(defun eat--t-device-status-report (n)
  "Report device (terminal) status.

If N is 5, send OK sequence.  If N is 6, send the current Y and X
coordinate to client."
  (pcase n
    (5
     (funcall (eat--t-term-input-fn eat--t-term) eat--t-term "\e[0n"))
    (6
     (let ((cursor (eat--t-disp-cursor
                    (eat--t-term-display eat--t-term))))
       (funcall (eat--t-term-input-fn eat--t-term) eat--t-term
                (format "\e[%i;%iR" (eat--t-cur-y cursor)
                        (eat--t-cur-x cursor)))))))

(defun eat--t-set-cursor-state (state)
  "Set cursor state to STATE.

STATE one of the `:invisible', `:block', `:blinking-block',
`:underline', `:blinking-underline', `:bar', `:blinking-bar'."
  (if (eq state :invisible)
      (when (eat--t-term-cur-visible-p eat--t-term)
        (setf (eat--t-term-cur-visible-p eat--t-term) nil)
        (funcall (eat--t-term-set-cursor-fn eat--t-term) eat--t-term
                 :invisible))
    (unless (and (eat--t-term-cur-visible-p eat--t-term)
                 (eq (eat--t-term-cur-state eat--t-term) state))
      ;; Update state.
      (setf (eat--t-term-cur-state eat--t-term) state)
      (setf (eat--t-term-cur-visible-p eat--t-term) t)
      ;; Inform the UI.
      (funcall (eat--t-term-set-cursor-fn eat--t-term) eat--t-term
               state))))

(defun eat--t-set-cursor-style (style)
  "Set cursor state as described by STYLE."
  (when (<= 0 style 6)
    (let ((state (aref [ :blinking-block :blinking-block :block
                         :blinking-underline :underline
                         :blinking-bar :bar]
                       style)))
      (if (eat--t-term-cur-visible-p eat--t-term)
          (eat--t-set-cursor-state state)
        (setf (eat--t-term-cur-state eat--t-term) state)))))

(defun eat--t-show-cursor ()
  "Make the cursor visible."
  (when (not (eat--t-term-cur-visible-p eat--t-term))
    (eat--t-set-cursor-state (eat--t-term-cur-state eat--t-term))))

(defun eat--t-hide-cursor ()
  "Make the cursor invisible."
  (when (eat--t-term-cur-visible-p eat--t-term)
    (eat--t-set-cursor-state :invisible)))

(defun eat--t-blinking-cursor ()
  "Make the cursor blink."
  (let ((state (pcase (eat--t-term-cur-state eat--t-term)
                 (:block :blinking-block)
                 (:underline :blinking-underline)
                 (:bar :blinking-bar)
                 (state state))))
    (if (eat--t-term-cur-visible-p eat--t-term)
        (eat--t-set-cursor-state state)
      (setf (eat--t-term-cur-state eat--t-term) state))))

(defun eat--t-non-blinking-cursor ()
  "Make the cursor not blink."
  (let ((state (pcase (eat--t-term-cur-state eat--t-term)
                 (:blinking-block :block)
                 (:blinking-underline :underline)
                 (:blinking-bar :bar)
                 (state state))))
    (if (eat--t-term-cur-visible-p eat--t-term)
        (eat--t-set-cursor-state state)
      (setf (eat--t-term-cur-state eat--t-term) state))))

(defun eat--t-enable-bracketed-yank ()
  "Enable bracketed yank mode."
  (setf (eat--t-term-bracketed-yank eat--t-term) t))

(defun eat--t-disable-bracketed-yank ()
  "Disable bracketed yank mode."
  (setf (eat--t-term-bracketed-yank eat--t-term) nil))

(defun eat--t-enable-alt-disp ()
  "Enable alternative display."
  ;; Effective only when alternative display is enabled by user.
  (when eat-enable-alternative-display
    ;; Make sure we not already in the alternative display.
    (unless (eat--t-term-main-display eat--t-term)
      ;; Store the current display, including scrollback.
      (let ((main-disp (eat--t-copy-disp
                        (eat--t-term-display eat--t-term))))
        (setf (eat--t-disp-begin main-disp)
              (- (eat--t-disp-begin main-disp) (point-min)))
        (setf (eat--t-disp-old-begin main-disp)
              (- (eat--t-disp-old-begin main-disp) (point-min)))
        (setf (eat--t-disp-cursor main-disp)
              (eat--t-copy-cur (eat--t-disp-cursor main-disp)))
        (setf (eat--t-disp-saved-cursor main-disp)
              (eat--t-copy-cur (eat--t-disp-saved-cursor main-disp)))
        (setf (eat--t-cur-position (eat--t-disp-cursor main-disp))
              (- (point) (point-min)))
        (setf (eat--t-term-main-display eat--t-term)
              (cons main-disp (buffer-string)))
        ;; Delete everything, and move to the beginning of terminal.
        (delete-region (point-min) (point-max))
        (eat--t-goto 1 1)))))

(defun eat--t-disable-alt-disp (&optional dont-move-cursor)
  "Disable alternative display.

If DONT-MOVE-CURSOR is non-nil, don't move cursor from current
position."
  ;; Make sure we in the alternative display.
  (when (eat--t-term-main-display eat--t-term)
    (let ((main-disp (eat--t-term-main-display eat--t-term))
          (old-y (eat--t-cur-y
                  (eat--t-disp-cursor
                   (eat--t-term-display eat--t-term))))
          (old-x (eat--t-cur-x
                  (eat--t-disp-cursor
                   (eat--t-term-display eat--t-term))))
          (width (eat--t-disp-width
                  (eat--t-term-display eat--t-term)))
          (height (eat--t-disp-height
                   (eat--t-term-display eat--t-term))))
      ;; Delete everything.
      (delete-region (point-min) (point-max))
      ;; Restore the main display.
      (insert (cdr main-disp))
      (setf (eat--t-cur-position (eat--t-disp-cursor (car main-disp)))
            (copy-marker (+ (point-min)
                            (eat--t-cur-position
                             (eat--t-disp-cursor (car main-disp))))))
      (setf (eat--t-disp-old-begin (car main-disp))
            (copy-marker (+ (point-min)
                            (eat--t-disp-old-begin (car main-disp)))))
      (setf (eat--t-disp-begin (car main-disp))
            (copy-marker (+ (point-min)
                            (eat--t-disp-begin (car main-disp)))))
      (setf (eat--t-term-display eat--t-term) (car main-disp)
            (eat--t-term-main-display eat--t-term) nil)
      (goto-char (eat--t-cur-position
                  (eat--t-disp-cursor
                   (eat--t-term-display eat--t-term))))
      ;; Maybe the terminal was resized after enabling alternative
      ;; display, so we have to resize again.
      (eat--t-resize width height)
      ;; Restore cursor position if DONT-MOVE-CURSOR is non-nil.
      (when dont-move-cursor
        (eat--t-goto old-y old-x)))))

(defun eat--t-insert-char (n)
  "Insert N empty (space) characters, preserving cursor."
  (let* ((disp (eat--t-term-display eat--t-term))
         (cursor (eat--t-disp-cursor disp)))
    ;; Make sure N is positive.  If N is more than the number of
    ;; available columns available, set N to the maximum possible
    ;; value.
    (setq n (min (- (eat--t-disp-width disp)
                    (1- (eat--t-cur-x cursor)))
                 (max (or n 1) 1)))
    ;; Return if N is zero.
    (unless (zerop n)
      ;; If the position isn't safe, replace the multi-column
      ;; character with spaces to make it safe.
      (eat--t-make-pos-safe)
      (save-excursion
        (let ((face (eat--t-term-face eat--t-term)))
          ;; Insert N spaces, with SGR background if that attribute is
          ;; set.
          (eat--t-repeated-insert
           ?\s n (and (eat--t-face-bg face) (eat--t-face-face face))))
        ;; Remove the characters that went beyond the edge of
        ;; display.
        (eat--t-col-motion (- (eat--t-disp-width disp)
                              (+ (1- (eat--t-cur-x cursor)) n)))
        ;; Make sure we delete any multi-column character
        ;; completely.
        (eat--t-move-before-to-safe)
        (delete-region (point) (car (eat--t-eol)))))))

(defun eat--t-delete-char (n)
  "Delete N characters, preserving cursor."
  (let* ((disp (eat--t-term-display eat--t-term))
         (face (eat--t-term-face eat--t-term))
         (cursor (eat--t-disp-cursor disp)))
    ;; Make sure N is positive.  If N is more than the number of
    ;; available columns available, set N to the maximum possible
    ;; value.
    (setq n (min (- (eat--t-disp-width disp)
                    (1- (eat--t-cur-x cursor)))
                 (max (or n 1) 1)))
    ;; Return if N is zero.
    (unless (zerop n)
      ;; If the position isn't safe, replace the multi-column
      ;; character with spaces to make it safe.
      (eat--t-make-pos-safe)
      (save-excursion
        (let ((m (point)))
          ;; Delete N character on current line.
          (eat--t-col-motion n)
          (delete-region m (point))
          ;; Replace any partially-overwritten character with spaces.
          (eat--t-fix-partial-multi-col-char)
          ;; If SGR background attribute is set, fill N characters at
          ;; the right edge of display with that background.
          (when (eat--t-face-bg face)
            (save-excursion
              (eat--t-goto-eol)
              (let ((empty (1+ (- (eat--t-disp-width disp)
                                  (eat--t-cur-x cursor)
                                  (- (point) m)))))
                ;; Reach the position from where to start filling.
                ;; Use spaces if needed.
                (when (> empty n)
                  (eat--t-repeated-insert ?\s (- empty n)))
                ;; Fill with background.
                (eat--t-repeated-insert
                 ?\s (min empty n) (eat--t-face-face face))))))))))

(defun eat--t-erase-char (n)
  "Make next N character cells empty, preserving cursor."
  (let* ((disp (eat--t-term-display eat--t-term))
         (face (eat--t-term-face eat--t-term))
         (cursor (eat--t-disp-cursor disp)))
    ;; Make sure N is positive.  If N is more than the number of
    ;; available columns available, set N to the maximum possible
    ;; value.
    (setq n (min (- (eat--t-disp-width disp)
                    (1- (eat--t-cur-x cursor)))
                 (max (or n 1) 1)))
    ;; Return if N is zero.
    (unless (zerop n)
      ;; If the position isn't safe, replace the multi-column
      ;; character with spaces to make it safe.
      (eat--t-make-pos-safe)
      (save-excursion
        (let ((m (point)))
          ;; Delete N character on current line.
          (eat--t-col-motion n)
          (delete-region m (point))
          ;; Replace any partially-overwritten character with spaces.
          (eat--t-fix-partial-multi-col-char)
          ;; Insert N spaces, with background if SGR background
          ;; attribute is set.
          (eat--t-repeated-insert
           ?\s n (and (eat--t-face-bg face)
                      (eat--t-face-face face))))))))

(defun eat--t-insert-line (n)
  "Insert N empty lines, preserving cursor."
  (let* ((disp (eat--t-term-display eat--t-term))
         (cursor (eat--t-disp-cursor disp))
         (scroll-begin (eat--t-term-scroll-begin eat--t-term))
         (scroll-end (eat--t-term-scroll-end eat--t-term)))
    ;; N should be positive and shouldn't exceed the number of lines
    ;; below cursor position and inside current scroll region.
    (setq n (min (- (1+ (- scroll-end scroll-begin))
                    (1- (eat--t-cur-y cursor)))
                 (max (or n 1) 1)))
    ;; Make sure we are in the scroll region and N is positive, return
    ;; on failure.
    (when (and (<= scroll-begin (eat--t-cur-y cursor) scroll-end)
               (not (zerop n)))
      ;; This function doesn't move the cursor, but pushes all the
      ;; line below and including current line.  So to keep the cursor
      ;; unmoved, go to the beginning of line and insert enough spaces
      ;; to not move the cursor.
      (eat--t-goto-bol)
      (let ((face (eat--t-term-face eat--t-term)))
        (eat--t-repeated-insert ?\s (1- (eat--t-cur-x cursor))
                                (and (eat--t-face-bg face)
                                     (eat--t-face-face face)))
        (goto-char
         (prog1 (point)
           ;; Insert N lines.
           (if (not (eat--t-face-bg face))
               (eat--t-repeated-insert ?\n n)
             ;; SGR background attribute set, so fill the inserted
             ;; lines with background.
             (dotimes (i n)
               ;; Fill a line.
               (eat--t-repeated-insert
                ?\s (if (not (zerop i))
                        (eat--t-disp-width disp)
                      ;; The first inserted line is already filled
                      ;; partially, so calculate the number columns
                      ;; left to fill.
                      (1+ (- (eat--t-disp-width disp)
                             (eat--t-cur-x cursor))))
                (eat--t-face-face face))
               ;; New line.
               (insert ?\n)))
           ;; Delete the lines that were just pushed beyond the end of
           ;; scroll region.
           (eat--t-goto-eol (- (1+ (- scroll-end scroll-begin))
                               (+ (- (eat--t-cur-y cursor)
                                     (1- scroll-begin))
                                  n)))
           (delete-region (point) (car (eat--t-eol n)))))))))

(defun eat--t-delete-line (n)
  "Delete N lines, preserving cursor."
  (let* ((disp (eat--t-term-display eat--t-term))
         (cursor (eat--t-disp-cursor disp))
         (x (eat--t-cur-x cursor))
         (scroll-begin (eat--t-term-scroll-begin eat--t-term))
         (scroll-end (eat--t-term-scroll-end eat--t-term)))
    ;; N should be positive and shouldn't exceed the number of
    ;; lines below cursor position and inside current scroll
    ;; region.
    (setq n (min (- (1+ (- scroll-end scroll-begin))
                    (1- (eat--t-cur-y cursor)))
                 (max (or n 1) 1)))
    ;; Make sure we are in the scroll region and N is positive, return
    ;; on failure.
    (when (and (<= scroll-begin (eat--t-cur-y cursor) scroll-end)
               (not (zerop n)))
      ;; Delete N lines (including the current one).
      (eat--t-goto-bol)
      (save-excursion
        (let ((m (point)))
          (eat--t-goto-bol n)
          (delete-region m (point))))
      (let ((face (eat--t-term-face eat--t-term)))
        ;; Keep the lines beyond end of scroll region unmoved.
        (when (or (< scroll-end (eat--t-disp-height disp))
                  (eat--t-face-bg face))
          (let* ((pos (point))
                 (move (- (1+ (- scroll-end scroll-begin))
                          (- (+ (eat--t-cur-y cursor) n)
                             (1- scroll-begin))))
                 (moved (eat--t-goto-eol move)))
            (when (or (/= (point) (point-max))
                      (eat--t-face-bg face))
              ;; Move to the end of scroll region.
              (eat--t-repeated-insert ?\n (- move moved))
              ;; Insert enough new lines, fill them when SGR
              ;; background attribute is set.
              (if (not (eat--t-face-bg face))
                  (eat--t-repeated-insert ?\n n)
                (dotimes (_ n)
                  (insert ?\n)
                  (eat--t-repeated-insert ?\s (eat--t-disp-width disp)
                                          (eat--t-face-face face)))))
            (goto-char pos))))
      ;; Go to column where cursor is to preserve cursor position, use
      ;; spaces if needed to reach the position.
      (eat--t-repeated-insert
       ?\s (- (1- x) (eat--t-col-motion (1- x)))))))

(defun eat--t-repeat-last-char (&optional n)
  "Repeat last character N times."
  ;; N must be at least one.
  (setq n (max (or n 1) 1))
  (let* ((disp (eat--t-term-display eat--t-term))
         (char
          ;; Get the character before cursor.
          (when (< (eat--t-disp-begin disp) (point))
            (if (get-text-property (1- (point)) 'eat--t-wrap-line)
                ;; The character before cursor is a newline to break
                ;; a long line, so use the character before that.
                (when (< (eat--t-disp-begin disp) (1- (point)))
                  (char-before (1- (point))))
              (char-before)))))
    ;; Insert `char' N times.  Make sure `char' is a non-nil and not
    ;; a newline.
    (when (and char (/= char ?\n))
      (eat--t-write (make-string n char)))))

(defun eat--t-change-scroll-region (&optional top bottom)
  "Change the scroll region from lines TOP to BOTTOM (inclusive).

TOP defaults to 1 and BOTTOM defaults to the height of the display."
  (let ((disp (eat--t-term-display eat--t-term)))
    (setq top (or top 1))
    (setq bottom (or bottom (eat--t-disp-height disp)))
    ;; According to DEC's documentation (found somewhere on the
    ;; internet, but can't remember where), TOP and BOTTOM must be
    ;; within display, and BOTTOM must be below TOP.  Otherwise the
    ;; control function is a nop.
    (when (< 0 top bottom (1+ (eat--t-disp-height disp)))
      (setf (eat--t-term-scroll-begin eat--t-term) top
            (eat--t-term-scroll-end eat--t-term) bottom)
      (eat--t-goto 1 1))))

(defun eat--t-insert-mode ()
  "Enable insert mode and disable replace mode."
  (setf (eat--t-term-ins-mode eat--t-term) t))

(defun eat--t-replace-mode ()
  "Enable replace mode and disable insert mode."
  (setf (eat--t-term-ins-mode eat--t-term) nil))

(defun eat--t-set-sgr-params (params)
  "Set SGR parameters PARAMS."
  (let ((face (eat--t-term-face eat--t-term)))
    ;; Set attributes.
    (while params
      (pcase (pop params)
        (`(,(or 0 'nil))
         (1value (setf (eat--t-face-fg face) nil))
         (1value (setf (eat--t-face-bg face) nil))
         (1value (setf (eat--t-face-intensity face) nil))
         (1value (setf (eat--t-face-italic face) nil))
         (1value (setf (eat--t-face-underline face) nil))
         (1value (setf (eat--t-face-underline-color face) nil))
         (1value (setf (eat--t-face-crossed face) nil))
         (1value (setf (eat--t-face-conceal face) nil))
         (1value (setf (eat--t-face-inverse face) nil))
         (1value (setf (eat--t-face-blink face) nil))
         (setf (eat--t-face-font face)
               (aref (eat--t-term-font-faces eat--t-term) 0)))
        ('(1)
         (setf (eat--t-face-intensity face)
               (eat--t-term-bold-face eat--t-term)))
        ('(2)
         (setf (eat--t-face-intensity face)
               (eat--t-term-faint-face eat--t-term)))
        ('(3)
         (setf (eat--t-face-italic face)
               (eat--t-term-italic-face eat--t-term)))
        ('(4)
         (1value (setf (eat--t-face-underline face) 'line)))
        ('(4 0)
         (1value (setf (eat--t-face-underline face) nil)))
        ('(4 1)
         (1value (setf (eat--t-face-underline face) 'line)))
        ('(4 2)
         (1value (setf (eat--t-face-underline face) 'line)))
        ('(4 3)
         (1value (setf (eat--t-face-underline face) 'wave)))
        ('(4 4)
         (1value (setf (eat--t-face-underline face) 'wave)))
        ('(4 5)
         (1value (setf (eat--t-face-underline face) 'wave)))
        ('(5)
         (setf (eat--t-face-blink face)
               (eat--t-term-slow-blink-face eat--t-term)))
        ('(6)
         (setf (eat--t-face-blink face)
               (eat--t-term-fast-blink-face eat--t-term)))
        ('(7)
         (1value (setf (eat--t-face-inverse face) t)))
        ('(8)
         (1value (setf (eat--t-face-conceal face) t)))
        ('(9)
         (1value (setf (eat--t-face-crossed face) t)))
        (`(,(and (pred (lambda (font) (<= 10 font 19)))
                 font))
         (setf (eat--t-face-font face)
               (aref (eat--t-term-font-faces eat--t-term)
                     (- font 10))))
        ('(21)
         (1value (setf (eat--t-face-underline face) 'line)))
        ('(22)
         (1value (setf (eat--t-face-intensity face) nil)))
        ('(23)
         (1value (setf (eat--t-face-italic face) nil)))
        ('(24)
         (1value (setf (eat--t-face-underline face) nil)))
        ('(25)
         (1value (setf (eat--t-face-blink face) nil)))
        ('(27)
         (1value (setf (eat--t-face-inverse face) nil)))
        ('(28)
         (1value (setf (eat--t-face-conceal face) nil)))
        ('(29)
         (1value (setf (eat--t-face-crossed face) nil)))
        (`(,(and (pred (lambda (color) (<= 30 color 37)))
                 color))
         (setf (eat--t-face-fg face)
               (face-foreground
                (aref (eat--t-term-color-faces eat--t-term)
                      (- color 30))
                nil t)))
        ('(38)
         (pcase (pop params)
           ('(2)
            (setf (eat--t-face-fg face)
                  (let ((r (car (pop params)))
                        (g (car (pop params)))
                        (b (car (pop params))))
                    (when (and r (<= 0 r 255)
                               g (<= 0 g 255)
                               b (<= 0 b 255))
                      (format "#%02x%02x%02x" r g b)))))
           ('(5)
            (let ((color (car (pop params))))
              (setf (eat--t-face-fg face)
                    (when (and color (<= 0 color 255))
                      (face-foreground
                       (aref (eat--t-term-color-faces eat--t-term)
                             color)
                       nil t)))))))
        ('(39)
         (1value (setf (eat--t-face-fg face) nil)))
        (`(,(and (pred (lambda (color) (<= 40 color 47)))
                 color))
         (setf (eat--t-face-bg face)
               (face-foreground
                (aref (eat--t-term-color-faces eat--t-term)
                      (- color 40))
                nil t)))
        ('(48)
         (setf (eat--t-face-bg face)
               (pcase (pop params)
                 ('(2)
                  (let ((r (car (pop params)))
                        (g (car (pop params)))
                        (b (car (pop params))))
                    (when (and r (<= 0 r 255)
                               g (<= 0 g 255)
                               b (<= 0 b 255))
                      (format "#%02x%02x%02x" r g b))))
                 ('(5)
                  (let ((color (car (pop params))))
                    (when (and color (<= 0 color 255))
                      (face-foreground
                       (aref (eat--t-term-color-faces eat--t-term)
                             color)
                       nil t)))))))
        ('(49)
         (1value (setf (eat--t-face-bg face) nil)))
        ('(58)
         (setf (eat--t-face-underline-color face)
               (pcase (pop params)
                 ('(2)
                  (let ((r (car (pop params)))
                        (g (car (pop params)))
                        (b (car (pop params))))
                    (when (and r (<= 0 r 255)
                               g (<= 0 g 255)
                               b (<= 0 b 255))
                      (format "#%02x%02x%02x" r g b))))
                 ('(5)
                  (let ((color (car (pop params))))
                    (when (and color (<= 0 color 255))
                      (face-foreground
                       (aref (eat--t-term-color-faces eat--t-term)
                             color)
                       nil t)))))))
        ('(59)
         (1value (setf (eat--t-face-underline-color face) nil)))
        (`(,(and (pred (lambda (color) (<= 90 color 97)))
                 color))
         (setf (eat--t-face-fg face)
               (face-foreground
                (aref (eat--t-term-color-faces eat--t-term)
                      (- color 82))
                nil t)))
        (`(,(and (pred (lambda (color) (<= 100 color 107)))
                 color))
         (setf (eat--t-face-bg face)
               (face-foreground
                (aref (eat--t-term-color-faces eat--t-term)
                      (- color 92))
                nil t)))))
    ;; Update face according to the attributes.
    (setf (eat--t-face-face face)
          `(,@(and-let* ((fg (or (if (eat--t-face-conceal face)
                                     (eat--t-face-bg face)
                                   (eat--t-face-fg face))
                                 (cond
                                  ((eat--t-face-inverse face)
                                   (face-foreground 'default))
                                  ((eat--t-face-conceal face)
                                   (face-background 'default))))))
                (list (if (eat--t-face-inverse face)
                          :background
                        :foreground)
                      fg))
            ,@(and-let* ((bg (or (eat--t-face-bg face)
                                 (and (eat--t-face-inverse face)
                                      (face-background 'default)))))
                (list (if (eat--t-face-inverse face)
                          :foreground
                        :background)
                      bg))
            ,@(and-let* ((underline (eat--t-face-underline face)))
                (list
                 :underline
                 (list :color (eat--t-face-underline-color face)
                       :style underline)))
            ,@(and-let* ((crossed (eat--t-face-crossed face)))
                ;; REVIEW: How about colors?  No terminal supports
                ;; crossed attribute with colors, so we'll need to be
                ;; creative to add the feature.
                `(:strike-through t))
            :inherit
            (,@(and-let* ((intensity (eat--t-face-intensity face)))
                 (list intensity))
             ,@(and-let* ((italic (eat--t-face-italic face)))
                 (list italic))
             ,@(and-let* ((blink (eat--t-face-blink face)))
                 (list blink))
             ,(eat--t-face-font face))))))

(defun eat--t-enable-keypad ()
  "Enable keypad."
  (1value (setf (eat--t-term-keypad-mode eat--t-term) t)))

(defun eat--t-disable-keypad ()
  "Disable keypad."
  (1value (setf (eat--t-term-keypad-mode eat--t-term) nil)))

(defun eat--t-enable-sgr-mouse-encoding ()
  "Arrange that the following mouse events will be encoded like SGR."
  (setf (eat--t-term-mouse-encoding eat--t-term) 'sgr))

(defun eat--t-disable-sgr-mouse-encoding ()
  "Arrange that the following mouse events won't be encoded like SGR."
  (setf (eat--t-term-mouse-encoding eat--t-term) nil))

(defun eat--t-set-mouse-mode (mode)
  "Set current mouse mode to MODE.

MODE should be one of nil and `x10', `normal', `button-event',
`any-event'."
  (setf (eat--t-term-mouse-mode eat--t-term) mode)
  ;; When MODE is nil, disable mouse.
  (unless mode
    (eat--t-disable-sgr-mouse-encoding))
  ;; `x10' mouse mode doesn't need to keep track of the mouse buttons
  ;; pressed.
  (when (or (not mode)
            (eq mode 'x10))
    (setf (eat--t-term-mouse-pressed eat--t-term) nil))
  ;; Inform the UI.
  (funcall (eat--t-term-grab-mouse-fn eat--t-term) eat--t-term
           (pcase-exhaustive mode
             ('x10 :click)
             ('normal :modifier-click)
             ('button-event :drag)
             ('any-event :all)
             ('nil nil))))

(defun eat--t-enable-x10-mouse ()
  "Enable X10 mouse tracking."
  (eat--t-set-mouse-mode 'x10))

(defun eat--t-enable-normal-mouse ()
  "Enable normal mouse tracking."
  (eat--t-set-mouse-mode 'normal))

(defun eat--t-enable-button-event-mouse ()
  "Enable button-event mouse tracking."
  (eat--t-set-mouse-mode 'button-event))

(defun eat--t-enable-any-event-mouse ()
  "Enable any-event mouse tracking."
  (eat--t-set-mouse-mode 'any-event))

(defun eat--t-disable-mouse ()
  "Disable mouse tracking."
  (eat--t-set-mouse-mode nil))

(defun eat--t-enable-focus-event ()
  "Enable sending focus events."
  (1value (setf (eat--t-term-focus-event-mode eat--t-term) t))
  (funcall (eat--t-term-set-focus-ev-mode-fn eat--t-term) eat--t-term
           t))

(defun eat--t-disable-focus-event ()
  "Disable sending focus events."
  (1value (setf (eat--t-term-focus-event-mode eat--t-term) nil))
  (funcall (eat--t-term-set-focus-ev-mode-fn eat--t-term) eat--t-term
           nil))

(defun eat--t-set-title (title)
  "Set the title of terminal to TITLE."
  ;; Update title.
  (setf (eat--t-term-title eat--t-term) title)
  ;; Inform the UI.
  (funcall (eat--t-term-set-title-fn eat--t-term) eat--t-term title))

(defun eat--t-set-cwd (url)
  "Set the working directory of terminal to URL."
  (setq url (url-generic-parse-url url))
  (when (string= (url-type url) "file")
    (let ((host (url-host url))
          (dir (expand-file-name
                (file-name-as-directory
                 (url-unhex-string (url-filename url))))))
      ;; Inform the UI.
      (funcall (eat--t-term-set-cwd-fn eat--t-term)
               eat--t-term host dir))))

(defun eat--t-send-device-attrs (n format)
  "Return device attributes.

FORMAT is the format of parameters in output.  N should be zero."
  (pcase-exhaustive format
    ('nil
     (when (= (or n 0) 0)
       (funcall (eat--t-term-input-fn eat--t-term) eat--t-term
                "\e[?12;4c")))
    (?>
     (when (= (or n 0) 0)
       (funcall (eat--t-term-input-fn eat--t-term) eat--t-term
                "\e[>0;0;0c")))))

(defun eat--t-send-graphics-attrs (attr operation)
  "Send graphics attributes.

ATTR is the attribute requested, OPERATION is the thing to do (only
reading an attribute is supported)."
  (funcall
   (eat--t-term-input-fn eat--t-term) eat--t-term
   (if (memq operation '(1 4))
       (pcase attr
         (1
          ;; TODO: Maybe provide an user option to control the value?
          ;; count?
          (format "\e[?1;0;256S"))
         (2
          ;; TODO: Maybe provide an user option to control the value?
          (let ((disp (eat--t-term-display eat--t-term)))
            (format "\e[?2;0;%i;%iS"
                    (min (* (eat--t-disp-width disp)
                            (eat--t-term-char-width eat--t-term))
                         1000)
                    (min (* (eat--t-disp-height disp)
                            (eat--t-term-char-height eat--t-term))
                         1000))))
         (_
          (format "\e[?%i;1S" attr)))
     (format "\e[?%i;%iS" attr
             (if (<= 1 attr 2) (if (<= 2 operation 3) 3 2) 1)))))

(defun eat--t-report-foreground-color ()
  "Report the current default foreground color to the client."
  (funcall
   (eat--t-term-input-fn eat--t-term) eat--t-term
   (let ((rgb (or (color-values (face-foreground 'default))
                  ;; On terminals like TTYs the above returns nil.
                  ;; Terminals usually have a white foreground, so...
                  '(255 255 255))))
     (format "\e]10;rgb:%04x/%04x/%04x\e\\"
             (pop rgb) (pop rgb) (pop rgb)))))

(defun eat--t-report-background-color ()
  "Report the current default background color to the client."
  (funcall
   (eat--t-term-input-fn eat--t-term) eat--t-term
   (let ((rgb (or (color-values (face-background 'default))
                  ;; On terminals like TTYs the above returns nil.
                  ;; Terminals usually have a black background, so...
                  '(0 0 0))))
     (format "\e]11;rgb:%04x/%04x/%04x\e\\"
             (pop rgb) (pop rgb) (pop rgb)))))

(defun eat--t-manipulate-selection (targets data)
  "Set and send current selection.

TARGETS is a string containing zero or more characters from the set
`c', `p', `q', `s', `0', `1', `2', `3', `4', `5', `6', and `7'.  DATA
is the selection data encoded in base64."
  (when (string-empty-p targets)
    (setq targets "s0"))
  (if (string= data "?")
      ;; The client is requesting for clipboard content, let's try to
      ;; fulfill the request.
      (funcall
       (eat--t-term-input-fn eat--t-term) eat--t-term
       (let ((str nil)
             (n 0))
         ;; Remove invalid and duplicate targets from TARGETS before
         ;; processing it and sending it back.
         (setq targets
               (apply #'string
                      (cl-delete-duplicates
                       (cl-delete-if-not
                        (lambda (c) (or (<= ?0 c ?7)
                                        (memq c '(?c ?p ?q ?s))))
                        (string-to-list targets)))))
         (while (and (not str) (< n (length targets)))
           (setq
            str
            (pcase (aref targets n)
              ;; c, p, q and s targets are handled by the UI, and they
              ;; might refuse to give the clipboard content.
              (?c
               (funcall
                (eat--t-term-manipulate-selection-fn eat--t-term)
                eat--t-term :clipboard t))
              (?p
               (funcall
                (eat--t-term-manipulate-selection-fn eat--t-term)
                eat--t-term :primary t))
              (?q
               (funcall
                (eat--t-term-manipulate-selection-fn eat--t-term)
                eat--t-term :secondary t))
              (?s
               (funcall
                (eat--t-term-manipulate-selection-fn eat--t-term)
                eat--t-term :select t))
              ;; 0 to 9 targets are handled by us, and always work.
              ((and (pred (<= ?0))
                    (pred (>= ?7))
                    i)
               (aref (eat--t-term-cut-buffers eat--t-term)
                     (- i ?0)))))
           (cl-incf n))
         ;; No string to send, so send an empty string.
         (unless str (setq str ""))
         (format "\e]52;%s;%s\e\\" targets
                 (base64-encode-string (encode-coding-string
                                        str locale-coding-system)
                                       'no-line-break))))
    ;; The client is requesting to set clipboard content, let's try to
    ;; fulfill the request.
    (let ((str (ignore-errors
                 (decode-coding-string (base64-decode-string data)
                                       locale-coding-system))))
      (seq-doseq (target targets)
        (pcase target
          ;; c, p, q and s targets are handled by the UI, and they
          ;; might reject the new clipboard content.
          (?c
           (funcall (eat--t-term-manipulate-selection-fn eat--t-term)
                    eat--t-term :clipboard str))
          (?p
           (funcall (eat--t-term-manipulate-selection-fn eat--t-term)
                    eat--t-term :primary str))
          (?q
           (funcall (eat--t-term-manipulate-selection-fn eat--t-term)
                    eat--t-term :secondary str))
          (?s
           (funcall (eat--t-term-manipulate-selection-fn eat--t-term)
                    eat--t-term :select str))
          ;; 0 to 7 targets are handled by us, and always work.
          ((and (pred (<= ?0))
                (pred (>= ?7))
                i)
           (aset (eat--t-term-cut-buffers eat--t-term) (- i ?0)
                 str)))))))

(defun eat--t-sixel-init ()
  "Initialize Sixel mode."
  (let ((default-palette
         (eval-when-compile
           (vconcat '("#000000" "#3333cc" "#cc2121" "#33cc33"
                      "#cc33cc" "#33cccc" "#cccc33" "#878787"
                      "#424242" "#545499" "#994242" "#549954"
                      "#995499" "#549999" "#999954" "#cccccc")
                    (make-list 240 "#000000")))))
    (dotimes (i 256)
      (setf (aref (eat--t-term-sixel-palette eat--t-term) i)
            (aref default-palette i))))
  ;; We just follow XTerm and set the initial foreground color to 3.
  ;; But even the XTerm authors are unsure about what was the actual
  ;; default.
  (setf (eat--t-term-sixel-color eat--t-term) 3)
  (while (< (eat--t-term-sixel-buffer-size eat--t-term)
            (+ (* (eat--t-term-char-height eat--t-term) 2) 5))
    (let ((new
           (cons (cons 0 (make-vector 1000 nil))
                 (cons (cadr (eat--t-term-sixel-buffer eat--t-term))
                       (eat--t-term-sixel-buffer eat--t-term)))))
      (setf (cddr (cadr (eat--t-term-sixel-buffer eat--t-term))) new)
      (setf (cadr (eat--t-term-sixel-buffer eat--t-term)) new)
      (setf (eat--t-term-sixel-buffer eat--t-term) new))
    (cl-incf (eat--t-term-sixel-buffer-size eat--t-term)))
  (let* ((beg (eat--t-term-sixel-buffer eat--t-term))
         (line beg)
         (loop t))
    (while loop
      (cl-loop for i from 0 to (1- (caar line))
               do (aset (cdar line) i nil))
      (setf (caar line) 0)
      (setq line (cddr line))
      (when (eq line beg)
        (setq loop nil))))
  (let ((cursor (eat--t-disp-cursor
                 (eat--t-term-display eat--t-term))))
    (setf (eat--t-cur-sixel-x cursor) 0)
    (setf (eat--t-cur-sixel-y cursor) 0)
    (setf (eat--t-cur-sixel-beg cursor)
          (eat--t-term-sixel-buffer eat--t-term))
    (unless (eat--t-term-sixel-scroll-mode eat--t-term)
      (setf (eat--t-term-sixel-initial-cursor-pos eat--t-term)
            (cons (eat--t-cur-y cursor) (eat--t-cur-x cursor)))
      (eat--t-goto 1 1))))

(defun eat--t-sixel-write (str beg end count)
  "Write substring [BEG..END) of STR COUNT times to Sixel buffer."
  (let ((cursor (eat--t-disp-cursor
                 (eat--t-term-display eat--t-term))))
    (dotimes (_ count)
      (cl-loop
       for i from beg to (1- end) do
       (when (= (eat--t-cur-sixel-x cursor) 1000)
         (setf (eat--t-cur-sixel-x cursor) 999))
       (let ((bitmap (- (aref str i) ??))
             (j 0)
             (line (eat--t-cur-sixel-beg cursor))
             (color (aref (eat--t-term-sixel-palette eat--t-term)
                          (eat--t-term-sixel-color eat--t-term))))
         (while (< j 6)
           (when (/= (logand bitmap (ash 1 j)) 0)
             (aset (cdar line) (eat--t-cur-sixel-x cursor) color))
           (setf line (cddr line))
           (cl-incf j)))
       (cl-incf (eat--t-cur-sixel-x cursor))))
    (let ((i 5)
          (line (eat--t-cur-sixel-beg cursor)))
      (while (>= i 0)
        (setf (caar line) (max (eat--t-cur-sixel-x cursor)
                               (caar line)))
        (setf line (cddr line))
        (cl-decf i)))
    (when (= (eat--t-cur-sixel-x cursor) 1000)
      (setf (eat--t-cur-sixel-x cursor) 999))))

(defun eat--t-sixel-render-bitmap (bitmap)
  "Render BITMAP.

CHAR-SIZE is the width and height of a character."
  (let ((char-size (cons (length (aref bitmap 0)) (length bitmap))))
    (pcase-exhaustive (eat--t-term-sixel-render-format eat--t-term)
      ('none)
      ('background
       (when-let* ((color (aref (aref bitmap 0) 0)))
         (put-text-property (point) (1+ (point)) 'face
                            `(:background ,color))))
      ('half-block
       (let ((fg (aref (aref bitmap (/ (cdr char-size) 2)) 0))
             (bg (aref (aref bitmap 0) 0)))
         (when (or fg bg)
           (put-text-property
            (point) (1+ (point)) 'display
            (propertize
             "▄" 'face
             `(,@(and bg `(:background ,bg))
               :foreground ,(or fg (face-background 'default))))))))
      ('svg
       (put-text-property
        (point) (1+ (point)) 'display
        `(image
          :type svg
          :data ,(apply
                  #'concat
                  (format "<svg width=\"%i\" height=\"%i\""
                          (car char-size) (cdr char-size))
                  " version=\"1.1\""
                  " xmlns=\"http://www.w3.org/2000/svg\""
                  " xmlns:xlink=\"http://www.w3.org/1999/xlink\">"
                  (let ((strs '("</svg>")))
                    (dotimes (i (cdr char-size))
                      (dotimes (j (car char-size))
                        (when-let* ((color (aref (aref bitmap i) j)))
                          (push
                           (concat
                            "<rect width=\"1\" height=\"1\""
                            (format " x=\"%i\" y=\"%i\"" j i)
                            (format " fill=\"%s\"></rect>" color))
                           strs))))
                    strs))
          ,@(eat--t-term-sixel-image-extra-props eat--t-term))))
      ('xpm
       (put-text-property
        (point) (1+ (point)) 'display
        `(image
          :type xpm
          :data ,(let ((color-map nil)
                       (pixmap nil)
                       (color-key-length
                        (length (format "%x" (* (car char-size)
                                                (cdr char-size))))))
                   (dotimes (i (cdr char-size))
                     (push nil pixmap)
                     (dotimes (j (car char-size))
                       (let ((idx (format
                                   (format "%%0%ix" color-key-length)
                                   (+ (* i (car char-size)) j)))
                             (color (or (aref (aref bitmap i) j)
                                        "None")))
                         (push (format "%s c %s" idx color) color-map)
                         (push idx (car pixmap)))))
                   (concat
                    "/* XPM */\n"
                    "static char * XFACE[] = {\n"
                    (format "\"%i %i %i %i\",\n" (car char-size)
                            (cdr char-size) (* (car char-size)
                                               (cdr char-size))
                            color-key-length)
                    (mapconcat (lambda (line)
                                 (format "\"%s\",\n" line))
                               color-map "")
                    (mapconcat (lambda (row)
                                 (format "\"%s\"" (string-join
                                                   (nreverse row))))
                               (nreverse pixmap) ",\n")
                    "\n};"))
          ,@(eat--t-term-sixel-image-extra-props eat--t-term)))))))

(defun eat--t-sixel-flush-line (nullify)
  "Flush current (not Sixel) line to the display.

If NULLIFY is non-nil, nullify flushed part of Sixel buffer."
  (let* ((disp (eat--t-term-display eat--t-term))
         (cursor (eat--t-disp-cursor disp))
         (sixel-col-count 0)
         (char-count 0)
         (lines [])
         (char-size (cons (eat--t-term-char-width eat--t-term)
                          (eat--t-term-char-height eat--t-term))))
    (when (< (length lines) (cdr char-size))
      (setq lines (make-vector (cdr char-size) nil)))
    (let ((line (eat--t-term-sixel-buffer eat--t-term)))
      (dotimes (i (cdr char-size))
        (setq sixel-col-count (max sixel-col-count (caar line)))
        (aset lines i (car line))
        (setf line (cddr line))))
    (setq char-count
          (min
           (/ (+ sixel-col-count (1- (car char-size)))
              (car char-size))
           (- (eat--t-disp-width disp) (1- (eat--t-cur-x cursor)))))
    (save-excursion
      (let ((j 0))
        (dotimes (_ char-count)
          (unless (equal (get-text-property
                          (point) 'eat--t-sixel-bitmap-size)
                         char-size)
            (let ((color
                   (unless (memq (char-after (point)) '(?\n nil))
                     (plist-get (get-text-property (point) 'face)
                                :background)))
                  (bitmap (make-vector (cdr char-size) nil)))
              (dotimes (i (cdr char-size))
                (aset bitmap i (make-vector (car char-size) color)))
              (insert
               (propertize " " 'eat--t-sixel-bitmap-size char-size
                           'eat--t-sixel-bitmap bitmap))
              (unless (memq (char-after (point)) '(?\n nil))
                (delete-region (point) (1+ (point))))
              (backward-char)))
          (let ((bitmap (get-text-property
                         (point) 'eat--t-sixel-bitmap))
                (i 0))
            (while (and (< i (car char-size))
                        (< j 1000))
              (dotimes (k (cdr char-size))
                (when-let* ((color (aref (cdr (aref lines k)) j)))
                  (setf (aref (aref bitmap k) i) color)))
              (cl-incf i)
              (cl-incf j))
            (eat--t-sixel-render-bitmap bitmap))
          (forward-char)
          (eat--t-fix-partial-multi-col-char 'preserve-face))))
    (dotimes (_ (cdr char-size))
      (let ((line (eat--t-term-sixel-buffer eat--t-term)))
        (when nullify
          (cl-loop for i from 0 to (1- (caar line))
                   do (aset (cdar line) i nil))
          (setf (caar line) 0))
        (setf (eat--t-term-sixel-buffer eat--t-term) (cddr line))))
    (cl-decf (eat--t-cur-sixel-y cursor) (cdr char-size))))

(defun eat--t-sixel-newline ()
  "Move to a new Sixel line."
  (let ((cursor (eat--t-disp-cursor
                 (eat--t-term-display eat--t-term))))
    (setf (eat--t-cur-sixel-x cursor) 0)
    (cl-incf (eat--t-cur-sixel-y cursor) 6)
    (dotimes (_ 6)
      (setf (eat--t-cur-sixel-beg cursor)
            (cddr (eat--t-cur-sixel-beg cursor))))
    (while (>= (eat--t-cur-sixel-y cursor)
               (eat--t-term-char-height eat--t-term))
      (eat--t-sixel-flush-line 'nullify)
      (if (eat--t-term-sixel-scroll-mode eat--t-term)
          (eat--t-index)
        (eat--t-cur-down)))))

(defun eat--t-sixel-set-color-reg (reg spec)
  "Set Sixel color register REG as described by SPEC."
  (when (<= reg 255)
    (let ((color
           (cond
            ((= (car spec) 1)
             (when (and (<= (nth 1 spec) 360)
                        (<= (nth 2 spec) 100)
                        (<= (nth 3 spec) 100))
               (let ((rgb (color-hsl-to-rgb (/ (nth 1 spec) 360.0)
                                            (/ (nth 3 spec) 100.0)
                                            (/ (nth 2 spec) 100.0))))
                 (color-rgb-to-hex (nth 0 rgb) (nth 1 rgb)
                                   (nth 2 rgb) 2))))
            ((= (car spec) 2)
             (when (and (<= (nth 1 spec) 100)
                        (<= (nth 2 spec) 100)
                        (<= (nth 3 spec) 100))
               (color-rgb-to-hex (/ (nth 1 spec) 100.0)
                                 (/ (nth 2 spec) 100.0)
                                 (/ (nth 3 spec) 100.0) 2))))))
      (when color
        (aset (eat--t-term-sixel-palette eat--t-term) reg color)))))

(defun eat--t-sixel-cleanup ()
  "Cleanup before potential exit from Sixel mode."
  (cl-letf* ((cursor (eat--t-disp-cursor
                      (eat--t-term-display eat--t-term)))
             ((eat--t-cur-sixel-y cursor) (eat--t-cur-sixel-y cursor))
             ((eat--t-term-sixel-buffer eat--t-term)
              (eat--t-term-sixel-buffer eat--t-term)))
    (while (>= (eat--t-cur-sixel-y cursor) -5)
      (eat--t-sixel-flush-line nil)
      (if (eat--t-term-sixel-scroll-mode eat--t-term)
          (eat--t-index)
        (eat--t-cur-down))))
  (unless (eat--t-term-sixel-scroll-mode eat--t-term)
    (eat--t-goto
     (car (eat--t-term-sixel-initial-cursor-pos eat--t-term))
     (cdr (eat--t-term-sixel-initial-cursor-pos eat--t-term)))))

(defun eat--t-sixel-enable-scrolling ()
  "Enable Sixel scrolling mode."
  (setf (eat--t-term-sixel-scroll-mode eat--t-term) t))

(defun eat--t-sixel-disable-scrolling ()
  "Disable Sixel scrolling mode."
  (setf (eat--t-term-sixel-scroll-mode eat--t-term) nil))

(defun eat--t-ui-cmd (cmd)
  "Call UI's UIC handler to handle CMD."
  (funcall (eat--t-term-ui-cmd-fn eat--t-term) eat--t-term cmd))

(defun eat--t-set-modes (params format)
  "Set modes according to PARAMS in format FORMAT."
  ;; Dispatch the request to appropriate function.
  (pcase format
    ('nil
     (while params
       (pcase (pop params)
         ('(4)
          (eat--t-insert-mode)))))
    (??
     (while params
       (pcase (pop params)
         ('(1)
          (eat--t-enable-keypad))
         ('(7)
          (eat--t-enable-auto-margin))
         ('(9)
          (eat--t-enable-x10-mouse))
         ('(12)
          (eat--t-blinking-cursor))
         ('(25)
          (eat--t-show-cursor))
         ('(80)
          (eat--t-sixel-disable-scrolling))
         ('(1000)
          (eat--t-enable-normal-mouse))
         ('(1002)
          (eat--t-enable-button-event-mouse))
         ('(1003)
          (eat--t-enable-any-event-mouse))
         ('(1004)
          (eat--t-enable-focus-event))
         ('(1006)
          (eat--t-enable-sgr-mouse-encoding))
         ('(1048)
          (eat--t-save-cur))
         (`(,(or 1047 1049))
          (eat--t-enable-alt-disp))
         ('(2004)
          (eat--t-enable-bracketed-yank)))))))

(defun eat--t-reset-modes (params format)
  "Reset modes according to PARAMS in format FORMAT."
  ;; Dispatch the request to appropriate function.
  (pcase format
    ('nil
     (while params
       (pcase (pop params)
         ('(4)
          (eat--t-replace-mode)))))
    (??
     (while params
       (pcase (pop params)
         ('(1)
          (eat--t-disable-keypad))
         ('(7)
          (eat--t-disable-auto-margin))
         ('(12)
          (eat--t-non-blinking-cursor))
         ('(25)
          (eat--t-hide-cursor))
         ('(80)
          (eat--t-sixel-enable-scrolling))
         (`(,(or 9 1000 1002 1003))
          (eat--t-disable-mouse))
         ('(1004)
          (eat--t-disable-focus-event))
         ('(1006)
          (eat--t-disable-sgr-mouse-encoding))
         ('(1047)
          (eat--t-disable-alt-disp 'dont-move-cursor))
         ('(1048)
          (eat--t-restore-cur))
         ('(1049)
          (eat--t-disable-alt-disp))
         ('(2004)
          (eat--t-disable-bracketed-yank)))))))

(defun eat--t-handle-output (output)
  "Parse and evaluate OUTPUT."
  (let ((index 0))
    (while (/= index (length output))
      (pcase-exhaustive (eat--t-term-parser-state eat--t-term)
        ('nil
         (let ((ins-beg index))
           (while (and (/= index (length output))
                       (not (memq (aref output index)
                                  '( ?\0 ?\a ?\b ?\t ?\n ?\v ?\f ?\r
                                     ?\C-n ?\C-o ?\e #x7f))))
             (cl-incf index))
           (when (/= ins-beg index)
             ;; Insert.
             (eat--t-write output ins-beg index))
           (when (/= index (length output))
             ;; Dispatch control sequence.
             (cl-incf index)
             (pcase (aref output (1- index))
               (?\a
                (eat--t-bell))
               (?\b
                (eat--t-cur-left 1))
               (?\t
                (eat--t-horizontal-tab 1))
               (?\n
                (eat--t-line-feed))
               (?\v
                (eat--t-index))
               (?\f
                (eat--t-form-feed))
               (?\r
                ;; Avoid going to line home just before a line feed,
                ;; we can just insert a new line if we are at the
                ;; end of display.
                (unless (and (/= index (length output))
                             (= (aref output index) ?\n))
                  (eat--t-carriage-return)))
               (?\C-n
                (eat--t-change-charset 'g1))
               (?\C-o
                (eat--t-change-charset 'g0))
               (?\e
                (1value (setf (eat--t-term-parser-state eat--t-term)
                              '(read-esc))))
               ;; Others are ignored.
               ))))
        ('(read-esc)
         (let ((type (aref output index)))
           (cl-incf index)
           (1value (setf (eat--t-term-parser-state eat--t-term) nil))
           ;; Dispatch control sequence.
           (pcase type
             ;; ESC (.
             (?\(
              (setf (eat--t-term-parser-state eat--t-term)
                    '(read-charset-standard g0 "")))
             ;; ESC ).
             (?\)
              (setf (eat--t-term-parser-state eat--t-term)
                    '(read-charset-standard g1 "")))
             ;; ESC *.
             (?*
              (setf (eat--t-term-parser-state eat--t-term)
                    '(read-charset-standard g2 "")))
             ;; ESC +.
             (?+
              (setf (eat--t-term-parser-state eat--t-term)
                    '(read-charset-standard g3 "")))
             ;; ESC -.
             (?-
              (setf (eat--t-term-parser-state eat--t-term)
                    '(read-charset-vt300 g1 "")))
             ;; ESC ..
             (?.
              (setf (eat--t-term-parser-state eat--t-term)
                    '(read-charset-vt300 g2 "")))
             ;; ESC /.
             (?/
              (setf (eat--t-term-parser-state eat--t-term)
                    '(read-charset-vt300 g3 "")))
             ;; ESC 7.
             (?7
              (eat--t-save-cur))
             ;; ESC 8.
             (?8
              (eat--t-restore-cur))
             ;; ESC D.
             (?D
              (eat--t-index))
             ;; ESC E.
             (?E
              (eat--t-line-feed))
             ;; ESC M.
             (?M
              (eat--t-reverse-index))
             ;; ESC P, or DCS.
             (?P
              (1value (setf (eat--t-term-parser-state eat--t-term)
                            `(read-dcs-params (read-dcs-function)
                                              ,(list nil)))))
             ;; ESC X, or SOS.
             (?X
              (1value (setf (eat--t-term-parser-state eat--t-term)
                            '(read-sos ""))))
             ;; ESC [, or CSI.
             (?\[
              (1value (setf (eat--t-term-parser-state eat--t-term)
                            '(read-csi-format))))
             ;; ESC ], or OSC.
             (?\]
              (1value (setf (eat--t-term-parser-state eat--t-term)
                            '(read-osc ""))))
             ;; ESC ^, or PM.
             (?^
              (1value (setf (eat--t-term-parser-state eat--t-term)
                            '(read-pm ""))))
             ;; ESC _, or APC.
             (?_
              (1value (setf (eat--t-term-parser-state eat--t-term)
                            '(read-apc ""))))
             ;; ESC c.
             (?c
              (eat--t-reset))
             ;; ESC n.
             (?n
              (eat--t-change-charset 'g2))
             ;; ESC o.
             (?o
              (eat--t-change-charset 'g3)))))
        ('(read-csi-format)
         (let ((format nil))
           (pcase (aref output index)
             (??
              (setq format ??)
              (cl-incf index))
             (?>
              (setq format ?>)
              (cl-incf index))
             (?=
              (setq format ?=)
              (cl-incf index)))
           (setf (eat--t-term-parser-state eat--t-term)
                 `(read-csi-params ,format ,(list (list nil))))))
        (`(read-csi-params ,format ,params)
         ;; Interpretion of the parameter depends on `format' and
         ;; other things (including things we haven't gotten yet)
         ;; according to the standard.  We don't recognize any other
         ;; format of parameters, so we can skip any checks.
         (let ((loop t))
           (while loop
             (cond
              ((= index (length output))
               ;; Output exhausted.  We need to wait for more.
               (setf (eat--t-term-parser-state eat--t-term)
                     `(read-csi-params ,format ,params))
               (setq loop nil))
              ((not (<= ?0 (aref output index) ?\;))
               ;; End of parameters.
               ;; NOTE: All parameter and their parts are in reverse
               ;; order!
               (setf (eat--t-term-parser-state eat--t-term)
                     `(read-csi-function ,format ,params nil))
               (setq loop nil))
              (t
               (cond
                ((= (aref output index) ?:)
                 ;; New parameter substring.
                 (push nil (car params)))
                ((= (aref output index) ?\;)
                 ;; New parameter.
                 (push (list nil) params))
                (t                    ; (<= ?0 (aref output index) ?9)
                 ;; Number, save it.
                 (setf (caar params)
                       (+ (* (or (caar params) 0) 10)
                          (- (aref output index) #x30)))))
               (cl-incf index))))))
        (`(read-csi-function ,format ,params ,function)
         (let ((loop t))
           (while loop
             (cond
              ((= index (length output))
               (setf (eat--t-term-parser-state eat--t-term)
                     `(read-csi-function ,format ,params ,function))
               (setq loop nil)))
             (push (aref output index) function)
             (cl-incf index)
             (when (<= ?@ (car function) ?~)
               ;; Now we have enough information to execute it!
               (setq loop nil)
               (setf (eat--t-term-parser-state eat--t-term) nil)
               ;; NOTE: `function' and `params' are in reverse order!
               (pcase (list function format params)
                 ;; CSI <n> @.
                 (`((?@) nil ((,n)))
                  (eat--t-insert-char n))
                 ;; CSI <n> A.
                 ;; CSI <n> k.
                 (`((,(or ?A ?k)) nil ((,n)))
                  (eat--t-cur-up n))
                 ;; CSI <n> B.
                 ;; CSI <n> e.
                 (`((,(or ?B ?e)) nil ((,n)))
                  (eat--t-cur-down n))
                 ;; CSI <n> C.
                 ;; CSI <n> a.
                 (`((,(or ?C ?a)) nil ((,n)))
                  (eat--t-cur-right n))
                 ;; CSI <n> D.
                 ;; CSI <n> j.
                 (`((,(or ?D ?j)) nil ((,n)))
                  (eat--t-cur-left n))
                 ;; CSI <n> E.
                 (`((?E) nil ((,n)))
                  (eat--t-beg-of-prev-line n))
                 ;; CSI <n> F.
                 (`((?F) nil ((,n)))
                  (eat--t-beg-of-next-line n))
                 ;; CSI <n> G.
                 ;; CSI <n> `.
                 (`((,(or ?G ?`)) nil ((,n)))
                  (eat--t-cur-horizontal-abs n))
                 ;; CSI <n> ; <m> H
                 ;; CSI <n> ; <m> f
                 (`((,(or ?H ?f)) nil ,(and (pred listp) params))
                  (eat--t-goto (caadr params) (caar params)))
                 ;; CSI <n> I.
                 (`((?I) nil ((,n)))
                  (eat--t-horizontal-tab n))
                 ;; CSI <n> J.
                 (`((?J) nil ((,n)))
                  (eat--t-erase-in-disp n))
                 ;; CSI <n> K.
                 (`((?K) nil ((,n)))
                  (eat--t-erase-in-line n))
                 ;; CSI <n> L.
                 (`((?L) nil ((,n)))
                  (eat--t-insert-line n))
                 ;; CSI <n> M.
                 (`((?M) nil ((,n)))
                  (eat--t-delete-line n))
                 ;; CSI <n> P.
                 (`((?P) nil ((,n)))
                  (eat--t-delete-char n))
                 ;; CSI <n> S.
                 (`((?S) nil ((,n)))
                  (eat--t-scroll-up n))
                 ;; CSI ? <n> ; <m> ; ... S.
                 (`((?S) ?? ,(or `((,_) (,operation) (,attr))
                                 `((,_) (,_) (,operation) (,attr))))
                  (eat--t-send-graphics-attrs attr operation))
                 ;; CSI <n> T.
                 (`((?T) nil ((,n)))
                  (eat--t-scroll-down n))
                 ;; CSI <n> X.
                 (`((?X) nil ((,n)))
                  (eat--t-erase-char n))
                 ;; CSI <n> Z.
                 (`((?Z) nil ((,n)))
                  (eat--t-horizontal-backtab n))
                 ;; CSI <n> b.
                 (`((?b) nil ((,n)))
                  (eat--t-repeat-last-char n))
                 ;; CSI <n> c.
                 ;; CSI > <n> c.
                 (`((?c) ,format ((,n)))
                  (eat--t-send-device-attrs n format))
                 ;; CSI <n> d.
                 (`((?d) nil ((,n)))
                  (eat--t-cur-vertical-abs n))
                 ;; CSI ... h.
                 ;; CSI ? ... h.
                 (`((?h) ,format ,(and (pred listp) params))
                  ;; Reverse `params' to get it into the correct
                  ;; order.
                  (setq params (nreverse params))
                  (let ((p params))
                    (while p
                      (setf (car p) (nreverse (car p)))
                      (setq p (cdr p))))
                  (eat--t-set-modes params format))
                 ;; CSI ... l.
                 ;; CSI ? ... l.
                 (`((?l) ,format ,(and (pred listp) params))
                  ;; Reverse `params' to get it into the correct
                  ;; order.
                  (setq params (nreverse params))
                  (let ((p params))
                    (while p
                      (setf (car p) (nreverse (car p)))
                      (setq p (cdr p))))
                  (eat--t-reset-modes params format))
                 ;; CSI ... m.
                 (`((?m) nil ,(and (pred listp) params))
                  ;; Reverse `params' to get it into the correct
                  ;; order.
                  (setq params (nreverse params))
                  (let ((p params))
                    (while p
                      (setf (car p) (nreverse (car p)))
                      (setq p (cdr p))))
                  (eat--t-set-sgr-params params))
                 ;; CSI 6 n.
                 (`((?n) nil ((,n)))
                  (eat--t-device-status-report n))
                 ;; CSI <n> SP q.
                 (`((?q ?\ ) nil ((,n)))
                  (eat--t-set-cursor-style n))
                 ;; CSI <n> ; <n> r.
                 (`((?r) nil ,(and (pred listp) params))
                  (eat--t-change-scroll-region (caadr params)
                                               (caar params)))
                 ;; CSI s.
                 (`((?s) nil nil)
                  (eat--t-save-cur))
                 ;; CSI u.
                 (`((?u) nil nil)
                  (eat--t-restore-cur)))))))
        (`(,(and (or 'read-sos 'read-osc 'read-pm 'read-apc) state)
           ,buf)
         ;; Find the end of string.
         (let ((match (string-match (if (eq state 'read-osc)
                                        (rx (or ?\a ?\\))
                                      (rx ?\\))
                                    output index)))
           (if (not match)
               (progn
                 ;; Not found, store the text to process it later when
                 ;; we get the end of string.
                 (setf (eat--t-term-parser-state eat--t-term)
                       `(,state ,(concat buf (substring output
                                                        index))))
                 (setq index (length output)))
             ;; Matched!  Get the string from the output and previous
             ;; runs.
             (let ((str (concat buf (substring output index
                                               match))))
               (setq index (match-end 0))
               ;; Is it really the end of string?
               (if (and (= (aref output match) ?\\)
                        (not (or (zerop (length str))
                                 (= (aref str (1- (length str)))
                                    ?\e))))
                   ;; No.  Push the '\' character to process later.
                   (setf (eat--t-term-parser-state eat--t-term)
                         `(,state ,(concat str "\\")))
                 ;; Yes!  It's the end!  We can parse it.
                 (when (= (aref output match) ?\\)
                   (setq str (substring str 0 (1- (length str)))))
                 (setf (eat--t-term-parser-state eat--t-term) nil)
                 ;; Dispatch control sequence.
                 (pcase state
                   ('read-osc
                    (pcase str
                      ;; OSC 0 ; <t> ST.
                      ;; OSC 2 ; <t> ST.
                      ((rx string-start (or ?0 ?2) ?\;
                           (let title (zero-or-more anything))
                           string-end)
                       (eat--t-set-title title))
                      ;; OSC 7 ; <t> ST.
                      ((rx string-start ?7 ?\;
                           (let url (zero-or-more anything))
                           string-end)
                       (eat--t-set-cwd url))
                      ;; OSC 1 0 ; ? ST.
                      ("10;?"
                       (eat--t-report-foreground-color))
                      ;; OSC 1 1 ; ? ST.
                      ("11;?"
                       (eat--t-report-background-color))
                      ;; OSC 5 1 ; <s> ST.
                      ((rx string-start "51;"
                           (let cmd (zero-or-more anything))
                           string-end)
                       (eat--t-ui-cmd cmd))
                      ;; OSC 5 2 ; <t> ; <s> ST.
                      ((rx string-start "52;"
                           (let targets
                             (zero-or-more (not (any ?\;))))
                           ?\; (let data (zero-or-more anything))
                           string-end)
                       (eat--t-manipulate-selection
                        targets data))))))))))
        (`(read-dcs-params ,next-state ,params)
         ;; There is no standard format of device control strings, but
         ;; all DEC and XTerm DCS sequences (including those we
         ;; support) follow this particular format.
         (let ((loop t))
           (while loop
             (cond
              ((= index (length output))
               ;; Output exhausted.  We need to wait for more.
               (setf (eat--t-term-parser-state eat--t-term)
                     `(read-dcs-params ,next-state ,params))
               (setq loop nil))
              ((not (or (<= ?0 (aref output index) ?9)
                        (= (aref output index) ?\;)))
               ;; End of parameters.
               ;; NOTE: All parameter and their parts are in reverse
               ;; order!
               (setf (eat--t-term-parser-state eat--t-term)
                     `(,@next-state ,params))
               (setq loop nil))
              (t
               (if (= (aref output index) ?\;)
                   ;; New parameter.
                   (push nil params)
                 ;; Number, save it.
                 (setf (car params)
                       (+ (* (or (car params) 0) 10)
                          (- (aref output index) #x30))))
               (cl-incf index))))))
        (`(read-dcs-function ,params)
         (cl-incf index)
         (pcase (aref output (1- index))
           (?q
            (setf (eat--t-term-parser-state eat--t-term)
                  `(read-sixel init ,params)))
           (?\e
            (setf (eat--t-term-parser-state eat--t-term)
                  '(read-potential-st (read-dcs-fallback))))
           (_
            (setf (eat--t-term-parser-state eat--t-term)
                  '(read-dcs-fallback))
            (cl-decf index))))
        (`(read-potential-st ,else)
         (if (/= (aref output index) ?\\)
             (setf (eat--t-term-parser-state eat--t-term) else)
           (setf (eat--t-term-parser-state eat--t-term) nil)
           (cl-incf index)))
        (`(read-dcs-fallback)
         (let ((loop t))
           (while (and loop (/= index (length output)))
             (when (= (aref output index) ?\e)
               (setf (eat--t-term-parser-state eat--t-term)
                     '(read-potential-st (read-dcs-fallback)))
               (setq loop nil))
             (cl-incf index))))
        (`(read-sixel ,cmd ,params)
         (when cmd
           (pcase cmd
             ('init
              (eat--t-sixel-init))
             ('set-color
              (when (and (= (length params) 1)
                         (<= (or (car params) 0) 255))
                (setf (eat--t-term-sixel-color eat--t-term)
                      (or (car params) 0)))
              (when (= (length params) 5)
                (cl-destructuring-bind (z y x coord-sys reg) params
                  (eat--t-sixel-set-color-reg
                   (or reg 0) (list coord-sys (or x 0) (or y 0)
                                    (or z 0))))))
             ('rle
              (eat--t-sixel-write output index (1+ index)
                                  (or (car params) 0))
              (cl-incf index))
             ('set-raster-attr
              ;; TODO: Implement.
              ))
           (setf (eat--t-term-parser-state eat--t-term)
                 `(read-sixel nil nil)))
         (let ((loop t))
           (while (and loop (/= index (length output)))
             (if (<= ?? (aref output index) ?~)
                 (let ((ins-beg index))
                   (while (and (/= index (length output))
                               (<= ?? (aref output index) ?~))
                     (cl-incf index))
                   (eat--t-sixel-write output ins-beg index 1))
               (cl-incf index)
               (pcase (aref output (1- index))
                 (?!
                  (setf (eat--t-term-parser-state eat--t-term)
                        `(read-dcs-params (read-sixel rle)
                                          ,(list nil)))
                  (setq loop nil))
                 (?-
                  (eat--t-sixel-newline))
                 (?$
                  (setf (eat--t-cur-sixel-x
                         (eat--t-disp-cursor
                          (eat--t-term-display eat--t-term)))
                        0))
                 (?\#
                  (setf (eat--t-term-parser-state eat--t-term)
                        `(read-dcs-params (read-sixel set-color)
                                          ,(list nil)))
                  (setq loop nil))
                 (?\"
                  (setf (eat--t-term-parser-state eat--t-term)
                        `(read-dcs-params (read-sixel set-raster-attr)
                                          ,(list nil)))
                  (setq loop nil))
                 (?\e
                  (eat--t-sixel-cleanup)
                  (setf (eat--t-term-parser-state eat--t-term)
                        '(read-potential-st (read-dcs-fallback)))
                  (setq loop nil)))))))
        (`(read-charset-standard ,slot ,buf)
         ;; Find the end.
         (let ((match (string-match (rx (any ?0 ?2 ?4 ?5 ?6 ?7 ?9 ?<
                                             ?= ?> ?? ?A ?B ?C ?E ?H
                                             ?K ?Q ?R ?Y ?Z ?f))
                                    output index)))
           (if (not match)
               (progn
                 ;; Not found, store the text to process it later when
                 ;; we find the end.
                 (setf (eat--t-term-parser-state eat--t-term)
                       `(read-charset-standard
                         ,slot ,(concat buf (substring
                                             output index))))
                 (setq index (length output)))
             ;; Got the end!
             (let ((str (concat buf (substring output index
                                               (match-end 0)))))
               (setq index (match-end 0))
               (setf (eat--t-term-parser-state eat--t-term) nil)
               ;; Set the character set.
               (eat--t-set-charset
                slot
                (pcase str
                  ;; ESC ( 0.
                  ;; ESC ) 0.
                  ;; ESC * 0.
                  ;; ESC + 0.
                  ("0" 'dec-line-drawing)
                  ;; ESC ( B.
                  ;; ESC ) B.
                  ;; ESC * B.
                  ;; ESC + B.
                  ("B" 'us-ascii)))))))
        (`(read-charset-vt300 ,_slot)
         (cl-incf index)
         (setf (eat--t-term-parser-state eat--t-term) nil)
         ;; Nothing.  This is here to just recognize the sequence.
         )))))

(defun eat--t-resize (width height)
  "Resize terminal to WIDTH x HEIGHT."
  (let* ((disp (eat--t-term-display eat--t-term))
         (cursor (eat--t-disp-cursor disp))
         (old-width (eat--t-disp-width disp))
         (old-height (eat--t-disp-height disp)))
    ;; Don't do anything if size hasn't changed, or the new size is
    ;; too small.
    (when (and (not (and (eq old-width width)
                         (eq old-height height)))
               (>= width 1)
               (>= height 1))
      ;; Update state.
      (setf (eat--t-disp-width disp) width)
      (setf (eat--t-disp-height disp) height)
      (setf (eat--t-term-scroll-begin eat--t-term) 1)
      (setf (eat--t-term-scroll-end eat--t-term)
            (eat--t-disp-height disp))
      (set-marker (eat--t-cur-position cursor) (point))
      (if (eat--t-term-main-display eat--t-term)
          ;; For alternative display, just delete the part of the
          ;; display that went out of the edges.  So if the terminal
          ;; was enlarged, we don't have anything to do.
          (when (or (< width old-width)
                    (< height old-height))
            ;; Go to the beginning of display.
            (goto-char (eat--t-disp-begin disp))
            (let ((l 0))
              (while (and (< l height) (not (eobp)))
                (eat--t-col-motion width)
                (delete-region (point) (car (eat--t-eol)))
                (unless (eobp)
                  (if (< (1+ l) height)
                      (forward-char)
                    (delete-region (point) (point-max))
                    (let ((y (eat--t-cur-y cursor))
                          (x (eat--t-cur-x cursor)))
                      (eat--t-goto 1 1)
                      (eat--t-goto y x))))
                (cl-incf l))))
        ;; REVIEW: This works, but it is very simple.  Most
        ;; terminals have more sophisticated mechanisms to do this.
        ;; It would be nice thing have them here.
        ;; Go to the beginning of display.
        (goto-char (eat--t-disp-begin disp))
        ;; Try to move to the end of previous line, maybe that's a
        ;; part of a too long line.
        (unless (bobp)
          (backward-char))
        ;; Join all long lines.
        (while (not (eobp))
          (eat--t-join-long-line))
        ;; Go to display beginning again and break long lines.
        (goto-char (eat--t-disp-begin disp))
        (while (not (eobp))
          (eat--t-break-long-line (eat--t-disp-width disp)))
        ;; Calculate the beginning position of display.
        (goto-char (point-max))
        ;; TODO: This part needs explanation.
        (let ((disp-begin (car (eat--t-bol (- (1- height))))))
          (when (< (eat--t-disp-begin disp) disp-begin)
            (goto-char (max (- (eat--t-disp-begin disp) 1)
                            (point-min)))
            (set-marker (eat--t-disp-begin disp) disp-begin)
            (while (< (point) (1- (eat--t-disp-begin disp)))
              (eat--t-join-long-line
               (1- (eat--t-disp-begin disp))))))
        ;; Update the cursor if needed.
        (when (< (eat--t-cur-position cursor)
                 (eat--t-disp-begin disp))
          (set-marker (eat--t-cur-position cursor)
                      (eat--t-disp-begin disp)))
        ;; Update the coordinates of cursor.
        (goto-char (eat--t-cur-position cursor))
        (setf (eat--t-cur-x cursor) (1+ (eat--t-current-col)))
        (goto-char (eat--t-disp-begin disp))
        (setf (eat--t-cur-y cursor)
              (let ((y 0))
                (while (< (point) (eat--t-cur-position cursor))
                  (condition-case nil
                      (search-forward
                       "\n" (eat--t-cur-position cursor))
                    (search-failed
                     (goto-char (eat--t-cur-position cursor))))
                  (cl-incf y))
                (when (or (= (point) (point-min))
                          (= (char-before) ?\n))
                  (cl-incf y))
                (max y 1)))))))

;;;###autoload
(defun eat-term-make (buffer position)
  "Make a Eat terminal at POSITION in BUFFER."
  (eat--t-make-term
   :buffer buffer
   :begin (copy-marker position t)
   :end (copy-marker position)
   :display (eat--t-make-disp
             :begin (copy-marker position)
             :old-begin (copy-marker position)
             :cursor (eat--t-make-cur
                      :position (copy-marker position)))))

(defun eat-term-p (object)
  "Return non-nil if OBJECT is a Eat terminal."
  (eat--t-term-p object))

(defun eat-term-live-p (object)
  "Return non-nil if OBJECT is a live Eat terminal."
  (and (eat-term-p object)
       (not (not (eat--t-term-buffer object)))))

(defmacro eat--t-ensure-live-term (object)
  "Signal error if OBJECT is not a live Eat terminal."
  `(unless (eat-term-live-p ,object)
     (error "%s is not a live Eat terminal"
            ,(upcase (symbol-name object)))))

(defmacro eat--t-with-env (terminal &rest body)
  "Setup the environment for TERMINAL and eval BODY in it."
  (declare (indent 1))
  `(let ((eat--t-term ,terminal))
     (eat--t-ensure-live-term ,terminal)
     (with-current-buffer (eat--t-term-buffer eat--t-term)
       (save-excursion
         (save-restriction
           (narrow-to-region (eat--t-term-begin eat--t-term)
                             (eat--t-term-end eat--t-term))
           (goto-char (eat--t-cur-position
                       (eat--t-disp-cursor
                        (eat--t-term-display eat--t-term))))
           (unwind-protect
               (progn ,@body)
             (set-marker (eat--t-cur-position
                          (eat--t-disp-cursor
                           (eat--t-term-display eat--t-term)))
                         (point))
             (set-marker (eat--t-term-begin eat--t-term) (point-min))
             (set-marker (eat--t-term-end eat--t-term)
                         (point-max))))))))

(defun eat-term-delete (terminal)
  "Delete TERMINAL and do any cleanup to do."
  (eat--t-ensure-live-term terminal)
  (let ((inhibit-quit t)
        (eat--t-term terminal))
    (with-current-buffer (eat--t-term-buffer eat--t-term)
      (save-excursion
        (save-restriction
          (narrow-to-region (eat--t-term-begin eat--t-term)
                            (eat--t-term-end eat--t-term))
          (eat--t-set-cursor-state :default)
          ;; Go to the beginning of display.
          (goto-char (eat--t-disp-begin
                      (eat--t-term-display eat--t-term)))
          ;; Join all long lines.
          (unless (bobp)
            (backward-char))
          (while (not (eobp))
            (eat--t-join-long-line)))))
    (setf (eat--t-term-buffer eat--t-term) nil)))

(defun eat-term-reset (terminal)
  "Reset TERMINAL."
  (let ((inhibit-quit t))
    (eat--t-with-env terminal
      (eat--t-reset))))

(defun eat-term-parameter (terminal parameter)
  "Return the value of parameter PARAMETER of TERMINAL."
  (eat--t-ensure-live-term terminal)
  (gethash parameter (eat--t-term-params terminal)))

(defun eat-term-parameters (terminal)
  "Return the parameter-alist of TERMINAL."
  (eat--t-ensure-live-term terminal)
  (let ((alist nil))
    (maphash (lambda (key val) (push (cons key val) alist))
             (eat--t-term-params terminal))))

(defun eat-term-set-parameter (terminal parameter value)
  "Set the value of parameter PARAMETER of TERMINAL to VALUE."
  (eat--t-ensure-live-term terminal)
  ;; Handle special parameters, and reject invalid values.
  (pcase parameter
    ('input-function
     (unless (functionp value)
       (signal 'wrong-type-argument (list 'functionp value)))
     (setf (eat--t-term-input-fn terminal) value))
    ('ring-bell-function
     (unless (functionp value)
       (signal 'wrong-type-argument (list 'functionp value)))
     (setf (eat--t-term-bell-fn terminal) value))
    ('set-cursor-function
     (unless (functionp value)
       (signal 'wrong-type-argument (list 'functionp value)))
     (setf (eat--t-term-set-cursor-fn terminal) value))
    ('grab-mouse-function
     (unless (functionp value)
       (signal 'wrong-type-argument (list 'functionp value)))
     (setf (eat--t-term-grab-mouse-fn terminal) value))
    ('grab-focus-events-function
     (unless (functionp value)
       (signal 'wrong-type-argument (list 'functionp value)))
     (setf (eat--t-term-set-focus-ev-mode-fn terminal) value))
    ('manipulate-selection-function
     (unless (functionp value)
       (signal 'wrong-type-argument (list 'functionp value)))
     (setf (eat--t-term-manipulate-selection-fn terminal) value))
    ('set-title-function
     (unless (functionp value)
       (signal 'wrong-type-argument (list 'functionp value)))
     (setf (eat--t-term-set-title-fn terminal) value))
    ('set-cwd-function
     (unless (functionp value)
       (signal 'wrong-type-argument (list 'functionp value)))
     (setf (eat--t-term-set-cwd-fn terminal) value))
    ('ui-command-function
     (unless (functionp value)
       (signal 'wrong-type-argument (list 'functionp value)))
     (setf (eat--t-term-ui-cmd-fn terminal) value))
    ('char-dimensions
     (unless (and (consp value)
                  (integerp (car value))
                  (> (car value) 0)
                  (integerp (cdr value))
                  (> (cdr value) 0))
       (signal 'wrong-type-argument (list 'consp value)))
     (setf (eat--t-term-char-width terminal) (car value))
     (setf (eat--t-term-char-height terminal) (cdr value)))
    ('sixel-render-format
     (unless (memq value '(background half-block svg xpm none))
       (error "`sixel-render-format' parameter must be set to one of \
the supported formats"))
     (setf (eat--t-term-sixel-render-format terminal) value))
    ('sixel-image-extra-properties
     (setf (eat--t-term-sixel-image-extra-props terminal) value))
    ('bold-face
     (unless (and (symbolp value) (facep value))
       (signal 'wrong-type-argument (list '(symbolp facep) value)))
     (setf (eat--t-term-bold-face terminal) value))
    ('faint-face
     (unless (and (symbolp value) (facep value))
       (signal 'wrong-type-argument (list '(symbolp facep) value)))
     (setf (eat--t-term-faint-face terminal) value))
    ('italic-face
     (unless (and (symbolp value) (facep value))
       (signal 'wrong-type-argument (list '(symbolp facep) value)))
     (setf (eat--t-term-italic-face terminal) value))
    ('slow-blink-face
     (unless (and (symbolp value) (facep value))
       (signal 'wrong-type-argument (list '(symbolp facep) value)))
     (setf (eat--t-term-slow-blink-face terminal) value))
    ('fast-blink-face
     (unless (and (symbolp value) (facep value))
       (signal 'wrong-type-argument (list '(symbolp facep) value)))
     (setf (eat--t-term-fast-blink-face terminal) value))
    ((and (pred symbolp)
          (let (rx string-start "color-"
                   (let number (one-or-more (any (?0 . ?9))))
                   "-face" string-end)
            (symbol-name parameter))
          (let (and (pred (<= 0))
                    (pred (>= 255))
                    index)
            (string-to-number number)))
     (unless (and (symbolp value) (facep value))
       (signal 'wrong-type-argument (list '(symbolp facep) value)))
     (setf (aref (eat--t-term-color-faces terminal) index)
           value))
    ((and (pred symbolp)
          (let (rx string-start "font-"
                   (let number (one-or-more (any (?0 . ?9))))
                   "-face" string-end)
            (symbol-name parameter))
          (let (and (pred (<= 0))
                    (pred (>= 255))
                    index)
            (string-to-number number)))
     (unless (and (symbolp value) (facep value))
       (signal 'wrong-type-argument (list '(symbolp facep) value)))
     (setf (aref (eat--t-term-font-faces terminal) index)
           value)))
  ;; Set the parameter.
  (puthash parameter value (eat--t-term-params terminal)))

(gv-define-setter eat-term-parameter (value terminal parameter)
  `(eat-term-set-parameter ,terminal ,parameter ,value))

(defun eat-term-cursor-type (terminal)
  "Return the cursor state of TERMINAL.

The return value can be one of the following:

  `:invisible'          Invisible cursor.
  `:block'              Block (filled box) cursor (default).
  `:blinking-block'     Blinking block cursor.
  `:bar'                Vertical bar cursor.
  `:blinking-bar'       Blinking vertical bar cursor.
  `:underline'          Horizontal bar cursor.
  `:blinking-underline' Blinking horizontal bar cursor."
  (eat--t-ensure-live-term terminal)
  (if (eat--t-term-cur-visible-p terminal)
      (eat--t-term-cur-state terminal)
    :invisible))

(defun eat-term-beginning (terminal)
  "Return the beginning position of TERMINAL.

Don't use markers to store the position, call this function whenever
you need the position."
  (eat--t-ensure-live-term terminal)
  (eat--t-term-begin terminal))

(defun eat-term-end (terminal)
  "Return the end position of TERMINAL.

This is also the end position of TERMINAL's display.

Don't use markers to store the position, call this function whenever
you need the position."
  (eat--t-ensure-live-term terminal)
  (eat--t-term-end terminal))

(defun eat-term-display-beginning (terminal)
  "Return the beginning position of TERMINAL's display."
  (eat--t-ensure-live-term terminal)
  (eat--t-disp-begin (eat--t-term-display terminal)))

(defun eat-term-display-cursor (terminal)
  "Return the cursor's current position on TERMINAL's display."
  (eat--t-ensure-live-term terminal)
  (let* ((disp (eat--t-term-display terminal))
         (cursor (eat--t-disp-cursor disp)))
    ;; The cursor might be after the edge of the display.  But we
    ;; don't want the UI to show that, so show cursor at the edge.
    (if (> (eat--t-cur-x cursor) (eat--t-disp-width disp))
        (1- (eat--t-cur-position cursor))
      (eat--t-cur-position cursor))))

(defun eat-term-title (terminal)
  "Return the current title of TERMINAL."
  (eat--t-ensure-live-term terminal)
  (eat--t-term-title terminal))

(defun eat-term-size (terminal)
  "Return the size of TERMINAL as (WIDTH . HEIGHT)."
  (eat--t-ensure-live-term terminal)
  (let ((disp (eat--t-term-display terminal)))
    (cons (eat--t-disp-width disp) (eat--t-disp-height disp))))

(defun eat-term-process-output (terminal output)
  "Process OUTPUT from client and show it on TERMINAL's display."
  (let ((inhibit-quit t))
    (eat--t-with-env terminal
      (eat--t-handle-output output))))

(defun eat-term-redisplay (terminal)
  "Prepare TERMINAL for displaying."
  (let ((inhibit-quit t))
    (eat--t-with-env terminal
      (let ((disp (eat--t-term-display eat--t-term)))
        (when (< (eat--t-disp-old-begin disp)
                 (eat--t-disp-begin disp))
          ;; Join long lines.
          (let ((limit (copy-marker (1- (eat--t-disp-begin disp)))))
            (save-excursion
              (goto-char (max (1- (eat--t-disp-old-begin disp))
                              (point-min)))
              (while (< (point) limit)
                (eat--t-join-long-line limit))))
          ;; Truncate scrollback.
          (when eat-term-scrollback-size
            (delete-region
             (point-min)
             (max (point-min) (- (point) eat-term-scrollback-size))))
          (set-marker (eat--t-disp-old-begin disp)
                      (eat--t-disp-begin disp)))))))

(defun eat-term-resize (terminal width height)
  "Resize TERMINAL to WIDTH x HEIGHT."
  (let ((inhibit-quit t))
    (eat--t-with-env terminal
      (eat--t-resize width height))))

(defun eat-term-in-alternative-display-p (terminal)
  "Return non-nil when TERMINAL is in alternative display mode."
  (eat--t-ensure-live-term terminal)
  (eat--t-term-main-display terminal))

(defun eat-term-input-event (terminal n event &optional ref-pos)
  "Send EVENT as input N times to TERMINAL.

EVENT should be a event.  It can be any standard Emacs event, or a
event list of any of the following forms:

  (eat-focus-in)
    Terminal just gained focus.

  (eat-focus-out)
    Terminal just lost focus.

REF-POS is a mouse position list pointing to the start of terminal
display satisfying the predicate `posnp'.  It is used to calculate the
position of mouse events and `eat-mouse-drag' events on terminal when
given.

For mouse events, events should be sent on both mouse button press and
release unless the mouse grabing mode is `:click', otherwise the
client process may get confused."
  (eat--t-ensure-live-term terminal)
  (let ((disp (eat--t-term-display terminal)))
    (cl-flet ((send (str)
                (funcall (eat--t-term-input-fn terminal)
                         terminal str)))
      (dotimes (_ (or n 1))
        (pcase event
          ;; Arrow key, `insert', `delete', `deletechar', `home',
          ;; `end', `prior', `next' and their modifier variants.
          ((and (or 'up 'down 'right 'left
                    'C-up 'C-down 'C-right 'C-left
                    'M-up 'M-down 'M-right 'M-left
                    'S-up 'S-down 'S-right 'S-left
                    'C-M-up 'C-M-down 'C-M-right 'C-M-left
                    'C-S-up 'C-S-down 'C-S-right 'C-S-left
                    'M-S-up 'M-S-down 'M-S-right 'M-S-left
                    'C-M-S-up 'C-M-S-down 'C-M-S-right 'C-M-S-left
                    'insert 'C-insert 'M-insert 'S-insert 'C-M-insert
                    'C-S-insert 'M-S-insert 'C-M-S-insert
                    'delete 'C-delete 'M-delete 'S-delete 'C-M-delete
                    'C-S-delete 'M-S-delete 'C-M-S-delete
                    'deletechar 'C-deletechar 'M-deletechar
                    'S-deletechar 'C-M-deletechar 'C-S-deletechar
                    'M-S-deletechar 'C-M-S-deletechar
                    'home 'C-home 'M-home 'S-home 'C-M-home 'C-S-home
                    'M-S-home 'C-M-S-home
                    'end 'C-end 'M-end 'S-end 'C-M-end 'C-S-end
                    'M-S-end 'C-M-S-end
                    'prior 'C-prior 'M-prior 'S-prior 'C-M-prior
                    'C-S-prior 'M-S-prior 'C-M-S-prior
                    'next 'C-next 'M-next 'S-next 'C-M-next 'C-S-next
                    'M-S-next 'C-M-S-next)
                ev)
           (send
            (format
             "\e%s%c"
             (if (not (or (memq 'control (event-modifiers ev))
                          (memq 'meta (event-modifiers ev))
                          (memq 'shift (event-modifiers ev))))
                 (pcase (event-basic-type ev)
                   ('insert "[2")
                   ((or 'delete 'deletechar) "[3")
                   ('prior "[5")
                   ('next "[6")
                   (_ (if (eat--t-term-keypad-mode terminal)
                          "O"
                        "[")))
               (format
                "[%c;%c"
                (pcase (event-basic-type ev)
                  ('insert ?2)
                  ((or 'delete 'deletechar) ?3)
                  ('prior ?5)
                  ('next ?6)
                  (_ ?1))
                (pcase-exhaustive (event-modifiers ev)
                  ((and (pred (memq 'control))
                        (pred (memq 'meta))
                        (pred (memq 'shift)))
                   ?8)
                  ((and (pred (memq 'control))
                        (pred (memq 'meta)))
                   ?7)
                  ((and (pred (memq 'control))
                        (pred (memq 'shift)))
                   ?6)
                  ((and (pred (memq 'meta))
                        (pred (memq 'shift)))
                   ?4)
                  ((pred (memq 'control))
                   ?5)
                  ((pred (memq 'meta))
                   ?3)
                  ((pred (memq 'shift))
                   ?2))))
             (pcase (event-basic-type ev)
               ('up ?A)
               ('down ?B)
               ('right ?C)
               ('left ?D)
               ('home ?H)
               ('end ?F)
               (_ ?~)))))
          ((or 'backspace ?\C-?)
           (send "\C-?"))
          ('C-backspace
           (send "\C-h"))
          ((or 'M-backspace
               (pred (lambda (ev)
                       (and (equal (event-basic-type ev) ?\C-?)
                            (equal (event-modifiers ev) '(meta))))))
           (send "\e\C-?"))
          ('C-M-backspace
           (send "\e\C-h"))
          ('tab
           (send "\t"))
          ('backtab
           (send "\e[Z"))
          ;; Function keys.
          ((and (pred symbolp)
                fn-key
                (let (rx string-start "f"
                         (let fn-num (one-or-more (any (?0 . ?9))))
                         string-end)
                  (symbol-name fn-key))
                (let (and (pred (<= 1))
                          (pred (>= 63))
                          key)
                  (string-to-number fn-num)))
           (send
            (aref
             ["\eOP" "\eOQ" "\eOR" "\eOS" "\e[15~" "\e[17~" "\e[18~"
              "\e[19~" "\e[20~" "\e[21~" "\e[23~" "\e[24~" "\e[1;2P"
              "\e[1;2Q" "\e[1;2R" "\e[1;2S" "\e[15;2~" "\e[17;2~"
              "\e[18;2~" "\e[19;2~" "\e[20;2~" "\e[21;2~" "\e[23;2~"
              "\e[24;2~" "\e[1;5P" "\e[1;5Q" "\e[1;5R" "\e[1;5S"
              "\e[15;5~" "\e[17;5~" "\e[18;5~" "\e[19;5~" "\e[20;5~"
              "\e[21;5~" "\e[23;5~" "\e[24;5~" "\e[1;6P" "\e[1;6Q"
              "\e[1;6R" "\e[1;6S" "\e[15;6~" "\e[17;6~" "\e[18;6~"
              "\e[19;6~" "\e[20;6~" "\e[21;6~" "\e[23;6~" "\e[24;6~"
              "\e[1;3P" "\e[1;3Q" "\e[1;3R" "\e[1;3S" "\e[15;3~"
              "\e[17;3~" "\e[18;3~" "\e[19;3~" "\e[20;3~" "\e[21;3~"
              "\e[23;3~" "\e[24;3~" "\e[1;4P" "\e[1;4Q" "\e[1;4R"]
             (1- key))))
          ((and (or (pred numberp)
                    (pred symbolp))
                char)
           ;; Adapted from Term source.
           (when (symbolp char)
             ;; Convert `return' to C-m, etc.
             (let ((tmp (get char 'event-symbol-elements)))
               (when tmp
                 (setq char (car tmp)))
               (and (symbolp char)
                    (setq tmp (get char 'ascii-character))
                    (setq char tmp))))
           (when (numberp char)
             (let ((base (event-basic-type char))
                   (mods (event-modifiers char)))
               ;; Try to avoid event-convert-list if possible.
               (if (and (characterp char)
                        (not (memq 'meta mods))
                        (not (and (memq 'control mods)
                                  (memq 'shift mods))))
                   (send (format "%c" char))
                 (when (memq 'control mods)
                   (setq mods (delq 'shift mods)))
                 (let ((ch (pcase (event-convert-list
                                   (append (remq 'meta mods)
                                           (list base)))
                             (?\C-\s ?\C-@)
                             (?\C-/ ?\C-?)
                             (?\C-- ?\C-_)
                             (c c))))
                   (when (characterp ch)
                     (send (cond
                            ((and (memq 'meta mods)
                                  (memq ch '(?\[ ?O)))
                             "\e")
                            (t
                             (format
                              (if (memq 'meta mods) "\e%c" "%c")
                              ch))))))))))
          ;; Mouse handling.
          ((and (guard (eat--t-term-mouse-mode terminal))
                mouse
                (pred eventp)
                (or (and (let mouse-type (event-basic-type mouse))
                         (let (rx string-start "mouse-"
                                  (let key-num (one-or-more
                                                (any (?0 . ?9))))
                                  string-end)
                           (symbol-name mouse-type))
                         (let (and (pred (<= 1))
                                   (pred (>= 11))
                                   mouse-num)
                           (string-to-number key-num)))
                    (and (let 'wheel-up (event-basic-type mouse))
                         (let mouse-num 4))
                    (and (let 'wheel-down (event-basic-type mouse))
                         (let mouse-num 5))
                    (and (let 'wheel-right (event-basic-type mouse))
                         (let mouse-num 6))
                    (and (let 'wheel-left (event-basic-type mouse))
                         (let mouse-num 7))))
           (let* ((modifiers (event-modifiers mouse))
                  (pos (if (memq 'drag modifiers)
                           (event-end mouse)
                         (event-start mouse)))
                  (x-y (if (eval-when-compile
                             (< emacs-major-version 29))
                           (posn-col-row pos)
                         (with-suppressed-warnings
                             ((callargs posn-col-row))
                           (posn-col-row pos 'use-window))))
                  (x (1+ (car x-y)))
                  (y (1+ (cdr x-y)))
                  (button
                   (let ((b (aref
                             [0 1 2 64 65 66 67 128 129 130 131]
                             (1- mouse-num))))
                     (when (memq 'shift modifiers)
                       (cl-incf b 4))
                     (when (memq 'meta modifiers)
                       (cl-incf b 8))
                     (when (memq 'control modifiers)
                       (cl-incf b 16))
                     b)))
             (when ref-pos
               (let ((ref-x-y
                      (if (eval-when-compile
                            (< emacs-major-version 29))
                          (posn-col-row ref-pos)
                        (with-suppressed-warnings
                            ((callargs posn-col-row))
                          (posn-col-row ref-pos 'use-window)))))
                 (cl-decf x (car ref-x-y))
                 (cl-decf y (cdr ref-x-y))))
             (when (and (<= 1 x (eat--t-disp-width disp))
                        (<= 1 y (eat--t-disp-height disp))
                        (or (eat--t-term-mouse-encoding terminal)
                            (and (<= x 95)
                                 (<= y 95)
                                 (<= button 95))))
               (if (eq (eat--t-term-mouse-mode terminal) 'x10)
                   (when (and (< button 3)
                              (or (memq 'click modifiers)
                                  (memq 'drag modifiers)))
                     (send
                      (if (eq (eat--t-term-mouse-encoding terminal)
                              'sgr)
                          (format "\e[<%i;%i;%iM" button x y)
                        (format "\e[M%c%c%c" (+ button 32) (+ x 32)
                                (+ y 32)))))
                 (cond
                  ;; `down-mouse-1' and friends.
                  ((memq 'down modifiers)
                   ;; For `mouse-1', `mouse-2' and `mouse-3', keep
                   ;; track the button's state, we'll need it when
                   ;; button event mouse mode is enabled.
                   (when (< (logand button 3) 3)
                     (setf (eat--t-term-mouse-pressed terminal)
                           ;; In XTerm and Kitty, mouse-1 is
                           ;; prioritized over mouse-2, and mouse-2
                           ;; over mouse-3.  However St doesn't keep
                           ;; track of multiple buttons.
                           (sort
                            (cons button (eat--t-term-mouse-pressed
                                          terminal))
                            #'<)))
                   (send
                    (if (eq (eat--t-term-mouse-encoding terminal)
                            'sgr)
                        (format "\e[<%i;%i;%iM" button x y)
                      (format "\e[M%c%c%c" (+ button 32) (+ x 32)
                              (+ y 32)))))
                  ;; `mouse-1', `mouse-2', `mouse-3', and their
                  ;; `drag'ged variants.
                  ((and (or (memq 'click modifiers)
                            (memq 'drag modifiers))
                        (<= mouse-num 3))
                   ;; For `mouse-1', `mouse-2' and `mouse-3', keep
                   ;; track the button's state, we'll need it when
                   ;; button event mouse mode is enabled.
                   (setf (eat--t-term-mouse-pressed terminal)
                         (cl-delete-if
                          (lambda (b)
                            (= (logand b 3) (logand button 3)))
                          (eat--t-term-mouse-pressed terminal)))
                   (send
                    (if (eq (eat--t-term-mouse-encoding terminal)
                            'sgr)
                        (format "\e[<%i;%i;%im" button x y)
                      (format "\e[M%c%c%c" (+ (logior button 3) 32)
                              (+ x 32) (+ y 32)))))
                  ;; Mouse wheel, `mouse-4' and friends.
                  (t
                   (send
                    (if (eq (eat--t-term-mouse-encoding terminal)
                            'sgr)
                        (format "\e[<%i;%i;%iM" button x y)
                      (format "\e[M%c%c%c" (+ button 32) (+ x 32)
                              (+ y 32))))))))))
          ;; Mouse movement tracking.
          ((and (guard (memq (eat--t-term-mouse-mode terminal)
                             '(button-event any-event)))
                (pred mouse-movement-p)
                movement)
           (let* ((pos (event-start movement))
                  (x-y (if (eval-when-compile
                             (< emacs-major-version 29))
                           (posn-col-row pos)
                         (with-suppressed-warnings
                             ((callargs posn-col-row))
                           (posn-col-row pos 'use-window))))
                  (x (1+ (car x-y)))
                  (y (1+ (cdr x-y)))
                  (button
                   (if (car (eat--t-term-mouse-pressed terminal))
                       (+ (car (eat--t-term-mouse-pressed terminal))
                          32)
                     35)))
             (when ref-pos
               (let ((ref-x-y
                      (if (eval-when-compile
                            (< emacs-major-version 29))
                          (posn-col-row ref-pos)
                        (with-suppressed-warnings
                            ((callargs posn-col-row))
                          (posn-col-row ref-pos 'use-window)))))
                 (cl-decf x (car ref-x-y))
                 (cl-decf y (cdr ref-x-y))))
             (when (and (or (eq (eat--t-term-mouse-mode terminal)
                                'any-event)
                            (/= button 35))
                        (<= 1 x (eat--t-disp-width disp))
                        (<= 1 y (eat--t-disp-height disp))
                        (or (eat--t-term-mouse-encoding terminal)
                            (and (<= x 95)
                                 (<= y 95)
                                 (<= button 95))))
               (send
                (if (eq (eat--t-term-mouse-encoding terminal)
                        'sgr)
                    (format "\e[<%i;%i;%iM" button x y)
                  (format "\e[M%c%c%c" (+ button 32) (+ x 32)
                          (+ y 32)))))))
          ;; Focus events.
          ('(eat-focus-in)
           (when (eat--t-term-focus-event-mode terminal)
             (send "\e[I")))
          ('(eat-focus-out)
           (when (eat--t-term-focus-event-mode terminal)
             (send "\e[O"))))))))

(defun eat-term-send-string (terminal string)
  "Send STRING to TERMINAL directly."
  (eat--t-ensure-live-term terminal)
  (funcall (eat--t-term-input-fn terminal) terminal string))

(defun eat-term-send-string-as-yank (terminal args)
  "Send ARGS to TERMINAL, honoring bracketed yank mode.

Each argument in ARGS can be either string or character."
  (eat--t-ensure-live-term terminal)
  (funcall (eat--t-term-input-fn terminal) terminal
           (let ((str (mapconcat (lambda (s)
                                   (if (stringp s) s (string s)))
                                 args "")))
             (if (eat--t-term-bracketed-yank terminal)
                 ;; REVIEW: What if `str' itself contains these escape
                 ;; sequences?  St doesn't care and just wraps the
                 ;; string with these magic escape sequences, while
                 ;; Kitty tries to be smart.
                 (format "\e[200~%s\e[201~" str)
               str))))

(defun eat-term-make-keymap (input-command categories exceptions)
  "Make a keymap binding INPUT-COMMAND to the events of CATEGORIES.

CATEGORIES is a list whose elements should be a one of the following
keywords:

  `:ascii'              All self-insertable characters, plus
                        `backspace', `DEL', `insert', `delete' and
                        `deletechar' keys, with all possible
                        modifiers.
  `:arrow'              Arrow keys with all possible modifiers.
  `:navigation'         Navigation keys: home, end, prior (or page up)
                        and next (or page down) with all possible
                        modifiers.
  `:function'           Function keys (f1 - f63).
  `:mouse-click'        `mouse-1', `mouse-2' and `mouse-3'.
  `:mouse-modifier'     All mouse events except mouse movement.
  `:mouse-movement'     Mouse movement.

EXCEPTIONS is a list of key sequences to not bind.  Don't use
\"M-...\" key sequences in EXCEPTIONS, use \"ESC ...\" instead."
  (let ((map (make-sparse-keymap)))
    (cl-flet ((bind (key)
                (unless (member key exceptions)
                  (define-key map key input-command))))
      (when (memq :ascii categories)
        ;; Bind ASCII and self-insertable characters except ESC.
        (bind [remap self-insert-command])
        (cl-loop
         for i from ?\C-@ to ?\C-?
         do (unless (= i meta-prefix-char)
              (bind (vector i))))
        ;; Bind `tab', `backspace', `delete', `deletechar', and all
        ;; modified variants.
        (dolist (key '( tab backtab backspace C-backspace
                        M-backspace C-M-backspace
                        insert C-insert M-insert S-insert C-M-insert
                        C-S-insert M-S-insert C-M-S-insert
                        delete C-delete M-delete S-delete C-M-delete
                        C-S-delete M-S-delete C-M-S-delete
                        deletechar C-deletechar M-deletechar
                        S-deletechar C-M-deletechar C-S-deletechar
                        M-S-deletechar C-M-S-deletechar))
          (bind (vector key)))
        ;; Bind these non-encodable keys.  They are translated.
        (dolist (key '(?\C-- ?\C-? ?\C-\s))
          (bind (vector key)))
        ;; Bind M-<ASCII> keys.
        (unless (member (vector meta-prefix-char) exceptions)
          (define-key map (vector meta-prefix-char)
                      (make-sparse-keymap))
          (cl-loop
           for i from ?\C-@ to ?\C-?
           do (unless (memq i '(?O ?\[))
                (bind (vector meta-prefix-char i))))
          (bind (vector meta-prefix-char meta-prefix-char))))
      (when (memq :arrow categories)
        (dolist (key '( up down right left
                        C-up C-down C-right C-left
                        M-up M-down M-right M-left
                        S-up S-down S-right S-left
                        C-M-up C-M-down C-M-right C-M-left
                        C-S-up C-S-down C-S-right C-S-left
                        M-S-up M-S-down M-S-right M-S-left
                        C-M-S-up C-M-S-down C-M-S-right C-M-S-left))
          (bind (vector key))))
      (when (memq :navigation categories)
        (dolist (key '( home C-home M-home S-home C-M-home C-S-home
                        M-S-home C-M-S-home
                        end C-end M-end S-end C-M-end C-S-end
                        M-S-end C-M-S-end
                        prior C-prior M-prior S-prior C-M-prior
                        C-S-prior M-S-prior C-M-S-prior
                        next C-next M-next S-next C-M-next C-S-next
                        M-S-next C-M-S-next))
          (bind (vector key))))
      (when (memq :function categories)
        (cl-loop
         for i from 1 to 63
         do (let ((key (intern (format "f%i" i))))
              (bind (vector key)))))
      (when (memq :mouse-click categories)
        (dolist (key '(mouse-1 mouse-2 mouse-3))
          (bind (vector key))))
      (when (memq :mouse-modifier categories)
        (dolist (key
                 '( down-mouse-1 drag-mouse-1 down-mouse-2
                    drag-mouse-2 down-mouse-3 drag-mouse-3
                    C-down-mouse-1 C-drag-mouse-1 C-down-mouse-2
                    C-drag-mouse-2 C-down-mouse-3 C-drag-mouse-3
                    M-down-mouse-1 M-drag-mouse-1 M-down-mouse-2
                    M-drag-mouse-2 M-down-mouse-3 M-drag-mouse-3
                    S-down-mouse-1 S-drag-mouse-1 S-down-mouse-2
                    S-drag-mouse-2 S-down-mouse-3 S-drag-mouse-3
                    C-M-down-mouse-1 C-M-drag-mouse-1
                    C-M-down-mouse-2 C-M-drag-mouse-2
                    C-M-down-mouse-3 C-M-drag-mouse-3
                    C-S-down-mouse-1 C-S-drag-mouse-1
                    C-S-down-mouse-2 C-S-drag-mouse-2
                    C-S-down-mouse-3 C-S-drag-mouse-3
                    M-S-down-mouse-1 M-S-drag-mouse-1
                    M-S-down-mouse-2 M-S-drag-mouse-2
                    M-S-down-mouse-3 M-S-drag-mouse-3
                    C-M-S-down-mouse-1 C-M-S-drag-mouse-1
                    C-M-S-down-mouse-2 C-M-S-drag-mouse-2
                    C-M-S-down-mouse-3 C-M-S-drag-mouse-3 mouse-1
                    mouse-2 mouse-3 mouse-4 mouse-5 mouse-6 mouse-7
                    mouse-8 mouse-9 mouse-10 mouse-11 C-mouse-1
                    C-mouse-2 C-mouse-3 C-mouse-4 C-mouse-5
                    C-mouse-6 C-mouse-7 C-mouse-8 C-mouse-9
                    C-mouse-10 C-mouse-11 M-mouse-1 M-mouse-2
                    M-mouse-3 M-mouse-4 M-mouse-5 M-mouse-6
                    M-mouse-7 M-mouse-8 M-mouse-9 M-mouse-10
                    M-mouse-11 S-mouse-1 S-mouse-2 S-mouse-3
                    S-mouse-4 S-mouse-5 S-mouse-6 S-mouse-7
                    S-mouse-8 S-mouse-9 S-mouse-10 S-mouse-11
                    C-M-mouse-1 C-M-mouse-2 C-M-mouse-3 C-M-mouse-4
                    C-M-mouse-5 C-M-mouse-6 C-M-mouse-7 C-M-mouse-8
                    C-M-mouse-9 C-M-mouse-10 C-M-mouse-11
                    C-S-mouse-1 C-S-mouse-2 C-S-mouse-3 C-S-mouse-4
                    C-S-mouse-5 C-S-mouse-6 C-S-mouse-7 C-S-mouse-8
                    C-S-mouse-9 C-S-mouse-10 C-S-mouse-11
                    M-S-mouse-1 M-S-mouse-2 M-S-mouse-3 M-S-mouse-4
                    M-S-mouse-5 M-S-mouse-6 M-S-mouse-7 M-S-mouse-8
                    M-S-mouse-9 M-S-mouse-10 M-S-mouse-11
                    C-M-S-mouse-1 C-M-S-mouse-2 C-M-S-mouse-3
                    C-M-S-mouse-4 C-M-S-mouse-5 C-M-S-mouse-6
                    C-M-S-mouse-7 C-M-S-mouse-8 C-M-S-mouse-9
                    C-M-S-mouse-10 C-M-S-mouse-11 wheel-up
                    wheel-down wheel-right wheel-left C-wheel-up
                    C-wheel-down C-wheel-right C-wheel-left
                    M-wheel-up M-wheel-down M-wheel-right
                    M-wheel-left S-wheel-up S-wheel-down
                    S-wheel-right S-wheel-left C-M-wheel-up
                    C-M-wheel-down C-M-wheel-right C-M-wheel-left
                    C-S-wheel-up C-S-wheel-down C-S-wheel-right
                    C-S-wheel-left M-S-wheel-up M-S-wheel-down
                    M-S-wheel-right M-S-wheel-left C-M-S-wheel-up
                    C-M-S-wheel-down C-M-S-wheel-right
                    C-M-S-wheel-left))
          (bind (vector key))))
      (when (memq :mouse-movement categories)
        (bind [mouse-movement])))
    map))

(defun eat-term-name ()
  "Return the value of `TERM' environment variable for Eat."
  (if (stringp eat-term-name)
      eat-term-name
    (funcall eat-term-name)))

(defun eat-term-get-suitable-term-name (&optional display)
  "Return the most suitable value for `TERM' for DISPLAY.

If the number of colors supported by display (as returned by
`display-color-cells') is more than 256, return \"eat-truecolor\", if
it is more than 8 but less than or equal to 256, return
\"eat-256color\", if is more than 1 but less than or equal to 8,
return \"eat-color\", otherwise return \"eat-mono\"."
  (let ((colors (display-color-cells display)))
    (cond ((> colors 256) "eat-truecolor")
          ((> colors 8) "eat-256color")
          ((> colors 1) "eat-color")
          (t "eat-mono"))))

(defun eat-term-filter-string (string)
  "Filter Eat's special text properties from STRING."
  (with-temp-buffer
    (insert string)
    ;; Join long lines.
    (goto-char (point-min))
    (while (not (eobp))
      (eat--t-join-long-line))
    ;; Remove the invisible spaces used with multi-column characters.
    (goto-char (point-min))
    (while (not (eobp))
      (let ((invisible-p (get-text-property
                          (point) 'eat--t-invisible-space))
            (next-change (or (next-single-property-change
                              (point) 'eat--t-invisible-space)
                             (point-max))))
        (when invisible-p
          (delete-region (point) next-change))
        (goto-char next-change)))
    (remove-text-properties (point-min) (point-max)
                            '( eat--t-char-width nil
                               eat--t-sixel-bitmap-size nil
                               eat--t-sixel-bitmap nil))
    (buffer-string)))


;;;; Blink mode.

(defvar eat--slow-blink-state nil
  "Current state of slowly blinking text, t means inverse video.")

(defvar eat--fast-blink-state nil
  "Current state of rapidly blinking text, t means inverse video.")

(defvar eat--slow-blink-remap nil
  "Face remapping cookie of slowly blinking face.")

(defvar eat--fast-blink-remap nil
  "Face remapping cookie of rapidly blinking face.")

(defvar eat--slow-blink-timer nil
  "Timer for blinking slowly blinking text.")

(defvar eat--fast-blink-timer nil
  "Timer for blinking rapidly blinking text.")

(declare-function face-remap-add-relative "face-remap"
                  (face &rest specs))
(declare-function face-remap-remove-relative "face-remap" (cookie))

(defun eat--flip-slow-blink-state ()
  "Flip the state of slowly blinking text."
  (face-remap-remove-relative eat--slow-blink-remap)
  (setq eat--slow-blink-remap
        (face-remap-add-relative
         'eat-slow-blink
         `(:box nil :inverse-video ,(not eat--slow-blink-state)))
        eat--slow-blink-state (not eat--slow-blink-state)))

(defun eat--flip-fast-blink-state ()
  "Flip the state of rapidly blinking text."
  (face-remap-remove-relative eat--fast-blink-remap)
  (setq eat--fast-blink-remap
        (face-remap-add-relative
         'eat-fast-blink
         `(:box nil :inverse-video ,(not eat--fast-blink-state)))
        eat--fast-blink-state (not eat--fast-blink-state)))

(defun eat--blink-stop-timers ()
  "Start blinking timers."
  (when eat--slow-blink-timer
    (cancel-timer eat--slow-blink-timer)
    (setq eat--slow-blink-timer nil))
  (when eat--fast-blink-timer
    (cancel-timer eat--fast-blink-timer)
    (setq eat--fast-blink-timer nil)))

(defun eat--blink-start-timers ()
  "Start blinking timers."
  (eat--blink-stop-timers)
  (setq eat--slow-blink-timer
        (run-with-timer t (/ (float eat-slow-blink-frequency))
                        #'eat--flip-slow-blink-state))
  (setq eat--fast-blink-timer
        (run-with-timer t (/ (float eat-fast-blink-frequency))
                        #'eat--flip-fast-blink-state)))

(define-minor-mode eat-blink-mode
  "Toggle blinking of text with blink attribute."
  :lighter " Eat-Blink"
  (let ((locals '( eat--slow-blink-state eat--fast-blink-state
                   eat--slow-blink-remap eat--fast-blink-remap
                   eat--slow-blink-timer eat--fast-blink-timer)))
    (cond
     (eat-blink-mode
      (setq eat-blink-mode nil)
      (require 'face-remap)
      (setq eat-blink-mode t)
      (mapc #'make-local-variable locals)
      (setq eat--slow-blink-state nil)
      (setq eat--fast-blink-state nil)
      (setq eat--slow-blink-remap
            (face-remap-add-relative 'eat-term-slow-blink
                                     '(:box nil)))
      (setq eat--fast-blink-remap
            (face-remap-add-relative 'eat-term-fast-blink
                                     '(:box nil)))
      (add-hook 'pre-command-hook #'eat--blink-stop-timers nil t)
      (add-hook 'post-command-hook #'eat--blink-start-timers nil t))
     (t
      (eat--blink-stop-timers)
      (face-remap-remove-relative eat--slow-blink-remap)
      (face-remap-remove-relative eat--fast-blink-remap)
      (remove-hook 'pre-command-hook #'eat--blink-stop-timers t)
      (remove-hook 'post-command-hook #'eat--blink-start-timers t)
      (mapc #'kill-local-variable locals)))))


;;;; Buffer-local Cursor Blinking.

(defvar eat--cursor-blink-type nil
  "Type of blinking cursor.")

(defvar eat--cursor-blink-state nil
  "Current state of slowly blinking text, non-nil means on.")

(defvar eat--cursor-blink-timer nil
  "Timer for blinking slowly blinking text.")

(defvar eat--cursor-blink-mode)

(defun eat--flip-cursor-blink-state ()
  "Flip the state of slowly blinking text."
  (when (and eat--cursor-blink-mode
             (display-graphic-p))
    (setq-local cursor-type (if eat--cursor-blink-state
                                (caddr eat--cursor-blink-type)
                              (car eat--cursor-blink-type)))
    (setq eat--cursor-blink-state (not eat--cursor-blink-state))
    ;; REVIEW: This is expensive, and some causes flickering.  Any
    ;; better way?
    (when-let* ((window (get-buffer-window nil 'visible)))
      (redraw-frame (window-frame window)))))

(defun eat--cursor-blink-stop-timers ()
  "Stop blinking timers."
  (unless eat--cursor-blink-state
    (eat--flip-cursor-blink-state))
  (when eat--cursor-blink-timer
    (cancel-timer eat--cursor-blink-timer)
    (setq eat--cursor-blink-timer nil)))

(defun eat--cursor-blink-start-timers ()
  "Start blinking timers."
  (eat--cursor-blink-stop-timers)
  (setq eat--cursor-blink-timer
        (run-with-timer t (/ (float (cadr eat--cursor-blink-type)))
                        #'eat--flip-cursor-blink-state)))

(define-minor-mode eat--cursor-blink-mode
  "Toggle blinking of cursor."
  :interactive nil
  (let ((locals '(eat--cursor-blink-state eat--cursor-blink-timer)))
    (cond
     (eat--cursor-blink-mode
      (mapc #'make-local-variable locals)
      (setq eat--cursor-blink-state nil)
      (setq eat--cursor-blink-timer nil)
      (add-hook 'pre-command-hook #'eat--cursor-blink-stop-timers nil
                t)
      (add-hook 'post-command-hook #'eat--cursor-blink-start-timers
                nil t)
      (add-hook 'kill-buffer-hook #'eat--cursor-blink-stop-timers nil
                t)
      (when (current-idle-time)
        (eat--cursor-blink-start-timers)))
     (t
      (eat--cursor-blink-stop-timers)
      (remove-hook 'pre-command-hook #'eat--cursor-blink-stop-timers
                   t)
      (remove-hook 'post-command-hook #'eat--cursor-blink-start-timers
                   t)
      (remove-hook 'kill-buffer-hook #'eat--cursor-blink-stop-timers
                   t)
      (mapc #'kill-local-variable locals)))))


;;;; User Interface.

(defvar eat-terminal nil
  "The terminal emulator.")

(defvar eat--synchronize-scroll-function nil
  "Function to synchronize scrolling between terminal and window.")

(defvar eat--shell-command-status 0
  "If the current shell command has finished, its exit status.")

(defvar eat--shell-prompt-begin nil
  "Beginning of last shell prompt.")

(defvar eat--shell-prompt-mark nil
  "Display property used to put a mark before the previous prompt.")

(defvar eat--shell-prompt-mark-overlays nil
  "List of overlay used to put marks before shell prompts.")

(defvar eat--inhibit-auto-line-mode nil
  "Non-nil means don't enter line mode.")

(defvar eat--auto-line-mode-prev-mode nil
  "The input mode active before line mode.")

(defvar eat--auto-line-mode-pending-toggles nil
  "Automatic line mode toggles left to do.

Don't change the toplevel value of this, let-bind instead.")

(defun eat-reset ()
  "Perform a terminal reset."
  (interactive)
  (when eat-terminal
    (let ((inhibit-read-only t))
      (eat-term-reset eat-terminal)
      (eat-term-redisplay eat-terminal))
    (run-hooks 'eat-update-hook)))

(defun eat--set-cursor (_ state)
  "Set cursor type according to STATE.

STATE can be one of the following:

  `:invisible'          Invisible cursor.
  `:block'              Block (filled box) cursor (default).
  `:blinking-block'     Blinking block cursor.
  `:bar'                Vertical bar cursor.
  `:blinking-bar'       Blinking vertical bar cursor.
  `:underline'          Horizontal bar cursor.
  `:blinking-underline' Blinking horizontal bar cursor.
  Any other value         Block cursor."
  (setq-local
   eat--cursor-blink-type
   (pcase state
     (:invisible eat-invisible-cursor-type)
     (:block eat-default-cursor-type)
     (:blinking-block eat-very-visible-cursor-type)
     (:bar eat-vertical-bar-cursor-type)
     (:blinking-bar eat-very-visible-vertical-bar-cursor-type)
     (:underline eat-horizontal-bar-cursor-type)
     (:blinking-underline eat-very-visible-horizontal-bar-cursor-type)
     (_ eat-default-cursor-type)))
  (setq-local cursor-type (car eat--cursor-blink-type))
  (when (xor (cadr eat--cursor-blink-type) eat--cursor-blink-mode)
    (eat--cursor-blink-mode
     (if (cadr eat--cursor-blink-type) +1 -1))))

(defun eat--manipulate-kill-ring (_ selection data)
  "Manipulate `kill-ring'.

SELECTION can be one of `:clipboard', `:primary', `:secondary',
`:select'.  When DATA is a string, set the selection to that string,
when DATA is nil, unset the selection, and when DATA is t, return the
selection, or nil if none."
  (let ((inhibit-eol-conversion t)
        (select-enable-clipboard (eq selection :clipboard))
        (select-enable-primary (eq selection :primary)))
    (pcase-exhaustive data
      ('t
       (when eat-enable-yank-to-terminal
         (ignore-error error
           (current-kill 0 'do-not-move))))
      ('nil
       (when eat-enable-kill-from-terminal
         (kill-new "")))
      ((and (pred stringp) str)
       (when eat-enable-kill-from-terminal
         (kill-new str))))))

(defun eat--bell (_)
  "Ring the bell."
  (ding t))

(defun eat--sixel-render-format ()
  "Return the suitable Sixel render format."
  (cl-block nil
    (dolist (fmt eat-sixel-render-formats)
      (pcase-exhaustive fmt
        ('none (cl-return 'none))
        ('background (cl-return 'background))
        ('half-block (when (char-displayable-p ?▄)
                       (cl-return 'half-block)))
        ('svg (when (and (display-graphic-p)
                         (image-type-available-p 'svg))
                (cl-return 'svg)))
        ('xpm (when (and (display-graphic-p)
                         (image-type-available-p 'xpm))
                (cl-return 'xpm)))))
    'none))

(defun eat--set-term-sixel-params ()
  "Set Sixel related parameters of the terminal."
  (let* ((render-fmt (eat--sixel-render-format))
         (dimensions
          (pcase render-fmt
            ((or 'background 'none) '(1 . 1))
            ('half-block '(1 . 2))
            (_ (cons (default-font-width) (default-font-height)))))
         (scale-x (* eat-sixel-aspect-ratio eat-sixel-scale))
         (scale-y eat-sixel-scale))
    (setq dimensions
          (cons (max 1 (round (/ (car dimensions) (float scale-x))))
                (max 1 (round (/ (cdr dimensions) (float scale-y))))))
    (setf (eat-term-parameter eat-terminal 'sixel-render-format)
          render-fmt)
    (setf (eat-term-parameter eat-terminal 'char-dimensions)
          dimensions)
    (unless (memq render-fmt '(none background half-block))
      (let ((font-size
             (font-get (font-spec :name (face-font 'default))
                       :size)))
        (setf
         (eat-term-parameter eat-terminal
                             'sixel-image-extra-properties)
         `( :ascent center
            :height ,(cons (/ (float (default-font-height)) font-size)
                           'em)
            :width ,(cons (/ (float (default-font-width)) font-size)
                          'em)))))))

(defun eat--set-cwd (_ host cwd)
  "Set CWD as the current working directory (`default-directory').

If HOST isn't the host Emacs is running on, don't do anything."
  (when (and eat-enable-directory-tracking
             (string= host (system-name)))
    (ignore-errors
      (cd-absolute cwd))))

(defun eat--set-cwd-uic (host path)
  "Set PATH to the CWD, if HOST is same as the host name."
  (let ((dir (ignore-errors (expand-file-name
                             (file-name-as-directory
                              (decode-coding-string
                               (base64-decode-string path)
                               locale-coding-system)))))
        (hostname (ignore-errors (decode-coding-string
                                  (base64-decode-string host)
                                  locale-coding-system))))
    (when (and dir hostname)
      (eat--set-cwd nil hostname dir))))

(defun eat--pre-prompt ()
  "Save the beginning position of shell prompt."
  (setq eat--shell-prompt-begin (point-marker))
  ;; FIXME: It's a crime to touch processes in this section.
  (when (eq eat-query-before-killing-running-terminal 'auto)
    (set-process-query-on-exit-flag
     (eat-term-parameter eat-terminal 'eat--process) nil)))

(defvar eat--line-mode)
(defvar eat--semi-char-mode)
(defvar eat--char-mode)

(defun eat--line-mode-enter-auto-1 ()
  "Enter line mode."
  (unless (or eat--inhibit-auto-line-mode eat--line-mode)
    (unless eat--line-mode
      (setq eat--auto-line-mode-prev-mode
            (cond (eat--semi-char-mode 'semi-char)
                  (eat--char-mode 'char)
                  (t 'emacs)))
      (eat-line-mode)
      ;; We're entering automatically, so we should be able to exit it
      ;; automatically.
      (setq eat--inhibit-auto-line-mode nil))))

(defun eat--line-mode-enter-auto ()
  "Arrange that line mode will be enabled eventually."
  (push 'enter eat--auto-line-mode-pending-toggles))

(defun eat--line-mode-exit-auto-1 ()
  "Exit line mode."
  (when (and (not eat--inhibit-auto-line-mode)
             eat--auto-line-mode-prev-mode)
    (pcase eat--auto-line-mode-prev-mode
      ('emacs (eat-emacs-mode))
      ('semi-char (eat-semi-char-mode))
      ('char (eat-char-mode)))
    (setq eat--auto-line-mode-prev-mode nil)
    (when (/= (eat-term-end eat-terminal) (point-max))
      (eat-line-send))
    ;; Toggle line mode _after_ we exit from
    ;; `eat-term-process-output'.
    (eat--line-mode -1)
    (setq buffer-undo-list nil)))

(defun eat--line-mode-exit-auto ()
  "Arrange that line mode will be disabled eventually."
  (push 'exit eat--auto-line-mode-pending-toggles))

(defun eat--line-mode-do-toggles ()
  "Do the pending line mode toggle."
  (let* ((inhibit-quit t)
         (actions (nreverse eat--auto-line-mode-pending-toggles))
         (toggle nil))
    (while (setq toggle (pop actions))
      (pcase-exhaustive toggle
        ('enter (eat--line-mode-enter-auto-1))
        ('exit (eat--line-mode-exit-auto-1)))
      ;; Don't do extra unnecessary toggles.
      (let ((loop t))
        (while loop
          (setq loop nil)
          (while (eq toggle (car actions))
            (pop actions))
          (while (and (car actions) (cadr actions)
                      (not (eq (car actions) (cadr actions))))
            (pop actions)
            (pop actions)
            (setq loop t)))))
    (setq eat--auto-line-mode-pending-toggles nil)))

(defun eat--post-prompt ()
  "Put a mark in the marginal area and enter line mode."
  (when eat-enable-shell-prompt-annotation
    (let ((indicator
           (if (zerop eat--shell-command-status)
               (propertize
                eat-shell-prompt-annotation-success-margin-indicator
                'face '(eat-shell-prompt-annotation-success default))
             (propertize
              eat-shell-prompt-annotation-failure-margin-indicator
              'face '(eat-shell-prompt-annotation-failure default)))))
      ;; Update previous prompt's indicator using side-effect.
      (when eat--shell-prompt-mark
        (setf (cadr eat--shell-prompt-mark) indicator)
        (setq eat--shell-prompt-mark nil))
      ;; Show this prompt's indicator.
      (when eat--shell-prompt-begin
        (when (< eat--shell-prompt-begin (point))
          ;; Save it, we'll use side-effect.
          (setq eat--shell-prompt-mark
                `((margin ,eat-shell-prompt-annotation-position)
                  ,indicator))
          ;; Make overlay and put bookkeeping properties.
          (let ((identifier (gensym "eat--prompt-mark-identifier-"))
                (before-str
                 (propertize " " 'display eat--shell-prompt-mark))
                (ov (make-overlay eat--shell-prompt-begin
                                  (1+ eat--shell-prompt-begin))))
            (overlay-put ov 'before-string before-str)
            (overlay-put ov 'eat--shell-prompt-mark-id identifier)
            (add-text-properties
             eat--shell-prompt-begin (1+ eat--shell-prompt-begin)
             (list 'eat--before-string before-str
                   'eat--shell-prompt-mark-id identifier
                   'eat--shell-prompt-mark-overlay ov))
            (push ov eat--shell-prompt-mark-overlays))))))
  (when eat--shell-prompt-begin
    (when (< eat--shell-prompt-begin (point))
      ;; Put a text property for `eat-narrow-to-shell-prompt'.
      (put-text-property eat--shell-prompt-begin
                         (1+ eat--shell-prompt-begin)
                         'eat--shell-prompt-begin t)
      ;; Put a text property to allow shell prompt navigation.
      (put-text-property (1- (point)) (point)
                         'eat--shell-prompt-end t)))
  (setq eat--shell-prompt-begin nil)
  (when eat-enable-auto-line-mode
    (eat--line-mode-enter-auto)))

(defun eat--post-cont-prompt ()
  "Enter line mode."
  (when eat-enable-auto-line-mode
    (eat--line-mode-enter-auto)))

(defun eat--correct-shell-prompt-mark-overlays (buffer)
  "Correct all overlays used to add mark before shell prompt.

BUFFER is the terminal buffer."
  (when (and (buffer-live-p buffer)
             (buffer-local-value 'eat-terminal buffer)
             eat-enable-shell-prompt-annotation)
    (with-current-buffer buffer
      (while-no-input
        ;; Delete all outdated overlays.
        (dolist (ov eat--shell-prompt-mark-overlays)
          (unless (and (<= (point-min) (overlay-start ov)
                           (1- (point-max)))
                       (eq (overlay-get ov 'eat--shell-prompt-mark-id)
                           (get-text-property
                            (overlay-start ov)
                            'eat--shell-prompt-mark-id)))
            (delete-overlay ov)
            (setq eat--shell-prompt-mark-overlays
                  (delq ov eat--shell-prompt-mark-overlays))))
        (save-excursion
          ;; Recreate overlays if needed.
          (goto-char (max (eat-term-beginning eat-terminal)
                          (point-min)))
          (while (< (point) (min (eat-term-end eat-terminal)
                                 (point-max)))
            (when (get-text-property
                   (point) 'eat--shell-prompt-mark-id)
              (let ((ov (get-text-property
                         (point) 'eat--shell-prompt-mark-overlay)))
                (unless (and
                         ov (overlay-buffer ov)
                         (eq (overlay-get
                              ov 'eat--shell-prompt-mark-id)
                             (get-text-property
                              (point) 'eat--shell-prompt-mark-id)))
                  ;; Recreate.
                  (when ov
                    (delete-overlay ov)
                    (setq eat--shell-prompt-mark-overlays
                          (delq ov eat--shell-prompt-mark-overlays)))
                  (setq ov (make-overlay (point) (1+ (point))))
                  (overlay-put ov 'before-string
                               (get-text-property
                                (point) 'eat--before-string))
                  (overlay-put ov 'eat--shell-prompt-mark-id
                               (get-text-property
                                (point) 'eat--shell-prompt-mark-id))
                  (push ov eat--shell-prompt-mark-overlays))))
            (goto-char (or (next-single-property-change
                            (point) 'eat--shell-prompt-mark-id nil
                            (min (eat-term-end eat-terminal)
                                 (point-max)))
                           (min (eat-term-end eat-terminal)
                                (point-max))))))))))

(defun eat--set-cmd (cmd)
  "Add CMD to `shell-command-history'."
  (when-let* ((eat-enable-shell-command-history)
              (command (ignore-errors (decode-coding-string
                                       (base64-decode-string cmd)
                                       locale-coding-system))))
    (add-to-history 'shell-command-history command)))

(defun eat--pre-cmd ()
  "Update shell prompt mark to indicate command is running."
  ;; FIXME: It's a crime to touch processes in this section.
  (when (eq eat-query-before-killing-running-terminal 'auto)
    (set-process-query-on-exit-flag
     (eat-term-parameter eat-terminal 'eat--process) t))
  (when (and eat-enable-shell-prompt-annotation
             eat--shell-prompt-mark)
    (setf (cadr eat--shell-prompt-mark)
          (propertize
           eat-shell-prompt-annotation-running-margin-indicator
           'face '(eat-shell-prompt-annotation-running default))))
  (when eat-enable-auto-line-mode
    (eat--line-mode-exit-auto)))

(defun eat--set-cmd-status (code)
  "Set CODE as the current shell command's exit status."
  (when eat-enable-shell-prompt-annotation
    ;; We'll update the mark later when the prompt appears.
    (setq eat--shell-command-status code)))

(defun eat--before-new-prompt ()
  "Allow entering line mode."
  (setq eat--inhibit-auto-line-mode nil))

(defun eat--get-shell-history (hist format)
  "Get shell history from HIST in format FORMAT."
  (pcase hist
    (`(,host . ,file)
     (setq host (ignore-errors
                  (decode-coding-string (base64-decode-string host)
                                        locale-coding-system)))
     (setq file (ignore-errors
                  (decode-coding-string (base64-decode-string file)
                                        locale-coding-system)))
     (if (and host file
              (string= host (system-name))
              (file-readable-p file))
         (let ((str nil))
           (eat-term-send-string eat-terminal "\e]51;e;I;0\e\\")
           (with-temp-buffer
             (insert-file-contents file)
             (setq str (buffer-string)))
           (eat--line-populate-input-ring str format))
       (eat-term-send-string
        eat-terminal
        (format "\e]51;e;I;%s\e\\" eat-line-input-ring-size))))
    ((pred stringp)
     (eat--line-populate-input-ring
      (ignore-errors
        (decode-coding-string (base64-decode-string hist)
                              locale-coding-system))
      format))))

(defun eat--handle-message (name &rest args)
  "Handle message with handler name NAME and ARGS."
  (when-let* ((name (ignore-errors (decode-coding-string
                                    (base64-decode-string name)
                                    locale-coding-system)))
              (handler (assoc name eat-message-handler-alist)))
    (save-restriction
      (widen)
      (save-excursion
        (apply (cdr handler)
               (mapcar (lambda (arg)
                         (ignore-errors (decode-coding-string
                                         (base64-decode-string arg)
                                         locale-coding-system)))
                       args))))))

(defun eat--handle-uic (_ cmd)
  "Handle UI Command sequence CMD."
  (pcase cmd
    ;; In XTerm, OSC 51 is reserved for Emacs shell.  I have no idea
    ;; why, but Vterm uses this OSC to set the current directory and
    ;; remotely execute Emacs Lisp code.  Vterm uses the characters
    ;; 'A' and 'E' as the first character of second parameter of this
    ;; OSC.  We use 'e' as the second parameter, followed by one or
    ;; more parameters.
    ;; UIC e ; A ; <t> ; <s> ST.
    ((rx string-start "e;A;"
         (let host (zero-or-more (not (any ?\;))))
         ?\; (let path (zero-or-more anything))
         string-end)
     (eat--set-cwd-uic host path))
    ;; UIC e ; B ST.
    ("e;B"
     (eat--pre-prompt))
    ;; UIC e ; C ST.
    ("e;C"
     (eat--post-prompt))
    ;; UIC e ; D ST.
    ("e;D"
     ;; Start of continuation prompt.
     ;; Defined but unused.
     )
    ;; UIC e ; E ST.
    ("e;E"
     (eat--post-cont-prompt))
    ;; UIC e ; F ; <t> ST.
    ((rx string-start "e;F;"
         (let cmd (zero-or-more anything))
         string-end)
     (eat--set-cmd cmd))
    ;; UIC e ; G ST
    ("e;G"
     (eat--pre-cmd))
    ;; UIC e ; H ; <n> ST.
    ((rx string-start "e;H;"
         (let status (one-or-more digit))
         string-end)
     (eat--set-cmd-status (string-to-number status)))
    ;; UIC e ; I ; 0 ; <t> ; <t> ; <t> ST.
    ((rx string-start "e;I;0;"
         (let format (zero-or-more (not (any ?\;))))
         ?\; (let host (zero-or-more (not (any ?\;))))
         ?\; (let path (zero-or-more anything))
         string-end)
     (eat--get-shell-history (cons host path) format))
    ;; UIC e ; I ; 1 ; <t> ; <t> ST.
    ((rx string-start "e;I;1;"
         (let format (zero-or-more (not (any ?\;))))
         ?\; (let hist (zero-or-more anything))
         string-end)
     (eat--get-shell-history hist format))
    ;; UIC e ; J ST.
    ("e;J"
     (eat--before-new-prompt))
    ;; UIC e ; M ; ... ST.
    ((rx string-start "e;M;"
         (let msg (zero-or-more anything))
         string-end)
     (apply #'eat--handle-message (string-split msg ";")))))

(defun eat-previous-shell-prompt (&optional arg)
  "Go to the previous shell prompt.

When numeric prefix argument, ARG, is given, go to ARGth previous
shell prompt."
  (interactive "p")
  (dotimes (_ (or arg 1))
    (let ((previous (previous-single-property-change
                     (point) 'eat--shell-prompt-end)))
      (goto-char (or previous (point-min)))
      (when (get-text-property (point) 'eat--shell-prompt-end)
        (setq previous (previous-single-property-change
                        (point) 'eat--shell-prompt-end))
        (goto-char (or previous (point-min))))
      (unless previous
        (user-error "No previous prompt")))))

(defun eat-next-shell-prompt (&optional arg)
  "Go to the next shell prompt.

When numeric prefix argument, ARG, is given, go to ARGth next shell
prompt."
  (interactive "p")
  (dotimes (_ (or arg 1))
    (let ((next (next-single-property-change
                 (point) 'eat--shell-prompt-end)))
      (goto-char (or next (point-max)))
      (when (get-text-property (point) 'eat--shell-prompt-end)
        (goto-char (or (next-single-property-change
                        (point) 'eat--shell-prompt-end)
                       (point-max))))
      (unless next
        (user-error "No next prompt")))))

(defun eat-narrow-to-shell-prompt ()
  "Narrow buffer to the shell prompt and following output at point."
  (interactive)
  (widen)
  (narrow-to-region
   (save-excursion
     (while (not (or (bobp) (get-text-property
                             (point) 'eat--shell-prompt-begin)))
       (goto-char (or (previous-single-property-change
                       (point) 'eat--shell-prompt-begin)
                      (point-min))))
     (point))
   (save-excursion
     (when (and (not (eobp))
                (get-text-property (point) 'eat--shell-prompt-begin))
       (goto-char (or (next-single-property-change
                       (point) 'eat--shell-prompt-begin)
                      (point-max))))
     (while (not (or (eobp) (get-text-property
                             (point) 'eat--shell-prompt-begin)))
       (goto-char (or (next-single-property-change
                       (point) 'eat--shell-prompt-begin)
                      (point-max))))
     (point))))


;;;;; Input.

(defvar eat--mouse-grabbing-type nil
  "Current mouse grabbing type/mode.")

(defvar eat--mouse-pressed-buttons nil
  "Mouse buttons currently pressed.")

(defvar eat--mouse-last-position nil
  "Last position of mouse, nil when not dragging.")

(defvar eat--mouse-drag-transient-map-exit nil
  "Function to exit mouse dragging transient map.")

(defun eat-self-input (n &optional e)
  "Send E as input N times.

N defaults to 1 and E defaults to `last-command-event' and should be a
event."
  (interactive
   (list (prefix-numeric-value current-prefix-arg)
         (if (and (> (length (this-command-keys)) 1)
                  (eq (aref (this-command-keys)
                            (- (length (this-command-keys)) 2))
                      meta-prefix-char))
             ;; HACK: Capture meta modifier (ESC prefix) in terminal.
             (cond
              ((eq last-command-event meta-prefix-char)
               last-command-event)
              ((characterp last-command-event)
               (aref
                (kbd (format "M-%c" last-command-event))
                0))
              ((symbolp last-command-event)
               (aref
                (kbd (format "M-<%S>" last-command-event))
                0))
              (t
               last-command-event))
           last-command-event)))
  (when (memq (event-basic-type e)
              '( mouse-1 mouse-2 mouse-3 mouse-4 mouse-5 mouse-6
                 mouse-7 mouse-8 mouse-9 mouse-10 mouse-11))
    (select-window (posn-window (event-start e))))
  (when eat-terminal
    (unless (mouse-movement-p e)
      (funcall eat--synchronize-scroll-function
               (eat--synchronize-scroll-windows 'force-selected)))
    (if (memq (event-basic-type e)
              '( mouse-1 mouse-2 mouse-3 mouse-4 mouse-5 mouse-6
                 mouse-7 mouse-8 mouse-9 mouse-10 mouse-11
                 mouse-movement))
        (let ((disp-begin-posn
               (posn-at-point
                (eat-term-display-beginning eat-terminal)))
              (e (if (or (not eat--mouse-last-position)
                         (eq (posn-window
                              (if (memq 'drag (event-modifiers e))
                                  (event-end e)
                                (event-start e)))
                             (posn-window eat--mouse-last-position)))
                     e
                   (pcase e
                     (`(,type ,_)
                      `(,type ,eat--mouse-last-position))
                     (`(,type ,start ,_)
                      `(,type ,start ,eat--mouse-last-position))
                     (ev ev)))))
          (if (not (mouse-movement-p e))
              (eat-term-input-event eat-terminal n e disp-begin-posn)
            (if (not eat--mouse-pressed-buttons)
                (when (eq eat--mouse-grabbing-type :all)
                  (eat-term-input-event eat-terminal n e
                                        disp-begin-posn))
              (when (memq eat--mouse-grabbing-type '(:all :drag))
                (eat-term-input-event eat-terminal n e
                                      disp-begin-posn))
              (setq eat--mouse-last-position (event-start e))))
          (when (memq (event-basic-type e) '(mouse-1 mouse-2 mouse-3))
            (when (or (memq 'click (event-modifiers e))
                      (memq 'drag (event-modifiers e)))
              (setq eat--mouse-pressed-buttons
                    (delq (event-basic-type e)
                          eat--mouse-pressed-buttons))
              (unless eat--mouse-pressed-buttons
                (setq eat--mouse-last-position nil)
                (when eat--mouse-drag-transient-map-exit
                  (funcall eat--mouse-drag-transient-map-exit)
                  (setq eat--mouse-drag-transient-map-exit nil))))
            (when (memq 'down (event-modifiers e))
              (push (event-basic-type e) eat--mouse-pressed-buttons)
              (setq eat--mouse-last-position (event-start e))
              (unless eat--mouse-drag-transient-map-exit
                (let ((old-track-mouse track-mouse)
                      (buffer (current-buffer)))
                  (setq track-mouse 'dragging)
                  (setq eat--mouse-drag-transient-map-exit
                        (set-transient-map
                         (let ((map (eat-term-make-keymap
                                     #'eat-self-input
                                     '(:mouse-modifier
                                       :mouse-movement)
                                     nil)))
                           ;; Some of the events will of course end up
                           ;; looked up with a mode-line, header-line
                           ;; or vertical-line prefix ...
                           (define-key map [mode-line] map)
                           (define-key map [header-line] map)
                           (define-key map [tab-line] map)
                           (define-key map [vertical-line] map)
                           ;; ... and some maybe even with a right- or
                           ;; bottom-divider prefix.
                           (define-key map [right-divider] map)
                           (define-key map [bottom-divider] map))
                         #'always
                         (lambda ()
                           (with-current-buffer buffer
                             (setq track-mouse
                                   old-track-mouse))))))))))
      (eat-term-input-event eat-terminal n e))))

(defun eat-quoted-input ()
  "Read a character and send it as INPUT."
  (declare (interactive-only "Use `eat-self-input' instead."))
  (interactive)
  ;; HACK: Quick hack to allow inputting `C-g'.  Any better way to do
  ;; this?
  (eat-self-input
   1 (let ((inhibit-quit t)
           ;; Don't trigger `quit' exiting this `let'.
           (quit-flag nil))
       (read-event))))

(defun eat-input-char (character count)
  "Input CHARACTER, COUNT times.

Interactively, ask for the character CHARACTER to input.  The numeric prefix
argument COUNT specifies how many times to insert CHARACTER."
  (declare (interactive-only "Use `eat-self-input' instead."))
  (interactive (list (read-char-by-name
                      "Insert character (Unicode name or hex): ")
                     (prefix-numeric-value current-prefix-arg)))
  (eat-self-input count character))

(defvar yank-transform-functions) ; In `simple'.

(defun eat-yank (&optional arg)
  "Same as `yank', but for Eat.

ARG is passed to `yank', which see."
  (interactive "*P")
  (when eat-terminal
    (funcall eat--synchronize-scroll-function
             (eat--synchronize-scroll-windows 'force-selected))
    (eat-term-send-string-as-yank
     eat-terminal
     (let ((yank-hook (bound-and-true-p yank-transform-functions)))
       (with-temp-buffer
         (setq-local yank-transform-functions yank-hook)
         (yank arg)
         (buffer-string))))))

(defun eat-yank-from-kill-ring (string &optional arg)
  "Same as `yank-from-kill-ring', but for Eat.

STRING and ARG are passed to `yank-pop', which see."
  (interactive
   (progn
     (unless (eval-when-compile (>= emacs-major-version 28))
       (error "`eat-yank-from-kill-ring' requires at least Emacs 28"))
     (list (read-from-kill-ring "Yank from kill-ring: ")
           current-prefix-arg)))
  (unless (eval-when-compile (>= emacs-major-version 28))
    (error "`eat-yank-from-kill-ring' requires at least Emacs 28"))
  (when eat-terminal
    (funcall eat--synchronize-scroll-function
             (eat--synchronize-scroll-windows 'force-selected))
    (eat-term-send-string-as-yank
     eat-terminal
     (let ((yank-hook (bound-and-true-p yank-transform-functions)))
       (with-temp-buffer
         (setq-local yank-transform-functions yank-hook)
         (yank-from-kill-ring string arg)
         (buffer-string))))))

(defun eat-mouse-yank-primary (&optional event)
  "Send the primary selection to the terminal.

EVENT is the mouse event."
  (interactive "e")
  (when select-active-regions
    (let (select-active-regions)
      (deactivate-mark)))
  (unless (windowp (posn-window (event-start event)))
    (error "Position not in text area of window"))
  (select-window (posn-window (event-start event)))
  (eat-term-send-string-as-yank
   eat-terminal (gui-get-primary-selection)))

(defun eat-mouse-yank-secondary (&optional event)
  "Send the secondary selection to the terminal.

EVENT is the mouse event."
  (interactive "e")
  (unless (windowp (posn-window (event-start event)))
    (error "Position not in text area of window"))
  (select-window (posn-window (event-start event)))
  (let ((secondary (gui-get-selection 'SECONDARY)))
    (if secondary
        (eat-term-send-string-as-yank eat-terminal secondary)
      (error "No secondary selection"))))

(defun eat-xterm-paste (event)
  "Handle paste operation EVENT from XTerm."
  (interactive "e")
  (unless (eq (car-safe event) 'xterm-paste)
    (error "`eat-xterm-paste' must be bind to `xterm-paste' event"))
  (let ((pasted-text (nth 1 event)))
    (if (bound-and-true-p xterm-store-paste-on-kill-ring)
        ;; Put the text onto the kill ring and then insert it into the
        ;; buffer.
        (let ((interprogram-paste-function (lambda () pasted-text)))
          (eat-yank))
      ;; Insert the text without putting it onto the kill ring.
      (eat-term-send-string-as-yank eat-terminal pasted-text))))

(defun eat-send-password ()
  "Read password from minibuffer and send it to the terminal."
  (declare (interactive-only t))
  (interactive)
  (unless eat-terminal
    (user-error "Process not running"))
  (eat-term-send-string eat-terminal (read-passwd "Password: "))
  (eat-self-input 1 'return))

;; When changing these keymaps, be sure to update the manual, README
;; and commentary.
(defvar eat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C-c ?\M-d] #'eat-char-mode)
    (define-key map [?\C-c ?\C-j] #'eat-semi-char-mode)
    (define-key map [?\C-c ?\C-l] #'eat-line-mode)
    (define-key map [?\C-c ?\C-k] #'eat-kill-process)
    (define-key map [?\C-c ?\C-p] #'eat-previous-shell-prompt)
    (define-key map [?\C-c ?\C-n] #'eat-next-shell-prompt)
    (define-key map [?\C-x ?n ?d] #'eat-narrow-to-shell-prompt)
    (define-key map [xterm-paste] #'ignore)
    map)
  "Keymap for Eat mode.")

(defun eat--prepare-semi-char-mode-map ()
  "Prepare `eat-semi-char-mode-map'."
  (let ((map (eat-term-make-keymap
              #'eat-self-input '(:ascii :arrow :navigation)
              `([?\C-c] [?\C-q] [?\C-y] [?\e ?y]
                ,@eat-semi-char-non-bound-keys))))
    (define-key map [?\C-q] #'eat-quoted-input)
    (define-key map [?\C-y] #'eat-yank)
    (define-key map [?\M-y] #'eat-yank-from-kill-ring)
    (define-key map [?\C-c ?\C-c] #'eat-self-input)
    (define-key map [?\C-c ?\C-e] #'eat-emacs-mode)
    (define-key map [S-insert] #'eat-yank)
    (define-key map [remap insert-char] #'eat-input-char)
    (define-key map [remap mouse-yank-primary]
                #'eat-mouse-yank-primary)
    (define-key map [remap mouse-yank-secondary]
                #'eat-mouse-yank-secondary)
    (define-key map [xterm-paste] #'eat-xterm-paste)
    map))

(defvar eat-semi-char-mode-map (ignore-errors
                                 (eat--prepare-semi-char-mode-map))
  "Keymap for Eat semi-char mode.")

(defun eat-update-semi-char-mode-map ()
  "Update \"semi-char\" keybinding mode's keymap."
  (setq eat-semi-char-mode-map (eat--prepare-semi-char-mode-map)))

(defvar eat-char-mode-map
  (let ((map (eat-term-make-keymap
              #'eat-self-input '(:ascii :arrow :navigation :function)
              '([?\e ?\C-m]))))
    (define-key map [?\C-\M-m] #'eat-semi-char-mode)
    (define-key map [xterm-paste] #'eat-xterm-paste)
    map)
  "Keymap for Eat char mode.")

(defvar eat-line-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C-c ?\C-e] #'eat-emacs-mode)
    (define-key map [?\t] #'completion-at-point)
    (define-key map [?\C-m] #'eat-line-send-input)
    (define-key map [?\C-d] #'eat-line-delchar-or-eof)
    (define-key map [?\C-c ?\C-c] #'eat-line-send-interrupt)
    (define-key map [?\C-c ?\s] #'newline)
    (define-key map [?\M-p] #'eat-line-previous-input)
    (define-key map [?\M-n] #'eat-line-next-input)
    (define-key map [C-up] #'eat-line-previous-input)
    (define-key map [C-down] #'eat-line-next-input)
    (define-key map [?\M-r]
                #'eat-line-history-isearch-backward-regexp)
    (define-key map [?\C-c ?\C-r] #'eat-line-find-input)
    (define-key map [?\C-c ?\M-r]
                #'eat-line-previous-matching-input-from-input)
    (define-key map [?\C-c ?\M-s]
                #'eat-line-next-matching-input-from-input)
    map)
  "Keymap for Eat line mode.")

(defvar eat--mouse-click-mode-map
  (eat-term-make-keymap #'eat-self-input '(:mouse-click) nil)
  "Keymap for `eat--mouse-click-mode'.")

(defvar eat--mouse-modifier-click-mode-map
  (eat-term-make-keymap #'eat-self-input '(:mouse-modifier) nil)
  "Keymap for `eat--mouse-modifier-click-mode'.")

(defvar eat--mouse-movement-mode-map
  (eat-term-make-keymap #'eat-self-input '(:mouse-movement) nil)
  "Keymap for `eat--mouse-movement-mode'.")

(define-minor-mode eat--semi-char-mode
  "Minor mode for semi-char mode keymap."
  :interactive nil
  :keymap eat-semi-char-mode-map)

(define-minor-mode eat--char-mode
  "Minor mode for char mode keymap."
  :interactive nil
  :keymap eat-char-mode-map)

(define-minor-mode eat--mouse-click-mode
  "Minor mode for mouse click keymap."
  :interactive nil)

(define-minor-mode eat--mouse-modifier-click-mode
  "Minor mode for mouse click with modifiers keymap."
  :interactive nil)

(define-minor-mode eat--mouse-movement-mode
  "Minor mode for mouse movement keymap."
  :interactive nil)

(defun eat-emacs-mode ()
  "Switch to Emacs keybindings mode."
  (interactive)
  (eat--line-mode-exit)
  (eat--semi-char-mode -1)
  (eat--char-mode -1)
  (setq buffer-read-only t)
  (eat--grab-mouse nil eat--mouse-grabbing-type)
  (force-mode-line-update))

(defun eat-semi-char-mode ()
  "Switch to semi-char mode."
  (interactive)
  (unless eat-terminal
    (error "Process not running"))
  (setq buffer-read-only nil)
  (eat--line-mode-exit)
  (eat--char-mode -1)
  (eat--semi-char-mode +1)
  (eat--grab-mouse nil eat--mouse-grabbing-type)
  (force-mode-line-update))

(defun eat-char-mode ()
  "Switch to char mode."
  (interactive)
  (unless eat-terminal
    (error "Process not running"))
  (setq buffer-read-only nil)
  (eat--line-mode-exit)
  (eat--semi-char-mode -1)
  (eat--char-mode +1)
  (eat--grab-mouse nil eat--mouse-grabbing-type)
  (force-mode-line-update))

(defvar eat--eshell-semi-char-mode)
(defvar eat--eshell-char-mode)

(defun eat--grab-mouse (_ mode)
  "Grab mouse.

MODE should one of:

  nil                 Disable mouse.
  `:click'              Pass `mouse-1', `mouse-2', and `mouse-3'
                        clicks.
  `:modifier-click'     Pass all mouse clicks, including control,
                        meta and shift modifiers.
  `:drag'               All of :modifier-click, plus dragging
                        (moving mouse while pressed) information.
  `:all'                Pass all mouse events, including movement.
  Any other value     Disable mouse."
  (setq eat--mouse-grabbing-type mode)
  (pcase (and eat-enable-mouse
              (or eat--semi-char-mode
                  eat--char-mode
                  eat--eshell-semi-char-mode
                  eat--eshell-char-mode)
              mode)
    (:all
     (setq track-mouse t)
     (eat--mouse-click-mode -1)
     (eat--mouse-modifier-click-mode +1)
     (eat--mouse-movement-mode +1))
    ((or :modifier-click :drag)
     (setq track-mouse nil)
     (eat--mouse-click-mode -1)
     (eat--mouse-movement-mode -1)
     (eat--mouse-modifier-click-mode +1))
    (:click
     (setq track-mouse nil)
     (eat--mouse-modifier-click-mode -1)
     (eat--mouse-movement-mode -1)
     (eat--mouse-click-mode +1))
    (_
     (setq track-mouse nil)
     (eat--mouse-click-mode -1)
     (eat--mouse-modifier-click-mode -1)
     (eat--mouse-movement-mode -1))))


;;;;; Line Mode.

(define-minor-mode eat--line-mode
  "Minor mode for line mode."
  :interactive nil
  :keymap eat-line-mode-map
  (if eat--line-mode
      (let ((inhibit-read-only t))
        (add-hook 'pre-command-hook #'eat--line-move-to-input nil t)
        (add-text-properties (eat-term-beginning eat-terminal)
                             (eat-term-end eat-terminal)
                             '(front-sticky t rear-nonsticky t)))
    (remove-hook 'pre-command-hook #'eat--line-move-to-input t)
    (let ((inhibit-read-only t))
      (when (/= (eat-term-beginning eat-terminal)
                (eat-term-end eat-terminal))
        (remove-text-properties
         (eat-term-beginning eat-terminal)
         (eat-term-end eat-terminal)
         '(front-sticky nil rear-nonsticky nil))))))

(defun eat-line-mode ()
  "Switch to line mode."
  (interactive)
  (unless eat-terminal
    (error "Process not running"))
  (eat--line-mode +1)
  (eat--semi-char-mode -1)
  (eat--char-mode -1)
  (eat--grab-mouse nil eat--mouse-grabbing-type)
  (setq buffer-read-only nil)
  ;; Delete the undo list so that `undo' doesn't mess up with the
  ;; terminal.
  (setq buffer-undo-list nil)
  ;; Don't let auto line mode exit line mode.
  (setq eat--inhibit-auto-line-mode t))

(defun eat--line-mode-exit ()
  "Exit line mode, called only by interactive commands."
  (when eat--line-mode
    (when (/= (eat-term-end eat-terminal) (point-max))
      (eat-line-send))
    (eat--line-mode -1)
    (setq buffer-undo-list nil)
    (setq eat--inhibit-auto-line-mode t)
    (setq eat--auto-line-mode-prev-mode nil)))

(defun eat--line-move-to-input ()
  "Move point to the input line."
  (when (and eat-line-auto-move-to-input
             (< (point) (eat-term-end eat-terminal))
             (eq #'self-insert-command this-command))
    (deactivate-mark)
    (push-mark)
    (goto-char (point-max))))

(defun eat-line-send-default ()
  "Send shell prompt input directly to the terminal."
  (eat-term-send-string eat-terminal (buffer-string))
  ;; If output arrives after sending the string, new output may get
  ;; included in the narrowed region.  So we narrow it again so that
  ;; we don't get a `text-read-only' for trying to delete text in the
  ;; terminal.
  (narrow-to-region (eat-term-end eat-terminal) (point-max)))

(defun eat-line-send ()
  "Send shell prompt input to the terminal."
  (save-excursion
    (save-restriction
      (narrow-to-region (eat-term-end eat-terminal) (point-max))
      (funcall eat-line-input-send-function)
      (delete-region (point-min) (point-max))
      (eat--line-reset-input-ring-vars)
      (setq buffer-undo-list nil)))
  (goto-char (eat-term-display-cursor eat-terminal)))

(defvar eat--line-input-ring)

(defun eat-line-send-input (&optional no-newline)
  "Send shell prompt input to the terminal.

If called without any prefix argument, or if NO-NEWLINE is nil, append
a newline to the input before sending it."
  (interactive "P")
  (if (not (<= (eat-term-end eat-terminal) (point)))
      (call-interactively #'newline)
    (unless (= (eat-term-end eat-terminal) (point-max))
      (unless eat--line-input-ring
        (setq eat--line-input-ring
              (make-ring eat-line-input-ring-size)))
      (ring-insert eat--line-input-ring
                   (buffer-substring-no-properties
                    (eat-term-end eat-terminal) (point-max))))
    (unless no-newline
      (goto-char (point-max))
      (insert "\n"))
    (eat-line-send)))

(defun eat-line-delchar-or-eof (arg)
  "Delete character or send shell prompt input to the terminal.

ARG is the prefix arg, passed to `delete-char' when deleting
character."
  (interactive "p")
  (if (not (= (eat-term-end eat-terminal) (point-max)))
      (delete-char arg)
    (insert "\C-d")
    (eat-line-send)))

(defun eat-line-send-interrupt ()
  "Clear the input and send `C-c' to the shell."
  (interactive)
  (delete-region (eat-term-end eat-terminal) (point-max))
  (goto-char (point-max))
  (insert "\C-c")
  (eat-line-send))


;;;;;; History.

;; The following code in this page (or section) is adapted from
;; Comint source.

(defvar eat--line-input-ring nil
  "Ring holding the history of inputs.")

(defvar eat--line-input-ring-index nil
  "Index of last matched history element.")

(defvar eat--line-stored-incomplete-input nil
  "Stored input for history cycling.")

(defvar eat--line-matching-input-from-input-string ""
  "Input previously used to match input history.")

(defvar eat--saved-line-input-history-isearch 'not-saved
  "Saved value of `eat-line-input-history-isearch'.")

(defun eat--line-reset-input-ring-vars ()
  "Reset variable after a new shell prompt."
  (setq eat--line-input-ring-index nil)
  (setq eat--line-stored-incomplete-input nil)
  (setq eat--line-matching-input-from-input-string ""))

(defun eat--line-populate-input-ring (hist format)
  "Populate `eat--line-input-ring' from HIST in format FORMAT."
  (setq eat--line-input-ring (make-ring eat-line-input-ring-size))
  (pcase format
    ("bash"
     (dolist (item (string-split hist "\n" 'omit-nulls))
       (when (/= (aref item 0) ?#)
         (ring-insert eat--line-input-ring item))))
    ("zsh"
     (dolist (item (string-split hist "\n" 'omit-nulls))
       (ring-insert eat--line-input-ring
                    (string-trim item (rx ": " (zero-or-more digit)
                                          ?: (zero-or-more digit)
                                          ?\;)))))))

(defun eat-line-load-input-history-from-file (file format)
  "Load input history from FILE.

FORMAT is the format of FILE."
  (interactive
   (let ((file (read-file-name "History file: ")))
     (list file (completing-read
                 "History file format: " '("bash" "zsh")
                 nil t (pcase (file-name-nondirectory file)
                         (".bash_history" "bash")
                         (".zsh_history" "zsh"))))))
  (let ((str nil))
    (with-temp-buffer
      (insert-file-contents file)
      (setq str (buffer-string)))
    (eat--line-populate-input-ring str format)))

(defun eat--line-ask-for-regexp-arg (prompt)
  "Return list of regexp and prefix arg using PROMPT."
  (let* (;; Don't clobber this.
         (last-command last-command)
         (regexp (read-from-minibuffer
                  prompt nil nil nil
                  'minibuffer-history-search-history)))
    ;; If the user didn't enter anything, nothing is added to m-h-s-h.
    ;; Use the previous search regexp, if there is one.
    (list (if (string-equal regexp "")
              (or (car minibuffer-history-search-history)
                  regexp)
            regexp)
          (prefix-numeric-value current-prefix-arg))))

(defun eat--line-search-arg (arg)
  "Check point, and return ARG, or one if ARG is zero."
  ;; First make sure there is a ring and that we are after the
  ;; terminal region.
  (cond ((< (point) (eat-term-end eat-terminal))
         (user-error "Not at command line"))
        ((or (null eat--line-input-ring)
             (ring-empty-p eat--line-input-ring))
         (user-error "Empty input ring"))
        ((zerop arg)
         ;; ARG zero resets search from beginning, and uses ARG 1.
         (setq eat--line-input-ring-index nil)
         1)
        (t
         arg)))

(defun eat-line-restore-input ()
  "Restore unfinished input."
  (interactive)
  (when eat--line-input-ring-index
    (delete-region (eat-term-end eat-terminal) (point-max))
    (when (> (length eat--line-stored-incomplete-input) 0)
      (insert eat--line-stored-incomplete-input)
      (message "Input restored"))
    (setq eat--line-input-ring-index nil)))

(defun eat--line-search-start (arg)
  "Index to start a directional search, ARG indicates the direction."
  (if eat--line-input-ring-index
      ;; If a search is running, offset by 1 in direction of ARG.
      (mod (+ eat--line-input-ring-index (if (> arg 0) 1 -1))
           (ring-length eat--line-input-ring))
    ;; For a new search, start from end if ARG is negative, or from
    ;; beginning otherwise.
    (if (> arg 0)
        0
      (1- (ring-length eat--line-input-ring)))))

(defun eat--line-prev-input-string (arg)
  "Return the string ARG places along the input ring.
Moves relative to `eat--line-input-ring-index'."
  (ring-ref eat--line-input-ring
            (if eat--line-input-ring-index
                (mod (+ arg eat--line-input-ring-index)
                     (ring-length eat--line-input-ring))
              arg)))

(defun eat-line-previous-input (arg)
  "Cycle backwards through input history, saving input.

Negative ARG means search forward instead."
  (interactive "*p")
  (if (and eat--line-input-ring-index
           ;; Are we leaving the "end" of the ring?
           (or (and (< arg 0)           ; going down
                    (eq eat--line-input-ring-index 0))
               (and (> arg 0)           ; going up
                    (eq eat--line-input-ring-index
                        (1- (ring-length eat--line-input-ring)))))
           eat--line-stored-incomplete-input)
      (eat-line-restore-input)
    (eat-line-previous-matching-input "." arg)))

(defun eat-line-next-input (arg)
  "Cycle forwards through input history, saving input.

Negative ARG means search backward instead."
  (interactive "*p")
  (eat-line-previous-input (- arg)))

(defun eat--line-prev-matching-input-str (regexp arg)
  "Return the string matching REGEXP ARG places along the input ring.
Moves relative to `eat--line-input-ring-index'."
  (let* ((pos (eat--line-prev-matching-input-str-pos regexp arg)))
    (if pos (ring-ref eat--line-input-ring pos))))

(defun eat--line-prev-matching-input-str-pos
    (regexp arg &optional start)
  "Return the index matching REGEXP ARG places along the input ring.
Moves relative to START, or `eat--line-input-ring-index'."
  (when (or (not (ring-p eat--line-input-ring))
            (ring-empty-p eat--line-input-ring))
    (user-error "No history"))
  (let* ((len (ring-length eat--line-input-ring))
         (motion (if (> arg 0) 1 -1))
         (n (mod (- (or start (eat--line-search-start arg)) motion)
                 len))
         (tried-each-ring-item nil)
         (prev nil))
    ;; Do the whole search as many times as the argument says.
    (while (and (/= arg 0) (not tried-each-ring-item))
      ;; Step once.
      (setq prev n)
      (setq n (mod (+ n motion) len))
      ;; If we haven't reached a match, step some more.
      (while (and (< n len) (not tried-each-ring-item)
                  (not (string-match regexp
                                     (ring-ref
                                      eat--line-input-ring n))))
        (setq n (mod (+ n motion) len))
        ;; If we have gone all the way around in this search.
        (setq tried-each-ring-item (= n prev)))
      (setq arg (if (> arg 0) (1- arg) (1+ arg))))
    ;; Now that we know which ring element to use, if we found it,
    ;; return that.
    (when (string-match regexp (ring-ref eat--line-input-ring n))
      n)))

(defun eat-line-previous-matching-input (regexp n &optional restore)
  "Search backwards through input history for match for REGEXP.

\(Previous history elements are earlier commands.)
With prefix argument N, search for Nth previous match.
If N is negative, find the next or Nth next match.

If RESTORE is non-nil, restore input in case of wrap."
  (interactive (eat--line-ask-for-regexp-arg
                "Previous input matching (regexp): "))
  (setq n (eat--line-search-arg n))
  (let ((pos (eat--line-prev-matching-input-str-pos regexp n)))
    ;; Has a match been found?
    (if (null pos)
        (user-error "Not found")
      (if (and eat--line-input-ring-index
               restore
               (or (and (< n 0)
                        (< eat--line-input-ring-index pos))
                   (and (> n 0)
                        (> eat--line-input-ring-index pos))))
          ;; We have a wrap; restore contents.
          (eat-line-restore-input)
        ;; If leaving the edit line, save partial input.
        (if (null eat--line-input-ring-index) ;not yet on ring
            (setq eat--line-stored-incomplete-input
                  (buffer-substring-no-properties
                   (eat-term-end eat-terminal) (point-max))))
        (setq eat--line-input-ring-index pos)
        (unless isearch-mode
          (let ((message-log-max nil))  ; Do not write to *Messages*.
            (message "History item: %d" (1+ pos))))
        (delete-region (eat-term-end eat-terminal) (point-max))
        (insert (ring-ref eat--line-input-ring pos))))))

(defun eat-line-next-matching-input (regexp n)
  "Search forwards through input history for match for REGEXP.
\(Later history elements are more recent commands.)
With prefix argument N, search for Nth following match.
If N is negative, find the previous or Nth previous match."
  (interactive (eat--line-ask-for-regexp-arg
                "Next input matching (regexp): "))
  (eat-line-previous-matching-input regexp (- n)))

(defun eat-line-previous-matching-input-from-input (n)
  "Search backwards through input history for match for current input.
\(Previous history elements are earlier commands.)
With prefix argument N, search for Nth previous match.
If N is negative, search forwards for the -Nth following match."
  (interactive "p")
  (let ((opoint (point)))
    (unless (memq last-command
                  '(eat-line-previous-matching-input-from-input
                    eat-line-next-matching-input-from-input))
      ;; Starting a new search
      (setq eat--line-matching-input-from-input-string
            (buffer-substring (eat-term-end eat-terminal)
                              (point-max)))
      (setq eat--line-input-ring-index nil))
    (eat-line-previous-matching-input
     (concat "^" (regexp-quote
                  eat--line-matching-input-from-input-string))
     n t)
    (when (eq eat-line-move-point-for-matching-input 'after-input)
      (goto-char opoint))))

(defun eat-line-next-matching-input-from-input (n)
  "Search forwards through input history for match for current input.
\(Following history elements are more recent commands.)
With prefix argument N, search for Nth following match.
If N is negative, search backwards for the -Nth previous match."
  (interactive "p")
  (eat-line-previous-matching-input-from-input (- n)))

(defun eat-line-find-input ()
  "Find and insert input history using minibuffer."
  (declare (interactive-only t))
  (interactive)
  (when (or (not (ring-p eat--line-input-ring))
            (ring-empty-p eat--line-input-ring))
    (user-error "No history"))
  (let ((str (completing-read
              "Input: "
              (seq-uniq (ring-elements eat--line-input-ring)) nil
              nil (buffer-substring (eat-term-end eat-terminal)
                                    (point-max))))
        (i 0)
        (pos nil))
    (while (and (< i (ring-length eat--line-input-ring)) (not pos))
      (when (equal (ring-ref eat--line-input-ring i) str)
        (setq pos i))
      (cl-incf i))
    (when pos
      (setq eat--line-input-ring-index pos))
    (delete-region (eat-term-end eat-terminal) (point-max))
    (insert str)))

(defun eat-line-history-isearch-backward ()
  "Search for a string backward in input history using Isearch."
  (interactive)
  (setq eat--saved-line-input-history-isearch
        eat-line-input-history-isearch)
  (setq eat-line-input-history-isearch t)
  (isearch-backward nil t))

(defun eat-line-history-isearch-backward-regexp ()
  "Search for a regular expression backward in input history using Isearch."
  (interactive)
  (setq eat--saved-line-input-history-isearch
        eat-line-input-history-isearch)
  (setq eat-line-input-history-isearch t)
  (isearch-backward-regexp nil t))

(defun eat--line-history-isearch-setup ()
  "Set up Eat buffer for using Isearch to search the input history."
  (when (or (eq eat-line-input-history-isearch t)
            (and (eq eat-line-input-history-isearch 'dwim)
                 (>= (point) (eat-term-end eat-terminal))))
    (setq isearch-message-prefix-add "history ")
    (setq isearch-search-fun-function
          #'eat--line-history-isearch-search)
    (setq isearch-wrap-function #'eat--line-history-isearch-wrap)
    (setq isearch-push-state-function
          #'eat--line-history-isearch-push-state)
    (make-local-variable 'isearch-lazy-count)
    (setq isearch-lazy-count nil)
    (add-hook 'isearch-mode-end-hook
              'eat--line-history-isearch-end nil t)))

(defun eat--line-history-isearch-end ()
  "Clean up after terminating Isearch."
  (setq isearch-message-prefix-add nil)
  (setq isearch-search-fun-function 'isearch-search-fun-default)
  (setq isearch-wrap-function nil)
  (setq isearch-push-state-function nil)
  ;; Force isearch to not change mark.
  (setq isearch-opoint (point))
  (kill-local-variable 'isearch-lazy-count)
  (remove-hook 'isearch-mode-end-hook
               'eat--line-history-isearch-end t)
  (unless (or isearch-suspended
              (eq eat--saved-line-input-history-isearch 'not-saved))
    (setq eat-line-input-history-isearch
          eat--saved-line-input-history-isearch)
    (setq eat--saved-line-input-history-isearch 'not-saved)))

(defun eat--line-goto-input (pos)
  "Put input history item of the absolute history position POS."
  ;; If leaving the edit line, save partial unfinished input.
  (when (null eat--line-input-ring-index)
    (setq eat--line-stored-incomplete-input
          (buffer-substring-no-properties
           (eat-term-end eat-terminal) (point-max))))
  (setq eat--line-input-ring-index pos)
  (delete-region (eat-term-end eat-terminal) (point-max))
  (if (and pos (not (ring-empty-p eat--line-input-ring)))
      (insert (ring-ref eat--line-input-ring pos))
    ;; Restore partial unfinished input.
    (when (> (length eat--line-stored-incomplete-input) 0)
      (insert eat--line-stored-incomplete-input))))

(defun eat--line-history-isearch-search ()
  "Return the proper search function, for Isearch in input history."
  (lambda (string bound noerror)
    (let ((search-fun (isearch-search-fun-default))
          found)
      ;; Avoid lazy-highlighting matches in the input line and in the
      ;; output when searching forward.  Lazy-highlight calls this
      ;; lambda with the bound arg, so skip the prompt and the output.
      (when (and bound isearch-forward
                 (< (point) (eat-term-end eat-terminal)))
        (goto-char (eat-term-end eat-terminal)))
      (or
       ;; 1. First try searching in the initial input line
       (funcall search-fun string (if isearch-forward
                                      bound
                                    (eat-term-end eat-terminal))
                noerror)
       ;; 2. If the above search fails, start putting next/prev
       ;; history elements in the input line successively, and search
       ;; the string in them.  Do this only when bound is nil
       ;; (i.e. not while lazy-highlighting search strings in the
       ;; current input line).
       (unless bound
         (condition-case nil
             (progn
               (while (not found)
                 (cond
                  (isearch-forward
                   ;; Signal an error here explicitly, because
                   ;; `eat-line-next-input' doesn't signal an
                   ;; error.
                   (when (null eat--line-input-ring-index)
                     (error "End of history; no next item"))
                   (eat-line-next-input 1)
                   (goto-char (eat-term-end eat-terminal)))
                  (t
                   ;; Signal an error here explicitly, because
                   ;; `eat-line-previous-input' doesn't signal an
                   ;; error.
                   (when (eq eat--line-input-ring-index
                             (1- (ring-length eat--line-input-ring)))
                     (error
                      "Beginning of history; no preceding item"))
                   (eat-line-previous-input 1)
                   (goto-char (point-max))))
                 (setq isearch-barrier (point))
                 (setq isearch-opoint (point))
                 ;; After putting the next/prev history element,
                 ;; search the string in them again, until
                 ;; `eat-line-next-input' or `eat-line-previous-input'
                 ;; raises an error at the beginning/end of history.
                 (setq found
                       (funcall search-fun string
                                (unless isearch-forward
                                  ;; For backward search, don't search
                                  ;; in the terminal region
                                  (eat-term-end eat-terminal))
                                noerror)))
               ;; Return point of the new search result
               (point))
           ;; Return nil on the error "no next/preceding item"
           (error nil)))))))

(defun eat--line-history-isearch-wrap ()
  "Wrap the input history search when search fails.

Move point to the first history element for a forward search,
or to the last history element for a backward search."
  ;; When `eat--line-history-isearch-search' fails on reaching the
  ;; beginning/end of the history, wrap the search to the first/last
  ;; input history element.
  (if isearch-forward
      (eat--line-goto-input (1- (ring-length eat--line-input-ring)))
    (eat--line-goto-input nil))
  (goto-char (if isearch-forward
                 (eat-term-end eat-terminal)
               (point-max))))

(defun eat--line-history-isearch-push-state ()
  "Save a function restoring the state of input history search.

Save `eat--line-input-ring-index' to the additional state parameter
in the search status stack."
  (let ((index eat--line-input-ring-index))
    (lambda (cmd)
      (eat--line-history-isearch-pop-state cmd index))))

(defun eat--line-history-isearch-pop-state (_cmd hist-pos)
  "Restore the input history search state.
Go to the history element by the absolute history position HIST-POS."
  (eat--line-goto-input hist-pos))


;;;;; Major Mode.

(defun eat--synchronize-scroll-windows (&optional force-selected)
  "Return the list of windows whose scrolling should be synchronized.

When FORCE-SELECTED is non-nil, always include `buffer' and the
selected window in the list if the window is showing the current
buffer."
  `(,@(and (or force-selected
               eat--char-mode
               (= (eat-term-display-cursor eat-terminal) (point)))
           '(buffer))
    ,@(seq-filter
       (lambda (window)
         (or (and force-selected (eq window (selected-window)))
             (= (eat-term-display-cursor eat-terminal)
                (window-point window))))
       (get-buffer-window-list))))

(defun eat--synchronize-scroll (windows)
  "Synchronize scrolling and point between terminal and WINDOWS.

WINDOWS is a list of windows.  WINDOWS may also contain the special
symbol `buffer', in which case the point of current buffer is set."
  (dolist (window windows)
    (if (eq window 'buffer)
        (goto-char (eat-term-display-cursor eat-terminal))
      (with-selected-window window
        (set-window-point nil (eat-term-display-cursor eat-terminal))
        (recenter
         (- (how-many "\n" (eat-term-display-beginning eat-terminal)
                      (eat-term-display-cursor eat-terminal))
            (cdr (eat-term-size eat-terminal))
            (max 0 (- (floor (window-screen-lines))
                      (cdr (eat-term-size eat-terminal))))))))))

(defun eat--setup-glyphless-chars ()
  "Setup the display of glyphless characters."
  (setq-local glyphless-char-display
              (copy-sequence (default-value 'glyphless-char-display)))
  (set-char-table-extra-slot
   glyphless-char-display 0
   (if (display-graphic-p) 'empty-box 'thin-space)))

(defun eat--filter-buffer-substring (begin end &optional delete)
  "Filter buffer substring from BEGIN to END and return that.

When DELETE is given and non-nil, delete the text between BEGIN and
END if it's safe to do so."
  (let ((str (buffer-substring begin end)))
    (remove-text-properties 0 (length str)
                            '( read-only nil
                               rear-nonsticky nil
                               front-sticky nil
                               field nil
                               eat--before-string nil
                               eat--shell-prompt-mark-id nil
                               eat--shell-prompt-mark-overlay nil
                               eat--shell-prompt-begin nil
                               eat--shell-prompt-end nil)
                            str)
    (setq str (eat-term-filter-string str))
    (when (and delete
               (or (not eat-terminal)
                   (and (<= (eat-term-end eat-terminal) begin)
                        (<= (eat-term-end eat-terminal) end))
                   (and (<= begin (eat-term-beginning eat-terminal))
                        (<= end (eat-term-beginning eat-terminal)))))
      (delete-region begin end))
    str))

(define-derived-mode eat-mode fundamental-mode "Eat"
  "Major mode for Eat."
  :group 'eat-ui
  (mapc #'make-local-variable
        '(buffer-read-only
          buffer-undo-list
          filter-buffer-substring-function
          mode-line-process
          mode-line-buffer-identification
          glyphless-char-display
          cursor-type
          track-mouse
          scroll-margin
          hscroll-margin
          eat-terminal
          eat--synchronize-scroll-function
          eat--mouse-grabbing-type
          eat--shell-command-status
          eat--shell-prompt-begin
          eat--shell-prompt-mark
          eat--shell-prompt-mark-overlays
          eat--inhibit-auto-line-mode
          eat--auto-line-mode-prev-mode
          eat--line-input-ring
          eat--line-input-ring-index
          eat--line-stored-incomplete-input
          eat--line-matching-input-from-input-string
          isearch-search-fun-function
          isearch-wrap-function
          isearch-push-state-function
          eat--pending-output-chunks
          eat--output-queue-first-chunk-time
          eat--process-output-queue-timer
          eat--shell-prompt-annotation-correction-timer))
  ;; This is intended; input methods don't work on read-only buffers.
  (setq buffer-read-only nil)
  (setq scroll-margin 0)
  (setq hscroll-margin 0)
  (setq eat--synchronize-scroll-function #'eat--synchronize-scroll)
  (setq filter-buffer-substring-function
        #'eat--filter-buffer-substring)
  (setq bidi-paragraph-direction 'left-to-right)
  (setq eat--mouse-grabbing-type nil)
  (add-hook 'isearch-mode-hook 'eat--line-history-isearch-setup nil t)
  (setq mode-line-process
        '(""
          (:eval
           (when eat-terminal
             (cond
              (eat--semi-char-mode
               '("["
                 (:propertize
                  "semi-char"
                  help-echo "mouse-1: Switch to char mode, \
mouse-2: Switch to line mode, mouse-3: Switch to emacs mode"
                  mouse-face mode-line-highlight
                  local-map
                  (keymap
                   (mode-line
                    . (keymap
                       (down-mouse-1 . eat-char-mode)
                       (down-mouse-2 . eat-line-mode)
                       (down-mouse-3 . eat-emacs-mode)))))
                 "]"))
              (eat--char-mode
               '("["
                 (:propertize
                  "char"
                  help-echo "mouse-1: Switch to semi-char mode, \
mouse-2: Switch to line mode, mouse-3: Switch to emacs mode"
                  mouse-face mode-line-highlight
                  local-map
                  (keymap
                   (mode-line
                    . (keymap
                       (down-mouse-1 . eat-semi-char-mode)
                       (down-mouse-2 . eat-line-mode)
                       (down-mouse-3 . eat-emacs-mode)))))
                 "]"))
              (eat--line-mode
               '("["
                 (:propertize
                  "line"
                  help-echo "mouse-1: Switch to semi char mode, \
mouse-2: Switch to emacs mode, mouse-3: Switch to char mode"
                  mouse-face mode-line-highlight
                  local-map
                  (keymap
                   (mode-line
                    . (keymap
                       (down-mouse-1 . eat-semi-char-mode)
                       (down-mouse-2 . eat-emacs-mode)
                       (down-mouse-3 . eat-char-mode)))))
                 "]"))
              (t
               '("["
                 (:propertize
                  "emacs"
                  help-echo "mouse-1: Switch to semi char mode, \
mouse-3: Switch to char mode"
                  mouse-face mode-line-highlight
                  local-map
                  (keymap
                   (mode-line
                    . (keymap
                       (down-mouse-1 . eat-semi-char-mode)
                       (down-mouse-2 . eat-line-mode)
                       (down-mouse-3 . eat-char-mode)))))
                 "]")))))
          ":%s"))
  (setq mode-line-buffer-identification
        `(12 (""
              ,(nconc
                (propertized-buffer-identification "%b")
                '(" "
                  (:propertize
                   (:eval
                    (when-let* ((eat-terminal)
                                (title (eat-term-title eat-terminal))
                                ((not (string-empty-p title))))
                      (format "(%s)" (string-replace "%" "%%"
                                                     title))))
                   help-echo "Title"))))))
  (eat-emacs-mode)
  ;; Make sure glyphless character don't display a huge box glyph,
  ;; that would break the display.
  (eat--setup-glyphless-chars)
  ;; Setup completion for line mode.
  (shell-completion-vars)
  (when eat-enable-blinking-text
    (eat-blink-mode +1))
  (when eat-enable-shell-prompt-annotation
    (let ((margin-width
           (max
            (string-width
             eat-shell-prompt-annotation-running-margin-indicator)
            (string-width
             eat-shell-prompt-annotation-success-margin-indicator)
            (string-width
             eat-shell-prompt-annotation-failure-margin-indicator))))
      (pcase-exhaustive eat-shell-prompt-annotation-position
        ('left-margin
         (setq left-margin-width margin-width))
        ('right-margin
         (setq right-margin-width margin-width))))
    ;; Make sure the marginal area is resized.
    (dolist (win (get-buffer-window-list))
      (set-window-buffer win (current-buffer)))))


;;;;; Process Handling.

(defvar eat--pending-output-chunks nil
  "The list of pending output chunks.

The output chunks are pushed, so last output appears first.")

(defvar eat--output-queue-first-chunk-time nil
  "Time when the first chunk in the current output queue was pushed.")

(defvar eat--process-output-queue-timer nil
  "Timer to process output queue.")

(defvar eat--shell-prompt-annotation-correction-timer nil
  "Timer to correct shell prompt annotations.")

(defun eat-kill-process ()
  "Kill Eat process in current buffer."
  (interactive)
  (when-let* ((eat-terminal)
              (proc (eat-term-parameter eat-terminal 'eat--process)))
    (delete-process proc)))

(defun eat--send-string (process string)
  "Send to PROCESS the contents of STRING as input.

This is equivalent to `process-send-string', except that long input
strings are broken up into chunks of size `eat-input-chunk-size'.
Processes are given a chance to output between chunks.  This can help
prevent processes from hanging when you send them long inputs on some
OS's."
  (let ((i 0)
        (j eat-input-chunk-size)
        (l (length string)))
    (while (< i l)
      (process-send-string process (substring string i (min j l)))
      (accept-process-output)
      (cl-incf i eat-input-chunk-size)
      (cl-incf j eat-input-chunk-size))))

(defun eat--send-input (_ input)
  "Send INPUT to subprocess."
  (when-let* ((eat-terminal)
              (proc (eat-term-parameter eat-terminal 'eat--process)))
    (eat--send-string proc input)))

(defun eat--process-output-queue (buffer)
  "Process the output queue on BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-quit t)        ; Don't disturb!
            (sync-windows (eat--synchronize-scroll-windows))
            (eat--auto-line-mode-pending-toggles nil))
        (save-restriction
          (widen)
          (let ((inhibit-read-only t)
                (inhibit-modification-hooks t)
                ;; Don't let `undo' mess up with the terminal.
                (buffer-undo-list t))
            (when eat--process-output-queue-timer
              (cancel-timer eat--process-output-queue-timer))
            (setq eat--output-queue-first-chunk-time nil)
            (while eat--pending-output-chunks
              (let ((queue eat--pending-output-chunks)
                    (eat--output-queue-first-chunk-time t))
                (setq eat--pending-output-chunks nil)
                (dolist (output (nreverse queue))
                  (eat-term-process-output eat-terminal output))))
            (eat-term-redisplay eat-terminal)
            ;; Truncate output of previous dead processes.
            (when (and eat-term-scrollback-size
                       (< eat-term-scrollback-size
                          (- (point) (point-min))))
              (delete-region
               (point-min)
               (max (point-min)
                    (- (eat-term-display-beginning eat-terminal)
                       eat-term-scrollback-size))))
            (setq eat--shell-prompt-annotation-correction-timer
                  (run-with-timer
                   eat-shell-prompt-annotation-correction-delay
                   nil #'eat--correct-shell-prompt-mark-overlays
                   buffer))
            (add-text-properties
             (eat-term-beginning eat-terminal)
             (eat-term-end eat-terminal)
             `( read-only t field eat-terminal
                ,@(when eat--line-mode
                    '(front-sticky t rear-nonsticky t))))))
        (eat--line-mode-do-toggles)
        (funcall eat--synchronize-scroll-function sync-windows))
      (run-hooks 'eat-update-hook))))

(defun eat--filter (process output)
  "Handle OUTPUT from PROCESS."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (when eat--process-output-queue-timer
        (cancel-timer eat--process-output-queue-timer))
      (when eat--shell-prompt-annotation-correction-timer
        (cancel-timer eat--shell-prompt-annotation-correction-timer))
      (unless eat--output-queue-first-chunk-time
        (setq eat--output-queue-first-chunk-time (current-time)))
      (push output eat--pending-output-chunks)
      (unless (eq eat--output-queue-first-chunk-time t)
        (let ((time-left
               (- eat-maximum-latency
                  (float-time
                   (time-subtract
                    nil eat--output-queue-first-chunk-time)))))
          (if (<= time-left 0)
              (eat--process-output-queue (current-buffer))
            (setq eat--process-output-queue-timer
                  (run-with-timer
                   (min time-left eat-minimum-latency) nil
                   #'eat--process-output-queue
                   (current-buffer)))))))))

(defun eat--sentinel (process message)
  "Sentinel for Eat buffers.

PROCESS is the process and MESSAGE is the description of what happened
to it."
  (let ((buffer (process-buffer process)))
    (when (memq (process-status process) '(signal exit))
      (if (buffer-live-p buffer)
          (with-current-buffer buffer
            (let ((inhibit-read-only t)
                  ;; We're is going to write outside of the terminal,
                  ;; so we won't synchronize buffer scroll here as we
                  ;; will set the buffer point automatically by
                  ;; writing to the buffer.
                  (eat--synchronize-scroll-function #'ignore))
              (when eat--process-output-queue-timer
                (cancel-timer eat--process-output-queue-timer)
                (setq eat--process-output-queue-timer nil))
              (eat--process-output-queue buffer)
              (when eat--shell-prompt-annotation-correction-timer
                (cancel-timer
                 eat--shell-prompt-annotation-correction-timer)
                (setq eat--shell-prompt-annotation-correction-timer
                      nil))
              (when eat-enable-shell-prompt-annotation
                (eat--correct-shell-prompt-mark-overlays buffer)
                (setq eat--shell-command-status 0)
                (setq eat--shell-prompt-begin nil)
                (setq eat--shell-prompt-mark nil)
                (setq eat--shell-prompt-mark-overlays nil))
              (when eat--line-mode
                (eat--line-mode -1)
                (delete-region (eat-term-end eat-terminal)
                               (point-max)))
              (eat-emacs-mode)
              (remove-text-properties
               (eat-term-beginning eat-terminal)
               (eat-term-end eat-terminal)
               '(read-only nil field nil))
              (eat-term-delete eat-terminal)
              (setq eat-terminal nil)
              (eat--set-cursor nil :default)
              (eat--grab-mouse nil nil)
              (goto-char (point-max))
              (insert "\nProcess " (process-name process) " "
                      message)
              (setq buffer-read-only nil))
            (run-hook-with-args 'eat-exit-hook process)
            (delete-process process))
        (set-process-buffer process nil)))))

(defun eat--adjust-process-window-size (process windows)
  "Resize process window and terminal.  Return new dimensions.

PROCESS is the process whose window to resize, and WINDOWS is the list
of window displaying PROCESS's buffer."
  (let ((size (funcall window-adjust-process-window-size-function
                       process windows)))
    (when size
      (let ((width (max (car size) 1))
            (height (max (cdr size) 1))
            (inhibit-read-only t)
            (sync-windows (eat--synchronize-scroll-windows)))
        (eat-term-resize eat-terminal width height)
        (eat-term-redisplay eat-terminal)
        (funcall eat--synchronize-scroll-function sync-windows))
      (pcase major-mode
        ('eat-mode
         (run-hooks 'eat-update-hook))
        ('eshell-mode
         (run-hooks 'eat-eshell-update-hook))))
    size))

(defun eat--kill-buffer (_process)
  "Kill current buffer."
  (kill-buffer (current-buffer)))

;; Adapted from Term.
(defun eat-exec (buffer name command startfile switches)
  "Start up a process in BUFFER for Eat mode.

Run COMMAND with SWITCHES.  Set NAME as the name of the process.
Blast any old process running in the buffer.  Don't set the buffer
mode.  You can use this to cheaply run a series of processes in the
same Eat buffer.  The hook `eat-exec-hook' is run after each exec."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (when-let* ((eat-terminal)
                  (proc (eat-term-parameter
                         eat-terminal 'eat--process)))
        (remove-hook 'eat-exit-hook #'eat--kill-buffer t)
        (delete-process proc))
      ;; Ensure final newline.
      (goto-char (point-max))
      (unless (or (= (point-min) (point-max))
                  (= (char-before (point-max)) ?\n))
        (insert ?\n))
      (unless (= (point-min) (point-max))
        (insert "\n\n"))
      (setq eat-terminal (eat-term-make buffer (point)))
      (eat-semi-char-mode)
      (when-let* ((window (get-buffer-window nil t)))
        (with-selected-window window
          (eat-term-resize eat-terminal (window-max-chars-per-line)
                           (floor (window-screen-lines)))))
      (setf (eat-term-parameter eat-terminal 'input-function)
            #'eat--send-input)
      (setf (eat-term-parameter eat-terminal 'set-cursor-function)
            #'eat--set-cursor)
      (setf (eat-term-parameter eat-terminal 'grab-mouse-function)
            #'eat--grab-mouse)
      (setf (eat-term-parameter
             eat-terminal 'manipulate-selection-function)
            #'eat--manipulate-kill-ring)
      (setf (eat-term-parameter eat-terminal 'ring-bell-function)
            #'eat--bell)
      (setf (eat-term-parameter eat-terminal 'set-cwd-function)
            #'eat--set-cwd)
      (setf (eat-term-parameter eat-terminal 'ui-command-function)
            #'eat--handle-uic)
      (eat--set-term-sixel-params)
      ;; Crank up a new process.
      (let* ((size (eat-term-size eat-terminal))
             (process-environment
              (nconc
               (list
                (concat "TERM=" (eat-term-name))
                (concat "TERMINFO=" eat-term-terminfo-directory)
                (concat "INSIDE_EMACS=" eat-term-inside-emacs)
                (concat "EAT_SHELL_INTEGRATION_DIR="
                        eat-term-shell-integration-directory))
               process-environment))
             (process-connection-type t)
             ;; We should suppress conversion of end-of-line format.
             (inhibit-eol-conversion t)
             (process
              (make-process
               :name name
               :buffer buffer
               :command `("/usr/bin/env" "sh" "-c"
                          ,(format "stty -nl echo rows %d columns \
%d sane 2>%s ; if [ $1 = .. ]; then shift; fi; exec \"$@\""
                                   (cdr size) (car size)
                                   null-device)
                          ".."
                          ,command ,@switches)
               :filter #'eat--filter
               :sentinel #'eat--sentinel
               :file-handler t)))
        (process-put process 'adjust-window-size-function
                     #'eat--adjust-process-window-size)
        (set-process-query-on-exit-flag
         process eat-query-before-killing-running-terminal)
        ;; Jump to the end, and set the process mark.
        (goto-char (point-max))
        (set-marker (process-mark process) (point))
        (setf (eat-term-parameter eat-terminal 'eat--process)
              process)
        (setf (eat-term-parameter eat-terminal 'eat--input-process)
              process)
        (setf (eat-term-parameter eat-terminal 'eat--output-process)
              process)
        (when eat-kill-buffer-on-exit
          (add-hook 'eat-exit-hook #'eat--kill-buffer 90 t))
        ;; Feed it the startfile.
        (when startfile
          ;; This is guaranteed to wait long enough
          ;; but has bad results if the shell does not prompt at all
          ;;          (while (= size (buffer-size))
          ;;            (sleep-for 1))
          ;; I hope 1 second is enough!
          (sleep-for 1)
          (goto-char (point-max))
          (insert-file-contents startfile)
          (process-send-string
           process (delete-and-extract-region (point) (point-max)))))
      (eat-term-redisplay eat-terminal))
    (run-hook-with-args 'eat-exec-hook (eat-term-parameter
                                        eat-terminal 'eat--process))
    buffer))


;;;;; Entry Points.

(defun eat-make (name program &optional startfile &rest switches)
  "Make a Eat process NAME in a buffer, running PROGRAM.

The name of the buffer is made by surrounding NAME with `*'s.  If
there is already a running process in that buffer, it is not
restarted.  Optional third arg STARTFILE is the name of a file to send
the contents of to the process.  SWITCHES are the arguments to
PROGRAM."
  (let ((buffer (get-buffer-create (concat "*" name "*"))))
    ;; If no process, or nuked process, crank up a new one and put
    ;; buffer in Eat mode.  Otherwise, leave buffer and existing
    ;; process alone.
    (when (not (let ((proc (get-buffer-process buffer)))
                 (and proc (memq (process-status proc)
                                 '(run stop open listen connect)))))
      (with-current-buffer buffer
        (eat-mode))
      (eat-exec buffer name program startfile switches))
    buffer))

(defun eat--1 (program arg display-buffer-fn)
  "Start a new Eat terminal emulator in a buffer.

PROGRAM and ARG is same as in `eat' and `eat-other-window'.
DISPLAY-BUFFER-FN is the function to display the buffer."
  (let ((program (or program (or explicit-shell-file-name
                                 (getenv "ESHELL")
                                 shell-file-name)))
        (buffer
         (cond
          ((numberp arg)
           (get-buffer-create (format "%s<%d>" eat-buffer-name arg)))
          (arg
           (generate-new-buffer eat-buffer-name))
          (t
           (get-buffer-create eat-buffer-name)))))
    (with-current-buffer buffer
      (unless (eq major-mode #'eat-mode)
        (eat-mode))
      (funcall display-buffer-fn buffer)
      (unless (and eat-terminal
                   (eat-term-parameter eat-terminal 'eat--process))
        (eat-exec buffer (buffer-name) "/usr/bin/env" nil
                  (list "sh" "-c" program)))
      buffer)))

;;;###autoload
(defun eat (&optional program arg)
  "Start a new Eat terminal emulator in a buffer.

Start a new Eat session, or switch to an already active session.
Return the buffer selected (or created).

With a non-numeric prefix ARG, create a new session.

With a numeric prefix ARG (like \\[universal-argument] 42 \\[eat]),
switch to the session with that number, or create it if it doesn't
already exist.

With double prefix argument ARG, ask for the program to run and run it
in a newly created session.

PROGRAM can be a shell command."
  (interactive
   (list (when (equal current-prefix-arg '(16))
           (read-shell-command "Run program: "
                               (or explicit-shell-file-name
                                   (getenv "ESHELL")
                                   shell-file-name)))
         current-prefix-arg))
  (eat--1 program arg #'pop-to-buffer-same-window))

;;;###autoload
(defun eat-other-window (&optional program arg)
  "Start a new Eat terminal emulator in a buffer in another window.

Start a new Eat session, or switch to an already active session.
Return the buffer selected (or created).

With a non-numeric prefix ARG, create a new session.

With a numeric prefix ARG switch to the session with that number, or
create it if it doesn't already exist.

With double prefix argument ARG, ask for the program to run and run it
in a newly created session.

PROGRAM can be a shell command."
  (interactive
   (list (when (equal current-prefix-arg '(16))
           (read-shell-command "Run program: "
                               (or explicit-shell-file-name
                                   (getenv "ESHELL")
                                   shell-file-name)))
         current-prefix-arg))
  (eat--1 program arg #'pop-to-buffer))


;;;; Eshell integration.

;;;;; Input.

;; When changing these keymaps, be sure to update the manual, README
;; and commentary.
(defvar eat-eshell-emacs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C-c ?\C-j] #'eat-eshell-semi-char-mode)
    (define-key map [remap eshell-toggle-direct-send] ; C-c M-d
                #'eat-eshell-char-mode)
    (define-key map [remap undo] #'undefined) ; Disable `undo'.
    (define-key map [xterm-paste] #'ignore)
    map)
  "Keymap for Eat Eshell \"emacs\" mode.")

(defun eat--eshell-prepare-semi-char-mode-map ()
  "Prepare `eat-eshell-semi-char-mode-map'."
  (let ((map (eat-term-make-keymap
              #'eat-self-input '(:ascii :arrow :navigation)
              `([?\C-c] [?\C-q] [?\C-y] [?\e ?y]
                ,@eat-eshell-semi-char-non-bound-keys))))
    (define-key map [?\C-q] #'eat-quoted-input)
    (define-key map [?\C-y] #'eat-yank)
    (define-key map [?\M-y] #'eat-yank-from-kill-ring)
    (define-key map [?\C-c ?\C-e] #'eat-eshell-emacs-mode)
    (define-key map [S-insert] #'eat-yank)
    (define-key map [remap insert-char] #'eat-input-char)
    (define-key map [remap mouse-yank-primary]
                #'eat-mouse-yank-primary)
    (define-key map [remap mouse-yank-secondary]
                #'eat-mouse-yank-secondary)
    (define-key map [xterm-paste] #'eat-xterm-paste)
    map))

(defvar eat-eshell-semi-char-mode-map
  (ignore-errors
    (eat--eshell-prepare-semi-char-mode-map))
  "Keymap for Eat Eshell semi-char mode.")

(defun eat-eshell-update-semi-char-mode-map ()
  "Update \"semi-char\" keybinding mode's keymap in Eshell."
  (setq eat-eshell-semi-char-mode-map
        (eat--eshell-prepare-semi-char-mode-map)))

(defvar eat-eshell-char-mode-map
  (let ((map (eat-term-make-keymap
              #'eat-self-input '(:ascii :arrow :navigation :function)
              '([?\e ?\C-m]))))
    (define-key map [?\C-\M-m] #'eat-eshell-semi-char-mode)
    (define-key map [xterm-paste] #'eat-xterm-paste)
    map)
  "Keymap for Eat Eshell char mode.")

(define-minor-mode eat--eshell-process-running-mode
  "Minor mode for \"emacs\" mode keymap when process is running."
  :interactive nil
  :keymap eat-eshell-emacs-mode-map)

(define-minor-mode eat--eshell-semi-char-mode
  "Minor mode for semi-char mode keymap."
  :interactive nil
  :keymap eat-eshell-semi-char-mode-map
  ;; HACK: Some keys like `C-c' are overriden by other keymaps
  ;; (possibly by the keymaps of other minor modes), so we also put
  ;; the keymap to `minor-mode-overriding-map-alist' to make Emacs
  ;; prioritize us.
  (setq minor-mode-overriding-map-alist
        (delete (cons #'eat--eshell-semi-char-mode
                      eat-eshell-semi-char-mode-map)
                minor-mode-overriding-map-alist))
  (when eat--eshell-semi-char-mode
    (push (cons #'eat--eshell-semi-char-mode
                eat-eshell-semi-char-mode-map)
          minor-mode-overriding-map-alist)))

(define-minor-mode eat--eshell-char-mode
  "Minor mode for char mode keymap."
  :interactive nil
  :keymap eat-eshell-char-mode-map
  ;; HACK: Some keys like `C-c' are overriden by other keymaps
  ;; (possibly by the keymaps of other minor modes), so we also put
  ;; the keymap to `minor-mode-overriding-map-alist' to make Emacs
  ;; prioritize us.
  (setq minor-mode-overriding-map-alist
        (delete (cons #'eat--eshell-char-mode
                      eat-eshell-char-mode-map)
                minor-mode-overriding-map-alist))
  (when eat--eshell-char-mode
    (push (cons #'eat--eshell-char-mode eat-eshell-char-mode-map)
          minor-mode-overriding-map-alist)))

(defun eat-eshell-emacs-mode ()
  "Switch to Emacs keybindings mode."
  (interactive)
  (eat--eshell-semi-char-mode -1)
  (eat--eshell-char-mode -1)
  (setq buffer-read-only t)
  (eat--grab-mouse nil eat--mouse-grabbing-type)
  (force-mode-line-update))

(defun eat-eshell-semi-char-mode ()
  "Switch to semi-char mode."
  (interactive)
  (when eat-terminal
    (setq buffer-read-only nil)
    (eat--eshell-char-mode -1)
    (eat--eshell-semi-char-mode +1)
    (eat--grab-mouse nil eat--mouse-grabbing-type)
    (force-mode-line-update)))

(defun eat-eshell-char-mode ()
  "Switch to char mode."
  (interactive)
  (when eat-terminal
    (setq buffer-read-only nil)
    (eat--eshell-semi-char-mode -1)
    (eat--eshell-char-mode +1)
    (eat--grab-mouse nil eat--mouse-grabbing-type)
    (force-mode-line-update)))


;;;;; Process Handling.

(defvar eat--eshell-invocation-directory nil
  "The directory from where the current process was started.")

(defvar eshell-last-output-start) ; In `esh-mode'.
(defvar eshell-last-output-end) ; In `esh-mode'.
(defvar eshell-output-filter-functions) ; In `esh-mode'.
(defvar eshell-parent-buffer) ; In `em-term'.
(declare-function eshell-head-process "esh-cmd" ())
(declare-function eshell-resume-eval "esh-cmd" ())

(defun eat--eshell-handle-uic (_ cmd)
  "Handle UI Command sequence CMD."
  (pcase cmd
    ;; UIC e ; A ; <t> ; <s> ST.
    ((rx string-start "e;A;"
         (let host (zero-or-more (not (any ?\;))))
         ?\; (let path (zero-or-more anything))
         string-end)
     (eat--set-cwd-uic host path))
    ;; UIC e ; F ; <t> ST.
    ((rx string-start "e;F;"
         (let cmd (zero-or-more anything))
         string-end)
     (eat--set-cmd cmd))
    ;; UIC e ; I ; 0 ; <t> ST.
    ((rx string-start "e;I;0;" (zero-or-more anything) string-end)
     (eat-term-send-string eat-terminal "\e]51;e;I;0\e\\"))
    ;; UIC e ; M ; ... ST.
    ((rx string-start "e;M;"
         (let msg (zero-or-more anything))
         string-end)
     (apply #'eat--handle-message (string-split msg ";")))
    ;; Other sequences are ignored.
    ))

(defun eat--eshell-term-name (&rest _)
  "Return the value of `TERM' environment variable for Eshell."
  (eat-term-name))

(defun eat--eshell-output-filter ()
  "Handle output from subprocess."
  (let ((inhibit-quit t)            ; Don't disturb!
        (str (buffer-substring-no-properties
              eshell-last-output-start
              eshell-last-output-end)))
    (let ((inhibit-read-only t))
      (delete-region eshell-last-output-start eshell-last-output-end))
    (let ((sync-windows (eat--synchronize-scroll-windows))
          (inhibit-read-only t))
      (eat-term-process-output eat-terminal str)
      (eat-term-redisplay eat-terminal)
      (funcall eat--synchronize-scroll-function sync-windows))
    (let ((inhibit-read-only t))
      (let ((end (eat-term-end eat-terminal)))
        (set-marker eshell-last-output-start end)
        (set-marker eshell-last-output-end end)
        (set-marker (process-mark
                     (eat-term-parameter
                      eat-terminal 'eat--output-process))
                    end))))
  (run-hooks 'eat-eshell-update-hook))

(defun eat--eshell-setup-proc-and-term (proc)
  "Setup process PROC and a new terminal for it."
  (unless eat-terminal
    (process-put proc 'adjust-window-size-function
                 #'eat--adjust-process-window-size)
    (setq eat-terminal
          (eat-term-make (current-buffer)
                         (if (marker-buffer (process-mark proc))
                             (process-mark proc)
                           (point-max))))
    (set-marker (process-mark proc) (eat-term-end eat-terminal))
    (setf (eat-term-parameter eat-terminal 'input-function)
          #'eat--send-input)
    (setf (eat-term-parameter eat-terminal 'set-cursor-function)
          #'eat--set-cursor)
    (setf (eat-term-parameter eat-terminal 'grab-mouse-function)
          #'eat--grab-mouse)
    (setf (eat-term-parameter
           eat-terminal 'manipulate-selection-function)
          #'eat--manipulate-kill-ring)
    (setf (eat-term-parameter eat-terminal 'ring-bell-function)
          #'eat--bell)
    (setf (eat-term-parameter eat-terminal 'set-cwd-function)
          #'eat--set-cwd)
    (setf (eat-term-parameter eat-terminal 'ui-command-function)
          #'eat--eshell-handle-uic)
    (eat--set-term-sixel-params)
    (setf (eat-term-parameter eat-terminal 'eat--process) proc)
    (unless (eval-when-compile (>= emacs-major-version 29))
      (setf (eat-term-parameter eat-terminal 'eat--input-process)
            proc))
    (setf (eat-term-parameter eat-terminal 'eat--output-process) proc)
    (when-let* ((window (get-buffer-window nil t)))
      (with-selected-window window
        (eat-term-resize eat-terminal (window-max-chars-per-line)
                         (floor (window-screen-lines)))))
    (eat-term-redisplay eat-terminal)
    (setq-local eshell-output-filter-functions
                '(eat--eshell-output-filter))
    (eat--eshell-process-running-mode +1)
    (eat-eshell-semi-char-mode)
    (run-hooks 'eat-eshell-exec-hook)))

(defun eat--eshell-cleanup ()
  "Cleanup everything."
  (when eat-terminal
    (let ((inhibit-read-only t))
      (cd-absolute eat--eshell-invocation-directory)
      (goto-char (eat-term-end eat-terminal))
      (unless (or (= (point) (point-min))
                  (= (char-before) ?\n))
        (insert ?\n))
      (set-marker eshell-last-output-start (point))
      (set-marker eshell-last-output-end (point))
      (eat--cursor-blink-mode -1)
      (eat--grab-mouse nil nil)
      (set-process-filter
       (eat-term-parameter
        eat-terminal 'eat--output-process)
       (if (eval-when-compile (< emacs-major-version 30))
           #'eshell-output-filter
         #'eshell-interactive-process-filter))
      (eat-term-delete eat-terminal)
      (setq eat-terminal nil)
      (kill-local-variable 'eshell-output-filter-functions)
      (eat--eshell-semi-char-mode -1)
      (eat--eshell-char-mode -1)
      (eat--eshell-process-running-mode -1)
      (setq buffer-read-only nil))
    (run-hooks 'eat-eshell-exit-hook)))

(declare-function eshell-output-filter "esh-mode" (process string))
(declare-function eshell-interactive-process-filter "esh-mode"
                  (process string))

(defun eat--eshell-process-output-queue (process buffer)
  "Process the output queue on BUFFER from PROCESS."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when eat--process-output-queue-timer
        (cancel-timer eat--process-output-queue-timer))
      (setq eat--output-queue-first-chunk-time nil)
      (while eat--pending-output-chunks
        (let ((queue eat--pending-output-chunks)
              (eat--output-queue-first-chunk-time t))
          (setq eat--pending-output-chunks nil)
          (if (eval-when-compile (< emacs-major-version 27))
              (eshell-output-filter
               process (string-join (nreverse queue)))
            (combine-change-calls
                (eat-term-beginning eat-terminal)
                (eat-term-end eat-terminal)
              ;; TODO: Is `string-join' OK or should we use a loop?
              (if (eval-when-compile (< emacs-major-version 30))
                  (eshell-output-filter
                   process (string-join (nreverse queue)))
                (eshell-interactive-process-filter
                 process (string-join (nreverse queue)))))))))))

(defun eat--eshell-filter (process string)
  "Process output STRING from PROCESS."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (when eat--process-output-queue-timer
        (cancel-timer eat--process-output-queue-timer))
      (unless eat--output-queue-first-chunk-time
        (setq eat--output-queue-first-chunk-time (current-time)))
      (push string eat--pending-output-chunks)
      (unless (eq eat--output-queue-first-chunk-time t)
        (let ((time-left
               (- eat-maximum-latency
                  (float-time
                   (time-subtract
                    nil eat--output-queue-first-chunk-time)))))
          (if (<= time-left 0)
              (eat--eshell-process-output-queue
               process (current-buffer))
            (setq eat--process-output-queue-timer
                  (run-with-timer
                   (min time-left eat-minimum-latency) nil
                   #'eat--eshell-process-output-queue process
                   (current-buffer)))))))))

(declare-function eshell-sentinel "esh-proc" (proc string))

(defun eat--eshell-sentinel (process message)
  "Process status message MESSAGE from PROCESS."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      ;; Eshell is going to write outside of the terminal, so we won't
      ;; synchronize buffer scroll here as it'll interfare with
      ;; Eshell.
      (cl-letf* ((eat--synchronize-scroll-function #'ignore)
                 (process-send-string
                  (symbol-function #'process-send-string))
                 ((symbol-function #'process-send-string)
                  (lambda (proc string)
                    (when (or (not (eq proc process))
                              (process-live-p proc))
                      (funcall process-send-string proc string)))))
        (eat--eshell-process-output-queue process (current-buffer)))
      (when (memq (process-status process) '(signal exit))
        (eat--eshell-cleanup))))
  (eshell-sentinel process message))

(declare-function eshell-search-path "esh-ext" (name))
(declare-function eshell-interactive-output-p "esh-io"
                  (&optional index handles))
(defvar eshell-current-subjob-p) ; In `esh-proc'.

;; HACK: This is a dirty hack, it can break easily.
(defun eat--eshell-adjust-make-process-args (fn command args)
  "Setup an environment to adjust `make-process' arguments.

Call FN with COMMAND and ARGS, and whenever `make-process' is called,
modify its argument to change the filter, the sentinel and invoke
`stty' from the new process."
  (if (or eshell-current-subjob-p
          (not (eshell-interactive-output-p))
          (and (not (eshell-search-path "stty"))
               (pcase eat-eshell-fallback-if-stty-not-available
                 ('nil nil)
                 ('t t)
                 ('ask (not (y-or-n-p "The program stty can't be \
found, input won't be shown if terminal emulation is enabled.  \
Disable terminal emulation? ")))
                 ((and (pred functionp) function)
                  (apply function command args)))))
      (funcall fn command args)
    (let ((hook (lambda (proc)
                  (set-process-filter proc #'eat--eshell-filter)
                  (set-process-sentinel proc #'eat--eshell-sentinel)
                  (eat--eshell-setup-proc-and-term proc))))
      (add-hook 'eshell-exec-hook hook 99)
      (unwind-protect
          (cond
           ;; Emacs 29 and above.
           ((eval-when-compile (>= emacs-major-version 29))
            (cl-letf*
                ((make-process (symbol-function #'make-process))
                 ((symbol-function #'make-process)
                  (lambda (&rest plist)
                    ;; Make sure we don't attack wrong process.
                    (if (not (equal
                              (plist-get plist :command)
                              (cons (file-local-name
                                     (expand-file-name command))
                                    args)))
                        (apply make-process plist)
                      (setf (plist-get plist :command)
                            `("/usr/bin/env" "sh" "-c"
                              ,(format "stty -nl echo rows %d columns\
 %d sane 2>%s ; if [ $1 = .. ]; then shift; fi; exec \"$@\""
                                       (floor (window-screen-lines))
                                       (window-max-chars-per-line)
                                       null-device)
                              ".." ,@(plist-get plist :command)))
                      (apply make-process plist)))))
              (funcall fn command args)))
           ;; Emacs 28.
           (t
            (cl-letf*
                ((start-file-process
                  (symbol-function #'start-file-process))
                 ((symbol-function #'start-file-process)
                  (lambda (name buffer &rest command)
                    (apply start-file-process name buffer
                           "/usr/bin/env" "sh" "-c"
                           (format "stty -nl echo rows %d columns %d \
sane 2>%s ; if [ $1 = .. ]; then shift; fi; exec \"$@\""
                                   (floor (window-screen-lines))
                                   (window-max-chars-per-line)
                                   null-device)
                           ".." command))))
              (funcall fn command args))))
        (remove-hook 'eshell-exec-hook hook)))))

(defun eat--eshell-set-input-process (&rest _)
  "Set the process that gets user input."
  (when eat-terminal
    (setf (eat-term-parameter eat-terminal 'eat--input-process)
          (eshell-head-process))))


;;;;; Minor Modes.

(defun eat--eshell-synchronize-scroll (windows)
  "Synchronize scrolling and point between terminal and WINDOWS.

WINDOWS is a list of windows.  WINDOWS may also contain the special
symbol `buffer', in which case the point of current buffer is set."
  (dolist (window windows)
    (if (eq window 'buffer)
        (goto-char (eat-term-display-cursor eat-terminal))
      (with-selected-window window
        (set-window-point nil (eat-term-display-cursor eat-terminal))
        (recenter
         (- (1+ (how-many "\n" (eat-term-display-cursor eat-terminal)
                          (eat-term-end eat-terminal)))))))))

(defun eat--eshell-update-cwd ()
  "Update the current working directory."
  (setq eat--eshell-invocation-directory default-directory))

(defvar eshell-variable-aliases-list) ; In `esh-var'.

(define-minor-mode eat--eshell-local-mode
  "Toggle Eat terminal emulation is Eshell."
  :interactive nil
  (let ((locals '(cursor-type
                  glyphless-char-display
                  scroll-margin
                  hscroll-margin
                  track-mouse
                  filter-buffer-substring-function
                  eat-terminal
                  eat--synchronize-scroll-function
                  eat--mouse-grabbing-type
                  eat--pending-output-chunks
                  eat--output-queue-first-chunk-time
                  eat--process-output-queue-timer
                  eat--eshell-invocation-directory)))
    (cond
     (eat--eshell-local-mode
      (mapc #'make-local-variable locals)
      (setq scroll-margin 0)
      (setq hscroll-margin 0)
      (setq eat--synchronize-scroll-function
            #'eat--eshell-synchronize-scroll)
      (setq filter-buffer-substring-function
            #'eat--filter-buffer-substring)
      (make-local-variable 'eshell-variable-aliases-list)
      (setq eshell-variable-aliases-list
            `(("TERM" eat--eshell-term-name t)
              ("TERMINFO" eat-term-terminfo-directory t)
              ("INSIDE_EMACS" eat-term-inside-emacs t)
              ("EAT_SHELL_INTEGRATION_DIR"
               eat-term-shell-integration-directory t)
              ,@eshell-variable-aliases-list))
      ;; Make sure glyphless character don't display a huge box glyph,
      ;; that would break the display.
      (eat--setup-glyphless-chars)
      (eat--eshell-update-cwd)
      (when eat-enable-blinking-text
        (eat-blink-mode +1)))
     (t
      (when eat-enable-blinking-text
        (eat-blink-mode -1))
      (mapc #'kill-local-variable locals)
      (setq eshell-variable-aliases-list
            (cl-delete-if
             (lambda (elem)
               (member elem
                       '(("TERM" eat--eshell-term-name t)
                         ("TERMINFO" eat-term-terminfo-directory t)
                         ("INSIDE_EMACS" eat-term-inside-emacs t)
                         ("EAT_SHELL_INTEGRATION_DIR"
                          eat-term-shell-integration-directory t))))
             eshell-variable-aliases-list))))))

(declare-function eshell-gather-process-output "esh-proc"
                  (command args))
(defvar eshell-last-async-proc) ; In `esh-cmd'.
(defvar eshell-last-async-procs) ; In `esh-cmd'.

;;;###autoload
(define-minor-mode eat-eshell-mode
  "Toggle Eat terminal emulation in Eshell."
  :global t
  :lighter (eat--eshell-local-mode
            (" Eat-Eshell"
             (:eval
              (when eat-terminal
                (cond
                 (eat--eshell-semi-char-mode
                  `("["
                    (:propertize
                     "semi-char"
                     help-echo
                     ,(concat "mouse-1: Switch to char mode, "
                              "mouse-3: Switch to emacs mode")
                     mouse-face mode-line-highlight
                     local-map
                     (keymap
                      (mode-line
                       . (keymap
                          (down-mouse-1 . eat-eshell-char-mode)
                          (down-mouse-3 . eat-eshell-emacs-mode)))))
                    "]"))
                 (eat--eshell-char-mode
                  '("["
                    (:propertize
                     "char"
                     help-echo
                     ,(concat "mouse-1: Switch to semi-char mode, "
                              "mouse-3: Switch to emacs mode")
                     mouse-face mode-line-highlight
                     local-map
                     (keymap
                      (mode-line
                       . (keymap
                          (down-mouse-1 . eat-eshell-semi-char-mode)
                          (down-mouse-3 . eat-eshell-emacs-mode)))))
                    "]"))
                 (t
                  `("["
                    (:propertize
                     "emacs"
                     help-echo
                     ,(concat "mouse-1: Switch to semi-char mode, "
                              "mouse-3: Switch to char mode")
                     mouse-face mode-line-highlight
                     local-map
                     (keymap
                      (mode-line
                       . (keymap
                          (down-mouse-1 . eat-eshell-semi-char-mode)
                          (down-mouse-3 . eat-eshell-char-mode)))))
                    "]")))))))
  :group 'eat-eshell
  (cond
   (eat-eshell-mode
    (let ((buffers nil))
      (setq eat-eshell-mode nil)
      (require 'esh-mode)
      (require 'esh-proc)
      (require 'esh-var)
      (require 'esh-cmd)
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (eq major-mode #'eshell-mode)
            (when (if (eval-when-compile (< emacs-major-version 29))
                      (bound-and-true-p eshell-last-async-proc)
                    (bound-and-true-p eshell-last-async-procs))
              (user-error
               (concat "Can't toggle Eat Eshell mode while"
                       " any Eshell process is running")))
            (push buffer buffers))))
      (setq eat-eshell-mode t)
      (dolist (buffer buffers)
        (with-current-buffer buffer
          (eat--eshell-local-mode +1))))
    (add-hook 'eshell-mode-hook #'eat--eshell-local-mode)
    (add-hook 'eshell-directory-change-hook #'eat--eshell-update-cwd)
    (advice-add #'eshell-gather-process-output :around
                #'eat--eshell-adjust-make-process-args)
    (when (eval-when-compile (>= emacs-major-version 29))
      (advice-add #'eshell-resume-eval :after
                  #'eat--eshell-set-input-process)))
   (t
    (let ((buffers nil))
      (setq eat-eshell-mode t)
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (and (eq major-mode #'eshell-mode)
                     eat--eshell-local-mode)
            (when (if (eval-when-compile (< emacs-major-version 29))
                      (bound-and-true-p eshell-last-async-proc)
                    (bound-and-true-p eshell-last-async-procs))
              (user-error
               (concat "Can't toggle Eat Eshell mode while"
                       " any Eshell process is running")))
            (push buffer buffers))))
      (setq eat-eshell-mode nil)
      (dolist (buffer buffers)
        (with-current-buffer buffer
          (eat--eshell-local-mode -1))))
    (remove-hook 'eshell-mode-hook #'eat--eshell-local-mode)
    (remove-hook 'eshell-directory-change-hook
                 #'eat--eshell-update-cwd)
    (advice-remove #'eshell-gather-process-output
                   #'eat--eshell-adjust-make-process-args)
    (when (eval-when-compile (>= emacs-major-version 29))
      (advice-remove #'eshell-resume-eval
                     #'eat--eshell-set-input-process)))))


;;;; Eshell Visual Command Handling.

(defvar eshell-destroy-buffer-when-process-dies) ; In `em-term'.

;; Adapted from `em-term'.
(defun eat--eshell-visual-sentinel (proc _msg)
  "Clean up the buffer visiting PROC.

If `eshell-destroy-buffer-when-process-dies' is non-nil, destroy
the buffer.

MSG describes PROC's status."
  (when eshell-destroy-buffer-when-process-dies
    (let ((proc-buf (process-buffer proc)))
      (when (and proc-buf (buffer-live-p proc-buf)
                 (not (eq 'run (process-status proc)))
                 (= (process-exit-status proc) 0))
        (if (eq (current-buffer) proc-buf)
            (when-let* ((buf (and (boundp 'eshell-parent-buffer)
                                  (buffer-live-p eshell-parent-buffer)
                                  eshell-parent-buffer)))
              (switch-to-buffer buf)))
        (kill-buffer proc-buf)))))

(defvar eshell-interpreter-alist) ; In `esh-ext'.
(declare-function eshell-find-interpreter "esh-ext"
                  (file args &optional no-examine-p))
(declare-function eshell-stringify-list "esh-util" (args))

(defun eat--eshell-exec-visual (&rest args)
  "Run the specified PROGRAM in a terminal emulation buffer.

ARGS are passed to the program.  At the moment, no piping of input is
allowed."
  (require 'esh-ext)
  (require 'esh-util)
  (let* ((eshell-interpreter-alist nil)
         (interp (eshell-find-interpreter (car args) (cdr args)))
         (program (car interp))
         (args (flatten-tree
                (eshell-stringify-list (append (cdr interp)
                                               (cdr args)))))
         (eat-buf
          (generate-new-buffer
           (concat "*" (file-name-nondirectory program) "*")))
         (eshell-buf (current-buffer)))
    (with-current-buffer eat-buf
      (switch-to-buffer eat-buf)
      (eat-mode)
      (setq-local eshell-parent-buffer eshell-buf)
      (setq-local eat-kill-buffer-on-exit nil)
      (eat-exec eat-buf program program nil args)
      (let ((proc (get-buffer-process eat-buf)))
        (if (and proc (eq 'run (process-status proc)))
            (let ((sentinel (process-sentinel proc)))
              (add-function  :after (var sentinel)
                             #'eat--eshell-visual-sentinel)
              (set-process-sentinel proc sentinel))
          (error "Failed to invoke visual command")))
      (eat-semi-char-mode)))
  nil)

(declare-function eshell-exec-visual "em-term" (&rest args))

;;;###autoload
(define-minor-mode eat-eshell-visual-command-mode
  "Toggle running Eshell visual commands with Eat."
  :group 'eat-eshell
  :global t
  (if eat-eshell-visual-command-mode
      (advice-add #'eshell-exec-visual :override
                  #'eat--eshell-exec-visual)
    (advice-remove #'eshell-exec-visual #'eat--eshell-exec-visual)))


;;;; Project Integration.

(declare-function project-root "project" (project))
(declare-function project-prefixed-buffer-name "project" (mode))

;;;###autoload
(defun eat-project (&optional arg)
  "Start Eat in the current project's root directory.

Start a new Eat session, or switch to an already active session.
Return the buffer selected (or created).

With a non-numeric prefix ARG, create a new session.

With a numeric prefix ARG (like
\\[universal-argument] 42 \\[eat-project]), switch to the session with
that number, or create it if it doesn't already exist."
  (interactive "P")
  (require 'project)
  (let* ((default-directory (project-root (project-current t)))
         (eat-buffer-name (project-prefixed-buffer-name "eat")))
    (eat nil arg)))

;;;###autoload
(defun eat-project-other-window (&optional arg)
  "Start Eat in the current project root directory in another window.

Start a new Eat session, or switch to an already active session.
Return the buffer selected (or created).

With a non-numeric prefix ARG, create a new session.

With a numeric prefix ARG (like
\\[universal-argument] 42 \\[eat-project]), switch to the session with
that number, or create it if it doesn't already exist."
  (interactive "P")
  (require 'project)
  (let* ((default-directory (project-root (project-current t)))
         (eat-buffer-name (project-prefixed-buffer-name "eat")))
    (eat-other-window nil arg)))


;;;; Tracing.

;;;;; Recording Trace Data.

(defconst eat--trace-recorded-variables
  '(eat-term-scrollback-size
    eat-enable-alternative-display)
  "The variable to record in trace output.")

(defvar eat--trace-output-buffer nil
  "Buffer where the trace data is written to.")

(defun eat--trace-log (time operation &rest args)
  "Log TIME, OPERATION and ARGS into trace output.

TIME defaults to the current time.

The current buffer should be the trace output buffer.  Move the point
to the end of (accessible portion of) buffer."
  (goto-char (point-max))
  ;; Hope that `float-time' won't roll over while tracing.  ;-)
  (insert (replace-regexp-in-string
           (rx (any (0 . 31)))
           (lambda (string) (format "\\\\x%02x" (aref string 0)))
           (format "%S" `(,(float-time time) ,operation ,@args)))
          ?\n))

(defun eat--trace-stop ()
  "Stop tracing the terminal in current buffer."
  (when eat--trace-output-buffer
    (with-current-buffer eat--trace-output-buffer
      (eat--trace-log nil 'finish)))
  (remove-hook 'kill-buffer-hook #'eat--trace-stop t)
  (kill-local-variable 'eat--trace-output-buffer))

(defun eat--trace-exec (fn buffer name command startfile switches)
  "Trace `eat-exec'.

BUFFER is the buffer and COMMAND and SWITCHES are the invocation
command.  BUFFER, NAME, COMMAND, STARTFILE and SWITCHES are passed to
FN, `eat-exec', which see."
  (let ((time (current-time)))
    (prog1
        (funcall fn buffer name command startfile switches)
      (let ((buf (generate-new-buffer
                  (format "*eat-trace %s*: %s"
                          (buffer-name buffer)
                          (mapconcat #'shell-quote-argument
                                     (cons command switches) " "))))
            (width nil)
            (height nil)
            (variables nil))
        (with-current-buffer buffer
          (setq-local eat--trace-output-buffer buf)
          (add-hook 'kill-buffer-hook #'eat--trace-stop nil t)
          (let ((size (eat-term-size eat-terminal)))
            (setq width (car size))
            (setq height (cdr size)))
          (dolist (var eat--trace-recorded-variables)
            (push (cons var (symbol-value var)) variables)))
        (with-current-buffer buf
          (when (fboundp 'lisp-data-mode) (lisp-data-mode))
          (insert ";; -*- mode: lisp-data -*-\n")
          (eat--trace-log time 'create 'eat width height
                          variables))))))

(defun eat--trace-process-output-queue (fn buffer)
  "Trace `eat--process-output-queue'.

BUFFER is passed to FN, `eat--process-output-queue', which see."
  (if (or (not (buffer-live-p buffer))
          (not (buffer-local-value 'eat--trace-output-buffer buffer)))
      (funcall fn buffer)
    (cl-letf* ((eat-term-process-output
                (symbol-function #'eat-term-process-output))
               ((symbol-function #'eat-term-process-output)
                (lambda (terminal output)
                  (when (buffer-live-p eat--trace-output-buffer)
                    (with-current-buffer eat--trace-output-buffer
                      (eat--trace-log nil 'output output)))
                  (funcall eat-term-process-output terminal output)))
               (eat-term-redisplay
                (symbol-function #'eat-term-redisplay))
               ((symbol-function #'eat-term-redisplay)
                (lambda (terminal)
                  (when (buffer-live-p eat--trace-output-buffer)
                    (with-current-buffer eat--trace-output-buffer
                      (eat--trace-log nil 'redisplay)))
                  (funcall eat-term-redisplay terminal))))
      (funcall fn buffer))))

(defun eat--trace-adjust-process-window-size (fn process windows)
  "Trace `eat--adjust-process-window-size'.

PROCESS and WINDOWS are passed to FN,
`eat--adjust-process-window-size', which see."
  (cl-letf*
      ((eat-term-resize (symbol-function #'eat-term-resize))
       ((symbol-function #'eat-term-resize)
        (lambda (terminal width height)
          (when (buffer-live-p eat--trace-output-buffer)
            (with-current-buffer eat--trace-output-buffer
              (eat--trace-log nil 'resize width height)))
          (funcall eat-term-resize terminal width height)))
       (eat-term-redisplay (symbol-function #'eat-term-redisplay))
       ((symbol-function #'eat-term-redisplay)
        (lambda (terminal)
          (when (buffer-live-p eat--trace-output-buffer)
            (with-current-buffer eat--trace-output-buffer
              (eat--trace-log nil 'redisplay)))
          (funcall eat-term-redisplay terminal))))
    (funcall fn process windows)))

(defun eat--trace-sentinel (fn &rest args)
  "Trace `eat--sentinel'.

Elements of ARGS are passed to FN, `eat--sentinel', which see."
  (cl-letf* ((eat-term-delete (symbol-function #'eat-term-delete))
             ((symbol-function #'eat-term-delete)
              (lambda (terminal)
                (when (buffer-live-p eat--trace-output-buffer)
                  (eat--trace-stop))
                (funcall eat-term-delete terminal))))
    (apply fn args)))

(defun eat--trace-reset (fn)
  "Trace `eat-reset'.

FN is original definition of `eat-reset'."
  (cl-letf*
      ((eat-term-reset (symbol-function #'eat-term-reset))
       ((symbol-function #'eat-term-reset)
        (lambda (terminal)
          (when (buffer-live-p eat--trace-output-buffer)
            (with-current-buffer eat--trace-output-buffer
              (eat--trace-log nil 'reset)))
          (funcall eat-term-reset terminal)))
       (eat-term-redisplay (symbol-function #'eat-term-redisplay))
       ((symbol-function #'eat-term-redisplay)
        (lambda (terminal)
          (when (buffer-live-p eat--trace-output-buffer)
            (with-current-buffer eat--trace-output-buffer
              (eat--trace-log nil 'redisplay)))
          (funcall eat-term-redisplay terminal))))
    (funcall fn)))

(defun eat--trace-eshell-adjust-make-process-args (fn &rest args)
  "Trace `eat--eshell-adjust-make-process-args'.

ARGS is passed to FN, `eat--eshell-adjust-make-process-args', which
see."
  (cl-letf*
      ((command nil)
       (make-process (symbol-function #'make-process))
       ((symbol-function #'make-process)
        (lambda (&rest plist)
          (prog1
              (apply make-process plist)
            (setq command (nthcdr 5 (plist-get plist :command))))))
       (eat--eshell-setup-proc-and-term
        (symbol-function #'eat--eshell-setup-proc-and-term))
       ((symbol-function #'eat--eshell-setup-proc-and-term)
        (lambda (proc)
          (let ((time (current-time)))
            (prog1
                (funcall eat--eshell-setup-proc-and-term proc)
              (when (eq (eat-term-parameter
                         eat-terminal 'eat--output-process)
                        proc)
                (let ((buf (generate-new-buffer
                            (format "*eat-trace %s*: %s"
                                    (buffer-name)
                                    (mapconcat
                                     #'shell-quote-argument
                                     command " "))))
                      (width nil)
                      (height nil)
                      (variables nil))
                  (setq-local eat--trace-output-buffer buf)
                  (add-hook 'kill-buffer-hook #'eat--trace-stop nil t)
                  (let ((size (eat-term-size eat-terminal)))
                    (setq width (car size))
                    (setq height (cdr size)))
                  (dolist (var eat--trace-recorded-variables)
                    (push (cons var (symbol-value var)) variables))
                  (with-current-buffer buf
                    (when (fboundp 'lisp-data-mode) (lisp-data-mode))
                    (insert ";; -*- lisp-data -*-\n")
                    (eat--trace-log time 'create 'eshell width height
                                    variables)))))))))
    (apply fn args)))

(defun eat--trace-eshell-output-filter (fn)
  "Trace `eat--eshell-output-filter'.

FN is the original definition of `eat--eshell-output-filter', which
see."
  (if (not (buffer-live-p eat--trace-output-buffer))
      (funcall fn)
    (cl-letf* ((eat-term-process-output
                (symbol-function #'eat-term-process-output))
               ((symbol-function #'eat-term-process-output)
                (lambda (terminal output)
                  (with-current-buffer eat--trace-output-buffer
                    (eat--trace-log nil 'output output))
                  (funcall eat-term-process-output terminal output)))
               (eat-term-redisplay
                (symbol-function #'eat-term-redisplay))
               ((symbol-function #'eat-term-redisplay)
                (lambda (terminal)
                  (with-current-buffer eat--trace-output-buffer
                    (eat--trace-log nil 'redisplay))
                  (funcall eat-term-redisplay terminal))))
      (funcall fn))))

(defun eat--trace-eshell-cleanup (fn)
  "Trace `eat--eshell-cleanup'.

FN is the original definition of `eat--eshell-cleanup', which see."
  (if (not (buffer-live-p eat--trace-output-buffer))
      (funcall fn)
    (cl-letf* ((eat-term-delete (symbol-function #'eat-term-delete))
               ((symbol-function #'eat-term-delete)
                (lambda (terminal)
                  (eat--trace-stop)
                  (funcall eat-term-delete terminal))))
      (funcall fn))))

(define-minor-mode eat-trace-mode
  "Toggle tracing Eat terminal."
  :global t
  :require 'eat
  :lighter " Eat-Trace"
  (if eat-trace-mode
      (progn
        (advice-add #'eat-exec :around #'eat--trace-exec)
        (advice-add #'eat--process-output-queue :around
                    #'eat--trace-process-output-queue)
        (advice-add #'eat--adjust-process-window-size :around
                    #'eat--trace-adjust-process-window-size)
        (advice-add #'eat--sentinel :around #'eat--trace-sentinel)
        (advice-add #'eat-reset :around #'eat--trace-reset)
        (advice-add #'eat--eshell-adjust-make-process-args :around
                    #'eat--trace-eshell-adjust-make-process-args)
        (advice-add #'eat--eshell-output-filter :around
                    #'eat--trace-eshell-output-filter)
        (advice-add #'eat--eshell-cleanup :around
                    #'eat--trace-eshell-cleanup))
    (advice-remove #'eat-exec #'eat--trace-exec)
    (advice-remove #'eat--process-output-queue
                   #'eat--trace-process-output-queue)
    (advice-remove #'eat--adjust-process-window-size
                   #'eat--trace-adjust-process-window-size)
    (advice-remove #'eat--sentinel #'eat--trace-sentinel)
    (advice-remove #'eat-reset #'eat--trace-reset)
    (advice-remove #'eat--eshell-adjust-make-process-args
                   #'eat--trace-eshell-adjust-make-process-args)
    (advice-remove #'eat--eshell-output-filter
                   #'eat--trace-eshell-output-filter)
    (advice-remove #'eat--eshell-cleanup
                   #'eat--trace-eshell-cleanup)
    (dolist (buffer (buffer-list))
      (when (buffer-local-value 'eat--trace-output-buffer buffer)
        (with-current-buffer buffer
          (setq-local eat--trace-output-buffer nil))))))


;;;;; Trace Data Replay.

(defvar eat--trace-replay-buffer nil
  "The buffer replaying the trace data in current buffer.")

(defvar eat--trace-replay-marker nil
  "The point from where to read the next sexp.")

(defvar eat--trace-replay-current-sexp-overlay nil
  "Overlay indicating the current sexp.")

(defvar eat--trace-replay-source-buffer nil
  "The source buffer containing the trace output.")

(defvar eat--trace-replay-recording-start-time 0.0
  "Time when recording was started.")

(defvar eat--trace-replay-frame-count 0
  "The number of the frames in the trace output.")

(defvar eat--trace-replay-progress-frame 0
  "The number of the frames before the current position.")

(defvar eat--trace-replay-progress nil
  "The number of seconds of trace output was shown.")

(defun eat--trace-replay-eval (data)
  "Evalulate DATA as trace output."
  (let ((inhibit-read-only t))
    (setq eat--trace-replay-progress
          (- (car data) eat--trace-replay-recording-start-time))
    (pcase-exhaustive data
      (`(,time create ,_ui ,width ,height ,variables)
       (setq eat--trace-replay-recording-start-time time)
       (setq eat--trace-replay-progress 0)
       (dolist (var eat--trace-recorded-variables)
         (set (make-local-variable var) (alist-get var variables)))
       (setq eat-terminal (eat-term-make (current-buffer) (point)))
       (setf (eat-term-parameter eat-terminal 'set-cursor-function)
             #'eat--set-cursor)
       (setf (eat-term-parameter eat-terminal 'ring-bell-function)
             #'eat--bell)
       (eat-term-resize eat-terminal width height)
       (eat-term-redisplay eat-terminal))
      (`(,_time output ,string)
       (eat-term-process-output eat-terminal string))
      (`(,_time redisplay)
       (eat-term-redisplay eat-terminal))
      (`(,_time resize ,width ,height)
       (eat-term-resize eat-terminal width height))
      (`(,_time reset)
       (eat-term-reset eat-terminal))
      (`(,_time finish)
       (eat-term-delete eat-terminal)))
    (eat--synchronize-scroll (get-buffer-window-list))))

(defun eat--trace-replay-eval-next ()
  "Evaluate next sexp in trace output."
  (with-current-buffer eat--trace-replay-source-buffer
    (goto-char eat--trace-replay-marker)
    (ignore-error end-of-file
      (let ((data (read (current-buffer))))
        (set-marker eat--trace-replay-marker (point))
        (backward-list)
        (move-overlay eat--trace-replay-current-sexp-overlay
                      (point) (point))
        (when-let* ((window (get-buffer-window)))
          (set-window-point window (point)))
        (with-current-buffer eat--trace-replay-buffer
          (cl-incf eat--trace-replay-progress-frame)
          (eat--trace-replay-eval data))))))

(defun eat-trace-replay ()
  "Replay terminal according to trace output in current buffer."
  (interactive)
  (unless (buffer-live-p eat--trace-replay-buffer)
    (setq-local eat--trace-replay-buffer
                (generate-new-buffer
                 (format "*eat-trace-replay*: %s" (buffer-name))))
    (setq-local eat--trace-replay-marker (point-min-marker))
    (let ((ov (make-overlay (point-min) (point-min))))
      (overlay-put ov 'before-string
                   #(" " 0 1 (display (left-fringe right-triangle))))
      (setq-local eat--trace-replay-current-sexp-overlay ov))
    (goto-char (point-min))
    (let ((source (current-buffer))
          (frame-count 0))
      (ignore-error end-of-file
        (while (read (current-buffer))
          (cl-incf frame-count)))
      (goto-char (point-min))
      (with-current-buffer eat--trace-replay-buffer
        (eat-trace-replay-mode)
        (setq eat--trace-replay-source-buffer source)
        (setq eat--trace-replay-frame-count frame-count))))
  (display-buffer eat--trace-replay-buffer))

(defun eat-trace-replay-next-frame (&optional n)
  "Show the Nth next frame.

N defaults to 1.  Interactively, N is the prefix argument."
  (interactive "p")
  (dotimes (_ n)
    (eat--trace-replay-eval-next)))

(defun eat-trace--cleanup ()
  "Clean up the source buffer before the terminal being killed."
  (when (buffer-live-p eat--trace-replay-source-buffer)
    (with-current-buffer eat--trace-replay-source-buffer
      (setq eat--trace-replay-buffer nil)
      (setq eat--trace-replay-marker nil)
      (delete-overlay eat--trace-replay-current-sexp-overlay))))

(defvar eat-trace-replay-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'eat-trace-replay-next-frame)
    (define-key map (kbd "<down>") #'eat-trace-replay-next-frame)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for Eat-Trace-Replay mode.")

(define-derived-mode eat-trace-replay-mode special-mode
  "Eat-Trace-Replay"
  "Major mode for replaying terminal according to trace output."
  (mapc #'make-local-variable '(eat-terminal
                                eat--trace-replay-source-buffer
                                eat--trace-replay-recording-start-time
                                eat--trace-replay-progress
                                eat--trace-replay-frame-count
                                eat--trace-replay-progress-frame))
  (setq-local
   mode-line-process
   '("[" (:eval (number-to-string eat--trace-replay-progress-frame))
     "/" (:eval (number-to-string eat--trace-replay-frame-count))
     "]"))
  (add-hook 'kill-buffer-hook #'eat-trace--cleanup nil t))


;;;; Miscellaneous.

(defun eat-compile-terminfo ()
  "Compile terminfo databases of Eat."
  (interactive)
  ;; Check for required files and programs.
  (let ((source-path (expand-file-name "eat.ti" eat--install-path))
        (tic-path (executable-find "tic")))
    (unless (file-exists-p source-path)
      (error "Eat not installed properly: %s"
             "Terminfo source file not found"))
    (unless tic-path
      (error "Terminfo compiler `tic' not found"))
    (message "Compiling terminfo databases...")
    ;; Compile.
    (let* ((command (format "env TERMINFO=\"%s\" %s -x %s"
                            eat-term-terminfo-directory tic-path
                            source-path))
           (status
            (with-temp-buffer
              (make-directory eat-term-terminfo-directory 'parents)
              (let ((proc (start-process-shell-command
                           "eat-terminfo-compile"
                           (current-buffer) command)))
                (while (process-live-p proc)
                  (sleep-for 0.02))
                (process-exit-status proc)))))
      (if (= status 0)
          (message "Compiling terminfo databases...done")
        (message "Compiling terminfo databases...error")
        (error "Command `%s' exited with non-zero exit code %i"
               command status)))))


;;;; Footer.

(defun eat-reload ()
  "Reload Eat."
  (interactive)
  (unless eat--being-loaded
    ;; Remove .elc suffix to load native compiled version if possible.
    (load (string-remove-suffix ".elc" eat--load-file-path))))

(setq eat--being-loaded nil)

(provide 'eat)
;;; eat.el ends here
