;;; corfu-terminal.el --- Corfu popup on terminal -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Akib Azmain Turja.

;; Author: Akib Azmain Turja <akib@disroot.org>
;; Created: 2022-04-11
;; Version: 0.7
;; Package-Requires: ((emacs "26.1") (corfu "0.36") (popon "0.13"))
;; Keywords: convenience
;; Homepage: https://codeberg.org/akib/emacs-corfu-terminal

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

;; Corfu uses child frames to display candidates.  This makes Corfu
;; unusable on terminal.  This package replaces that with popup/popon,
;; which works everywhere.  Use M-x corfu-terminal-mode to enable.
;; You'll probably want to enable it only on terminal.  In that case,
;; put the following in your init file:

;;   (unless (display-graphic-p)
;;     (corfu-terminal-mode +1))

;;; Code:

(require 'subr-x)
(require 'corfu)
(require 'popon)
(require 'cl-lib)

(defgroup corfu-terminal nil
  "Corfu popup on terminal."
  :group 'convenience
  :link '(url-link "https://codeberg.org/akib/emacs-corfu-terminal")
  :prefix "corfu-terminal-")

(defcustom corfu-terminal-enable-on-minibuffer t
  "Non-nil means enable corfu-terminal on minibuffer."
  :type 'boolean)

(defcustom corfu-terminal-resize-minibuffer t
  "Non-nil means resize minibuffer to show popup."
  :type 'boolean)

(defcustom corfu-terminal-position-right-margin 0
  "Number of columns of margin at the right of window.

Always keep the popup this many columns away from the right edge of
the window.

Note: If the popup breaks or crosses the right edge of window, you may
set this variable to warkaround it.  But remember, that's a *bug*, so
if that ever happens to you please report the issue at
https://codeberg.org/akib/emacs-corfu-terminal/issues."
  :type 'integer)

(defcustom corfu-terminal-disable-on-gui t
  "Don't use popon UI on GUI."
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil)))

(defvar corfu-terminal--popon nil
  "Popon object.")

(defvar corfu-terminal--last-position nil
  "Position of last popon, and some data to make sure that's valid.")

(cl-defmethod corfu--popup-support-p (&context (corfu-terminal-mode
                                                (eql t)))
  "Return whether corfu-terminal supports showing popon now."
  (or (not (minibufferp))
      corfu-terminal-enable-on-minibuffer
      (and corfu-terminal-disable-on-gui
           (display-graphic-p))))

(cl-defmethod corfu--popup-hide (&context (corfu-terminal-mode
                                           (eql t)))
  "Hide popup.

If `corfu-terminal-disable-on-gui' is non-nil and  `display-graphic-p'
returns non-nil then call FN instead, where FN should be the original
definition in Corfu."
  (if (and corfu-terminal-disable-on-gui
           (display-graphic-p))
      (cl-call-next-method)
    (when corfu-terminal--popon
      (setq corfu-terminal--popon
            (popon-kill corfu-terminal--popon)))))

(cl-defmethod corfu--popup-show ( pos off width lines
                                  &context (corfu-terminal-mode
                                            (eql t))
                                  &optional curr lo bar)
  "Show popup at OFF columns before POS.

Show LINES, a list of lines.  Highlight CURRth line as current
selection.  Show a vertical scroll bar of size BAR + 1 from LOth line.

If `corfu-terminal-disable-on-gui' is non-nil and  `display-graphic-p'
returns non-nil then call FN instead, where FN should be the original
definition in Corfu."
  (if (and corfu-terminal-disable-on-gui
           (display-graphic-p))
      (cl-call-next-method)
    (corfu--popup-hide) ; Hide the popup first.
    (when (and (window-minibuffer-p)
               (< (/ (window-body-height nil 'pixelwise)
                     (default-font-height))
                  (1+ (length lines)))
               corfu-terminal-resize-minibuffer
               (not (frame-root-window-p (selected-window))))
      (window-resize nil (- (1+ (length lines))
                            (/ (window-body-height nil 'pixelwise)
                               (default-font-height)))))
    (let* ((bar-width (ceiling (* (default-font-width)
                                  corfu-bar-width)))
           (margin-left-width (ceiling (* (default-font-width)
                                          corfu-left-margin-width)))
           (margin-right-width (max (ceiling
                                     (* (default-font-width)
                                        corfu-right-margin-width))
                                    bar-width))
           (scroll-bar
            (when (< 0 bar-width)
              (if (display-graphic-p)
                  (concat
                   (propertize
                    " " 'display
                    `(space
                      :width (,(- margin-right-width bar-width))))
                   (propertize " " 'display
                               `(space :width (,bar-width))
                               'face 'corfu-bar))
                (concat
                 (make-string (- margin-right-width bar-width) ?\ )
                 (propertize (make-string bar-width ?\ ) 'face
                             'corfu-bar)))))
           (margin-left
            (when (> margin-left-width 0)
              (if (display-graphic-p)
                  (propertize
                   " " 'display `(space :width (,margin-left-width)))
                (make-string margin-left-width ?\ ))))
           (margin-right
            (when (> margin-right-width 0)
              (if (display-graphic-p)
                  (propertize
                   " " 'display `(space :width (,margin-right-width)))
                (make-string margin-right-width ?\ ))))
           (popon-width
            (if (display-graphic-p)
                (+ width (round (/ (+ margin-left-width
                                      margin-right-width)
                                   (default-font-width))))
              (+ width margin-left-width margin-right-width)))
           (popon-pos
            (if (equal (cdr corfu-terminal--last-position)
                       (list (posn-point pos) popon-width
                             (window-start) (buffer-modified-tick)))
                (car corfu-terminal--last-position)
              (let ((x-y (popon-x-y-at-posn pos)))
                (cons
                 (max
                  (min (- (car x-y) (+ off margin-left-width))
                       (- (window-max-chars-per-line)
                          corfu-terminal-position-right-margin
                          popon-width))
                  0)
                 (if (and (< (/ (window-body-height nil 'pixelwise)
                                (default-font-height))
                             (+ (1+ (cdr x-y)) (length lines)))
                          (>= (cdr x-y) (length lines)))
                     (- (cdr x-y) (length lines))
                   (1+ (cdr x-y))))))))
      (setq corfu-terminal--last-position
            (list popon-pos (posn-point pos) popon-width
                  (window-start) (buffer-modified-tick)))
      (setq corfu-terminal--popon
            (popon-create
             (cons
              (string-join
               (seq-map-indexed
                (lambda (line line-number)
                  (let ((str
                         (concat
                          margin-left line
                          (make-string (- width (string-width line))
                                       ?\ )
                          (if (and lo (<= lo line-number (+ lo bar)))
                              scroll-bar
                            margin-right))))
                    (add-face-text-property 0 (length str)
                                            (if (eq line-number curr)
                                                'corfu-current
                                              'corfu-default)
                                            t str)
                    str))
                lines)
               "\n")
              popon-width)
             popon-pos))
      nil)))

;;;###autoload
(define-minor-mode corfu-terminal-mode
  "Corfu popup on terminal."
  :global t
  :group 'corfu-terminal)

(provide 'corfu-terminal)
;;; corfu-terminal.el ends here
