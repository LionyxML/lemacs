;;; popon.el --- "Pop" floating text "on" a window -*- lexical-binding: t -*-

;; Copyright (C) 2022 Akib Azmain Turja.

;; Author: Akib Azmain Turja <akib@disroot.org>
;; Created: 2022-04-11
;; Version: 0.13
;; Package-Requires: ((emacs "25.1"))
;; Keywords: lisp extensions frames
;; Homepage: https://codeberg.org/akib/emacs-popon

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

;; Popon allows you to pop text on a window, what we call a popon.
;; Popons are window-local and sticky, they don't move while
;; scrolling, and they even don't go away when switching buffer, but
;; you can bind a popon to a specific buffer to only show on that
;; buffer.

;; If some popons are annoying you and you can't kill them, do M-x
;; popon-kill-all to kill all popons.

;;; Code:

(require 'subr-x)
(require 'cl-lib)

(defun popon--render-lines (framebuffer x y lines width)
  "Place LINES on top of FRAMEBUFFER.

Place LINES on top of text at line X and column Y on FRAMEBUFFER and
return FRAMEBUFFER.  LINES is a list of list as string.  FRAMEBUFFER
is a list, each element is of form: (LINE MODIFIED EXTENDED
OTHERS...), where LINE is as string, MODIFIED is t when LINE is
modified and EXTENDED is t when the line has been extended.  OTHERS is
not modified in any way.  Each line in LINES is assumed to occupy
WIDTH character.  FRAMEBUFFER and LINES shouldn\\='t contain newlines.
Example:

\(`popon--render-lines'
 \\='((\"GNU Emacs is “free software”; this means\" nil nil)
   (\"that everyone is free to use it and free\" nil nil)
   (\"to redistribute it under certain\"         nil nil)
   (\"conditions.  GNU Emacs is not in the\"     nil nil)
   (\"public domain; it is copyrighted and\"     nil nil)
   (\"there are restrictions on its\"            nil nil)
   (\"distribution, but these restrictions are\" nil nil)
   (\"designed to permit everything that a\"     nil nil foo)
   (\"good cooperating citizen would want to\"   nil nil bar)
   (\"do.  What is not allowed is to try to\"    nil nil)
   (\"prevent others from further sharing any\"  nil nil)
   (\"version of GNU Emacs that they might get\" nil nil)
   (\"from you.  The precise conditions are\"    nil nil)
   (\"found in the GNU General Public License\"  nil nil)
   (\"that comes with Emacs and also appears\"   nil nil)
   (\"in this manual(1).  See Copying.\"         t nil))
 11 1 \\='(\"+--^^^^^^^^^^^^^-------------------------+\"
        \"|Free software is a type of software that|\"
        \"|respects user freedom.  Think free as in|\"
        \"|free speech, not as in free beer.       |\"
        \"+----------------------------------------+\")
 42)
=> ((\"GNU Emacs is “free software”; this means\"              nil nil)
    (\"that everyo+--^^^^^^^^^^^^^-------------------------+\" t t)
    (\"to redistri|Free software is a type of software that|\" t t)
    (\"conditions.|respects user freedom.  Think free as in|\" t t)
    (\"public doma|free speech, not as in free beer.       |\" t t)
    (\"there are r+----------------------------------------+\" t t)
    (\"distribution, but these restrictions are\"              nil nil)
    (\"designed to permit everything that a\"                  nil nil foo)
    (\"good cooperating citizen would want to\"                nil nil bar)
    (\"do.  What is not allowed is to try to\"                 nil nil)
    (\"prevent others from further sharing any\"               nil nil)
    (\"version of GNU Emacs that they might get\"              nil nil)
    (\"from you.  The precise conditions are\"                 nil nil)
    (\"found in the GNU General Public License\"               nil nil)
    (\"that comes with Emacs and also appears\"                nil nil)
    (\"in this manual(1).  See Copying.\"                      t nil))"
  (let ((tab-size tab-width)
        ;; The text might have `read-only' property.
        (inhibit-read-only t))
    (with-temp-buffer
      (setq-local tab-width tab-size) ; Preseve tab width.
      (dotimes (i (length lines))
        (when (< (+ y i) (length framebuffer))
          (erase-buffer)
          (insert (car (nth (+ y i) framebuffer)))
          (goto-char (point-min))
          (let ((end (line-end-position)))
            (move-to-column x t)
            (let ((mark (point)))
              (move-to-column (+ x width) t)
              (setf (car (nth (+ y i) framebuffer))
                    (concat (buffer-substring (point-min) mark)
                            (nth i lines)
                            (buffer-substring (point) (point-max))))
              (setf (cadr (nth (+ y i) framebuffer)) t)
              (when (< end (point-max))
                (setf (cadr (cdr (nth (+ y i) framebuffer))) t))))))))
  framebuffer)

;;;###autoload
(defun poponp (object)
  "Return t if OBJECT is a popon."
  (and (listp object)
       (eq (car-safe object) 'popon)))

;;;###autoload
(defun popon-live-p (object)
  "Return t if OBJECT is a popon and not killed."
  (and (poponp object)
       (plist-get (cdr object) :live)
       (and (plist-get (cdr object) :window)
            (window-live-p (plist-get (cdr object) :window)))
       (or (not (plist-get (cdr object) :buffer))
           (buffer-live-p (plist-get (cdr object) :buffer)))
       t))

;;;###autoload
(defun popon-get (popon prop)
  "Get the PROP property of popon POPON."
  (plist-get (plist-get (cdr popon) :plist) prop))

;;;###autoload
(defun popon-put (popon prop value)
  "Set the PROP property of popon POPON to VALUE."
  (setcdr popon
          (plist-put (cdr popon) :plist
                     (plist-put (plist-get (cdr popon) :plist)
                                prop value))))

;;;###autoload
(defun popon-properties (popon)
  "Return a copy the property list of popon POPON."
  (copy-sequence (plist-get (cdr popon) :plist)))

;;;###autoload
(defun popon-position (popon)
  "Return the position of popon POPON as a cons (X, Y).

When popon POPON is killed, return nil."
  (when (popon-live-p popon)
    (cons (plist-get (cdr popon) :x)
          (plist-get (cdr popon) :y))))

;;;###autoload
(defun popon-size (popon)
  "Return the size of popon POPON as a cons (WIDTH . HEIGHT).

When popon POPON is killed, return nil."
  (when (popon-live-p popon)
    (cons (plist-get (cdr popon) :width)
          (length (plist-get (cdr popon) :lines)))))

;;;###autoload
(defun popon-window (popon)
  "Return the window popon POPON belongs to.

Return nil if popon POPON is killed."
  (when (popon-live-p popon)
    (plist-get (cdr popon) :window)))

;;;###autoload
(defun popon-buffer (popon)
  "Return the buffer popon POPON belongs to.

Return nil if popon POPON is killed."
  (when (popon-live-p popon)
    (plist-get (cdr popon) :buffer)))

;;;###autaload
(defun popon-priority (popon)
  "Return the priority of popon POPON.

Return nil if popon POPON is killed."
  (when (popon-live-p popon)
    (plist-get (cdr popon) :priority)))

;;;###autoload
(defun popon-text (popon)
  "Return the text popon POPON is displaying.

POPON may be a killed popon.  Return nil if POPON isn't a popon at
all."
  (when (poponp popon)
    (mapconcat #'identity (plist-get (cdr popon) :lines) "\n")))

(defun popon--render (popon framebuffer offset)
  "Render POPON in FRAMEBUFFER at vertical offset OFFSET."
  (popon--render-lines framebuffer
                       (+ (plist-get (cdr popon) :x) offset)
                       (plist-get (cdr popon) :y)
                       (plist-get (cdr popon) :lines)
                       (plist-get (cdr popon) :width)))

;;;###autoload
(defun popon-create (text pos &optional window buffer priority)
  "Create a popon showing TEXT at POS of WINDOW.

Display popon only if WINDOW is displaying BUFFER.

POS is a cons (X, Y), where X is column and Y is line in WINDOW.  TEXT
should be a string or a cons cell of form (STR . WIDTH).  When TEXT is
a string, each line of it should be of same length (i.e `string-width'
should return the same length for every line).  When TEXT is a cons
cell, STR is used as the text to display and each line of it should be
of visual length width.

PRIORITY is a number (integer or float) between -100 and 100.  Popons
with larger priority values are rendered first."
  (when (not (or (consp text) (stringp text)))
    (signal 'wrong-type-argument
            `((or (consp text) (stringp text)) ,text)))
  (when (not (consp pos))
    (signal 'wrong-type-argument `(consp ,pos)))
  (when (and window (not (windowp window)))
    (signal 'wrong-type-argument `(windowp ,window)))
  (when (and buffer (not (bufferp buffer)))
    (signal 'wrong-type-argument `(bufferp ,buffer)))
  (when (and priority (not (numberp priority)))
    (signal 'wrong-type-argument `(numberp ,priority)))
  (let* ((lines (split-string (if (consp text) (car text) text) "\n"))
         (popon `(popon :live t
                        :x ,(car pos)
                        :y ,(cdr pos)
                        :lines ,lines
                        :width ,(or (and (consp text) (cdr text))
                                    (string-width (car lines)))
                        :window ,(or window (selected-window))
                        :buffer ,buffer
                        :priority ,(or priority 0)
                        :plist nil)))
    (push popon (window-parameter window 'popon-list))
    (popon-update)
    popon))

;;;###autoload
(defun popon-kill (popon)
  "Kill popon POPON.

Do nothing if POPON isn't a live popon.  Return nil."
  (when (popon-live-p popon)
    (let ((window (popon-window popon)))
      (setf (window-parameter window 'popon-list)
            (delete popon (window-parameter window 'popon-list))))
    (setcdr popon (plist-put (cdr popon) :live nil))
    (popon-update)
    nil))

(defvar-local popon--line-beginnings nil
  "List of line beginning of current buffer.

The value is of form (TICK . LINE-BEGINNINGS), where LINE-BEGINNINGS
is the sorted list of beginning of lines and TICK is the value of tick
counter when LINE-BEGINNINGS was calculated.")

(defun popon--buffer-visible-substring (start end)
  "Return the visible contents of part of the current buffer.

Take the `invisible' text property into account.

START and END specifies which part to return.  They can be in any
order."
  (when (> start end)
    (let ((tmp end))
      (setq end start)
      (setq start tmp)))
  (let ((str ""))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (let ((prop (get-char-property (point) 'invisible))
              (next-change (next-single-char-property-change
                            (point) 'invisible nil end)))
          (cond
           ((null prop)
            (setq str (concat str (buffer-substring
                                   (point) next-change))))
           ((and (consp buffer-invisibility-spec)
                 (member (cons prop t) buffer-invisibility-spec))
            (setq str
                  (concat str (apply #'propertize "..." 'invisible nil
                                     (text-properties-at (point)))))))
          (goto-char next-change))))
    str))

(defun popon--make-framebuffer ()
  "Create a framebuffer for current window and buffer."
  (let ((framebuffer nil)
        (line-boundaries
         (let ((pair popon--line-beginnings)
               (boundaries nil))
           (when (eq (car pair) (buffer-modified-tick))
             (setq pair (cdr pair))
             (while pair
               (when (and (integerp (car pair))
                          (integerp (cadr pair)))
                 (push (cons (car pair) (cadr pair)) boundaries))
               (setq pair (cdr pair))))
           boundaries)))
    (save-excursion
      (goto-char (window-start))
      (let ((mark (point))
            (point-to-line nil)
            (next-invisible
             (if (get-char-property (point) 'invisible)
                 (point)
               (next-single-char-property-change
                (point) 'invisible nil (window-end)))))
        (dotimes (i (floor (window-screen-lines)))
          (if-let ((next (alist-get (point) line-boundaries)))
              (goto-char next)
            (if truncate-lines
                (forward-line 1)
              (vertical-motion 1))
            (when (< next-invisible (point))
              (let ((next-visible
                     (let ((pos next-invisible))
                       (while (get-char-property pos 'invisible)
                         (setq pos (next-single-char-property-change
                                    pos 'invisible nil (window-end))))
                       pos)))
                (setq next-invisible (next-single-char-property-change
                                      next-visible 'invisible nil
                                      (window-end)))
                (while (> next-visible (point))
                  (if truncate-lines
                      (forward-line 1)
                    (vertical-motion 1))))))
          (let ((line (alist-get mark point-to-line)))
            (unless line
              (setq line i)
              (setf (alist-get mark point-to-line) line))
            (let* ((str (popon--buffer-visible-substring
                         mark (point)))
                   (disp-str (if (>= emacs-major-version 26)
                                 (string-trim-right str "\n")
                               (let ((i (string-match-p
                                         (concat "\\(?:\n\\)\\'")
                                         str)))
                                 (if i (substring str 0 i) str)))))
              (unless (string-empty-p disp-str)
                (let ((prefix
                       (if (and (> mark (point-min))
                                (= (char-before mark) ?\n))
                           (or (get-text-property 0 'line-prefix
                                                  disp-str)
                               line-prefix)
                         (or (get-text-property 0 'wrap-prefix
                                                disp-str)
                             wrap-prefix))))
                  (when (stringp prefix)
                    (setq disp-str
                          (propertize (concat prefix disp-str)
                                      ;; Set these to empty strings to
                                      ;; avoid using the buffer-local
                                      ;; variables.
                                      'line-prefix ""
                                      'wrap-prefix "")))))
              (push (list disp-str nil nil line mark
                          (if (equal str disp-str)
                              (point)
                            (1- (point))))
                    framebuffer)))
          (push (cons mark (point)) line-boundaries)
          (setq mark (point)))))
    (let ((line-beginnings nil))
      (dolist (pair (sort (delete-dups line-boundaries)
                          #'car-less-than-car))
        (unless (eq (car line-beginnings) (car pair))
          (when (car line-beginnings)
            (push nil line-beginnings))
          (push (car pair) line-beginnings))
        (push (cdr pair) line-beginnings))
      (push nil line-beginnings)
      (setq popon--line-beginnings (cons (buffer-modified-tick)
                                         (nreverse line-beginnings))))
    (nreverse framebuffer)))

(defun popon--make-overlays (framebuffer)
  "Make overlays to display FRAMEBUFFER on window."
  (let ((line-map nil))
    (let ((i 0)
          (lines-left framebuffer))
      (while lines-left
        (let ((line (pop lines-left)))
          (when (nth 1 line)
            (when (and (< (point-min) (nth 4 line) (point-max))
                       (not (eq (char-before (nth 4 line)) ?\n)))
              ;; Let's assume that the last glyph of a wrapped
              ;; line is a invisible space.  Then we can replace
              ;; it with a newline, instead of inserting.
              (let ((ov (make-overlay (1- (nth 4 line))
                                      (nth 4 line))))
                (push ov (window-parameter nil 'popon-overlays))
                (overlay-put ov 'window (selected-window))
                (overlay-put ov 'display (copy-sequence
                                          '(space :width (0))))
                (overlay-put ov 'after-string
                             (propertize "\n"
                                         'line-prefix ""
                                         'wrap-prefix ""))))
            (let* ((key (cons (nth 4 line) (nth 5 line)))
                   (pair (assoc key line-map)))
              (unless pair
                (setq pair (list key nil nil))
                (push pair line-map))
              (setf (cadr pair) (or (cadr pair) (nth 2 line)))
              (push (cons (- i (nth 3 line)) (car line))
                    (cadr (cdr pair))))))
        (setq i (1+ i))))
    (dolist (block line-map)
      (let ((ov (make-overlay (caar block) (cdar block))))
        (push ov (window-parameter nil 'popon-overlays))
        (overlay-put ov 'window (selected-window))
        (overlay-put ov 'line-prefix "")
        (overlay-put ov 'wrap-prefix "")
        (overlay-put ov 'display (copy-sequence '(space :width (0))))
        (overlay-put
         ov 'before-string
         (let ((text "")
               (current-offset 0))
           (when (and (= (caar block) (cdar block) (point-max))
                      (not (equal (buffer-substring-no-properties
                                   (1- (caar block)) (caar block))
                                  "\n")))
             (setq text "\n"))
           (dolist (line (sort (cadr (cdr block))
                               #'car-less-than-car))
             (setq text (concat text
                                (make-string (- (car line)
                                                current-offset)
                                             ?\n)
                                (cdr line)))
             (setq current-offset (car line)))
           (add-face-text-property 0 (length text) 'default 'append
                                   text)

           ;; Pay attention to the `font-lock-face' property.
           (when font-lock-mode
             (let ((pos 0))
               (while (< pos (length text))
                 (let ((next-pos (or (next-single-property-change
                                      pos 'font-lock-face text)
                                     (length text))))
                   (when-let ((face (get-pos-property
                                     pos 'font-lock-face text)))
                     (add-face-text-property pos next-pos face nil
                                             text))
                   (setq pos next-pos)))))
           text)))
      (when (and (cadr block)
                 (< (cdar block) (point-max))
                 (not (let* ((extend-attr
                              (lambda (face)
                                (if (facep face)
                                    (when (>= emacs-major-version 27)
                                      (face-attribute face :extend))
                                  (if-let (val (plist-member
                                                face :extend))
                                      (cadr val)
                                    'unspecified))))
                             (extend 'unspecified))
                        (let ((faceprop (get-char-property
                                         (cdar block) 'face)))
                          (catch 'done
                            (if (face-list-p faceprop)
                                (dolist (face faceprop)
                                  (setq extend (funcall extend-attr
                                                        face))
                                  (when (booleanp extend)
                                    (throw 'done extend)))
                              (setq extend (funcall extend-attr
                                                    faceprop))
                              (when (booleanp extend)
                                (throw 'done extend)))
                            (funcall extend-attr 'default))))))
        (let ((ov (make-overlay (cdar block) (1+ (cdar block)))))
          (push ov (window-parameter nil 'popon-overlays))
          (overlay-put ov 'window (selected-window))
          (overlay-put ov 'face 'default))))))

(defun popon--redisplay-1 (force)
  "Redisplay popon overlays.

When FORCE is non-nil, update all overlays."
  (let ((popon-available-p nil)
        (any-popon-visible-p nil))
    (dolist (frame (frame-list))
      (dolist (window (window-list frame))
        (set-window-parameter
         window 'popon-list
         (cl-remove-if-not #'popon-live-p
                           (window-parameter window 'popon-list)))
        (when (window-parameter window 'popon-list)
          (setq popon-available-p t))
        (let ((popons
               (cl-remove-if-not
                (lambda (popon)
                  (and (or (null (popon-buffer popon))
                           (eq (popon-buffer popon) (window-buffer)))
                       (< (cdr (popon-position popon))
                          (with-selected-window window
                            (floor (window-screen-lines))))
                       (< (car (popon-position popon))
                          (- (window-width window)
                             (if (fboundp 'line-number-display-width)
                                 (with-selected-window window
                                   (line-number-display-width))
                               0)
                             (if (display-graphic-p)
                                 (let ((fringes (window-fringes
                                                 window)))
                                   (/ (+ (car fringes) (cadr fringes))
                                      (frame-char-width frame)))
                               (if (zerop (window-hscroll)) 0 1))))))
                (copy-sequence (window-parameter
                                window 'popon-list)))))
          (when (or force
                    (not
                     (and
                      (eq (window-parameter
                           window 'popon-window-start)
                          (window-start window))
                      (eq (window-parameter
                           window 'popon-window-hscroll)
                          (window-hscroll window))
                      (eq (window-parameter
                           window 'popon-window-buffer)
                          (window-buffer window))
                      (null (cl-set-exclusive-or
                             popons
                             (window-parameter
                              window 'popon-visible-popons))))))
            (while (window-parameter window 'popon-overlays)
              (delete-overlay
               (pop (window-parameter window 'popon-overlays))))
            (when popons
              (setq popons (sort popons (lambda (a b)
                                          (< (popon-priority a)
                                             (popon-priority b)))))
              (with-selected-window window
                (let* ((framebuffer (popon--make-framebuffer)))
                  (dolist (popon popons)
                    (popon--render popon framebuffer
                                   (window-hscroll)))
                  (popon--make-overlays framebuffer))))
            (set-window-parameter window 'popon-visible-popons popons)
            (set-window-parameter window 'popon-window-start
                                  (window-start window))
            (set-window-parameter window 'popon-window-hscroll
                                  (window-hscroll window))
            (set-window-parameter window 'popon-window-buffer
                                  (window-buffer window))))
        (when (window-parameter window 'popon-visible-popons)
          (setq any-popon-visible-p t))))
    (if any-popon-visible-p
        (add-hook 'pre-redisplay-functions #'popon--pre-redisplay)
      (remove-hook 'pre-redisplay-functions #'popon--pre-redisplay))
    (if popon-available-p
        (add-hook 'window-configuration-change-hook #'popon-update)
      (remove-hook 'window-configuration-change-hook
                   #'popon-update))))

;;;###autoload
(defun popon-redisplay ()
  "Redisplay popon overlays."
  (popon--redisplay-1 t))

;;;###autoload
(defun popon-update ()
  "Update popons if needed."
  (popon--redisplay-1 nil))

(defun popon--pre-redisplay (_)
  "Update popons."
  (popon-update))

;;;###autoload
(defun popon-x-y-at-posn (posn)
  "Return the (X, Y) coodinate at position POSN as a cons.

Return nil if a popon can't be shown at position POSN.

NOTE: This uses `posn-at-point', which is slow.  So try to minimize
calls to this function."
  (when (and posn (posn-point posn))
    (let* ((window-start-x-y
            (if (>= emacs-major-version 29)
                (posn-col-row (posn-at-point (window-start))
                              'use-window)
              (posn-col-row (posn-at-point (window-start)))))
           (point-x-y
            (if (>= emacs-major-version 29)
                (posn-col-row posn 'use-window)
              (posn-col-row posn)))
           (x-y (cons (if (and (or (not truncate-lines) word-wrap)
                               (if truncate-partial-width-windows
                                   (>= (window-total-width)
                                       truncate-partial-width-windows)
                                 t))
                          (- (car point-x-y) (car window-start-x-y))
                        (- (save-excursion
                             (goto-char (posn-point posn))
                             (current-column))
                           (window-hscroll)))
                      (- (cdr point-x-y) (cdr window-start-x-y)))))
      (when (and (>= (car x-y) 0)
                 (>= (cdr x-y) 0))
        x-y))))

;;;###autoload
(defun popon-x-y-at-pos (point)
  "Return the (X, Y) coodinate of POINT in selected window as a cons.

Return nil if POINT is not in visible text area.

NOTE: This uses `posn-at-point', which is slow.  So try to minimize
calls to this function."
  (popon-x-y-at-posn (posn-at-point point)))

;;;###autoload
(defun popon-kill-all ()
  "Kill all popons."
  (interactive)
  (dolist (frame (frame-list))
    (dolist (window (window-list frame))
      (while (window-parameter window 'popon-list)
        (popon-kill (pop (window-parameter window 'popon-list)))))))

(provide 'popon)
;;; popon.el ends here
