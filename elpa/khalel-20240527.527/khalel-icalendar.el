;;; khalel-icalendar.el --- Gnus iCalendar integration for khalel -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Hanno Perrey
;;
;; Author: Hanno Perrey <http://gitlab.com/hperrey>
;; Maintainer: Hanno Perrey <hanno@hoowl.se>
;; Created: november 29, 2022
;; Modified: november 29, 2022
;; Keywords: event, calendar, ics, khal, iCalendar, gnus
;; Homepage: https://gitlab.com/hperrey/khalel
;; Package-Requires: ((emacs "27.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This package provides `gnus-icalendar' integration for `khalel' to allow
;; importing events from iCalendar invitaitons. The included functions require
;; `gnus-icalendar' and will be activated by advice around functions setting up
;; buttons in `gnus-view' to handle iCalendar invitations via mail. Works in
;; `mu4e-view' too.
;;
;;; Code:

(require 'gnus-icalendar)
(require 'khalel)

(defun khalel-icalendar--gnus-import-invite (handle)
  "Decode HANDLE of an invitation from `gnus-icalendar' and import to `khal'."
  (let ((fn (make-temp-file "khalel-mm-invite-"))
        (cal (khalel--get-calendar)))
    ;; extract ics from handle
    (gnus-icalendar-with-decoded-handle handle (write-file fn))
    (let* ((import (khalel--khal-import cal fn))
           (exitstat (plist-get import :exit-status)))
      (if (/= 0 exitstat)
          (let ((buf (generate-new-buffer "*khalel-import-errors*")))
            (khalel--make-temp-window buf 16)
            (with-current-buffer buf
              (insert
               (message
                (format
                 "%s failed on import of %s for calendar '%s' and exited with status %d\n"
                 (plist-get import :process)
                 fn
                 cal
                 exitstat)))
              (insert "\n" (plist-get import :output) "\n")
              (special-mode)))
        (progn
          (when
              khalel-import-events-after-capture
            (khalel-import-events))
          (message (format "Imported event into calendar '%s'" cal)))
        (zerop exitstat)))))

(defun khalel-icalendar--inject-gnus-inline-buttons (orig-fun &rest args)
  "Advice ORIG-FUN called with ARGS to add `khalel' buttons to `gnus-icalendar'."
  (let*
      ((handle (cadr args))
      (event (car args))
       (buttons (apply orig-fun args)))
  (append
   buttons
   (list
   `("Show Agenda" gnus-icalendar-show-org-agenda ,event)
   `("khal import" khalel-icalendar--gnus-import-invite ,handle)))))

(advice-remove 'gnus-icalendar-event:inline-reply-buttons #'khalel-icalendar--inject-gnus-inline-buttons)
(advice-add 'gnus-icalendar-event:inline-reply-buttons :around #'khalel-icalendar--inject-gnus-inline-buttons)


(provide 'khalel-icalendar)
;;; khalel-icalendar.el ends here
