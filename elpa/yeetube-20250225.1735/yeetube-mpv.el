;;; yeetube-mpv.el --- Provide yeetube mpv functionality  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions youtube videos
;; URL: https://git.thanosapollo.org/yeetube

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package is a yeetube extension, to start an mpv process &
;; remotely control it.

;;; Code:

(defcustom yeetube-mpv-disable-video nil
  "Add no-video flag when using mpv."
  :type 'boolean
  :group 'yeetube)

(defcustom yeetube-mpv-enable-torsocks nil
  "Enable torsocks."
  :type 'boolean
  :group 'yeetube)

(defcustom yeetube-mpv-additional-flags ""
  "Additional flags to pass to mpv."
  :type 'string
  :group 'yeetube)

(defvar yeetube-mpv-torsocks (executable-find "torsocks")
  "Path to torsocks executable.")

(defvar yeetube-mpv-video-quality "720"
  "Video resolution/quality.
Accepted values include: 1080, 720, 480, 360, 240, 144")

(defvar yeetube-mpv-currently-playing nil
  "Currently playing information.")

(defun yeetube-mpv-modeline-string ()
  "Return modeline string for yeetube-mpv."
  (if yeetube-mpv-currently-playing
      (format "%s" yeetube-mpv-currently-playing)
    "nil"))

(defun yeetube-mpv-change-video-quality ()
  "Change video quality."
  (interactive)
  (let ((new-value (completing-read (format "Set video quality (current value %s):" yeetube-mpv-video-quality)
				    '("1080" "720" "480" "360" "240" "144") nil t)))
    (setf yeetube-mpv-video-quality new-value)))

(defun yeetube-mpv-toggle-torsocks ()
  "Toggle torsocks."
  (interactive)
  (pcase yeetube-mpv-torsocks
    ('t (setf yeetube-mpv-torsocks nil)
	(message "yeetube: Torsocks disabled"))
    ('nil (setf yeetube-mpv-torsocks t)
	  (message "yeetube: Torsocks enabled"))))

(defun yeetube-mpv-check ()
  "Check if mpv and yt-dlp is installed."
  (unless (and (executable-find "mpv")
	       (executable-find "yt-dlp"))
    (error "Unable to play video.  Please install `yt-dlp' and `mpv'")))

(defun yeetube-mpv-process (command)
  "Start yeetube process for shell COMMAND."
  (yeetube-mpv-check)
  (let ((yeetube-mpv-process "yeetube"))
    (dolist (process (process-list))
      (when (string-match yeetube-mpv-process (process-name process))
	(kill-process process)))
    (sit-for 0.1)
    (unless (get-process yeetube-mpv-process)
      (start-process-shell-command
       "yeetube" nil command))))

(defun yeetube-mpv-ytdl-format-video-quality (resolution)
  "Return shell quoted argument for ytdlp with RESOLUTION."
  (shell-quote-argument (format "bestvideo[height<=?%s]+bestaudio/best" resolution)))

(defun yeetube-mpv-play (input &optional info)
  "Start yeetube process to play INPUT using mpv.

INFO: Information to display with `yeetube-mpv-modeline-mode'

This function is not specific to just playing urls.  Feel free to use
to play local files."
  (let ((yeetube-mpv-path (executable-find "mpv")))
    (yeetube-mpv-process
     (concat (when yeetube-mpv-enable-torsocks
	       (concat yeetube-mpv-torsocks " "))
	     yeetube-mpv-path " --ytdl-format="
	     (yeetube-mpv-ytdl-format-video-quality yeetube-mpv-video-quality)
	     " "
	     (shell-quote-argument input)
	     (when yeetube-mpv-disable-video " --no-video")
	     yeetube-mpv-additional-flags))
    (message (if yeetube-mpv-enable-torsocks
		 "yeetube: Starting mpv process (using torsocks)"
	       "yeetube: Starting mpv process"))
    (setf yeetube-mpv-currently-playing (format "[%s]" info))))

(define-minor-mode yeetube-mpv-modeline-mode
  "Minor mode for showing currently playing information on the modeline.

To use this mode, you should set `yeetube-play-function' to
`yeetube-mpv-play'."
  :global t
  :group 'yeetube
  :lighter nil
  (if yeetube-mpv-modeline-mode
      (progn
	(add-to-list 'global-mode-string '(:eval
					   (format " ♫:%s" (yeetube-mpv-modeline-string))))
	(force-mode-line-update))
    (setf global-mode-string
          (seq-remove (lambda (item)
                        (and (listp item) (eq (car item) :eval)
                             (string-prefix-p " ♫:" (format "%s" (eval (cadr item))))))
                      global-mode-string))
    (force-mode-line-update)))

(defun yeetube-mpv-toggle-no-video-flag ()
  "Toggle no video flag for mpv player."
  (interactive)
  (if yeetube-mpv-disable-video
      (progn (setf yeetube-mpv-disable-video nil)
	     (message "yeetube: mpv enabled video"))
    (setf yeetube-mpv-disable-video t)
    (message "yeetube: mpv disabled video")))

(defun yeetube-mpv-send-keypress (key)
  "Send KEY to `yeetube-mpv-process'."
  (interactive "sKey: ")
  (process-send-string "yeetube" key))

(defun yeetube-mpv-toggle-pause ()
  "Toggle pause mpv."
  (interactive)
  (yeetube-mpv-send-keypress "p")
  (message "yeetube: toggle pause"))

(defun yeetube-mpv-toggle-fullscreen ()
  "Toggle fullscreen."
  (interactive)
  (yeetube-mpv-send-keypress "f")
  (message "toggle fullscreen"))

(defun yeetube-mpv-toggle-video ()
  "Toggle video mpv."
  (interactive)
  (yeetube-mpv-send-keypress "_")
  (message "yeetube: toggle video"))

(defun yeetube-mpv-forward ()
  "Forward video."
  (interactive)
  (yeetube-mpv-send-keypress "[C"))

(defun yeetube-mpv-backward ()
  "Go backwards in video."
  (interactive)
  (yeetube-mpv-send-keypress "[D"))

(defun yeetube-mpv-quit ()
  "Quit mpv."
  (interactive)
  (yeetube-mpv-send-keypress "q")
  (message "yeetube: quit"))

(provide 'yeetube-mpv)
;;; yeetube-mpv.el ends here
