;;; yeetube.el --- Scrape YouTube, Play with mpv & Download with yt-dlp  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions youtube videos
;; URL: https://thanosapollo.org/projects/yeetube/
;; Package-Version: 20250218.405
;; Package-Revision: e37a4ff950a0

;; Package-Requires: ((emacs "27.2") (compat "29.1.4.2"))

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

;; This package provides the ability to scrape YouTube, with the results
;; displayed in a tabulated list.
;;
;; Core features include:
;;
;; - Executing YouTube queries
;; - Playing videos, by default using MPV
;; - Downloading videos, using yt-dlp
;; - Bookmarking and saving video URLs
;; - A standalone, minimalist yt-dlp frontend

;;; Code:

(require 'compat)
(require 'url)
(require 'tabulated-list)
(require 'cl-lib)
(require 'socks)
(require 'url-handlers)
(require 'mm-decode)
(require 'xdg)

(require 'yeetube-mpv)

(defgroup yeetube nil
  "Youtube Front-End."
  :group 'external
  :prefix "yeetube-")

(defcustom yeetube-results-limit 20
  "Define a limit for search results."
  :type 'number
  :group 'yeetube)

(defcustom yeetube-play-function #'yeetube-mpv-play
  "Select media player function."
  :type 'function
  :group 'yeetube)

(defcustom yeetube-download-audio-format nil
  "Download videos as specified audio only formats."
  :type '(radio (const :tag "None" nil)
		(const :tag "AAC" "aac")
		(const :tag "ALAC" "alac")
		(const :tag "FLAC" "flac")
		(const :tag "M4A" "m4a")
		(const :tag "MP3" "mp3")
		(const :tag "OPUS" "opus")
		(const :tag "Vorbis" "vorbis")
		(const :tag "WAV" "wav"))
  :group 'yeetube)

(defcustom yeetube-download-directory (or (xdg-user-dir "DOWNLOAD") (getenv "HOME"))
  "Default directory to downlaod videos."
  :type 'string
  :group 'yeetube)

(defcustom yeetube-filter "Relevance"
  "Sort search results for value.
Valid options include:
- \"Relevance\"
- \"Date\"
- \"Views\"
- \"Rating\""
  :type '(radio (const "Relevance")
		(const "Date")
		(const "Views")
		(const "Rating")))

(defcustom yeetube-default-sort-column nil
  "Column to sort the search results table."
  :type '(radio (const :tag "None" nil)
		(const "Title")
                (const "Views")
                (const "Duration")
                (const "Channel"))
  :group 'yeetube)

(defcustom yeetube-default-sort-ascending nil
  "Whether to sort the search results in ascending order."
  :type 'boolean
  :group 'yeetube)

(defcustom yeetube-enable-tor nil
  "Enable routing through tor."
  :type 'boolean
  :group 'yeetube)

(defcustom yeetube-enable-emojis t
  "Enable emojis in *yeetube* buffer."
  :type 'boolean
  :group 'yeetube)

(defcustom yeetube-pop-to-same-window-p t
  "When non-nil, create *yeetube* buffer at the same window."
  :type 'boolean
  :group 'yeetube)

(defgroup yeetube-faces nil
  "Faces used by yeetube."
  :group 'yeetube
  :tag "Yeetube Faces"
  :prefix 'yeetube-face)

(defcustom yeetube-thumbnail-size '(120 . 90)
  "Thumbnail size (height width)."
  :type '(cons integer integer)
  :group 'yeetube)

(defcustom yeetube-display-thumbnails-p t
  "When non-nil, fetch & display thumbnails."
  :type 'boolean
  :group 'yeetube)

(defface yeetube-face-header-query
  '((t :inherit font-lock-function-name-face))
  "Face used for the video published date."
  :group 'yeetube-faces)

(defface yeetube-face-duration
  '((t :inherit font-lock-string-face))
  "Face used for the video duration."
  :group 'yeetube-faces)

(defface yeetube-face-view-count
  '((t :inherit font-lock-keyword-face))
  "Face used for the video view count."
  :group 'yeetube-faces)

(defface yeetube-face-title
  '((t :inherit font-lock-variable-use-face))
  "Face used for video title."
  :group 'yeetube-faces)

(defface yeetube-face-channel
  '((t :inherit font-lock-function-call-face))
  "Face used for video channel name."
  :group 'yeetube-faces)

(defface yeetube-face-date
  '((t :inherit font-lock-doc-face))
  "Face used for published date."
  :group 'yeetube-faces)

(defvar yeetube-invidious-instances
  '("vid.puffyan.us" "inv.nadeko.net" "invidious.flokinet.to")
  "List of invidious instaces.")

(defvar yeetube-content nil
  "Scraped content.")

(defvar yeetube-saved-videos nil
  "Saved/bookmarked video urls.")

(defvar yeetube-history nil
  "Stored urls & titles of recently played content.")

(defvar yeetube-search-history nil
  "History of search terms.")

(defvar yeetube-video-url "https://youtube.com/watch?v="
  "URL used to play videos from.

You can change this value to an invidious instance.  Although yeetube
will still query youtube, `yeetube-play' will use the above url to play
videos from.")

(defvar yeetube-playlist-url "https://youtube.com/playlist?list="
  "URL used to play playlists from.

You can change this value to an invidious instance.  Although yeetube
will still query youtube, `yeetube-play' will use the above url to play
videos from.")

(defvar yeetube--channel-id nil
  "Value of channel which `yeetube-channel-videos' used for.")

(defun yeetube-get-url (&optional id type)
  "Get video or playlist url for entry ID, adjusted for TYPE."
  (let* ((id (or id (tabulated-list-get-id)))
	 (entry (cadr (assoc id yeetube-content)))
	 (type (or type (aref entry (- (length entry) 1)))))
    (format "%s%s" (if (eq type 'video)
		       yeetube-video-url
		     yeetube-playlist-url)
	    id)))

;;;###autoload
(defun yeetube-play ()
  "Play video at point in *yeetube* buffer."
  (interactive)
  (save-excursion
    ;; When point is on thumbnail, id will be nil.
    (and (null (tabulated-list-get-id)) (end-of-line))
    (let* ((id (tabulated-list-get-id))
	   (entry-content (cadr (assoc id yeetube-content)))
	   (video-url (yeetube-get-url id))
	   (video-title (aref entry-content (if yeetube-display-thumbnails-p 1 0)))
	   (proc (apply yeetube-play-function video-url
			(when yeetube-mpv-modeline-mode (list video-title)))))
      (when (processp proc)
	(process-put proc :now-playing video-title))
      (push (list :url video-url :title video-title) yeetube-history)
      (message "Playing: %s" video-title))))

;;;###autoload
(defun yeetube-replay ()
  "Select entry from history to replay.

Select entry title from `yeetube-history' and play corresponding URL."
  (interactive)
  (let* ((titles (mapcar (lambda (entry) (cl-getf entry :title)) yeetube-history))
         (selected (completing-read "Replay: " titles))
         (selected-entry (cl-find-if (lambda (entry)
				       (string= selected (cl-getf entry :title)))
				     yeetube-history))
	 (title (cl-getf selected-entry :title))
         (url (cl-getf selected-entry :url)))
    (funcall yeetube-play-function url (when yeetube-mpv-modeline-mode title))
    (message "Replaying: %s" selected)))

(defun yeetube-load-saved-videos ()
  "Load saved videos."
  (let ((file-path (concat user-emacs-directory "yeetube")))
    (if (file-exists-p file-path)
	(with-temp-buffer
	  (insert-file-contents file-path)
	  (goto-char (point-min))
	  (let ((contents (read (current-buffer))))
	    (setf yeetube-saved-videos contents)))
      (write-region "nil" nil file-path))))

;;;###autoload
(defun yeetube-save-video (arg)
  "Save url at point.

If ARG is non-nil, save as a playlist URL."
  (interactive "P")
  (yeetube-load-saved-videos)
  (let ((name (read-string "Save as: "))
	(url (yeetube-get-url (tabulated-list-get-id) (if arg 'playlist 'video))))
    (push (cons name url) yeetube-saved-videos)))

;; We could use keywords here, but it would break users saved videos
;; from previous versions.
;;;###autoload
(defun yeetube-play-saved-video ()
  "Select & Play a saved video."
  (interactive)
  (yeetube-load-saved-videos)
  (let* ((video (completing-read "Select video: " yeetube-saved-videos nil t))
	 (url (cdr (assoc video yeetube-saved-videos)))
	 (title (car (assoc video yeetube-saved-videos))))
    (funcall yeetube-play-function url (when yeetube-mpv-modeline-mode title))
    (message "Playing: %s" (car (assoc video yeetube-saved-videos)))))

;;;###autoload

(defun yeetube-remove-saved-video ()
  "Select video to remove from saved videos."
  (interactive)
  (yeetube-load-saved-videos)
  (let ((video (completing-read "Select video: " yeetube-saved-videos nil t)))
    (setf yeetube-saved-videos (remove (assoc video yeetube-saved-videos) yeetube-saved-videos))))

;;;###autoload
(defun yeetube-remove-all-saved-videos ()
  "Clear yeetube saved."
  (interactive)
  (let ((clear-saved (y-or-n-p "Delete saved?")))
    (when clear-saved
      (setf yeetube-saved-videos nil))))

(defun yeetube-update-saved-videos-list (_symbol new-value _where _environment)
  "Updated saved videos.

SYMBOL-NAME is the name of the symbol to update.
NEW-VALUE is the new value for the symbol.
OPERATION is the operation to perform.
WHERE indicates where in the buffer the update should happen."
  (with-temp-buffer (find-file (concat user-emacs-directory "yeetube"))
		    (erase-buffer)
		    (setf yeetube-saved-videos new-value)
		    (insert (pp-to-string yeetube-saved-videos))
		    (save-buffer)
		    (kill-buffer)))

(defvar yeetube-filter-code-alist
  '(("Relevance" . "EgIQAQ%253D%253D")
    ("Date" . "CAISAhAB")
    ("Views" . "CAMSAhAB")
    ("Rating" . "CAESAhAB"))
  "Filter codes.")

(defvar yeetube-request-headers
  '(("Accept-Language" . "Accept-Language: en-US,en;q=0.9")
    ("Accept" . "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8")
    ("User-Agent" . "Mozilla/5.0 (Windows NT 10.0; rv:126.0) Gecko/20100101 Firefox/126.0"))
  "HTTP Request extra headers.")

(defun yeetube-get-filter-code (filter)
  "Get FILTER code for sorting search results."
  (cdr (assoc filter yeetube-filter-code-alist)))

(defmacro yeetube-with-tor-socks (&rest body)
  "Execute BODY with torsocks."
  `(let ((url-gateway-method 'socks)
         (socks-noproxy '("localhost"))
         (socks-server '("Default server" "127.0.0.1" 9050 5)))
     ,@body))

(defun yeetube--callback (status)
  "Yeetube callback handling STATUS."
  (let ((url-buffer (current-buffer))
	(pop-to-buffer-func (if yeetube-pop-to-same-window-p
				#'pop-to-buffer-same-window
			      #'pop-to-buffer)))
    (unwind-protect
        (if-let ((err (plist-get :error status)))
            (message "Error %s in retrieving yeetube results: %S" (car err) (cdr err))
          (with-temp-buffer
            (set-buffer-multibyte t)
            (url-insert url-buffer)
            (decode-coding-region (point-min) (point-max) 'utf-8)
            (yeetube-get-content))
          (funcall pop-to-buffer-func "*yeetube*")
          (yeetube-mode))
      (kill-buffer url-buffer))))

(defun yeetube-display-content-from-url (url)
  "Display the video results from URL."
  (let ((url-request-extra-headers yeetube-request-headers))
    (if yeetube-enable-tor
        (yeetube-with-tor-socks
         (url-retrieve url #'yeetube--callback nil 'silent 'inhibit-cookies))
      (url-retrieve url #'yeetube--callback nil 'silent 'inhibit-cookies))))

(defun yeetube--image-callback (status entry buffer)
  "Yeetube callback for thumbnail images handling STATUS.
Image is inserted in BUFFER for ENTRY."
  (let ((url-buffer (current-buffer)))
    (unwind-protect
        (if-let ((err (plist-get status :error)))
            (message "Error %s in retrieving a thumbnail: %S" (car err) (cdr err))
          (if-let ((handle (mm-dissect-buffer t))
                   (image (mm-get-image handle)))
              (progn
                (setf (image-property image :max-width) (car yeetube-thumbnail-size)
                      (image-property image :max-height) (cdr yeetube-thumbnail-size))
                ;; Debugging Statement
                ;; (message "Retrieved and processing image for %s" (plist-get entry :title))
                (with-current-buffer buffer
                  (with-silent-modifications
                    (save-excursion
                      (goto-char (point-min))
                      (search-forward (format "[[%s.jpg]]" (car entry)))
                      ;; Ensure to remove the placeholder text
                      (delete-region (match-beginning 0) (match-end 0))
                      (insert-image image))))))
	  (kill-buffer url-buffer)))))

(defun yeetube--retrieve-thumbnail (url str buffer)
  "Retrieve thumbnail from URL and show it in place of STR in BUFFER."
  (let ((url-request-extra-headers yeetube-request-headers))
    (when yeetube-display-thumbnails-p
      (if yeetube-enable-tor
          (yeetube-with-tor-socks
           (url-queue-retrieve url #'yeetube--image-callback `(,str ,buffer)
                               'silent 'inhibit-cookies))
	(url-queue-retrieve url #'yeetube--image-callback `(,str ,buffer)
                            'silent 'inhibit-cookies)))))

(defun yeetube-read-query ()
  "Interactively read a search term."
  (read-string "Yeetube Search: " nil 'yeetube-search-history))

;;;###autoload
(defun yeetube-search (query)
  "Search for QUERY."
  (interactive (list (yeetube-read-query)))
  (yeetube-display-content-from-url
   (format "https://youtube.com/search?q=%s%s"
           (url-hexify-string query)
           (if yeetube-filter
	       (format "&sp=%s" (yeetube-get-filter-code yeetube-filter))
	     ""))))

(defun yeetube-channel-id-at-point ()
  "Return yeetube channel id at point."
  (let* ((id (tabulated-list-get-id))
	 (content (cadr (assoc id yeetube-content)))
	 (channel-id (aref content (- (length content) 2))))
    channel-id))

(defun yeetube-channel-videos (&optional channel-id)
  "View videos for the channel with CHANNEL-ID."
  (interactive (list (or (yeetube-channel-id-at-point)
			 (format "@%s" (read-string "Channel: ")))))
  (setf yeetube--channel-id (substring channel-id 2))
  (yeetube-display-content-from-url (format "https://youtube.com/%s/videos" channel-id)))

(defun yeetube-channel-streams (&optional channel-id)
  "View streams for the channel with CHANNEL-ID."
  (interactive (list (or (yeetube-channel-id-at-point)
			 (format "@%s" (read-string "Channel: ")))))
  (setf yeetube--channel-id (substring channel-id 2))
  (yeetube-display-content-from-url (format "https://youtube.com/%s/streams" channel-id)))

(defun yeetube-channel-search (channel-id query)
  "Search channel with CHANNEL-ID for videoes matching QUERY."
  (interactive (list (yeetube-channel-id-at-point) (yeetube-read-query)))
  (yeetube-display-content-from-url
   (format "https://youtube.com/%s/search?query=%s"
           channel-id (url-hexify-string query))))

(defun yeetube-video-or-playlist-page ()
  "View videos in playlist or those found on the video page."
  (interactive)
  (yeetube-display-content-from-url (yeetube-get-url)))

;;;###autoload
(defun yeetube-browse-url ()
  "Open URL for video at point, using an invidious instance."
  (interactive)
  (let ((invidious-instance (cond ((and (listp yeetube-invidious-instances)
					(length> yeetube-invidious-instances 1))
				   (nth (random (length yeetube-invidious-instances))
					yeetube-invidious-instances))
				  ((and (listp yeetube-invidious-instances)
					(length= yeetube-invidious-instances 1))
				   (car yeetube-invidious-instances))
				  ((stringp yeetube-invidious-instances)
				   yeetube-invidious-instances))))
    (browse-url
     (replace-regexp-in-string "youtube.com" invidious-instance (yeetube-get-url)))))

(defun yeetube--scrape-string (pos item &optional sub-item)
  "Scrape string corresponding of SUB-ITEM of ITEM after POS."
  (goto-char pos)
  (search-forward item nil t)
  (when sub-item
    (search-forward sub-item nil t))
  (forward-char)
  (search-forward "\"")
  (backward-char)
  (if (fboundp 'json-parse-buffer)
      (json-parse-buffer)
    (json-read)))

(defun yeetube-view-count-format (string)
  "Add commas for STRING."
  (let* ((string (replace-regexp-in-string "[^0-9]" "" string))
         (len (length string))
         (result ""))
    (cl-loop for i from 0 to (1- len)
             do (setf result (concat (substring string (- len i 1) (- len i)) result))
             if (and (> (- len (1+ i)) 0)
                     (= (% (1+ i) 3) 0))
             do (setf result (concat "," result)))
    result))

(defun yeetube-get-content ()
  "Get content from youtube."
  (setf yeetube-content nil)
  (goto-char (point-min))
  (let ((count 0)
        (result-rx
	 (rx "\"" (or "video" (and (or "playlist" "compact") (? "Video"))) "Renderer\""))
        id ids videop pos)
    ;; Keep scraping while there are results and the limit is not reached
    (while (and (< count yeetube-results-limit)
                (re-search-forward result-rx nil t))
      ;; Increment count
      (cl-incf count)
      (setq pos (point))
      (setq videop (not (equal (match-string 0) "\"playlistRenderer\"")))
      (setq id (yeetube--scrape-string pos (if videop "videoId" "playlistId")))
      (unless (member id ids)
        (push id ids)
        (save-excursion
	  ;; Scrape necessary data and push to list of contents
          (let ((title (yeetube--scrape-string pos "title"
					       (if videop "text"
						 "simpleText")))
                (view-count (when videop
			      (yeetube--scrape-string pos "viewCountText" "simpleText")))
                (duration (if videop
                              (yeetube--scrape-string pos "lengthText" "simpleText")
                            (format "%s videos"
				    (yeetube--scrape-string pos "videoCount"))))
                (channel (yeetube--scrape-string pos "longBylineText" "text"))
                (channel-id (yeetube--scrape-string pos "canonicalBaseUrl"))
                (thumbnail (yeetube--scrape-string pos "thumbnail" "url"))
                (date (when videop
			(yeetube--scrape-string pos "publishedTimeText" "simpleText")))
                (entry))
	    (when (string= channel title) (setf channel yeetube--channel-id))
            (setq thumbnail (string-replace
                             "hq720" "default"
                             (substring thumbnail 0 (string-search "?" thumbnail))))
	    ;; Create an entry with properties.
            (setq entry
                  (list id
			(format "[[%s.jpg]]" id)
			(propertize
			 (if videop title (concat "Playlist: " title))
			 'face 'yeetube-face-title)
                        (propertize
			 (yeetube-view-count-format (or view-count ""))
			 'face 'yeetube-face-view-count)
                        (propertize duration 'face 'yeetube-face-duration)
			(propertize (string-replace "Streamed " "" (or date ""))
				    'face 'yeetube-face-date)
			(propertize channel 'face 'yeetube-face-channel)
			channel-id
			(if videop 'video 'playlist)))
            (yeetube--retrieve-thumbnail thumbnail entry "*yeetube*")
	    ;; Push entry in a format to be used with tabulated-list
            (push (list (car entry) (if yeetube-display-thumbnails-p
					(vconcat (cdr entry))
				      (vconcat (cddr entry))))
		  yeetube-content))))))
  ;; Reverse the list of entries before returning
  (cl-callf nreverse yeetube-content))

(add-variable-watcher 'yeetube-saved-videos #'yeetube-update-saved-videos-list)

;; Yeetube Downlaod:

(defvar yeetube-ytdlp (executable-find "yt-dlp")
  "Path for yt-dlp executable.")

;;;###autoload
(defun yeetube-download-change-directory ()
  "Change download directory."
  (interactive)
  (setf yeetube-download-directory
        (read-directory-name "Select a directory: ")))

;;;###autoload
(defun yeetube-download-change-audio-format (audio-format)
  "Change download format to AUDIO-FORMAT."
  (interactive "sSpecify Audio Format(no for nil): ")
  (setf yeetube-download-audio-format audio-format)
  (when (equal yeetube-download-audio-format "no")
    (setf yeetube-download-audio-format nil)))

(defun yeetube-download--ytdlp (url &optional name audio-format)
  "Download URL using yt-dlp.

Optional values:
 NAME for custom file name.
 AUDIO-FORMAT to extract and keep contents as specified audio-format only."
  (unless (executable-find "yt-dlp")
    (error "Executable for yt-dlp not found.  Please install yt-dlp"))
  (let* ((tor-command (when yeetube-enable-tor (executable-find "torsocks")))
         (name-command (when name (format "-o %s" (shell-quote-argument name))))
         (format-command (when audio-format
			   (format "--extract-audio --audio-format %s"
				   (shell-quote-argument audio-format))))
         (command (mapconcat 'identity (delq nil
					     (list tor-command
						   (executable-find "yt-dlp")
						   (shell-quote-argument url)
						   name-command format-command))
			     " ")))
    (call-process-shell-command command nil 0)))

;;;###autoload
(defun yeetube-download-video (&optional url)
  "Download entry at point in *yeetube* buffer with yt-dlp.

Content will be downloaded at `yeetube-download-directory'.
Optionally, provide custom own URL."
  (interactive)
  (let* ((id (tabulated-list-get-id))
	 (entry-content (cadr (assoc id yeetube-content)))
	 (type (aref entry-content (- (length entry-content) 1)))
	 (url (or (yeetube-get-url id type) url))
	 (title (or (aref entry-content 0) "Unknown")))
    (when (string-prefix-p "http" url)
      (let ((default-directory yeetube-download-directory))
        (yeetube-download--ytdlp url nil yeetube-download-audio-format)
        (message "Downloading: '%s' at '%s'"
		 title yeetube-download-directory)))))

;; TODO: Add option to use ffmpeg
;;;###autoload
(defun yeetube-download-videos ()
  "Bulk download videos using yt-dlp.
This command is not meant to be used in the *Yeetube Search* buffer.

Usage Example:
Open a Dired buffer and navigate where you want to download your
videos, then run this command interactively.  You can leave the name
prompt blank to keep the default name."
  (interactive)
  (let ((download-counter 1))
    (cl-loop
     for url = (read-string "Enter URL (q to quit): ")
     until (string= url "q")
     do (let ((name (read-string (format "Custom name (download counter: %d) "
					 download-counter))))
          (yeetube-download--ytdlp url name yeetube-download-audio-format)
          (cl-incf download-counter)))))

(defun yeetube-propertize-vector (content &rest fields-face-pairs)
  "Create a vector with each item propertized with its corresponding face.

CONTENT is a list of strings.
FIELDS-FACE-PAIRS is a list of fields and faces."
  (apply #'vector
         (cl-loop for (field face) on fields-face-pairs by #'cddr
                  collect (propertize (cl-getf content field) 'face face))))

;; Yeetube Mode
(defvar-keymap yeetube-mode-map
  :doc "Keymap for yeetube commands"
  "RET" #'yeetube-play
  "M-RET" #'yeetube-search
  "C-<return>" #'yeetube-video-or-playlist-page
  "b" #'yeetube-browse-url
  "c" #'yeetube-channel-videos
  "d" #'yeetube-download-video
  "D" #'yeetube-download-change-directory
  "a" #'yeetube-download-change-audio-format
  "p" #'yeetube-mpv-toggle-pause
  "v" #'yeetube-mpv-toggle-video
  "V" #'yeetube-mpv-toggle-no-video-flag
  "s" #'yeetube-save-video
  "S" #'yeetube-channel-streams
  "P" #'yeetube-play-saved-video
  "r" #'yeetube-replay
  "T" #'yeetube-mpv-toggle-torsocks
  "C-q" #'yeetube-mpv-change-video-quality
  "q" #'quit-window)

(defun yeetube--sort-views (a b)
  "Sort entries A and B by view count."
  (let ((views-a (string-to-number (replace-regexp-in-string "," "" (aref (cadr a) 2))))
        (views-b (string-to-number (replace-regexp-in-string "," "" (aref (cadr b) 2)))))
    (< views-a views-b)))

(defun yeetube--duration-to-seconds (duration)
  "Convert DURATION string in ='HH:MM:SS' format to total seconds."
  (let* ((parts (mapcar #'string-to-number (split-string duration ":")))
         (len (length parts)))
    (cond
     ((= len 3) (+ (* (nth 0 parts) 3600) (* (nth 1 parts) 60) (nth 2 parts)))
     ((= len 2) (+ (* (nth 0 parts) 60) (nth 1 parts)))
     ((= len 1) (nth 0 parts))
     (t 0))))

(defun yeetube--sort-duration (a b)
  "Sort entries A and B by duration."
  (let ((duration-a (yeetube--duration-to-seconds (aref (cadr a) 3)))
        (duration-b (yeetube--duration-to-seconds (aref (cadr b) 3))))
    (< duration-a duration-b)))

(defun yeetube--parse-relative-date (date)
  "Convert relative DATE like '2 days ago' to a comparable number based on seconds."
  (let* ((split-date (split-string date " "))
         (value (string-to-number (nth 0 split-date)))
         (unit (nth 1 split-date))
         (seconds-per-unit
	  (cond
           ((or (string= "second" unit) (string= "seconds" unit)) 1)
           ((or (string= "minute" unit) (string= "minutes" unit)) 60)
           ((or (string= "hour" unit) (string= "hours" unit)) (* 60 60))
           ((or (string= "day" unit) (string= "days" unit)) (* 60 60 24))
           ((or (string= "week" unit) (string= "weeks" unit)) (* 60 60 24 7))
           ((or (string= "month" unit) (string= "months" unit)) (* 60 60 24 30))
           ((or (string= "year" unit) (string= "years" unit)) (* 60 60 24 365))
           (t 0))))
    (* value seconds-per-unit)))

(defun yeetube--sort-date (a b)
  "Sort entries A and B by relative date."
  (let ((date-a (yeetube--parse-relative-date (aref (cadr a) 4)))
        (date-b (yeetube--parse-relative-date (aref (cadr b) 4))))
    (< date-a date-b)))

(defun yeetube--tabulated-list-format (thumbnail-p)
  "Return tabulated-list format vector.

If THUMBNAIL-P is non-nil, add thumbnail."
  (let ((list-format `[("Thumbnail"  ,(/ (window-width) 10) nil)
		       ("Title" ,(/ (window-width) 3) t)
		       ("Views" ,(/ (window-width) 10) yeetube--sort-views)
		       ("Duration" ,(/ (window-width) 10)  yeetube--sort-duration)
		       ("Date" ,(/ (window-width) 8) yeetube--sort-date)
		       ("Channel" ,(/ (window-width) 8) t)]))
    (if thumbnail-p list-format (cl-subseq list-format 1))))

(defun yeetube-tabulated-list (&optional thumbnail-p)
  "Return a tabulated list, adjusted for `window-width'

If THUMBNAIL-P is non-nil, display thumbnails.."
  (let ((thumbnail-p (or thumbnail-p yeetube-display-thumbnails-p)))
    (setf tabulated-list-format (yeetube--tabulated-list-format thumbnail-p)
	  tabulated-list-entries yeetube-content
	  tabulated-list-sort-key
	  (cons yeetube-default-sort-column yeetube-default-sort-ascending))
    (tabulated-list-print)))

(define-derived-mode yeetube-mode tabulated-list-mode "Yeetube"
  "Yeetube mode."
  :keymap yeetube-mode-map
  (yeetube-tabulated-list)
  (setq-local yeetube-mpv-show-status t)
  (display-line-numbers-mode 0)
  (tabulated-list-init-header)
  (when (and (fboundp 'emojify-mode)
	     yeetube-enable-emojis)
    (emojify-mode 1)))

(provide 'yeetube)
;;; yeetube.el ends here
