;;; yeetube-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from yeetube.el

(autoload 'yeetube-play "yeetube" "\
Play video at point in *yeetube* buffer." t)
(autoload 'yeetube-replay "yeetube" "\
Select entry from history to replay.

Select entry title from `yeetube-history' and play corresponding URL." t)
(autoload 'yeetube-save-video "yeetube" "\
Save url at point.

If ARG is non-nil, save as a playlist URL.

(fn ARG)" t)
(autoload 'yeetube-play-saved-video "yeetube" "\
Select & Play a saved video." t)
(autoload 'yeetube-remove-saved-video "yeetube" "\
Select video to remove from saved videos." t)
(autoload 'yeetube-remove-all-saved-videos "yeetube" "\
Clear yeetube saved." t)
(autoload 'yeetube-search "yeetube" "\
Search for QUERY.

(fn QUERY)" t)
(autoload 'yeetube-browse-url "yeetube" "\
Open URL for video at point, using an invidious instance." t)
(autoload 'yeetube-download-change-directory "yeetube" "\
Change download directory." t)
(autoload 'yeetube-download-change-audio-format "yeetube" "\
Change download format to AUDIO-FORMAT.

(fn AUDIO-FORMAT)" t)
(autoload 'yeetube-download-video "yeetube" "\
Download entry at point in *yeetube* buffer with yt-dlp.

Content will be downloaded at `yeetube-download-directory'.
Optionally, provide custom own URL.

(fn &optional URL)" t)
(autoload 'yeetube-download-videos "yeetube" "\
Bulk download videos using yt-dlp.
This command is not meant to be used in the *Yeetube Search* buffer.

Usage Example:
Open a Dired buffer and navigate where you want to download your
videos, then run this command interactively.  You can leave the name
prompt blank to keep the default name." t)
(register-definition-prefixes "yeetube" '("yeetube-"))


;;; Generated autoloads from yeetube-mpv.el

(register-definition-prefixes "yeetube-mpv" '("yeetube-mpv-"))

;;; End of scraped data

(provide 'yeetube-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; yeetube-autoloads.el ends here
