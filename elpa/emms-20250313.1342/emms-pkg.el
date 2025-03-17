;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "emms" "20250313.1342"
  "The Emacs Multimedia System."
  '((cl-lib  "0.5")
    (nadvice "0.3")
    (seq     "0"))
  :url "https://www.gnu.org/software/emms/"
  :commit "3286ac88bf2c306bd66431205eb80ca3bff3e7ef"
  :revdesc "3286ac88bf2c"
  :keywords '("emms" "mp3" "ogg" "flac" "music" "mpeg" "video" "multimedia")
  :authors '(("Jorgen Schäfer" . "forcer@forcix.cx"))
  :maintainers '(("Yoni Rabkin" . "yrk@gnu.org")))
