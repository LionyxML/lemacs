;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "emms" "20250204.2022"
  "The Emacs Multimedia System."
  '((cl-lib  "0.5")
    (nadvice "0.3")
    (seq     "0"))
  :url "https://www.gnu.org/software/emms/"
  :commit "5e9922f2a45fc4b869bd4947540903ea15acfeb2"
  :revdesc "5e9922f2a45f"
  :keywords '("emms" "mp3" "ogg" "flac" "music" "mpeg" "video" "multimedia")
  :authors '(("Jorgen Schäfer" . "forcer@forcix.cx"))
  :maintainers '(("Yoni Rabkin" . "yrk@gnu.org")))
