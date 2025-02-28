;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "evil" "20250227.1831"
  "Extensible vi layer."
  '((emacs    "24.1")
    (cl-lib   "0.5")
    (goto-chg "1.6")
    (nadvice  "0.3"))
  :url "https://github.com/emacs-evil/evil"
  :commit "ad3e95f6e3253ddf2d33377ebbff7c82082ab75a"
  :revdesc "ad3e95f6e325"
  :keywords '("emulations")
  :maintainers '(("Tom Dalziel" . "tom.dalziel@gmail.com")))
