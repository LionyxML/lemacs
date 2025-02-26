;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "evil" "20250121.1800"
  "Extensible vi layer."
  '((emacs    "24.1")
    (cl-lib   "0.5")
    (goto-chg "1.6")
    (nadvice  "0.3"))
  :url "https://github.com/emacs-evil/evil"
  :commit "c222ce1ea6fbefaed08308c061371ebdf01b078f"
  :revdesc "c222ce1ea6fb"
  :keywords '("emulations")
  :maintainers '(("Tom Dalziel" . "tom.dalziel@gmail.com")))
