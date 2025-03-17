;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "evil" "20250302.655"
  "Extensible vi layer."
  '((emacs    "24.1")
    (cl-lib   "0.5")
    (goto-chg "1.6")
    (nadvice  "0.3"))
  :url "https://github.com/emacs-evil/evil"
  :commit "ac620beace5f28fbf40cd69765975bf6e915c01c"
  :revdesc "ac620beace5f"
  :keywords '("emulations")
  :maintainers '(("Tom Dalziel" . "tom.dalziel@gmail.com")))
