;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "geiser-guile" "20240920.35"
  "Guile and Geiser talk to each other."
  '((emacs     "26.1")
    (transient "0.3")
    (geiser    "0.28.1"))
  :url "https://gitlab.com/emacs-geiser/guile"
  :commit "a0f111f8dedd31c593c4ed12c0b99745f3c1340f"
  :revdesc "a0f111f8dedd"
  :keywords '("languages" "guile" "scheme" "geiser")
  :authors '(("Jose Antonio Ortega Ruiz" . "(jao@gnu.org)"))
  :maintainers '(("Jose Antonio Ortega Ruiz" . "(jao@gnu.org)")))
