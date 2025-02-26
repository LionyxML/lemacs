;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "forge" "20250223.1744"
  "Access Git forges from Magit."
  '((emacs         "29.1")
    (compat        "30.0.2.0")
    (closql        "2.2.0")
    (emacsql       "4.1.0")
    (ghub          "4.2.0")
    (let-alist     "1.0.6")
    (llama         "0.6.0")
    (magit         "4.3.0")
    (markdown-mode "2.6")
    (seq           "2.24")
    (transient     "0.8.4")
    (yaml          "1.0.0"))
  :url "https://github.com/magit/forge"
  :commit "29bf441b391ffc59a28fa9e820236df2e182b4ee"
  :revdesc "29bf441b391f"
  :keywords '("git" "tools" "vc")
  :authors '(("Jonas Bernoulli" . "emacs.forge@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.forge@jonas.bernoulli.dev")))
