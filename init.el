;;; init.el --- LEmacs (Lionyx Emacs) -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; Emacs init file responsible for either loading a pre-compiled configuration
;; file or tangling and loading a literate org configuration file.
;;
;;; Code:
;; Don't attempt to find/apply special file handlers to files loaded during
;; startup.
(let ((file-name-handler-alist nil))
  ;; If config is pre-compiled, then load that
  (if (file-exists-p (expand-file-name "lemacs-init.elc" user-emacs-directory))
      (load-file (expand-file-name "lemacs-init.elc" user-emacs-directory))
    ;; Otherwise use org-babel to tangle and load the configuration. Also
    ;; compiles it and deletes the 'redundant' .el file.
    (require 'org)
    (org-babel-load-file (expand-file-name "lemacs-init.org" user-emacs-directory))
    (byte-compile-file (expand-file-name "lemacs-init.el" user-emacs-directory))
    (delete-file (expand-file-name "lemacs-init.el" user-emacs-directory))))

;;; init.el ends here
