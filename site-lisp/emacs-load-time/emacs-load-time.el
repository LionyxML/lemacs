;;; emacs-load-time.el --- Leave a trace of loaded packages (with timing)

;; Copyright (C) 2013-2014 Fabrice Niessen

;; Author: Fabrice Niessen <(concat "fniessen" at-sign "pirilampo.org")>
;; URL: https://github.com/fniessen/emacs-load-time
;; Version: 20140923.1104
;; Keywords: emacs, load time, packages, tree, performance

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package leaves traces in your *Messages* buffer of any package required
;; or loaded, with how much time it took to load it.
;;
;; To get more debug info about the packages being loaded, put the following
;; quite at the beginning your Emacs configuration file:
;;
;;     (require 'emacs-load-time)

;;; Code:

;;---------------------------------------------------------------------------
;; user-configurable variables

(defvar elt-verbose t
  "If non-nil, means show messages describing loaded packages.")

;;---------------------------------------------------------------------------
;; internal variables

(defvar elt-missing-packages nil
  "List of packages that `try-require' or `locate-library' can't find.")

(defconst elt-require-depth 0
  "Starting depth for emacs-load-time tree.")

;;---------------------------------------------------------------------------
;; commands

;; require a feature/library if available; if not, fail silently
(defun try-require (feature)
  "Attempt to load a feature or library.

Return true if the library given as argument is successfully loaded. If not,
instead of an error, just add the package to a list of missing packages."
  (let ((prefix (concat (make-string (* 4 elt-require-depth) ? ) "    ")))
    (condition-case err
        (progn                          ; protected form
          (if (stringp feature)
              (load-library feature)
            (require feature))
          t)                            ; necessary for correct behavior in
                                        ; conditional expressions
      (file-error
       (progn
         (when elt-verbose
           (message "%sRequiring %s <from %s>... missing"
                    prefix feature
                    (ignore-errors (file-name-base load-file-name))))
         (add-to-list 'elt-missing-packages feature 'append))
       nil))))

(when elt-verbose

  (defadvice locate-library (around leuven-locate-library activate)
    "Locate Emacs library named LIBRARY and report time spent."
    (let ((filename (ad-get-arg 0))
          (time-start (float-time))
          (prefix (concat (make-string (* 4 elt-require-depth) ? ) "    ")))
      (if ad-do-it
          (message "%sLocating library %s... located (in %.3f s)"
                   prefix filename
                   (- (float-time) time-start))
        (add-to-list 'elt-missing-packages filename 'append)
        (message "%sLocating library %s... missing (in %.3f s)"
                 prefix filename
                 (- (float-time) time-start)))))

  (defadvice require (around leuven-require activate)
    "Leave a trace of packages being loaded."
    (let* ((feature (ad-get-arg 0))
           (prefix-already (concat (make-string (* 4 elt-require-depth) ? ) "└── "))
           (prefix-open    (concat (make-string (* 4 elt-require-depth) ? ) "└─► "))
           (prefix-close   (concat (make-string (* 4 elt-require-depth) ? ) "  ► ")))
      (cond ((featurep feature)
             (message "%s%s <from %s>... already loaded"
                      prefix-already feature
                      (ignore-errors (file-name-base load-file-name))
                      )
             (setq ad-return-value feature)) ; set the return value in the case
                                             ; `ad-do-it' is not called
            (t
             (let ((time-start))
               (ad-disable-advice 'locate-library 'around 'leuven-locate-library)
               (message "%s%s <from %s>... %s"
                        prefix-open feature
                        (ignore-errors (file-name-base load-file-name))
                        (locate-library (symbol-name feature)))
               (ad-activate 'locate-library)
               (setq time-start (float-time))
               (let ((elt-require-depth (1+ elt-require-depth)))
                 ad-do-it)
               (message "%s%s <from %s>... loaded in %.3f s"
                        prefix-close feature
                        (ignore-errors (file-name-base load-file-name))
                        (- (float-time) time-start)))))))

  (defadvice load (around leuven-load activate)
     "Execute a file of Lisp code named FILE and report time spent."
     (let ((filename (ad-get-arg 0))
           (time-start (float-time))
           (prefix-open  (concat (make-string (* 4 elt-require-depth) ? ) "└─● "))
           (prefix-close (concat (make-string (* 4 elt-require-depth) ? ) "  ● ")))
       (ad-disable-advice 'locate-library 'around 'leuven-locate-library)
       (message "%s%s <from %s>...%s"
                prefix-open filename
                (ignore-errors (file-name-base load-file-name))
                (ignore-errors
                  (if (not (string-match-p
                            (concat "^" (expand-file-name filename)
                                    "\\.?e?l?c?")
                            (locate-library filename)))
                      (concat " " (locate-library filename))
                    "")))               ; don't print full file name once again!
       (ad-activate 'locate-library)
       ad-do-it
       (message "%s%s <from %s>... loaded in %.3f s"
                prefix-close filename
                (ignore-errors (file-name-base load-file-name))
                (- (float-time) time-start))))

  ;; wrapper around `eval-after-load' (added in GNU Emacs 24.4)
  (defmacro with-eval-after-load (file &rest body)
    "Execute BODY after FILE is loaded."
    (declare (indent 1) (debug t))
    `(eval-after-load ,file
       '(progn
          (let ((time-start))
            (message "{{{ Running code block specific to %s..."
                     ,file)
            (setq time-start (float-time))
            ,@body
            (message "}}} Running code block specific to %s... done in %.3f s"
                     ,file
                     (- (float-time) time-start)))))))

(add-hook 'after-init-hook
          `(lambda ()
             ;; warn that some packages were missing
             (when elt-verbose
               (dolist (pkg elt-missing-packages)
                 (message "(warning) Package %s not found" pkg))))
             t)

;;---------------------------------------------------------------------------
;; that's it

(provide 'emacs-load-time)

;; Local Variables:
;; time-stamp-format: "%:y%02m%02d.%02H%02M"
;; time-stamp-start: "Version: "
;; time-stamp-end: "$"
;; no-byte-compile: t
;; End:

;;; emacs-load-time.el ends here
