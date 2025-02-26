;;; handlebars-mode.el --- A major mode for editing Handlebars files.

;; Author: Tony Gentilcore
;;       Chris Wanstrath
;;       Daniel Hackney
;;       Daniel Evans
;;
;; Package-Version: 20150211.1749
;; Package-Revision: 81f6b73fea8f

;; This file is not part of Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; 1) Copy this file somewhere in your Emacs `load-path'.  To see what
;;    your `load-path' is, run inside emacs: C-h v load-path<RET>
;;
;; 2) Add the following to your .emacs file:
;;
;;    (require 'handlebars-mode)

;; The indentation still has minor bugs due to the fact that
;; templates do not require valid HTML.

;; It would be nice to be able to highlight attributes of HTML tags,
;; however this is difficult due to the presence of CTemplate symbols
;; embedded within attributes.

(eval-when-compile
  (require 'font-lock))


(defgroup handlebars nil
  ""
  :group 'languages)

(defface handlebars-mode-section-face
  '((t (:inherit font-lock-keyword-face)))
  ""
  :group 'handlebars)

(defface handlebars-mode-comment-face
  '((t (:inherit font-lock-comment-face)))
  ""
  :group 'handlebars)

(defface handlebars-mode-include-face
  '((t (:inherit font-lock-function-name-face)))
  ""
  :group 'handlebars)

(defface handlebars-mode-builtins-face
  '((t (:inherit font-lock-variable-name-face)))
  ""
  :group 'handlebars)

(defface handlebars-mode-variable-face
  '((t (:inherit font-lock-constant-face)))
  ""
  :group 'handlebars)



(defvar handlebars-mode-version "1.3"
  "Version of `handlebars-mode.el'.")

;; TODO: this keystrokes should be altered to avoid conflict with mustache-mode
(defvar handlebars-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m" 'reindent-then-newline-and-indent)
    (define-key map "\C-ct" 'handlebars-insert-tag)
    (define-key map "\C-cv" 'handlebars-insert-variable)
    (define-key map "\C-cs" 'handlebars-insert-section)
    map)
  "Keymap for handlebars-mode major mode")

(defvar handlebars-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?<  "(>  " st)
    (modify-syntax-entry ?>  ")<  " st)
    (modify-syntax-entry ?\" ".   " st)
    (modify-syntax-entry ?\\ ".   " st)
    (modify-syntax-entry ?'  "w   " st)
    st)
  "Syntax table in use in handlebars-mode buffers.")

(defcustom handlebars-basic-offset 2
  "The basic indentation offset for handlebars."
  :group 'handlebars
  :type 'integer)

;; Constant regular expressions to identify template elements.
(defconst handlebars-mode-handlebars-token "\\([a-zA-Z_.][a-zA-Z0-9_:=\?!.-]*\s+\\)*[a-zA-Z_.][a-zA-Z0-9_:=\?!.-]*")
(defconst handlebars-mode-section (concat "\\({{[#^/]\s*"
                                   handlebars-mode-handlebars-token
                                   "\s*}}\\)"))
(defconst handlebars-mode-open-section (concat "\\({{#\s*"
                                        handlebars-mode-handlebars-token
                                        "\s*}}\\)"))
(defconst handlebars-mode-close-section (concat "{{/\\(\s*"
                                         handlebars-mode-handlebars-token
                                         "\s*\\)}}"))
;; TODO(tonyg) Figure out a way to support multiline comments.
(defconst handlebars-mode-comment "\\({{!.*?}}\\)")
(defconst handlebars-mode-include (concat "\\({{[><]\s*"
                                   handlebars-mode-handlebars-token
                                   "\s*}}\\)"))
(defconst handlebars-mode-variable (concat "\\({{\s*"
                                    handlebars-mode-handlebars-token
                                    "\s*}}\\)"))
(defconst handlebars-mode-variable (concat "\\({{{?\s*"
                                    handlebars-mode-handlebars-token
                                    "\s*}}}?\\)"))
(defconst handlebars-mode-else (concat "\\({{\s*else\s*}}\\)"))
(defconst handlebars-mode-variable-path (concat "\\({{\s*./\s*"
                                    handlebars-mode-handlebars-token
                                    "\s*}}\\)"))
(defconst handlebars-mode-variable-path-parent (concat "\\({{\s*../\s*"
                                    handlebars-mode-handlebars-token
                                    "\s*}}\\)"))
(defconst handlebars-mode-builtins
  (concat
   "\\({{\\<\s*"
   (regexp-opt
    '("BI_NEWLINE" "BI_SPACE")
    t)
   "\s*\\>}}\\)"))
(defconst handlebars-mode-close-section-at-start (concat "^[ \t]*?"
                                                  handlebars-mode-close-section))

;; Constant regular expressions to identify html tags.
;; Taken from HTML 4.01 / XHTML 1.0 Reference found at:
;; http://www.w3schools.com/tags/default.asp.
(defconst handlebars-mode-html-constant "\\(&#?[a-z0-9]\\{2,5\\};\\)")
(defconst handlebars-mode-pair-tag
  (concat
   "\\<"
   (regexp-opt
    '("a" "abbr" "acronym" "address" "applet" "area" "b" "bdo"
      "big" "blockquote" "body" "button" "caption" "center" "cite"
      "code" "col" "colgroup" "dd" "del" "dfn" "dif" "div" "dl"
      "dt" "em" "fieldset" "font" "form" "frame" "frameset" "h1"
      "header" "nav" "footer" "section"
      "h2" "h3" "h4" "h5" "h6" "head" "html" "i" "iframe" "ins"
      "kbd" "label" "legend" "li" "link" "map" "menu" "noframes"
      "noscript" "object" "ol" "optgroup" "option" "p" "pre" "q"
      "s" "samp" "script" "select" "small" "span" "strike"
      "strong" "style" "sub" "sup" "table" "tbody" "td" "textarea"
      "tfoot" "th" "thead" "title" "tr" "tt" "u" "ul" "var" "aside")
    t)
   "\\>"))
(defconst handlebars-mode-standalone-tag
  (concat
   "\\<"
   (regexp-opt
    '("base" "br" "hr" "img" "input" "meta" "param")
    t)
   "\\>"))
(defconst handlebars-mode-open-tag (concat "<\\("
                                    handlebars-mode-pair-tag
                                    "\\)"))
(defconst handlebars-mode-close-tag (concat "</\\("
                                     handlebars-mode-pair-tag
                                     "\\)>"))
(defconst handlebars-mode-close-tag-at-start (concat "^[ \t]*?"
                                              handlebars-mode-close-tag))

(defconst handlebars-mode-blank-line "^[ \t]*?$")
(defconst handlebars-mode-else-line "^[ \t]*?{{[ \t]*?else[ \t]*?}}")
(defconst handlebars-mode-dangling-open (concat "\\("
                                         handlebars-mode-open-section
                                         "\\)\\|\\("
                                         handlebars-mode-open-tag
                                         "\\)[^/]*$"))

(defun handlebars-insert-tag (tag)
  "Inserts an HTML tag."
  (interactive "sTag: ")
  (handlebars-indent)
  (insert (concat "<" tag ">"))
  (insert "\n\n")
  (insert (concat "</" tag ">"))
  (handlebars-indent)
  (forward-line -1)
  (handlebars-indent))

(defun handlebars-insert-variable (variable)
  "Inserts a tpl variable."
  (interactive "sVariable: ")
  (insert (concat "{{" variable "}}")))

(defun handlebars-insert-section (section)
  "Inserts a tpl section."
  (interactive "sSection: ")
  (handlebars-indent)
  (insert (concat "{{#" section "}}\n"))
  (insert "\n")
  (insert (concat "{{/" section "}}"))
  (handlebars-indent)
  (forward-line -1)
  (handlebars-indent))

(defun handlebars-indent ()
  "Indent current line"
  ;; Set the point to beginning of line.
  (beginning-of-line)
  ;; If we are at the beginning of the file, indent to 0.
  (if (bobp)
      (indent-line-to 0)
    (let ((tag-stack 1) (close-tag "") (cur-indent 0) (old-pnt (point-marker))
          (close-at-start) (open-token) (dangling-open))
      (progn
        ;; Determine if this is a template line or an html line.
        (if (looking-at "^[ \t]*?{{")
            (setq close-at-start handlebars-mode-close-section-at-start
                  open-token "{{#")
          (setq close-at-start handlebars-mode-close-tag-at-start
                open-token "<"))

        ;; If there is a closing tag at the start of the line, search back
        ;; for its opener and indent to that level.
        (if (looking-at close-at-start)
            (progn
              (save-excursion
                (setq close-tag (match-string 1))
                ;; Keep searching for a match for the close tag until
                ;; the tag-stack is 0.
                (while (and (not (bobp))
                            (> tag-stack 0)
                            (re-search-backward (concat (replace-regexp-in-string "{{#" "{{#?" open-token)
                                                        "\\(/?\\)"
                                                        close-tag) nil t))
                  (if (string-equal (match-string 1) "/")
                      ;; We found another close tag, so increment tag-stack.
                      (setq tag-stack (+ tag-stack 1))
                    ;; We found an open tag, so decrement tag-stack.
                    (setq tag-stack (- tag-stack 1)))
                  (setq cur-indent (current-indentation))))
              (if (> tag-stack 0)
                  (save-excursion
                    (forward-line -1)
                    (setq cur-indent (current-indentation)))))
          ;; This was not a closing tag, so we check if the previous line
          ;; was an opening tag.
          (save-excursion
            ;; Keep moving back until we find a line that is not blank
            (while (progn
                     (forward-line -1)
                     (and (not (bobp)) (looking-at handlebars-mode-blank-line))))
            (setq cur-indent (current-indentation))
            (if (or (re-search-forward handlebars-mode-dangling-open old-pnt t) (looking-at handlebars-mode-else-line))
                (setq cur-indent (+ cur-indent handlebars-basic-offset)))))

        ;; Reduce the indentation by one level if it is an else tag.
        (if (looking-at handlebars-mode-else-line)
            (setq cur-indent (- cur-indent handlebars-basic-offset)))

        ;; Finally, we execute the actual indentation.
        (if (> cur-indent 0)
            (indent-line-to cur-indent)
          (indent-line-to 0))))))

(defconst handlebars-mode-font-lock-keywords
  `((,handlebars-mode-section (1 'handlebars-mode-section-face))
    (,handlebars-mode-else (1 'handlebars-mode-section-face))
    (,handlebars-mode-comment (1 'handlebars-mode-comment-face))
    (,handlebars-mode-include (1 'handlebars-mode-include-face))
    (,handlebars-mode-builtins (1 'handlebars-mode-builtins-face))
    (,handlebars-mode-variable (1 font-lock-constant-face))
    (,handlebars-mode-variable-path (1 font-lock-constant-face))
    (,handlebars-mode-variable-path-parent (1 font-lock-constant-face))
    (,(concat "</?\\(" handlebars-mode-pair-tag "\\)") (1 font-lock-function-name-face))
    (,(concat "<\\(" handlebars-mode-standalone-tag "\\)") (1 font-lock-function-name-face))
    (,handlebars-mode-html-constant (1 font-lock-variable-name-face))))

;;;###autoload
(define-derived-mode handlebars-mode fundamental-mode "Handlebars"
  (set (make-local-variable 'indent-line-function) 'handlebars-indent)
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'comment-start) "{{!")
  (set (make-local-variable 'comment-end) "}}")
  (set (make-local-variable 'font-lock-defaults) '(handlebars-mode-font-lock-keywords)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.handlebars$" . handlebars-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.hbs$" . handlebars-mode))

(provide 'handlebars-mode)

;;; handlebars-mode.el ends here
