;;; ellama.el --- Tool for interacting with LLMs -*- lexical-binding: t -*-

;; Copyright (C) 2023-2025  Free Software Foundation, Inc.

;; Author: Sergey Kostyaev <sskostyaev@gmail.com>
;; URL: http://github.com/s-kostyaev/ellama
;; Keywords: help local tools
;; Package-Requires: ((emacs "28.1") (llm "0.22.0") (plz "0.8") (transient "0.7") (compat "29.1"))
;; Package-Version: 20250224.417
;; Package-Revision: d9456fdc4b6f
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Created: 8th Oct 2023

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Ellama is a tool for interacting with large language models from Emacs.
;; It allows you to ask questions and receive responses from the
;; LLMs.  Ellama can perform various tasks such as translation, code
;; review, summarization, enhancing grammar/spelling or wording and
;; more through the Emacs interface.  Ellama natively supports streaming
;; output, making it effortless to use with your preferred text editor.
;;

;;; Code:

(require 'eieio)
(require 'llm)
(require 'llm-provider-utils)
(require 'transient)
(require 'compat)
(eval-when-compile (require 'rx))

(defgroup ellama nil
  "Tool for interacting with LLMs."
  :group 'tools)

(defcustom ellama-user-nick "User"
  "User nick in logs."
  :group 'ellama
  :type 'string)

(defcustom ellama-assistant-nick "Ellama"
  "Assistant nick in logs."
  :group 'ellama
  :type 'string)

(defcustom ellama-nick-prefix-depth 2
  "Prefix depth."
  :group 'ellama
  :type 'integer)

(defcustom ellama-language "English"
  "Language for ellama translation."
  :group 'ellama
  :type 'string)

(defcustom ellama-provider nil
  "Backend LLM provider."
  :group 'ellama
  :type '(sexp :validate llm-standard-provider-p))

(defcustom ellama-session-remove-reasoning t
  "Remove internal reasoning from the session after ellama provide an answer.
This can improve long-term communication with reasoning models."
  :group 'ellama
  :type 'boolean)

(defcustom ellama-output-remove-reasoning t
  "Remove internal reasoning from ellama output.
Make reasoning models more useful for many cases."
  :group 'ellama
  :type 'boolean)

(defcustom ellama-session-hide-org-quotes t
  "Hide org quotes in ellama session buffer."
  :group 'ellama
  :type 'boolean)

(defcustom ellama-chat-translation-enabled nil
  "Enable chat translations."
  :group 'ellama
  :type 'boolean)

(defcustom ellama-translation-provider nil
  "LLM provider for chat translation."
  :group 'ellama
  :type '(sexp :validate llm-standard-provider-p))

(defcustom ellama-summarization-provider nil
  "LLM provider for summarization."
  :group 'ellama
  :type '(sexp :validate llm-standard-provider-p))

(defcustom ellama-coding-provider nil
  "LLM provider for coding tasks."
  :group 'ellama
  :type '(sexp :validate llm-standard-provider-p))

(defcustom ellama-providers nil
  "LLM provider list for fast switching."
  :group 'ellama
  :type '(alist :key-type string
		:value-type (sexp :validate llm-standard-provider-p)))

(defvar spinner-types)

(defcustom ellama-spinner-type 'progress-bar
  "Spinner type for ellama."
  :group 'ellama
  :type `(choice ,@(if (boundp 'spinner-types)
		       (mapcar
			(lambda (type)
			  `(const ,(car type)))
			spinner-types)
		     '(const progress-bar))))

(defcustom ellama-spinner-enabled nil
  "Enable spinner during text generation."
  :group 'ellama
  :type 'boolean)

(defcustom ellama-context-line-always-visible nil
  "Make context header or mode line always visible, even with empty context."
  :group 'ellama
  :type 'boolean)

(defcustom ellama-command-map
  (let ((map (make-sparse-keymap)))
    ;; code
    (define-key map (kbd "c c") 'ellama-code-complete)
    (define-key map (kbd "c a") 'ellama-code-add)
    (define-key map (kbd "c e") 'ellama-code-edit)
    (define-key map (kbd "c i") 'ellama-code-improve)
    (define-key map (kbd "c r") 'ellama-code-review)
    (define-key map (kbd "c m") 'ellama-generate-commit-message)
    ;; summarize
    (define-key map (kbd "s s") 'ellama-summarize)
    (define-key map (kbd "s w") 'ellama-summarize-webpage)
    (define-key map (kbd "s c") 'ellama-summarize-killring)
    ;; session
    (define-key map (kbd "s l") 'ellama-load-session)
    (define-key map (kbd "s r") 'ellama-session-rename)
    (define-key map (kbd "s d") 'ellama-session-delete)
    (define-key map (kbd "s a") 'ellama-session-switch)
    ;; improve
    (define-key map (kbd "i w") 'ellama-improve-wording)
    (define-key map (kbd "i g") 'ellama-improve-grammar)
    (define-key map (kbd "i c") 'ellama-improve-conciseness)
    (define-key map (kbd "P") 'ellama-proofread)
    ;; make
    (define-key map (kbd "m l") 'ellama-make-list)
    (define-key map (kbd "m t") 'ellama-make-table)
    (define-key map (kbd "m f") 'ellama-make-format)
    ;; ask
    (define-key map (kbd "a a") 'ellama-ask-about)
    (define-key map (kbd "a i") 'ellama-chat)
    (define-key map (kbd "a l") 'ellama-ask-line)
    (define-key map (kbd "a s") 'ellama-ask-selection)
    ;; text
    (define-key map (kbd "w") 'ellama-write)
    (define-key map (kbd "t t") 'ellama-translate)
    (define-key map (kbd "t b") 'ellama-translate-buffer)
    (define-key map (kbd "t c") 'ellama-complete)
    (define-key map (kbd "t e") 'ellama-chat-translation-enable)
    (define-key map (kbd "t d") 'ellama-chat-translation-disable)
    ;; define
    (define-key map (kbd "d w") 'ellama-define-word)
    ;; context
    (define-key map (kbd "x b") 'ellama-context-add-buffer)
    (define-key map (kbd "x d") 'ellama-context-add-directory)
    (define-key map (kbd "x f") 'ellama-context-add-file)
    (define-key map (kbd "x s") 'ellama-context-add-selection)
    (define-key map (kbd "x i") 'ellama-context-add-info-node)
    (define-key map (kbd "x m") 'ellama-manage-context)
    (define-key map (kbd "x r") 'ellama-context-reset)
    ;; provider
    (define-key map (kbd "p s") 'ellama-provider-select)
    map)
  "Keymap for ellama commands."
  :group 'ellama
  :type 'keymap)

(defun ellama-setup-keymap ()
  "Set up the Ellama keymap and bindings."
  (interactive)
  (when (boundp 'ellama-keymap-prefix)
    (defvar ellama-keymap (make-sparse-keymap)
      "Keymap for Ellama Commands")

    (when ellama-keymap-prefix
      (define-key global-map (kbd ellama-keymap-prefix) ellama-command-map))))

(defcustom ellama-keymap-prefix nil
  "Key sequence for Ellama Commands."
  :type 'string
  :set (lambda (symbol value)
	 (custom-set-default symbol value)
	 (when value
	   (ellama-setup-keymap)))
  :group 'ellama)

(defcustom ellama-ollama-binary "ollama"
  "Path to ollama binary."
  :type 'string
  :group 'ellama)

(defcustom ellama-auto-scroll nil
  "If enabled ellama buffer will scroll automatically during generation."
  :type 'boolean
  :group 'ellama)

(defcustom ellama-fill-paragraphs '(text-mode)
  "When to wrap paragraphs."
  :group 'ellama
  :type `(choice
          (const :tag "Never fill paragraphs" nil)
          (const :tag "Always fill paragraphs" t)
          (function :tag "By predicate")
          (repeat :tag "In specific modes" (symbol))))

(defcustom ellama-name-prompt-words-count 5
  "Count of words in prompt to generate name."
  :group 'ellama
  :type 'integer)

(defcustom ellama-naming-scheme 'ellama-generate-name-by-words
  "How to name sessions.
If you choose custom function, that function should accept PROVIDER, ACTION
and PROMPT arguments.

PROVIDER is an llm provider.

ACTION is a symbol, current command.

PROMPT is a prompt string."
  :group 'ellama
  :type `(choice
          (const :tag "By first N words of prompt" ellama-generate-name-by-words)
          (const :tag "By current time" ellama-generate-name-by-time)
	  (const :tag "By generating name with LLM based on prompt." ellama-generate-name-by-llm)
	  (const :tag "By generating name with reasoning LLM based on prompt." ellama-generate-name-by-reasoning-llm)
          (function :tag "By custom function")))

(defcustom ellama-define-word-prompt-template "Define %s"
  "Prompt template for `ellama-define-word'."
  :group 'ellama
  :type 'string)

(defcustom ellama-summarize-prompt-template "<INSTRUCTIONS>
You are a summarizer. You write a summary of the input **IN THE SAME
LANGUAGE AS ORIGINAL INPUT TEXT**. Summarize input text concisely and
comprehensively, ensuring all key details are included accurately.
Focus on clarity and maintain a straightforward presentation.
</INSTRUCTIONS>
<INPUT>
%s
</INPUT>"
  "Prompt template for `ellama-summarize'."
  :group 'ellama
  :type 'string)

(defcustom ellama-code-review-prompt-template "You are professional software engineer. Review provided code and make concise suggestions."
  "Prompt template for `ellama-code-review'."
  :group 'ellama
  :type 'string)

(defcustom ellama-change-prompt-template "Change the following text, %s, just output the final text without additional quotes around it:\n%s"
  "Prompt template for `ellama-change'."
  :group 'ellama
  :type 'string)

(defcustom ellama-write-prompt-template "<SYSTEM>
Write text, based on provided context and instruction. Do not add any explanation or acknowledgement, just follow instruction.
</SYSTEM>
<INSTRUCTION>
%s
</INSTRUCTION>"
  "Prompt template for `ellama-write'."
  :group 'ellama
  :type 'string)

(defcustom ellama-improve-grammar-prompt-template "improve grammar and spelling"
  "Prompt template for `ellama-improve-grammar'."
  :group 'ellama
  :type 'string)

(defcustom ellama-improve-wording-prompt-template "use better wording"
  "Prompt template for `ellama-improve-wording'."
  :group 'ellama
  :type 'string)

(defcustom ellama-proofread-prompt-template "proofread"
  "Prompt template for `ellama-proofread'."
  :group 'ellama
  :type 'string)

(defcustom ellama-improve-conciseness-prompt-template "make it as simple and concise as possible"
  "Prompt template for `ellama-improve-conciseness'."
  :group 'ellama
  :type 'string)

(defcustom ellama-code-edit-prompt-template "Regarding the following code, %s, only output the result code in format ```language\n...\n```:\n```\n%s\n```\nWrite all the code in single code block."
  "Prompt template for `ellama-code-edit'."
  :group 'ellama
  :type 'string)

(defcustom ellama-code-improve-prompt-template "Enhance the following code, only output the result code in format ```language\n...\n```:\n```\n%s\n```\nWrite all the code in single code block."
  "Prompt template for `ellama-code-improve'."
  :group 'ellama
  :type 'string)

(defcustom ellama-code-complete-prompt-template "Continue the following code, only write new code in format ```language\n...\n```:\n```\n%s\n```\nWrite all the code in single code block."
  "Prompt template for `ellama-code-complete'."
  :group 'ellama
  :type 'string)

(defcustom ellama-code-add-prompt-template "Based on context, %s, only output the result in format ```\n...\n```\nWrite all the code in single code block."
  "Prompt template for `ellama-code-add'."
  :group 'ellama
  :type 'string)

(defcustom ellama-generate-commit-message-template "<INSTRUCTIONS>
You are professional software developer.

Write concise commit message based on diff in the following format:
<FORMAT>
First line should contain short title described major change in functionality.
Then one empty line. Then detailed description of all changes.
</FORMAT>
<EXAMPLE>
Improve abc

Improved abc feature by adding new xyz module.
</EXAMPLE>

**Reply with commit message only without any quotes.**
</INSTRUCTIONS>

<DIFF>
%s
</DIFF>"
  "Prompt template for `ellama-generate-commit-message'."
  :group 'ellama
  :type 'string)

(defcustom ellama-make-format-prompt-template "Render the following text as a %s:\n%s"
  "Prompt template for `ellama-make-format'."
  :group 'ellama
  :type 'string)

(defcustom ellama-make-list-prompt-template "markdown list"
  "Prompt template for `ellama-make-list'."
  :group 'ellama
  :type 'string)

(defcustom ellama-make-table-prompt-template "markdown table"
  "Prompt template for `ellama-make-table'."
  :group 'ellama
  :type 'string)

(defcustom ellama-get-name-template "I will get you user query, you should return short topic only, what this conversation about. NEVER respond to query itself. Topic must be short and concise. Do not add additional words like 'the topic is', respond with topic only.
<example>
Query: Why is sky blue?
Topic: Blue sky
</example>
<query>
%s
</query>
Topic:
"
  "Prompt template for `ellama-get-name'."
  :group 'ellama
  :type 'string)

(defcustom ellama-translation-template "<INSTRUCTIONS>
You are expert text translator. Translate input text to %s. Do
not explain what you are doing. Do not self reference. You are an
expert translator that will be tasked with translating and
improving the spelling/grammar/literary quality of a piece of
text. Please rewrite the translated text in your tone of voice
and writing style. Ensure that the meaning of the original text
is not changed.
</INSTRUCTIONS>
<INPUT>
%s
</INPUT>"
  "Translation template."
  :group 'ellama
  :type 'string)

(defcustom ellama-extract-string-list-template "You are professional data extractor. Extract %s as json array of strings
<EXAMPLE>
{\"data\":[\"First element\", \"Second element\"]}
</EXAMPLE>"
  "Extract string list template."
  :group 'ellama
  :type 'string)

(defcustom ellama-semantic-identity-template "Determine if two texts have the same meaning. If they are similar but differ in key aspects, they are not the same. Return the answer as a JSON object.
<TEXT_1>
%s
</TEXT_1>
<TEXT_2>
%s
</TEXT_2>
<EXAMPLE>
{
  \"think\": \"Think if texts have same meaning\",
  \"same\": true
}
</EXAMPLE>"
  "Extract string list template."
  :group 'ellama
  :type 'string)

(defcustom ellama-semantic-identity-reasoning-template "Determine if two texts have the same meaning. If they are similar but differ in key aspects, they are not the same. Return the answer as a JSON object.
<CONTEXT>
%s
</CONTEXT>
<TEXT_1>
%s
</TEXT_1>
<TEXT_2>
%s
</TEXT_2>
<EXAMPLE>
{
  \"think\": \"Think if texts have same meaning in provided context\",
  \"same\": true
}
</EXAMPLE>"
  "Extract string list template with context and reasoning."
  :group 'ellama
  :type 'string)

(defcustom ellama-extraction-provider nil
  "LLM provider for data extraction."
  :group 'ellama
  :type '(sexp :validate llm-standard-provider-p))

(defcustom ellama-chat-done-callback nil
  "Callback that will be called on ellama chat response generation done.
It should be a function with single argument generated text string."
  :group 'ellama
  :type 'function)

(defcustom ellama-major-mode 'org-mode
  "Major mode for ellama commands."
  :group 'ellama
  :type 'symbol)

(defcustom ellama-translate-italic t
  "Translate italic during markdown to org transformations."
  :group 'ellama
  :type 'boolean)

(defcustom ellama-session-auto-save t
  "Automatically save ellama sessions if set."
  :group 'ellama
  :type 'boolean)

(defcustom ellama-show-quotes nil
  "Show quotes in chat context."
  :group 'ellama
  :type 'boolean)

(defcustom ellama-chat-display-action-function nil
  "Display action function for `ellama-chat'."
  :group 'ellama
  :type 'function)

(defcustom ellama-instant-display-action-function nil
  "Display action function for `ellama-instant'."
  :group 'ellama
  :type 'function)

(define-minor-mode ellama-session-mode
  "Minor mode for ellama session buffers."
  :interactive nil
  (if ellama-session-mode
      (progn
        (add-hook 'after-save-hook 'ellama--save-session nil t)
        (add-hook 'kill-buffer-hook 'ellama--session-deactivate nil t))
    (remove-hook 'kill-buffer-hook 'ellama--session-deactivate)
    (remove-hook 'after-save-hook 'ellama--save-session)
    (ellama--session-deactivate)))

(define-minor-mode ellama-request-mode
  "Minor mode for ellama buffers with active request to llm."
  :interactive nil
  :lighter " ellama:generating"
  :keymap '(([remap keyboard-quit] . ellama--cancel-current-request-and-quit))
  (if ellama-request-mode
      (add-hook 'kill-buffer-hook 'ellama--cancel-current-request nil t)
    (remove-hook 'kill-buffer-hook 'ellama--cancel-current-request)
    (ellama--cancel-current-request)))

(defvar-local ellama--change-group nil)

(defvar-local ellama--current-request nil)

(defconst ellama--code-prefix
  (rx (minimal-match
       (zero-or-more anything) (literal "```") (zero-or-more anything) (+ (or "\n" "\r")))))

(defconst ellama--code-suffix
  (rx (minimal-match
       (literal "```") (zero-or-more anything))))

(defun ellama--code-filter (text)
  "Filter code prefix/suffix from TEXT."
  ;; Trim left first as `string-trim' trims from the right and ends up deleting all the code.
  (string-trim-right (string-trim-left text ellama--code-prefix) ellama--code-suffix))

(defun ellama--fill-long-lines (text)
  "Fill long lines only in TEXT."
  (if ellama-fill-paragraphs
      (with-temp-buffer
	(insert (propertize text 'hard t))
	(let ((use-hard-newlines t))
	  (fill-region (point-min) (point-max) nil t t))
	(buffer-substring-no-properties (point-min) (point-max)))
    text))

(defun ellama--replace-first-begin-src (text)
  "Replace first begin src in TEXT."
  (if (not (string-match-p (rx (literal "#+BEGIN_SRC")) text))
      (replace-regexp-in-string "^[[:space:]]*```\\(\\(.\\|\n\\)*\\)" "#+BEGIN_SRC\\1" text)
    text))

(defun ellama--replace-bad-code-blocks (text)
  "Replace code src blocks in TEXT."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    ;; skip good code blocks
    (while (re-search-forward "#\\+BEGIN_SRC\\(.\\|\n\\)*?#\\+END_SRC" nil t))
    (while (re-search-forward "#\\+END_SRC\\(\\(.\\|\n\\)*?\\)#\\+END_SRC" nil t)
      (replace-match "#+BEGIN_SRC\\1#+END_SRC"))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun ellama--replace (from to beg end)
  "Replace FROM to TO in region BEG END."
  (goto-char beg)
  (while (and
	  (> end beg)
	  (re-search-forward from end t))
    (replace-match to)))

(defun ellama--apply-transformations (beg end)
  "Apply md to org transformations for region BEG END."
  ;; headings
  (ellama--replace "^# " "* " beg end)
  (ellama--replace "^## " "** " beg end)
  (ellama--replace "^### " "*** " beg end)
  (ellama--replace "^#### " "**** " beg end)
  (ellama--replace "^##### " "***** " beg end)
  (ellama--replace "^###### " "****** " beg end)
  ;; bold
  (ellama--replace "__\\(.+?\\)__" "*\\1*" beg end)
  (ellama--replace "\\*\\*\\(.+?\\)\\*\\*" "*\\1*" beg end)
  (ellama--replace "<b>\\(.+?\\)</b>" "*\\1*" beg end)
  ;; italic
  (when ellama-translate-italic
    (ellama--replace "_\\(.+?\\)_" "/\\1/" beg end))
  (ellama--replace "<i>\\(.+?\\)</i>" "/\\1/" beg end)
  ;; underlined
  (ellama--replace "<u>\\(.+?\\)</u>" "_\\1_" beg end)
  ;; inline code
  (ellama--replace "`\\(.+?\\)`" "~\\1~" beg end)
  ;; lists
  (ellama--replace "^\\* " "+ " beg end)
  ;; strikethrough
  (ellama--replace "~~\\(.+?\\)~~" "+\\1+" beg end)
  (ellama--replace "<s>\\(.+?\\)</s>" "+\\1+" beg end)
  ;; badges
  (ellama--replace "\\[\\!\\[.*?\\](\\(.*?\\))\\](\\(.*?\\))" "[[\\2][file:\\1]]" beg end)
  ;;links
  (ellama--replace "\\[\\(.*?\\)\\](\\(.*?\\))" "[[\\2][\\1]]" beg end)

  ;; filling long lines
  (goto-char beg)
  (when ellama-fill-paragraphs
    (let ((use-hard-newlines t))
      (fill-region beg end nil t t))))

(defun ellama--replace-outside-of-code-blocks (text)
  "Replace markdown elements in TEXT with org equivalents.
Skip code blocks and math environments."
  (with-temp-buffer
    (insert (propertize text 'hard t))
    (goto-char (point-min))
    (let (block-start
	  block-end
	  (prev-point (point-min)))
      ;; Process regions outside of blocks
      (while (re-search-forward "\\(#\\+BEGIN_SRC\\|\\$\\$\\|\\$\\)" nil t)
        (setq block-start (match-beginning 0))
	(goto-char block-start)
        (let ((block-type (cond ((looking-at "#\\+BEGIN_SRC") 'src)
                                ((looking-at "\\$\\$") 'math-display)
                                ((looking-at "\\$") 'math-inline))))
          ;; Apply transformations to text before the block
          (ellama--apply-transformations prev-point block-start)
          ;; Skip over the block content
          (goto-char block-start)
          (setq block-end
		(cond
		 ((eq block-type 'src)
                  (if (re-search-forward "#\\+END_SRC" nil t) (point) (point-max)))
		 ((eq block-type 'math-display)
                  (if (re-search-forward "\\$\\$.+\\$\\$" nil t) (point) (point-max)))
		 ((eq block-type 'math-inline)
                  (if (re-search-forward "\\$.+\\$" nil t) (point) (point-max)))))
          (when block-end
	    (goto-char block-end))
	  (setq prev-point (point))))
      ;; Process any remaining text after the last block
      (ellama--apply-transformations prev-point (point-max)))
    (prog1
	(buffer-substring-no-properties (point-min) (point-max))
      (kill-buffer (current-buffer)))))

(defun ellama--translate-markdown-to-org-filter (text)
  "Filter to translate code blocks from markdown syntax to org syntax in TEXT.
This filter contains only subset of markdown syntax to be good enough."
  (thread-last
    text
    ;; code blocks
    (replace-regexp-in-string "^[[:space:]]*```\\(.+\\)$" "#+BEGIN_SRC \\1")
    (ellama--replace-first-begin-src)
    (replace-regexp-in-string "^<!-- language: \\(.+\\) -->\n```" "#+BEGIN_SRC \\1")
    (replace-regexp-in-string "^[[:space:]]*```$" "#+END_SRC")
    (replace-regexp-in-string "^[[:space:]]*```" "#+END_SRC\n")
    (replace-regexp-in-string "```" "\n#+END_SRC\n")
    (replace-regexp-in-string "<think>[\n]?" "#+BEGIN_QUOTE\n")
    (replace-regexp-in-string "[\n]?</think>[\n]?" "\n#+END_QUOTE\n")
    (ellama--replace-bad-code-blocks)
    (ellama--replace-outside-of-code-blocks)))

(defcustom ellama-enable-keymap t
  "Enable or disable Ellama keymap."
  :type 'boolean
  :group 'ellama
  :set (lambda (symbol value)
	 (custom-set-default symbol value)
	 (if value
	     (ellama-setup-keymap)
	   ;; If ellama-enable-keymap is nil, remove the key bindings
	   (define-key global-map (kbd ellama-keymap-prefix) nil))))

(defcustom ellama-sessions-directory (file-truename
				      (file-name-concat
				       user-emacs-directory
				       "ellama-sessions"))
  "Directory for saved ellama sessions."
  :type 'string
  :group 'ellama)

(defcustom ellama-naming-provider nil
  "LLM provider for generating names."
  :group 'ellama
  :type '(sexp :validate llm-standard-provider-p))

(defcustom ellama-always-show-chain-steps nil
  "Always show ellama chain buffers."
  :type 'boolean
  :group 'ellama)

(defvar-local ellama--current-session nil)

(defvar ellama--current-session-id nil)

(defvar ellama--active-sessions (make-hash-table :test #'equal))

(cl-defstruct ellama-session
  "A structure represent ellama session.

ID is an unique identifier of session, string.

PROVIDER is an llm provider of session.

FILE is a path to file contains string representation of this session, string.

PROMPT is a variable contains last prompt in this session.

CONTEXT will be ignored.  Use global context instead.

EXTRA contains additional information."
  id provider file prompt context extra)

(defun ellama-get-session-buffer (id)
  "Return ellama session buffer by provided ID."
  (gethash id ellama--active-sessions))

(defconst ellama--forbidden-file-name-characters (rx (any "/\\?%*:|\"<>.;=")))

(defun ellama--fix-file-name (name)
  "Change forbidden characters in the NAME to acceptable."
  (replace-regexp-in-string
   ellama--forbidden-file-name-characters
   "_"
   name))

(defun ellama-generate-name-by-words (provider action prompt)
  "Generate name for ACTION by PROVIDER by getting first N words from PROMPT."
  (let* ((cleaned-prompt (replace-regexp-in-string "/" "_" prompt))
         (prompt-words (split-string cleaned-prompt)))
    (string-join
     (flatten-tree
      (list (split-string (format "%s" action) "-")
	    (seq-take prompt-words ellama-name-prompt-words-count)
	    (if (> (length prompt-words) ellama-name-prompt-words-count)
		"..."
	      nil)
	    (format "(%s)" (llm-name provider))))
     " ")))

(defun ellama-get-name (prompt)
  "Generate session name by LLM based on PROMPT."
  (let ((provider (or ellama-naming-provider ellama-provider)))
    (string-trim-right
     (string-trim
      (seq-first
       (split-string
	(llm-chat provider (llm-make-simple-chat-prompt
			    (format ellama-get-name-template prompt)))
	"\n")))
     "\\.")))

(defun ellama-remove-reasoning (text)
  "Remove R1-like reasoning from TEXT."
  (string-trim (replace-regexp-in-string
		"<think>\\(.\\|\n\\)*</think>"
		""
		text)))

(defun ellama-generate-name-by-llm (provider _action prompt)
  "Generate name for ellama ACTION by PROVIDER and PROMPT by LLM."
  (format "%s (%s)"
	  (ellama-get-name prompt)
	  (llm-name provider)))

(defun ellama-generate-name-by-reasoning-llm (provider _action prompt)
  "Generate name for ellama ACTION by PROVIDER and PROMPT by LLM."
  (format "%s (%s)"
	  (ellama-remove-reasoning
	   (llm-chat (or ellama-naming-provider ellama-provider)
		     (llm-make-simple-chat-prompt
		      (format ellama-get-name-template prompt))))
	  (llm-name provider)))

(defun ellama-get-current-time ()
  "Return string representation of current time."
  (replace-regexp-in-string
   "\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\'" "\\1:\\2"
   (format-time-string "%FT%T%z" (current-time))))

(defun ellama-generate-name-by-time (_provider _action _prompt)
  "Generate name for ellama session by current time."
  (ellama-get-current-time))

(defun ellama-generate-name (provider action prompt)
  "Generate name for ellama ACTION by PROVIDER according to PROMPT."
  (ellama--fix-file-name (funcall ellama-naming-scheme provider action prompt)))

(defun ellama-get-nick-prefix-for-mode ()
  "Return preferred header prefix char based om the current mode.
Defaults to #, but supports `org-mode'.  Depends on `ellama-major-mode'."
  (let* ((prefix-char
          (cond ((provided-mode-derived-p ellama-major-mode 'org-mode) ?*)
                (t ?#))))
    (make-string ellama-nick-prefix-depth prefix-char)))

(defun ellama-get-session-file-extension ()
  "Return file extension based om the current mode.
Defaults to md, but supports org.  Depends on \"ellama-major-mode.\""
  (cond ((provided-mode-derived-p ellama-major-mode 'org-mode) "org")
        (t "md")))

(defvar ellama--global-context nil
  "Global context.")

(defun ellama-new-session (provider prompt &optional ephemeral)
  "Create new ellama session with unique id.
Provided PROVIDER and PROMPT will be used in new session.
If EPHEMERAL non nil new session will not be associated with any file."
  (let* ((name (ellama-generate-name provider 'ellama prompt))
	 (count 1)
	 (name-with-suffix (format "%s %d" name count))
	 (id (if (not (ellama-get-session-buffer name))
		 name
	       (while (ellama-get-session-buffer name-with-suffix)
		 (setq count (+ count 1))
		 (setq name-with-suffix (format "%s %d" name count)))
	       name-with-suffix))
	 (file-name (when (and (not ephemeral)
			       ellama-session-auto-save)
		      (file-name-concat
		       ellama-sessions-directory
		       (concat id "." (ellama-get-session-file-extension)))))
	 (session (make-ellama-session
		   :id id :provider provider :file file-name))
	 (buffer (if file-name
		     (progn
		       (make-directory ellama-sessions-directory t)
		       (find-file-noselect file-name))
		   (get-buffer-create id))))
    (setq ellama--current-session-id id)
    (puthash id buffer ellama--active-sessions)
    (with-current-buffer buffer
      (funcall ellama-major-mode)
      (setq ellama--current-session session)
      (ellama-session-mode +1))
    session))

(defun ellama--cancel-current-request ()
  "Cancel current running request."
  (declare-function spinner-stop "ext:spinner")
  (when ellama--current-request
    (llm-cancel-request ellama--current-request)
    (when ellama-spinner-enabled
      (require 'spinner)
      (spinner-stop))
    (setq ellama--current-request nil)))

(defun ellama--cancel-current-request-and-quit ()
  "Cancel the current request and quit."
  (interactive)
  (ellama--cancel-current-request)
  (ellama-request-mode -1)
  (keyboard-quit))

(defun ellama--session-deactivate ()
  "Deactivate current session."
  (ellama--cancel-current-request)
  (when-let* ((session ellama--current-session)
              (id (ellama-session-id session)))
    (when (string= (buffer-name)
                   (buffer-name (ellama-get-session-buffer id)))
      (remhash id ellama--active-sessions)
      (when (equal ellama--current-session-id id)
	(setq ellama--current-session-id nil)))))

(defun ellama--get-session-file-name (file-name)
  "Get ellama session file name for FILE-NAME."
  (let* ((base-name (file-name-nondirectory file-name))
	 (dir (file-name-directory file-name))
	 (session-file-name
	  (file-name-concat
	   dir
	   (concat "." base-name ".session.el"))))
    session-file-name))

(defun ellama--get-translation-file-name (file-name)
  "Get ellama translation file name for FILE-NAME."
  (let* ((base-name (file-name-base file-name))
	 (ext (file-name-extension file-name))
	 (dir (file-name-directory file-name))
	 (translation-file-name
	  (file-name-concat
	   dir
	   (concat base-name ".translation"
		   (when ext
		     (concat "." ext))))))
    translation-file-name))

(defun ellama--save-session ()
  "Save current ellama session."
  (when ellama--current-session
    (let* ((session ellama--current-session)
	   (file-name (ellama-session-file session))
	   (session-file-name (ellama--get-session-file-name file-name)))
      (with-temp-file session-file-name
	(insert (prin1-to-string session))))))

;;;###autoload
(defun ellama-load-session ()
  "Load ellama session from file."
  (interactive)
  (when-let* ((dir (if current-prefix-arg
		       (read-directory-name
			"Select directory containing sessions: "
			ellama-sessions-directory)
		     ellama-sessions-directory))
	      (file-name (file-name-concat
			  ellama-sessions-directory
			  (completing-read
			   "Select session to load: "
			   (directory-files
			    ellama-sessions-directory nil "^[^\.].*"))))
	      (session-file-name (ellama--get-session-file-name file-name))
	      (session-file-exists (file-exists-p session-file-name))
	      (buffer (find-file-noselect file-name))
	      (session-buffer (find-file-noselect session-file-name)))
    (with-current-buffer session-buffer
      (goto-char (point-min))
      ;; old sessions support
      (when (string= "(setq "
		     (buffer-substring-no-properties 1 7))
	(goto-char (point-min))
	;; skip "("
	(forward-char)
	;; skip setq
	(forward-sexp)
	;; skip ellama--current-session
	(forward-sexp)
	;; skip space
	(forward-char)
	;; remove all above
	(kill-region (point-min) (point))
	(goto-char (point-max))
	;; remove ")"
	(delete-char -1)
	;; save session in new format
	(save-buffer)
	(goto-char (point-min))))
    (with-current-buffer buffer
      ;; support sessions without user nick at the end of buffer
      (when (not (save-excursion
		   (save-match-data
		     (goto-char (point-max))
		     (and (search-backward (concat (ellama-get-nick-prefix-for-mode) " " ellama-user-nick ":\n") nil t)
			  (search-forward (concat (ellama-get-nick-prefix-for-mode) " " ellama-user-nick ":\n") nil t)
			  (equal (point) (point-max))))))
	(goto-char (point-max))
	(insert (ellama-get-nick-prefix-for-mode) " " ellama-user-nick ":\n")
	(save-buffer))
      (let* ((session (read session-buffer))
	     ;; workaround for old sessions
	     (offset (cl-struct-slot-offset 'ellama-session 'extra))
	     (extra (when (> (length session)
			     offset)
		      (aref session offset))))
	(setq ellama--current-session
	      (make-ellama-session
	       :id (ellama-session-id session)
	       :provider (ellama-session-provider session)
	       :file (ellama-session-file session)
	       :prompt (ellama-session-prompt session)
	       :extra extra)))
      (setq ellama--current-session-id (ellama-session-id ellama--current-session))
      (puthash (ellama-session-id ellama--current-session)
	       buffer ellama--active-sessions)
      (ellama-session-mode +1))
    (kill-buffer session-buffer)
    (ellama-hide-quotes)
    (display-buffer buffer (when ellama-chat-display-action-function
			     `((ignore . (,ellama-chat-display-action-function)))))))

;;;###autoload
(defun ellama-session-delete ()
  "Delete ellama session."
  (interactive)
  (let* ((id (completing-read
	      "Select session to remove: "
	      (hash-table-keys ellama--active-sessions)))
	 (buffer (ellama-get-session-buffer id))
	 (file (buffer-file-name buffer))
	 (session-file (when file (ellama--get-session-file-name file)))
	 (translation-file (when file (ellama--get-translation-file-name file))))
    (kill-buffer buffer)
    (when file (delete-file file t))
    (when session-file (delete-file session-file t))
    (mapc
     (lambda (buf)
       (when (and
	      translation-file
	      (buffer-file-name buf)
	      (file-equal-p (buffer-file-name buf)
			    translation-file))
	 (kill-buffer buf)))
     (buffer-list))
    (when (and translation-file (file-exists-p translation-file))
      (delete-file translation-file t))))

(defun ellama-activate-session (id)
  "Change current active session to session with ID."
  (setq ellama--current-session-id id))

;;;###autoload
(defun ellama-session-switch ()
  "Change current active session."
  (interactive)
  (let* ((id (completing-read
	      "Select session to activate: "
	      (hash-table-keys ellama--active-sessions)))
	 (buffer (ellama-get-session-buffer id)))
    (ellama-activate-session id)
    (display-buffer buffer (when ellama-chat-display-action-function
			     `((ignore . (,ellama-chat-display-action-function)))))))

;;;###autoload
(defun ellama-session-kill ()
  "Select and kill one of active sessions."
  (interactive)
  (let* ((id (completing-read
	      "Select session to kill: "
	      (hash-table-keys ellama--active-sessions)))
	 (buffer (ellama-get-session-buffer id)))
    (kill-buffer buffer)))

;;;###autoload
(defun ellama-session-rename ()
  "Rename current ellama session."
  (interactive)
  (let* ((id (if ellama--current-session
		 (ellama-session-id ellama--current-session)
	       ellama--current-session-id))
	 (buffer (when id (ellama-get-session-buffer id)))
	 (session (when buffer (with-current-buffer buffer
				 ellama--current-session)))
	 (file-name (when buffer (buffer-file-name buffer)))
	 (file-ext (when file-name (file-name-extension file-name)))
	 (dir (when file-name (file-name-directory file-name)))
	 (session-file-name (when file-name (ellama--get-session-file-name file-name)))
	 (new-id (read-string
		  "New session name: "
		  id))
	 (new-file-name (when dir (file-name-concat
				   dir
				   (concat new-id "." file-ext))))
	 (new-session-file-name
	  (when new-file-name (ellama--get-session-file-name new-file-name))))
    (when new-file-name (with-current-buffer buffer
			  (set-visited-file-name new-file-name)))
    (when buffer (with-current-buffer buffer
		   (rename-buffer (or new-file-name new-id))))
    (when (and file-name (file-exists-p file-name))
      (rename-file file-name new-file-name))
    (when (and session-file-name (file-exists-p session-file-name))
      (rename-file session-file-name new-session-file-name))
    (when session (setf (ellama-session-id session) new-id))
    (when (equal ellama--current-session-id id)
      (setq ellama--current-session-id new-id))
    (remhash id ellama--active-sessions)
    (puthash new-id buffer ellama--active-sessions)
    (when (and buffer ellama-session-auto-save)
      (with-current-buffer buffer
	(save-buffer)))))

(defvar ellama--context-buffer " *ellama-context*")

(defcustom ellama-context-posframe-enabled nil
  "Enable showing posframe with ellama context."
  :group 'ellama
  :type 'boolean)

;;;###autoload
(defun ellama-context-reset ()
  "Clear global context."
  (interactive)
  (setq ellama--global-context nil)
  (with-current-buffer ellama--context-buffer
    (erase-buffer))
  (ellama-update-context-show))

(defun ellama-context--element-remove-by-name (name)
  "Remove all context element that matches by NAME."
  (setq ellama--global-context
	(cl-remove-if (lambda (el)
			(string= name (ellama-context-element-display el)))
		      ellama--global-context)))

;;;###autoload
(defun ellama-context-element-remove-by-name ()
  "Remove a context element by its name from the global context.
This function prompts the user to select a context element from
the list of unique elements currently present in the global
context and removes it.  After removal, it updates the display of
the context."
  (interactive)
  (ellama-context--element-remove-by-name
   (completing-read
    "Remove context element: "
    (seq-uniq (mapcar #'ellama-context-element-display ellama--global-context))))
  (ellama-update-context-show))

;; Context elements

(defclass ellama-context-element () ()
  "A structure for holding information about a context element.")

(cl-defgeneric ellama-context-element-add (element)
  "Add the ELEMENT to the Ellama context.")

(cl-defgeneric ellama-context-element-extract (element)
  "Extract the content of the context ELEMENT.")

(cl-defgeneric ellama-context-element-display (element)
  "Display the context ELEMENT.")

(cl-defgeneric ellama-context-element-format (element mode)
  "Format the context ELEMENT for the major MODE.")

(defcustom ellama-context-poshandler 'posframe-poshandler-frame-top-center
  "Position handler for displaying context buffer."
  :group 'ellama
  :type 'function)

(defcustom ellama-context-border-width 1
  "Border width for the context buffer."
  :group 'ellama
  :type 'integer)

(defun ellama-update-context-show ()
  "Update and show context in posframe of header line."
  (declare-function posframe-show "ext:posframe")
  (declare-function posframe-hide "ext:posframe")
  (with-current-buffer ellama--context-buffer
    (erase-buffer)
    (if ellama--global-context
	(insert (format
		 " ellama ctx: %s"
		 (string-join
		  (mapcar
		   (lambda (el)
		     (ellama-context-element-display el))
		   ellama--global-context)
		  "  ")))
      (insert " ellama ctx")))
  (when ellama-context-posframe-enabled
    (require 'posframe)
    (if ellama--global-context
	(posframe-show
	 ellama--context-buffer
	 :poshandler ellama-context-poshandler
	 :internal-border-width ellama-context-border-width)
      (posframe-hide ellama--context-buffer)))
  (ellama-context-update-header-line))

(defface ellama-face '((t (:inherit shadow)))
  "Base face for all ellama things.")

(defface ellama-context-line-face '((t (:inherit (mode-line-buffer-id ellama-face))))
  "Face for ellama context line.")

(defun ellama-context-line ()
  "Return current global context line."
  (propertize (with-current-buffer ellama--context-buffer
		(buffer-substring-no-properties
		 (point-min) (point-max)))
	      'help-echo "mouse-1: manage ellama context"
	      'mouse-face 'header-line-format
	      'face 'ellama-context-line-face
	      'keymap (let ((m (make-sparse-keymap)))
			(define-key m [header-line mouse-1] #'ellama-transient-context-menu)
			(define-key m [mode-line mouse-1] #'ellama-transient-context-menu)
			m)))

;;;###autoload
(define-minor-mode ellama-context-header-line-mode
  "Toggle Ellama Context header line mode."
  :group 'ellama
  (add-hook 'window-state-change-hook #'ellama-context-update-header-line)
  (if ellama-context-header-line-mode
      (ellama-context-update-header-line)
    (when (listp header-line-format)
      (setq header-line-format (delete '(:eval (ellama-context-line)) header-line-format)))))

;;;###autoload
(define-globalized-minor-mode ellama-context-header-line-global-mode
  ellama-context-header-line-mode
  ellama-context-header-line-mode)

(defun ellama-context-update-header-line ()
  "Update and display context information in the header line."
  (when (listp header-line-format)
    (if (and ellama-context-header-line-mode
	     (or ellama-context-line-always-visible
		 ellama--global-context))
	(add-to-list 'header-line-format '(:eval (ellama-context-line)) t)
      (setq header-line-format (delete '(:eval (ellama-context-line)) header-line-format)))))

;;;###autoload
(define-minor-mode ellama-context-mode-line-mode
  "Toggle Ellama Context mode line mode."
  :group 'ellama
  (add-hook 'window-state-change-hook #'ellama-context-update-mode-line)
  (if ellama-context-mode-line-mode
      (ellama-context-update-mode-line)
    (setq mode-line-format (delete '(:eval (ellama-context-line)) mode-line-format))))

;;;###autoload
(define-globalized-minor-mode ellama-context-mode-line-global-mode
  ellama-context-mode-line-mode
  ellama-context-mode-line-mode)

(defun ellama-context-turn-on-mode-line-mode ()
  "Turn on `ellama-context-mode-line-mode' if appropriate."
  (when (or (eq major-mode 'text-mode)
            (derived-mode-p 'text-mode))
    (ellama-context-mode-line-mode 1)))

(defun ellama-context-update-mode-line ()
  "Update and display context information in the mode line."
  (if (and ellama-context-mode-line-mode
	   (or ellama-context-line-always-visible
	       ellama--global-context))
      (add-to-list 'mode-line-format '(:eval (ellama-context-line)) t)
    (setq mode-line-format (delete '(:eval (ellama-context-line)) mode-line-format))))

(cl-defmethod ellama-context-element-add ((element ellama-context-element))
  "Add the ELEMENT to the Ellama context."
  (setf ellama--global-context (nreverse ellama--global-context))
  (cl-pushnew element ellama--global-context
	      :test #'equal-including-properties)
  (setf ellama--global-context (nreverse ellama--global-context))
  (get-buffer-create ellama--context-buffer t)
  (ellama-update-context-show))

(defcustom ellama-manage-context-display-action-function #'display-buffer-same-window
  "Display action function for `ellama-render-context'."
  :group 'ellama
  :type 'function)

(defvar ellama-context-buffer "*ellama-context*")

(defvar-keymap ellama-context-mode-map
  :doc "Local keymap for Ellama context mode buffers."
  :full t
  :parent special-mode-map
  "n"       #'next-line
  "p"       #'previous-line
  "q"       #'quit-window
  "g"       #'ellama-manage-context
  "a"       #'ellama-transient-context-menu
  "d"       #'ellama-remove-context-element-at-point
  "RET"     #'ellama-preview-context-element-at-point)

(define-derived-mode ellama-context-mode
  fundamental-mode
  "ellama-ctx"
  "Toggle Ellama Context mode."
  :keymap ellama-context-mode-map
  :group 'ellama)

;;;###autoload
(defun ellama-send-buffer-to-new-chat ()
  "Send current buffer to new chat session."
  (interactive)
  (ellama-chat
   (buffer-substring-no-properties (point-min) (point-max))
   t))

(defvar-keymap ellama-blueprint-mode-map
  :doc "Local keymap for Ellama blueprint mode buffers."
  :parent global-map
  "C-c C-c" #'ellama-send-buffer-to-new-chat
  "C-c C-k" (lambda () (interactive) (kill-buffer (current-buffer))))

;;;###autoload
(define-derived-mode ellama-blueprint-mode
  fundamental-mode
  "ellama-blueprint"
  "Toggle Ellama Blueprint mode."
  :keymap ellama-blueprint-mode-map
  :group 'ellama
  (setq header-line-format
	"'C-c C-c' to send  'C-c C-k' to cancel"))

(defun ellama-update-context-buffer ()
  "Update ellama context buffer."
  (let* ((buf (get-buffer-create ellama-context-buffer))
         (inhibit-read-only t))
    (with-current-buffer buf
      (read-only-mode +1)
      (ellama-context-mode)
      (erase-buffer)
      (dolist (el ellama--global-context)
        (insert (ellama-context-element-display el))
        (put-text-property (pos-bol) (pos-eol) 'context-element el)
        (insert "\n"))
      (goto-char (point-min)))))

;;;###autoload
(defun ellama-manage-context ()
  "Manage the global context."
  (interactive)
  (ellama-update-context-buffer)
  (display-buffer
   ellama-context-buffer
   (when ellama-manage-context-display-action-function
     `((ignore . (,ellama-manage-context-display-action-function))))))

(defvar-keymap ellama-preview-context-mode-map
  :doc "Local keymap for Ellama preview context mode buffers."
  :full t
  :parent special-mode-map
  "q"       #'quit-window)

(define-minor-mode ellama-preview-context-mode
  "Toggle Ellama Preview Context mode."
  :keymap ellama-preview-context-mode-map
  :group 'ellama)

(defcustom ellama-preview-context-element-display-action-function nil
  "Display action function for `ellama-preview-context-element'."
  :group 'ellama
  :type 'function)

(defun ellama-preview-context-element (element)
  "Preview context ELEMENT content."
  (let* ((name
	  (concat (make-temp-name
		   (concat " *ellama-context-"
			   (ellama-context-element-display element)
			   "-"))
		  "*"))
	 (buf (get-buffer-create name)))
    (with-current-buffer buf
      (insert (ellama-context-element-extract element))
      (read-only-mode +1)
      (ellama-preview-context-mode +1)
      (display-buffer
       buf
       (when ellama-preview-context-element-display-action-function
	 `((ignore . (,ellama-preview-context-element-display-action-function))))))))

(defun ellama-remove-context-element (element)
  "Remove context ELEMENT from global context."
  (setf ellama--global-context
	(cl-remove element ellama--global-context :test #'equal-including-properties)))

;;;###autoload
(defun ellama-preview-context-element-at-point ()
  "Preview ellama context element at point."
  (interactive)
  (when-let ((elt (get-text-property (point) 'context-element)))
    (ellama-preview-context-element elt)))

;;;###autoload
(defun ellama-remove-context-element-at-point ()
  "Remove ellama context element at point from global context."
  (interactive)
  (when-let ((elt (get-text-property (point) 'context-element)))
    (ellama-remove-context-element elt)
    (ellama-manage-context)
    (ellama-update-context-show)))

;; Buffer context element

(defclass ellama-context-element-buffer (ellama-context-element)
  ((name :initarg :name :type string))
  "A structure for holding information about a context element.")

(cl-defmethod ellama-context-element-extract
  ((element ellama-context-element-buffer))
  "Extract the content of the context ELEMENT."
  (with-slots (name) element
    (with-current-buffer name
      (let* ((data (buffer-substring-no-properties (point-min) (point-max)))
	     (content (if (derived-mode-p 'org-mode)
			  (ellama-convert-org-to-md data)
			data)))
	content))))

(cl-defmethod ellama-context-element-display
  ((element ellama-context-element-buffer))
  "Display the context ELEMENT."
  (with-slots (name) element
    name))

(cl-defmethod ellama-context-element-format
  ((element ellama-context-element-buffer) (mode (eql 'markdown-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (with-slots (name) element
    (format "```emacs-lisp\n(display-buffer \"%s\")\n```\n" name)))

(cl-defmethod ellama-context-element-format
  ((element ellama-context-element-buffer) (mode (eql 'org-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (with-slots (name) element
    (format "[[elisp:(display-buffer \"%s\")][%s]]" name name)))

;; Buffer quote context elements

(defclass ellama-context-element-buffer-quote (ellama-context-element)
  ((name :initarg :name :type string)
   (content :initarg :content :type string))
  "A structure for holding information about a context element.")

(cl-defmethod ellama-context-element-extract
  ((element ellama-context-element-buffer-quote))
  "Extract the content of the context ELEMENT."
  (oref element content))

(cl-defmethod ellama-context-element-display
  ((element ellama-context-element-buffer-quote))
  "Display the context ELEMENT."
  (oref element name))

(cl-defmethod ellama-context-element-format
  ((element ellama-context-element-buffer-quote) (mode (eql 'markdown-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (with-slots (name content) element
    (if ellama-show-quotes
	(format "[%s](%s):\n%s\n\n"
		name name
		(ellama--md-quote content))
      (format "[%s](%s):\n```emacs-lisp\n(display-buffer \"%s\")"
	      name name (ellama--quote-buffer content)))))

(cl-defmethod ellama-context-element-format
  ((element ellama-context-element-buffer-quote) (mode (eql 'org-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (with-slots (name content) element
    (if ellama-show-quotes
	(format "[[%s][%s]]:\n#+BEGIN_QUOTE\n%s\n#+END_QUOTE\n"
		name name (ellama--org-quote content))
      (format "[[%s][%s]] [[elisp:(display-buffer \"%s\")][show]]"
	      name name (ellama--quote-buffer content)))))

;; File context element

(defclass ellama-context-element-file (ellama-context-element)
  ((name :initarg :name :type string))
  "A structure for holding information about a context element.")

(cl-defmethod ellama-context-element-extract
  ((element ellama-context-element-file))
  "Extract the content of the context ELEMENT."
  (with-slots (name) element
    (with-temp-buffer
      (insert-file-contents name)
      (let* ((data (buffer-substring-no-properties (point-min) (point-max)))
	     (ext (file-name-extension name)))
	(if (string= ext "org")
	    (ellama-convert-org-to-md data)
	  data)))))

(cl-defmethod ellama-context-element-display
  ((element ellama-context-element-file))
  "Display the context ELEMENT."
  (with-slots (name) element
    (file-name-nondirectory name)))

(cl-defmethod ellama-context-element-format
  ((element ellama-context-element-file) (mode (eql 'markdown-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (with-slots (name) element
    (format "[%s](<%s>)" name name)))

(cl-defmethod ellama-context-element-format
  ((element ellama-context-element-file) (mode (eql 'org-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (with-slots (name) element
    (format "[[file:%s][%s]]" name name)))

;; Info node context element

(defclass ellama-context-element-info-node (ellama-context-element)
  ((name :initarg :name :type string))
  "A structure for holding information about a context element.")

(cl-defmethod ellama-context-element-extract
  ((element ellama-context-element-info-node))
  "Extract the content of the context ELEMENT."
  (with-slots (name) element
    (with-temp-buffer
      (info name (current-buffer))
      (buffer-substring-no-properties (point-min) (point-max)))))

(cl-defmethod ellama-context-element-display
  ((element ellama-context-element-info-node))
  "Display the context ELEMENT."
  (with-slots (name) element
    (format "(info \"%s\")" name)))

(cl-defmethod ellama-context-element-format
  ((element ellama-context-element-info-node) (mode (eql 'markdown-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (with-slots (name) element
    (format "```emacs-lisp\n(info \"%s\")\n```\n" name)))

(cl-defmethod ellama-context-element-format
  ((element ellama-context-element-info-node) (mode (eql 'org-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (with-slots (name) element
    (format "[[%s][%s]]"
	    (replace-regexp-in-string
	     "(\\(.?*\\)) \\(.*\\)" "info:\\1#\\2" name)
	    (if (and ellama-chat-translation-enabled
		     (not ellama--current-session))
		(ellama--translate-string name)
	      name))))

;; Text context element

(defclass ellama-context-element-text (ellama-context-element)
  ((content :initarg :content :type string))
  "A structure for holding information about a context element.")

(cl-defmethod ellama-context-element-extract
  ((element ellama-context-element-text))
  "Extract the content of the context ELEMENT."
  (oref element content))

(defcustom ellama-text-display-limit 15
  "Limit for text display in context elements."
  :group 'ellama
  :type 'integer)

(cl-defmethod ellama-context-element-display
  ((element ellama-context-element-text))
  "Display the context ELEMENT."
  (with-slots (content) element
    (format "\"%s\"" (concat
		      (string-limit
		       content
		       ellama-text-display-limit)
		      "..."))))

(cl-defmethod ellama-context-element-format
  ((element ellama-context-element-text) (mode (eql 'markdown-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (oref element content))

(cl-defmethod ellama-context-element-format
  ((element ellama-context-element-text) (mode (eql 'org-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (oref element content))

;; Webpage quote context elements

(defclass ellama-context-element-webpage-quote (ellama-context-element)
  ((name :initarg :name :type string)
   (url :initarg :url :type string)
   (content :initarg :content :type string))
  "A structure for holding information about a context element.")

(cl-defmethod ellama-context-element-extract
  ((element ellama-context-element-webpage-quote))
  "Extract the content of the context ELEMENT."
  (oref element content))

(cl-defmethod ellama-context-element-display
  ((element ellama-context-element-webpage-quote))
  "Display the context ELEMENT."
  (with-slots (name) element
    name))

(cl-defmethod ellama-context-element-display
  ((element ellama-context-element-webpage-quote))
  "Display the context ELEMENT."
  (with-slots (name) element
    name))

(defun ellama--quote-buffer (quote)
  "Return buffer name for QUOTE."
  (let* ((buf-name (concat (make-temp-name "*ellama-quote-") "*"))
	 (buf (get-buffer-create buf-name t)))
    (with-current-buffer buf
      (with-silent-modifications
	(insert quote)))
    buf-name))

(cl-defmethod ellama-context-element-format
  ((element ellama-context-element-webpage-quote) (mode (eql 'markdown-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (with-slots (name url content) element
    (if ellama-show-quotes
	(format "[%s](%s):\n%s\n\n"
		name url
		(ellama--md-quote content))
      (format
       "[%s](%s):\n```emacs-lisp\n(display-buffer \"%s\")\n```\n"
       name url (ellama--quote-buffer content)))))

(defun ellama--md-quote (content)
  "Return quoted CONTENT for markdown."
  (with-temp-buffer
    (insert (propertize content 'hard t))
    (let ((fill-prefix "> ")
	  (use-hard-newlines t)
	  (comment-start ">")
	  (comment-empty-lines t))
      (comment-region (point-min) (point-max) ">")
      (fill-region (point-min) (point-max) nil t t))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun ellama--org-quote (content)
  "Return transformed CONTENT for org quotes."
  (replace-regexp-in-string "^*" " *" content))

(cl-defmethod ellama-context-element-format
  ((element ellama-context-element-webpage-quote) (mode (eql 'org-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (with-slots (name url content) element
    (if ellama-show-quotes
	(format "[[%s][%s]]:\n#+BEGIN_QUOTE\n%s\n#+END_QUOTE\n"
		url name (ellama--org-quote content))
      (format "[[%s][%s]] [[elisp:(display-buffer \"%s\")][show]]"
	      url name (ellama--quote-buffer content)))))

;; Info node quote context elements

(defclass ellama-context-element-info-node-quote (ellama-context-element)
  ((name :initarg :name :type string)
   (content :initarg :content :type string))
  "A structure for holding information about a context element.")

(cl-defmethod ellama-context-element-extract
  ((element ellama-context-element-info-node-quote))
  "Extract the content of the context ELEMENT."
  (oref element content))

(cl-defmethod ellama-context-element-display
  ((element ellama-context-element-info-node-quote))
  "Display the context ELEMENT."
  (with-slots (name) element
    (format "(info \"%s\")" name)))

(cl-defmethod ellama-context-element-format
  ((element ellama-context-element-info-node-quote) (mode (eql 'markdown-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (with-slots (name content) element
    (if ellama-show-quotes
	(format "```emacs-lisp\n(info \"%s\")\n```\n%s\n\n"
		name
		(ellama--md-quote content))
      (format "```emacs-lisp\n(info \"%s\")\n```\nshow:\n```emacs-lisp\n(display-buffer \"%s\")\n```\n" name (ellama--quote-buffer content)))))

(cl-defmethod ellama-context-element-format
  ((element ellama-context-element-info-node-quote) (mode (eql 'org-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (with-slots (name content) element
    (if ellama-show-quotes
	(format "[[%s][%s]]:\n#+BEGIN_QUOTE\n%s\n#+END_QUOTE\n"
		(replace-regexp-in-string
		 "(\\(.?*\\)) \\(.*\\)" "info:\\1#\\2" name)
		(if (and ellama-chat-translation-enabled
			 (not ellama--current-session))
		    (ellama--translate-string name)
		  name)
		(ellama--org-quote content))
      (format "[[%s][%s]] [[elisp:(display-buffer \"%s\")][show]]"
	      (replace-regexp-in-string
	       "(\\(.?*\\)) \\(.*\\)" "info:\\1#\\2" name)
	      (if (and ellama-chat-translation-enabled
		       (not ellama--current-session))
		  (ellama--translate-string name)
		name)
	      (ellama--quote-buffer content)))))

;; File quote context elements

(defclass ellama-context-element-file-quote (ellama-context-element)
  ((path :initarg :path :type string)
   (content :initarg :content :type string))
  "A structure for holding information about a context element.")

(cl-defmethod ellama-context-element-extract
  ((element ellama-context-element-file-quote))
  "Extract the content of the context ELEMENT."
  (oref element content))

(cl-defmethod ellama-context-element-display
  ((element ellama-context-element-file-quote))
  "Display the context ELEMENT."
  (with-slots (path) element
    (file-name-nondirectory path)))

(cl-defmethod ellama-context-element-format
  ((element ellama-context-element-file-quote) (mode (eql 'markdown-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (with-slots (path content) element
    (if ellama-show-quotes
	(format "[%s](%s):\n%s\n\n"
		path path
		(ellama--md-quote content))
      (format "[%s](%s):\n```emacs-lisp\n(display-buffer \"%s\")"
	      path path (ellama--quote-buffer content)))))

(cl-defmethod ellama-context-element-format
  ((element ellama-context-element-file-quote) (mode (eql 'org-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (with-slots (path content) element
    (if ellama-show-quotes
	(format "[[%s][%s]]:\n#+BEGIN_QUOTE\n%s\n#+END_QUOTE\n"
		path path (ellama--org-quote content))
      (format "[[%s][%s]] [[elisp:(display-buffer \"%s\")][show]]"
	      path path (ellama--quote-buffer content)))))


;;;###autoload
(defun ellama-context-add-file ()
  "Add file to context."
  (interactive)
  (let* ((file-name (read-file-name "Select file: " nil nil t))
         (element (ellama-context-element-file :name file-name)))
    (ellama-context-element-add element)))

(defun ellama-context-add-file-quote-noninteractive (path content)
  "Add file with PATH quote CONTENT to context."
  (let ((element (ellama-context-element-file-quote
		  :path path :content content)))
    (ellama-context-element-add element)))

;;;###autoload
(defun ellama-context-add-file-quote ()
  "Add file quote to context interactively."
  (interactive)
  (let ((path (buffer-file-name (current-buffer)))
	(content (if (region-active-p)
		     (buffer-substring-no-properties
		      (region-beginning)
		      (region-end))
		   (buffer-substring-no-properties
		    (point-min)
		    (point-max)))))
    (if (not path)
	(warn "should be called from buffer associated with file")
      (ellama-context-add-file-quote-noninteractive path content))))

;;;###autoload
(defun ellama-context-add-buffer (buf)
  "Add BUF to context."
  (interactive "bSelect buffer: ")
  (let* ((buffer-name (if (stringp buf)
			  buf
			(buffer-name buf)))
	 (element (ellama-context-element-buffer :name buffer-name)))
    (ellama-context-element-add element)))

;;;###autoload
(defun ellama-context-add-directory (dir)
  "Add all files in DIR to the context."
  (interactive "DSelect directory: ")
  (dolist (file-name (directory-files dir t "^[^\.].*"))
    (unless (file-directory-p file-name)
      (let ((element (ellama-context-element-file :name file-name)))
        (ellama-context-element-add element)))))

;;;###autoload
(defun ellama-context-add-selection ()
  "Add active region to context."
  (interactive)
  (if (region-active-p)
      (let* ((data (buffer-substring-no-properties (region-beginning) (region-end)))
	     (content (if (derived-mode-p 'org-mode)
			  (ellama-convert-org-to-md data)
			data))
	     (file-name (buffer-file-name))
	     (buffer-name (buffer-name (current-buffer)))
             (element (if file-name
			  (ellama-context-element-file-quote :path file-name
							     :content content)
			(ellama-context-element-buffer-quote :name buffer-name :content content))))
        (ellama-context-element-add element))
    (warn "No active region")))

(defun ellama-context-add-text (text)
  "Add TEXT to context."
  (let ((element (ellama-context-element-text :content text)))
    (ellama-context-element-add element)))

(declare-function Info-copy-current-node-name "info")

;;;###autoload
(defun ellama-context-add-info-node (node)
  "Add info NODE to context."
  (interactive (list (Info-copy-current-node-name)))
  (let ((element (ellama-context-element-info-node :name node)))
    (ellama-context-element-add element)))

(defun ellama-context-add-info-node-quote-noninteractive (name content)
  "Add info node with NAME quote CONTENT to context."
  (let ((element (ellama-context-element-info-node-quote
		  :name name :content content)))
    (ellama-context-element-add element)))

;;;###autoload
(defun ellama-context-add-info-node-quote ()
  "Add info node quote to context interactively."
  (interactive)
  (let ((name (Info-copy-current-node-name))
	(content (if (region-active-p)
		     (buffer-substring-no-properties
		      (region-beginning)
		      (region-end))
		   (buffer-substring-no-properties
		    (point-min)
		    (point-max)))))
    (if (not name)
	(warn "should be called from `info' buffer")
      (ellama-context-add-info-node-quote-noninteractive name content))))

(defun ellama-context-add-webpage-quote-noninteractive (name url content)
  "Add webpage with NAME and URL quote CONTENT to context."
  (let ((element (ellama-context-element-webpage-quote
		  :name name :url url :content content)))
    (ellama-context-element-add element)))

;;;###autoload
(defun ellama-context-add-webpage-quote-eww ()
  "Add webpage quote to context interactively from `eww'."
  (interactive)
  (defvar eww-data)
  (declare-function eww-current-url "eww")
  (if (eq major-mode 'eww-mode)
      (let* ((name (plist-get eww-data :title))
	     (url (eww-current-url))
	     (content (if (region-active-p)
			  (buffer-substring-no-properties
			   (region-beginning)
			   (region-end))
			(buffer-substring-no-properties
			 (point-min)
			 (point-max)))))
	(ellama-context-add-webpage-quote-noninteractive name url content))
    (warn "Should be called from `eww'.")))

(defun ellama--translate-string (s)
  "Translate string S to `ellama-language' syncronously."
  (llm-chat
   (or ellama-translation-provider ellama-provider)
   (llm-make-simple-chat-prompt
    (format ellama-translation-template
	    ellama-language
	    s
	    ellama-language))))

(defun ellama--format-context (_)
  "Format context for chat buffer."
  (let ((mode (if (derived-mode-p 'org-mode) 'org-mode 'markdown-mode)))
    (if-let* ((context ellama--global-context))
        (concat (string-join
	         (cons "Context:"
                       (mapcar (lambda (elt)
                                 (ellama-context-element-format elt mode))
                               context))
	         "\n")
	        "\n\n")
      "")))

(defun ellama--prompt-with-context (prompt)
  "Add context to PROMPT for sending to llm."
  (let* ((context ellama--global-context))
    (if context
	(concat (string-join
		 (cons "Context:"
		       (mapcar #'ellama-context-element-extract context))
		 "\n")
		"\n\n"
		prompt)
      prompt)))

(defun ellama-chat-buffer-p (buffer)
  "Return non-nil if BUFFER is an ellama chat buffer."
  (with-current-buffer buffer
    (not (not ellama--current-session))))

(defun ellama-get-current-session-id ()
  "Return current session id.
If buffer contains ellama session return its id.
Otherwire return id of current active session."
  (if ellama--current-session
      (ellama-session-id ellama--current-session)
    ellama--current-session-id))

(defun ellama-get-current-session ()
  "Return current session.
If buffer contains ellama session return it.
Otherwire return current active session."
  (if ellama--current-session
      ellama--current-session
    (when ellama--current-session-id
      (with-current-buffer (ellama-get-session-buffer ellama--current-session-id)
	ellama--current-session))))

(defun ellama-collapse-org-quotes ()
  "Collapse quote blocks in curent buffer."
  (declare-function org-element-map "ext:org-element")
  (declare-function org-element-parse-buffer "ext:org-element")
  (declare-function org-element-property "ext:org-element")
  (declare-function org-hide-block-toggle "ext:org-compat")
  (when (derived-mode-p 'org-mode)
    (progn (save-excursion
	     (goto-char (point-min))
	     (org-element-map (org-element-parse-buffer) 'quote-block
	       (lambda (block)
		 (goto-char (org-element-property :begin block))
		 (org-hide-block-toggle 't)))))))

(defun ellama-hide-quotes ()
  "Hide quotes in current session buffer if needed."
  (when-let* ((ellama-session-hide-org-quotes)
	      (session-id ellama--current-session-id)
	      (buf (ellama-get-session-buffer session-id)))
    (with-current-buffer buf
      (ellama-collapse-org-quotes))))

(defun ellama-stream (prompt &rest args)
  "Query ellama for PROMPT.
ARGS contains keys for fine control.

:provider PROVIDER -- PROVIDER is an llm provider for generation.

:buffer BUFFER -- BUFFER is the buffer (or `buffer-name') to insert ellama reply
in.  Default value is (current-buffer).

:point POINT -- POINT is the point in buffer to insert ellama reply at.

:filter FILTER -- FILTER is a function that's applied to (partial) response
strings before they're inserted into the BUFFER.

:session SESSION -- SESSION is a ellama conversation session.

:session-id ID -- ID is a ellama session unique identifier.

:ephemeral-session BOOL -- if BOOL is set session will not be saved to named
file by default.

:system STR -- send STR to model as system message.

:on-error ON-ERROR -- ON-ERROR a function that's called with an error message on
failure (with BUFFER current).

:on-done ON-DONE -- ON-DONE a function or list of functions that's called with
 the full response text when the request completes (with BUFFER current)."
  (declare-function spinner-start "ext:spinner")
  (declare-function spinner-stop "ext:spinner")
  (let* ((session-id (plist-get args :session-id))
	 (session (or (plist-get args :session)
		      (when session-id
			(with-current-buffer (ellama-get-session-buffer session-id)
			  ellama--current-session))))
	 (provider (if session
		       (ellama-session-provider session)
		     (or (plist-get args :provider)
			 ellama-provider
			 (ellama-get-first-ollama-chat-model))))
	 (buffer (or (plist-get args :buffer)
		     (when (ellama-session-p session)
		       (ellama-get-session-buffer (ellama-session-id session)))
		     (current-buffer)))
	 (point (or (plist-get args :point)
		    (with-current-buffer buffer (point))))
	 (filter (or (plist-get args :filter) #'identity))
	 (errcb (or (plist-get args :on-error)
		    (lambda (msg)
		      (error "Error calling the LLM: %s" msg))))
	 (donecb (or (plist-get args :on-done) #'ignore))
	 (prompt-with-ctx (ellama--prompt-with-context prompt))
	 (system (plist-get args :system))
	 (llm-prompt (if session
			 (if (llm-chat-prompt-p (ellama-session-prompt session))
			     (progn
			       (llm-chat-prompt-append-response
				(ellama-session-prompt session)
				prompt-with-ctx)
			       (when system
				 (llm-chat-prompt-append-response
				  (ellama-session-prompt session)
				  system 'system))
			       (ellama-session-prompt session))
			   (setf (ellama-session-prompt session)
				 (llm-make-chat-prompt prompt-with-ctx :context system)))
		       (llm-make-simple-chat-prompt prompt-with-ctx)))
	 (stop-scroll))
    (with-current-buffer buffer
      (ellama-request-mode +1)
      (let* ((start (make-marker))
	     (end (make-marker))
	     (distance-to-end (- (point-max) (point)))
	     (new-pt)
	     (insert-text
	      (lambda (text)
		;; Erase and insert the new text between the marker cons.
		(with-current-buffer buffer
		  ;; Manually save/restore point as save-excursion doesn't
		  ;; restore the point into the middle of replaced text.
		  (let* ((pt (point))
			 (new-distance-to-end (- (point-max) (point))))
		    (save-excursion
		      (if (and (eq (window-buffer (selected-window))
				   buffer)
			       (not (equal distance-to-end new-distance-to-end)))
			  (setq stop-scroll t)
			(setq stop-scroll nil))
		      (goto-char start)
		      (delete-region start end)
		      (insert (funcall filter text))
                      (when (and ellama-fill-paragraphs
				 (pcase ellama-fill-paragraphs
				   ((cl-type function) (funcall ellama-fill-paragraphs))
				   ((cl-type boolean) ellama-fill-paragraphs)
				   ((cl-type list) (and (apply #'derived-mode-p
							       ellama-fill-paragraphs)
							(not (equal major-mode 'org-mode))))))
			(fill-region start (point)))
		      (setq new-pt (point)))
		    (if (and ellama-auto-scroll (not stop-scroll))
			(progn
			  (goto-char new-pt)
			  (ellama--scroll buffer))
		      (goto-char pt)))
		  (undo-amalgamate-change-group ellama--change-group)))))
	(setq ellama--change-group (prepare-change-group))
	(activate-change-group ellama--change-group)
	(ellama-set-markers start end point)
	(when ellama-spinner-enabled
	  (require 'spinner)
	  (spinner-start ellama-spinner-type))
	(let ((request (llm-chat-streaming
			provider
			llm-prompt
			insert-text
			(lambda (text)
			  (funcall insert-text
				   (string-trim
				    (if (and ellama-output-remove-reasoning
					     (not session))
					(ellama-remove-reasoning text)
				      text)))
			  (with-current-buffer buffer
			    (accept-change-group ellama--change-group)
			    (when ellama-spinner-enabled
			      (spinner-stop))
			    (if (and (listp donecb)
				     (functionp (car donecb)))
				(mapc (lambda (fn) (funcall fn text))
				      donecb)
			      (funcall donecb text))
			    (when ellama-session-hide-org-quotes
			      (ellama-collapse-org-quotes))
			    (when (and ellama--current-session
				       ellama-session-remove-reasoning)
			      (mapc (lambda (interaction)
				      (setf (llm-chat-prompt-interaction-content
					     interaction)
					    (ellama-remove-reasoning
					     (llm-chat-prompt-interaction-content
					      interaction))))
				    (llm-chat-prompt-interactions
				     (ellama-session-prompt
				      ellama--current-session))))
			    (setq ellama--current-request nil)
			    (ellama-request-mode -1)))
			(lambda (_ msg)
			  (with-current-buffer buffer
			    (cancel-change-group ellama--change-group)
			    (when ellama-spinner-enabled
			      (spinner-stop))
			    (funcall errcb msg)
			    (setq ellama--current-request nil)
			    (ellama-request-mode -1))))))
	  (with-current-buffer buffer
	    (setq ellama--current-request request)))))))

(defun ellama-set-markers (start end point)
  "Set markers for START and END positions at POINT."
  (set-marker start point)
  (set-marker end point)
  (set-marker-insertion-type start nil)
  (set-marker-insertion-type end t))

(defun ellama-chain (initial-prompt forms &optional acc)
  "Call chain of FORMS on INITIAL-PROMPT.
ACC will collect responses in reverse order (previous answer will be on top).
Each form is a plist that can contain different options:

:provider PROVIDER - use PROVIDER instead of `ellama-provider'.

:transform FUNCTION - use FUNCTION to transform result of previous step to new
prompt.  FUCTION will be called with two arguments INITIAL-PROMPT and ACC.

:session SESSION - use SESSION in current step.

:session-id ID -- ID is a ellama session unique identifier.

:chat BOOL - if BOOL use chat buffer, otherwise use temp buffer.  Make sense for
last step only.

:show BOOL - if BOOL show buffer for this step."
  (let* ((hd (car forms))
	 (tl (cdr forms))
	 (provider (or (plist-get hd :provider) ellama-provider))
	 (transform (plist-get hd :transform))
	 (prompt (if transform
		     (apply transform (list initial-prompt acc))
		   initial-prompt))
	 (session-id (plist-get hd :session-id))
	 (session (or (plist-get hd :session)
		      (when session-id
			(with-current-buffer (ellama-get-session-buffer session-id)
			  ellama--current-session))))
	 (chat (plist-get hd :chat))
	 (show (or (plist-get hd :show) ellama-always-show-chain-steps))
	 (buf (if (or (and (not chat)) (not session))
		  (get-buffer-create (make-temp-name
				      (ellama-generate-name provider real-this-command prompt)))
		(ellama-get-session-buffer ellama--current-session-id))))
    (when show
      (display-buffer buf (if chat (when ellama-chat-display-action-function
				     `((ignore . (,ellama-chat-display-action-function))))
			    (when ellama-instant-display-action-function
			      `((ignore . (,ellama-instant-display-action-function)))))))
    (with-current-buffer buf
      (funcall ellama-major-mode))
    (if chat
	(ellama-chat
	 prompt
	 nil
	 :provider provider
	 :on-done (lambda (res)
		    (when tl
		      (ellama-chain res tl (cons res acc)))))
      (ellama-stream
       prompt
       :provider provider
       :buffer buf
       :session session
       :filter (when (derived-mode-p 'org-mode)
		 #'ellama--translate-markdown-to-org-filter)
       :on-done (lambda (res)
		  (when tl
		    (ellama-chain res tl (cons res acc))))))))

;;;###autoload
(defun ellama-solve-reasoning-problem (problem)
  "Solve reasoning PROBLEM with absctraction of thought.
Problem will be solved with the chain of questions to LLM."
  (interactive "sProblem: ")
  (ellama-chain
   problem
   '((:chat t
	    :transform (lambda (problem _)
			 (format "Problem:
%s

Let's think logically and provide abstract higher order plan how to solve this kind
of problems. Don't dive into small details only provide high-level plan." problem)))
     (:chat t
	    :transform (lambda (_ _)
			 "Provide more detailed plan. On what details should we pay attention?"))
     (:chat t
	    :transform (lambda (_ _)
			 "Now revise the plan and provide the final solution."))
     (:chat t
	    :transform (lambda (_ _)
			 "Provide short final answer based on final solution.")))))

;;;###autoload
(defun ellama-solve-domain-specific-problem (problem)
  "Solve domain-specific PROBLEM with `ellama-chain'."
  (interactive "sProblem: ")
  (ellama-chain
   problem
   `((:transform (lambda (problem _)
		   (format "Problem:
%s

Which specialist suits better for solving this kind of problems?"
			   problem)))
     (:transform (lambda (res _)
		   (format "Message:
%s

Extract profession from this message. Be short and concise."
			   res)))
     (:chat t
	    :transform (lambda (profession _)
			 (format
			  "You are professional %s. Do your best and create detailed plan how to solve this problem:
%s"
			  (string-trim profession) ,problem)))
     (:chat t
	    :transform (lambda (_ _)
			 "Now revise the plan and provide the final solution."))
     (:chat t
	    :transform (lambda (_ _)
			 "Provide short final answer based on final solution.")))))

(declare-function org-export-to-buffer "ox")
(defvar org-export-show-temporary-export-buffer)

(defun ellama-convert-org-to-md (text)
  "Translate TEXT from org syntax to markdown syntax."
  (require 'ox)
  (require 'ox-md)
  (let ((buf (make-temp-name "ellama-"))
	(org-export-show-temporary-export-buffer nil))
    (with-temp-buffer
      (insert "#+OPTIONS: toc:nil broken-links:mark\n" text)
      (org-export-to-buffer 'md buf
	nil nil t t nil (lambda () (text-mode))))
    (with-current-buffer buf
      (prog1
	  (string-trim (buffer-substring-no-properties (point-min) (point-max)))
	(kill-buffer buf)))))

(defun ellama-get-last-user-message ()
  "Return last not sent user message in current session buffer."
  (when ellama--current-session
    (save-excursion
      (save-match-data
	(goto-char (point-max))
	(and (search-backward (concat (ellama-get-nick-prefix-for-mode) " " ellama-user-nick ":\n") nil t)
	     (search-forward (concat (ellama-get-nick-prefix-for-mode) " " ellama-user-nick ":\n") nil t)
	     (buffer-substring-no-properties (point) (point-max)))))))

(defun ellama--scroll (&optional buffer)
  "Scroll within BUFFER.
A function for programmatically scrolling the buffer during text generation."
  (when-let ((ellama-auto-scroll)
	     (buf (or buffer (current-buffer)))
	     (window (get-buffer-window buf)))
    (with-selected-window window
      (when (ellama-chat-buffer-p buf)
	(goto-char (point-max)))
      (recenter -1)
      (redisplay))))

(defun ellama-chat-done (text &optional on-done)
  "Chat done.
Will call `ellama-chat-done-callback' and ON-DONE on TEXT."
  (save-excursion
    (goto-char (point-max))
    (insert "\n\n" (ellama-get-nick-prefix-for-mode) " " ellama-user-nick ":\n")
    (when ellama-session-auto-save
      (save-buffer)))
  (ellama--scroll)
  (when ellama-chat-done-callback
    (funcall ellama-chat-done-callback text))
  (when on-done
    (funcall on-done text)))

(defun ellama--translate-generated-text-on-done (translation-buffer)
  "Translate generated text into TRANSLATION-BUFFER."
  (lambda (generated)
    (ellama-chat-done generated)
    (display-buffer translation-buffer (when ellama-chat-display-action-function
					 `((ignore . (,ellama-chat-display-action-function)))))
    (with-current-buffer translation-buffer
      (save-excursion
	(goto-char (point-max))
	(ellama-stream
	 (format ellama-translation-template
		 ellama-language
		 generated
		 ellama-language)
	 :provider (or ellama-translation-provider ellama-provider)
	 :on-done #'ellama-chat-done
	 :filter (when (derived-mode-p 'org-mode)
		   #'ellama--translate-markdown-to-org-filter))))))

(defun ellama--call-llm-with-translated-prompt (buffer session translation-buffer)
  "Call llm with translated text in BUFFER with SESSION from TRANSLATION-BUFFER."
  (lambda (result)
    (ellama-chat-done result)
    (save-excursion
      (goto-char (point-max))
      (delete-char -2)
      (delete-char (- (length result))))
    (display-buffer buffer (when ellama-chat-display-action-function
			     `((ignore . (,ellama-chat-display-action-function)))))
    (with-current-buffer buffer
      (save-excursion
	(goto-char (point-max))
	(insert (ellama-get-nick-prefix-for-mode) " " ellama-user-nick ":\n"
		(ellama--format-context session) result "\n\n"
		(ellama-get-nick-prefix-for-mode) " " ellama-assistant-nick ":\n")
	(ellama-stream result
		       :session session
		       :on-done (ellama--translate-generated-text-on-done translation-buffer)
		       :filter (when (derived-mode-p 'org-mode)
				 #'ellama--translate-markdown-to-org-filter))))))

(defun ellama--translate-interaction (prompt translation-buffer buffer session)
  "Translate chat PROMPT in TRANSLATION-BUFFER for BUFFER with SESSION."
  (display-buffer translation-buffer (when ellama-chat-display-action-function
				       `((ignore . (,ellama-chat-display-action-function)))))
  (with-current-buffer translation-buffer
    (save-excursion
      (goto-char (point-max))
      (insert (ellama-get-nick-prefix-for-mode) " " ellama-user-nick ":\n"
	      (ellama--format-context session) prompt "\n\n"
	      (ellama-get-nick-prefix-for-mode) " " ellama-assistant-nick ":\n")
      (ellama-stream
       (format ellama-translation-template
	       "english"
	       prompt
	       "english")
       :provider (or ellama-translation-provider ellama-provider)
       :filter (when (derived-mode-p 'org-mode)
		 #'ellama--translate-markdown-to-org-filter)
       :on-done
       (ellama--call-llm-with-translated-prompt buffer session translation-buffer)))))

;;;###autoload
(defun ellama-chat (prompt &optional create-session &rest args)
  "Send PROMPT to ellama chat with conversation history.

If CREATE-SESSION set, creates new session even if there is an active session.
ARGS contains keys for fine control.

:provider PROVIDER -- PROVIDER is an llm provider for generation.

:session SESSION -- SESSION is a ellama conversation session.

:session-id ID -- ID is a ellama session unique identifier.

:system STR -- send STR to model as system message.

:on-done ON-DONE -- ON-DONE a function that's called with
the full response text when the request completes (with BUFFER current)."
  (interactive "sAsk ellama: ")
  (let* ((ollama-binary (executable-find ellama-ollama-binary))
	 (providers (append
		     `(("default model" . ellama-provider)
		       ,(if (and ollama-binary
				 (file-exists-p ollama-binary))
			    '("ollama model" . (ellama-get-ollama-local-model))))
		     ellama-providers))
	 (variants (mapcar #'car providers))
	 (system (plist-get args :system))
	 (donecb (plist-get args :on-done))
	 (provider (if current-prefix-arg
		       (eval (alist-get
			      (completing-read "Select model: " variants)
			      providers nil nil #'string=))
		     (or (plist-get args :provider)
			 ellama-provider
			 (ellama-get-first-ollama-chat-model))))
	 (session (or (plist-get args :session)
		      (if (or create-session
			      current-prefix-arg
			      (and provider
				   (or (plist-get args :provider)
				       (not (equal provider ellama-provider)))
				   ellama--current-session-id
				   (with-current-buffer (ellama-get-session-buffer
							 ellama--current-session-id)
				     (not (equal
					   provider
					   (ellama-session-provider ellama--current-session)))))
			      (and (not ellama--current-session)
				   (not ellama--current-session-id)))
			  (ellama-new-session provider prompt)
			(or ellama--current-session
			    (with-current-buffer (ellama-get-session-buffer
						  (or (plist-get args :session-id)
						      ellama--current-session-id))
			      ellama--current-session)))))
	 (buffer (ellama-get-session-buffer
		  (ellama-session-id session)))
	 (file-name (ellama-session-file session))
	 (translation-buffer (when ellama-chat-translation-enabled
			       (if file-name
				   (progn
				     (find-file-noselect
				      (ellama--get-translation-file-name file-name)))
				 (get-buffer-create (ellama-session-id session))))))
    (if ellama-chat-translation-enabled
	(ellama--translate-interaction prompt translation-buffer buffer session)
      (display-buffer buffer (when ellama-chat-display-action-function
			       `((ignore . (,ellama-chat-display-action-function)))))
      (with-current-buffer buffer
	(save-excursion
	  (goto-char (point-max))
	  (if (equal (point-min) (point-max)) ;; empty buffer
	      (insert (ellama-get-nick-prefix-for-mode) " " ellama-user-nick ":\n"
		      (ellama--format-context session) (ellama--fill-long-lines prompt) "\n\n"
		      (ellama-get-nick-prefix-for-mode) " " ellama-assistant-nick ":\n")
	    (insert (ellama--format-context session) (ellama--fill-long-lines prompt) "\n\n"
		    (ellama-get-nick-prefix-for-mode) " " ellama-assistant-nick ":\n"))
	  (ellama-stream prompt
			 :session session
			 :system system
			 :on-done (if donecb (list 'ellama-chat-done donecb)
				    'ellama-chat-done)
			 :filter (when (derived-mode-p 'org-mode)
				   #'ellama--translate-markdown-to-org-filter)))))))

;;;###autoload
(defun ellama-chat-send-last-message ()
  "Send last user message extracted from current ellama chat buffer."
  (interactive)
  (when-let* ((session ellama--current-session)
	      (message (ellama-get-last-user-message))
	      ((length> message 0))
	      (text (if (derived-mode-p 'org-mode)
			(ellama-convert-org-to-md message)
		      message)))
    (goto-char (point-max))
    (insert "\n\n")
    (when ellama--global-context
      (insert (ellama--format-context session)))
    (insert (ellama-get-nick-prefix-for-mode) " " ellama-assistant-nick ":\n")
    (ellama-stream text
		   :session session
		   :on-done #'ellama-chat-done
		   :filter (when (derived-mode-p 'org-mode)
			     #'ellama--translate-markdown-to-org-filter))))

;;;###autoload
(defun ellama-ask-about ()
  "Ask ellama about selected region or current buffer."
  (interactive)
  (let ((input (read-string "Ask ellama about this text: ")))
    (if (region-active-p)
	(ellama-context-add-selection)
      (ellama-context-add-buffer (buffer-name (current-buffer))))
    (ellama-chat input)))

;;;###autoload
(defun ellama-ask-selection ()
  "Send selected region or current buffer to ellama chat."
  (interactive)
  (let ((text (if (region-active-p)
		  (buffer-substring-no-properties (region-beginning) (region-end))
		(buffer-substring-no-properties (point-min) (point-max)))))
    (ellama-chat text)))

;;;###autoload
(defun ellama-complete ()
  "Complete text in current buffer."
  (interactive)
  (let* ((beg (if (region-active-p)
		  (region-beginning)
		(point-min)))
	 (end (if (region-active-p)
		  (region-end)
		(point)))
	 (text (buffer-substring-no-properties beg end)))
    (ellama-stream text)))

(defvar vc-git-diff-switches)
(declare-function vc-diff-internal "vc")
(declare-function vc-deduce-fileset "vc")

(defun ellama--diff-cached ()
  "Diff staged."
  (require 'vc)
  (let* ((default-directory
	  (if (string= ".git"
		       (car (reverse
			     (cl-remove
			      ""
			      (file-name-split default-directory)
			      :test #'string=))))
	      (file-name-parent-directory default-directory)
	    default-directory))
	 (vc-git-diff-switches "--cached")
	 (diff (with-temp-buffer
		 (vc-diff-internal
		  nil (vc-deduce-fileset t) nil nil nil (current-buffer))
		 (buffer-substring-no-properties (point-min) (point-max)))))
    (if (string-empty-p diff)
	nil
      diff)))

(defun ellama--diff ()
  "Diff unstaged."
  (require 'vc)
  (let* ((default-directory
	  (if (string= ".git"
		       (car (reverse
			     (cl-remove
			      ""
			      (file-name-split default-directory)
			      :test #'string=))))
	      (file-name-parent-directory default-directory)
	    default-directory))
	 (vc-git-diff-switches t)
	 (diff (with-temp-buffer
		 (vc-diff-internal
		  nil (vc-deduce-fileset t) nil nil nil (current-buffer))
		 (buffer-substring-no-properties (point-min) (point-max)))))
    (if (string-empty-p diff)
	nil
      diff)))

;;;###autoload
(defun ellama-generate-commit-message ()
  "Generate commit message based on diff."
  (interactive)
  (save-window-excursion
    (when-let* ((default-directory
		 (if (string= ".git"
			      (car (reverse
				    (cl-remove
				     ""
				     (file-name-split default-directory)
				     :test #'string=))))
		     (file-name-parent-directory default-directory)
		   default-directory))
		(diff (or (ellama--diff-cached)
			  (ellama--diff))))
      (ellama-stream
       (format ellama-generate-commit-message-template diff)
       :provider ellama-coding-provider))))

;;;###autoload
(defun ellama-ask-line ()
  "Send current line to ellama chat."
  (interactive)
  (let ((text (thing-at-point 'line)))
    (ellama-chat text)))

(defun ellama-instant (prompt &rest args)
  "Prompt ellama for PROMPT to reply instantly.

ARGS contains keys for fine control.

:provider PROVIDER -- PROVIDER is an llm provider for generation.

:system STR -- send STR to model as system message.

:on-done ON-DONE -- ON-DONE a function or list of functions that's called with
 the full response text when the request completes (with BUFFER current)."
  (let* ((provider (or (plist-get args :provider)
		       ellama-provider))
	 (buffer-name (ellama-generate-name provider real-this-command prompt))
	 (buffer (get-buffer-create (if (get-buffer buffer-name)
					(make-temp-name (concat buffer-name " "))
				      buffer-name)))
	 (system (plist-get args :system))
	 (donecb (plist-get args :on-done))
	 filter)
    (with-current-buffer buffer
      (funcall ellama-major-mode)
      (when (derived-mode-p 'org-mode)
	(setq filter 'ellama--translate-markdown-to-org-filter)))
    (display-buffer buffer (when ellama-instant-display-action-function
			     `((ignore . (,ellama-instant-display-action-function)))))
    (ellama-stream prompt
		   :system system
		   :buffer buffer
		   :filter filter
		   :provider provider
		   :on-done donecb)))

;;;###autoload
(defun ellama-translate ()
  "Ask ellama to translate selected region or word at point."
  (interactive)
  (let ((text (if (region-active-p)
		  (buffer-substring-no-properties (region-beginning) (region-end))
		(thing-at-point 'word))))
    (ellama-instant
     (format ellama-translation-template
	     ellama-language text ellama-language)
     :provider ellama-translation-provider)))

;;;###autoload
(defun ellama-translate-buffer ()
  "Ask ellama to translate current buffer."
  (interactive)
  (let ((text (buffer-substring-no-properties (point-min) (point-max))))
    (ellama-instant
     (format ellama-translation-template
	     ellama-language text ellama-language)
     :provider ellama-translation-provider)))

;;;###autoload
(defun ellama-define-word ()
  "Find definition of current word."
  (interactive)
  (ellama-instant (format ellama-define-word-prompt-template
			  (thing-at-point 'word))))

;;;###autoload
(defun ellama-summarize ()
  "Summarize selected region or current buffer."
  (interactive)
  (let ((text (if (region-active-p)
		  (buffer-substring-no-properties (region-beginning) (region-end))
		(buffer-substring-no-properties (point-min) (point-max)))))
    (ellama-instant (format ellama-summarize-prompt-template text)
		    :provider (or ellama-summarization-provider ellama-provider))))

;;;###autoload
(defun ellama-summarize-killring ()
  "Summarize text from the kill ring."
  (interactive)
  (let ((text (current-kill 0)))
    (if (string-empty-p text)
        (message "No text in the kill ring to summarize.")
      (ellama-instant (format ellama-summarize-prompt-template text)
		      :provider (or ellama-summarization-provider ellama-provider)))))

;;;###autoload
(defun ellama-code-review ()
  "Review code in selected region or current buffer."
  (interactive)
  (if (region-active-p)
      (ellama-context-add-selection)
    (ellama-context-add-buffer (current-buffer)))
  (ellama-chat ellama-code-review-prompt-template nil :provider ellama-coding-provider))

;;;###autoload
(defun ellama-write (instruction)
  "Write text based on context and INSTRUCTION at point."
  (interactive "sInstruction: ")
  (when (region-active-p)
    (ellama-context-add-selection))
  (ellama-stream (format ellama-write-prompt-template instruction)
		 :point (point)
		 :filter (when (derived-mode-p 'org-mode)
			   #'ellama--translate-markdown-to-org-filter)))

;;;###autoload
(defun ellama-change (change &optional edit-template)
  "Change selected text or text in current buffer according to provided CHANGE.
When the value of EDIT-TEMPLATE is 4, or with one `universal-argument' as
prefix (\\[universal-argument]), prompt the user to amend the template."
  (interactive "sWhat needs to be changed: \np")
  (let* ((beg (if (region-active-p)
		  (region-beginning)
		(point-min)))
	 (end (if (region-active-p)
		  (region-end)
		(point-max)))
         (template-orig (format ellama-change-prompt-template change "%s"))
         (template (if (= edit-template 4)
                       (read-from-minibuffer "Template: " template-orig)
                     template-orig))
	 (text (buffer-substring-no-properties beg end)))
    (kill-region beg end)
    (ellama-stream
     (format template text)
     :point beg)))

;;;###autoload
(defun ellama-improve-grammar (&optional edit-template)
  "Enhance the grammar and spelling in the currently selected region or buffer.
When the value of EDIT-TEMPLATE is 4, or with one `universal-argument' as
prefix (\\[universal-argument]), prompt the user to amend the template."
  (interactive "p")
  (ellama-change ellama-improve-grammar-prompt-template edit-template))

;;;###autoload
(defun ellama-improve-wording (&optional edit-template)
  "Enhance the wording in the currently selected region or buffer.
When the value of EDIT-TEMPLATE is 4, or with one `universal-argument' as
prefix (\\[universal-argument]), prompt the user to amend the template."
  (interactive "p")
  (ellama-change ellama-improve-wording-prompt-template edit-template))

;;;###autoload
(defun ellama-proofread (&optional edit-template)
  "Proofread the currently selected region or buffer.
When the value of EDIT-TEMPLATE is 4, or with one `universal-argument' as
prefix (\\[universal-argument]), prompt the user to amend the template."
  (interactive "p")
  (ellama-change ellama-proofread-prompt-template edit-template))

;;;###autoload
(defun ellama-improve-conciseness (&optional edit-template)
  "Make the text of the currently selected region or buffer concise and simple.
When the value of EDIT-TEMPLATE is 4, or with one `universal-argument' as
prefix (\\[universal-argument]), prompt the user to amend the template."
  (interactive "p")
  (ellama-change ellama-improve-conciseness-prompt-template edit-template))

;;;###autoload
(defun ellama-code-edit (change)
  "Change selected code or code in current buffer according to provided CHANGE."
  (interactive "sWhat needs to be changed in this code: ")
  (let* ((beg (if (region-active-p)
		  (region-beginning)
		(point-min)))
	 (end (if (region-active-p)
		  (region-end)
		(point-max)))
	 (text (buffer-substring-no-properties beg end)))
    (kill-region beg end)
    (ellama-stream
     (format
      ellama-code-edit-prompt-template
      change text)
     :provider ellama-coding-provider
     :filter #'ellama--code-filter
     :point beg)))

;;;###autoload
(defun ellama-code-improve ()
  "Change selected code or code in current buffer according to provided CHANGE."
  (interactive)
  (let* ((beg (if (region-active-p)
		  (region-beginning)
		(point-min)))
	 (end (if (region-active-p)
		  (region-end)
		(point-max)))
	 (text (buffer-substring-no-properties beg end)))
    (kill-region beg end)
    (ellama-stream
     (format
      ellama-code-improve-prompt-template
      text)
     :provider ellama-coding-provider
     :filter #'ellama--code-filter
     :point beg)))

;;;###autoload
(defun ellama-code-complete ()
  "Complete selected code or code in current buffer."
  (interactive)
  (let* ((beg (if (region-active-p)
		  (region-beginning)
		(point-min)))
	 (end (if (region-active-p)
		  (region-end)
		(point)))
	 (text (buffer-substring-no-properties beg end)))
    (ellama-stream
     (format
      ellama-code-complete-prompt-template
      text)
     :provider ellama-coding-provider
     :filter #'ellama--code-filter
     :point end)))

;;;###autoload
(defun ellama-code-add (description)
  "Generate and insert new code based on DESCRIPTION.
This function prompts the user to describe the code they want to generate.
If a region is active, it includes the selected text as context for code
generation."
  (interactive "sDescribe the code to be generated: ")
  (when (region-active-p)
    (ellama-context-add-selection))
  (ellama-stream
   (format
    ellama-code-add-prompt-template
    description)
   :provider ellama-coding-provider
   :filter #'ellama--code-filter))


;;;###autoload
(defun ellama-make-format (needed-format)
  "Render selected text or text in current buffer as NEEDED-FORMAT."
  (interactive "sSpecify required format: ")
  (let* ((beg (if (region-active-p)
		  (region-beginning)
		(point-min)))
	 (end (if (region-active-p)
		  (region-end)
		(point-max)))
	 (text (buffer-substring-no-properties beg end)))
    (kill-region beg end)
    (ellama-stream
     (format
      ellama-make-format-prompt-template
      needed-format text)
     :point beg)))

;;;###autoload
(defun ellama-make-list ()
  "Create markdown list from active region or current buffer."
  (interactive)
  (ellama-make-format ellama-make-list-prompt-template))

;;;###autoload
(defun ellama-make-table ()
  "Create markdown table from active region or current buffer."
  (interactive)
  (ellama-make-format ellama-make-table-prompt-template))

(defun ellama-summarize-webpage (url)
  "Summarize webpage fetched from URL.

Summarize the URL at point if `thing-at-point' is present, or using
`shr-url-at-point' if a URL is at point in modes like `eww' or `elfeed',
otherwise prompt user for URL to summarize."
  (interactive
   (list
    (if-let ((url (or (and (fboundp 'thing-at-point) (thing-at-point 'url))
                      (and (fboundp 'shr-url-at-point) (shr-url-at-point nil)))))
        url
      (read-string "Enter URL you want to summarize: "))))
  (let ((buffer-name (url-retrieve-synchronously url t)))
    ;; (display-buffer buffer-name)
    (with-current-buffer buffer-name
      (goto-char (point-min))
      (or (search-forward "<!DOCTYPE" nil t)
          (search-forward "<html" nil))
      (beginning-of-line)
      (kill-region (point-min) (point))
      (shr-insert-document (libxml-parse-html-region (point-min) (point-max)))
      (goto-char (point-min))
      (or (search-forward "<!DOCTYPE" nil t)
          (search-forward "<html" nil))
      (beginning-of-line)
      (kill-region (point) (point-max))
      (ellama-summarize))))

(defun ellama-make-semantic-similar-p-with-context (context)
  "Return function for checking semantic similarity of two texts in CONTEXT."
  (lambda (text1 text2)
    "Check if TEXT1 means the same as TEXT2."
    (plist-get
     (json-parse-string
      (llm-chat
       (or ellama-extraction-provider ellama-provider)
       (llm-make-chat-prompt
	(format ellama-semantic-identity-reasoning-template context text1 text2)
	:response-format '(:type object :properties
				 (:think (:type string)
					 :same (:type boolean))
				 :required ["think" "same"])))
      :object-type 'plist
      :false-object nil)
     :same)))

(defun ellama-semantic-similar-p (text1 text2)
  "Check if TEXT1 means the same as TEXT2."
  (plist-get
   (json-parse-string
    (llm-chat
     (or ellama-extraction-provider ellama-provider)
     (llm-make-chat-prompt
      (format ellama-semantic-identity-template text1 text2)
      :response-format '(:type object :properties
			       (:think (:type string)
				       :same (:type boolean))
			       :required ["think" "same"])))
    :object-type 'plist
    :false-object nil)
   :same))

(defun ellama--make-extract-string-list-prompt (elements input)
  "Create LLM prompt for list of ELEMENTS extraction from INPUT."
  (llm-make-chat-prompt
   input
   :context (format ellama-extract-string-list-template elements)
   :response-format '(:type object :properties
			    (:data (:type array :items (:type string)))
			    :required (data))))

(defun ellama-extract-string-list (elements input &rest args)
  "Extract list of ELEMENTS from INPUT syncronously.
Return list of strings.  ARGS contains keys for fine control.

:provider PROVIDER -- PROVIDER is an llm provider for generation."
  (let ((provider (or (plist-get args :provider)
		      ellama-extraction-provider
		      ellama-provider)))
    (plist-get (json-parse-string
		(llm-chat
		 provider
		 (ellama--make-extract-string-list-prompt elements input))
		:object-type 'plist
		:array-type 'list)
	       :data)))

(defun ellama-extract-string-list-async (elements callback input &rest args)
  "Extract list of ELEMENTS from INPUT asyncronously.
Call CALLBACK on result list of strings.  ARGS contains keys for fine control.

:provider PROVIDER -- PROVIDER is an llm provider for generation."
  (let ((provider (or (plist-get args :provider)
		      ellama-extraction-provider
		      ellama-provider)))
    (llm-chat-async
     provider
     (ellama--make-extract-string-list-prompt elements input)
     (lambda (res)
       (funcall callback
		(plist-get (json-parse-string
			    res
			    :object-type 'plist
			    :array-type 'list)
			   :data)))
     (lambda (err)
       (user-error err)))))

(defun ellama-get-ollama-model-names ()
  "Get ollama model names."
  (mapcar (lambda (s)
	    (car (split-string s)))
	  (seq-drop
	   (process-lines
	    (executable-find ellama-ollama-binary)
	    "ls")
	   ;; skip header line
	   1)))

(defun ellama-embedding-model-p (name)
  "Check if NAME is an embedding model."
  (when-let ((model (llm-models-match name)))
    (not (not (member 'embedding (llm-model-capabilities model))))))

(defun ellama-get-ollama-chat-model-names ()
  "Get ollama chat model names."
  (cl-remove-if #'ellama-embedding-model-p (ellama-get-ollama-model-names)))

(defun ellama-get-ollama-embedding-model-names ()
  "Get ollama embedding model names."
  (cl-remove-if-not #'ellama-embedding-model-p (ellama-get-ollama-model-names)))

(defun ellama-get-first-ollama-chat-model ()
  "Get first available ollama model."
  (declare-function make-llm-ollama "ext:llm-ollama")
  (when (executable-find ellama-ollama-binary)
    (require 'llm-ollama)
    (make-llm-ollama
     :chat-model
     (car (ellama-get-ollama-chat-model-names)))))

(defun ellama-get-ollama-model-name ()
  "Get ollama model name from installed locally."
  (interactive)
  (completing-read
   "Select ollama model: "
   (ellama-get-ollama-model-names)))

(defun ellama-get-ollama-local-model ()
  "Return llm provider for interactively selected ollama model."
  (interactive)
  (declare-function llm-ollama-p "ext:llm-ollama")
  (declare-function llm-ollama-host "ext:llm-ollama")
  (declare-function llm-ollama-port "ext:llm-ollama")
  (let ((model-name (ellama-get-ollama-model-name))
	(host (when (llm-ollama-p ellama-provider)
		(llm-ollama-host ellama-provider)))
	(port (when (llm-ollama-p ellama-provider)
		(llm-ollama-port ellama-provider))))
    (if host
	(make-llm-ollama
	 :chat-model model-name :embedding-model model-name :host host :port port)
      (make-llm-ollama
       :chat-model model-name :embedding-model model-name))))

(defvar ellama-transient-ollama-model-name "")
(defvar ellama-transient-temperature 0.7)
(defvar ellama-transient-context-length 4096)
(defvar ellama-transient-host "localhost")
(defvar ellama-transient-port 11434)

(transient-define-suffix ellama-transient-set-ollama-model ()
  "Set ollama model name."
  (interactive)
  (setq ellama-transient-ollama-model-name (ellama-get-ollama-model-name)))

(transient-define-suffix ellama-transient-set-temperature ()
  "Set temperature value."
  (interactive)
  (setq ellama-transient-temperature (read-number "Enter temperature: ")))

(transient-define-suffix ellama-transient-set-context-length ()
  "Set context length."
  (interactive)
  (setq ellama-transient-context-length (read-number "Enter context length: ")))

(transient-define-suffix ellama-transient-set-host ()
  "Set host address."
  (interactive)
  (setq ellama-transient-host (read-string "Enter host: ")))

(transient-define-suffix ellama-transient-set-port ()
  "Set port number."
  (interactive)
  (setq ellama-transient-port (read-number "Enter port: ")))

(defvar ellama-provider-list '(ellama-provider
			       ellama-coding-provider
			       ellama-translation-provider
			       ellama-extraction-provider
			       ellama-summarization-provider
			       ellama-naming-provider)
  "List of ollama providers.")

(transient-define-suffix ellama-transient-model-get-from-provider ()
  "Fill transient model from provider."
  (interactive)
  (ellama-fill-transient-ollama-model
   (eval (read
	  (completing-read "Select provider: "
			   (mapcar #'prin1-to-string ellama-provider-list))))))

(transient-define-suffix ellama-transient-model-get-from-current-session ()
  "Fill transient model from current session."
  (interactive)
  (when ellama--current-session-id
    (ellama-fill-transient-ollama-model
     (with-current-buffer (ellama-get-session-buffer ellama--current-session-id)
       (ellama-session-provider ellama--current-session)))))

(transient-define-suffix ellama-transient-set-provider ()
  "Set transient model to provider."
  (interactive)
  (let ((provider (read
		   (completing-read "Select provider: "
				    (mapcar #'prin1-to-string ellama-provider-list)))))
    (set provider
	 (ellama-construct-ollama-provider-from-transient))
    ;; if you change `ellama-provider' you probably want to start new chat session
    (when (equal provider 'ellama-provider)
      (setq ellama--current-session-id nil))))

(transient-define-prefix ellama-select-ollama-model ()
  "Select ollama model."
  [["Model"
    ("f" "Load from provider" ellama-transient-model-get-from-provider
     :transient t)
    ("F" "Load from current session" ellama-transient-model-get-from-current-session
     :description (lambda () (format "Load from current session (%s)" ellama--current-session-id))
     :transient t)
    ("m" "Set Model" ellama-transient-set-ollama-model
     :transient t
     :description (lambda () (format "Model (%s)" ellama-transient-ollama-model-name)))
    ("t" "Set Temperature" ellama-transient-set-temperature
     :transient t
     :description (lambda () (format "Temperature (%.2f)" ellama-transient-temperature)))
    ("c" "Set Context Length" ellama-transient-set-context-length
     :transient t
     :description (lambda () (format "Context Length (%d)" ellama-transient-context-length)))
    ("S" "Set provider" ellama-transient-set-provider
     :transient t)
    ("s" "Set provider and quit" ellama-transient-set-provider)]
   ["Connection"
    ("h" "Set Host" ellama-transient-set-host
     :transient t
     :description (lambda () (if ellama-transient-host
				 (format "Host (%s)" ellama-transient-host)
			       "Host")))
    ("p" "Set Port" ellama-transient-set-port
     :transient t
     :description (lambda () (if ellama-transient-port
				 (format "Port (%s)" ellama-transient-port)
			       "Port")))]
   ["Quit" ("q" "Quit" transient-quit-one)]])

(defun ellama-fill-transient-ollama-model (provider)
  "Set transient ollama model from PROVIDER."
  (declare-function llm-ollama-p "ext:llm-ollama")
  (declare-function llm-ollama-host "ext:llm-ollama")
  (declare-function llm-ollama-port "ext:llm-ollama")
  (declare-function llm-ollama-chat-model "ext:llm-ollama")
  (declare-function llm-ollama-default-chat-temperature "ext:llm-ollama")
  (declare-function llm-ollama-default-chat-non-standard-params "ext:llm-ollama")
  (when (llm-ollama-p provider)
    (setq ellama-transient-ollama-model-name (llm-ollama-chat-model provider))
    (setq ellama-transient-temperature (or (llm-ollama-default-chat-temperature provider) 0.7))
    (setq ellama-transient-host (llm-ollama-host provider))
    (setq ellama-transient-port (llm-ollama-port provider))
    (let* ((other-params (llm-ollama-default-chat-non-standard-params provider))
	   (ctx-len (when other-params (alist-get
					"num_ctx"
					(seq--into-list other-params)
					nil nil #'string=))))
      (setq ellama-transient-context-length (or ctx-len 4096)))))

(defun ellama-construct-ollama-provider-from-transient ()
  "Make provider with ollama mode in transient menu."
  (make-llm-ollama
   :chat-model ellama-transient-ollama-model-name
   :default-chat-temperature ellama-transient-temperature
   :host ellama-transient-host
   :port ellama-transient-port
   :default-chat-non-standard-params
   `[("num_ctx" . ,ellama-transient-context-length)]))

(transient-define-prefix ellama-transient-code-menu ()
  "Code Commands."
  [["Code Commands"
    ("c" "Complete" ellama-code-complete)
    ("a" "Add" ellama-code-add)
    ("e" "Edit" ellama-code-edit)
    ("i" "Improve" ellama-code-improve)
    ("r" "Review" ellama-code-review)
    ("m" "Generate Commit Message" ellama-generate-commit-message)]
   ["Quit" ("q" "Quit" transient-quit-one)]])

(transient-define-prefix ellama-transient-summarize-menu ()
  "Summarize Commands."
  [["Summarize Commands"
    ("s" "Summarize" ellama-summarize)
    ("w" "Summarize Webpage" ellama-summarize-webpage)
    ("k" "Summarize Killring" ellama-summarize-killring)]
   ["Quit" ("q" "Quit" transient-quit-one)]])

(transient-define-prefix ellama-transient-session-menu ()
  "Session Commands."
  [["Session Commands"
    ("l" "Load Session" ellama-load-session)
    ("r" "Rename Session" ellama-session-rename)
    ("d" "Delete Session" ellama-session-delete)
    ("a" "Activate Session" ellama-session-switch)
    ("k" "Kill Session" ellama-session-kill)]
   ["Quit" ("q" "Quit" transient-quit-one)]])

(transient-define-prefix ellama-transient-improve-menu ()
  "Improve Commands."
  [["Improve Commands"
    ("w" "Improve Wording" ellama-improve-wording)
    ("g" "Improve Grammar" ellama-improve-grammar)
    ("c" "Improve Conciseness" ellama-improve-conciseness)]
   ["Quit" ("q" "Quit" transient-quit-one)]])

(transient-define-prefix ellama-transient-make-menu ()
  "Make Commands."
  [["Make Commands"
    ("l" "Make List" ellama-make-list)
    ("t" "Make Table" ellama-make-table)
    ("f" "Make Format" ellama-make-format)]
   ["Quit" ("q" "Quit" transient-quit-one)]])

(transient-define-prefix ellama-transient-ask-menu ()
  "Ask Commands."
  [["Ask Commands"
    ("l" "Ask Line" ellama-ask-line)
    ("s" "Ask Selection" ellama-ask-selection)
    ("a" "Ask About" ellama-ask-about)]
   ["Quit" ("q" "Quit" transient-quit-one)]])

(transient-define-prefix ellama-transient-translate-menu ()
  "Translate Commands."
  [["Translate Commands"
    ("t" "Translate Text" ellama-translate)
    ("b" "Translate Buffer" ellama-translate-buffer)
    ("e" "Enable Translation" ellama-chat-translation-enable)
    ("d" "Disable Translation" ellama-chat-translation-disable)]
   ["Quit" ("q" "Quit" transient-quit-one)]])

(transient-define-prefix ellama-transient-context-menu ()
  "Context Commands."
  ["Context Commands"
   :description (lambda ()
		  (ellama-update-context-buffer)
		  (format "Current context:
%s" (with-current-buffer ellama-context-buffer
      (buffer-substring (point-min) (point-max)))))
   ["Add"
    ("b" "Add Buffer" ellama-context-add-buffer)
    ("d" "Add Directory" ellama-context-add-directory)
    ("f" "Add File" ellama-context-add-file)
    ("s" "Add Selection" ellama-context-add-selection)
    ("i" "Add Info Node" ellama-context-add-info-node)]
   ["Manage"
    ("m" "Manage context" ellama-manage-context)
    ("D" "Delete element" ellama-context-element-remove-by-name)
    ("r" "Context reset" ellama-context-reset)]
   ["Quit" ("q" "Quit" transient-quit-one)]])

(transient-define-prefix ellama-transient-main-menu ()
  "Main Menu."
  ["Main"
   [("c" "Chat" ellama-chat)
    ("B" "Chat with community blueprint" ellama-community-prompts-select-blueprint)]
   [("a" "Ask Commands" ellama-transient-ask-menu)
    ("C" "Code Commands" ellama-transient-code-menu)]]
  ["Text"
   [("w" "Write" ellama-write)
    ("P" "Proofread" ellama-proofread)
    ("k" "Text Complete" ellama-complete)
    ("g" "Text change" ellama-change)
    ("d" "Define word" ellama-define-word)]
   [("s" "Summarize Commands" ellama-transient-summarize-menu)
    ("i" "Improve Commands" ellama-transient-improve-menu)
    ("t" "Translate Commands" ellama-transient-translate-menu)
    ("m" "Make Commands" ellama-transient-make-menu)]]
  ["System"
   [("o" "Ollama model" ellama-select-ollama-model)
    ("p" "Provider selection" ellama-provider-select)]
   [("S" "Session Commands" ellama-transient-session-menu)
    ("x" "Context Commands" ellama-transient-context-menu)]]
  [["Problem solving"
    ("R" "Solve reasoning problem" ellama-solve-reasoning-problem)
    ("D" "Solve domain specific problem" ellama-solve-domain-specific-problem)]]
  [["Quit" ("q" "Quit" transient-quit-one)]])

;;;###autoload
(defun ellama-provider-select ()
  "Select ellama provider."
  (interactive)
  (let* ((ollama-binary (executable-find ellama-ollama-binary))
	 (providers (append
                     `(("default model" . ellama-provider)
		       ,(if (and ollama-binary
				 (file-exists-p ollama-binary))
			    '("ollama model" . (ellama-get-ollama-local-model))))
                     ellama-providers))
	 (variants (mapcar #'car providers)))
    (setq ellama-provider
	  (eval (alist-get
		 (completing-read "Select model: " variants)
		 providers nil nil #'string=)))
    (setq ellama--current-session-id nil)))

;;;###autoload
(defun ellama-chat-translation-enable ()
  "Enable chat translation."
  (interactive)
  (setq ellama-chat-translation-enabled t))

;;;###autoload
(defun ellama-chat-translation-disable ()
  "Enable chat translation."
  (interactive)
  (setq ellama-chat-translation-enabled nil))

(provide 'ellama)
;;; ellama.el ends here.
