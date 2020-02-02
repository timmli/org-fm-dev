;;; org-minutes.el --- faster minutes with org-minutes -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Timm Lichte

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use,
;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following
;; conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:
;; This package helps to write and export minutes that are written in
;; the org-minutes format.  The org-minutes format uses an org-list
;; where items can be typed or assigned a participant.

;;; Example:
;; 1) John, Kim, Sue, Alexandra :: A first TOP
;;    - A: Peter :: Something to do.
;;    - D: :: Decision made
;; 2) A second TOP
;;    - I: Sue :: Some information
;;    - AC: :: Cleared agenda item
;;    - ?:Open question regarding minutes:?
;;    - N: This is a note outside the protocol.

;;; Usage:
;; Run `org-mintes-export' to export a heading that contains minutes.
;; Appropriate faces can be activated with `org-minutes-minor-mode'.

;;; Code:

(require 'org)
(require 'loop)

(defvar org-minutes-question-regexp
	"\\(\\?\\(:\\||\\)\\)\\(.*?\\)\\(\\(:\\||\\)\\?\\)")

(defun org-minutes-make-regexp (cat)
	"Make regular expression for some category CAT of org-minutes items."
	(concat
	 "^\\([[:blank:]]*\\)\\([0-9]+\)\\|-\\)[[:blank:]]+\\("
	 cat
	 "[[:blank:]]*\\(.*?\\)[[:blank:]]+\\(::\\|||\\)\\) "))

(defvar org-minutes-keywords-latex-alist
	'(("MINUTES_TITLE" "#+TITLE: %s")
		("MINUTES_AUTHOR" "#+AUTHOR: %s")
		("MINUTES_LANGUAGE" "#+LATEX_HEADER: \\newcommand{\\minuteslanguage}{%s}")
		("MINUTES_LATEX_STYLE" "#+LATEX_HEADER: \\input{%s}")
		("MINUTES_CHAIR" "#+LATEX_HEADER: \\chair{%s}")
		("MINUTES_EVENT" "#+LATEX_HEADER: \\event{%s}")
		("MINUTES_PLACE" "#+LATEX_HEADER: \\place{%s}")
		("MINUTES_PARTICIPANTS" "#+LATEX_HEADER: \\participants{%s}")
		("MINUTES_DATE" "#+DATE: %s")
		("MINUTES_DRAFT_TEXT" "#+LATEX_HEADER: \\drafttext{%s}")
		("MINUTES_OPTIONS" "#+OPTIONS: %s")
		)
	"Alist for mapping org-minutes keywords to LaTeX commands.
The order in ORG-MINUTES-KEYWORDS-ALIST determines the order of the inserted LaTeX header.")

(defun org-drawer-delete (name)
  "Delete all drawers in buffer with title NAME.
Inspired by: https://emacs.stackexchange.com/a/38367/12336"
  (interactive)
  (save-excursion
		(beginning-of-buffer)
    (while
				(save-match-data
					(if (search-forward-regexp org-drawer-regexp nil t) ; do not emit an error when there is no match
							(goto-char (match-beginning 1)))
					(looking-at name))
			(org-mark-element)
			(delete-region (region-beginning) (region-end))
			(org-remove-empty-drawer-at (point)))))

(defun org-minutes-clean-heading ()
	"Clean up current heading."
	(org-drawer-delete "LOGBOOK")					; remove LOGBOOK
	(org-drawer-delete "PROPERTIES")			; remove PROPERTIES
	(org-deadline '(4))										; remove DEADLINE
	(org-schedule '(4))										; remove SCHEDULE
	;; remove root heading
	(org-back-to-heading)
	(org-show-subtree)
	(kill-line)
	)

(defun org-minutes-replace-questions-with-latex ()
	"Replace all open questions with LaTeX command \OpenQuestion."
	(save-excursion
		(while (re-search-forward org-minutes-question-regexp nil t)
			(replace-match
			 (let ((scope (match-string 3)))
				 (concat "@@latex:\\\\OpenQuestion{@@" scope "@@latex:}@@"))))))

(defun org-minutes-replace-tags-with-latex ()
	"Replace all item tags with appropriate LaTeX commands."
	(save-excursion
		(while (re-search-forward
						(org-minutes-make-regexp "\\(A:\\|AC:\\|B:\\|C:\\|D:\\|E:\\|I:\\|N:\\|\\[ \\]\\|\\[X\\]\\)?")
						nil t)
			(replace-match
			 (let ((indentation (match-string 1))
						 (mark (match-string 2))
						 (cat (match-string 4))
						 (name (match-string 5))
						 (separator (match-string 6)))
				 (if (string= indentation "")
						 (concat "* " 					; no indentation
										 (if (stringp name)
												 (concat " @@latex:\\\\texorpdfstring{\\\\InformationTagTOPMargin{@@"
																 name "@@latex:}}{}@@")))
					 (concat indentation mark ; with identation
									 (cond ((or (string= cat "A:") (string= cat "[ ]"))
													(concat " @@latex:\\\\ActionTag@@"))
												 ((or (string= cat "AC:") (string= cat "[X]"))
													(concat " @@latex:\\\\ClearedTag@@"))
												 ((string= cat "E:")
													(concat " @@latex:\\\\EntscheidungTag@@"))
												 ((string= cat "D:")
													(concat " @@latex:\\\\DecisionTag@@"))
												 ((or (string= cat "I:"))
													(concat " @@latex:\\\\InformationTag@@"))
												 ((or (string= cat "C:") (string= cat "B:"))
													(concat " @@latex:\\\\ConsultationTag@@"))
												 ((string= cat "N:")
													(concat " @@latex:\\\\NoteTag@@"))
												 (t " @@latex:\\\\NoTag@@")
												 )
									 (concat "@@latex:{@@" name "@@latex:}@@"
													 "@@latex:{@@" separator "@@latex:}@@")
									 (cond ((or (string= cat "A:") (string= cat "[ ]"))
													(concat " @@latex:\\\\ActionTagMargin@@@@latex:{@@"
																	name
																	"@@latex:}@@"))
												 ((or (string= cat "AC:") (string= cat "[X]"))
													(concat " @@latex:\\\\ClearedTagMargin@@@@latex:{@@ "
																	name
																	"@@latex:}@@"))
												 ((string= cat "E:")
													(concat " @@latex:\\\\EntscheidungTagMargin@@@@latex:{@@ "
																	name
																	"@@latex:}@@"))
												 ((string= cat "D:")
													(concat " @@latex:\\\\DecisionTagMargin@@@@latex:{@@ "
																	name
																	"@@latex:}@@"))
												 ((or (string= cat "I:"))
													(concat " @@latex:\\\\InformationTagMargin@@@@latex:{@@ "
																	name
																	"@@latex:}@@"))
												 ((or (string= cat "C:") (string= cat "B:"))
													(concat " @@latex:\\\\ConsultationTagMargin@@@@latex:{@@ "
																	name
																	"@@latex:}@@"))
												 ))))
			 t ))	; fixed case
		))

(defun org-minutes-replace-untagged-with-latex ()
	"Replace all untagged topics with appropriate LaTeX commands."
	(save-excursion
		(while (re-search-forward "^[0-9]+\) " nil t)
			(replace-match "* "))))

(defun org-minutes-convert-keywords ()
	"Collect all #+MINUTES keywords and convert them to LaTeX commands."
	(save-excursion
		(let ((keyword-alist-input org-minutes-keywords-latex-alist)
					(keyword-alist-output))
			(while (re-search-forward "\\#\\+MINUTES" nil t)
				(let* ((element-key (org-element-property :key (org-element-at-point)))
							 (element-value (org-element-property :value (org-element-at-point))))
					(when (assoc element-key keyword-alist-input)
						(let ((keyword-alist-input-value (car (cdr (assoc element-key keyword-alist-input)))))
							(add-to-list 'keyword-alist-output
													 `(,element-key
														 ,(concat
															 (format keyword-alist-input-value element-value)
															 "\n")
														 ;; ,(if (string-match "###" keyword-alist-input-value)
														 ;; 			(concat
														 ;; 			 (replace-match element-value nil t keyword-alist-input-value)
														 ;; 			 "\n")
														 ;; 		(concat
														 ;; 		 keyword-alist-input-value
														 ;; 		 element-value
														 ;; 		 "\n"))
														 )
													 )))))
			keyword-alist-output
			)))

(defun org-minutes-convert-participants-list ()
	"Convert drawer PARTICIPANTS-LIST to LaTeX string."
	(save-excursion
		(if (re-search-forward ":PARTICIPANTS-LIST:" nil t)
				(progn (forward-line 1)
							 (beginning-of-line)
							 (if (eq (car (org-element-at-point)) 'plain-list)
									 (replace-regexp-in-string "\n" "" (org-list-to-latex (org-list-to-lisp)))
								 ""))
			""
			)))

(defun org-minutes-insert-latex-header (keyword-latex-alist)
	"Insert LaTeX-related header using KEYWORD-LATEX-ALIST."
	(save-excursion
		(progn 										 ; move to top of buffer and create a new line
			(goto-char (point-min))
			(end-of-line)
			(newline))
		(loop-for-each key (mapcar 'car org-minutes-keywords-latex-alist)
			(when (assoc key keyword-latex-alist)
				(insert (car (cdr (assoc key keyword-latex-alist))))))
		;; process & delete drawer PARTICPANTS-LIST
		(insert
		 (let ((participants-list (org-minutes-convert-participants-list))
					 (keyword-latex (car (cdr (assoc "MINUTES_PARTICIPANTS" org-minutes-keywords-latex-alist)))))
			 (concat (format keyword-latex participants-list) "\n")
			 ;; (when (string-match "%s"
			 ;; 										 keyword-latex)
			 ;; 	 (concat
			 ;; 		(replace-match participants-list
			 ;; 									 nil t
			 ;; 									 keyword-latex)
			 ;; 		"\n")))
			 ))
		(org-drawer-delete "PARTICIPANTS-LIST")
		))

(defun org-minutes-export ()
	"Export minutes in org-minutes format.
This function uses the regular `org-export-dispatcher'."
	(interactive)
	(save-excursion
		(progn
			(org-back-to-heading)
			(set-mark-command nil)
			(org-next-visible-heading 1)
			(if (org-at-heading-p) (previous-line)))
		(when (use-region-p)
			(copy-to-buffer "*Minutes*" (region-beginning) (region-end))
			(deactivate-mark)
			(let ((buffer (buffer-name)))
				(switch-to-buffer "*Minutes*")
				(org-mode)
				(beginning-of-buffer)
				;; clean up root heading
				(org-minutes-clean-heading)
				;; process document attributes
				(org-minutes-insert-latex-header (org-minutes-convert-keywords))
				;; process open questions
				(org-minutes-replace-questions-with-latex)
				;; process tagged items
				(org-minutes-replace-tags-with-latex)
				;; process untagged topics
				(org-minutes-replace-untagged-with-latex)
				;; dispatch export and go back to original buffer
				(deactivate-mark)
				(org-export-dispatch)
				(switch-to-buffer buffer)
				)
			)))


(defface org-minutes-agenda-face
	'((t (
				:box t
				:foreground "red"
				:weight bold)))
	"Face for the agenda type of minutes items.")

(defface org-minutes-cleared-agenda-face
	'((t (
				:inherit org-checkbox-done-text
				:box t
				:weight bold)))
	"Face for the cleared type of minutes items.")

(defface org-minutes-decision-face
	'((t (
				:box t
				:foreground "LimeGreen"
				:weight bold)))
	"Face for the decision type of minutes items.")

(defface org-minutes-information-face
	'((t (
				:box t
				:foreground "CornflowerBlue"
				:weight bold)))
	"Face for the information type of minutes items.")

(defface org-minutes-note-face
	'((t (
				:box t
				:foreground "orange"
				:weight bold)))
	"Face for the note type of minutes items.")

(defface org-minutes-question-face
	'((t (
				:box t
				:foreground "orange"
				:weight bold)))
	"Face for the question type of minutes items.")

(define-minor-mode org-minutes-minor-mode
	"Minor mode for org-minutes. This minor mode makes available
some useful faces for highlighting the type and assignment of
org-minutes items."
	:lighter " om"

	(font-lock-add-keywords
	 'org-mode
	 `((,org-minutes-question-regexp
			(1 '(font-lock-comment-face))
			(3 '(org-minutes-question-face))
			(4 '(font-lock-comment-face)))
		 (,(org-minutes-make-regexp "\\(A:\\|\\[ \\]\\)")
			(3 '(org-minutes-agenda-face)))
		 (,(org-minutes-make-regexp "\\(AC:\\|\\[X\\]\\)")
			(3 '(org-minutes-cleared-agenda-face)))
		 (,(org-minutes-make-regexp "D:")
			(3 '(org-minutes-decision-face)))
		 (,(org-minutes-make-regexp "E:")
			(3 '(org-minutes-decision-face)))
		 (,(org-minutes-make-regexp "N:")
			(3 '(org-minutes-note-face)))
		 (,(org-minutes-make-regexp "\\(B:\\|C:\\|I:\\)?")
			(3 '(org-minutes-information-face))))
	 )

	)

;; (add-hook 'org-mode-hook 'org-minutes-minor-mode)

(provide 'org-minutes)

;;; org-minutes.el ends here
