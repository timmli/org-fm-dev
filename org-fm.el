;;; org-fm.el --- fast minutes with org-fm -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Timm Lichte

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
;; the org-fm format.  The org-fm format uses an org-list
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
;; Run `org-fm-export' to export a heading that contains the minutes.
;; Appropriate faces can be activated with `org-fm-minor-mode'.

;;; Code:

(require 'org)
(require 'loop)

(defvar org-fm-question-regexp
	"\\(\\?\\(:\\||\\)\\)\\(.*?\\)\\(\\(:\\||\\)\\?\\)")

(defvar org-fm-abbreviation-escape-symbol
	"##%s")

(defun org-fm-make-regexp (cat)
	"Make regular expression for some category CAT of org-fm items."
	(concat
	 "^\\([[:blank:]]*\\)\\([0-9]+\)\\|-\\)"
	 "[[:blank:]]+\\("
	 cat
	 "\\(?:[[:blank:]]*\\("
	 org-element--timestamp-regexp
	 "\\)\\)?"
	 "[[:blank:]]*\\(.*?\\)[[:blank:]]+\\(::\\|||\\)\\) "))

(defvar org-fm-keywords-latex-alist
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
	"Alist for mapping org-fm keywords to LaTeX commands.
The order in ORG-FM-KEYWORDS-ALIST determines the order of the inserted LaTeX header.")

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

(defun org-fm-clean-heading ()
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

(defun org-fm-replace-questions-with-latex ()
	"Replace all open questions with LaTeX command \OpenQuestion."
	(save-excursion
		(while (re-search-forward org-fm-question-regexp nil t)
			(replace-match
			 (let ((scope (match-string 3)))
				 (concat "@@latex:\\\\OpenQuestion{@@" scope "@@latex:}@@"))))))

(defun org-fm-replace-tags-with-latex ()
	"Replace all item tags with appropriate LaTeX commands."
	(save-excursion
		(while (re-search-forward
						(org-fm-make-regexp "\\(A:\\|AC:\\|B:\\|C:\\|D:\\|E:\\|I:\\|N:\\|\\[ \\]\\|\\[X\\]\\)?")
						nil t)
			(replace-match
			 (let ((indentation (match-string 1))
						 (mark (match-string 2))
						 (cat (match-string 4))
						 (time (match-string 5))
						 (name (match-string 7))
						 (separator (match-string 8)))
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

(defun org-fm-replace-untagged-with-latex ()
	"Replace all untagged topics with appropriate LaTeX commands."
	(save-excursion
		(while (re-search-forward "^[0-9]+\) " nil t)
			(replace-match "* "))))

(defun org-fm-convert-keywords ()
	"Collect all #+MINUTES keywords and convert them to LaTeX commands."
	(save-excursion
		(let ((keyword-alist-input org-fm-keywords-latex-alist)
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

(defun org-fm-convert-participants-list ()
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

(defun org-fm-insert-latex-header (keyword-latex-alist)
	"Insert LaTeX-related header using KEYWORD-LATEX-ALIST."
	(save-excursion
		(progn 										 ; move to top of buffer and create a new line
			(goto-char (point-min))
			(end-of-line)
			(newline))
		(loop-for-each key (mapcar 'car org-fm-keywords-latex-alist)
			(when (assoc key keyword-latex-alist)
				(insert (car (cdr (assoc key keyword-latex-alist))))))
		;; process & delete drawer PARTICPANTS-LIST
		(insert
		 (let ((participants-list (org-fm-convert-participants-list))
					 (keyword-latex (car (cdr (assoc "MINUTES_PARTICIPANTS" org-fm-keywords-latex-alist)))))
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

(defun org-fm-make-abbreviation-hash (partlist)
	"Convert a list of of participants (strings) to a hash of abbreviations to names."
	(let ((abrv-hash (make-hash-table :test 'equal)))
		(dolist (participant partlist)
			(when (string-match "^[[:blank:]]*\\[\\(?:X\\| \\)\\][[:blank:]]+\\(.*?\\)[[:blank:]]+\\[\\(.*?\\)\\(?:=\\(.*?\\)\\)?\\]" participant)
				(let* ((fullname (string-trim (match-string 1 participant)))
							 (abbrev (string-trim (match-string 2 participant)))
							 (name
								(or (match-string 3 participant)
										(if (or
												 (string-match "^\\(.*?\\),[ ]+" fullname)
												 (string-match "[ ]+\\(.*?\\)$" fullname))
												(match-string 1 fullname)))))
					(if name											; `name' must be non-nil
							(puthash abbrev name abrv-hash))
					)))
		abrv-hash))

(defun org-fm-expand-abbreviations ()
	"Fetch abbreviation hash from the PARTICIPANTS-LIST 
and replace abbreviations with names in the subsequent org-fm items."
	(save-excursion
		(setq counter 0) 
		(let ((abrv-hash
					 (if (re-search-forward ":PARTICIPANTS-LIST:" nil t)
							 (progn (forward-line 1)
											(beginning-of-line)
											(if (eq (car (org-element-at-point)) 'plain-list)
													(org-fm-make-abbreviation-hash 
													 (mapcar 'car (cdr (org-list-to-lisp)))))))))
			(if (re-search-forward "^[[:blank:]]*[0-9]+)[[:blank:]]" nil t)
					(dolist (key (hash-table-keys abrv-hash))
						(save-excursion
							(while (re-search-forward
											(concat "\\(" (word-search-regexp key) "\\)")
											nil t)
								(if (not (looking-back (format org-fm-abbreviation-escape-symbol key))) ; escape abbreviation expansion 
										(replace-match (gethash key abrv-hash) t))
								(setq counter (+ counter 1))))))
			(message "org-fm: %d abbreviation(s) replaced." counter))))

(defun org-fm-remove-abbreviations-from-partlist ()
	"Remove the first pair of square brackets of a participant."
	(save-excursion
		(if (re-search-forward ":PARTICIPANTS-LIST:" nil t)
				(progn
					(forward-line 1)
					(beginning-of-line)
					(org-narrow-to-element)
					(while (re-search-forward "^[[:blank:]]*-[[:blank:]]+\\[.\\][[:blank:]]+.*?\\[.*?\\]" nil t)
						(looking-back "\\[.*?\\]")
						(replace-match ""))
					(org-toggle-narrow-to-subtree)))))

(defun org-fm-remove-abbreviation-escape ()
	"Remove the symbol for escaping abbreviation expansion in org-fm items."
	(save-excursion
		(if (re-search-forward "^[[:blank:]]*[0-9]+)[[:blank:]]" nil t)
				(while (search-forward (format org-fm-abbreviation-escape-symbol "") nil t)
					(replace-match "")))))


(defun org-fm-export ()
	"Export minutes in org-fm format.
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
				;; replace abbreviations with names
				(org-fm-expand-abbreviations)
				(org-fm-remove-abbreviations-from-partlist)
				(org-fm-remove-abbreviation-escape)
				;; clean up root heading
				(org-fm-clean-heading)
				;; process document attributes
				(org-fm-insert-latex-header (org-fm-convert-keywords))
				;; process open questions
				(org-fm-replace-questions-with-latex)
				;; process tagged items
				(org-fm-replace-tags-with-latex)
				;; process untagged topics
				(org-fm-replace-untagged-with-latex)
				;; dispatch export and go back to original buffer
				(deactivate-mark)
				(org-export-dispatch)
				(switch-to-buffer buffer)
				))))

(defun org-fm-add-or-update-timestamp ()
	"Add or update timestamp in org-fm item at point."
	(interactive)
	(save-excursion
		(beginning-of-line)
		(if (org-at-item-p)
				(if (looking-at (org-fm-make-regexp "\\([A-Z]+:\\)?"))
						(if (match-beginning 5)			; update timestamp
								(progn (goto-char (match-beginning 5))
											 (delete-region (match-beginning 5) (match-end 5))
											 (org-insert-time-stamp (org-current-time) t t))
							(if (match-beginning 4)		; insert new timestamp after cat
									(progn (goto-char (match-end 4))
												 (insert " ")
												 (org-insert-time-stamp (org-current-time) t t))
								(if (match-beginning 2)	; insert new timestamp after mark
										(progn (goto-char (match-end 2))
													 (insert " ")
													 (org-insert-time-stamp (org-current-time) t t)))))
					(progn (org-beginning-of-line) ; insert new timestamp and separator
								 (org-insert-time-stamp (org-current-time) t t)
								 (insert " :: ")
								 ))
			(message "org-fm: no item at point!"))))

(defface org-fm-agenda-face
	'((t (
				:box t
				:foreground "red"
				:weight bold)))
	"Face for the agenda type of minutes items.")

(defface org-fm-cleared-agenda-face
	'((t (
				:inherit org-checkbox-done-text
				:box t
				:weight bold)))
	"Face for the cleared type of minutes items.")

(defface org-fm-decision-face
	'((t (
				:box t
				:foreground "LimeGreen"
				:weight bold)))
	"Face for the decision type of minutes items.")

(defface org-fm-information-face
	'((t (
				:box t
				:foreground "CornflowerBlue"
				:weight bold)))
	"Face for the information type of minutes items.")

(defface org-fm-note-face
	'((t (
				:box t
				:foreground "orange"
				:weight bold)))
	"Face for the note type of minutes items.")

(defface org-fm-question-face
	'((t (
				:box t
				:foreground "orange"
				:weight bold)))
	"Face for the question type of minutes items.")

(define-minor-mode org-fm-minor-mode
	"Minor mode for org-fm. This minor mode makes available
some useful faces for highlighting the type and assignment of
org-fm items."
	:lighter " om"

	(font-lock-add-keywords
	 'org-mode
	 `((,org-fm-question-regexp
			(1 '(font-lock-comment-face))
			(3 '(org-fm-question-face))
			(4 '(font-lock-comment-face)))
		 (,(org-fm-make-regexp "\\(B:\\|C:\\|I:\\)?")
			(3 '(org-fm-information-face) prepend))
		 (,(org-fm-make-regexp "\\(A:\\|\\[ \\]\\)")
			(3 '(org-fm-agenda-face) prepend))
		 (,(org-fm-make-regexp "\\(AC:\\|\\[X\\]\\)")
			(3 '(org-fm-cleared-agenda-face) prepend))
		 (,(org-fm-make-regexp "D:")
			(3 '(org-fm-decision-face) prepend))
		 (,(org-fm-make-regexp "E:")
			(3 '(org-fm-decision-face) prepend))
		 (,(org-fm-make-regexp "N:")
			(3 '(org-fm-note-face) prepend)))
	 'append)

	)

;; (add-hook 'org-mode-hook 'org-fm-minor-mode)

(provide 'org-fm)

;;; org-fm.el ends here
