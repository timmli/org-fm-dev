(defun org-drawer-delete (name)
  "Delete all drawers in buffer with title NAME.
Inspired by: https://emacs.stackexchange.com/a/38367/12336
"
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

(defvar org-minutes-question-regexp
	"\\(\\?\\(:\\||\\)\\)\\(.*?\\)\\(\\(:\\||\\)\\?\\)")

(defun org-minutes-clean-heading ()
	"Clean up current heading"
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
	(while (re-search-forward org-minutes-question-regexp nil t)
		(replace-match
		 (let ((scope (match-string 3)))
			 (concat "@@latex:\\\\OpenQuestion{@@" scope "@@latex:}@@")))))

(defun org-minutes-replace-tags-with-latex ()
	"Replace all item tags with appropriate LaTeX commands."
	(while (re-search-forward "^\\([[:blank:]]*\\)\\([0-9]+\)\\|-\\)[[:blank:]]+\\(A:\\|C:\\|E:\\|D:\\|I:\\|\\[ \\]\\)?[[:blank:]]*\\(.*?\\)[[:blank:]]+\\(::\\|||\\)" nil t)
		(replace-match
		 (let ((indentation (match-string 1))
					 (mark (match-string 2))
					 (cat (match-string 3))
					 (name (match-string 4))
					 (separator (match-string 5)))
			 (if (string= indentation "") 
					 (concat "* " 					; no indentation
									 (if (stringp name)
											 (concat " @@latex:\\\\InformationTagMargin{@@"
															 name "@@latex:}@@")))
				 (concat indentation mark ; with identation
								 (cond ((or (string= cat "A:") (string= cat "[ ]"))
												(concat " @@latex:\\\\ActionTag@@"))
											 ((string= cat "C:")
												(concat " @@latex:\\\\ClearedTag@@"))
											 ((string= cat "E:")
												(concat " @@latex:\\\\EntscheidungTag@@"))
											 ((string= cat "D:")
												(concat " @@latex:\\\\DecisionTag@@"))
											 ((string= cat "I:")
												(concat " @@latex:\\\\InformationTag@@"))
											 )
								 (concat "@@latex:{@@" name "@@latex:}@@"
												 "@@latex:{@@" separator "@@latex:}@@")
								 (cond ((or (string= cat "A:") (string= cat "[ ]"))
												(concat " @@latex:\\\\ActionTagMargin@@@@latex:{@@"
																name
																"@@latex:}@@"))
											 ((string= cat "C:")
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
											 ((string= cat "I:")
												(concat " @@latex:\\\\InformationTagMargin@@@@latex:{@@ "
																name
																"@@latex:}@@")))))) 
		 t ))	; fixed case	
	)

(defun org-minutes-replace-untagged-with-latex ()
	"Replace all untagged topics with appropriate LaTeX commands."
	(while (re-search-forward "^[0-9]+\) " nil t)
		(replace-match "* ")))

(defconst org-minutes-keywords-alist
	'(("MINUTES-AUTHOR" "\\author")
		("MINUTES-DATE" "\\date"))
	"ORG-MINUTES-KEYWORDS-ALIST contains a mapping from org-minutes keywords to LaTeX commands.
The order in ORG-MINUTES-KEYWORDS-ALIST determines the order of the LaTeX header.")

(defun org-minutes-convert-keywords-to-latex ()
	"Collect all #+MINUTES keywords and convert them to LaTeX commands."
	(goto-char (point-min))
	(let ((keyword-alist-input org-minutes-keywords-alist)
				(keyword-alist-output))
		(while (re-search-forward "\\#\\+MINUTES" nil t)
			(let* ((element-key (org-element-property :key (org-element-at-point)))
						 (element-value (org-element-property :value (org-element-at-point))))
				(when (assoc element-key keyword-alist-input)
					(add-to-list 'keyword-alist-output
											 `(,element-key
												 ,(concat "#+LATEX: "
																	(car (cdr (assoc element-key keyword-alist-input)))
																	"{"
																	"\n"
																	element-value
																	"\n"
																	"#+LATEX:}"))
											 ))))
		keyword-alist-output
		)
	)

(defun org-minutes-insert-latex-header (keyword-latex-alist)
	""
	(goto-char (point-min)) 
	(end-of-line)
	(newline)
	(loop-for-each key (mapcar 'car org-minutes-keywords-alist)
		(when (assoc key keyword-latex-alist)
			(insert (car (cdr (assoc key keyword-latex-alist))))))
	)

(defun org-minutes-export ()
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
				;; clean up root heading
				(org-minutes-clean-heading)
				;; TODO: process document attributes
				(beginning-of-buffer)
				(org-minutes-replace-document-attributes-with-latex)
				;; process open questions
				(beginning-of-buffer)
				(org-minutes-replace-questions-with-latex)
				;; process tagged items
				(beginning-of-buffer)
				(org-minutes-replace-tags-with-latex)
				;; process untagged topics
				(beginning-of-buffer)
				(org-minutes-replace-untagged-with-latex)
				;; dispatch export and go back to original buffer
				(deactivate-mark)
				(org-export-dispatch)
				(switch-to-buffer buffer)
				)			
			)))
