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

(defconst org-minutes-keywords-latex-alist
	'(("MINUTES_TITLE" "#+TITLE: ###")
		("MINUTES_AUTHOR" "#+AUTHOR: ###")
		("MINUTES_LATEX_STYLE" "#+LATEX_HEADER: \\input{###}")
		("MINUTES_CHAIR" "#+LATEX_HEADER: \\chair{###}")
		("MINUTES_EVENT" "#+LATEX_HEADER: \\event{###}")
		("MINUTES_PLACE" "#+LATEX_HEADER: \\place{###}")
		("MINUTES_PARTICIPANTS" "#+LATEX_HEADER: \\participants{###}")
		("MINUTES_DATE" "#+DATE: ###")
		("MINUTES_DRAFT_TEXT" "#+LATEX_HEADER: \\SetWatermarkText{###}")
		("MINUTES_OPTIONS" "#+OPTIONS: ###")
		)
	"ORG-MINUTES-KEYWORDS-ALIST contains a mapping from org-minutes keywords to LaTeX commands.
The order in ORG-MINUTES-KEYWORDS-ALIST determines the order of the inserted LaTeX header.")

(defun org-minutes-convert-keywords ()
	"Collect all #+MINUTES keywords and convert them to LaTeX commands."
	(goto-char (point-min))
	(let ((keyword-alist-input org-minutes-keywords-latex-alist)
				(keyword-alist-output))
		(while (re-search-forward "\\#\\+MINUTES" nil t)
			(let* ((element-key (org-element-property :key (org-element-at-point)))
						 (element-value (org-element-property :value (org-element-at-point))))
				(when (assoc element-key keyword-alist-input) 
					(let ((keyword-alist-input-value (car (cdr (assoc element-key keyword-alist-input)))))
						(add-to-list 'keyword-alist-output
												 `(,element-key
													 ,(if (string-match "###" keyword-alist-input-value)
																(concat
																 (replace-match element-value nil t keyword-alist-input-value)
																 "\n")
															(concat 
															 keyword-alist-input-value
															 element-value
															 "\n")))
												 )))))
		keyword-alist-output
		))

(defun org-minutes-convert-participants-list ()
	"TODO"
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
	""
	(progn 										 ; move to top of buffer and create a new line
		(goto-char (point-min)) 
		(end-of-line)
		(newline))
	(loop-for-each key (mapcar 'car org-minutes-keywords-latex-alist)
		(when (assoc key keyword-latex-alist)
			(insert (car (cdr (assoc key keyword-latex-alist))))))
	(insert
	 (let ((participants-list (org-minutes-convert-participants-list))
				 (keyword-latex (car (cdr (assoc "MINUTES_PARTICIPANTS" org-minutes-keywords-latex-alist)))))
		 ;;;; This is a hack
		 ;; (concat
		 ;; 	"#+BEGIN_EXPORT latex: \n \\participants{"
		 ;; 	participants-list
		 ;; 	"}\n#+END_EXPORT"
		 ;; 	)
		 ;;;; This does not work because keyword-latex contains line breaks:
		 (when (string-match "###"
		 										 keyword-latex)
		 	 (concat
		 		(replace-match participants-list
		 									 nil t
		 									 keyword-latex)
		 		"\n"
		 		)
		 	 )
		 ))
	(org-drawer-delete "PARTICIPANTS-LIST") ; FIXME: should be moved elsewhere
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
				;; process document attributes
				(beginning-of-buffer)
				(org-minutes-insert-latex-header (org-minutes-convert-keywords))
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
