(defun tl/org-drawer-delete (name)
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

(defun tl/org-minutes-make-regexp (cat)
	(concat
	 "^\\([[:blank:]]*\\)\\([0-9]+\)\\|-\\)[[:blank:]]+\\(\\("
	 cat
	 "\\)[[:blank:]]*\\(.*?\\)[[:blank:]]+\\(::\\|||\\)\\)"))

(defvar tl/org-minutes-question-regexp
	"\\(\\?\\(:\\||\\)\\)\\(.*?\\)\\(\\(:\\||\\)\\?\\)")

(defun tl/org-minutes-export ()
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
				(tl/org-drawer-delete "LOGBOOK")
				(tl/org-drawer-delete "PROPERTIES")
				(org-deadline '(4))
				(org-schedule '(4))
				(org-back-to-heading)
				(org-show-subtree)
				(kill-line)
				;; open questions
				(beginning-of-buffer)
				(while (re-search-forward tl/org-minutes-question-regexp nil t)
					(replace-match
					 (let ((scope (match-string 3)))
						 (concat "@@latex:\\\\colorbox{orange}{?}{\\\\color{orange}" scope "}@@"))))
				;; tagged items
				(beginning-of-buffer)
				(while (re-search-forward "^\\([[:blank:]]*\\)\\([0-9]+\)\\|-\\)[[:blank:]]+\\(A:\\|C:\\|E:\\|D:\\|I:\\|\\[ \\]\\)?[[:blank:]]*\\(.*?\\)[[:blank:]]+\\(::\\|||\\)" nil t)
					(replace-match
					 (let ((indentation (match-string 1))
								 (mark (match-string 2))
								 (cat (match-string 3))
								 (name (match-string 4))
								 (separator (match-string 5)))
						 (if (string= indentation "") 
								 (concat "* " 					; no indentation
												 (if (and (stringp name))
														 (concat " @@latex:\\\\protect\\\\marginpar{\\\\colorbox{blue!20}{I}@@ "
																		 name "@@latex:}@@")))
							 (concat indentation mark ; with identation
											 (cond ((or (string= cat "A:") (string= cat "[ ]"))
															(concat " @@latex:\\\\colorbox{red}{A}@@"))
														 ((string= cat "C:")
															(concat " @@latex:\\\\colorbox{green!20!black}{C}@@"))
														 ((string= cat "E:")
															(concat " @@latex:\\\\colorbox{green}{E}@@"))
														 ((string= cat "D:")
															(concat " @@latex:\\\\colorbox{green}{D}@@"))
														 ((string= cat "I:")
															(concat " @@latex:\\\\colorbox{blue!20}{I}@@"))
														 )
											 (concat " " name " " "$" separator "$")
											 (cond ((or (string= cat "A:") (string= cat "[ ]"))
															(concat " @@latex:\\\\marginpar{\\\\colorbox{red}{A}@@ "
																			name "@@latex:}@@"))
														 ((string= cat "C:")
															(concat " @@latex:\\\\marginpar{\\\\colorbox{green!20!black}{C}@@ "
																			name "@@latex:}@@"))
														 ((string= cat "E:")
															(concat " @@latex:\\\\marginpar{\\\\colorbox{green}{E}@@ "
																			name "@@latex:}@@"))
														 ((string= cat "D:")
															(concat " @@latex:\\\\marginpar{\\\\colorbox{green}{D}@@ "
																			name "@@latex:}@@"))
														 ((string= cat "I:")
															(concat " @@latex:\\\\marginpar{\\\\colorbox{blue!20}{I}@@ "
																			name "@@latex:}@@")))))) 
					 t ))	; fixed case
				;; untagged topics
				(beginning-of-buffer)
				(while (re-search-forward "^[0-9]+\) " nil t)
					(replace-match "* "))
				(deactivate-mark)
				(org-export-dispatch)
				(switch-to-buffer buffer)
				)			
			)))
