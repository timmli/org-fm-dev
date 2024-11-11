;;; org-fm.el --- fast minutes with org-fm -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Timm Lichte

;; Author: Timm Lichte <timm.lichte@uni-tuebingen.de>
;; URL: https://github.com/timmli/org-fm-dev/blob/master/org-fm.el
;; Version: 0
;; Last modified: 2024-07-30 Tue 15:52:36
;; Package-Requires: ((org-mode "9"))
;; Keywords: Org

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
;; Run `org-fm-export-to-latex' to export a heading that contains the minutes.
;; Appropriate faces can be activated with `org-fm-minor-mode'.

;;; Code:

(require 'org)

;;====================
;;
;; General variables
;;
;;--------------------

(defvar org-fm-inline-question-regexp
  "\\(\\?\\(:\\||\\)\\)\\(.*?\\)\\(\\(:\\||\\)\\?\\)"
  "Regular expression for inline questions.
  Example: ?:inline question:?")

(defvar org-fm-inline-alert-regexp
  "\\(!\\(:\\||\\)\\)\\(.*?\\)\\(\\(:\\||\\)!\\)"
  "Regular expression for inline alerts.
  Example: !:inline alert:!")

(defvar org-fm-inline-comment-regexp
  "\\(#\\(:\\||\\)\\)\\(.*?\\)\\(\\(:\\||\\)#\\)"
  "Regular expression for inline comments.
  Example: #:inline comment:#")

(defvar org-fm-abbreviation-escape-symbol
  "##%s"
  "Format string for escaping abbreviations.")

(defun org-fm-make-regexp (cat)
  "Make regular expression for org-fm items of some category CAT."
  (concat
   "^\\([[:blank:]]*\\)\\([0-9]+\)\\|-\\)"
   "[[:blank:]]+\\("
   cat
   "\\(?:[[:blank:]]*\\("
   org-element--timestamp-regexp
   "\\)\\)?"
   "[[:blank:]]*\\(.*?\\)[[:blank:]]+\\(::\\|||\\)\\)[[:space:]]"))


;;====================
;;
;; Faces
;;
;;--------------------

(defface org-fm-agenda-face
  '((t ( :box t
         :foreground "red"
         :weight bold)))
  "Face for the agenda type of minutes items.")

(defface org-fm-cleared-agenda-face
  '((t ( :inherit org-checkbox-done-text
         :box t
         :weight bold)))
  "Face for the cleared type of minutes items.")

(defface org-fm-decision-face
  '((t ( :box t
         :foreground "LimeGreen"
         :weight bold)))
  "Face for the decision type of minutes items.")

(defface org-fm-information-face
  '((t ( :box t
         :foreground "CornflowerBlue"
         :weight bold)))
  "Face for the information type of minutes items.")

(defface org-fm-note-face
  '((t ( :box t
         :foreground "orange"
         :weight bold)))
  "Face for the note type of minutes items.")

(defface org-fm-question-face
  '((t ( :box t
         :foreground "orange"
         :weight bold)))
  "Face for the question type of minutes items.")

(defface org-fm-alert-face
  '((t ( :foreground "red"
         :weight bold)))
  "Face for the alert type of minutes items.")


(defface org-fm-comment-face
  '((t ( :inherit font-lock-comment-face
         :box t
         :weight bold)))
  "Face for the comment type of minutes items.")


;;====================
;;
;; Manipulate items
;;
;;--------------------

(defun org-fm-add-or-update-timestamp ()
  "Add or update timestamp in org-fm item at point."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (org-at-item-p)
        (if (looking-at (org-fm-make-regexp "\\([A-Z]+:\\)?"))
            (if (match-beginning 5)     ; update timestamp
                (progn (goto-char (match-beginning 5))
                       (delete-region (match-beginning 5) (match-end 5))
                       (org-insert-time-stamp (org-current-time) t t))
              (if (match-beginning 4)   ; insert new timestamp after cat
                  (progn (goto-char (match-end 4))
                         (insert " ")
                         (org-insert-time-stamp (org-current-time) t t))
                (if (match-beginning 2) ; insert new timestamp after mark
                    (progn (goto-char (match-end 2))
                           (insert " ")
                           (org-insert-time-stamp (org-current-time) t t)))))
          (progn (org-beginning-of-line) ; insert new timestamp and separator
                 (org-insert-time-stamp (org-current-time) t t)
                 (insert " :: ")
                 ))
      (message "org-fm: no item at point!"))))

(defun org-fm-clean-text ()
  "Remove or replace symbols that may cause troubles during a LaTeX
  export."
  (interactive)
  (save-excursion
    (let ((count1 0)
          (count2 0))
      ;; Remove zero-width space
      (goto-char (point-min))
      (while (re-search-forward "​" nil t)
        (replace-match "")
        (setq count1 (1+ count1)))
      ;; Replace quote symbols
      (goto-char (point-min))
      (while (re-search-forward "„\\|“\\|”" nil t)
        (replace-match "\"")
        (setq count2 (1+ count2)))
      ;; Replace quote symbols
      (goto-char (point-min))
      (while (re-search-forward "’" nil t)
        (replace-match "'")
        (setq count2 (1+ count2)))
      (message (format "org-fm-clean-text: removed %d chars, replaced %d chars" count1 count2))))) 


;;====================
;;
;; Particiant list
;;
;;--------------------

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
          (if name                      ; `name' must be non-nil
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
                           (mapcar 'car (cdr (org-list-to-lisp))))))))
          (case-fold-search nil)) ; make re-search-forward case sensitive
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


;;====================
;;
;; Export to HTML
;;
;;--------------------

;; It is notoriously unclear how to export lists with description items
;; (marked with "::"). The following tries to export them as regular
;; lists.

;; TODO: Use XML-DOM
;; (let* ((xml-dom-tree (with-temp-buffer
;;                        (insert data)
;;                        (libxml-parse-xml-region (point-min) (point-max)))))
;;   ...

(defun org-fm-html-description-list-to-regular-list (data)
  "Convert HTML description list in string DATA to regular list."
  (replace-regexp-in-string
   "<li>(no term) ::"
   "<li>"
   (replace-regexp-in-string
    "<dt>\\([^<]+\\)</dt>[[:space:]]*<dd>\\([^<]+\\)"
    "<li>\\1 :: \\2"
    (replace-regexp-in-string
     "</dd>"
     "</li>"
     (replace-regexp-in-string
      "<dl.*?>"
      "<ul class=\"org-ul\">"
      (replace-regexp-in-string
       "</dl>"
       "</ul>" 
       (replace-regexp-in-string
        "<li id=\"\\(.*?\\)\">"
        "<li>\\1 :: "
        data)))))))

(defun org-fm-html-participant-list (data)
  "Tweak participation/checkbox list in string DATA."
  (replace-regexp-in-string
   "<li.*?>\\(☐\\|☒\\)"
   "<li style=\"list-style-type:none\">\\1"
   (replace-regexp-in-string
    "<code>\\[&#xa0;\\]</code>"
    "☐"
    (replace-regexp-in-string
     "<code>\\[X\\]</code>"
     "☒"
     data))))

(defun org-fm-export-list-item-to-html (data backend info)
  "Process list item in DATA during HTML export. Note that this
  function is applied to each list item, not to the list as a whole."
  (when (org-export-derived-backend-p backend 'html)
    (org-fm-html-participant-list
     (org-fm-html-description-list-to-regular-list data)
     )))

;; Usage:
;; (add-to-list 'org-export-filter-plain-list-functions
;;              'org-fm-export-list-item-to-html)

(defun org-fm-html-adjust-style (data)
  "Adjust style of the first HTML list in DATA using a DOM generated
  with libxml."
  (if (libxml-available-p)
      (let* ((html-dom-tree (with-temp-buffer
                              (insert data)
                              ;; Remove inline comments
                              (save-excursion
                                (beginning-of-buffer)
                                (while (re-search-forward org-fm-inline-comment-regexp nil t)
                                  (replace-match "")))
                              ;; Generate DOM
                              (libxml-parse-html-region (point-min) (point-max))))
             (body-first-child (car (dom-children (car (dom-by-tag html-dom-tree 'body))))))

        ;; Participants: When ul, for each li with a box, append the text of the children in grey.
        (when (eq (dom-tag body-first-child) 'ul)
          (cl-loop
           for node in (dom-children body-first-child)
           when (eq (dom-tag (list node)) 'li) ; without (list node): Wrong type argument: listp, ""
           do (cl-loop
               for node-child in (dom-children node)
               ;; Trim string child 
               when (stringp node-child)
               do (progn (dom-add-child-before node
                                               (concat (string-trim node-child) " ")
                                               node-child)
                         (dom-remove-node html-dom-tree node-child))
               ;; Extract text from ul children
               when (eq (dom-tag (list node-child)) 'ul)
               do (progn
                    (dom-append-child node
                                      (concat "<span style=\"color:grey\">"
                                              "("
                                              (string-trim (dom-texts node-child))
                                              ")</span>"))
                    (dom-remove-node html-dom-tree node-child)))
           ;; Replace li with div
           when (eq (dom-tag (list node)) 'li)
           do (progn (dom-add-child-before (dom-parent html-dom-tree node)
                                           (concat "<div>" (dom-text node) "</div>")
                                           node)
                     (dom-remove-node html-dom-tree node))))

        ;; Colorize items
        (cl-loop
         for node in (dom-by-tag html-dom-tree 'li)
         for text-child = (nth 2 node)
         when (string-match "^[[:space:]]*\\(\\(\\(..?\\): \\)?.*::\\)\\( .*\\)" text-child)
         do (let* ((type (match-string 3 text-child))
                   (header (match-string 1 text-child))
                   (body (match-string 4 text-child))
                   (new-node (cond ((string= "A" type)
                                    (concat "<span style=\"color:red;font-weight:bold\">"
                                            header body "</span>"))
                                   ((or (string= "D" type)
                                        (string= "E" type))
                                    (concat "<span style=\"color:green;font-weight:bold\">"
                                            header body "</span>"))
                                   ((string= "AC" type)
                                    (concat "<span style=\"color:green;font-weight:bold\">"
                                            header "</span>" body))
                                   ((string= "N" type)
                                    (concat "<span style=\"color:orange;font-weight:bold\">"
                                            header "</span>" body))
                                   (t (concat "<span style=\"color:blue\">"
                                              header "</span>" body))
                                   )))
              (dom-add-child-before node
                                    new-node
                                    text-child)
              (dom-remove-node html-dom-tree text-child)))
        
        ;; Topics: When ol, highlight all li children
        (when (eq (dom-tag body-first-child) 'ol)
          (cl-loop
           for node in (dom-children body-first-child)
           when (eq (dom-tag (list node)) 'li) ; without (list node): Wrong type argument: listp, ""  
           do (cl-loop
               for node-child in (dom-children node)
               when (stringp node-child)
               do (progn
                    (dom-add-child-before node
                                          (concat
                                           "<span style=\""
                                           "font-weight:bold;"
                                           "background-color:rgb(192, 192, 192);"
                                           "text-decoration-line:underline;"
                                           "\">"
                                           (string-trim node-child) "</span>")
                                          node-child)
                    (dom-remove-node html-dom-tree node-child)))))

        (with-temp-buffer
		      (dom-print html-dom-tree)
		      (buffer-string)))
    (progn
      (message "Could not find libxml, skipping org-fm-html-adjust-style.")
      data)
    ))

(defun org-fm-export-whole-list-to-html (data backend info)
  (when (org-export-derived-backend-p backend 'html)
    (org-fm-html-adjust-style data)))

;; Usage:
;; (add-to-list 'org-export-filter-final-output-functions
;;              'org-fm-export-whole-list-to-html)

;;====================
;;
;; Export to LaTeX
;;
;;--------------------

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

(defun org-fm-delete-drawer (name)
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
  (org-fm-delete-drawer "LOGBOOK")         ; remove LOGBOOK
  (org-fm-delete-drawer "PROPERTIES")      ; remove PROPERTIES
  (org-deadline '(4))                   ; remove DEADLINE
  (org-schedule '(4))                   ; remove SCHEDULE
  ;; remove root heading
  (org-back-to-heading)
  (org-show-subtree)
  (kill-line))

(defun org-fm-replace-questions-with-latex ()
  "Replace all open questions with LaTeX command \OpenQuestion."
  (save-excursion
    (while (re-search-forward org-fm-inline-question-regexp nil t)
      (replace-match
       (let ((scope (match-string 3)))
         (concat "@@latex:\\\\OpenQuestion{@@" scope "@@latex:}@@"))))))

(defun org-fm-replace-inline-elements-with-latex ()
  "Replace all inline questions and comments with LaTeX commands."
  (save-excursion
    (while (re-search-forward org-fm-inline-question-regexp nil t)
      (replace-match
       (let ((scope (match-string 3)))
         (concat "@@latex:\\\\OpenQuestion{@@" scope "@@latex:}@@")))))
  (save-excursion
    (while (re-search-forward org-fm-inline-alert-regexp nil t)
      (replace-match
       (let ((scope (match-string 3)))
         (concat "@@latex:\\\\Alert{@@" scope "@@latex:}@@")))))
  (save-excursion
    (while (re-search-forward org-fm-inline-comment-regexp nil t)
      (replace-match
       (let ((scope (match-string 3)))
         (concat "@@latex:\\\\Comment{@@" scope "@@latex:}@@"))))))

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
             (concat "* "           ; no indentation
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
                          (concat " @@latex:\\\\NoteTag{@@" time "@@latex:}@@"))
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
       t )) ; fixed case
    ))

(defun org-fm-replace-untagged-with-latex ()
  "Replace all untagged topics with appropriate LaTeX commands."
  (save-excursion
    (while (re-search-forward "^[0-9]+\) " nil t)
      (replace-match "* "))))

(defun org-fm-convert-keywords-to-latex ()
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
                             ;;       (concat
                             ;;        (replace-match element-value nil t keyword-alist-input-value)
                             ;;        "\n")
                             ;;     (concat
                             ;;      keyword-alist-input-value
                             ;;      element-value
                             ;;      "\n"))
                             )
                           )))))
      keyword-alist-output)))

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
    (progn                     ; move to top of buffer and create a new line
      (goto-char (point-min))
      (end-of-line)
      (newline))
    (cl-loop
     for key in (mapcar 'car org-fm-keywords-latex-alist)
     do (when (assoc key keyword-latex-alist)
          (insert (car (cdr (assoc key keyword-latex-alist))))))
    ;; process & delete drawer PARTICPANTS-LIST
    (insert
     (let ((participants-list (org-fm-convert-participants-list))
           (keyword-latex (car (cdr (assoc "MINUTES_PARTICIPANTS" org-fm-keywords-latex-alist)))))
       (concat (format keyword-latex participants-list) "\n")
       ;; (when (string-match "%s"
       ;;                      keyword-latex)
       ;;    (concat
       ;;     (replace-match participants-list
       ;;                    nil t
       ;;                    keyword-latex)
       ;;     "\n")))
       ))
    (org-fm-delete-drawer "PARTICIPANTS-LIST")))

(defun org-fm-export-to-latex ()
  "Export minutes in org-fm format to LaTeX.
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
        (org-fm-insert-latex-header (org-fm-convert-keywords-to-latex))
        ;; process inline elements
        (org-fm-replace-inline-elements-with-latex)
        ;; process tagged items
        (org-fm-replace-tags-with-latex)
        ;; process untagged topics
        (org-fm-replace-untagged-with-latex)
        ;; dispatch export and go back to original buffer
        (deactivate-mark)
        (org-export-dispatch)
        (switch-to-buffer buffer)
        ))))


;;====================
;;
;; Minor mode
;;
;;--------------------

(define-minor-mode org-fm-minor-mode
  "Minor mode for org-fm. This minor mode makes available
some useful faces for highlighting the type and assignment of
org-fm items."
  :lighter " fm"

  ;; Appearence
  (font-lock-add-keywords
   'org-mode
   `((,org-fm-inline-question-regexp
      (1 '(font-lock-comment-face))
      (3 '(org-fm-question-face))
      (4 '(font-lock-comment-face)))
     (,org-fm-inline-alert-regexp
      (1 '(font-lock-comment-face))
      (3 '(org-fm-alert-face))
      (4 '(font-lock-comment-face)))
     (,org-fm-inline-comment-regexp
      (1 '(font-lock-comment-face))
      (3 '(org-fm-comment-face))
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
      (3 '(org-fm-note-face) prepend))
     (,(org-fm-make-regexp "Q:")
      (3 '(org-fm-question-face) prepend)))
   'append)

  )

;; Usage:
;; (add-hook 'org-mode-hook 'org-fm-minor-mode)


(provide 'org-fm)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; org-fm.el ends here
