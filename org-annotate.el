;;; org-annotate.el --- Inline-note link syntax for Org  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Eric Abrahamsen

;; Author: Eric Abrahamsen <eric@ericabrahamsen.net>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides a new link type for Org that allows you to create
;; annotations on arbitrary chunks of text.  The link prefix is
;; "note:".

;; Add notes with `org-annotate-add-note'.  Following the link will
;; display the text of the note in a pop-up buffer.  The buffer is in
;; special-mode, hit "q" to dismiss it.

;; Call `org-annotate-display-notes' to see all notes in a buffer.
;; Press "?" in this buffer to see more options.

;; Customize how notes are exported in different backends by setting
;; the `org-annotate-[backend]-export-function' options, where
;; "backend" is a valid backend identifier.  Each option should point
;; to a function that accepts two arguments, the path and description
;; strings of the link, and returns a single formatted string for
;; insertion in the exported text.  Some default functions are
;; provided for HTML, LaTeX and ODT, see the `org-annotate-export-*'
;; functions.

;; Todo:

;; 1. Is it possible to have multi-line filled tabular list items?
;; Long notes are not very useful if you can't see the whole thing.

;; 2. Maybe a minor mode for ease of manipulating notes?

;;; With thanks to John Kitchin for getting the ball rolling, and
;;; contributing code:
;;; http://kitchingroup.cheme.cmu.edu/blog/2015/04/24/Commenting-in-org-files/

;;; Code:

(require 'org)
(require 'cl-lib)
(require 'tabulated-list)

(if (fboundp 'org-link-set-parameters)
    (org-link-set-parameters "note"
                             :follow #'org-annotate-display-note
                             :export #'org-annotate-export-note
                             :activate-func #'org-annotate-activate-note)
  (org-add-link-type "note" #'org-annotate-display-note #'org-annotate-export-note))

(defgroup org-annotate nil
  "Annotation link type for Org."
  :tag "Org Annotations"
  :group 'org)

(defcustom org-annotate-display-buffer "*Org Annotation*"
  "Name of the buffer for temporary display of note text."
  :group 'org-annotate
  :type 'string)

(defcustom org-annotate-list-buffer-prefix "*Org Annotation List: "
  "Common prefix for buffers displaying notes in an Org
buffer/subtree."
  :group 'org-annotate
  :type 'string)

(defcustom org-annotate-list-table-buffer "*Org Annotations Table*"
  "Name of buffer for temporary \"export\" of note list
buffers to Org table format."
  :group 'org-annotate
  :type 'string)

(defcustom org-annotate-export-functions
  '((html . org-annotate-export-html-tooltip)
    (latex . org-annotate-export-latex-marginpar)
    (odt . org-annotate-export-odt-comment))
  "Alist mapping org-export backends to export functions."
  :group 'org-annotate
  :type '(alist :key-type (symbol :tag "Backend")
                :value-type function))


(defcustom org-annotate-special-brackets nil
  "Brackets used for display of annotation boundaries.
If non-nil, it should be a list of three strings to be used as
left bracket, middle separator, and right bracket."
  :type '(choice
	      (const :tag "None" nil)
	      (list string string string)))

(defun org-annotate-export-html-tooltip (path desc)
  (format "<font color=\"red\"><abbr title=\"%s\" color=\"red\">COMMENT</abbr></font> %s" path (or desc "")))

(defun org-annotate-export-latex-todonote (path desc)
  (format "%s\\todo{%s}" (or desc "") path))

(defun org-annotate-export-latex-marginpar (path desc)
  (format "%s\\marginpar{%s}" (or desc "") path))

(defun org-annotate-export-latex-footnote (path desc)
  (format "%s\\footnote{%s}" (or desc "") path))

(defun org-annotate-export-odt-comment (path desc)
  (format (if desc
              (let ((an-name (concat "__Annot_" (number-to-string (random)))))
                (format "<office:annotation office:name=\"%s\"><dc:creator>%%s</dc:creator><dc:date>%%s</dc:date><text:list><text:list-item><text:p>%s</text:p></text:list-item></text:list></office:annotation>%s<office:annotation-end office:name=\"%s\"/>"
                        an-name path desc an-name))
            (format "<office:annotation><dc:creator>%%s</dc:creator><dc:date>%%s</dc:date><text:list><text:list-item><text:p>%s</text:p></text:list-item></text:list></office:annotation>"
                    path))
          (user-full-name)
          (let ((ct (current-time)))
            (concat (format-time-string "%FT%T." ct) (number-to-string (nth 2 ct))))))

(defun org-annotate-export-note (path desc format)
  (let ((export-func
         (alist-get format org-annotate-export-functions)))
    (if (and export-func
	         (fboundp export-func))
	    (funcall export-func path desc)
      ;; If there's no function to handle the note, just delete it.
      (or desc ""))))

(defun org-annotate-display-note (linkstring)
  (when linkstring
    (with-current-buffer
	(get-buffer-create org-annotate-display-buffer)
      (let ((inhibit-read-only t))
	(erase-buffer)
	(insert linkstring)))
    (display-buffer-below-selected
     (get-buffer-create org-annotate-display-buffer)
     '(nil (window-height . fit-window-to-buffer)))
    (select-window (get-buffer-window org-annotate-display-buffer) t)
    (special-mode)
    (local-set-key (kbd "w") #'org-annotate-display-copy)))

(defun org-annotate-display-copy ()
  "Used within the special-mode buffers popped up using
`org-annotate-display-note', to copy the text of the note to the
kill ring.  Bound to \"w\" in those buffers."
  (interactive)
  (copy-region-as-kill
   (point-min)
   (save-excursion
     (goto-char (point-max))
     (skip-chars-backward " \n\t")
     (point))))

;;;###autoload
(defun org-annotate-add-note ()
  (interactive)
  (cond
   ((and (org-in-regexp org-link-bracket-re 1)
         (equal "note" (car (split-string (match-string 1) ":"))))
    (org-insert-link)) ; edit note
   ((use-region-p)
    (let ((selected-text
           (buffer-substring (region-beginning) (region-end))))
      (setf (buffer-substring (region-beginning) (region-end))
            (org-link-make-string
             (concat "note:" (read-string "Note: "))
             selected-text))))
   (t (insert (org-link-make-string
               (concat "note:" (read-string "Note: ")))))))

;; The purpose of making this buffer-local is defeated by the fact
;; that we only have one *Org Annotations List* buffer!
(defvar-local org-annotate-notes-source nil
  "Buffer/marker pair pointing to the source of notes for a
  given note-list buffer.")

;;;###autoload
(defun org-annotate-display-notes (arg)
  "Display all notes in the current buffer (or, with a prefix
arg, in the current subtree) in a tabulated list form."
  (interactive "P")
  (let* ((source-buf (current-buffer))
	 (marker (when arg
		   (save-excursion
		     (org-back-to-heading t)
		     (point-marker))))
	 (list-buf (get-buffer-create
		    (concat org-annotate-list-buffer-prefix
			    (buffer-name source-buf)
			    (if marker
				(concat "-"
					(number-to-string
					 (marker-position marker)))
			      "") "*"))))
    (switch-to-buffer-other-window list-buf)
    (unless (eq major-mode 'org-annotate-list-mode)
      (org-annotate-list-mode)
      (setq org-annotate-notes-source (cons source-buf marker)))
    (org-annotate-refresh-list)))

(defun org-annotate-collect-links ()
  "Do the work of finding all the notes in the current buffer
or subtree."
  (when org-annotate-notes-source
    (with-current-buffer (car org-annotate-notes-source)
      (save-restriction
	(widen)
	(let* ((marker (cdr org-annotate-notes-source))
	       (beg (or marker (point-min)))
	       (end (if marker
			(save-excursion
			  (goto-char marker)
			  (outline-next-heading)
			  (point))
		      (point-max)))
	       links)
	  (goto-char beg)
	  (while (re-search-forward org-link-bracket-re end t)
	    (let ((path (match-string-no-properties 1))
		  (text (match-string-no-properties 2))
		  start)
	      (when (string-match-p "\\`note:" path)
		(setq path
		      (org-link-unescape
		       (replace-regexp-in-string
			"\n+" " "
			(replace-regexp-in-string "\\`note:" "" path))))
		(setq text (if text
			       (org-link-unescape
				(replace-regexp-in-string "\n+" " " text))
			     "[no text]"))
		;; "start" (ie point at the beginning of the link), is
		;; used as the list item id in the tabular view, for
		;; finding specific notes.
		(setq start
		      (save-excursion
			(goto-char
			 (org-element-property :begin (org-element-context)))
			(point-marker)))
		;; The format required by tabular list mode.
		(push (list start (vector text path)) links))))
	  (when links
	    (reverse links)))))))

(defun org-annotate-refresh-list ()
  (let ((links (org-annotate-collect-links))
	(max-width 0))
    (if links
	(progn
	  (dolist (l links)
	    (setq max-width
		  (max max-width
		       (string-width (aref (cadr l) 0)))))
	  (setq tabulated-list-entries links
		tabulated-list-format
		(vector `("Text" ,(min max-width 40) t) '("Note" 40 t)))
	  (tabulated-list-init-header)
	  (tabulated-list-print))
      (message "No notes found")
      nil)))

(defvar org-annotate-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "o") #'org-annotate-list-display)
    (define-key map (kbd "O") #'org-annotate-list-pop-to)
    (define-key map (kbd "d") #'org-annotate-list-delete)
    (define-key map (kbd "t") #'org-annotate-list-to-table)
    map)
  "Local keymap for Org annotations list buffers.")

(define-derived-mode org-annotate-list-mode
    tabulated-list-mode "Org Annotations"
  "Mode for viewing Org notes as a tabular list.

\\<org-annotate-list-mode-map>
\\{org-annotate-list-mode-map}"
  (setq tabulated-list-sort-key nil)
  (add-hook 'tabulated-list-revert-hook
	    #'org-annotate-refresh-list nil t))

(defun org-annotate-list-pop-to ()
  (interactive)
  (let ((dest-marker (tabulated-list-get-id)))
    (switch-to-buffer-other-window (marker-buffer dest-marker))
    (goto-char dest-marker)))

(defun org-annotate-list-display ()
  (interactive)
  (let ((dest-marker (tabulated-list-get-id)))
    (display-buffer (marker-buffer dest-marker))
    (set-window-point
     (get-buffer-window (marker-buffer dest-marker))
     dest-marker)))

(defun org-annotate-list-delete ()
  (interactive)
  (let ((dest-marker (tabulated-list-get-id)))
    (display-buffer (marker-buffer dest-marker))
    (save-window-excursion
      (org-annotate-list-pop-to)
      (org-annotate-delete-note))
    (unless (org-annotate-refresh-list)
      (quit-window))))

(defun org-annotate-delete-note ()
  "Delete the note at point."
  (interactive)
  (let* ((elm (org-element-context))
	 (note-begin (org-element-property :begin elm))
	 (note-end (org-element-property :end elm))
	 (space-at-end (save-excursion
			 (goto-char note-end)
			 (looking-back " " (- (point) 2)))))

    (unless (string= (org-element-property :type elm) "note")
      (error "Not on a note"))

    (setf (buffer-substring note-begin note-end)
	  (cond
	   ;; The link has a description. Replace link with description
	   ((org-element-property :contents-begin elm)
	    (concat (buffer-substring
		     (org-element-property :contents-begin elm)
		     (org-element-property :contents-end elm))
		    (if space-at-end " " "")))
	   ;; No description. just delete the note
	   (t
	    "")))))

(defun org-annotate-list-to-table ()
  (interactive)
  (let ((entries
	 (mapcar
	  (lambda (e)
	    (list (aref (cadr e) 0) (aref (cadr e) 1)))
	  tabulated-list-entries))
	(source org-annotate-notes-source))
    (switch-to-buffer-other-window org-annotate-list-table-buffer)
    (erase-buffer)
    (insert "* Notes from " (buffer-name (car source)) "\n\n")
    (dolist (e entries)
      (insert (car e) "\t" (cadr e) "\n"))
    (org-mode)
    (org-table-convert-region
     (save-excursion
       (org-back-to-heading t)
       (forward-line 2)
       (point))
     (point) "\t")
    (org-reveal)))


;; * Font locking note links via activation function
;; (John Kitchin would approve)

(defface org-annotate-face
  `((t (:inherit org-link
        :weight bold)))
  "Face for note links in org-mode.")

(defface org-annotate-text-face
  '((t (:inherit default)))
  "Face for inline text of note links in org-mode.")

(defface org-annotate-bracket-face
  '((t (:inherit default)))
  "Face for visible brackets of note links in org mode")

(defun org-annotate-activate-note (start end _path bracketp)
  "Add text properties to display annotation links in a special way"
  (when (and org-annotate-special-brackets bracketp)
    (save-match-data
      (save-excursion
        (goto-char start)
        (when (looking-at org-link-bracket-re)
          (add-text-properties start (+ 2 start)
                               `(invisible nil
                                           face org-annotate-bracket-face
                                           display ,(nth 0 org-annotate-special-brackets)))
          (add-text-properties (- end 2) end
                               `(invisible nil
                                           face org-annotate-bracket-face
                                           display ,(nth 2 org-annotate-special-brackets)))
          (add-text-properties (match-beginning 1) (match-end 1)
                               '(invisible nil face org-annotate-face))
          (when (match-end 2) ; with desc
            (add-text-properties (match-beginning 2) (match-end 2)
                                 '(invisible nil face org-annotate-text-face))
            (add-text-properties (match-end 1) (match-beginning 2)
                                 `(invisible nil
                                             face org-annotate-bracket-face
                                             display ,(nth 1 org-annotate-special-brackets)))))))))

;; * Org-mode menu
(defun org-annotate-org-menu ()
  "Add org-annotate menu to the Org menu."

  (easy-menu-change
   '("Org") "Annotations"
   '( ["Insert note" org-annotate-add-note]
      ["Delete note" org-annotate-delete-note]
      ["List notes" org-annotate-display-notes]
      "--"
      )
   "Show/Hide")

  (easy-menu-change '("Org") "--" nil "Show/Hide"))

(add-hook 'org-mode-hook 'org-annotate-org-menu)

(provide 'org-annotate)
;;; org-annotate.el ends here
