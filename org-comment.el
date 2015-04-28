;;; org-comment.el --- Comment-type link syntax for Org  -*- lexical-binding: t; -*-

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

;; Provides a new link type for Org that allows you to create comments
;; on arbitrary chunks of text.  The link prefix is "comment:".

;; Add comments with `org-comment-add-comment'.  Following the link
;; will display the text of the comment in a pop-up buffer.  The
;; buffer is in special-mode, hit "q" to dismiss it.

;; Call `org-comment-display-comments' to see all comments in a
;; buffer.  Press "?" in this buffer to see more options.

;; Customize how comments are exported in different backends by
;; setting the `org-comment-[backend]-export-function' options, where
;; "backend" is a valid backend identifier.  Each option should point
;; to a function that accepts two arguments, the path and description
;; strings of the link, and returns a single formatted string for
;; insertion in the exported text.  Some default functions are
;; provided for HTML, LaTeX and ODT, see the `org-comment-export-*'
;; functions.

;; Todo:

;; 1. Is it possible to have multi-line filled tabular list items?
;; Long comments are not very useful if you can't see the whole thing.

;; 2. Maybe a minor mode for ease of manipulating comments?

;; 3. Minor mode for the pop-up display buffer, inheriting from special-mode.

;;; With thanks to John Kitchin for getting the ball rolling, and
;;; contributing code:
;;; http://kitchingroup.cheme.cmu.edu/blog/2015/04/24/Commenting-in-org-files/

;;; Code:

(require 'org)
(require 'cl-lib)
(require 'tabulated-list)

(org-add-link-type
 "comment"
 #'org-comment-display-comment
 #'org-comment-export-comment)

(defgroup org-comment nil
  "Comment link type for Org."
  :tag "Org Comments"
  :group 'org)

(defcustom org-comment-display-buffer "*Org Comment*"
  "Name of the buffer for temporary display of comment text."
  :group 'org-comment
  :type 'string)

(defcustom org-comment-list-buffer-prefix "*Org Comment List: "
  "Common prefix for buffers displaying comments in an Org
buffer/subtree."
  :group 'org-comment
  :type 'string)

(defcustom org-comment-list-table-buffer "*Org Comment Table*"
  "Name of buffer for temporary \"export\" of comment list
buffers to Org table format."
  :group 'org-comment
  :type 'string)

(defcustom org-comment-html-export-function
  #'org-comment-export-html-tooltip
  "The HTML export style for Org comments, as a symbol. Currently
only supports tooltip."
  :group 'org-comment
  :type 'function)

(defcustom org-comment-latex-export-function
  #'org-comment-export-latex-marginpar
  "The LaTeX export style for Org comments, as a
symbol. Currently supports marginpar, todonote, and footnote."
  :group 'org-comment
  :type 'function)

(defcustom org-comment-odt-export-function
  #'org-comment-export-odt-comment
  "The ODT export style for Org comments, as a symbol.  Currently
only supports comment."
  :group 'org-comment
  :type 'function)

(defun org-comment-export-html-tooltip (path desc)
  (format "<font color=\"red\"><abbr title=\"%s\" color=\"red\">COMMENT</abbr></font> %s" path (or desc "")))

(defun org-comment-export-latex-todonote (path desc)
  (format "%s\\todo{%s}" (or desc "") path))

(defun org-comment-export-latex-marginpar (path desc)
  (format "%s\\marginpar{%s}" (or desc "") path))

(defun org-comment-export-latex-footnote (path desc)
  (format "%s\\footnote{%s}" (or desc "") path))

(defun org-comment-export-odt-comment (path desc)
  (format "%s<office:annotation><dc:creator>%s</dc:creator><dc:date>%s</dc:date><text:p>%s</text:p></office:annotation>"
	  desc "I made this!"
	  (format-time-string "%FT%T%z" (current-time))
	  path))

(defun org-comment-export-comment (path desc format)
  (let ((export-func
	 (intern-soft (format "org-comment-%s-export-function" format))))
    (if (and export-func
	     (fboundp export-func))
	(funcall export-func path desc)
      ;; If there's no function to handle the comment, just delete it.
      desc)))

(defun org-comment-display-comment (linkstring)
  (when linkstring
    (with-current-buffer
	(get-buffer-create org-comment-display-buffer)
      (let ((inhibit-read-only t))
	(erase-buffer)
	(insert linkstring)))
    (display-buffer-below-selected
     (get-buffer-create org-comment-display-buffer)
     '(nil (window-height . fit-window-to-buffer)))
    (select-window (get-buffer-window org-comment-display-buffer) t)
    (special-mode)
    (local-set-key (kbd "w") #'org-comment-display-copy)))

(defun org-comment-display-copy ()
  "Used within the special-mode buffers popped up using
`org-comment-display-comment', to copy the text of the comment to
the kill ring.  Bound to \"w\" in those buffers."
  (interactive)
  (copy-region-as-kill
   (point-min)
   (save-excursion
     (goto-char (point-max))
     (skip-chars-backward " \n\t")
     (point))))

;;;###autoload
(defun org-comment-add-comment ()
  (interactive)
  (if (use-region-p)
      (let ((selected-text
	     (buffer-substring (region-beginning) (region-end))))
        (setf (buffer-substring (region-beginning) (region-end))
              (format "[[comment:%s][%s]]"
                      (read-string "Comment: ") selected-text)))
  (insert (format "[[comment:%s]]" (read-string "Comment: ")))))

;; The purpose of making this buffer-local is defeated by the fact
;; that we only have one *Org Comment List* buffer!
(defvar-local org-comment-comments-source nil
  "Buffer/marker pair pointing to the source of comments for a
  given comment-list buffer.")

;;;###autoload
(defun org-comment-display-comments (arg)
  "Display all comments in the current buffer (or, with a prefix
arg, in the current subtree) in a tabulated list form."
  (interactive "P")
  (let* ((source-buf (current-buffer))
	 (marker (when arg
		   (save-excursion
		     (org-back-to-heading t)
		     (point-marker))))
	 (list-buf (get-buffer-create
		    (concat org-comment-list-buffer-prefix
			    (buffer-name source-buf)
			    (if marker
				(concat "-"
					(number-to-string
					 (marker-position marker)))
			      "") "*"))))
    (switch-to-buffer-other-window list-buf)
    (unless (eq major-mode 'org-comment-list-mode)
      (org-comment-list-mode)
      (setq org-comment-comments-source (cons source-buf marker)))
    (org-comment-refresh-list)))

(defun org-comment-collect-links ()
  "Do the work of finding all the comments in the current buffer
or subtree."
  (when org-comment-comments-source
    (with-current-buffer (car org-comment-comments-source)
      (save-restriction
	(widen)
	(let* ((marker (cdr org-comment-comments-source))
	       (beg (or marker (point-min)))
	       (end (if marker
			(save-excursion
			  (goto-char marker)
			  (outline-next-heading)
			  (point))
		      (point-max)))
	       links)
	  (goto-char beg)
	  (while (re-search-forward org-bracket-link-regexp end t)
	    (let ((path (match-string-no-properties 1))
		  (text (match-string-no-properties 3))
		  start)
	      (when (string-match-p "\\`comment:" path)
		(setq path
		      (org-link-unescape
		       (replace-regexp-in-string
			"\n+" " "
			(replace-regexp-in-string "\\`comment:" "" path))))
		(setq text (if text
			       (org-link-unescape
				(replace-regexp-in-string "\n+" " " text))
			     "[no text]"))
		;; "start" (ie point at the beginning of the link), is
		;; used as the list item id in the tabular view, for
		;; finding specific comments.
		(setq start
		      (save-excursion
			(goto-char
			 (org-element-property :begin (org-element-context)))
			(point-marker)))
		;; The format required by tabular list mode.
		(push (list start (vector text path)) links))))
	  (when links
	    (reverse links)))))))

(defun org-comment-refresh-list ()
  (let ((links (org-comment-collect-links))
	(max-width 0))
    (if links
	(progn
	  (dolist (l links)
	    (setq max-width
		  (max max-width
		       (string-width (aref (cadr l) 0)))))
	  (setq tabulated-list-entries links
		tabulated-list-format
		(vector `("Text" ,(min max-width 40) t) '("Comment" 40 t)))
	  (tabulated-list-init-header)
	  (tabulated-list-print))
      (message "No comments found")
      nil)))

(defvar org-comment-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "o") #'org-comment-list-display)
    (define-key map (kbd "O") #'org-comment-list-pop-to)
    (define-key map (kbd "d") #'org-comment-list-delete)
    (define-key map (kbd "t") #'org-comment-list-to-table)
    map)
  "Local keymap for Org comments list buffers.")

(define-derived-mode org-comment-list-mode
    tabulated-list-mode "Org Comments"
  "Mode for viewing Org comments as a tabular list.

\\<org-comment-list-mode-map>
\\{org-comment-menu-mode-map}"
  (setq tabulated-list-sort-key nil)
  (add-hook 'tabulated-list-revert-hook
	    #'org-comment-refresh-list))

(defun org-comment-list-pop-to ()
  (interactive)
  (let ((dest-marker (tabulated-list-get-id)))
    (switch-to-buffer-other-window (marker-buffer dest-marker))
    (goto-char dest-marker)))

(defun org-comment-list-display ()
  (interactive)
  (let ((dest-marker (tabulated-list-get-id)))
    (display-buffer (marker-buffer dest-marker))
    (set-window-point
     (get-buffer-window (marker-buffer dest-marker))
     dest-marker)))

(defun org-comment-list-delete ()
  (interactive)
  (let ((dest-marker (tabulated-list-get-id)))
    (display-buffer (marker-buffer dest-marker))
    (save-window-excursion
      (org-comment-list-pop-to)
      (org-comment-delete-comment))
    (unless (org-comment-refresh-list)
      (quit-window))))

(defun org-comment-delete-comment ()
  "Delete the comment at point."
  (interactive)
  (let* ((elm (org-element-context))
	 (comment-begin (org-element-property :begin elm))
	 (comment-end (org-element-property :end elm))
	 (space-at-end (save-excursion
			 (goto-char comment-end)
			 (looking-back " " (- (point) 2)))))

    (unless (string= (org-element-property :type elm) "comment")
      (error "Not on a comment"))

    (setf (buffer-substring comment-begin comment-end)
	  (cond
	   ;; The link has a description. Replace link with description
	   ((org-element-property :contents-begin elm)
	    (concat (buffer-substring
		     (org-element-property :contents-begin elm)
		     (org-element-property :contents-end elm))
		    (if space-at-end " " "")))
	   ;; No description. just delete the comment
	   (t
	    "")))))

(defun org-comment-list-to-table ()
  (interactive)
  (let ((entries
	 (mapcar
	  (lambda (e)
	    (list (aref (cadr e) 0) (aref (cadr e) 1)))
	  tabulated-list-entries))
	(source org-comment-comments-source))
    (switch-to-buffer-other-window org-comment-list-table-buffer)
    (erase-buffer)
    (insert "* Comments from " (buffer-name (car source)) "\n\n")
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

;; * John Kitchin additions
;; ** Colorizing comment links
(defvar org-comment-foreground "red"
  "Font color for comments.")

(defvar org-comment-background "yellow"
  "Background color for comments.")

(defvar org-comment-re
  "\\(\\[\\[\\)?comment:\\([^]]\\)+\\]?\\[?\\([^]]\\)*\\(\\]\\]\\)"
  "Regex for comment links. I am not sure how robust this is. It works so far.")

(defface org-comment-face
  `((t (:inherit org-link
		 :weight bold
		 :background ,org-comment-background
		 :foreground ,org-comment-foreground)))
  "Face for comment links in org-mode.")

(defun org-comment-colorize-links ()
  "Colorize org-ref links."
  (hi-lock-mode 1)
  (highlight-regexp org-comment-re 'org-comment-face))

;; * Org-mode menu
(defun org-comment-org-menu ()
  "Add org-comment menu to the Org menu."

  (easy-menu-change
   '("Org") "Comment"
   '( ["Insert comment" org-comment-add-comment]
      ["Delete comment" org-comment-delete-comment]
      ["List comments" org-comment-display-comments]
      "--"
      )
   "Show/Hide")

  (easy-menu-change '("Org") "--" nil "Show/Hide"))

(add-hook 'org-mode-hook 'org-comment-org-menu)

(provide 'org-comment)
;;; org-comment.el ends here
