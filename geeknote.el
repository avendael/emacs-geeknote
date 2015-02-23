;;; geeknote.el --- Use Evernote in Emacs through geeknote

;; Copyright (C) 2015 Evan Dale Aromin

;; Author: Evan Dale Aromin
;; Version: 0.2
;; Keywords: evernote, geeknote, note, emacs-evernote, evernote-mode
;; Package-Requires: ((emacs "24"))
;; URL: http://github.com/avendael/emacs-geeknote

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package wraps common geeknote commands into elisp.  With this, geeknote
;; can be interacted with within emacs instead of through a shell.
;;
;; The command `geeknote' is expected to be present on the user's `$PATH'.
;; Please follow the geeknote installation instructions to obtain this command.

;;; Code:
(defgroup geeknote nil
  "Interact with evernote through emacs."
  :group 'tools
  :group 'convenience)

(defcustom geeknote-command "geeknote"
  "The geeknote command.
It's either a path to the geeknote script as an argument to python, or simply
`geeknote` if the command is already on your PATH."
  :group 'geeknote
  :type 'string)

;;;###autoload
(defun geeknote-setup ()
  "Setup geeknote."
  (interactive)
  (eshell-command (concat
                   geeknote-command " settings --editor emacsclient")))

;;;###autoload
(defun geeknote-create (title)
  "Create a new note with the given title.

TITLE the title of the new note to be created."
  (interactive "sName: ")
  (message (format "Creating note: %s" title))
  (let ((note-title (geeknote-parse-title title))
	(note-tags (geeknote-parse-tags title))
	(note-notebook (geeknote-parse-notebook title)))
  (async-shell-command
   (format (concat geeknote-command " create --content WRITE --title %s --tags %s"
                   (when note-notebook " --notebook %s"))
           (shell-quote-argument note-title)
           (shell-quote-argument (or note-tags ""))
           (shell-quote-argument (or note-notebook ""))))))


;;;###autoload
(defun geeknote-show (title)
  "Open an existing note.

TITLE the title of the note to show."
  (interactive "sName: ")
  (message (format "Showing note: %s" title))
  (eshell-command
   (format (concat geeknote-command " show %s")
           (shell-quote-argument title))))

;;;###autoload
(defun geeknote-edit (title)
  "Open up an existing note for editing.

TITLE the title of the note to edit."
  (interactive "sName: ")
  (message (format "Editing note: %s" title))
  (async-shell-command
   (format (concat geeknote-command " edit --note %s")
           (shell-quote-argument title))))

;;;###autoload
(defun geeknote-remove (title)
  "Delete an existing note.

TITLE the title of the note to delete."
  (interactive "sName: ")
  (message (format "Deleting note: %s" title))
  (eshell-command
   (format (concat geeknote-command " remove --note %s --force")
           (shell-quote-argument title))))

;;;###autoload
(defun geeknote-find (keyword)
  "Search for a note with the given keyword.

KEYWORD the keyword to search the notes with."
  (interactive "skeyword: ")
  (eshell-command
   (format (concat geeknote-command " find --search %s --content-search")
           (shell-quote-argument keyword))))

;;;###autoload
(defun geeknote-move (note notebook)
  "Move a NOTE to a different NOTEBOOK.  If the provided NOTEBOOK is
non-existent, it will be created.

NOTE the title of the note to move.
NOTEBOOK the title of the notebook where NOTE should be moved."
  (interactive "sName: \nsMove note %s to notebook: ")
  (message (format "Moving note %s to notebook %s..." note notebook))
  (async-shell-command
   (format (concat geeknote-command " edit --note %s --notebook %s")
                   (shell-quote-argument note)
                   (shell-quote-argument notebook))))

(defun geeknote-parse-title (title)
  "Rerieve the title from the provided string. Filters out @notebooks and #tags.

TITLE is the input given when asked for a new note title."
  (let ((wordlist (split-string title)))
    (mapconcat (lambda (s) s)
	       (delq nil
		     (mapcar (lambda (str)
			       (cond
				((string-prefix-p "@" str) nil)
				((string-prefix-p "#" str) nil)
				(t str)))
			     wordlist))
	       " ")))

(defun geeknote-parse-notebook (title)
  "Rerieve the @notebook from the provided string. Returns nil if none.

TITLE is the input given when asked for a new note title."
  (let ((wordlist (split-string title)))
    (elt
     (delq nil
	   (mapcar (lambda (str)
		     (cond
		      ((string-prefix-p "@" str) (substring str 1))
		      (t nil)))
		   wordlist))
     0)))

(defun geeknote-parse-tags (title)
  "Rerieve the #tags from the provided string. Returns nil if none.

TITLE is the input given when asked for a new note title."
  (let ((wordlist (split-string title)))
    (mapconcat (lambda (s) s)
	       (delq nil
		     (mapcar (lambda (str)
			       (cond
				((string-prefix-p "#" str) (substring str 1))
				(t nil)))
			     wordlist))
	       ", ")))

(provide 'geeknote)
;;; geeknote.el ends here
