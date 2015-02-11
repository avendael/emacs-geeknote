;;; geeknote.el --- Use Evernote in Emacs through geeknote

;; Copyright (C) 2015 Evan Dale Aromin

;; Author: Evan Dale Aromin
;; Version: 0.1.3
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
  (async-shell-command
   (format (concat geeknote-command " create --content WRITE --title %s")
           (shell-quote-argument title))))

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
  ;; (eshell-command
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
   (format "geeknote find --search %s --content-search"
           (shell-quote-argument keyword))))

(provide 'geeknote)
;;; geeknote.el ends here
