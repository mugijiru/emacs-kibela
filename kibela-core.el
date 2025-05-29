;;; kibela-core.el --- Kibela core functionality -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Mugijiru

;; Author: mugijiru <106833+mugijiru@users.noreply.github.com>
;; Maintainer: mugijiru <106833+mugijiru@users.noreply.github.com>
;; URL: https://github.com/mugijiru/emacs-kibela
;; Version: 2.0.0
;; Package-Requires: ((emacs "28.2"))
;; Keywords: kibela, tools

;; This file is part of Kibela client for Emacs.

;;; Commentary:

;; Core functionality and variables for Kibela client.

;;; Code:

(defvar-local kibela-note-base nil
  "Holds the state of the note when it was retrieved.
Used when updating the note.")

(defvar-local kibela-note-template nil
  "Holds the note template to be used.
Used when creating a note from a template.")

(defvar kibela-default-group nil
  "Variable to store the default posting group.")

(defvar-local kibela-note-can-be-updated nil
  "Variable to store whether the note can be updated.")

(defvar-local kibela-note-id nil
  "Variable to store the note ID.")

(defvar-local kibela-note-url nil
  "Variable to store the note URL.")

(defvar-local kibela-note-groups nil)
(defvar-local kibela-note-folders nil)
(defvar-local kibela-note-liked-by-me nil)

(defcustom kibela-per-page 40
  "Maximum number of items to retrieve at once, such as in a list of notes."
  :group 'kibela
  :type 'integer)

(defvar-local kibela-first-cursor nil
  "Stores the cursor of the first note displayed in the note list.
Used to return to the previous page.")

(defvar-local kibela-last-cursor nil
  "Stores the cursor of the last note displayed in the note list.
Used to advance to the next page.")

(defvar-local kibela-has-next-page nil
  "Whether the next page exists in the note list.")

(defvar-local kibela-has-prev-page nil
  "Whether the previous page exists in the note list.")

(cl-defun
    kibela--build-header-line
    (groups &optional (folders '()) &key (liked-by-me-p nil) (exist-note-p nil))
  "Build header-line string from group/folder information.
Used in edit and new-from-template modes.

GROUPS is a list of groups the note belongs to.
FOLDERS is a list of folders containing the note.
These values are used to construct the header line.

LIKED-BY-ME-P indicates if the current user has liked the note.
EXIST-NOTE-P indicates if the note actually exists."
  (let*
      ((groups-without-folder
        (seq-remove
         (lambda (group)
           (seq-find
            (lambda (folder)
              (let* ((folder-group (assoc-default 'group folder))
                     (folder-group-id (assoc-default 'id folder-group))
                     (group-id (assoc-default 'id group)))
                (string-equal folder-group-id group-id)))
            folders))
         groups))
       (folder-names
        (mapcar
         (lambda (folder)
           (let* ((group (assoc-default 'group folder))
                  (kibela-group-name (assoc-default 'name group))
                  (full-name (assoc-default 'folderName folder))
                  (folder-paths (split-string full-name "/"))
                  (full-paths (append `(,kibela-group-name) folder-paths)))
             (string-join full-paths " > ")))
         folders))
       (group-names
        (mapcar
         (lambda (group) (assoc-default 'name group)) groups-without-folder))
       (liked-button
        ;; Like 済なのでクリック時は unlike する
        (propertize "♥" 'pointer 'hand))
       (unliked-button
        ;; Like していないのでクリック時は like する
        (propertize "♡" 'pointer 'hand))
       (liked-button-or-nil
        (if exist-note-p
            (if liked-by-me-p
                liked-button
              unliked-button)
          nil))
       (names
        (cl-remove-if
         #'null (append `(,liked-button-or-nil) group-names folder-names))))
    (string-join names " | ")))

(provide 'kibela-core)
;;; kibela-core.el ends here
