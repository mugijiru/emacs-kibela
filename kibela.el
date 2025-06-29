;;; kibela.el --- Kibela client -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Mugijiru

;; Author: mugijiru <106833+mugijiru@users.noreply.github.com>
;; Maintainer: mugijiru <106833+mugijiru@users.noreply.github.com>
;; URL: https://github.com/mugijiru/emacs-kibela
;; Version: 2.0.0
;; Package-Requires: ((emacs "28.2") (graphql "0.1.1") (request "0.3.3") (markdown-mode "2.5") (edit-indirect "0.1.10"))
;; Keywords: kibela, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; See the README.org file for details.

;;; Code:

(require 'graphql)
(require 'request)
(require 'json)
(require 'tabulated-list)
(require 'kibela-auth)
(require 'kibela-request)
(require 'kibela-markdown-mode)

(defvar tabulated-list-format)
(defvar tabulated-list-sort-key)


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

(defconst kibela-graphql-query-group-notes-prev
  (graphql-query
   (:arguments
    (($id . ID!) ($perPage . Int!) ($cursor . String))
    (group
     :arguments ((id . ($ id)))
     (notes
      :arguments
      ((last . ($ perPage))
       (before . ($ cursor))
       (orderBy . ((field . CONTENT_UPDATED_AT) (direction . DESC))))
      (pageInfo hasNextPage hasPreviousPage)
      (edges
       cursor
       (node id title content contentUpdatedAt coediting canBeUpdated url))))))
  "Query to retrieve notes preceding the specified note within a group.
Used for pagination in the group's note list.")

(defconst kibela-graphql-query-group-notes-next
  (graphql-query
   (:arguments
    (($id . ID!) ($perPage . Int!) ($cursor . String))
    (group
     :arguments ((id . ($ id)))
     (notes
      :arguments
      ((first . ($ perPage))
       (after . ($ cursor))
       (orderBy . ((field . CONTENT_UPDATED_AT) (direction . DESC))))
      (pageInfo hasNextPage hasPreviousPage)
      (edges
       cursor
       (node id title content contentUpdatedAt coediting canBeUpdated url))))))
  "Query to retrieve notes within a group.")

(defconst kibela-graphql-query-recent-browsing-notes-prev
  (graphql-query
   (:arguments
    (($perPage . Int!) ($cursor . String))
    (noteBrowsingHistories
     :arguments
     ((last . ($ perPage)) (before . ($ cursor)))
     (pageInfo hasNextPage hasPreviousPage)
     (edges
      cursor
      (node
       (note id title content contentUpdatedAt coediting canBeUpdated url))))))
  "Query to retrieve recently viewed notes.
Used for pagination.")

(defconst kibela-graphql-query-recent-browsing-notes-next
  (graphql-query
   (:arguments
    (($perPage . Int!) ($cursor . String))
    (noteBrowsingHistories
     :arguments
     ((first . ($ perPage)) (after . ($ cursor)))
     (pageInfo hasNextPage hasPreviousPage)
     (edges
      cursor
      (node
       (note id title content contentUpdatedAt coediting canBeUpdated url))))))
  "Query to retrieve recently viewed notes.")

(defconst kibela-graphql-query-note
  (graphql-query
   (:arguments
    (($id . ID!))
    (note
     :arguments
     ((id . ($ id)))
     title
     content
     coediting
     canBeUpdated
     isLikedByCurrentUser
     url
     (groups id name)
     (folders
      :arguments ((first . 100)) (edges (node id fullName (group id name)))))))
  "Query to retrieve a Note.")

(defconst kibela-graphql-query-default-group
  (graphql-query ((defaultGroup id name)))
  "Query to retrieve the default posting group.")

(defconst kibela-graphql-query-note-templates
  (graphql-query
   ((noteTemplates
     :arguments ((first . 100))
     (edges
      (node
       id
       name
       title
       evaluatedTitle
       content
       (groups id name)
       (folders id fullName evaluatedFullName (group id name)))))))
  "Query to retrieve a list of note templates.")

(defconst kibela-graphql-mutation-create-note
  (graphql-mutation
   (:arguments
    (($input . CreateNoteInput!))
    (createNote :arguments ((input . ($ input))) (note title content))))
  "Query to create a note.")

(defconst kibela-graphql-mutation-update-note
  (graphql-mutation
   (:arguments
    (($input . UpdateNoteInput!))
    (updateNote :arguments ((input . ($ input))) (note title content))))
  "Query to update a note.")

(defconst kibela-graphql-mutation-like-note
  (graphql-mutation
   (:arguments
    (($input . LikeInput!))
    (like
     :arguments ((input . ($ input)))
     (likers :arguments ((first . 100)) (totalCount) (edges (node id))))))
  "Query to like a note.")

(defconst kibela-graphql-mutation-unlike-note
  (graphql-mutation
   (:arguments
    (($input . UnlikeInput!))
    (unlike
     :arguments ((input . ($ input)))
     (likers :arguments ((first . 100)) (totalCount) (edges (node id))))))
  "Query to unlike a note.")


(cl-defun
    kibela--store-default-group-success (&key data &allow-other-keys)
  "Data storage process after successful default group fetch request.

DATA is the JSON from the successful request."
  (let* ((response-data (assoc-default 'data data))
         (group (assoc-default 'defaultGroup response-data)))
    (setq kibela-default-group group)))

(defun kibela-store-default-group ()
  "Fetch the default posting group."
  (cond
   (kibela-default-group
    nil)
   (t
    (let* ((query kibela-graphql-query-default-group))
      (kibela-request query nil #'kibela--store-default-group-success)))))

(defun kibela-build-collection-from-note-templates (note-templates)
  "Function to generate a collection from note templates.

NOTE-TEMPLATES is an array of note templates registered in Kibela."
  (mapcar
   (lambda (note-template)
     (let* ((name (assoc-default 'name note-template))
            (title (assoc-default 'evaluatedTitle note-template))
            (content (assoc-default 'content note-template))
            (original-coediting (assoc-default 'coediting note-template))
            (coediting
             (if (or (null original-coediting)
                     (eq original-coediting json-false))
                 nil
               t))
            (groups (assoc-default 'groups note-template))
            (group-ids
             (mapcar (lambda (group) (assoc-default 'id group)) groups))
            (row-folders (assoc-default 'folders note-template))
            (folders
             (mapcar
              (lambda (folder)
                (let* ((folder-name (assoc-default 'evaluatedFullName folder))
                       (group (assoc-default 'group folder))
                       (group-id (assoc-default 'id group)))
                  `((groupId . ,group-id)
                    (group . ,group)
                    (folderName . ,folder-name))))
              row-folders))
            (template
             `(:title
               ,title
               :content ,content
               :coediting ,coediting
               :group-ids ,group-ids
               :groups ,groups
               :folders ,folders)))
       (propertize name 'template template)))
   note-templates))

(defun kibela-select-note-template-action (selected)
  "Handler for note template selection.

SELECTED is the selected note template."
  (let ((template (get-text-property 0 'template selected)))
    (if template
        (kibela--new-note-from-template template))))

;;;###autoload
(defun kibela-note-new-from-template ()
  "Command to display a new note buffer from a selected template."
  (interactive)
  (unless (and kibela-team kibela-access-token)
    (kibela-switch-team))
  (let ((query kibela-graphql-query-note-templates))
    (kibela-request
     query nil
     (cl-function
      (lambda (&key data &allow-other-keys)
        (let* ((response-data
                (assq 'data (graphql-simplify-response-edges data)))
               (note-templates (assoc-default 'noteTemplates response-data))
               (collection
                (kibela-build-collection-from-note-templates note-templates))
               (selected-key (completing-read "Note templates: " collection))
               (selected
                (seq-find
                 (lambda (elt) (string-equal elt selected-key)) collection)))
          (kibela-select-note-template-action selected)))))
    t))

(cl-defun
    kibela--group-notes-success (&key data &allow-other-keys)
  "Process to fetch group and its notes.

DATA is the JSON from a successful request."
  (let* ((row-data (assoc-default 'data data))
         (row-group (assoc-default 'group row-data))
         (row-notes (assoc-default 'notes row-group))

         (page-info (assoc-default 'pageInfo row-notes))
         (has-prev-page (assoc-default 'hasPreviousPage page-info))
         (has-next-page (assoc-default 'hasNextPage page-info))
         (edges (assoc-default 'edges row-notes))
         (first-note (elt edges 0))
         (first-cursor (assoc-default 'cursor first-note))
         (last-note (elt (reverse edges) 0))
         (last-cursor (assoc-default 'cursor last-note)))
    (setq tabulated-list-entries nil)
    (mapc
     (lambda (note)
       (let* ((node (assoc-default 'node note))
              (id (assoc-default 'id node))
              (title (assoc-default 'title node))
              (updated-at (assoc-default 'contentUpdatedAt node))
              (entry
               `(id
                 [(,title
                   .
                   (face default action kibela-note-show-from-list id ,id))
                  (,updated-at
                   .
                   (face default action kibela-note-show-from-list id ,id))])))
         (push entry tabulated-list-entries)))
     edges)
    (setq kibela-first-cursor first-cursor)
    (setq kibela-last-cursor last-cursor)
    (setq kibela-has-prev-page (equal has-prev-page t))
    (setq kibela-has-next-page (equal has-next-page t))
    (tabulated-list-init-header)
    (tabulated-list-print)))

(defun kibela-note-show-from-list (marker)
  "Action to open a note from the list.

MARKER contains the cursor position in the note list."
  (let* ((pos (marker-position marker))
         (id (get-text-property pos 'id)))
    (kibela-note-show id)))

(defun kibela-group-notes-refresh ()
  "Process to reload the note list."
  (message "Fetch default group notes...")
  (let* ((group-id (assoc-default 'id kibela-default-group))
         (query kibela-graphql-query-group-notes-next)
         (variables `((id . ,group-id) (perPage . ,kibela-per-page))))
    (kibela-request query variables #'kibela--group-notes-success)))

(defun kibela-group-notes-next-page ()
  "Process to fetch the next page of notes."
  (interactive)
  (cond
   (kibela-has-next-page
    (let* ((group-id (assoc-default 'id kibela-default-group))
           (query kibela-graphql-query-group-notes-next)
           (variables
            `((id . ,group-id)
              (perPage . ,kibela-per-page)
              (cursor . ,kibela-last-cursor))))
      (kibela-request query variables #'kibela--group-notes-success)))
   (t
    (message "Current page is last"))))

(defun kibela-group-notes-prev-page ()
  "Process to fetch the previous page of notes."
  (interactive)
  (cond
   (kibela-has-prev-page
    (let* ((group-id (assoc-default 'id kibela-default-group))
           (query kibela-graphql-query-group-notes-prev)
           (variables
            `((id . ,group-id)
              (perPage . ,kibela-per-page)
              (cursor . ,kibela-first-cursor))))
      (kibela-request query variables #'kibela--group-notes-success)))
   (t
    (message "Current page is first"))))

(defvar kibela-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd ">") 'kibela-group-notes-next-page)
    (define-key map (kbd "<") 'kibela-group-notes-prev-page)
    map)
  "Keymap for \\='kibela-list-mode\\='.")

(define-derived-mode
  kibela-list-mode
  tabulated-list-mode
  "Kibela list view."
  (setq tabulated-list-format [("Title" 40 t) ("UpdatedAt" 20 t)])
  (setq tabulated-list-sort-key '("UpdatedAt" . t))
  (add-hook 'tabulated-list-revert-hook 'kibela-group-notes-refresh nil t)
  (use-local-map kibela-list-mode-map))

;;;###autoload
(defun kibela-group-notes ()
  "Open a list of notes.
Currently only shows notes from the default group."
  (interactive)
  (unless (and kibela-team kibela-access-token)
    (kibela-switch-team))
  (kibela-store-default-group)
  (let* ((group-id (assoc-default 'id kibela-default-group))
         (kibela-group-name (assoc-default 'name kibela-default-group))
         (buffer-name (concat "*Kibela* notes in " kibela-group-name))
         (buffer (get-buffer-create buffer-name)))
    (cond
     ((null group-id)
      (message "デフォルトグループの取得ができていません"))
     (t
      (switch-to-buffer buffer)
      (kibela-list-mode)
      (kibela-group-notes-refresh)))))

(cl-defun
    kibela--recent-browsing-notes-success (&key data &allow-other-keys)
  "Process to fetch recently viewed notes.

DATA is the JSON from a successful request."
  (let* ((row-data (assoc-default 'data data))
         (row-note-browsing-histories
          (assoc-default 'noteBrowsingHistories row-data))

         (page-info (assoc-default 'pageInfo row-note-browsing-histories))
         (has-prev-page (assoc-default 'hasPreviousPage page-info))
         (has-next-page (assoc-default 'hasNextPage page-info))
         (edges (assoc-default 'edges row-note-browsing-histories))
         (first-note-browsing-history (elt edges 0))
         (first-cursor (assoc-default 'cursor first-note-browsing-history))
         (last-note-browsing-history (elt (reverse edges) 0))
         (last-cursor (assoc-default 'cursor last-note-browsing-history))
         (entries nil))
    (setq tabulated-list-entries nil)

    (mapc
     (lambda (note-browsing-history)
       (let* ((node (assoc-default 'node note-browsing-history))
              (note (assoc-default 'note node))
              (id (assoc-default 'id note))
              (title (assoc-default 'title note))
              (updated-at (assoc-default 'contentUpdatedAt note))
              (entry
               `(id
                 [(,title
                   .
                   (face default action kibela-note-show-from-list id ,id))
                  (,updated-at
                   .
                   (face default action kibela-note-show-from-list id ,id))])))
         (push entry entries)))
     edges)
    (setq tabulated-list-entries (nreverse entries))
    (setq kibela-first-cursor first-cursor)
    (setq kibela-last-cursor last-cursor)
    (setq kibela-has-prev-page (equal has-prev-page t))
    (setq kibela-has-next-page (equal has-next-page t))
    (tabulated-list-init-header)
    (tabulated-list-print)))

(defun kibela-recent-browsing-notes-refresh ()
  "Process to reload the list of recently viewed notes."
  (message "Fetch recent browsing notes...")
  (let* ((query kibela-graphql-query-recent-browsing-notes-next)
         (variables `((perPage . ,kibela-per-page))))
    (kibela-request query variables #'kibela--recent-browsing-notes-success)))

(defun kibela-recent-browsing-notes-next-page ()
  "Process to fetch the next page of recently viewed notes."
  (interactive)
  (cond
   (kibela-has-next-page
    (let* ((query kibela-graphql-query-recent-browsing-notes-next)
           (variables
            `((perPage . ,kibela-per-page) (cursor . ,kibela-last-cursor))))
      (kibela-request
       query variables #'kibela--recent-browsing-notes-success)))
   (t
    (message "Current page is last"))))

(defun kibela-recent-browsing-notes-prev-page ()
  "Process to fetch the previous page of recently viewed notes."
  (interactive)
  (cond
   (kibela-has-prev-page
    (let* ((query kibela-graphql-query-recent-browsing-notes-prev)
           (variables
            `((perPage . ,kibela-per-page) (cursor . ,kibela-first-cursor))))
      (kibela-request
       query variables #'kibela--recent-browsing-notes-success)))
   (t
    (message "Current page is first"))))

(defvar kibela-recent-browsing-notes-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd ">") 'kibela-recent-browsing-notes-next-page)
    (define-key map (kbd "<") 'kibela-recent-browsing-notes-prev-page)
    map)
  "Keymap for \\='kibela-recent-browsing-notes-mode\\='.")

(define-derived-mode
  kibela-recent-browsing-notes-mode
  tabulated-list-mode
  "Kibela recent browsing notes"
  "Kibela list view for recent browsing notes."
  (setq tabulated-list-format [("Title" 40 t) ("UpdatedAt" 20 t)])
  (setq tabulated-list-sort-key nil)
  (add-hook 'tabulated-list-revert-hook 'kibela-recent-browsing-notes-refresh
            nil t)
  (use-local-map kibela-recent-browsing-notes-mode-map))


;;;###autoload
(defun kibela-recent-browsing-notes ()
  "Command to open the list of recently viewed notes."
  (interactive)
  (unless (and kibela-team kibela-access-token)
    (kibela-switch-team))
  (let* ((buffer-name "*Kibela* recent browsing notes")
         (buffer (get-buffer-create buffer-name)))
    (switch-to-buffer buffer)
    (kibela-recent-browsing-notes-mode)
    (kibela-recent-browsing-notes-refresh)))

(cl-defun
    kibela--like-success (&key _data &allow-other-keys)
  "Handler for successful like request.
Updates display to show liked status without verifying response."
  (let ((groups kibela-note-groups)
        (folders kibela-note-folders))
    (setq header-line-format
          (kibela--build-header-line
           groups
           folders
           :liked-by-me-p t
           :exist-note-p t))))

(defun kibela-like ()
  "Add a like to the current note."
  (interactive "e")
  (let ((query kibela-graphql-mutation-like-note)
        (variables `((input . ((likableId . ,kibela-note-id))))))
    (kibela-request query variables #'kibela--like-success)))

(cl-defun
    kibela--unlike-success (&key _data &allow-other-keys)
  "Handler for successful unlike request.
Updates display to show unliked status without verifying response."
  (let ((groups kibela-note-groups)
        (folders kibela-note-folders))
    (setq header-line-format
          (kibela--build-header-line
           groups
           folders
           :liked-by-me-p nil
           :exist-note-p t))))

(defun kibela-unlike ()
  "Remove a like from the current note."
  (interactive "e")
  (let ((query kibela-graphql-mutation-unlike-note)
        (variables `((input . ((likableId . ,kibela-note-id))))))
    (kibela-request query variables #'kibela--unlike-success)))

(defvar kibela-like-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] 'kibela-like)
    map)
  "Keymap for \\='kibela-like-button\\='.")

(defvar kibela-unlike-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] 'kibela-unlike)
    map)
  "Keymap for \\='kibela-unlike-button\\='.")

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
        (propertize "♥" 'pointer 'hand 'keymap kibela-unlike-button-map))
       (unliked-button
        ;; Like していないのでクリック時は like する
        (propertize "♡" 'pointer 'hand 'keymap kibela-like-button-map))
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

;;;###autoload
(defun kibela-note-new (title)
  "Prepare a buffer for creating a new note.

TITLE is the title of the new note to create."
  (interactive "stitle: ")
  (unless (and kibela-team kibela-access-token)
    (kibela-switch-team))
  (let ((buffer (get-buffer-create "*Kibela* newnote")))
    (kibela-store-default-group)
    (switch-to-buffer buffer)
    (insert (concat "# " title "\n\n"))
    (kibela-markdown-mode)
    (setq header-line-format
          (kibela--build-header-line `(,kibela-default-group)))
    t))

(defun kibela--new-note-from-template (template)
  "Prepare a buffer for creating a note from a template.

TEMPLATE is the note template to use for creation."
  (let* ((title (plist-get template :title))
         (content (plist-get template :content))
         (groups (plist-get template :groups))
         (folders (plist-get template :folders))
         (buffer (get-buffer-create "*Kibela* newnote")))
    (switch-to-buffer buffer)
    (insert (concat "# " title "\n\n" content))
    (kibela-markdown-mode)
    (setq header-line-format (kibela--build-header-line groups folders))
    (setq kibela-note-template template)))

(defun kibela-note-create ()
  "Create a note."
  (interactive)
  (let* ((query kibela-graphql-mutation-create-note)
         (buffer-content (substring-no-properties (buffer-string)))
         (title
          (substring-no-properties (cl-first (split-string buffer-content "\n"))
                                   2))
         (content (string-join (cddr (split-string buffer-content "\n")) "\n"))
         (coediting
          (if kibela-note-template
              (if (plist-get kibela-note-template :coediting)
                  t
                json-false)
            t))
         (draft
          (if kibela-note-template
              (plist-get kibela-note-template :draft)
            json-false))
         (group-ids
          (if kibela-note-template
              (plist-get kibela-note-template :group-ids)
            `(,(assoc-default 'id kibela-default-group))))
         (folders
          (if kibela-note-template
              (plist-get kibela-note-template :folders)))
         (variables
          `((input
             .
             ((title . ,title)
              (content . ,content) (groupIds . ,group-ids)
              (folders
               .
               ,(mapcar
                 (lambda (folder) (assq-delete-all 'group folder)) folders))
              (coediting . ,coediting) (draft . ,draft))))))
    (kibela-request
     query variables
     (cl-function
      (lambda (&key data &allow-other-keys)
        (let* ((errors (assoc-default 'errors data)))
          (cond
           (errors
            (let* ((message
                    (mapconcat (lambda (error)
                                 (assoc-default 'message error))
                               errors
                               "\n")))
              (message (concat "Error: " message))))
           (t
            (let* ((json-data (assoc-default 'data data))
                   (create-note (assoc-default 'createNote json-data))
                   (note (assoc-default 'note create-note))
                   (title (assoc-default 'title note))
                   (buffer (get-buffer-create "*Kibela* newnote")))
              (kill-buffer buffer)
              (setq kibela-note-template nil)
              (message (concat "create note '" title "' has succeed.")))))))))))

;;;###autoload
(defun kibela-note-show (id)
  "Display a note.

ID is the note's identifier.
Note that GraphQL uses encoded string IDs rather than numeric ones,
so IDs should be obtained through GraphQL queries rather than from URLs."
  (unless (and kibela-team kibela-access-token)
    (kibela-switch-team))
  (let ((query kibela-graphql-query-note)
        (variables `((id . ,id))))
    (kibela-request
     query variables
     (cl-function
      (lambda (&key data &allow-other-keys)
        (let* ((json-data
                (assoc-default 'data (graphql-simplify-response-edges data)))
               (note (assoc-default 'note json-data))
               (title (assoc-default 'title note))
               (content (assoc-default 'content note))
               (coediting (assoc-default 'coediting note))
               (can-be-updated (eq t (assoc-default 'canBeUpdated note)))
               (liked-by-me-p (eq t (assoc-default 'isLikedByCurrentUser note)))
               (url (assoc-default 'url note))
               (groups (assoc-default 'groups note))
               (group-ids
                (mapcar (lambda (group) (assoc-default 'id group)) groups))
               (row-folders (assoc-default 'folders note))
               (folders
                (mapcar
                 (lambda (folder)
                   (let* ((folder-name (assoc-default 'fullName folder))
                          (group (assoc-default 'group folder))
                          (group-id (assoc-default 'id group)))
                     `((groupId . ,group-id)
                       (group . ,group)
                       (folderName . ,folder-name))))
                 row-folders))
               (folders-for-base
                (mapcar
                 (lambda (folder)
                   (assq-delete-all 'group folder))
                 (copy-alist folders)))
               (buffer (get-buffer-create (concat "*Kibela* " id))))
          (switch-to-buffer buffer)
          (insert (concat "# " title "\n\n" content))
          (kibela-markdown-view-mode)
          (setq header-line-format
                (kibela--build-header-line
                 groups
                 folders
                 :liked-by-me-p liked-by-me-p
                 :exist-note-p t))
          (setq kibela-note-can-be-updated can-be-updated)
          (setq kibela-note-id id)
          (setq kibela-note-url url)
          (setq kibela-note-groups groups)
          (setq kibela-note-folders folders)
          (setq kibela-note-liked-by-me liked-by-me-p)
          (setq kibela-note-base
                `(("title" . ,title)
                  ("content" . ,content)
                  ("coediting" . ,coediting)
                  ("coediting" . ,coediting)
                  ("groupIds" . ,group-ids)
                  ("folders" . ,folders-for-base)))))))
    t))

(defun kibela-note-update ()
  "Update the current note."
  (interactive)
  (let* ((query kibela-graphql-mutation-update-note)
         (id (cl-second (split-string (buffer-name))))
         (buffer-content (substring-no-properties (buffer-string)))
         (title
          (substring-no-properties (cl-first (split-string buffer-content "\n"))
                                   2))
         (content (string-join (cddr (split-string buffer-content "\n")) "\n"))
         (coediting (assoc-default "coediting" kibela-note-base))
         (group-ids (assoc-default "groupIds" kibela-note-base))
         (folders (assoc-default "folders" kibela-note-base))
         (variables
          `((input
             .
             (("id" . ,id)
              ("newNote" .
               (("title" . ,title)
                ("content" . ,content)
                ("groupIds" . ,group-ids)
                ("folders" . ,folders)
                ("coediting" . ,coediting)))
              ("baseNote" . ,kibela-note-base) ("draft" . ,json-false))))))
    (kibela-request
     query variables
     (cl-function
      (lambda (&key data &allow-other-keys)
        (let* ((errors (assoc-default 'errors data)))
          (cond
           (errors
            (let* ((message
                    (mapconcat (lambda (error)
                                 (assoc-default 'message error))
                               errors
                               "\n")))
              (message (concat "Error: " message))))
           (t
            (let* ((json-data (assoc-default 'data data))
                   (update-note (assoc-default 'updateNote json-data))
                   (note (assoc-default 'note update-note))
                   (title (assoc-default 'title note))
                   (buffer (get-buffer-create (concat "*Kibela* " id))))
              (setq kibela-note-base nil)
              (kill-buffer buffer)
              (message (concat "update note '" title "' has succeed.")))))))))
    t))

(provide 'kibela)
;;; kibela.el ends here
