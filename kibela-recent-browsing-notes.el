;;; kibela-recent-browsing-notes.el --- Kibela recent browsing notes -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Mugijiru

;; Author: mugijiru <106833+mugijiru@users.noreply.github.com>
;; Maintainer: mugijiru <106833+mugijiru@users.noreply.github.com>
;; URL: https://github.com/mugijiru/emacs-kibela
;; Version: 2.0.0
;; Package-Requires: ((emacs "28.2") (kibela "2.0.0") (graphql "0.1.1"))
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

;; This file implements recent browsing notes features for Kibela client.

;;; Code:

(require 'tabulated-list)
(require 'graphql)
(declare-function kibela-note-show-from-list "kibela")
(declare-function kibela-request "kibela-request")
(defvar kibela-per-page)
(defvar kibela-first-cursor)
(defvar kibela-last-cursor)
(defvar kibela-has-prev-page)
(defvar kibela-has-next-page)
(defvar kibela-team)
(defvar kibela-access-token)

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

(provide 'kibela-recent-browsing-notes)
;;; kibela-recent-browsing-notes.el ends here
