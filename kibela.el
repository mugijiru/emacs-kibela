;;; kibela.el --- kibela for emacs                   -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Mugijiru

;; Author: mugijiru <106833+mugijiru@users.noreply.github.com>
;; Maintainer: mugijiru <106833+mugijiru@users.noreply.github.com>
;; URL: https://github.com/mugijiru/emacs-kibela
;; Version: 0.1.0
;; Package-Requires: ((graphql "0.1.1") (request "0.3.3"))
;; Keywords: tools

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

;;; Code:

(require 'graphql)
(require 'request)
(require 'json)
(require 'kibela-markdown-mode)

(defcustom kibela-team nil
  "Kibela team name for login."
  :group 'kibela
  :type 'string)

(defcustom kibela-access-token nil
  "Kibela access token for login."
  :group 'kibela
  :type 'string)

(defvar-local kibela-note-base nil
  "記事取得時の状態を保持する。記事更新時に利用する")

(defvar-local kibela-note-template nil
  "使用する記事テンプレートを保持する。テンプレートから記事を作成する時に利用する")

(defvar kibela-default-group nil
  "デフォルトの投稿先グループを保存する変数")

(defconst kibela-graphql-query-note
  (graphql-query
   (:arguments (($id . ID!))
               (note
                :arguments ((id . ($ id)))
                title
                content
                coediting
                (groups id name)
                (folders
                 :arguments((first . 100))
                 (edges
                  (node
                   id
                   fullName
                   (group
                    id
                    name))))
                canBeUpdated)))
  "Note を取得するためのクエリ")

(defconst kibela-graphql-query-default-group
  (graphql-query
   ((defaultGroup
     id
     name)))
  "デフォルトの投稿先グループを取得するためのクエリ")

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
       (groups
        id
        name)
       (folders
        id
        fullName
        evaluatedFullName
        (group
         id
         name)))))))
  "記事テンプレート一覧を取得するクエリ")

(defconst kibela-graphql-mutation-create-note
  (graphql-mutation
   (:arguments (($input . CreateNoteInput!))
    (createNote
     :arguments ((input . ($ input)))
     (note
      title
      content))))
  "Note を作成するためのクエリ")

(defconst kibela-graphql-mutation-update-note
  (graphql-mutation
   (:arguments (($input . UpdateNoteInput!))
    (updateNote
     :arguments ((input . ($ input)))
     (note
      title
      content))))
  "Note を更新するためのクエリ")

(defun kibela-endpoint ()
  "API endpoint"
  (concat "https://" kibela-team ".kibe.la/api/v1"))

(defun kibela-headers ()
  "HTTP request headers."
  `(("Content-Type" . "application/json")
    ("Accept" . "application/json")
    ("Authorization" . ,(concat "Bearer " kibela-access-token))))

(defun kibela-store-default-group ()
  "デフォルトの投稿先グループを取得する"
  (cond (kibela-default-group
         nil)
        (t
         (let* ((query kibela-graphql-query-default-group)
                (data (json-encode `((query . ,query)))))
           (request
             (kibela-endpoint)
             :type "POST"
             :data data
             :parser 'json-read
             :encoding 'utf-8
             :headers (kibela-headers)
             :success (cl-function
                       (lambda (&key data &allow-other-keys)
                         (let* ((response-data (assoc-default 'data data))
                                (group (assoc-default 'defaultGroup response-data)))
                           (setq kibela-default-group group))))
             :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                                   (pp args)
                                   (message "Got error: %S" error-thrown))))))))

(defun kibela-build-collection-from-note-templates (note-templates)
  (mapcar (lambda (note-template)
            (let* ((name (assoc-default 'name note-template))
                   (title (assoc-default 'evaluatedTitle note-template))
                   (content (assoc-default 'content note-template))
                   (groups (assoc-default 'groups note-template))
                   (group-ids (mapcar (lambda (group) (assoc-default 'id group)) groups))
                   (row-folders (assoc-default 'folders note-template))
                   (folders (mapcar (lambda (folder)
                                      (let* ((folder-name (assoc-default 'evaluatedFullName folder))
                                             (group (assoc-default 'group folder))
                                             (group-id (assoc-default 'id group)))
                                        `((groupId . ,group-id) (folderName . ,folder-name))))
                                    row-folders))
                   (template `(:title ,title :content ,content :group-ids ,group-ids :folders ,folders)))
              (propertize name 'template template)))
          note-templates))

(defun kibela-select-note-template-action (selected)
  (let ((template (get-text-property 0 'template selected)))
    (if template
        (kibela--new-note-from-template template))))

;;;###autoload
(defun kibela-note-new-from-template ()
  "記事テンプレートから選択したら新規作成用のバッファを表示するコマンド"
  (interactive)
  (let* ((query kibela-graphql-query-note-templates)
         (request-data (json-encode `((query . ,query)))))
    (request
      (kibela-endpoint)
      :type "POST"
      :data request-data
      :parser 'json-read
      :encoding 'utf-8
      :headers (kibela-headers)
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (let* ((response-data (assq 'data (graphql-simplify-response-edges data)))
                         (note-templates (assoc-default 'noteTemplates response-data))
                         (collection (kibela-build-collection-from-note-templates note-templates))
                         (selected (completing-read "Note templates: " collection)))
                    (kibela-select-note-template-action selected))))
      :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                            (pp args)
                            (message "Got error: %S" error-thrown))))))

;;;###autoload
(defun kibela-note-new (title)
  "記事を作成するバッファを用意する"
  (interactive "stitle: ")
  (let ((buffer (get-buffer-create "*Kibela* newnote")))
    (kibela-store-default-group)
    (switch-to-buffer buffer)
    (insert (concat "# " title "\n\n"))
    (kibela-markdown-mode)))

(defun kibela--new-note-from-template (template)
  "記事を作成するバッファを用意する"
  (let* ((title (plist-get template :title))
         (content (plist-get template :content))
         (group-ids (plist-get template :group-ids))
         (folders (plist-get template :folders))
         (buffer (get-buffer-create "*Kibela* newnote")))
    (switch-to-buffer buffer)
    (insert (concat "# " title "\n\n" content))
    (kibela-markdown-mode)
    (setq kibela-note-template template)))

(defun kibela-note-create ()
  "記事作成"
  (interactive)
  (let* ((query kibela-graphql-mutation-create-note)
         (buffer-content (substring-no-properties (buffer-string)))
         (title (substring-no-properties (first (split-string buffer-content "\n")) 2))
         (content (string-join (cddr (split-string buffer-content "\n")) "\n"))
         (coediting t) ;; TODO handle coediting
         (draft json-false) ;; TODO handle draft
         (group-ids (if kibela-note-template
                        (plist-get kibela-note-template :group-ids)
                      `(,(assoc-default 'id kibela-default-group))))
         (folders (if kibela-note-template
                      (plist-get kibela-note-template :folders)))
         (data `(("query" . ,query)
                 ("variables" . ((input . (("title" . ,title)
                                           ("content" . ,content)
                                           ("groupIds" . ,group-ids)
                                           ("folders" . ,folders)
                                           ("coediting" . ,coediting)
                                           ("draft" . ,draft)))))))
         (encoded-data (json-encode data)))
    (request
      (kibela-endpoint)
      :type "POST"
      :data encoded-data
      :parser 'json-read
      :encoding 'utf-8
      :headers (kibela-headers)
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (let* ((errors (assoc-default 'errors data)))
                    (cond (errors
                           (let* ((message (mapconcat (lambda (error)
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
                             (message (concat "create note '" title "' has succeed."))))))))
      :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                            (pp args)
                            (message "Got error: %S" error-thrown))))))

;;;###autoload
(defun kibela-note-show (id)
  "記事表示"
  (let ((query kibela-graphql-query-note))
    (request
      (kibela-endpoint)
      :type "POST"
      :data (json-encode `(("query" . ,query) ("variables" . ,(list (cons "id" id)))))
      :parser 'json-read
      :encoding 'utf-8
      :headers (kibela-headers)
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (let* ((json-data (assoc-default 'data (graphql-simplify-response-edges data)))
                         (note (assoc-default 'note json-data))
                         (title (assoc-default 'title note))
                         (content (assoc-default 'content note))
                         (coediting (assoc-default 'coediting note))
                         (groups (assoc-default 'groups note))
                         (group-ids (mapcar (lambda (group) (assoc-default 'id group)) groups))
                         (row-folders (assoc-default 'folders note))
                         (folders (mapcar (lambda (folder)
                                            (let* ((folder-name (assoc-default 'fullName folder))
                                                   (group (assoc-default 'group folder))
                                                   (group-id (assoc-default 'id group)))
                                              `((groupId . ,group-id) (folderName . ,folder-name))))
                                          row-folders))
                         (buffer (get-buffer-create (concat "*Kibela* " id))))
                    (switch-to-buffer buffer)
                    (insert (concat "# " title "\n\n" content))
                    (kibela-markdown-mode)
                    (setq kibela-note-base
                          `(("title" . ,title)
                            ("content" . ,content)
                            ("coediting" . ,coediting)
                            ("groupIds" . ,group-ids)
                            ("folders" . ,folders))))))
      :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                            (pp args)
                            (message "Got error: %S" error-thrown))))))

;;;###autoload
(defun kibela-note-update ()
  "記事更新"
  (interactive)
  (let* ((query kibela-graphql-mutation-update-note)
         (id (second (split-string (buffer-name))))
         (buffer-content (substring-no-properties (buffer-string)))
         (title (substring-no-properties (first (split-string buffer-content "\n")) 2))
         (content (string-join (cddr (split-string buffer-content "\n")) "\n"))
         (coediting (assoc-default "coediting" kibela-note-base))
         (group-ids (assoc-default "groupIds" kibela-note-base))
         (folders (assoc-default "folders" kibela-note-base))

         (data `(("query" . ,query)
                 ("variables" . ((input . (("id" . ,id)
                                           ("newNote" . (("title" . ,title)
                                                         ("content" . ,content)
                                                         ("groupIds" . ,group-ids)
                                                         ("folders" . ,folders)
                                                         ("coediting" . ,coediting)))
                                           ("baseNote" . ,kibela-note-base)
                                           ("draft" . ,json-false)))))))
         (encoded-data (json-encode data)))
    (request
      (kibela-endpoint)
      :type "POST"
      :data encoded-data
      :parser 'json-read
      :encoding 'utf-8
      :headers (kibela-headers)
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (let* ((errors (assoc-default 'errors data)))
                    (cond (errors
                           (let* ((message (mapconcat (lambda (error)
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
                             (message (concat "update note '" title "' has succeed."))))))))
      :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                            (pp args)
                            (message "Got error: %S" error-thrown))))))

(provide 'kibela)
;;; kibela.el ends here
