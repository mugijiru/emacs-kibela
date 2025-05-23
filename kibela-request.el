;;; kibela-request.el --- request code for kibela   -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Mugijiru

;; Author: Mugijiru <mugijiru@manjaro>
;; Maintainer: mugijiru <106833+mugijiru@users.noreply.github.com>
;; URL: https://github.com/mugijiru/emacs-kibela
;; Version: 0.2.0
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

(defvar kibela-team)
(defvar kibela-access-token)

(defun kibela--endpoint ()
  "API endpoint."
  (concat "https://" kibela-team ".kibe.la/api/v1"))

(defun kibela--headers ()
  "HTTP request headers."
  `(("Content-Type" . "application/json")
    ("Accept" . "application/json")
    ("Authorization" . ,(concat "Bearer " kibela-access-token))))

(defun kibela-request (query variables success)
  "Function to send requests to Kibela.

QUERY is the GraphQL query.
VARIABLES are the GraphQL variables.
SUCCESS is the handler for when the request succeeds."
  (let ((data (json-encode `((query . ,query) (variables . ,variables)))))
    (request
      (kibela--endpoint)
      :type "POST"
      :data data
      :parser 'json-read
      :encoding 'utf-8
      :headers (kibela--headers)
      :success success
      :error
      (cl-function
       (lambda (&rest args &key error-thrown &allow-other-keys)
         (message "Got error: %S" error-thrown))))))

(provide 'kibela-request)
;;; kibela-request.el ends here
