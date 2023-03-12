;;; kibela-markdown.el --- major-mode for kibela markdown  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Mugijiru

;; Author: mugijiru <106833+mugijiru@users.noreply.github.com>
;; Maintainer: mugijiru <106833+mugijiru@users.noreply.github.com>
;; URL: https://github.com/mugijiru/emacs-kibela
;; Version: 0.1.0
;; Keywords: kibela, markdown, tools

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

;;

;;; Code:

(require 'markdown-mode)

(defun kibela-markdown-post ()
  "Kibela に投稿する処理."
  (interactive)
  (if kibela-note-base
      (kibela-note-update)
    (kibela-note-create)))

(defun kibela-markdown--show-to-edit ()
  (interactive)
  (let* ((base kibela-note-base)
         (url kibela-note-url))
    (cond
     (kibela-note-can-be-updated
      (kibela-markdown-mode)
      (setq kibela-note-base base)
      (setq kibela-note-url url))
     (t
      (message "cannot edit this note.")))))

(defun kibela-markdown-open-in-browser ()
  (interactive)
  (if kibela-note-url
      (browse-url kibela-note-url)
    (message "URL is not exists")))

(defun kibela-markdown--kill-edit-buffer ()
  (interactive)
  (if kibela-note-base
      (let ((base kibela-note-base)
            (url kibela-note-url))
        (erase-buffer)
        (insert (concat "# " (assoc-default "title" base) "\n\n" (assoc-default "content" base)))
        (kibela-markdown-view-mode)
        (setq kibela-note-base base)
        (setq kibela-note-can-be-updated t))
    (kill-current-buffer)))

(defvar kibela-markdown-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map gfm-mode-map)
    (define-key map (kbd "C-c C-c C-c") 'kibela-markdown-post)
    (define-key map (kbd "C-c C-c C-o") 'kibela-markdown-open-in-browser)
    (define-key map (kbd "C-c C-z") 'kibela-markdown--kill-edit-buffer)
    map)
  "Keymap for `kibela-markdown-mode'.
See also `gfm-mode-map'.")

(define-derived-mode kibela-markdown-mode gfm-mode "Kibela Markdown"
  "Major mode for editing Kibela Markdown files."
  (use-local-map kibela-markdown-mode-map))

(defvar kibela-markdown-view-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map gfm-view-mode-map)
    (define-key map (kbd "C-c C-c C-o") 'kibela-markdown-open-in-browser)
    (define-key map (kbd "C-c C-e") 'kibela-markdown--show-to-edit)
    (define-key map (kbd "C-c C-z") 'kill-current-buffer)
    map)
  "Keymap for `kibela-markdown-view-mode'.
See also `gfm-view-mode-map'.")

(define-derived-mode kibela-markdown-view-mode gfm-view-mode "Kibela Markdown View"
  "Major mode for viewing Kibela Markdown files."
  (use-local-map kibela-markdown-view-mode-map))

(provide 'kibela-markdown-mode)
;;; kibela-markdown-mode.el ends here
