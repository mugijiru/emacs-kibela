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

(defvar kibela-markdown-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map markdown-mode-map)
    (define-key map (kbd "C-c p") 'kibela-markdown-post)
    map)
  "Keymap for `kibela-markdown-mode'.
See also `markdown-mode-map'.")

(define-derived-mode kibela-markdown-mode gfm-mode "Kibela Markdown"
  "Major mode for editing Kibela Markdown files."
  (use-local-map kibela-markdown-mode-map))

(provide 'kibela-markdown-mode)
;;; kibela-markdown-mode.el ends here
