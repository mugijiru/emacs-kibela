;;; kibela-auth.el --- Kibela authentication -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Mugijiru

;; Author: mugijiru <106833+mugijiru@users.noreply.github.com>
;; Maintainer: mugijiru <106833+mugijiru@users.noreply.github.com>
;; URL: https://github.com/mugijiru/emacs-kibela
;; Version: 2.0.0
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

;; Kibela authentication module

;;; Code:

(defvar kibela-default-group)

(defcustom kibela-auth-list nil
  "Authentication information for Kibela.
Each element has the form (NAME TEAM ACCESS-TOKEN)."
  :group 'kibela
  :type '(alist :value-type (string string string)))

(defvar kibela-team nil
  "Kibela team name for login.")

(defvar kibela-access-token nil
  "Kibela access token for login.")

;;;###autoload
(defun kibela-switch-team ()
  "Switch between teams to operate."
  (interactive)
  (let* ((selected (completing-read "Select team: " kibela-auth-list))
         (auth (assoc-default selected kibela-auth-list))
         (team (cl-first auth))
         (access-token (cl-second auth)))
    (cond
     (auth
      (kill-matching-buffers "\\*Kibela\\*" nil t)
      (setq kibela-team team)
      (setq kibela-access-token access-token)
      (setq kibela-default-group nil))
     (t
      (message "No match team.")))))

(provide 'kibela-auth)
;;; kibela-auth.el ends here
