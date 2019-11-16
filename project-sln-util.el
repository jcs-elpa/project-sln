;;; project-sln-util.el --- Util class.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Shen, Jen-Chieh <jcs090218@gmail.com>

;; This file is NOT part of GNU Emacs.

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
;; Util class
;;

;;; Code:


(defun project-sln-util--id-exists-p (lst id)
  "Check if ID exists in the LST."
  (let ((found nil) (index 0) (break nil) (prop nil))
    (while (and (not break) (< index (length lst)))
      (setq prop (nth index lst))
      (when (string= (car prop) id)
        (setq found t))
      (setq index (1+ index)))
    found))

(defun project-sln-util--append-val (lst id val)
  "Append VAL to LST by ID."
  (dolist (prop lst)
    (when (string= (car prop) id)
      (message "val: %s" val)
      (setf (cdr prop) (append (cdr prop) val))))
  lst)


(provide 'project-sln-util)
;;; project-sln-util.el ends here
