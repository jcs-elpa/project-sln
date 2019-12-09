;;; project-sln-util.el --- Util class  -*- lexical-binding: t; -*-

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

(require 'json)


(defun project-sln-util--read-file (path)
  "Read a file from PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun project-sln-util--is-contain-list-string (in-list in-str)
  "Check if a string IN-STR contain in any string in the string list IN-LIST."
  (cl-some #'(lambda (lb-sub-str) (string-match-p (regexp-quote lb-sub-str) in-str)) in-list))

;;; JSON

(defun project-sln-util--get-json-val (json-root ids &optional lvl)
  "Get JSON value by list of IDS in JSON-ROOT by increment LVL."
  (unless lvl (setq lvl 0))
  (let ((index 0) (break nil) (len (length json-root)) (json nil) (val nil)
        (lvl-id (nth lvl ids))
        (last-lvl (= (1- (length ids)) lvl)))
    (while (and (not break) (< index len))
      (setq json (nth index json-root))
      (when (string= (car json) lvl-id)
        (setq break t)
        (setq lvl (1+ lvl))
        (setq val (if (and (not last-lvl) (listp (cdr json)))
                      (project-sln-util--get-json-val (cdr json) ids lvl)
                    (cdr json))))
      (setq index (1+ index)))
    val))

(defun project-sln-util--set-json-val (json-root ids new-val &optional lvl)
  "Set JSON value to NEW-VAL by list of IDS in JSON-ROOT by increment LVL."
  (unless lvl (setq lvl 0))
  (let ((index 0) (break nil) (len (length json-root)) (json nil)
        (lvl-id (nth lvl ids))
        (last-lvl (= (1- (length ids)) lvl)))
    (while (and (not break) (< index len))
      (setq json (nth index json-root))
      (when (string= (car json) lvl-id)
        (setq break t)
        (setq lvl (1+ lvl))
        (setf (cdr json) (if (and (not last-lvl) (listp (cdr json)))
                             (project-sln-util--set-json-val (cdr json) ids new-val lvl)
                           new-val)))
      (setq index (1+ index)))
    (unless break
      (if last-lvl
          (setq json-root (json-add-to-object json-root lvl-id new-val))
        (setq json-root (json-add-to-object json-root lvl-id ()))
        (setq json-root (project-sln-util--set-json-val json-root ids new-val lvl))))
    json-root))

(defun project-sln-util--add-json-val (json-root ids add-val)
  "Add ADD-VAL to JSON by list IDS starting from JSON-ROOT."
  (project-sln-util--set-json-val
   json-root
   ids
   (append
    (project-sln-util--get-json-val json-root ids)
    add-val)))


(provide 'project-sln-util)
;;; project-sln-util.el ends here
