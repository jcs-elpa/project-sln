;;; project-sln-parse.el --- Parse for each language.  -*- lexical-binding: t; -*-

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
;; Parse for each language
;;

;;; Code:

(require 'project-sln-util)


(defconst project-sln-parse--key-var "var"
  "Id key to represent variable.")

(defconst project-sln-parse--key-fnc "fnc"
  "Id key to represent function.")

(defvar project-sln-parse--nested-level 0
  "Count the nested level.
This is use to identify the nested level to compare with other near nodes around.")

(defvar project-sln-parse--recorded-nested-level 0
  "Record down current nested level.
The current nested level is store inside `project-sln-parse--nested-level' variable.")


(defun project-sln-parse--inc/dec-nested-level (val)
  "Increment/Decrement the parse nested level by VAL."
  (setq project-sln-parse--nested-level (+ project-sln-parse--nested-level val)))

(defun project-sln-parse--same-nested-level-p (&optional record-val)
  "Check if RECORD-VAL the same as current nested level."
  (unless record-val (setq record-val project-sln-parse--recorded-nested-level))
  (= record-val project-sln-parse--nested-level))

(defun project-sln-parse--nested-level-changed ()
  "This should be call every time we trying to execute a node from AST.
Return non-nil, if nested level changed.
Return nil, if nested level has not changed."
  (if (project-sln-parse--same-nested-level-p)
      nil
    ;; NOTE: Update the nested level record; ready for next check.
    (setq project-sln-parse--recorded-nested-level project-sln-parse--nested-level)
    t))


(defun project-sln--resolve-keywords-csharp (ast)
  "Resolve keywords in csharp by AST."
  (let ((keys '()) (flag-next-node nil)
        (level-changed nil)  ; Flag to check if nested level changed.
        (node-type "") (node-val "") (last-node-val "")
        (struct-name "")
        (type-name ""))
    (project-sln--walk-ast-tree
     ast
     (lambda (node)
       (setq level-changed (project-sln-parse--nested-level-changed))
       (setq node-type (car node))
       (setq node-val (cdr node))
       (if flag-next-node
           (progn
             (setq struct-name last-node-val)
             (setq keys (project-sln-util--add-json-val keys
                                                        (list struct-name)
                                                        (list (cons node-val ()))))
             (setq type-name node-val)
             (setq flag-next-node nil))
         (cond
          ((project-sln-util--is-contain-list-string
            '("class" "struct")
            node-val)
           (setq flag-next-node t))
          ((string= "=" node-val)
           (setq keys
                 (project-sln-util--add-json-val
                  keys
                  (list struct-name type-name project-sln-parse--key-var)
                  (list last-node-val)))
           )
          ((string= "(" node-val)
           (setq keys
                 (project-sln-util--add-json-val
                  keys
                  (list struct-name type-name project-sln-parse--key-fnc)
                  (list last-node-val)))
           )
          )
         (setq last-node-val node-val)
         )))
    keys))


(provide 'project-sln-parse)
;;; project-sln-parse.el ends here
