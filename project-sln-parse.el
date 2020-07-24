;;; project-sln-parse.el --- Parse for each language  -*- lexical-binding: t; -*-

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

;;; Core

(defun project-sln-parse--inc/dec-nested-level (val)
  "Increment/Decrement the parse nested level by VAL."
  (setq project-sln-parse--nested-level (+ project-sln-parse--nested-level val)))

(defun project-sln-parse--same-nested-level-p (&optional record-val)
  "Check if RECORD-VAL the same as current nested level."
  (unless record-val (setq record-val project-sln-parse--recorded-nested-level))
  (= record-val project-sln-parse--nested-level))

(defun project-sln-parse--nested-level-changed-p ()
  "This should be call every time we trying to execute a node from AST.
Return non-nil, if nested level changed.
Return nil, if nested level has not changed."
  (if (project-sln-parse--same-nested-level-p)
      nil
    ;; NOTE: Update the nested level record; ready for next check.
    (setq project-sln-parse--recorded-nested-level project-sln-parse--nested-level)
    t))

(defun project-sln-parse--defined-info (type pos)
  "Form the definition info data structure.
TYPE: type of the data.
POS: defined position."
  (list type pos))

(defun project-sln--search-node-history (history-nodes offset &optional type)
  "Search the HISTORY-NODES base on OFFSET.
TYPE can be :value or :position, otherwise will return a list of information."
  (let ((node (nth offset history-nodes)))
    (cl-case type
      (:value (nth 0 node))
      (:position (nth 1 node))
      (t node))))

(defun project-sln-parse--resolve-keywords-csharp (ast)
  "Resolve keywords in csharp by AST."
  (setq project-sln-parse--nested-level 0)
  (setq project-sln-parse--recorded-nested-level 0)
  (let ((keys '())
        (level-changed-p nil)  ; Flag to check if nested level changed.
        (level-changed 0)  ; Nested level changed in number.
        (node-val "") (history-nodes '())
        (hist-1-v "") (hist-2-v "") (hist-3-v "") (hist-4-v "") (hist-5-v "")
        (hist-1-p "") (hist-2-p "") (hist-3-p "") (hist-4-p "") (hist-5-p "")
        (struct-defined-p nil)
        (stack-struct-names '("global"))  ; The stack of `struct`/`class` definition.
        (stack-def-names '())
        (struct-name "global")  ; Either be `struct` or `class, etc.
        (def-name "")  ; Name of the function, variable, class name, etc.
        (pos -1))
    (project-sln--walk-ast-tree
     ast
     (lambda (node next-node)
       (setq level-changed (- project-sln-parse--nested-level project-sln-parse--recorded-nested-level))
       (setq level-changed-p (project-sln-parse--nested-level-changed-p))
       (progn  ; Get node information.
         (setq node-val (cdr node))
         (setq pos (cdr next-node)))
       (push (list node-val pos) history-nodes)  ; Add to history
       (progn  ; Get recent history.
         ;; Value
         (setq hist-1-v (project-sln--search-node-history history-nodes 1 :value))
         (setq hist-2-v (project-sln--search-node-history history-nodes 2 :value))
         (setq hist-3-v (project-sln--search-node-history history-nodes 3 :value))
         (setq hist-4-v (project-sln--search-node-history history-nodes 4 :value))
         (setq hist-5-v (project-sln--search-node-history history-nodes 5 :value))
         ;; Position
         (setq hist-1-p (project-sln--search-node-history history-nodes 1 :position))
         (setq hist-2-p (project-sln--search-node-history history-nodes 2 :position))
         (setq hist-3-p (project-sln--search-node-history history-nodes 3 :position))
         (setq hist-4-p (project-sln--search-node-history history-nodes 4 :position))
         (setq hist-5-p (project-sln--search-node-history history-nodes 5 :position)))
       (progn  ; Ensure struct name.
         (when (= project-sln-parse--nested-level (length stack-struct-names))
           (pop stack-struct-names)
           (pop stack-def-names))
         (setq struct-name (nth 0 stack-struct-names))
         (setq def-name (nth 0 stack-def-names)))
       (cond
        ((and struct-defined-p
              (project-sln-util--is-contain-list-string '("{" ":") node-val))
         (setq def-name hist-1-v)
         (push def-name stack-def-names)
         (setq keys (project-sln-util--add-json-val keys
                                                    (list struct-name)
                                                    (list (cons def-name ()))))
         (setq struct-defined-p nil))
        ((project-sln-util--is-contain-list-string '("class" "struct") node-val)
         (setq struct-defined-p t)
         (push node-val stack-struct-names))
        ((and (string-match-p "[=,)]" node-val)
              (not (string-match-p "[(]" hist-1-v)))
         (setq keys
               (project-sln-util--add-json-val
                keys
                (list struct-name def-name project-sln-parse--key-var)
                (list (cons hist-1-v
                            (project-sln-parse--defined-info hist-2-v hist-1-p))))))
        ((string-match-p "[(]" node-val)
         (setq keys
               (project-sln-util--add-json-val
                keys
                (list struct-name def-name project-sln-parse--key-fnc)
                (list (cons hist-1-v
                            (project-sln-parse--defined-info hist-2-v hist-1-p)))))))))
    keys))

(provide 'project-sln-parse)
;;; project-sln-parse.el ends here
