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

(require 'project-sln)
(require 'project-sln-util)


(defun project-sln--resolve-keywords-csharp (ast)
  "Resolve keywords in csharp by AST."
  (let ((keys '()) (flag nil) (node-type "") (node-val "") (last-node-val ""))
    (project-sln--walk-ast-tree
     ast
     (lambda (node)
       (setq node-type (car node))
       (setq node-val (cdr node))
       (if flag
           (progn
             (if (project-sln-util--id-exists-p keys last-node-val)
                 (setq keys (project-sln-util--append-val keys last-node-val (list node-val)))
               (push (cons last-node-val (list node-val)) keys))
             (setq flag nil))
         (when (project-sln--is-contain-list-string
                '("class" "struct")
                node-val)
           (setq last-node-val node-val)
           (setq flag t)))))
    keys))


(provide 'project-sln-parse)
;;; project-sln-parse.el ends here
