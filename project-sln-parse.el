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


(defun project-sln--resolve-keywords-csharp (ast)
  "Resolve keywords in csharp by AST."
  (let ((keys '()) (flag-next-node nil)
        (node-type "") (node-val "") (last-node-val "")
        (type-name ""))
    (project-sln--walk-ast-tree
     ast
     (lambda (node)
       (setq node-type (car node))
       (setq node-val (cdr node))
       (if flag-next-node
           (progn
             (setq keys (project-sln-util--append-val-safe keys last-node-val
                                                           (cons node-val nil)))
             (setq type-name last-node-val)
             (setq flag-next-node nil))
         (cond
          ((project-sln--is-contain-list-string
            '("class" "struct")
            node-val)
           (setq flag-next-node t))
          ((string= "=" node-val)
           (setq keys (project-sln-util--append-val-safe keys "var" last-node-val))
           ))
         (setq last-node-val node-val)
         )))
    keys))


(provide 'project-sln-parse)
;;; project-sln-parse.el ends here
