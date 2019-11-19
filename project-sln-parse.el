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


(defconst project-sln--key-var "var"
  "Id key to represent variable.")

(defconst project-sln--key-fnc "fnc"
  "Id key to represent function.")


(defun project-sln--resolve-keywords-csharp (ast)
  "Resolve keywords in csharp by AST."
  (let ((keys '()) (flag-next-node nil)
        (node-type "") (node-val "") (last-node-val "")
        (struct-name "")
        (type-name ""))
    (project-sln--walk-ast-tree
     ast
     (lambda (node)
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
                  (list struct-name type-name project-sln--key-var)
                  (list last-node-val)))
           )
          ((string= "(" node-val)
           (setq keys
                 (project-sln-util--add-json-val
                  keys
                  (list struct-name type-name project-sln--key-fnc)
                  (list last-node-val)))
           )
          )
         (setq last-node-val node-val)
         )))
    keys))


(provide 'project-sln-parse)
;;; project-sln-parse.el ends here
