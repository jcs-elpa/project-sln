;;; project-sln.el --- Project structure organizer  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Shen, Jen-Chieh
;; Created date 2019-11-10 21:03:32

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Project structure organizer.
;; Keyword: project structure organize
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1") (parse-it "0.1.7") (f "0.20.0"))
;; URL: https://github.com/jcs-elpa/project-sln

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
;; Project structure organizer.
;;

;;; Code:

(require 'cl-lib)
(require 'f)
(require 'parse-it)

(require 'project-sln-util)
(require 'project-sln-parse)

(defgroup project-sln nil
  "Project structure organizer."
  :prefix "project-sln-"
  :group 'tool
  :link '(url-link :tag "Github" "https://github.com/jcs-elpa/project-sln"))

(defcustom project-sln-cache-filename "project-cache.json"
  "Name of the cache file."
  :type 'string
  :group 'project-sln)

(defcustom project-sln-ignore-paths
  '("SCCS" "RCS" "CVS" "MCVS" "MTN" "_darcs" "{arch}"
    ".git" ".src" ".svn" ".hg" ".bzr"
    ".vs" ".vscode"
    "bin" "build" "res"
    "node_modules")
  "List of path will be ignored."
  :type 'list
  :group 'project-sln)

(defconst project-sln-mode-extension
  ;; From by => | `major-mode' | `parse-key' | `extensions' |
  '((("c-mode") "c" (".c" ".h"))
    (("c++-mode") "c++" (".c" ".cpp" ".h" ".hpp" ".hin" ".cin"))
    (("objc-mode") "objc" (".h" ".m"))
    (("csharp-mode") "csharp" (".cs"))
    (("js-mode" "js2-mode" "js3-mode") "js" (".js"))
    (("typescript-mode") "typescript" (".ts")))
  "List of extension corresponds to major mode.")

(defconst project-sln-cache-template
  '(("$MODE$" .
     (("parse-key" . "") ("ext" . ()) ("keys" . ()))))
  "Language section template that will converted to JSON.")

(defvar project-sln--parse-key "" "Currnet language parse key.")
(defvar project-sln--extensions '() "Currnet language extensions.")
(defvar project-sln--paths '() "Current language paths.")

(defvar project-sln--cache '() "Current project cache.")

;;; Util

(defun project-sln--project-dir ()
  "Return project directory path."
  (cdr (project-current)))

(defun project-sln--form-extension-regexp (ext)
  "Form EXT's regular expression for file extension checking."
  (format "\\%s$" ext))

(defun project-sln--indentify-mode-info ()
  "Find the current major mode's possible extension and parse key."
  (let ((index 0) (break nil) (mode-ext nil) (modes nil))
    (while (and (< index (length project-sln-mode-extension)) (not break))
      (setq mode-ext (nth index project-sln-mode-extension))
      (setq modes (nth 0 mode-ext))
      (when (project-sln-util--is-contain-list-string modes (symbol-name major-mode))
        (setq project-sln--parse-key (make-symbol (nth 1 mode-ext)))
        (setq project-sln--extensions (nth 2 mode-ext))
        (setq break t))
      (setq index (1+ index)))))

(defun project-sln--form-cache-file-path ()
  "Form the cache file's file path."
  (format "%s%s" (project-sln--project-dir) project-sln-cache-filename))

(defun project-sln--cache-file-exists-p ()
  "Check if the cache file exists."
  (and (project-sln--project-dir)
       (file-directory-p (project-sln--form-cache-file-path))))

(defun project-sln--f-directories-ignore-directories (path &optional rec)
  "Find all directories in PATH by ignored common directories with FN and REC."
  (let ((dirs (f-directories path))
        (valid-dirs '())
        (final-dirs '())
        (ignore-lst (append grep-find-ignored-directories
                            project-sln-ignore-paths
                            (if (boundp 'projectile-globally-ignored-directories)
                                projectile-globally-ignored-directories
                              '()))))
    (dolist (dir dirs)
      (unless (project-sln-util--is-contain-list-string ignore-lst dir)
        (push dir valid-dirs)))
    (when rec
      (dolist (dir valid-dirs)
        (push (project-sln--f-directories-ignore-directories dir rec) final-dirs)))
    (setq valid-dirs (reverse valid-dirs))
    (setq final-dirs (reverse final-dirs))
    (project-sln-util--flatten-list (append valid-dirs final-dirs))))

(defun project-sln--f-files-ignore-directories (path &optional fn rec)
  "Find all files in PATH by ignored common directories with FN and REC."
  (let ((dirs (append (list path) (project-sln--f-directories-ignore-directories path rec)))
        (files '()))
    (dolist (dir dirs)
      (push (f-files dir fn) files))
    (project-sln-util--flatten-list (reverse files))))

(defun project-sln--normalize-paths ()
  "Normalize the path to relative path to project directory."
  (let ((index 0) (path ""))
    (while (< index (length project-sln--paths))
      (setq path (nth index project-sln--paths))
      (setf (nth index project-sln--paths) (s-replace (project-sln--project-dir) "./" path))
      (setq index (1+ index)))))

;;; Core

(defun project-sln--valid-node-type-p (node-type)
  "Check if NODE-TYPE valid node type."
  (or (equal node-type :node-type)
      (equal node-type :value)
      (equal node-type :position)
      (equal node-type :children)))

(defun project-sln--walk-ast-tree (ast-tree fnc)
  "Walk through AST-TREE execute FNC."
  (let ((index 0) (node nil) (next-node nil))
    (while (< index (length ast-tree))
      (setq node (nth index ast-tree))
      (let ((node-type (car node)) (node-val (cdr node)) (valid-nt nil))
        (when node-val
          (if (listp node-val)
              (progn
                (setq valid-nt (project-sln--valid-node-type-p node-type))
                (when valid-nt
                  (project-sln-parse--inc/dec-nested-level 1))
                (project-sln--walk-ast-tree (if valid-nt node-val node) fnc)
                (when valid-nt
                  (project-sln-parse--inc/dec-nested-level -1)))
            (when (equal node-type :value)
              (setq next-node (nth (1+ index) ast-tree))  ; get position
              (funcall fnc node next-node)))))
      (setq index (1+ index)))))

(defun project-sln--resolve-keywords (ast)
  "Resolved keyword from AST."
  (funcall (intern (format "project-sln-parse--resolve-keywords-%s" project-sln--parse-key)) ast))

(defun project-sln--set-language-template (&optional parse-key mode ext path)
  "Fill up the language template using PARSE-KEY, EXT, MODE and PATH.
The language template here indciate variable `project-sln-cache-template'."
  (unless mode (setq mode major-mode))
  (unless parse-key (setq parse-key project-sln--parse-key))
  (unless ext (setq ext project-sln--extensions))
  (unless path (setq path project-sln--paths))
  (let ((cache-template (copy-sequence project-sln-cache-template))
        (default-directory (project-sln--project-dir))
        (keys '()) (parse-result nil))
    (setf (car (nth 0 cache-template)) mode)
    (setf (cdr (nth 0 (cdr (nth 0 cache-template)))) parse-key)
    (setf (cdr (nth 1 (cdr (nth 0 cache-template)))) ext)
    (dolist (fp path)
      (setq parse-result (parse-it project-sln--parse-key (expand-file-name fp)))
      (push (cons fp (project-sln--resolve-keywords parse-result)) keys))
    (setf (cdr (nth 2 (cdr (nth 0 cache-template)))) keys)
    cache-template))

(defun project-sln--read-cache ()
  "Read the cache file so the project will know where to go."
  (let ((cache-content (project-sln-util--read-file (project-sln--form-cache-file-path))))
    (setq project-sln--cache (json-read-from-string cache-content))))

(defun project-sln--write-cache ()
  "Write memory buffer to cache."
  (setq project-sln--cache (project-sln--set-language-template))
  (write-region (json-encode project-sln--cache)
                nil
                (project-sln--form-cache-file-path)))

(defun project-sln--new-cache ()
  "First time create cache, this may take a while."
  (project-sln--indentify-mode-info)
  (setq project-sln--paths (project-sln--f-files-ignore-directories
                            (project-sln--project-dir)
                            (lambda (fp)
                              (project-sln-util--is-contain-list-string project-sln--extensions fp))
                            t))
  (project-sln--normalize-paths)
  (project-sln--write-cache))

(defun project-sln--init ()
  "Initialize project sln."
  (setq project-sln--parse-key "")
  (setq project-sln--extensions '())
  (setq project-sln--paths '()))

(defun project-sln-evaluate-project ()
  "Evaluate the whole project into cache."
  (project-sln--init)
  (if (project-sln--project-dir)
      (if (project-sln--cache-file-exists-p)
          (project-sln--read-cache)
        (project-sln--new-cache))
    (user-error "[WARNING] Project root not found for evaluating")))

;;;###autoload
(defun project-sln-goto-definition-at-point ()
  "Goto the definition at current point."
  (interactive)
  (project-sln-evaluate-project)
  (message "cache: %s" project-sln--cache)
  )

(provide 'project-sln)
;;; project-sln.el ends here
