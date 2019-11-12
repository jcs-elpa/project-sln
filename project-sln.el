;;; project-sln.el --- Project structure organizer.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Shen, Jen-Chieh
;; Created date 2019-11-10 21:03:32

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Project structure organizer.
;; Keyword: project structure organize
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4") (parse-it "0.0.1"))
;; URL: https://github.com/jcs090218/project-sln

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

(require 'parse-it)


(defgroup project-sln nil
  "Project structure organizer."
  :prefix "project-sln-"
  :group 'tool
  :link '(url-link :tag "Github" "https://github.com/jcs090218/project-sln"))

(defcustom project-sln-cache-filename "project-cache"
  "Name of the cache file."
  :type 'string
  :group 'project-sln)

(defconst project-sln-mode-extension
  '((("c-mode" "c++-mode" "objc-mode") . (".c" ".cpp" ".h" ".hpp" ".hin" ".cin" ".m"))
    (("csharp-mode") . (".cs"))
    (("js-mode" "js2-mode" "js3-mode") . (".js"))
    (("typescript-mode") . (".ts")))
  "List of extension corresponds to major mode.")


(defun project-sln--is-contain-list-string (in-list in-str)
  "Check if a string IN-STR contain in any string in the string list IN-LIST."
  (cl-some #'(lambda (lb-sub-str) (string-match-p (regexp-quote lb-sub-str) in-str)) in-list))

(defun project-sln--find-mode-extension ()
  "Find the current major mode's possible extension."
  (let ((index 0) (break nil) (mode-ext nil) (modes nil) (exts nil))
    (while (and (< index (length project-sln-mode-extension)) (not break))
      (setq mode-ext (nth index project-sln-mode-extension))
      (setq modes (car mode-ext))
      (when (project-sln--is-contain-list-string modes (symbol-name major-mode))
        (setq exts (cdr mode-ext))
        (setq break t))
      (setq index (1+ index)))
    exts))

(defun project-sln--form-cache-file-path ()
  "Form the cache file's file path."
  (format "%s%s" (cdr (project-current)) project-sln-cache-filename))

(defun project-sln--cache-file-exists-p ()
  "Check if the cache file exists."
  (and (cdr (project-current))
       (file-directory-p (project-sln--form-cache-file-path))))

(defun project-sln--read-cache ()
  "Read the cache file so the project will know where to go."
  )

(defun project-sln--write-cache ()
  "Write memory buffer to cache."
  (write-region "Hello World 2" nil (project-sln--form-cache-file-path))
  )

(defun project-sln--new-cache ()
  "First time create cache, this may take a while."
  ;; TODO: ff-find-files
  )

(defun project-sln-evaluate-project ()
  "Evaluate the whole project into cache."
  (if (cdr (project-current))
      (if (project-sln--cache-file-exists-p)
          (project-sln--read-cache)
        (project-sln--new-cache))
    (user-error "[WARNING] Project root not found for evaluating")))

;;;###autoload
(defun project-sln-goto-definition-at-point ()
  ""
  (interactive)
  ;;(project-sln-evaluate-project)
  (message "%s" (project-sln--find-mode-extension))
  )


(provide 'project-sln)
;;; project-sln.el ends here
