
(require 'f)

(defun parse-it-test--load-files (dir files)
  "Load FILES under a DIR."
  (dolist (path files)
    (let ((filepath (concat (file-name-as-directory dir) path)))
      (when (and
             ;; ignore `-pkg' file.
             (not (string-match-p "-pkg.el" filepath))
             ;; Ignore test files.
             (not (string-match-p "test" filepath)))
        (load-file filepath)))))

(defun parse-it-test--all-dirs (lst)
  "Load LST of directory."
  (dolist (dir lst)
    (let ((files-el (directory-files dir nil "\\.el$"))
          (files-elc (directory-files dir nil "\\.elc$")))
      (parse-it-test--load-files dir files-el)
      (parse-it-test--load-files dir files-elc))))

;; --------------------------------------------------------------------------
;; Start testing..

;; NOTE: Add load path for project/package root.
(add-to-list 'load-path (expand-file-name "../"))

(let* ((project-path (expand-file-name "../../parse-it/"))
       (dirs (f-directories project-path))
       (final-list (append (list project-path) dirs)))
  (dolist (path final-list) (add-to-list 'load-path path))
  (parse-it-test--all-dirs final-list)  ; Load all core files.

  (require 'parse-it)

  (message "[INFO] Done require 'parse-it' ::\n"))
