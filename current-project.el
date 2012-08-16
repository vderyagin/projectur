;;; current-project.el --- Support for projects in Emacs

;; Copyright (C) 2012 Victor Deryagin

;; Author: Victor Deryagin <vderyagin@gmail.com>
;; Created: 3 Aug 2012
;; Version: 0.0.1

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'find-cmd)

(require 'current-project-directory-predicates)
(require 'current-project-commands)

(defvar cpr-project nil
  "Current project.")
(make-variable-buffer-local 'cpr-project)

(defvar cpr-ignored-dirs
  '(".hg" ".git" ".bzr" ".svn" "_darcs" "_MTN" "CVS" "RCS" "SCCS")
  "List of names of directories, content of which will not be considered part of the project.")

(defvar cpr-ignored-files
  '("*.elc" "*.rbc" "*.py[co]" "*.a" "*.o" "*.so" "*.bin"
    "*.class" "*.s[ac]ssc" "*.sqlite3" "TAGS" ".gitkeep")
  "List of wildcards, matching names of files, which will not be considered part of the project.")

(defvar cpr-type-specs
  '((:type "Ruby on Ralis application"
     :test cpr-rails-app-p
     :ignored-dirs ("tmp"))
    (:type "Generic Git project"
     :test cpr-git-repo-p)
    (:type "Generic Mercurial project"
     :test cpr-mercurial-repo-p))
  "A list of plists describing project types.")

(defun cpr-project (&optional property)
  "When PROPERTY argument is provided - returns that property of
`cpr-project', otherwise returns full object."
  (if property
      (plist-get cpr-project property)
      cpr-project))

(defun cpr-project-valid-p ()
  "Check whether current project is valid."
  (let ((root (cpr-project :root))
        (name (cpr-project :name))
        (type (cpr-project :type)))
    (and
     (stringp root)
     (stringp name)
     (stringp type)
     (not (equal root ""))
     (not (equal name ""))
     (not (equal type ""))
     (file-directory-p root))))

(defun cpr-fetch ()
  "Populate `cpr-project'"
  (setq cpr-project nil)                ; reset it first
  (let ((current-dir (file-name-as-directory default-directory)))
    (flet ((reached-filesystem-root-p ()
             (equal current-dir "/"))

           (goto-parent-directory ()
             (setq current-dir
                   (file-name-as-directory
                    (expand-file-name ".." current-dir))))

           (name-from-directory (dir)
             (file-name-nondirectory
              (directory-file-name dir)))

           (identify-directory (dir)
             (loop
                for spec in cpr-type-specs
                for matches-p = (funcall (plist-get spec :test) dir)
                for spec-type = (plist-get spec :type)
                if matches-p return spec-type))

           (set-project-properties (&key root name type)
             (loop
                for prop in '(root name type)
                for key = (intern (concat ":" (symbol-name prop)))
                for val = (symbol-value prop)
                if val do
                  (setq cpr-project
                        (plist-put cpr-project key val)))))

      (loop
         (when (reached-filesystem-root-p)
           (error "Current buffer does not belong to any project"))
         (let ((type (identify-directory current-dir)))
           (when type
             (set-project-properties
              :root current-dir
              :type type
              :name (name-from-directory current-dir))
             (return)))
         (goto-parent-directory)))))

(defun cpr-from-spec (param)
  (let ((spec
         (loop
            for spec in cpr-type-specs
            for spec-type = (plist-get spec :type)
            for found-spec-p = (equal spec-type (cpr-project :type))
            if found-spec-p return spec)))
    (plist-get spec param)))

(defun cpr-build-find-cmd ()
  "Construct find(1) command that returns all files, that belong to current project."
  (let ((ignored-dirs
         (append (cpr-from-spec :ignored-dirs)
                 cpr-ignored-dirs))
        (ignored-files
         (append (cpr-from-spec :ignored-files)
                 cpr-ignored-files)))
    (with-cpr-project
        (find-cmd
         `(prune (name ,@ignored-dirs))
         `(not (iname ,@ignored-files))
         '(type "f")))))

(defun cpr-files ()
  "Get list of all files, that belong to current project."
  (split-string
   (shell-command-to-string (cpr-build-find-cmd))))

;;;###autoload
(defmacro with-cpr-project (&rest body)
  "Execute BODY with `default-directory' bound to current project's root."
  (declare (indent 0))
  `(progn
     (unless (cpr-project-valid-p)
       (cpr-fetch))
     (let ((default-directory (cpr-project :root)))
       ,@body)))

(provide 'current-project)

;;; current-project.el ends here
