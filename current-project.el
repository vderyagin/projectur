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

(defvar cpr-ignored-dirs
  '(".hg" ".git" ".bzr" ".svn" "_darcs" "_MTN" "CVS" "RCS" "SCCS")
  "List of names of directories, content of which will not be considered part of the project.")

(defvar cpr-ignored-files-wildcards
  '("*.elc" "*.rbc" "*.py[co]" "*.a" "*.o" "*.so" "*.bin"
    "*.class" "*.s[ac]ssc" "*.sqlite3" "TAGS" ".gitkeep")
  "List of wildcards, matching names of files, which will not be considered part of the project.")

(defvar cpr-project nil
  "Current project.")
(make-variable-buffer-local 'cpr-project)

(defvar cpr-types-specs
  '((:type "Ruby on Ralis application"
     :test cpr-rails-app-p)
    (:type "Generic Git project"
     :test cpr-git-repo-p)
    (:type "Generic Mercurial project"
     :test cpr-mercurial-repo-p))
  "A list of plists describing project types.")

(defun cpr--root ()
  (plist-get cpr-project :root))

(defun cpr--type ()
  (plist-get cpr-project :type))

(defun cpr--name ()
  (plist-get cpr-project :name))

(defun cpr--set-property (property value)
  (setq cpr-project
        (plist-put cpr-project property value)))

(defun* cpr--set-properties (&key root name type)
  (when root
    (setq cpr-project
          (plist-put cpr-project :root root)))
  (when name
    (setq cpr-project
          (plist-put cpr-project :name name)))
  (when type
    (setq cpr-project
          (plist-put cpr-project :type type))))

(defun cpr-test-for-project (project)
  ""
  (let* ((project-type
          (cpr--type))
         (corresponding-spec
          (cpr-spec-for-project-type project-type)))
    (plist-get corresponding-spec :test)))

(defun cpr-valid-p ()
  ""
  (let ((root (cpr--root))
        (name (cpr--name))
        (type (cpr--type)))
    (and
     (stringp root)
     (stringp name)
     (stringp type)
     (member type
             (mapcar
              (lambda (spec)
                (plist-get spec :type))
              cpr-types-specs))
     (file-directory-p root)
     (funcall (cpr-test-for-project cpr-project) root)
     (equal name (cpr-name-from-directory root)))))

(defun cpr-name-from-directory (dir)
  (unless (file-name-absolute-p dir)
    (error "Argument should be absolute path"))
  (file-name-nondirectory
   (directory-file-name dir)))

(defun cpr-reset-project ()
  (setq cpr-project nil))

(defun cpr-fetch ()
  "Populate `cpr-project'"
  (cpr-reset-project)
  (let ((current-dir (file-name-as-directory default-directory)))
    (loop
       thereis cpr-project
       until (and (equal current-dir "/")
                  (error "Current buffer does not belong to any project"))
       do
         (let ((type (cpr-identify-directory current-dir)))
           (when type
             (cpr--set-properties
              :root current-dir
              :type type
              :name (cpr-name-from-directory current-dir))))
         (setq current-dir
               (file-name-as-directory
                (expand-file-name ".." current-dir))))))

(defun cpr-spec-for-project-type (project-type)
  "Return project spec based on its type, or nil if not found."
  (loop
     for spec in cpr-types-specs
     thereis (and
              (equal (plist-get spec :type) project-type)
              spec)))

(defun cpr-identify-directory (directory)
  "Return project type if DIRECTORY is a valid project root
accorging to rules defined in `cpr-types-specs',
nil otherwise"
  (and (file-directory-p directory)
       (loop
          for spec in cpr-types-specs
          thereis (and
                   (funcall (plist-get spec :test) directory)
                   (plist-get spec :type)))))

(defun cpr-build-find-cmd ()
  "Construct find(1) command that returns all files within current project."
  (with-project
      (find-cmd
       `(prune (name ,@cpr-ignored-dirs))
       `(not (iname ,@cpr-ignored-files-wildcards))
       '(type "f"))))

(defun cpr-files ()
  "Get list of all files within current project"
  (split-string
   (shell-command-to-string (cpr-build-find-cmd))))

;;;###autoload
(defmacro with-project (&rest body)
  `(progn
     (unless (cpr-valid-p)
       (cpr-fetch))
     (let ((default-directory (cpr--root)))
       ,@body)))

(provide 'current-project)

;;; current-project.el ends here
