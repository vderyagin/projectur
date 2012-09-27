;;; projectur.el --- Support for projects in Emacs

;; Copyright (C) 2012 Victor Deryagin

;; Author: Victor Deryagin <vderyagin@gmail.com>
;; Created: 3 Aug 2012
;; Version: 0.1.0

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

(defvar projectur-project-types
  '(("Version-controlled ruby project"
     :test projectur-ruby-project-under-version-control-p
     :tags-command "exuberant-ctags -e **/*.rb"
     :ignored-dirs ("tmp" "pkg"))
    ("Generic version-controlled project"
     :test projectur-version-controlled-repo-p))
  "A list with projects types descriptions.")

(defvar projectur-ignored-dirs
  '(".hg" ".git" ".bzr" ".svn" ".rbx" "_darcs" "_MTN" "CVS" "RCS" "SCCS")
  "List of names of directories, content of which will be excluded from any project.")

(defvar projectur-ignored-files
  '("*.elc" "*.rbc" "*.py[co]" "*.a" "*.o" "*.so" "*.bin" "*.class"
    "*.s[ac]ssc" "*.sqlite3" "TAGS" ".gitkeep" "*~" "#*#")
  "List of wildcards, matching names of files, which will be excluded from any project.")

(defvar projectur-history nil "List of visited projects.")

(defvar projectur-tags-command "exuberant-ctags -e --recurse ."
  "Shell command for generating TAGS file for project.
Executed in context of projects root directory.")

(defun projectur-history-cleanup ()
  "Delete invalid and duplicate projects from `projectur-history'."
  (setq projectur-history
        (loop
           for project in projectur-history
           for root = (projectur-project-root project)
           if (and
               (projectur-project-valid-p project)
               (not (member root seen-roots)))
           collect project into projects and collect root into seen-roots
           finally return projects)))

(defun projectur-history-add (project)
  "Add PROJECT to `projectur-history'."
  (when project
    (let ((root (projectur-project-root project))
          (conflicting-root (projectur-conflicting-root-from-history project)))
      (when conflicting-root
        (error (format
                "Failed to add project in '%s': conflict with other one in '%s'"
                (abbreviate-file-name root)
                (abbreviate-file-name conflicting-root))))

      (add-to-list 'projectur-history project)
      (projectur-history-cleanup))))

(defun projectur-conflicting-root-from-history (project)
  "Return root of project from `projectur-history' PROJECT conflicts with.
Return nil if no conflicts detected.  Conflict is understood as
parent-directory/subdirectory relationships between root of PROJECT
 and root of some other project from history.

Special case is when root of PROJECT matches root of project from history,
this is not considered a conflict, duplication is getting dealt with by
`projectur-history-cleanup'."
  (loop
     with root = (projectur-project-root project)
     for other-root in (mapcar 'projectur-project-root projectur-history)
     if (or (projectur-subdirectory-p root other-root)
            (projectur-subdirectory-p other-root root))
     return other-root))

(defun projectur-project-valid-p (project)
  "Return non-nil if PROJECT is valid, nil otherwise."
  (let ((root (car project))
        (test (plist-get (cdr project) :test)))
    (and
     (stringp root)
     (file-directory-p root)
     (if test
         (and (functionp test)
              (funcall test root))
         t))))

(defun projectur-current-project ()
  "Return project current buffer belongs to, nil if none."
  (let ((project (or
                  (projectur-project-try-find-in-history)
                  (projectur-project-try-fetch))))
    (projectur-history-add project)
    project))

(defun projectur-project-try-find-in-history ()
  "Make attempt to find current buffer's project in `projectur-history'.
Return nil if unsuccessful."
  (loop
     with current-directory = (file-name-as-directory default-directory)
     for project in projectur-history
     for root = (projectur-project-root project)
     if (or (string= current-directory root)
            (projectur-subdirectory-p current-directory root))
     return project))

(defun projectur-subdirectory-p (subdir dir)
  "Return non-nil if SUBDIR is a subdirectory of DIR, nil otherwise."
  (let ((subdir (file-name-as-directory subdir))
        (dir (file-name-as-directory dir)))
    (and (> (length subdir) (length dir))
         (string-prefix-p dir subdir))))

(defun projectur-project-try-fetch ()
  "Make attempt to fetch current project by going up filesystem tree.
Return nil if unsuccessful."
  (loop
     with project
     with dir = default-directory
     until (string= dir "/")
     thereis project
     do
       (setq project (projectur-project-with-root dir)
             dir (expand-file-name ".." dir))))

(defun projectur-project-with-root (root)
  "Return project with root in ROOT, nil if ROOT is not a root of any project."
  (loop
     for project-type in projectur-project-types
     for test-function = (plist-get (cdr project-type) :test)
     if (funcall test-function root)
     return (cons (file-name-as-directory root)
                  (cdr project-type))))

(defun* projectur-select-project-from-history (&optional (prompt "Select project: "))
  "Select single project from `projectur-history'."
  (projectur-complete
   prompt projectur-history
   (lambda (project)
     (let* ((root (abbreviate-file-name
                   (projectur-project-root project)))
            (name (projectur-project-name project)))
       (format "%-25s (%s)" name root)))))

(defun projectur-project-root (project)
  "Return root directory of PROJECT."
  (car project))

(defun projectur-project-tags-command (project)
  "Return TAGS generation comman for PROJECT."
  (or
   (plist-get (cdr project) :tags-command)
   projectur-tags-command))

(defun projectur-project-name (project)
  "Return name of PROJECT."
  (file-name-nondirectory
   (directory-file-name
    (projectur-project-root project))))

(defun projectur-project-ignored-dirs (project)
  "Return list of ignored directories for PROJECT."
  (append projectur-ignored-dirs
          (plist-get (cdr project) :ignored-dirs)))

(defun projectur-project-ignored-files (project)
  "Return list of wildcards of ignored files for PROJECT."
  (append projectur-ignored-files
          (plist-get (cdr project) :ignored-files)))

(defun projectur-find-cmd (project)
  "Generate find(1) command for finding al relevant files withing PROJECT."
  (let ((ignored-dirs (projectur-project-ignored-dirs project))
        (ignored-files (projectur-project-ignored-files project)))
    (projectur-with-project project
      (find-cmd
       `(prune (name ,@ignored-dirs))
       `(not (iname ,@ignored-files))
       '(type "f")
       '(print0)))))

(defun projectur-project-files (project)
  "List of absolute names for files, belonging to PROJECT."
  (let ((command (projectur-find-cmd project)))
    (delete ""
            (split-string
             (shell-command-to-string command)
             "\0"))))

(defun projectur-buffers (project)
  "Return list of buffers, visiting files, belonging to PROJECT."
  (loop
     for buf in (buffer-list)
     if (projectur-buffer-in-project-p buf project)
     collect buf))

(defun projectur-buffer-in-project-p (buffer-or-name project)
  "Return non-nil if BUFFER-OR-NAME is visiting a file, belonging to PROJECT."
  (let ((buf (get-buffer buffer-or-name))
        (root (projectur-project-root project)))
    (with-current-buffer buf
      (and
       buffer-file-name
       (string-prefix-p root buffer-file-name)))))

(defmacro projectur-with-project (project &rest body)
  "With `default-directory' bound to PROJECT root directory execute BODY."
  (declare (indent 1))
  `(progn
     (unless (projectur-project-valid-p ,project)
       (error (format "Invalid project: %s" ,project)))
     (let ((default-directory (projectur-project-root ,project)))
       ,@body)))

;;;###autoload
(defmacro projectur-define-command (command-name docstring &rest body)
  "Define COMMAND-NAME with DOCSTRING and BODY to be executed in context of project."
  (declare (indent 1))
  `(defun ,command-name (&optional choose-project)
     ,(concat
       docstring
       "\nIf called with prefix argument or current buffer does"
       "\nnot belong to any project, ask to choose project from list"
       "\nand use it as context for executing BODY."
       "\n"
       "\nin BODY you can use variable `project' which refers to the"
       "\nproject in context of which command is being executed.")
     (interactive "P")
     (let (project)
       (unless choose-project
         (setq project (projectur-current-project)))
       (unless project
         (setq project (projectur-select-project-from-history)))
       (projectur-with-project project
         ,@body))))

;;;###autoload
(font-lock-add-keywords
 'emacs-lisp-mode
 '(("(\\(projectur-define-command\\) +\\([^ ]+\\)"
    (1 'font-lock-keyword-face)
    (2 'font-lock-function-name-face))))

;;;###autoload
(defun projectur-set-project-root (dir)
  "Set DIR as root of current project."
  (interactive "DProject root: ")
  (projectur-history-add
   (cons (file-name-as-directory (expand-file-name dir))
         '(:test nil))))

;;;###autoload
(defun projectur-delete-from-history ()
  "Select project to delete from `projectur-history'."
  (interactive)
  (let ((project (projectur-select-project-from-history "Delete project: ")))
    (setq projectur-history
          (delete project projectur-history))
    (message "Project \"%s\" deleted from history."
             (abbreviate-file-name (projectur-project-root project)))))

;;;###autoload (autoload 'projectur-goto-root "projectur" nil t)
(projectur-define-command projectur-goto-root
  "Open root directory of current project."
  (find-file default-directory))

;;;###autoload (autoload 'projectur-find-file "projectur" nil t)
(projectur-define-command projectur-find-file
  "Open file from current project."
  (let ((files (projectur-project-files project))
        (root (projectur-project-root project)))
    (find-file
     (projectur-complete
      "Find file in project: " files
      (lambda (file) (file-relative-name file root))))))

;;;###autoload (autoload 'projectur-rgrep "projectur" nil t)
(projectur-define-command projectur-rgrep
  "Run `rgrep' command in context of the current project root directory."
  (call-interactively 'rgrep))

;;;###autoload (autoload 'projectur-execute-shell-command "projectur" nil t)
(projectur-define-command projectur-execute-shell-command
  "Execute shell command in context of the current project root directory."
  (call-interactively 'shell-command))

;;;###autoload (autoload 'projectur-ack "projectur" nil t)
(projectur-define-command projectur-ack
  "Run `ack' command (if available) in context of the current project root directory."
  (if (fboundp 'ack)
      (call-interactively 'ack)
      (error "You need `ack' command installed in order to use this functionality")))

;;;###autoload (autoload 'projectur-version-control "projectur" nil t)
(projectur-define-command projectur-version-control
  "Open appropriate version control interface for current project."
  (cond
    ((and
      (projectur-git-repo-p default-directory)
      (fboundp 'magit-status))
     (magit-status default-directory))
    ((and
      (projectur-mercurial-repo-p default-directory)
      (fboundp 'ahg-status))
     (ahg-status default-directory))
    (t
     (vc-dir default-directory nil))))

;;;###autoload (autoload 'projectur-generate-tags "projectur" nil t)
(projectur-define-command projectur-generate-tags
  "Generate TAGS file for current project."
  (let ((command (projectur-project-tags-command project)))
    (shell-command
     (read-string "Generate TAGS like this: "
                  command nil command))
    (setq tags-file-name (expand-file-name "TAGS"))))

;;;###autoload (autoload 'projectur-save "projectur" nil t)
(projectur-define-command projectur-save
  "Save all opened buffers that belong to current project."
  (mapc
   (lambda (buf)
     (with-current-buffer buf
       (save-buffer)))
   (projectur-buffers project)))

(defun* projectur-complete (prompt choices &optional (display-fn 'identity))
  "Select one of CHOICES, with PROMPT, use DISPLAY-FN for display if provided,
`identity' otherwise."
  (let* ((results-map
          (mapcar (lambda (choice)
                    (cons (funcall display-fn choice) choice))
                  choices))
         (display-choices
          (mapcar 'car results-map))
         (chosen
          (ido-completing-read prompt display-choices)))
    (cdr (assoc chosen results-map))))

(defun projectur-show-current-file ()
  "Show path of current file relative to its project root in minibuffer.
Show absolute path if current file does not belong to any project.
Display error if current buffer is not visiting a file."
  (interactive)
  (unless buffer-file-name
    (error "Current buffer does not belong to any project"))
  (let ((project (projectur-current-project)))
    (message
     "%s"
     (if project
         (file-relative-name buffer-file-name
                             (projectur-project-root project))
         (abbreviate-file-name buffer-file-name)))))

(defalias 'projectur-hg-repo-p 'projectur-mercurial-repo-p)
(defalias 'projectur-svn-repo-p 'projectur-subversion-repo-p)

(defun projectur-git-repo-p (dir)
  "Return non-nil if DIR is a root of git repository, nil otherwise."
  (file-directory-p
   (expand-file-name ".git" dir)))

(defun projectur-mercurial-repo-p (dir)
  "Return non-nil if DIR is a root of mercurial repository, nil otherwise."
  (file-directory-p
   (expand-file-name ".hg" dir)))

(defun projectur-subversion-repo-p (dir)
  "Return non-nil if DIR is a root of subversion repository, nil otherwise."
  (and
   (file-directory-p (expand-file-name ".svn" dir))
   (not
    (file-directory-p (expand-file-name "../.svn" dir)))))

(defun projectur-cvs-repo-p (dir)
  "Return non-nil if DIR is a root of CVS repository, nil otherwise."
  (and
   (file-directory-p (expand-file-name "CVS" dir))
   (not
    (file-directory-p (expand-file-name "../CVS" dir)))))

(defun projectur-darcs-repo-p (dir)
  "Return non-nil if DIR is a root of Darcs repository, nil otherwise."
  (file-directory-p
   (expand-file-name "_darcs" dir)))

(defun projectur-ruby-gem-p (dir)
  "Return non-nil if DIR is a root of ruby gem source tree, nil otherwise."
  (file-expand-wildcards
   (expand-file-name "*.gemspec" dir)))

(defun projectur-rails-app-p (dir)
  "Return non-nil if DIR is a root of ruby-on-rails application, nil otherwise."
  (file-regular-p
   (expand-file-name "script/rails" dir)))

(defun projectur-rake-project-p (dir)
  "Return non-nil if DIR is a root of project using rake, nil otherwise."
  (loop
     for rakefile in '("rakefile" "Rakefile" "rakefile.rb" "Rakefile.rb")
     thereis (file-regular-p (expand-file-name rakefile dir))))

(defun projectur-bundler-project-p (dir)
  "Return non-nil if DIR is a root of project using bundler, nil otherwise."
  (file-regular-p
   (expand-file-name "Gemfile" dir)))

(defun projectur-version-controlled-repo-p (dir)
  "Return non-nil if DIR is a root of version-controlled project, nil otherwise.
Supported version control systems: git, mercurial, subversion, cvs, darcs."
  (or
   (projectur-git-repo-p dir)
   (projectur-mercurial-repo-p dir)
   (projectur-subversion-repo-p dir)
   (projectur-cvs-repo-p dir)
   (projectur-darcs-repo-p dir)))

(defun projectur-ruby-project-under-version-control-p (dir)
  "Return non-nil if DIR is a root of version-controlled ruby project."
  (and
   (projectur-version-controlled-repo-p dir)
   (or
    (projectur-rails-app-p dir)
    (projectur-ruby-gem-p dir)
    (projectur-bundler-project-p dir)
    (file-regular-p (expand-file-name "spec/spec_helper.rb" dir))
    (file-regular-p (expand-file-name "test/test_helper.rb" dir))
    (file-regular-p (expand-file-name "features/support/env.rb" dir))
    (file-regular-p (expand-file-name ".rspec" dir))
    (file-regular-p (expand-file-name ".rvmrc" dir))
    (file-regular-p (expand-file-name ".rbenv-version" dir)))))

(provide 'projectur)

;;; projectur.el ends here

;; Local Variables:
;; lexical-binding: t
;; coding: us-ascii
;; End:
