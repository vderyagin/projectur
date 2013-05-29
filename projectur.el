;;; projectur.el --- Support for projects in Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2012-2013 Victor Deryagin

;; Author: Victor Deryagin <vderyagin@gmail.com>
;; Created: 3 Aug 2012
;; Version: 0.1.2

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'ido)

(defgroup projectur nil
  "Tool for managing and navigating projects."
  :prefix "projectur-"
  :group 'tools
  :group 'convenience)

(defcustom projectur-tags-default-command "exuberant-ctags -e --recurse ."
  "Shell command for generating TAGS file for project.
Executed in context of projects root directory."
  :group 'projectur
  :type 'string)

(defcustom projectur-ignored-dirs
  '(".hg" ".git" ".bzr" ".svn" ".rbx" "_darcs" "_MTN" "CVS" "RCS" "SCCS" "tmp" "node_modules"
    ".idea")
  "List of names of directories, content of which will be excluded from any project."
  :group 'projectur
  :type '(choice (repeat :tag "Ignored directories" string)
                 (const :tag "No ignored directories" nil)))

(defcustom projectur-ignored-files
  '("*.elc" "*.rbc" "*.py[co]" "*.a" "*.o" "*.so" "*.bin" "*.class"
    "*.s[ac]ssc" "*.sqlite3" "TAGS" ".gitkeep" "*~" "#*#")
  "List of wildcards, matching names of files, which will be excluded from any project."
  :group 'projectur
  :type '(choice (repeat :tag "Ignored files" string)
                 (const :tag "No ignored files" nil)))

(defcustom projectur-default-readme-file-name "Readme.md"
  "Default name for project README file."
  :group 'projectur
  :type 'string)

(defcustom projectur-project-types
  '((:type "Version-controlled ruby project"
     :test projectur-ruby-project-under-version-control-p
     :tags-command "exuberant-ctags -e **/*.rb"
     :ignored-dirs ("tmp" "pkg"))
    (:type "sbt project"
     :test projectur-sbt-project-p
     :ignored-dirs ("project" "target"))
    (:type "Generic version-controlled project"
     :test projectur-version-controlled-repo-p))
  "A list with projects types descriptions."
  :group 'projectur
  :type '(repeat (plist :tag "Project type specification")))

(defvar projectur-history nil "List of visited projects.")

;;;###autoload
(progn
  (defvar projectur-command-prefix (kbd "C-c p")
    "Prefix of projectur bindings.")

  (defvar projectur-map
    (let ((map (make-sparse-keymap)))
      (define-key map "!" 'projectur-execute-shell-command)
      (define-key map "+" 'projectur-set-project-root)
      (define-key map "." 'projectur-show-current-file)
      (define-key map "R" 'projectur-goto-readme)
      (define-key map "T" 'projectur-generate-tags)
      (define-key map "a" 'projectur-ack)
      (define-key map "d" 'projectur-delete-from-history)
      (define-key map "f" 'projectur-find-file)
      (define-key map "g" 'projectur-rgrep)
      (define-key map "r" 'projectur-goto-root)
      (define-key map (kbd "C-x C-s") 'projectur-save)
      (define-key map (kbd "C-x k") 'projectur-kill-buffers)
      map)
    "Key map with projectur commands.")

  (define-key global-map projectur-command-prefix projectur-map))

(defmacro projectur-with-project (project &rest body)
  "With `default-directory' bound to PROJECT root directory execute BODY."
  (declare (indent 1))
  `(progn
     (unless (projectur-project-valid-p ,project)
       (projectur-history-cleanup)
       (error (format "Invalid project: %s" ,project)))
     (let ((default-directory (projectur-project-root ,project)))
       ,@body)))

(defmacro projectur-with-current-project (&rest body)
  "With `default-directory' bound to root directory of current project execute BODY."
  (declare (indent 0))
  `(let ((project (projectur-current-project)))
     (unless (projectur-project-valid-p project)
       (error "Current buffer does not seem to belong to any project"))
     (projectur-with-project project
       ,@body)))

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
Return nil if no conflicts detected. Conflict is understood as
parent-directory/subdirectory relationships between root of PROJECT
 and root of some other project from history.

Special case is when root of PROJECT matches root of project from
history, this is not considered a conflict, duplication gets
dealt with by `projectur-history-cleanup'."
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
         (funcall test root)
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
     with current-directory = (expand-file-name (file-name-as-directory default-directory))
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
     with dir = (expand-file-name default-directory)
     initially (when (file-remote-p dir) (return))
     until (string= dir "/")
     thereis project
     do
       (setq project (projectur-project-with-root dir)
             dir (expand-file-name ".." dir))))

(defun projectur-project-with-root (root)
  "Return project with root in ROOT, nil if ROOT is not a root of any project."
  (loop
     for project-type in projectur-project-types
     for test-function = (plist-get project-type :test)
     if (funcall test-function root)
     return (cons (file-name-as-directory root)
                  project-type)))

(defun* projectur-select-project-from-history (&optional (prompt "Select project: "))
  "Select single project from `projectur-history'."
  (projectur-complete
   prompt projectur-history
   (lambda (project)
     (let* ((root (abbreviate-file-name
                   (projectur-project-root project)))
            (name (projectur-project-name project)))
       (format "%-30s (%s)" name root)))))

(defun projectur-project-root (project)
  "Return root directory of PROJECT."
  (car project))

(defun projectur-project-tags-command (project)
  "Return TAGS generation comman for PROJECT."
  (or
   (plist-get (cdr project) :tags-command)
   projectur-tags-default-command))

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

(defun projectur-project-readme (project)
  "Find README file for project PROJECT, return nil if none."
  (loop
     with root = (projectur-project-root project)
     for pattern in (mapcar
                     (lambda (p) (expand-file-name p root))
                     '("Readme*" "readme*" "README*"))
     thereis (car (file-expand-wildcards pattern))))

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

;;;###autoload
(defun projectur-goto-root (choose-project)
  "Open root directory of current project.
If CHOOSE-PROJECT is non-nil or current buffer does not belong
to any project, ask to choose project from list and use it as
context for executing."
  (interactive "P")
  (let (project)
    (unless choose-project
      (setq project (projectur-current-project)))
    (unless project
      (setq project (projectur-select-project-from-history)))
    (find-file (projectur-project-root project))))

;;;###autoload
(defun projectur-find-file (choose-project)
  "Open file from current project.
If CHOOSE-PROJECT is non-nil or current buffer does not belong
to any project, ask to choose project from list and use it as
context for executing."
  (interactive "P")
  (let (project)
    (unless choose-project
      (setq project (projectur-current-project)))
    (unless project
      (setq project (projectur-select-project-from-history)))
    (projectur-with-project project
      (let ((files (projectur-project-files project))
            (root (projectur-project-root project)))
        (find-file
         (projectur-complete
          "Find file in project: " files
          (lambda (file) (file-relative-name file root))))))))

;;;###autoload
(defun projectur-set-project-root (dir)
  "Set DIR as root of current project."
  (interactive "DProject root: ")
  (projectur-history-add
   (list (file-name-as-directory (expand-file-name dir)))))

;;;###autoload
(defun projectur-delete-from-history ()
  "Select project to delete from `projectur-history'."
  (interactive)
  (let ((project (projectur-select-project-from-history "Delete project: ")))
    (setq projectur-history
          (delete project projectur-history))
    (message "Project \"%s\" deleted from history."
             (abbreviate-file-name (projectur-project-root project)))))

;;;###autoload
(defun projectur-kill-buffers ()
  "Kill all buffers (even unsaved) visiting files from current project."
  (interactive)
  (let* ((project (projectur-current-project))
         (project-name (projectur-project-name project))
         (buffers (projectur-buffers project)))
    (if buffers
        (when (yes-or-no-p
               (format "About to kill buffers for all opened files from project '%s'. Are you sure? "
                       project-name))
          (mapc 'kill-buffer buffers))
        (message
         (format "Nothing to do, there are currently no opened files from project '%s'."
                 project-name)))))

;;;###autoload
(defun projectur-rgrep ()
  "Run `rgrep' command in context of the current project root directory."
  (interactive)
  (projectur-with-current-project
    (call-interactively 'rgrep)))

;;;###autoload
(defun projectur-execute-shell-command ()
  "Execute shell command in context of the current project root directory."
  (interactive)
  (projectur-with-current-project
    (call-interactively 'shell-command)))

;;;###autoload
(defun projectur-ack ()
  "Run `ack' command (if available) in context of the current project root directory."
  (interactive)
  (projectur-with-current-project
    (if (fboundp 'ack)
        (call-interactively 'ack)
        (error "You need `ack' command installed in order to use this functionality")))  )

;;;###autoload
(defun projectur-generate-tags ()
  "Generate TAGS file for current project."
  (interactive)
  (projectur-with-current-project
    (let ((command (projectur-project-tags-command project)))
      (shell-command
       (read-string "Generate TAGS like this: "
                    command nil command))
      (setq tags-file-name (expand-file-name "TAGS")))))

;;;###autoload
(defun projectur-save ()
  "Save all opened files that belong to current project."
  (interactive)
  (mapc
   (lambda (buf)
     (with-current-buffer buf
       (save-buffer)))
   (projectur-buffers (projectur-current-project))))

;;;###autoload
(defun projectur-revert ()
  "Revert all buffers visiting files from current project."
  (interactive)
  (mapc
   (lambda (buf)
     (with-current-buffer buf
       (revert-buffer nil 'noconfirm 'preserve-modes)))
   (projectur-buffers (projectur-current-project))))

;;;###autoload
(defun projectur-goto-readme ()
  "Go to README file in current project root directory, create one if it does not exist."
  (interactive)
  (let* ((project (projectur-current-project))
         (root (projectur-project-root project))
         (readme (or (projectur-project-readme project)
                     (expand-file-name projectur-default-readme-file-name root))))
    (find-file readme)))

(defun* projectur-complete (prompt choices &optional (display-fn 'identity))
  "Select one of CHOICES, with PROMPT, use DISPLAY-FN for display if provided,
`identity' otherwise."
  (let* ((ido-decorations '("\n-> " "" "\n   " "\n   ..." "[" "]"
                            " [No match]" " [Matched]" " [Not readable]"
                            " [Too big]" " [Confirm]"))
         (ido-enable-flex-matching t)
         (results-map
          (mapcar (lambda (choice)
                    (cons (funcall display-fn choice) choice))
                  choices))
         (display-choices
          (mapcar 'car results-map))
         (chosen
          (ido-completing-read prompt display-choices)))
    (cdr (assoc chosen results-map))))

;;;###autoload
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


(defun projectur-git-repo-p (dir)
  "Return non-nil if DIR is a root of git repository, nil otherwise."
  (file-directory-p (expand-file-name ".git" dir)))

(defun projectur-mercurial-repo-p (dir)
  "Return non-nil if DIR is a root of mercurial repository, nil otherwise."
  (file-directory-p (expand-file-name ".hg" dir)))
(defalias 'projectur-hg-repo-p 'projectur-mercurial-repo-p)

(defun projectur-subversion-repo-p (dir)
  "Return non-nil if DIR is a root of subversion repository, nil otherwise."
  (and
   (file-directory-p (expand-file-name ".svn" dir))
   (not (file-directory-p (expand-file-name "../.svn" dir)))))
(defalias 'projectur-svn-repo-p 'projectur-subversion-repo-p)

(defun projectur-bazaar-repo-p (dir)
  "Return non-nil if DIR is a root of bazaar repository, nil otherwise."
  (file-directory-p (expand-file-name ".bzr" dir)))
(defalias 'projectur-bzr-repo-p 'projectur-bazaar-repo-p)

(defun projectur-cvs-repo-p (dir)
  "Return non-nil if DIR is a root of CVS repository, nil otherwise."
  (and
   (file-directory-p (expand-file-name "CVS" dir))
   (not (file-directory-p (expand-file-name "../CVS" dir)))))

(defun projectur-darcs-repo-p (dir)
  "Return non-nil if DIR is a root of Darcs repository, nil otherwise."
  (file-directory-p (expand-file-name "_darcs" dir)))

(defun projectur-ruby-gem-p (dir)
  "Return non-nil if DIR is a root of ruby gem source tree, nil otherwise."
  (file-expand-wildcards (expand-file-name "*.gemspec" dir)))

(defun projectur-rails-app-p (dir)
  "Return non-nil if DIR is a root of ruby-on-rails application, nil otherwise."
  (file-regular-p (expand-file-name "script/rails" dir)))

(defun projectur-rake-project-p (dir)
  "Return non-nil if DIR is a root of project using rake, nil otherwise."
  (loop
     for rakefile in '("rakefile" "Rakefile" "rakefile.rb" "Rakefile.rb")
     thereis (file-regular-p (expand-file-name rakefile dir))))

(defun projectur-bundler-project-p (dir)
  "Return non-nil if DIR is a root of project using bundler, nil otherwise."
  (file-regular-p (expand-file-name "Gemfile" dir)))

(defun projectur-version-controlled-repo-p (dir)
  "Return non-nil if DIR is a root of version-controlled project, nil otherwise.
Supported VCS: git, mercurial, subversion, bazaar, cvs, darcs."
  (or
   (projectur-git-repo-p dir)
   (projectur-mercurial-repo-p dir)
   (projectur-subversion-repo-p dir)
   (projectur-bazaar-repo-p dir)
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
    (file-regular-p (expand-file-name ".ruby-version" dir))
    (file-regular-p (expand-file-name ".rbenv-version" dir)))))

(defun projectur-sbt-project-p (dir)
  "Return non-nil if DIR is a root of sbt project, nil otherwise."
  (file-regular-p (expand-file-name "build.sbt" dir)))

(provide 'projectur)

;;; projectur.el ends here
