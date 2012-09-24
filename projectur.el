;;; -*- lexical-binding: t -*-

(eval-when-compile
  (require 'cl))

(defvar projectur-project-types
  '(("Ruby on Ralis application"
     :test projectur-rails-app-p
     :ignored-dirs ("tmp"))
    ("Generic Git project"
     :test projectur-git-repo-p))
  "A list of plists describing project types.")

(defvar projectur-ignored-dirs
  '(".hg" ".git" ".bzr" ".svn" ".rbx" "_darcs" "_MTN" "CVS" "RCS" "SCCS")
  "List of names of directories, content of which will not be
  considered part of the project.")

(defvar projectur-ignored-files
  '("*.elc" "*.rbc" "*.py[co]" "*.a" "*.o" "*.so" "*.bin" "*.class"
    "*.s[ac]ssc" "*.sqlite3" "TAGS" ".gitkeep")
  "List of wildcards, matching names of files, which will not be
  considered part of the project.")

(defvar projectur-history nil
  "List of visited projects.")

(defun projectur-history-cleanup ()
  (setq projectur-history
        (loop
           for project in projectur-history
           if (projectur-project-valid-p project)
           collect project)))

(defun projectur-history-add (project)
  "Adds PROJECT to `projectur-history'."
  (add-to-list 'projectur-history project)
  (projectur-history-cleanup))

(defun projectur-project-valid-p (project)
  "Returns non-nil if PROJECT is valid, nil otherwise."
  (let ((root (car project))
        (test (plist-get (cdr project) :test)))
    (and
     (stringp root)
     (functionp test)
     (funcall test root))))

(defun projectur-current-project ()
  "Return project for the current buffer, or nil if current
buffer does not belong to any project"
  (let ((project (projectur-project-get)))
    (projectur-history-add project)
    project))

(defun projectur-project-get ()
  "Return current project or nil if current buffer does not belong to any."
    (loop
       with project
       with dir = default-directory
       until (string= dir "/")
       thereis project
       do
         (setq project (projectur-project-with-root dir)
               dir  (expand-file-name ".." dir))))

(defun projectur-project-with-root (root)
  "Return project with root in ROOT, nil if ROOT is not a root of any project."
  (loop
     for project-type in projectur-project-types
     for test-function = (plist-get (cdr project-type) :test)
     if (funcall test-function root)
     return (cons (file-name-as-directory root)
                  (cdr project-type))))

(defun projectur-choose-project-from-history ()
  "Select single project from `projectur-history'."
  (loop
     with roots =  (mapcar 'car projectur-history)
     with chosen-root = (ido-completing-read "Choose project:" roots)
     for project in projectur-history
     if (equal chosen-root (car project))
     return project))

(defun projectur-project-root (project)
  "Return root directory of PROJECT."
  (car project))

(defun projectur-project-ignored-dirs (project)
  "Return list of ignored directories for PROJECT."
  (plist-get (cdr project) :ignored-dirs))

(defun projectur-project-ignored-files (project)
  "Return list of wildcards of ignored files for PROJECT."
  (plist-get (cdr project) :ignored-files))

(defmacro projectur-with-project (project &rest body)
  "execute BODY with `default-directory' bound to PROJECT root directory."
  (declare (indent 1))
  `(let ((default-directory (projectur-project-root ,project)))
     ,@body))

(defmacro projectur-define-command (command-name docstring &rest body)
  "Define command COMMAND_NAME to be executed in"
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
     (let* ((current-project (projectur-current-project))
            (project (if (and current-project
                              (not choose-project))
                         current-project
                         (projectur-choose-project-from-history))))
       (projectur-with-project project
         ,@body))))

(defun projectur-find-cmd (project)
  "Find file in project."
  (let ((ignored-dirs (append projectur-ignored-dirs
                              (projectur-project-ignored-dirs project)))
        (ignored-files (append projectur-ignored-files
                               (projectur-project-ignored-files project))))
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

(projectur-define-command projectur-goto-root
  "Open root directory of current project."
  (find-file default-directory))

(projectur-define-command projectur-find-file
  "Open file from current project."
  (let ((files (projectur-project-files project)))
    (find-file
     (ido-completing-read
      "Find file in project:"
      files))))

(projectur-define-command projectur-rgrep
  "Run `rgrep' command in context of the current project root directory."
  (call-interactively 'rgrep))


(projectur-define-command cpr-execute-shell-command ()
  "Execute shell command in context of the current project root directory."
  (interactive)
  (call-interactively 'shell-command))


(font-lock-add-keywords
 'emacs-lisp-mode
 '(("(\\(projectur-define-command\\) +\\([^ ]+\\)"
    (1 'font-lock-keyword-face)
    (2 'font-lock-function-name-face))))




(defalias 'projectur-hg-repo-p 'projectur-mercurial-repo-p)
(defalias 'projectur-svn-repo-p 'projectur-subversion-repo-p)

(defun projectur-git-repo-p (dir)
  "Returns non-nil if DIR is a root of git repository, nil otherwise."
  (file-directory-p
   (expand-file-name ".git" dir)))

(defun projectur-mercurial-repo-p (dir)
  "Returns non-nil if DIR is a root of mercurial repository, nil otherwise."
  (file-directory-p
   (expand-file-name ".hg" dir)))

(defun projectur-subversion-repo-p (dir)
  "Returns non-nil if DIR is a root of subversion repository, nil otherwise."
  (and
   (file-directory-p (expand-file-name ".svn" dir))
   (not
    (file-directory-p (expand-file-name "../.svn" dir)))))

(defun projectur-cvs-repo-p (dir)
  "Returns non-nil if DIR is a root of CVS repository, nil otherwise."
  (and
   (file-directory-p (expand-file-name "CVS" dir))
   (not
    (file-directory-p (expand-file-name "../CVS" dir)))))

(defun projectur-darcs-repo-p (dir)
  "Returns non-nil if DIR is a root of Darcs repository, nil otherwise."
  (file-directory-p
   (expand-file-name "_darcs" dir)))

(defun projectur-ruby-gem-p (dir)
  "Returns non-nil if DIR is a root of ruby gem source tree, nil otherwise."
  (file-expand-wildcards
   (expand-file-name "*.gemspec" dir)))

(defun projectur-rails-app-p (dir)
  "Returns non-nil if DIR is a root of ruby-on-rails application, nil otherwise."
  (file-regular-p
   (expand-file-name "script/rails" dir)))

(defun projectur-rake-project-p (dir)
  "Returns non-nil if DIR is a root of project using rake, nil otherwise."
  (loop
     for rakefile in '("rakefile" "Rakefile" "rakefile.rb" "Rakefile.rb")
     thereis (file-regular-p (expand-file-name rakefile dir))))

(defun projectur-bundler-project-p (dir)
  "Returns non-nil if DIR is a root of project using bundler, nil otherwise."
  (file-regular-p
   (expand-file-name "Gemfile" dir)))
