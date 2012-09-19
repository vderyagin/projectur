(eval-when-compile
  (require 'cl))

(defalias 'cpr-hg-repo-p 'cpr-mercurial-repo-p)
(defalias 'cpr-svn-repo-p 'cpr-subversion-repo-p)

(defun cpr-git-repo-p (dir)
  "Returns non-nil if DIR is a root of git repository, nil otherwise."
  (file-directory-p
   (expand-file-name ".git" dir)))

(defun cpr-mercurial-repo-p (dir)
  "Returns non-nil if DIR is a root of mercurial repository, nil otherwise."
  (file-directory-p
   (expand-file-name ".hg" dir)))

(defun cpr-subversion-repo-p (dir)
  "Returns non-nil if DIR is a root of subversion repository, nil otherwise."
  (and
   (file-directory-p (expand-file-name ".svn" dir))
   (not
    (file-directory-p (expand-file-name "../.svn" dir)))))

(defun cpr-ruby-gem-p (dir)
  "Returns non-nil if DIR is a root of ruby gem source tree, nil otherwise."
  (file-expand-wildcards
   (expand-file-name "*.gemspec" dir)))

(defun cpr-rails-app-p (dir)
  "Returns non-nil if DIR is a root of ruby-on-rails application, nil otherwise."
  (file-regular-p
   (expand-file-name "script/rails" dir)))

(defun cpr-rake-project-p (dir)
  "Returns non-nil if DIR is a root of project using rake, nil otherwise."
  (loop
     for rakefile in '("rakefile" "Rakefile" "rakefile.rb" "Rakefile.rb")
     thereis (file-regular-p (expand-file-name rakefile dir))))

(defun cpr-bundler-project-p (dir)
  "Returns non-nil if DIR is a root of project using bundler, nil otherwise."
  (file-regular-p
   (expand-file-name "Gemfile" dir)))

(provide 'current-project-directory-predicates)
