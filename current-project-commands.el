(declare-function cpr--root "current-project" nil)

(defun cpr--file-relative (absolute-path)
  "Return path, relative to project root for ABSOLUTE-PATH."
  (file-relative-name absolute-path (cpr--root)))

(defun cpr--file-absolute (relative-path)
  "Return absolute path for RELATIVE-PATH inside a project."
  (expand-file-name relative-path (cpr--root)))

;;;###autoload
(defun cpr-find-file ()
  "Select file from project and open it in current buffer"
  (interactive)
  (let ((files (cpr-files))
        relative-path absolute-path)
    (setq relative-path (ido-completing-read
                         "Find file in project: "
                         (mapcar 'cpr--file-relative files))
          absolute-path (cpr--file-absolute relative-path))
    (find-file absolute-path)))

;;;###autoload
(defun cpr-goto-root ()
  "Go to currrent project's root directory."
  (interactive)
  (with-cpr-project
      (find-file default-directory)))

;;;###autoload
(defun cpr-rgrep ()
  "Run `rgrep' command in context of the current project root directory."
  (interactive)
  (with-cpr-project
      (call-interactively 'rgrep)))

;;;###autoload
(defun cpr-ack ()
  "Run `ack' command (if available) in context of the current project root directory."
  (interactive)
  (unless  (require 'ack nil 'noerror)
    (error "You need `ack' command installed in order to use this functionality"))
  (when (fboundp 'ack)
    (with-cpr-project
        (call-interactively 'ack))))

;;;###autoload
(defun cpr-smex ()
  "Run `smex' command (if available) in context of the current project root directory."
  (interactive)
  (unless (require 'smex nil 'noerror)
    (error "You need `smex' command (https://github.com/nonsequitur/smex/) installed in order to use this functionality"))
  (when (fboundp 'smex)
    (with-cpr-project
        (call-interactively 'smex))))

;;;###autoload
(defun cpr-execute-shell-command ()
  "Execute shell command in context of the current project root directory."
  (interactive)
  (with-cpr-project
      (call-interactively 'shell-command)))

;;;###autoload
(defun cpr-generate-tags ()
  "Use exuberant-ctags(1) to generate TAGS file for current project "
  (interactive)
  (with-cpr-project
      (setq tags-file-name (expand-file-name "TAGS"))
    (shell-command "exuberant-ctags -e -R .")))

;;;###autoload
(defun cpr-version-control ()
  "Open appropriate version control interface for current project."
  (interactive)
  (with-cpr-project
      (let ((root default-directory))
        (cond
          ((and
            (file-exists-p ".git")
            (fboundp 'magit-status))
           (magit-status root))
          ((and
            (file-exists-p ".hg")
            (fboundp 'ahg-status))
           (ahg-status root))
          (t
           (vc-dir root nil))))))

(provide 'current-project-commands)
