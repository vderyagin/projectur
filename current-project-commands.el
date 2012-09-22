;;;###autoload
(defun cpr-find-file (&optional choose-project)
  "Select file from project and open it in current buffer"
  (interactive "P")
  (let* ((cpr-project (if choose-project
                          (cpr-choose-project-from-history)
                          cpr-project))
         (files (cpr-files))
         relative-path
         absolute-path)
    (flet ((get-relative-path (abs)
             (file-relative-name abs (cpr-project :root)))
           (get-absolute-path (rel)
             (expand-file-name rel (cpr-project :root))))
      (setq relative-path (ido-completing-read
                           "Find file in project: "
                           (mapcar 'get-relative-path files))
            absolute-path (get-absolute-path relative-path)))
    (find-file absolute-path)))

;;;###autoload
(defun cpr-goto-root (&optional choose-project)
  "Go to currrent project's root directory."
  (interactive "P")
  (let ((cpr-project (if choose-project
                         (cpr-choose-project-from-history)
                         cpr-project)))
    (with-cpr-project
      (find-file default-directory))))

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
  (if (fboundp 'ack)
      (with-cpr-project (call-interactively 'ack))
      (error "You need `ack' command installed in order to use this functionality")))

;;;###autoload
(defun cpr-smex ()
  "Run `smex' command (if available) in context of the current project root directory."
  (interactive)
  (if (fboundp 'smex)
      (with-cpr-project (call-interactively 'smex))
      (error "You need `smex' command (https://github.com/nonsequitur/smex/) installed in order to use this functionality")))

;;;###autoload
(defun cpr-execute-shell-command ()
  "Execute shell command in context of the current project root directory."
  (interactive)
  (with-cpr-project
    (call-interactively 'shell-command)))

;;;###autoload
(defun cpr-generate-tags (&optional specify-command)
  "Use exuberant-ctags(1) to generate TAGS file for current project.
Allows to specify tags generation command when called with C-u."
  (interactive "P")
  (with-cpr-project
    (let ((command "exuberant-ctags -e -R ."))
      (setq tags-file-name (expand-file-name "TAGS"))
      (when specify-command
        (setq command
              (read-string "Generate TAGS like this: "
                           command nil command)))
      (shell-command command))))

;;;###autoload
(defun cpr-version-control ()
  "Open appropriate version control interface for current project."
  (interactive)
  (with-cpr-project
    (let ((root default-directory))
      (cond
        ((and
          (cpr-git-repo-p root)
          (fboundp 'magit-status))
         (magit-status root))
        ((and
          (cpr-mercurial-repo-p root)
          (fboundp 'ahg-status))
         (ahg-status root))
        (t
         (vc-dir root nil))))))

;;;###autoload
(defun cpr-save ()
  "Saves all buffers, visiting files within current project."
  (interactive)
  (mapc
   (lambda (buf)
     (with-current-buffer buf
       (save-buffer)))
   (cpr-buffers)))

(defun cpr-open-other-project-root ()
  (interactive)
  (let ((cpr-project (cpr-choose-project-from-history)))
    (cpr-goto-root)))

(defun cpr-open-file-from-other-project ()
  (interactive)
  (let ((cpr-project (cpr-choose-project-from-history)))
    (cpr-find-file)))

(provide 'current-project-commands)
