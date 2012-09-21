;;;###autoload
(defun cpr-rspec ()
  "Uses `compilation-mode' to run 'rspec' command
in context of the current project root directory"
  (interactive)
  (with-cpr-project
    (compile "rspec")))

(provide 'current-project-rspec)
