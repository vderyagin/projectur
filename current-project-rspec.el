(require 'current-project)

(define-compilation-mode rspec-mode "Rspec" "Mode for executing Rspec specs.")

(setq rspec-scroll-output t)

(add-hook 'rspec-start-hook (lambda () (goto-char (point-max))))
(add-hook 'rspec-filter-hook
          (defun rspec-colorize-buffer ()
            (let ((inhibit-read-only t))
              (ansi-color-apply-on-region (point-min) (point-max)))))

(defun cpr-rspec (arg)
  "Without prefix argument executes spec found at current point position.
With single prefix argument executes all spec found in current file.
With double prefix argument executes whole rspec suite of current project."
  (interactive "p")
  (cond
    ((eq arg 1)
     (cpr-rspec-execute-specs 'at-point))
    ((eq arg 4)
     (cpr-rspec-execute-specs 'file))
    ((eq arg 16)
     (cpr-rspec-execute-specs 'suite))))

;;;###autoload
(defun cpr-rspec-execute-specs (scope)
  "Executes rspec examples selected according to SCOPE.
if SCOPE = 'at-point - example found at current point position.
if SCOPE = 'file - examples found in currently visited file.
if SCOPE = 'suite - whole rspec suite."
  (with-cpr-project
    (let ((file (file-relative-name buffer-file-name default-directory))
          (line-number (line-number-at-pos))
          (command '("rspec")))

      (when (eq scope 'at-point)
        (add-to-list 'command "--line_number" 'append)
        (add-to-list 'command (number-to-string line-number) 'append))

      (unless (eq scope 'suite)
        (add-to-list 'command (shell-quote-argument file) 'append))

      (setq command (mapconcat 'identity command " "))

      (cpr-save)

      (compilation-start command 'rspec-mode))))

(provide 'current-project-rspec)
