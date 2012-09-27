;;; projectur-rspec.el --- Run RSpec examples in project.

;;; Commentary:

;;; Code:

(require 'projectur)

(require 'compile)

(defvar projectur-rspec-use-bundler nil
  "Non-nil means use bundler for executing rspec whenever available.")

(defun projectur-rspec-default-command ()
  "Return default command for rspec execution."
  (if (and projectur-rspec-use-bundler
           (executable-find "bundle"))
      '("bundle" "exec" "rspec")
      '("rspec")))

;;;###autoload
(defun projectur-rspec (&optional arg)
  "Without ARG execute spec found at current point position.
With single ARG executes all spec found in current file.
With double ARG executes whole rspec suite of current project."
  (interactive "p")
  (let ((scope
         (cond
           ((eq arg 1) 'at-point)
           ((eq arg 4) 'file)
           ((>= arg 16) 'suite))))
    (projectur-rspec-sanity-check scope)
    (projectur-rspec-execute-specs scope)))

(defun projectur-rspec-sanity-check (scope)
  "Check if current location is appropriate for running specs within SCOPE."
  (when (memq scope '(at-point file))
    (unless (and
             buffer-file-name
             (string-match-p "._spec\.rb" buffer-file-name))
      (error "Current buffer does not seem to be visiting RSpec spec file"))))

(defun projectur-rspec-execute-specs (scope)
  "Execute rspec examples selected according to SCOPE.
if SCOPE = 'at-point - example found at current point position.
if SCOPE = 'file - examples found in currently visited file.
if SCOPE = 'suite - whole rspec suite."
  (projectur-with-project (projectur-current-project)
    (let ((line-number (line-number-at-pos))
          (command (projectur-rspec-default-command))
          file)

      (setq command
            (append command
                    (when (eq scope 'at-point)
                      (list "--line_number" (number-to-string line-number)))
                    (unless (eq scope 'suite)
                      (setq file (file-relative-name buffer-file-name))
                      (list (shell-quote-argument file)))))

      (setq command (mapconcat 'identity command " "))

      (projectur-save)
      (compilation-start command 'rspec-mode))))


(define-compilation-mode rspec-mode "Rspec"
  "Mode for executing Rspec specs."
  (set (make-local-variable 'compilation-scroll-output) t)
  (add-hook 'compilation-filter-hook
            (lambda ()
              (let ((inhibit-read-only t))
                (ansi-color-apply-on-region (point-min) (point-max))))
            nil
            'make-it-local))

(provide 'projectur-rspec)

;;; projectur-rspec.el ends here

;; Local Variables:
;; lexical-binding: t
;; coding: us-ascii
;; End:
