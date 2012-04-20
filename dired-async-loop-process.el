(require 'dired-async-loop-common)

(defun da-loop-cmd-loop ()
  "Main execution loop for the background Elisp interpreter."
  (defun read-char nil ?y) ;; Always answer "yes" to any prompt
  (let ((command) (signature))
    (while t
      (setq command (read))
      (setq signature (md5 (prin1-to-string command)))
      (condition-case description
          (progn
            (if da-loop-debug
                (message "%s" (concat "[[Executing in background: "
                                      (prin1-to-string command) "]]")))
            (eval command)
            (message "[[Command successfully invoked in background]]"))
        (error (message "%s" (concat "[[*ERROR IN BACKGROUND JOB: "
                                     (prin1-to-string description) "*]]"))))
      (message "done"))))

(provide 'dired-async-loop-process)

;;; dired-async-loop-process.el ends here
