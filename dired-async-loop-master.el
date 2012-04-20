(require 'dired-async-loop-common)

(defcustom da-loop-timeout 30
  "Number of seconds to wait while idle before shutting down the interpreter.
After executing one or more operations in the background, the
Sunrise Loop Elisp interpreter will be killed automatically after
this amount of time."
  :group 'sunrise)

(defvar da-loop-process nil)
(defvar da-loop-timer nil)
(defvar da-loop-scope nil)
(defvar da-loop-queue nil)

(defun da-loop-start ()
  "Launch and initiate a new background Elisp interpreter.
The new interpreter runs in batch mode and inherits all functions
from the Sunrise Commander (sunrise-commander.el) and from this
file."
  (let* ((process-connection-type nil)
        (da-path (file-name-directory (symbol-file 'da-loop-start)))
        (da-common-path (symbol-file 'da-loop-debug))
        (da-process-path (expand-file-name "dired-async-loop-process.el" da-path))
        (emacs (concat invocation-directory invocation-name)))
    (setq da-loop-process (start-process
                         "Sunrise-Loop"
                         (if da-loop-debug "*DIRED-ASYNC-LOOP*" nil)
                         emacs
                         "-batch" "-q" "-no-site-file"
                         "-l" da-common-path "-l" da-process-path
                         "-eval" "(da-loop-cmd-loop)"))
    (if da-loop-debug
        (da-loop-enqueue '(setq da-loop-debug t))
      (set-process-filter da-loop-process 'da-loop-filter))
    (setq da-loop-queue nil)))

(defun da-loop-disable-timer ()
  "Disable the automatic shutdown timer.
This is done every time we send a new task to the background
interpreter, lest it gets nuked before completing its queue."
  (if da-loop-timer
      (progn
        (cancel-timer da-loop-timer)
        (setq da-loop-timer nil))))

(defun da-loop-enable-timer ()
  "Enable the automatic shutdown timer.
This is done every time we receive confirmation from the
background interpreter that all the tasks delegated to it have
been completed. Once this function is executed, if no new tasks
are enqueued before `da-loop-timeout' seconds, the interpreter is
killed."
  (da-loop-disable-timer)
  (setq da-loop-timer (run-with-timer da-loop-timeout nil 'da-loop-stop)))

(defun da-loop-stop (&optional interrupt)
  "Shut down the background Elisp interpreter and clean up after it."
  (interactive "p")
  (da-loop-disable-timer)
  (if da-loop-queue
      (if interrupt
          (progn
            (da-loop-notify "Aborted. Some operations may remain unfinished.")
            (setq da-loop-queue nil))
        (da-loop-enable-timer)))
  (unless da-loop-queue
    (delete-process da-loop-process)
    (setq da-loop-process nil)))

(defun da-loop-notify (msg)
  "Notify the user about an event."
  (message (concat "[[" msg "]]")))

(defun da-loop-enqueue (form)
  "Delegate evaluation of FORM to the background interpreter.
If no such interpreter is currently running, launches a new one."
  (da-loop-disable-timer)
  (unless da-loop-process
    (da-loop-start))
  (let ((command (prin1-to-string form)))
    (setq da-loop-queue (append da-loop-queue (list (md5 command))))
    (process-send-string da-loop-process command)
    (process-send-string da-loop-process "\n")))

(defun da-loop-filter (process output)
  "Process filter for the background interpreter."
  (mapc (lambda (line)
          (cond ((string-match "^\\[\\[\\*\\([^\]\*]+\\)\\*\\]\\]$" line)
                 (da-loop-notify (match-string 1 line)))

                ((and (or (string-match "^\\[\\[" line)
                          (string-match "^Sunrise Loop: " line))
                      (< 0 (length line)))
                 (message "%s" line))

                ((eq ?^ (string-to-char line))
                 (let ((command (substring line 1)))
                   (when (string= command (car da-loop-queue))
                     (pop da-loop-queue)
                     (da-loop-enable-timer)
                     (unless da-loop-queue
                       (da-loop-notify "Background job finished!")))))
                (t nil)))
        (split-string output "\n")))

(provide 'dired-async-loop-master)

;;; dired-async-loop-master.el ends here
