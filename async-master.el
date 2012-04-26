;;; async-master.el --- Allow for executing code in a different emacs process

;; Copyright (C) 2012 Damien Cassou
;;
;; Author: Damien Cassou <damien.cassou@gmail.com@gmail.com>
;; Version: 0.1
;; Created: 2012-04-26
;; Keywords: emacs package elisp asynchronous async


;; Async is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; Async is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Async; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;; This file is NOT part of GNU Emacs.

;; The following code originated from sunrise-x-loop.el, see CONTRIBUTORS


(require 'async-common)

(defcustom async-timeout 30
  "Number of seconds to wait while idle before shutting down the interpreter.
After executing one or more operations in the background, the
Async Elisp interpreter will be killed automatically after
this amount of time."
  :group 'async)

(defvar async-process nil)
(defvar async-timer nil)
(defvar async-scope nil)
(defvar async-queue nil)

(defun async-start ()
  "Launch and initiate a new background Elisp interpreter.
The new interpreter runs in batch mode and inherits all functions
from the async-process file."
  (let* ((process-connection-type nil)
        (async-path (file-name-directory (symbol-file 'async-start)))
        (async-common-path (symbol-file 'async-debug))
        (async-process-path (expand-file-name "async-process.el" async-path))
        (emacs (concat invocation-directory invocation-name)))
    (setq async-process (start-process
                         "Async-Loop"
                         (if async-debug async-output-buffer nil)
                         emacs
                         "-batch" "-q" "-no-site-file"
                         "-l" async-common-path "-l" async-process-path
                         "-eval" "(async-cmd-loop)"))
    (if async-debug
        (async-enqueue '(setq async-debug t))
      (set-process-filter async-process 'async-filter))
    (setq async-queue nil)))

(defun async-disable-timer ()
  "Disable the automatic shutdown timer.
This is done every time we send a new task to the background
interpreter, lest it gets nuked before completing its queue."
  (if async-timer
      (progn
        (cancel-timer async-timer)
        (setq async-timer nil))))

(defun async-enable-timer ()
  "Enable the automatic shutdown timer.
This is done every time we receive confirmation from the
background interpreter that all the tasks delegated to it have
been completed. Once this function is executed, if no new tasks
are enqueued before `async-timeout' seconds, the interpreter is
killed."
  (async-disable-timer)
  (setq async-timer (run-with-timer async-timeout nil 'async-stop)))

(defun async-stop (&optional interrupt)
  "Shut down the background Elisp interpreter and clean up after it."
  (interactive "p")
  (async-disable-timer)
  (if async-queue
      (if interrupt
          (progn
            (async-notify "Aborted. Some operations may remain unfinished.")
            (setq async-queue nil))
        (async-enable-timer)))
  (unless async-queue
    (delete-process async-process)
    (setq async-process nil)))

(defun async-notify (msg)
  "Notify the user about an event."
  (message (concat "[[" msg "]]")))

(defun async-enqueue (form)
  "Delegate evaluation of FORM to the background interpreter.
If no such interpreter is currently running, launches a new one."
  (async-disable-timer)
  (unless async-process
    (async-start))
  (let ((command (prin1-to-string form)))
    (setq async-queue (append async-queue (list (md5 command))))
    (process-send-string async-process command)
    (process-send-string async-process "\n")))

(defun async-filter (process output)
  "Process filter for the background interpreter."
  (mapc (lambda (line)
          (cond ((string-match "^\\[\\[\\*\\([^\]\*]+\\)\\*\\]\\]$" line)
                 (async-notify (match-string 1 line)))

                ((and (or (string-match "^\\[\\[" line)
                          (string-match "^Sunrise Loop: " line))
                      (< 0 (length line)))
                 (message "%s" line))

                ((eq ?^ (string-to-char line))
                 (let ((command (substring line 1)))
                   (when (string= command (car async-queue))
                     (pop async-queue)
                     (async-enable-timer)
                     (unless async-queue
                       (async-notify "Background job finished!")))))
                (t nil)))
        (split-string output "\n")))

(provide 'async-master)

;;; async-master.el ends here
