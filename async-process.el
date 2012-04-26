;;; async-process.el --- Emacs lisp interpreter for async.

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

(defun async-cmd-loop ()
  "Main execution loop for the background Elisp interpreter."
  (let ((command) (signature))
    (while t
      (setq command (read))
      (setq signature (md5 (prin1-to-string command)))
      (condition-case description
          (progn
            (if async-debug
                (message "%s" (concat "[[Executing in background: "
                                      (prin1-to-string command) "]]")))
            (eval command)
            (message "[[Command successfully invoked in background]]"))
        (error (message "%s" (concat "[[*ERROR IN BACKGROUND JOB: "
                                     (prin1-to-string description) "*]]"))))
      (message "done"))))

(provide 'async-process)

;;; async-process.el ends here
