;;; async-tests.el --- Unit-tests for async.

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

;; (add-to-list 'load-path default-directory)
(require 'async-master)
(require 'ert)
(require 'cl) ;; just for flet, is there a better way?


(defmacro async-deftest (name args &rest body)
  `(ert-deftest ,name ,args
       (let (expected)
	 (with-current-buffer (get-buffer-create async-output-buffer)
	   (erase-buffer)
	   (setq async-debug t)
	   (setq async-process nil)
	   (flet ((delegate (form)
			    (async-enqueue `(message (concat "Result = "
							     (prin1-to-string ,form)))))
		  (should-get (obj)
			      (setq expected obj)))
	     ,@body)
	   (while (null (progn
			  (goto-char (point-min))
			  (search-forward "[[Command successfully invoked in background]]"
					  nil
					  t)))
	     (message "waiting")
	     (sleep-for 0.2))
	   (goto-char (point-max))
	   (if expected
	       (search-backward (concat "Result = " (prin1-to-string expected))))
	   (async-stop)))))
	   
(async-deftest simple-command ()
  (delegate '(+ 1 1))
  (should-get 2))
