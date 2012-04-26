;;; async-common.el --- Definitions common to all async files

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

(defcustom async-debug t
  "Activate debug mode.
When set, the background elisp interpreter is launched in such a
way that all background input and output are sent to a buffer
named *ASYNC-LOOP* and automatic lifecycle management is
disabled (i.e. you have to kill the interpreter manually using
async-stop to get rid of it)."
  :group 'async
  :type 'boolean)

(defconst async-message-prefix "[[ASYNC]]")

(defcustom async-output-buffer "*ASYNC-LOOP*"
  "some doc"
  :group 'async
  :type 'string)

(provide 'async-common)

;;; async-common.el ends here
