(defcustom da-loop-debug t
  "Activate debug mode in the Sunrise Loop extension.
When set, the background elisp interpreter is launched in such a
way that all background input and output are sent to a buffer
named *SUNRISE LOOP* and automatic lifecycle management is
disabled (i.e. you have to kill the interpreter manually using
da-loop-stop to get rid of it)."
  :group 'sunrise
  :type 'boolean)

(provide 'dired-async-loop-common)

;;; dired-async-loop-common.el ends here
