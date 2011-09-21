(require 'pam-copyright)

; User-settable
; It is best not to directly change this file and instead override variables in your .emacs
; file with (setq variable value) commands (don't use defvar because it only sets a value
; if it's currently unboud or nil).  If you want global changes, put them in pam-site.el
; so that the changes are preserved between PAM upgrades.

(defvar pam-results-buf "*pam-results-buf*"
  "The name of the buffer where raw data from the analysis program appears.  Rename this if you
   like.")
(defvar pam-debug-buf "*pam-debug-buf*"
  "The name of the buffer where debugging information from the analysis program appears.  Rename this
   if you like.")

(defvar pam-default-analysis nil
  "Holds the default analysis to perform so that the user doesn't have to type it in all the time.
   In general, it can be either an executable name or a lisp-style list of the form
   (<executable> <arg1> ... <argn>), where all elements are strings or quoted values with properly
   escaped special characters (e.g. '.')." )

(defvar pam-default-sml-analysis nil
  "Holds the default SML analysis to perform so that the user doesn't have to type it in all the time.
   In general, it can be either an executable name or a lisp-style list of the form
   (<binary-prefix> <arg1> ... <argn>), where all elements are strings or quoted values with properly
   escaped special characters (e.g. '.').
   The first argument is assumed to be a binary file which is loaded by SML on the command line.
   For example, the SML/NJ compiler on an x86 Linux system would have the command
   'sml @SMLload=<binary file>.x86-linux <arg1> ... <argn>' (actually, the 'sml' part is
   the value of sml-program-name if it's defined, or by default 'sml-cm')" )

(defvar sml-program-name "sml-cm"
  "Used as a shortcut when running sml analyses.  The version bound by the sml-mode package will
   automatically override this if it's loaded first.")

(defvar pam-sml-system-configuration-table nil
  "A list of cons cells, each of which has a regular expression against which to match in the car field,
   and the extension to put after a file name in the cdr field.  Only insert values in this table if the
   predefined ones don't work for you.  This list is checked *before* any of the built-in
   heuristics are performed." )

(defvar pam-also-use-old-mouse-1-binding nil
  "If true, this variable tells PAM that when it overrides the actions that occur involving the first mouse
   button, the action(s) active before PAM was running should also be executed (but after those of PAM).
   The default is nil because the usual second button action is to yank in a cut or copied buffer fragment,
   which makes no sense in the same context as the normal PAM binding.")

(defvar pam-also-use-old-mouse-2-binding nil
  "If true, this variable tells PAM that when it overrides the actions that occur involving the second mouse
   button, the action(s) active before PAM was running should also be executed (but after those of PAM).
   The default is nil because the usual second button action is to yank in a cut or copied buffer fragment,
   which makes no sense in the same context as the normal PAM binding.")

(defvar pam-also-use-old-mouse-3-binding nil
  "If true, this variable tells PAM that when it overrides the actions that occur involving the third mouse
   button, the action(s) active before PAM was running should also be executed (but after those of PAM).
   The defaulat is nil because the usual third button action is to select the region between the cursor and
   the mouse, which makes no sense in the context of the normal PAM binding.")

;; Local_variables: PLEASE DO NOT CHANGE THESE!!

(defvar pam-process nil
  "Holds the process object if any for the current PAM execution")

; state variables:
(defvar pam-prev-buffer nil
  "The previous buffer to be restored after pam-reset is run")
(defvar pam-initialized nil
  "Holds whether PAM has received the start string from the child process")
(defvar newline 10
  "Single characaters are represented by their ASCII characters (hence 10 instead of '\n')" )
(defvar pam-partial-read ""
  "To hold partial input until a new line arrives" )
(defvar pam-current-analyze-file  ""
  "Holds the last file you tried to analyze with 'pam-analyze-file'" )
(defvar pam-region-select-begin -1
  "Holds the position of the left edge of the last region the user selected with the mouse" )
(defvar pam-region-select-end -1
  "Holds the position of the right edge of the last region the user selected with the mouse" )
(defvar pam-old-mouse-1-action nil
  "Holds the original function called when the first mouse button is pressed" )
(defvar pam-old-mouse-2-action nil
  "Holds the original function called when the second mouse button is pressed" )
(defvar pam-old-mouse-3-action nil
  "Holds the original function called when the third mouse button is pressed" )
(defvar pam-region-overlay nil
  "Holds the overlay of the currently highlighted area that was last clicked with the region-select hyperlink clicking
   mechanism" )
(defvar pam-buffers-not-to-kill-on-reset nil
  "Holds a list of buffers not to kill when running pam-reset; these are the ones that existed before PAM was running")

; history files:
(defvar pam-sml-analyze-file-history nil
  "The history list for pam-sml-analyze-file")
(defvar pam-full-analyze-file-history nil
  "Analagous to above")


(provide 'pam-vars)