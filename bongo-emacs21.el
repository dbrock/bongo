;;; bongo-emacs21.el --- compatibility functions for Emacs 21
;; Copyright (C) 2006, 2007  Daniel Brockman
;; Copyright (C) 1985, 1986, 1992, 1994, 1995, 1996, 1997, 1999, 2000,
;;   2001, 2002, 2003, 2004, 2005, 2006  Free Software Foundation, Inc.

;; Author: Daniel Brockman <daniel@brockman.se>
;; Created: December 27, 2006

;; This file is part of Bongo.

;; Bongo is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; Bongo is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with Bongo (see the file `COPYING');
;; if not, write to the Free Software Foundation, 51 Franklin
;; Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file contains functions that are necessary for Bongo
;; to work with Emacs 21.  Most definitions were simply
;; copied from Emacs 22 and then backported to Emacs 21.

;;; Code:

(eval-when-compile
  (require 'cl))

(defun bongo-face-foreground (face &optional frame inherit)
  "Call `face-foreground' with FACE and FRAME.
INHERIT is ignored, since it is not supported by Emacs 21."
  (face-foreground face frame))

(defun bongo-face-background (face &optional frame inherit)
  "Call `face-background' with FACE and FRAME.
INHERIT is ignored, since it is not supported by Emacs 21."
  (face-background face frame))

(defalias 'bongo-read-directory-name
  'read-file-name)

;;; The following function was copied from `subr.el'
;;; and modified to work with Emacs 21.

(defun bongo-run-mode-hooks (&rest hooks)
  "Run HOOKS and then `after-change-major-mode-hook'.
Major mode functions should use this."
  (apply 'run-hooks hooks)
  (run-hooks 'after-change-major-mode-hook))

;;; The following function was copied from `subr.el'.

(defun bongo-read-number (prompt &optional default)
  (let ((n nil))
    (when default
      (setq prompt
	    (if (string-match "\\(\\):[ \t]*\\'" prompt)
		(replace-match (format " (default %s)" default) t t prompt 1)
	      (replace-regexp-in-string "[ \t]*\\'"
					(format " (default %s) " default)
					prompt t t))))
    (while
	(progn
	  (let ((str (read-from-minibuffer prompt nil nil nil nil
					   (and default
						(number-to-string default)))))
	    (setq n (cond
		     ((zerop (length str)) default)
		     ((stringp str) (read str)))))
	  (unless (numberp n)
	    (message "Please enter a number.")
	    (sit-for 1)
	    t)))
    n))

;;; The following were copied from `image.el.'

(defconst bongo-image-type-file-name-regexps
  '(("\\.png\\'" . png)
    ("\\.gif\\'" . gif)
    ("\\.jpe?g\\'" . jpeg)
    ("\\.bmp\\'" . bmp)
    ("\\.xpm\\'" . xpm)
    ("\\.pbm\\'" . pbm)
    ("\\.xbm\\'" . xbm)
    ("\\.ps\\'" . postscript)
    ("\\.tiff?\\'" . tiff))
  "Alist of (REGEXP . IMAGE-TYPE) pairs used to identify image files.
When the name of an image file match REGEXP, it is assumed to
be of image type IMAGE-TYPE.")

(defun bongo-image-type-from-file-name (file)
  "Determine the type of image file FILE from its name.
Value is a symbol specifying the image type, or nil if type cannot
be determined."
  (let ((types bongo-image-type-file-name-regexps)
	type)
    (while types
      (if (string-match (car (car types)) file)
	  (setq type (cdr (car types))
		types nil)
	(setq types (cdr types))))
    type))

;;; The following macros were copied from `byte-run.el'.

(defmacro bongo-define-obsolete-function-alias
  (obsolete-name current-name &optional when docstring)
  "Make OBSOLETE-NAME an obsolete function alias for CURRENT-NAME.
This just calls `defalias' and `make-obsolete'.
DOCSTRING is ignored, because `defalias' does not accept the
corresponding argument in Emacs 21."
  (declare (doc-string 4))
  `(progn
     (defalias ,obsolete-name ,current-name)
     (make-obsolete ,obsolete-name ,current-name ,when)))

(defmacro bongo-define-obsolete-variable-alias
  (obsolete-name current-name &optional when docstring)
  "Make OBSOLETE-NAME an obsolete variable.
This just calls `make-obsolete-variable'.  It doesn't actually create a
variable alias, because there is no such thing in Emacs 21."
  (declare (doc-string 4))
  `(progn

;;; Not available in Emacs 21.
;;;      (defvaralias ,obsolete-name ,current-name ,docstring)

     (make-obsolete-variable ,obsolete-name ,current-name ,when)))


;;;; Fallback implementations of `process-{get,put}'.

(defvar bongo-process-alist nil)

(defun bongo-process-plist (process)
  (bongo-alist-get bongo-process-alist process))

(defun bongo-process-set-plist (process plist)
  (bongo-alist-put 'bongo-process-alist process plist))

(defun bongo-process-get (process property)
  "Return the value of PROPERTY for PROCESS."
  (plist-get (bongo-process-plist process) property))

(defun bongo-process-put (process property value)
  "Change the value of PROPERTY for PROCESS to VALUE."
  (bongo-process-set-plist
   process (plist-put (bongo-process-plist process)
                      property value)))


;;;; Custom

(require 'custom)

;;; The following functions were copied from `custom.el'
;;; and modified to work with Emacs 21.

(defun bongo-custom-set-minor-mode (variable value)
  ":set function for minor mode variables.
Normally, this sets the default value of VARIABLE to nil if VALUE
is nil and to t otherwise, but if `custom-local-buffer' is non-nil,
this sets the local binding in that buffer instead."
  (if custom-local-buffer
      (with-current-buffer custom-local-buffer
	(funcall variable (if value 1 0)))
    (funcall variable (if value 1 0))))

(defun bongo-customize-mark-as-set (symbol)
  "Mark current value of SYMBOL as being set from customize.

If the default value of SYMBOL is different from the saved value if any,
or else if it is different from the standard value, set the
`customized-value' property to a list whose car evaluates to the
default value.  Otherwise, set it to nil.

Return non-nil iff the `customized-value' property actually changed."

;;; Not available in Emacs 21.
;;;   (custom-load-symbol symbol)

  (require 'cus-edit)
  (let* ((get (or (get symbol 'custom-get) 'default-value))
         (value (funcall get symbol))
         (customized (get symbol 'customized-value))
         (old (or (get symbol 'saved-value) (get symbol 'standard-value))))
    ;; Mark default value as set iff different from old value.
    (if (not (and old
                  (equal value (condition-case nil
                                   (eval (car old))
                                 (error nil)))))
        (progn (put symbol 'customized-value (list (custom-quote value)))

;;; Not available in Emacs 21.
;;;                (custom-push-theme 'theme-value symbol 'user 'set
;;;                                   (custom-quote value))

               )
      (put symbol 'customized-value nil))
    ;; Changed?
    (not (equal customized (get symbol 'customized-value)))))

(defun bongo-custom-reevaluate-setting (symbol)
  "Reset the value of SYMBOL by re-evaluating its saved or standard value.
Use the :set function to do so.  This is useful for customizable options
that are defined before their standard value can really be computed."
  (funcall (or (get symbol 'custom-set) 'set-default)
           symbol
           (eval (car (or (get symbol 'saved-value)
                          (get symbol 'standard-value))))))


;;;; Define global minor mode

;;; The following function was copied from `easy-mmode.el'.

(defmacro bongo-define-global-minor-mode
  (global-mode mode turn-on &rest keys)
  "Make GLOBAL-MODE out of the buffer-local minor MODE.
TURN-ON is a function that will be called with no args in every buffer
  and that should try to turn MODE on if applicable for that buffer.
KEYS is a list of CL-style keyword arguments.  As the minor mode
  defined by this function is always global, any :global keyword is
  ignored.  Other keywords have the same meaning as in `define-minor-mode',
  which see.  In particular, :group specifies the custom group.
  The most useful keywords are those that are passed on to the
  `defcustom'.  It normally makes no sense to pass the :lighter
  or :keymap keywords to `define-global-minor-mode', since these
  are usually passed to the buffer-local version of the minor mode.

If MODE's set-up depends on the major mode in effect when it was
enabled, then disabling and reenabling MODE should make MODE work
correctly with the current major mode.  This is important to
prevent problems with derived modes, that is, major modes that
call another major mode in their body."

  (let* ((global-mode-name (symbol-name global-mode))
	 (pretty-name (easy-mmode-pretty-mode-name mode))
	 (pretty-global-name (easy-mmode-pretty-mode-name global-mode))
	 (group nil)
	 (extra-keywords nil)
	 (MODE-buffers (intern (concat global-mode-name "-buffers")))
	 (MODE-enable-in-buffers
	  (intern (concat global-mode-name "-enable-in-buffers")))
	 (MODE-check-buffers
	  (intern (concat global-mode-name "-check-buffers")))
	 (MODE-cmhh (intern (concat global-mode-name "-cmhh")))
	 (MODE-major-mode (intern (concat (symbol-name mode) "-major-mode")))
	 keyw)

    ;; Check keys.
    (while (keywordp (setq keyw (car keys)))
      (setq keys (cdr keys))
      (case keyw
	(:group (setq group (nconc group (list :group (pop keys)))))
	(:global (setq keys (cdr keys)))
	(t (push keyw extra-keywords) (push (pop keys) extra-keywords))))

    (unless group
      ;; We might as well provide a best-guess default group.
      (setq group
	    `(:group ',(intern (replace-regexp-in-string
				"-mode\\'" "" (symbol-name mode))))))

    `(progn
       (defvar ,MODE-major-mode nil)
       (make-variable-buffer-local ',MODE-major-mode)
       ;; The actual global minor-mode
       (define-minor-mode ,global-mode
	 ,(format "Toggle %s in every buffer.
With prefix ARG, turn %s on if and only if ARG is positive.
%s is actually not turned on in every buffer but only in those
in which `%s' turns it on."
		  pretty-name pretty-global-name pretty-name turn-on)
	 :global t ,@group ,@(nreverse extra-keywords)

	 ;; Setup hook to handle future mode changes and new buffers.
	 (if ,global-mode
	     (progn
	       (add-hook 'after-change-major-mode-hook
			 ',MODE-enable-in-buffers)
	       (add-hook 'find-file-hook ',MODE-check-buffers)
	       (add-hook 'change-major-mode-hook ',MODE-cmhh))
	   (remove-hook 'after-change-major-mode-hook ',MODE-enable-in-buffers)
	   (remove-hook 'find-file-hook ',MODE-check-buffers)
	   (remove-hook 'change-major-mode-hook ',MODE-cmhh))

	 ;; Go through existing buffers.
	 (dolist (buf (buffer-list))
	   (with-current-buffer buf
	     (if ,global-mode (,turn-on) (when ,mode (,mode -1))))))

       ;; Autoloading define-global-minor-mode autoloads everything
       ;; up-to-here.
       :autoload-end

       ;; List of buffers left to process.
       (defvar ,MODE-buffers nil)

       ;; The function that calls TURN-ON in each buffer.
       (defun ,MODE-enable-in-buffers ()
	 (dolist (buf ,MODE-buffers)
	   (when (buffer-live-p buf)
	     (with-current-buffer buf
	       (if ,mode
		   (unless (eq ,MODE-major-mode major-mode)
		     (,mode -1)
		     (,turn-on)
		     (setq ,MODE-major-mode major-mode))
		 (,turn-on)
		 (setq ,MODE-major-mode major-mode))))))
       (put ',MODE-enable-in-buffers 'definition-name ',global-mode)

       (defun ,MODE-check-buffers ()
	 (,MODE-enable-in-buffers)
	 (setq ,MODE-buffers nil)
	 (remove-hook 'post-command-hook ',MODE-check-buffers))
       (put ',MODE-check-buffers 'definition-name ',global-mode)

       ;; The function that catches kill-all-local-variables.
       (defun ,MODE-cmhh ()
	 (add-to-list ',MODE-buffers (current-buffer))
	 (add-hook 'post-command-hook ',MODE-check-buffers))
       (put ',MODE-cmhh 'definition-name ',global-mode))))

;;; Local Variables:
;;; coding: utf-8
;;; End:

(provide 'bongo-emacs21)
;;; bongo-emacs21.el ends here.
