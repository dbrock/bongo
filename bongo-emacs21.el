;;; bongo-emacs21.el --- compatibility functions for Emacs 21
;; Copyright (C) 2006  Daniel Brockman
;; Copyright (C) 1996, 1997, 1999, 2001, 2002, 2003, 2004,
;;   2005, 2006  Free Software Foundation, Inc.

;; Author: Daniel Brockman <daniel@brockman.se>
;; URL: http://www.brockman.se/software/bongo/
;; Created: December 27, 2006
;; Updated: December 27, 2006

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program (see the file `COPYING');
;; if not, write to the Free Software Foundation, 51 Franklin
;; Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file contains functions that are necessary for Bongo
;; to work with Emacs 21.  Most definitions were simply
;; copied from Emacs 22 and then backported to Emacs 21.

;;; Code:

;;; The following macros were copied from `byte-run.el'.

(defmacro bongo-define-obsolete-function-alias
  (obsolete-name current-name &optional when docstring)
  "Make OBSOLETE-NAME an obsolete function alias for CURRENT-NAME.
This just calls `defalias' and `make-obsolete'."
  (declare (doc-string 4))
  `(progn
     (defalias ,obsolete-name ,current-name ,docstring)
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


;;;; Custom

(require 'custom)

;;; The following functions were copied from `custom.el'
;;; and modified to work with Emacs 21.

(defun bongo-customize-mark-as-set (symbol)
  "Mark current value of SYMBOL as being set from customize.

If the default value of SYMBOL is different from the saved value if any,
or else if it is different from the standard value, set the
`customized-value' property to a list whose car evaluates to the
default value.  Otherwise, set it to nil.

Return non-nil iff the `customized-value' property actually changed."

;;; Not available in Emacs 21.
;;;   (custom-load-symbol symbol)

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

;;; Local Variables:
;;; coding: utf-8
;;; time-stamp-format: "%:b %:d, %:y"
;;; time-stamp-start: ";; Updated: "
;;; time-stamp-end: "$"
;;; time-stamp-line-limit: 20
;;; End:

(provide 'bongo-emacs21)
;;; bongo-emacs21.el ends here.
