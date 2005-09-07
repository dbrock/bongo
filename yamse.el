;;; yamse.el --- Yet'nother Multimedia System for Emacs
;; Copyright (C) 2005  Daniel Brockman

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with GNU Emacs; if not, write to the Free
;; Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

(defun dbrock-nuke (regexp)
  (interactive "sRegexp matching symbols to nuke: ")
  (dolist (symbol (apropos-internal regexp))
    (setplist (makunbound (fmakunbound symbol)) nil)))

(dbrock-nuke "\\<yamse\\>")

(defgroup yamse nil
  "Yet'nother Multimedia System for Emacs"
  :prefix "yamse-"
  :group 'multimedia
  :group 'applications)

(defcustom yamse-fields '(artist album track)
  "The fields that will be used to describe tracks and headers.

This list names the possible keys of a type of alist called an infoset.
The value of a field may be some arbitrarily complex data structure,
but the name of each field must be a simple symbol.

By default, each field consists of another alist:
 * the `artist' field consists of a single mandatory `name' subfield;
 * the `album' field consists of both a mandatory `title' subfield
   and an optional `year' subfield; and finally,
 * the `track' field consists of a mandatory `title' subfield
   and an optional `index' subfield.

Currently, this list needs to be completely ordered, starting with
the most general field and ending with the most specific field.
This restriction may be relaxed in the future to either allow partially
ordered field lists, or to abandon the hard-coded ordering completely.

The meaning and content of the fields are defined implicitly by the
functions that use and operate on fields and infosets (sets of fields).
Therefore, if you change this list, you probably also need to change
 (a) either `yamse-infoset-formatting-function' or
     `yamse-field-formatting-function', and
 (b) `yamse-infoset-from-file-name-function'."
  :type '(repeat symbol)
  :group 'yamse)

(defcustom yamse-default-buffer-name "*scratch-playlist*"
  "The name of the default playlist buffer."
  :type 'string
  :group 'yamse)



(defgroup yamse-faces nil
  "Faces used by Yamse."
  :group 'yamse)

(defface yamse-artist
  '((t (:inherit font-lock-keyword-face)))
  "Face used for artist names."
  :group 'yamse-faces)

(defface yamse-album
  '((t (:inherit default)))
  "Face used for albums (year, title, and punctuation)."
  :group 'yamse-faces)

(defface yamse-album-title
  '((t (:inherit (font-lock-type-face yamse-album))))
  "Face used for album titles."
  :group 'yamse-faces)

(defface yamse-album-year
  '((t (:inherit yamse-album)))
  "Face used for album years."
  :group 'yamse-faces)

(defface yamse-track
  '((t (:inherit default)))
  "Face used for tracks (index, title, and punctuation)."
  :group 'yamse-faces)

(defface yamse-track-title
  '((t (:inherit (font-lock-function-name-face yamse-track))))
  "Face used for track titles."
  :group 'yamse-faces)

(defface yamse-track-index
  '((t (:inherit yamse-track)))
  "Face used for track indices."
  :group 'yamse-faces)



(defcustom yamse-gnu-find-program "find"
  "The name of the GNU find executable."
  :type 'string
  :group 'yamse)

(defcustom yamse-header-format "[%s]"
  "Template for displaying header lines.
%s means the header line content."
  :type 'string
  :group 'yamse)

(defcustom yamse-indentation-string "  "
  "String prefixed to lines once for each level of indentation."
  :type 'string
  :group 'yamse)

;; (defgroup yamse-info nil
;;   "Displaying track info."
;;   :prefix "yamse-"
;;   :group 'yamse)

(defcustom yamse-infoset-formatting-function 'yamse-default-format-infoset
  "Function used to convert an info set into a string."
  :type 'function
  :group 'yamse)

(defcustom yamse-field-formatting-function 'yamse-default-format-field
  "Function used to convert an info field into a string.
This is used by the function `yamse-default-format-infoset'."
  :type 'function
  :group 'yamse)

(defcustom yamse-field-separator " —— " ;; " -- "
  "String used to separate field values.
This is used by the function `yamse-default-format-field'."
  :type 'string
  :group 'yamse)

(defcustom yamse-album-format "%t (%y)"
  "Template for displaying albums in Yamse.
This is used by the function `yamse-default-format-field'.
%t means the album title.
%y means the album year."
  :type 'string
  :group 'yamse)

(defcustom yamse-track-format "%i. %t"
  "Template for displaying tracks in Yamse.
This is used by the function `yamse-default-format-field'.
%t means the track title.
%i means the track index."
  :type 'string
  :group 'yamse)

;; (defgroup yamse-file-name nil
;;   "Parsing file names."
;;   :prefix "yamse-file-name-"
;;   :group 'yamse)

(defcustom yamse-file-name-track-regexp ".*\\.mp3$"
  "Regexp matching names of playable files.
This is used by `yamse-insert-directory' to filter out non-playable files."
  :type 'regexp
  :group 'yamse)

(defcustom yamse-infoset-from-file-name-function
  'yamse-default-infoset-from-file-name
  "Function used to convert file names into infosets."
  :type 'function
  :group 'yamse)

(defcustom yamse-file-name-field-separator " - "
  "String used to split file names into fields.
This is used by the function `yamse-default-infoset-from-file-name'."
  :type 'string
  :group 'yamse)

(defcustom yamse-file-name-album-year-regexp
  "^\\([0-9]{4}\\|'?[0-9]{2}\\)$"
  "Regexp matching album years.
This is used by the function `yamse-default-infoset-from-file-name'."
  :type 'regexp
  :group 'yamse)

(defcustom yamse-file-name-track-index-regexp "^[0-9]+$"
  "Regexp matching track indices.
This is used by the function `yamse-default-infoset-from-file-name'."
  :type 'regexp
  :group 'yamse)

(defun yamse-format-header (content)
  "Decorate CONTENT so as to make it look like a header.
This function uses `yamse-header-format'."
  (format yamse-header-format content))

(defun yamse-format-infoset (infoset)
  "Represent INFOSET as a user-friendly string.
This function just calls `yamse-infoset-formatting-function'."
  (funcall yamse-infoset-formatting-function infoset))

(defun yamse-default-format-infoset (infoset)
  "Format INFOSET by calling `yamse-format-field' on each field.
Separate the obtained formatted field values by `yamse-field-separator'."
  (mapconcat 'yamse-format-field infoset yamse-field-separator))

(defun yamse-join-fields (values)
  (mapconcat 'identity values yamse-field-separator))

(defun yamse-format-field (field)
  (funcall yamse-field-formatting-function field))

(defun yamse-default-format-field (field)
  (let ((type (car field))
        (data (cdr field)))
    (cond
     ((eq type 'artist)
      (propertize (yamse-alist-get data 'name)
                  'face 'yamse-artist))
     ((eq type 'album)
      (let ((title (propertize (yamse-alist-get data 'title)
                               'face 'yamse-album-title))
            (year (propertize (yamse-alist-get data 'year)
                              'face 'yamse-album-year)))
        (if (null year) title
          (format-spec yamse-album-format
                       `((?t . ,title) (?y . ,year))))))
     ((eq type 'track)
      (let ((title (propertize (yamse-alist-get data 'title)
                               'face 'yamse-track-title))
            (index (propertize (yamse-alist-get data 'index)
                               'face 'yamse-track-index)))
        (if (null index) title
          (format-spec yamse-track-format
                       `((?t . ,title) (?i . ,index)))))))))

(defun yamse-infoset-from-file-name (file-name)
  (funcall yamse-infoset-from-file-name-function file-name))

(defun yamse-default-infoset-from-file-name (file-name)
  (let* ((base-name (file-name-sans-extension
                     (file-name-nondirectory file-name)))
         (values (split-string base-name yamse-file-name-field-separator)))
    (when (> (length values) 5)
      (let ((fifth-and-rest (nthcdr 4 values)))
        (setcar fifth-and-rest (yamse-join-fields fifth-and-rest))
        (setcdr fifth-and-rest nil)))
    (cond
     ((= 5 (length values))
      `((artist (name . ,(nth 0 values)))
        (album (year . ,(nth 1 values))
               (title . ,(nth 2 values)))
        (track (index . ,(nth 3 values))
               (title . ,(nth 4 values)))))
     ((and (= 4 (length values))
           (string-match yamse-track-index-regexp (nth 2)))
      `((artist (name . ,(nth 0 values)))
        (album (title . ,(nth 1 values)))
        (track (index . ,(nth 2 values))
               (title . ,(nth 3 values)))))
     ((and (= 4 (length values))
           (string-match yamse-album-year-regexp (nth 1)))
      `((artist (name . ,(nth 0 values)))
        (album (year  . ,(nth 1 values))
               (title . ,(nth 2 values)))
        (track (title . ,(nth 3 values)))))
     ((= 4 (length values))
      `((artist (name . ,(nth 0 values)))
        (album (title . ,(nth 1 values)))
        (track (title . ,(yamse-join-fields (nthcdr 2 values))))))
     ((= 3 (length values))
      `((artist (name . ,(nth 0 values)))
        (album (title . ,(nth 1 values)))
        (track (title . ,(nth 2 values)))))
     ((= 2 (length values))
      `((artist (name . ,(nth 0 values)))
        (track (title . ,(nth 1 values)))))
     ((= 1 (length values))
      `((track (title . ,(nth 0 values))))))))


;;;; Basic point-manipulation routines

(defun yamse-goto-point (point)
  "Set point to POINT, if POINT is non-nil.
POINT may be a number, a marker or nil."
  (when point (goto-char point)))

(defun yamse-point-at-bol (&optional point)
  "Return the first character position of the line at POINT."
  (save-excursion (yamse-goto-point point) (point-at-bol)))

(defun yamse-point-at-eol (&optional point)
  "Return the last character position of the line at POINT."
  (save-excursion (yamse-goto-point point) (point-at-eol)))

(defun yamse-first-line-p (&optional point)
  "Return non-nil if POINT is on the first line."
  (= (yamse-point-at-bol point) (point-min)))

(defun yamse-last-line-p (&optional point)
  "Return non-nil if POINT is on the last line.
An empty line at the end of the buffer doesn't count."
  (>= (1+ (yamse-point-at-eol point)) (point-max)))

(defalias 'yamse-point-before-line #'yamse-point-at-bol
  "Return the first character position of the line at POINT.")

(defun yamse-point-after-line (&optional point)
  "Return the first character position after the line at POINT.
For lines that end with newlines, the point after the line
is the same as the point before the next line."
  (let ((eol (yamse-point-at-eol point)))
    (if (= eol (point-max)) eol (1+ eol))))

(defun yamse-point-before-previous-line (&optional point)
  "Return the first point of the line before the one at POINT.
If the line at POINT is the first line, return nil."
  (unless (yamse-first-line-p point)
    (yamse-point-at-bol (1- (yamse-point-at-bol point)))))

(defun yamse-point-before-next-line (&optional point)
  "Return the first point of the line after the one at POINT.
If the line at POINT is the last line, return nil."
  (unless (yamse-last-line-p point)
    (1+ (yamse-point-at-eol point))))

(defalias 'yamse-point-at-previous-line
  #'yamse-point-before-previous-line)

(defalias 'yamse-point-at-next-line
  #'yamse-point-before-next-line)

(defun yamse-point-after-section (&optional point)
  "Return the point after the section with its header on POINT."
  (unless (yamse-line-header-p point)
    (error "Point is not on a section header"))
  (save-excursion 
    (yamse-goto-point point)
    (let ((indentation (yamse-line-indentation)))
      (forward-line)
      (while (and (> (yamse-line-indentation) indentation)
                  (not (eobp)))
        (forward-line))
      (point))))

(defun yamse-next-track-line (&optional point)
  "Return the point of the nearest track line below POINT.
An error is signaled if POINT is not on a header line."
  (unless (yamse-line-header-p point)
    (error "Will only try to find next track from header lines"))
  (save-excursion
    (yamse-goto-point point)
    (while (not (yamse-line-track-p))
      (unless (zerop (forward-line))
        (error "Dangling header line")))
    (point)))

;; (defun yamse-field-dependencies (field)
;;   (or (assoc field yamse-field-dependencies) (list field)))

;; (defun yamse-fields-with-dependencies (fields)
;;   (remove-duplicates (mapcan 'yamse-field-dependencies fields)))

;; (defun yamse-filter-info (fields info)
;;   (yamse-filter-alist (yamse-fields-with-dependencies fields) info)))

(defun yamse-track-infoset (&optional point)
  "Return the infoset for the track at POINT.
You should use `yamse-line-infoset' most of the time."
  (unless (yamse-line-track-p point)
    (error "Point is not on a track line"))
  (yamse-infoset-from-file-name (yamse-line-file-name point)))

(defun yamse-header-infoset (&optional point)
  "Return the infoset for the header at POINT.
You should use `yamse-line-infoset' most of the time."
  (unless (yamse-line-header-p point)
    (error "Point is not on a header line"))
  (yamse-filter-alist (yamse-line-fields) (yamse-track-infoset
                                           (yamse-next-track-line))))

(defun yamse-line-infoset (&optional point)
  "Return the infoset for the line at POINT.
For track lines, the infoset is obtained by passing the file name to
  `yamse-file-name-parsing-function'.
For header lines, it is derived from the `yamse-fields' text property
  and the infoset of the nearest following track line."
    (cond
     ((yamse-line-track-p point) (yamse-track-infoset point))
     ((yamse-line-header-p point) (yamse-header-infoset point))))

(defun yamse-line-internal-infoset (&optional point)
  "Return the internal infoset for the line at POINT.
The internal infoset contains values of the internal fields only."
  (yamse-filter-alist (yamse-line-internal-fields point)
                      (yamse-line-infoset point)))

(defun yamse-line-field-value (field &optional point)
  "Return the value of FIELD for the line at POINT."
  (assoc field (yamse-line-infoset point)))

(defun yamse-line-fields (&optional point)
  "Return the names of the fields defined for the line at POINT."
  (if (yamse-line-header-p point)
      (yamse-line-get-property 'yamse-fields point)
    (mapcar 'car (yamse-line-infoset point))))

(defun yamse-line-external-fields (&optional point)
  "Return the names of the fields external to the line at POINT."
  (yamse-line-get-property 'yamse-external-fields point))

(defun yamse-line-set-external-fields (fields &optional point)
  "Set FIELDS to be external to the line at POINT.
FIELDS should be a list of field names."
  (save-excursion
    (yamse-goto-point point)
    (yamse-line-set-property 'yamse-external-fields fields)
    (if (yamse-line-empty-header-p)
        (yamse-delete-line)
      (yamse-redisplay-line))))

(defun yamse-line-internal-fields (&optional point)
  "Return the names of the fields internal to the line at POINT."
  (set-difference (yamse-line-fields point)
                  (yamse-line-external-fields point)))

(defun yamse-line-indentation (&optional point)
  "Return the number of external fields of the line at POINT."
  (length (yamse-line-external-fields point)))

(defun yamse-line-indented-p (&optional point)
  (> (yamse-line-indentation point) 0))

(defun yamse-line-external-fields-proposal (&optional point)
  "Return the external fields proposal of the line at POINT.
This proposal is a list of field names that subsequent lines can
externalize if their field values match those of this line.

For track lines, this is always the same as the external field names.
For header lines, the internal field names are also added."
  (cond ((yamse-line-track-p point)
         (yamse-line-external-fields point))
        ((yamse-line-header-p point)
         (append (yamse-line-external-fields point)
                 (yamse-line-internal-fields point)))))

(defun yamse-line-indentation-proposal (&optional point)
  "Return the number of external fields proposed by the line at POINT.
See `yamse-line-external-fields-proposal'."
  (cond ((yamse-line-track-p point)
         (yamse-line-indentation point))
        ((yamse-line-header-p point)
         (+ (length (yamse-line-external-fields point))
            (length (yamse-line-internal-fields point))))))

(defun yamse-line-proposed-external-fields (&optional point)
  "Return the external fields proposed to the line at POINT.
This is nil for the first line, and equal to the external field names
proposal of the previous line for all other lines."
  (if (yamse-first-line-p point) nil
    (yamse-line-external-fields-proposal
     (yamse-point-at-previous-line point))))

(defun yamse-line-proposed-indentation (&optional point)
  "Return the number of external fields proposed to the line at POINT.
See `yamse-line-proposed-external-fields'."
  (if (yamse-first-line-p point) 0
    (yamse-line-indentation-proposal
     (yamse-point-at-previous-line point))))

;; (defun yamse-line-relatively-outdented-p ()
;;   (< (yamse-line-indentation) (yamse-line-proposed-indentation)))

(defun yamse-line-file-name (&optional point)
  "Return the `yamse-file-name' text property of the file at POINT.
This will be nil for header lines and non-nil for track lines."
  (yamse-line-get-property 'yamse-file-name point))

(defun yamse-line-track-p (&optional point)
  "Return non-nil if the line at POINT is a track line."
  (not (null (yamse-line-file-name point))))

(defun yamse-line-header-p (&optional point)
  "Return non-nil if the line at POINT is a header line."
  (yamse-line-get-property 'yamse-header-p point))

(defun yamse-line-empty-header-p (&optional point)
  "Return non-nil if the line at POINT is an empty header line.
Empty header lines have no internal fields and are not supposed ever
to exist for long enough to be visible to the user."
  (and (yamse-line-header-p point)
       (null (yamse-line-internal-fields point))))


;;;; General convenience routines

;; (defmacro nor (&rest conditions)
;;   `(not (or ,@conditions)))

(defun yamse-shortest (a b)
  "Return the shorter of the lists A and B."
  (if (<= (length a) (length b)) a b))

(defun yamse-longest (a b)
  "Return the longer of the lists A and B."
  (if (>= (length a) (length b)) a b))

(defun yamse-equally-long-p (a b)
  "Return non-nil if the lists A and B have equal length."
  (= (length a) (length b)))

(defun yamse-set-equal-p (a b)
  "Return non-nil if A and B have equal elements.
The order of the elements is not significant."
  (null (set-exclusive-or a b)))

(defun yamse-alist-get (alist key)
  "Return the cdr of the element in ALIST whose car equals KEY.
If no such element exists, return nil."
  (cdr-safe (assoc key alist)))

(defun yamse-alist-put (alist key value)
  "Set the cdr of the element in ALIST whose car equals KEY to VALUE.
If no such element exists, add a new element to the start of ALIST.
This function destructively modifies ALIST and returns the new head.
If ALIST is a symbol, operate on the vaule of that symbol instead."
  (if (and (symbolp alist) (not (null alist)))
      (set alist (yamse-alist-put (symbol-value alist) key value))
    (let ((entry (assoc key alist)))
      (if entry (prog1 alist (setcdr entry value))
        (cons (cons key value) alist)))))

(defun yamse-filter-alist (keys alist)
  "Return a new list of each pair in ALIST whose car is in KEYS."
  (remove-if-not (lambda (pair)
                   (memq (car pair) keys)) alist))

(if (and (fboundp 'process-put) (fboundp 'process-get))
    (progn
      (defalias 'yamse-process-get #'process-get)
      (defalias 'yamse-process-put #'process-put))

  (defvar yamse-process-alist nil)

  (defun yamse-process-plist (process)
    (yamse-alist-get yamse-process-alist process))

  (defun yamse-process-set-plist (process plist)
    (yamse-alist-put 'yamse-process-alist process plist))

  (defun yamse-process-get (process property)
    "Return the value of PROPERTY for PROCESS."
    (plist-get (yamse-process-plist process) property))

  (defun yamse-process-put (process property value)
    "Change the value of PROPERTY for PROCESS to VALUE."
    (yamse-set-process-plist
     process (plist-put (yamse-process-plist process)
                        property value))))

(defun yamse-delete-line (&optional point)
  "Delete the line at POINT."
  (let ((inhibit-read-only t))
    (delete-region (yamse-point-before-line point)
                   (yamse-point-after-line point))))

(defun yamse-clear-line (&optional point)
  "Remove all contents of the line at POINT."
  (let ((inhibit-read-only t))
    (save-excursion
      (yamse-goto-point point)
      ;; Avoid deleting the newline, because that would
      ;; cause the markers on this line to become mixed up
      ;; with those on the next line.
      (delete-region (point-at-bol) (point-at-eol))
      (unless (eobp)
        ;; Remove all text properties from the newline.
        (set-text-properties (point) (1+ (point)) nil)))))

(defun yamse-region-line-count (beg end)
  "Return the number of lines between BEG and END.
If BEG and END are the same, return 0.
If they are distinct but on the same line, return 1."
  (save-excursion
    (goto-char beg)
    (let ((size 0))
      (while (< (point) end)
        (setq size (1+ size))
        (forward-line))
      size)))


;;;; Text properties

(defun yamse-line-get-property (name &optional point)
  "Return the value of the text property NAME at POINT."
  (get-text-property (or point (point)) name))

(defvar yamse-line-semantic-properties
  '(yamse-file-name yamse-header-p yamse-fields yamse-external-fields)
  "The list of semantic text properties used in Yamse buffers.
When redisplaying lines, semantic text properties are preserved,
whereas all other text properties (e.g., `face') are discarded.")

(defun yamse-line-get-semantic-properties (&optional point)
  "Return the list of semantic text properties at POINT.
The value of `yamse-line-semantic-properties' determines which
text properties are considered \"semantic\" by this function."
  (let ((properties (text-properties-at (or point (point))))
        (semantic-properties nil))
    (while properties
      (when (member (car properties) yamse-line-semantic-properties)
        (setq semantic-properties
              (cons (car properties)
                    (cons (cadr properties)
                          semantic-properties))))
      (setq properties (cddr properties)))
    semantic-properties))

(defun yamse-line-set-property (name value &optional point)
  "Set the text property NAME to VALUE on the line at POINT.
The text property will be set for every character on the line at POINT,
including any terminating newline."
  (let ((inhibit-read-only t))
    (put-text-property (yamse-point-before-line point)
                       (yamse-point-after-line point)
                       name value)))

(defun yamse-line-set-properties (properties &optional point)
  "Set the text properties PROPERTIES on the line at POINT.
The text properties will be set for every character on the line at POINT,
including any terminating newline."
  (let ((inhibit-read-only t))
    (add-text-properties (yamse-point-before-line point)
                         (yamse-point-after-line point)
                         properties)))

(defun yamse-line-remove-property (name &optional point)
  "Remove the text property NAME from the line at POINT.
The text property will be removed from every character on the line at POINT,
including any terminating newline."
  (let ((inhibit-read-only t))
    (remove-text-properties (yamse-point-before-line point)
                            (yamse-point-after-line point)
                            (list name nil))))


;;;; Sectioning

(defun yamse-region-field-common-p (beg end field)
  "Return non-nil if FIELD is common between BEG and END.
FIELD should be the name of a field (i.e., a symbol).
A field is common in a region if all lines inside the region
share the same value for the field."
  (save-excursion
    (let ((last-value nil)
          (common-p t))
      (goto-char beg)
      (setq last-value (yamse-line-field-value field))
      (forward-line)
      (while (and common-p (< (point) end))
        (if (equal last-value (yamse-line-field-value field))
            (forward-line)
          (setq common-p nil)))
      common-p)))

;; XXX: This will not work properly unless the fields are
;;      strictly hierarchical.
(defun yamse-region-common-fields (beg end)
  "Return the names of all fields that are common between BEG and END.
See `yamse-region-common-field-name-p'."
  (let ((fields (reverse yamse-fields))
        (common-fields nil))
    (while fields
      (if (yamse-region-field-common-p beg end (car fields))
          (when (null common-fields)
            (setq common-fields fields))
        (setq common-fields nil))
      (setq fields (cdr fields)))
    common-fields))

;;; (defun yamse-common-fields-at-point (point)
;;;   "In Yamse, return all fields that are common at POINT.
;;; The location POINT should be an integer or marker.
;;;
;;; A field is common at POINT if it is common in the region around
;;; the object at POINT and either the previous or the next object."
;;;   (save-excursion
;;;     (goto-char point)
;;;     (yamse-longest
;;;      (unless (yamse-first-p)
;;;        (yamse-common-fields-in-region (yamse-point-before-previous)
;;;                                       (yamse-point-after-current)))
;;;      (unless (yamse-last-p)
;;;        (yamse-common-fields-in-region (yamse-point-before-current)
;;;                                       (yamse-point-after-next))))))

;; XXX: This will not work properly unless the fields are
;;      strictly hierarchical.
(defun yamse-region-fields-external-p (beg end fields)
  "Return non-nil if FIELDS are external between BEG and END.
Return nil if there is a field in FIELDS that is not external for
at least one line in the region."
  (save-excursion
    (let ((external-p t))
      (goto-char beg)
      (while (and (< (point) end) external-p)
        (when (< (yamse-line-indentation) (length fields))
          (setq external-p nil))
        (forward-line))
      external-p)))

;; (defun yamse-external-fields-in-region-equal-p (beg end)
;;   "In Yamse, return the fields that are external in the region.
;; The region delimiters BEG and END should be integers or markers.

;; Only the fields that are external for all objects throughout
;; the region are considered to be external ``in the region.''"
;;   (save-excursion
;;     (goto-char beg)
;;     (let* ((equal t)
;;            (fields (yamse-external-fields))
;;            (values (yamse-get fields)))
;;       (while (and (< (point) end) equal)
;;         (unless (equal (yamse-get fields) values)
;;           (setq equal nil))
;;         (forward-line))
;;       equal)))

;;; (defun yamse-external-fields-at-point-equal-to-previous-p (&optional point)
;;;   (if (yamse-point-at-first-line-p point)
;;;       (zerop (yamse-indentation-at-point point))
;;;     (yamse-external-fields-in-region-equal-p
;;;      (yamse-point-before-previous-line point)
;;;      (yamse-point-after-line point))))

(defun yamse-line-potential-external-fields (&optional point)
  "Return the fields of the line at POINT that could be external.
That is, return the names of the fields that are common between
the line at POINT and the line before that.
If the line at POINT is the first line, return nil."
  (unless (yamse-first-line-p point)
    (yamse-region-common-fields
     (yamse-point-before-previous-line point)
     (yamse-point-after-line point))))

(defun yamse-line-externalizable-fields (&optional point)
  "Return the externalizable fields of the line at POINT.
That is, return the names of all internal fields of the line at POINT
that could be made external without changing anything else."
  (set-difference (intersection
                   (yamse-line-proposed-external-fields point)
                   (yamse-line-potential-external-fields point))
                  (yamse-line-external-fields point)))

(defun yamse-line-redundant-header-p (&optional point)
  "Return non-nil if the line at POINT is a redundant header.
Redundant headers are headers whose internal fields are all externalizable."
  (and (yamse-line-header-p point)
       (yamse-set-equal-p (yamse-line-externalizable-fields point)
                          (yamse-line-internal-fields point))))

(defun yamse-maybe-insert-intermediate-header ()
  "Make sure that the current line has a suitable header.
If the first outer header is too specific, split it in two."
  (when (and (not (yamse-first-line-p))
             (yamse-line-indented-p))
    (let ((indentation (yamse-line-indentation))
          (external-fields (yamse-line-external-fields)))
      (save-excursion
        (forward-line -1)
        ;; For each line with a non-zero indentation, there should
        ;; be another line which proposes exactly that indentation.
        (while (/= indentation (yamse-line-indentation-proposal))
          (when (< (yamse-line-indentation) indentation)
            (yamse-insert-header external-fields)
            (yamse-externalize-fields))
          (unless (zerop (forward-line -1))
            (error "Broken playlist sectioning")))))))

(defun yamse-externalize-fields ()
  "Externalize as many fields of the current line as possible.
This function may create a new section header, but only by splitting an
existing header into two (see `yamse-maybe-insert-intermediate-header')."
  (with-yamse-buffer
    (unless (zerop (yamse-line-proposed-indentation))
      (let ((fields (yamse-line-externalizable-fields)))
        (when (> (length fields) (yamse-line-indentation))
          (yamse-line-set-external-fields fields)
          (yamse-maybe-insert-intermediate-header))))))

(defun yamse-join-region (beg end &optional fields)
  "Externalize all fields that are common between BEG and END.
This function creates a new header if necessary.
If the lines cannot be joined, an error is signaled."
  (interactive "r")
  (when (null fields)
    (setq fields (yamse-region-common-fields beg end)))
  (when (null fields)
    (error "Cannot join tracks: no common fields"))
  (when (= 0 (yamse-region-line-count beg end))
    (error "Cannot join tracks: region empty"))
  (when (yamse-region-fields-external-p beg end fields)
    (error "Cannot join tracks: already joined"))
  (when (= 1 (yamse-region-line-count beg end))
    (error "Cannot join tracks: need more than one"))
  (save-excursion
    (setq end (move-marker (make-marker) end))
    (goto-char beg)
    (beginning-of-line)
    (let ((indent (length fields)))
      (while (< (point) end)
        (when (< (yamse-line-indentation) indent)
          (yamse-line-set-external-fields fields))
        (forward-line)))
    (move-marker end nil)
    (when (yamse-line-redundant-header-p)
      (yamse-delete-line))
    (goto-char beg)
    (yamse-insert-header)))

(defun yamse-join ()
  (interactive)
  (if mark-active
      (yamse-join-region (region-beginning) (region-end))
    (error "Not implemented")))


;;;; Displaying

(defun yamse-redisplay-line ()
  "Redisplay the current line, preserving all text properties."
  (let ((inhibit-read-only t)
        (indentation (yamse-line-indentation))
        (infoset (yamse-line-internal-infoset))
        (header-p (yamse-line-header-p))
        (properties (yamse-line-get-semantic-properties)))
    (save-excursion
      (yamse-clear-line)
      (dotimes (_ indentation) (insert yamse-indentation-string))
      (let ((content (yamse-format-infoset infoset)))
        (insert (if (not header-p) content
                  (yamse-format-header content))))
      (yamse-line-set-properties properties)
;;       (yamse-line-set-property 'face (if header-p 'yamse-header
;;                                        'yamse-track))
      )))

(defun yamse-redisplay (&optional arg)
  "Redisplay every line in the entire buffer.
With prefix argument, remove all indentation and headers."
  (interactive "P")
  (save-excursion
    (with-yamse-buffer
      (goto-char (point-min))
      (while (not (eobp))
        (cond
         ((and arg (yamse-line-header-p))
          (yamse-delete-line))
         ((and arg (yamse-line-track-p))
          (yamse-line-set-external-fields nil)
          (forward-line))
         (t
          (yamse-redisplay-line)
          (forward-line)))))))


;;;; Inserting

(defun yamse-insert-line (&rest properties)
  "Insert a new line with PROPERTIES before the current line.
Externalize as many fields of the new line as possible and redisplay it.
Point is left immediately after the new line."
  (with-yamse-buffer
    (let ((inhibit-read-only t))
      (insert (apply 'propertize "\n" properties)))
    (forward-line -1)
    (yamse-externalize-fields)
    (if (yamse-line-empty-header-p)
        (yamse-delete-line)
      (yamse-redisplay-line)
      (forward-line))))

(defun yamse-insert-header (&optional fields)
  "Insert a new header line with internal FIELDS.
FIELDS defaults to the external fields of the current line."
  (yamse-insert-line 'yamse-header-p t 'yamse-fields
                     (or fields (yamse-line-external-fields))))

(defun yamse-insert-file (file-name)
  "Insert a new track line corresponding to FILE-NAME.
If FILE-NAME names a directory, call `yamse-insert-directory'."
  (interactive (list (expand-file-name
                      (read-file-name "Insert track: "
                                      default-directory nil t))))
  (if (file-directory-p file-name)
      (yamse-insert-directory file-name)
    (yamse-insert-line 'yamse-file-name file-name)))

(defun yamse-insert-directory (directory-name)
  "Insert a new track line for each file in DIRECTORY-NAME.
Only insert files whose names match `yamse-file-name-regexp'.
Do not examine subdirectories of DIRECTORY-NAME."
  (interactive (list (expand-file-name
                      (read-directory-name "Insert directory: "
                                           default-directory nil t))))
  (unless (file-directory-p directory-name)
    (dolist (file-name (directory-files directory-name t
                                        yamse-file-name-track-regexp))
      (yamse-insert-file file-name))))

(defun yamse-insert-directory-tree (directory-name)
  "Insert a new track line for each file below DIRECTORY-NAME.
Only insert files whose names match `yamse-file-name-regexp'.

This function descends each subdirectory of DIRECTORY-NAME recursively,
but actually uses `yamse-gnu-find-program' to find the files."
  (interactive (list (expand-file-name
                      (read-directory-name "Insert directory tree: "
                                           default-directory nil t))))
  (with-temp-buffer
    (call-process yamse-gnu-find-program nil t nil
                  directory-name "-regextype" "emacs"
                  "-type" "f" "-iregex" yamse-file-name-track-regexp)
    (sort-lines nil (point-min) (point-max))
    (goto-char (point-min))
    (while (not (eobp))
      (yamse-insert-file (buffer-substring (point) (point-at-eol)))
      (forward-line))))


;;;; Player types

(defvar yamse-player-types
  '((mpg123 (default-matcher . "\\.[mM][pP][23]$")
            (constructor . yamse-start-mpg123-player))))

(defvar yamse-preferred-player-types nil)

(defun yamse-file-name-matches-p (file-name matcher)
  (cond
   ((eq t matcher) t)
   ((stringp matcher) (string-match matcher file-name))
   ((symbolp matcher) (funcall matcher file-name))
   (t (error "Bad file name matcher: %s" matcher))))

(defun yamse-best-player-type-for-file (file-name)
  (let ((best-player-type nil))
    (let ((list yamse-preferred-player-types))
      (while (and list (null best-player-type))
        (let* ((player-type (yamse-alist-get yamse-player-types
                                             (cdar list)))
               (matcher (or (caar list)
                            (yamse-alist-get player-type
                                             'default-matcher))))
          (when (yamse-file-name-matches-p file-name matcher)
            (setq best-player-type player-type)))
        (setq list (cdr list))))
    (unless best-player-type
      (let ((list yamse-player-types))
        (while (and list (null best-player-type))
          (when (yamse-file-name-matches-p
                 file-name (yamse-alist-get (car list) 'default-matcher))
            (setq best-player-type (car list))))
          (setq list (cdr list))))
    best-player-type))



(defcustom yamse-next-action 'yamse-play-next
  "The function to call after the current track finishes playing."
  :type '(choice
          (const :tag "Stop playback" yamse-stop)
          (const :tag "Play the next track" yamse-play-next)
          (const :tag "Play the same track again" yamse-replay-current)
          (const :tag "Play the previous track" yamse-play-previous)
          (const :tag "Play a random track" yamse-play-random))
  :group 'yamse)

(make-variable-buffer-local 'yamse-next-action)

(defun yamse-perform-next-action ()
  (interactive)
  (when yamse-next-action
    (funcall yamse-next-action)))


;;;; Players

(defvar yamse-player nil
  "The currently active player for this buffer, or nil.")
(make-variable-buffer-local 'yamse-player)

(defvar yamse-player-started-functions nil
  "Abnormal hook run when a player is started.")
(defvar yamse-player-succeeded-functions nil
  "Abnormal hook run when a player exits normally.")
(defvar yamse-player-failed-functions nil
  "Abnormal hook run when a player exits abnormally.")
(defvar yamse-player-killed-functions nil
  "Abnormal hook run when a player recieves a fatal signal.")
(defvar yamse-player-finished-functions nil
  "Abnormal hook run when a player exits for whatever reason.")

(defun yamse-player-succeeded (player)
  "Run the hooks appropriate for when PLAYER has succeeded."
  (with-current-buffer (yamse-player-buffer player)
    (run-hook-with-args 'yamse-player-succeeded-functions player)
    (yamse-player-finished player)))

(defun yamse-player-failed (player)
  "Run the hooks appropriate for when PLAYER has failed."
  (let ((process (yamse-player-process player)))
    (message "Process `%s' exited abnormally with code %d"
             (process-name process) (process-exit-status process)))
  (with-current-buffer (yamse-player-buffer player)
    (run-hook-with-args 'yamse-player-failed-functions player)
    (yamse-player-finished player)))

(defun yamse-player-killed (player)
  "Run the hooks appropriate for when PLAYER was killed."
  (let ((process (yamse-player-process player)))
    (message "Process `%s' received fatal signal %s"
             (process-name process) (process-exit-status process)))
  (with-current-buffer (yamse-player-buffer player)
    (run-hook-with-args 'yamse-player-killed-functions player)
    (yamse-player-finished player)))

(defun yamse-player-finished (player)
  "Run the hooks appropriate for when PLAYER has finished.
Then perform the next action according to `yamse-next-action'.
You should not call this function directly."
  (with-current-buffer (yamse-player-buffer player)
    (run-hook-with-args 'yamse-player-finished-functions player)
    (yamse-perform-next-action)))

(defun yamse-play (file-name &optional player-type-name)
  "Start playing FILE-NAME and return the new player.
In Yamse mode, first stop the currently active player, if any.
This function calls `yamse-start-player'."
  (when (eq major-mode 'yamse-mode)
    (when yamse-player
      (yamse-player-stop yamse-player)))
  (let ((player (yamse-start-player file-name player-type-name)))
    (prog1 player
      (when (eq major-mode 'yamse-mode)
        (setq yamse-player player)))))

(defun yamse-start-player (file-name &optional player-type-name)
  "Start and return a new player for FILE-NAME.
If you don't specify PLAYER-TYPE-NAME, Yamse will try to find
the best player for FILE-NAME.
This function runs `yamse-player-started-functions'."
  (let* ((player-type (if player-type-name
                          (yamse-alist-get yamse-player-types
                                           player-type-name)
                        (yamse-best-player-type-for-file file-name)))
         (player (funcall (yamse-alist-get player-type 'constructor)
                          file-name)))
    (prog1 player
      (run-hook-with-args 'yamse-player-started-functions player))))

(defun yamse-player-type (player)
  "Return the player type of PLAYER (`mpg123', `mplayer', etc.)."
  (car player))

(defun yamse-player-get (player property)
  "Return the value of PLAYER's PROPERTY."
  (yamse-alist-get (cdr player) property))

(defun yamse-player-put (player property value)
  "Set PLAYER's PROPERTY to VALUE."
  (setcdr player (yamse-alist-put (cdr player) property value)))

(defun yamse-player-call (player method &rest arguments)
  "Call METHOD on PLAYER with extra ARGUMENTS."
  (apply (yamse-player-get player method) player arguments))

(defun yamse-player-process (player)
  "Return the process associated with PLAYER."
  (yamse-player-get player 'process))

(defun yamse-player-buffer (player)
  "Return the buffer associated with PLAYER."
  (yamse-player-get player 'buffer))

(defun yamse-player-running-p (player)
  "Return non-nil if PLAYER's process is currently running."
  (eq 'run (process-status (yamse-player-process player))))

(defun yamse-player-explicitly-stopped-p (player)
  "Return non-nil if PLAYER was explicitly stopped."
  (yamse-player-get player 'explicitly-stopped))

(defun yamse-player-stop (player)
  "Tell PLAYER to stop playback completely.
When this function returns, PLAYER will no longer be usable."
  (yamse-player-put player 'explicitly-stopped t)
  (yamse-player-call player 'stop))

(defun yamse-player-pause/resume (player)
  "Tell PLAYER to toggle its paused state.
If PLAYER does not support pausing, signal an error."
  (yamse-player-call player 'pause/resume))

(defun yamse-player-seek-by (player n)
  "Tell PLAYER to seek to absolute position N.
If PLAYER does not support seeking, signal an error."
  (yamse-player-call player 'seek-by n))

(defun yamse-player-seek-to (player n)
  "Tell PLAYER to seek N units relative to the current position.
If PLAYER does not support seeking, signal an error."
  (yamse-player-call player 'seek-to n))


;;;; Default implementations of player features

(defun yamse-default-player-stop (player)
  "Delete the process associated with PLAYER."
  (delete-process (yamse-player-process player)))

(defun yamse-default-player-pause/resume (player)
  "Signal an error explaining that PLAYER does not support pausing."
  (error "Pausing is not supported for %s"
         (yamse-player-type player)))

(defun yamse-default-player-seek-by (player n)
  "Signal an error explaining that PLAYER does not support seeking."
  (error "Seeking is not supported for %s"
         (yamse-player-type player)))

(defun yamse-default-player-seek-to (player n)
  "Signal an error explaining that PLAYER does not support seeking."
  (error "Seeking is not supported for %s"
         (yamse-player-type player)))

(defun yamse-default-player-process-sentinel (process string)
  "If PROCESS has exited or been killed, run the appropriate hooks."
  (let ((status (process-status process))
        (player (yamse-process-get process 'yamse-player)))
    (cond
     ((eq status 'exit)
      (if (zerop (process-exit-status process))
          (yamse-player-succeeded player)
        (yamse-player-failed player)))
     ((eq status 'signal)
      (unless (yamse-player-explicitly-stopped-p player)
        (yamse-player-killed player))))))


;;;; The mpg123 player backend

(defgroup yamse-mpg123 nil
  "The mpg123 player backend."
  :group 'yamse)

(defcustom yamse-mpg123-program-name "mpg123"
  "The name of the mpg123-compatible executable."
  :type 'string
  :group 'yamse-mpg123)

(defcustom yamse-mpg123-device-type nil
  "The type of device (oss, alsa, esd, etc.) used by mpg123.
This corresponds to the `-o' option of mpg123."
  :type '(choice (const :tag "System default" nil)
                 (const :tag "ALSA" "alsa")
                 (const :tag "OSS" "oss")
                 (const :tag "Sun" "sun")
                 (const :tag "ESD" "esd")
                 (const :tag "ARTS" "arts")
                 (string :tag "Other (specify)"))
  :group 'yamse-mpg123)

(defcustom yamse-mpg123-device nil
  "The device (e.g., for ALSA, 1:0 or 2:1) used by mpg123.
This corresponds to the `-a' option of mpg123."
  :type '(choice (const :tag "System default" nil) string)
  :group 'yamse-mpg123)

(defcustom yamse-mpg123-interactive t
  "If non-nil, use the remote-control facility of mpg123.
Setting this to nil disables the pause and seek functionality."
  :type 'boolean
  :group 'yamse-mpg123)

(defun yamse-mpg123-is-mpg321-p ()
  "Return non-nil if the mpg123 program is actually mpg321."
  (string-match "^mpg321\\b" (shell-command-to-string
                              (concat yamse-mpg123-program-name
                                      " --version"))))

(defcustom yamse-mpg123-update-granularity
  (when (yamse-mpg123-is-mpg321-p) 30)
  "The number of frames to skip between each update from mpg321.
This corresponds to the mpg321-specific option `--skip-printing-frames'.
If your mpg123 does not support that option, set this variable to nil."
  :type '(choice (const :tag "None (lowest)" nil) integer)
  :group 'yamse-mpg123)

(defcustom yamse-mpg123-seek-granularity 150
  "The minimum number of frames to skip when seeking relatively.
This is used by `yamse-mpg123-seek-by'."
  :type 'integer
  :group 'yamse-mpg123)

(defcustom yamse-mpg123-extra-arguments nil
  "Extra command-line arguments to pass to mpg123.
These will come at the end or right before the file name."
  :type '(repeat string)
  :group 'yamse-mpg123)

(defun yamse-mpg123-process-filter (process string)
  (cond
   ((string-match "^@P 0$" string)
    (yamse-player-succeeded (yamse-process-get process 'yamse-player))
    (set-process-sentinel process nil)
    (delete-process process))))

(defun yamse-mpg123-player-interactive-p (player)
  "Return non-nil if PLAYER's process is interactive.
Interactive mpg123 processes support pausing and seeking."
  (yamse-alist-get player 'interactive-flag))

(defun yamse-mpg123-player-pause (player)
  (if (yamse-mpg123-player-interactive-p player)
      (process-send-string (yamse-player-process player) "PAUSE\n")
    (error "This mpg123 process does not support pausing")))

(defun yamse-mpg123-player-seek-to (player position)
  (if (yamse-mpg123-player-interactive-p player)
      (process-send-string (yamse-player-process player)
                           (format "JUMP %d\n" position))
    (error "This mpg123 process does not support seeking")))

(defun yamse-mpg123-player-seek-by (player delta)
  (if (yamse-mpg123-player-interactive-p player)
      (process-send-string
       (yamse-player-process player)
       (format "JUMP %s%d\n" (if (< delta 0) "-" "+")
               (* yamse-mpg123-seek-granularity (abs delta))))
    (error "This mpg123 process does not support seeking")))

(defun yamse-start-mpg123-player (file-name)
  (let* ((process-connection-type nil) ; Use a pipe, not a PTY.
         (arguments (append
                     (when yamse-mpg123-device-type
                       (list "-o" yamse-mpg123-device-type))
                     (when yamse-mpg123-device
                       (list "-a" yamse-mpg123-device))
                     (when yamse-mpg123-update-granularity
                       (list "--skip-printing-frames"
                             (number-to-string
                              yamse-mpg123-update-granularity)))
                     yamse-mpg123-extra-arguments
                     (if yamse-mpg123-interactive
                         '("-R" "dummy") (list file-name))))
         (process (apply 'start-process "yamse-mpg123" nil
                         yamse-mpg123-program-name arguments))
         (player `(mpg123
                   (process . ,process)
                   (buffer . , (when (eq major-mode 'yamse-mode)
                                 (current-buffer)))
                   (stop . yamse-default-player-stop)
                   (interactive-flag . ,yamse-mpg123-interactive)
                   (pause/resume . yamse-mpg123-player-pause/resume)
                   (seek-by . yamse-mpg123-player-seek-by)
                   (seek-to . yamse-mpg123-player-seek-to))))
    (prog1 player
      (set-process-sentinel process 'yamse-default-player-process-sentinel)
      (yamse-process-put process 'yamse-player player)
      (if (not yamse-mpg123-interactive)
          (set-process-filter process 'ignore)
        (set-process-filter process 'yamse-mpg123-process-filter)
        (process-send-string process (format "LOAD %s\n" file-name))))))


;;;; Controlling playback

(defun yamse-active-track-position ()
  "Return the character position of the active track, or nil."
  (marker-position overlay-arrow-position))

(defun yamse-set-active-track-position (&optional point)
  "Make the track on the line at POINT be the active track."
  (move-marker overlay-arrow-position (yamse-point-before-line point)))

(defun yamse-unset-active-track-position ()
  "Make it so that no track is active in this buffer."
  (move-marker overlay-arrow-position nil))

(defun yamse-line-active-track-p (&optional point)
  "Return non-nil if the line at POINT is the active track."
  (when (yamse-active-track-position)
    (and (>= (yamse-active-track-position)
             (yamse-point-before-line point))
         (< (yamse-active-track-position)
            (yamse-point-after-line point)))))

;;; (defun yamse-playing-p ()
;;;   "Return non-nil if there is an active player for this buffer."
;;;   (not (null yamse-player)))

(defun yamse-line-play (&optional point)
  "Start playing the track on the line at POINT."
  (interactive)
  (if (not (yamse-line-track-p point))
      (error "No track at point")
    (yamse-set-active-track-position point)
    (let ((player (yamse-play (yamse-line-file-name point))))
      (yamse-line-set-property 'yamse-player player point))))

(defun yamse-replay-current ()
  (interactive)
  (when (null (yamse-active-track-position))
    (error "No active track"))
  (yamse-line-play (yamse-active-track-position)))

(defun yamse-play-next ()
  (interactive)
  (when (null (yamse-active-track-position))
    (error "No active track"))
  (when (yamse-last-line-p (yamse-active-track-position))
    (error "Already at the last track"))
  (yamse-line-play (yamse-point-at-next-line
                    (yamse-active-track-position))))

(defun yamse-play-random ()
  (interactive)
  (unless (yamse-tracks-exist-p)
    (error "No tracks"))
  (save-excursion
    (goto-char (1+ (random (point-max))))
    (while (not (yamse-line-track-p))
      (goto-char (1+ (random (point-max))))
    (yamse-line-play))))

(defun yamse-stop ()
  (interactive)
  (when yamse-player
    (yamse-player-stop yamse-player))
  (yamse-unset-active-track-position))

(defun yamse-pause/resume ()
  (interactive)
  (if yamse-player
      (yamse-player-pause/resume yamse-player)
    (error "No active player")))

(defun yamse-seek-forward (&optional n)
  (interactive "p")
  (if yamse-player
      (yamse-player-seek-by yamse-player n)
    (error "No active player")))

(defun yamse-seek-backward (&optional n)
  (interactive "p")
  (if yamse-player
      (yamse-player-seek-by yamse-player (- n))
    (error "No active player")))



(defun yamse-kill-line (&optional arg)
  "In Yamse, kill the current line.
With prefix argument, kill that many lines from point.
See `kill-line'."
  (interactive "P")
  (let ((inhibit-read-only t))
    (cond
     ((yamse-line-track-p)
      (when (yamse-line-active-track-p)
        (yamse-unset-active-track-position))
      (let ((kill-whole-line t))
        (beginning-of-line)
        (kill-line arg)))
     ((yamse-line-header-p)
      (kill-region (yamse-point-before-line)
                   (yamse-point-after-section)))
     (t
      (kill-line arg)))
;;     (when (yamse-redundant-header-at-point-p)
;;       (yamse-delete-line))
    ))

(defun yamse-kill-region (&optional beg end)
  "In Yamse, kill the lines between point and mark.
See `kill-region'."
  (interactive "r")
  (setq end (move-marker (make-marker) end))
  (save-excursion
    (goto-char beg)
    (yamse-kill-line)
    (while (< (point) end)
      (append-next-kill)
      (yamse-kill-line)))
  (move-marker end nil))

(defun yamse-yank (&optional arg)
  "In Yamse, reinsert the last sequence of killed lines.
See `yank'."
  (interactive "P")
  (let ((inhibit-read-only t))
    (beginning-of-line)
    (yank arg)
    (let ((beg (region-beginning))
          (end (move-marker (make-marker) (region-end))))
      (save-excursion
        (goto-char beg)
        (while (and (< (point) end))
          (let ((player (yamse-line-get-property 'yamse-player)))
            (when player
              (if (and (eq player yamse-player)
                       (null (yamse-active-track-position)))
                  (yamse-set-active-track-position (point-at-bol))
                (yamse-line-remove-property 'yamse-player))))
          (forward-line))
        ;; These headers will stay if they are needed, or disappear
        ;; automatically otherwise.
        (goto-char end)
        (move-marker end nil)
        (yamse-insert-header)
        (goto-char beg)
        (yamse-insert-header)
        ;; In case the upper header does disappear, we need to merge
        ;; backwards to connect.
        (yamse-externalize-fields)))))

(defun yamse-yank-pop (&optional arg)
  "In Yamse, replace the just-yanked lines with different ones.
See `yank-pop'."
  (interactive "P")
  (let ((inhibit-read-only t))
    (yank-pop arg)
    (yamse-externalize-fields)))

(defun yamse-undo (&optional arg)
  "In Yamse, undo some previous changes.
See `undo'."
  (interactive "P")
  (let ((inhibit-read-only t))
    (undo arg)))

(defun yamse-quit ()
  "Quit Yamse by selecting some other buffer."
  (interactive)
  (bury-buffer))

(defun yamse-mode ()
  "Major mode for Yamse playlist buffers."
  (interactive)
  (kill-all-local-variables)
  (use-local-map yamse-mode-map)
  (setq buffer-read-only t)
  (setq major-mode 'yamse-mode)
  (setq mode-name "Playlist")
  (setq font-lock-defaults '())
  (set (make-local-variable 'overlay-arrow-position) (make-marker))
;;  (set (make-local-variable 'yank-excluded-properties) nil)
  (run-mode-hooks 'yamse-mode-hook))

(defvar yamse-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map "\C-m" 'yamse-line-play)
    (define-key map "q" 'yamse-quit)
    (define-key map "g" 'yamse-redisplay)
    (define-key map "j" 'yamse-join)
    (define-key map "k" 'yamse-kill-line)
    (substitute-key-definition
     'kill-line 'yamse-kill-line map global-map)
    (define-key map "w" 'yamse-kill-region)
    (substitute-key-definition
     'kill-region 'yamse-kill-region map global-map)
    (define-key map "y" 'yamse-yank)
    (substitute-key-definition
     'yank 'yamse-yank map global-map)
    (substitute-key-definition
     'yank-pop 'yamse-yank-pop map global-map)
    (substitute-key-definition
     'undo 'yamse-undo map global-map)
    (define-key map " " 'yamse-pause/resume)
    (define-key map "s" 'yamse-stop)
    (define-key map "f" 'yamse-seek-forward)
    (define-key map "b" 'yamse-seek-backward)
    (define-key map "if" 'yamse-insert-file)
    (define-key map "id" 'yamse-insert-directory)
    (define-key map "it" 'yamse-insert-directory-tree)
    map))

(defmacro with-yamse-buffer (&rest body)
  "Execute the forms in BODY in some Yamse buffer.
If the current buffer is a Yamse buffer, don't switch buffers.
Otherwise, switch to the default Yamse buffer.
If no Yamse buffer exists at all, create one."
  (declare (indent 0) (debug t))
  `(save-current-buffer
     (unless (eq major-mode 'yamse-mode)
       (switch-to-buffer (yamse-default-buffer)))
     ,@body))

(defun yamse-default-buffer ()
  "Return the default Yamse playlist buffer.
If no playlist buffer exists, create one."
  (or (get-buffer yamse-default-buffer-name)
      (save-excursion
        (set-buffer (get-buffer-create yamse-default-buffer-name))
        (yamse-mode)
        (current-buffer))))

(defun yamse ()
  "Start the Yamse multimedia system."
  (interactive)
  (switch-to-buffer (yamse-default-buffer)))

(provide 'yamse)
