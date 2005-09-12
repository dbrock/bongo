;;; bongo.el --- a buffer-oriented media player for Emacs
;; Copyright (C) 2005  Daniel Brockman

;; Author: Daniel Brockman <daniel@brockman.se>
;; URL: http://www.brockman.se/software/bongo/
;; Created: The shiny afternoon of September 3, 2005

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

;;; Code:

(defgroup bongo nil
  "Buffer-oriented media player"
  :prefix "bongo-"
  :group 'multimedia
  :group 'applications)

(defcustom bongo-fields '(artist album track)
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
 (a) either `bongo-infoset-formatting-function' or
     `bongo-field-formatting-function', and
 (b) `bongo-infoset-from-file-name-function'."
  :type '(repeat symbol)
  :group 'bongo)

(defcustom bongo-default-buffer-name "*scratch-playlist*"
  "The name of the default playlist buffer."
  :type 'string
  :group 'bongo)



(defgroup bongo-faces nil
  "Faces used by Bongo."
  :group 'bongo)

(defface bongo-artist
  '((t (:inherit font-lock-keyword-face)))
  "Face used for artist names."
  :group 'bongo-faces)

(defface bongo-album
  '((t (:inherit default)))
  "Face used for albums (year, title, and punctuation)."
  :group 'bongo-faces)

(defface bongo-album-title
  '((t (:inherit (font-lock-type-face bongo-album))))
  "Face used for album titles."
  :group 'bongo-faces)

(defface bongo-album-year
  '((t (:inherit bongo-album)))
  "Face used for album years."
  :group 'bongo-faces)

(defface bongo-track
  '((t (:inherit default)))
  "Face used for tracks (index, title, and punctuation)."
  :group 'bongo-faces)

(defface bongo-track-title
  '((t (:inherit (font-lock-function-name-face bongo-track))))
  "Face used for track titles."
  :group 'bongo-faces)

(defface bongo-track-index
  '((t (:inherit bongo-track)))
  "Face used for track indices."
  :group 'bongo-faces)



(defcustom bongo-gnu-find-program "find"
  "The name of the GNU find executable."
  :type 'string
  :group 'bongo)

(defcustom bongo-gnu-find-extra-arguments '("-regextype" "emacs")
  "Extra arguments to pass to GNU find."
  :type '(repeat string)
  :group 'bongo)

(defcustom bongo-header-format "[%s]"
  "Template for displaying header lines.
%s means the header line content."
  :type 'string
  :group 'bongo)

(defcustom bongo-indentation-string "  "
  "String prefixed to lines once for each level of indentation."
  :type 'string
  :group 'bongo)

;; (defgroup bongo-info nil
;;   "Displaying track info."
;;   :prefix "bongo-"
;;   :group 'bongo)

(defcustom bongo-infoset-formatting-function 'bongo-default-format-infoset
  "Function used to convert an info set into a string."
  :type 'function
  :group 'bongo)

(defcustom bongo-field-formatting-function 'bongo-default-format-field
  "Function used to convert an info field into a string.
This is used by the function `bongo-default-format-infoset'."
  :type 'function
  :group 'bongo)

(defcustom bongo-field-separator " —— " ;; " -- "
  "String used to separate field values.
This is used by the function `bongo-default-format-field'."
  :type 'string
  :group 'bongo)

(defcustom bongo-album-format "%t (%y)"
  "Template for displaying albums in Bongo.
This is used by the function `bongo-default-format-field'.
%t means the album title.
%y means the album year."
  :type 'string
  :group 'bongo)

(defcustom bongo-track-format "%i. %t"
  "Template for displaying tracks in Bongo.
This is used by the function `bongo-default-format-field'.
%t means the track title.
%i means the track index."
  :type 'string
  :group 'bongo)

;; (defgroup bongo-file-name nil
;;   "Parsing file names."
;;   :prefix "bongo-file-name-"
;;   :group 'bongo)

(defcustom bongo-file-name-track-regexp ".*\\.mp3$"
  "Regexp matching names of playable files.
This is used by `bongo-insert-directory' to filter out non-playable files."
  :type 'regexp
  :group 'bongo)

(defcustom bongo-infoset-from-file-name-function
  'bongo-default-infoset-from-file-name
  "Function used to convert file names into infosets."
  :type 'function
  :group 'bongo)

(defcustom bongo-file-name-field-separator " - "
  "String used to split file names into fields.
This is used by the function `bongo-default-infoset-from-file-name'."
  :type 'string
  :group 'bongo)

(defcustom bongo-file-name-album-year-regexp
  "^\\([0-9]{4}\\|'?[0-9]{2}\\)$"
  "Regexp matching album years.
This is used by the function `bongo-default-infoset-from-file-name'."
  :type 'regexp
  :group 'bongo)

(defcustom bongo-file-name-track-index-regexp "^[0-9]+$"
  "Regexp matching track indices.
This is used by the function `bongo-default-infoset-from-file-name'."
  :type 'regexp
  :group 'bongo)

(defun bongo-format-header (content)
  "Decorate CONTENT so as to make it look like a header.
This function uses `bongo-header-format'."
  (format bongo-header-format content))

(defun bongo-format-infoset (infoset)
  "Represent INFOSET as a user-friendly string.
This function just calls `bongo-infoset-formatting-function'."
  (funcall bongo-infoset-formatting-function infoset))

(defun bongo-default-format-infoset (infoset)
  "Format INFOSET by calling `bongo-format-field' on each field.
Separate the obtained formatted field values by `bongo-field-separator'."
  (mapconcat 'bongo-format-field infoset bongo-field-separator))

(defun bongo-join-fields (values)
  (mapconcat 'identity values bongo-field-separator))

(defun bongo-format-field (field)
  (funcall bongo-field-formatting-function field))

(defun bongo-default-format-field (field)
  (let ((type (car field))
        (data (cdr field)))
    (cond
     ((eq type 'artist)
      (propertize (bongo-alist-get data 'name) 'face 'bongo-artist))
     ((eq type 'album)
      (let ((title (bongo-alist-get data 'title))
            (year (bongo-alist-get data 'year)))
        (if (null year) (propertize title 'face 'bongo-album-title)
          (format-spec bongo-album-format
                       `((?t . ,(propertize
                                 title 'face 'bongo-album-title))
                         (?y . ,(propertize
                                 year 'face 'bongo-album-year)))))))
     ((eq type 'track)
      (let ((title (bongo-alist-get data 'title))
            (index (bongo-alist-get data 'index)))
        (if (null index) (propertize title 'face 'bongo-track-title)
          (format-spec bongo-track-format
                       `((?t . ,(propertize
                                 title 'face 'bongo-track-title))
                         (?i . ,(propertize
                                 index 'face 'bongo-track-index))))))))))

(defun bongo-infoset-from-file-name (file-name)
  (funcall bongo-infoset-from-file-name-function file-name))

(defun bongo-default-infoset-from-file-name (file-name)
  (let* ((base-name (file-name-sans-extension
                     (file-name-nondirectory file-name)))
         (values (split-string base-name bongo-file-name-field-separator)))
    (when (> (length values) 5)
      (let ((fifth-and-rest (nthcdr 4 values)))
        (setcar fifth-and-rest (bongo-join-fields fifth-and-rest))
        (setcdr fifth-and-rest nil)))
    (cond
     ((= 5 (length values))
      `((artist (name . ,(nth 0 values)))
        (album (year . ,(nth 1 values))
               (title . ,(nth 2 values)))
        (track (index . ,(nth 3 values))
               (title . ,(nth 4 values)))))
     ((and (= 4 (length values))
           (string-match bongo-file-name-track-index-regexp (nth 2 values)))
      `((artist (name . ,(nth 0 values)))
        (album (title . ,(nth 1 values)))
        (track (index . ,(nth 2 values))
               (title . ,(nth 3 values)))))
     ((and (= 4 (length values))
           (string-match bongo-file-name-album-year-regexp (nth 1 values)))
      `((artist (name . ,(nth 0 values)))
        (album (year  . ,(nth 1 values))
               (title . ,(nth 2 values)))
        (track (title . ,(nth 3 values)))))
     ((= 4 (length values))
      `((artist (name . ,(nth 0 values)))
        (album (title . ,(nth 1 values)))
        (track (title . ,(bongo-join-fields (nthcdr 2 values))))))
     ((= 3 (length values))
      `((artist (name . ,(nth 0 values)))
        (album (title . ,(nth 1 values)))
        (track (title . ,(nth 2 values)))))
     ((= 2 (length values))
      `((artist (name . ,(nth 0 values)))
        (track (title . ,(nth 1 values)))))
     ((= 1 (length values))
      `((track (title . ,(nth 0 values))))))))

(defun bongo-simple-infoset-from-file-name (file-name)
  `((track (title . ,(file-name-sans-extension
                      (file-name-nondirectory file-name))))))


;;;; Basic point-manipulation routines

(defun bongo-goto-point (point)
  "Set point to POINT, if POINT is non-nil.
POINT may be a number, a marker or nil."
  (when point (goto-char point)))

(defun bongo-point-at-bol (&optional point)
  "Return the first character position of the line at POINT."
  (save-excursion (bongo-goto-point point) (point-at-bol)))

(defun bongo-point-at-eol (&optional point)
  "Return the last character position of the line at POINT."
  (save-excursion (bongo-goto-point point) (point-at-eol)))

(defun bongo-first-line-p (&optional point)
  "Return non-nil if POINT is on the first line."
  (= (bongo-point-at-bol point) (point-min)))

(defun bongo-last-line-p (&optional point)
  "Return non-nil if POINT is on the last line.
An empty line at the end of the buffer doesn't count."
  (>= (1+ (bongo-point-at-eol point)) (point-max)))

(defun bongo-first-object-line-p (&optional point)
  "Return non-nil if POINT is on the first object line."
  (null (bongo-point-at-previous-object-line point)))

(defun bongo-last-object-line-p (&optional point)
  "Return non-nil if POINT is on the last object line."
  (null (bongo-point-at-next-object-line point)))

(defalias 'bongo-point-before-line #'bongo-point-at-bol
  "Return the first character position of the line at POINT.")

(defun bongo-point-after-line (&optional point)
  "Return the first character position after the line at POINT.
For lines that end with newlines, the point after the line
is the same as the point before the next line."
  (let ((eol (bongo-point-at-eol point)))
    (if (= eol (point-max)) eol (1+ eol))))

(defun bongo-point-before-previous-line (&optional point)
  "Return the first point of the line before the one at POINT.
If the line at POINT is the first line, return nil."
  (unless (bongo-first-line-p point)
    (bongo-point-at-bol (1- (bongo-point-at-bol point)))))

(defun bongo-point-before-next-line (&optional point)
  "Return the first point of the line after the one at POINT.
If the line at POINT is the last line, return nil."
  (unless (bongo-last-line-p point)
    (1+ (bongo-point-at-eol point))))

(defalias 'bongo-point-at-previous-line
  #'bongo-point-before-previous-line)

(defalias 'bongo-point-at-next-line
  #'bongo-point-before-next-line)

(defun bongo-point-before-previous-line-satisfying (predicate &optional point)
  "Return the position of the previous line satisfying PREDICATE.
If POINT is non-nil, the search starts before the line at POINT.
If POINT is nil, it starts before the current line.
If no matching line is found, return nil."
  (save-excursion
    (bongo-goto-point point)
    (when (not (bongo-first-line-p))
      (let (match)
        (while (and (not (bobp)) (not match))
          (forward-line -1)
          (when (funcall predicate)
            (setq match t)))
        (when match (point))))))

(defalias 'bongo-point-at-previous-line-satisfying
  #'bongo-point-before-previous-line-satisfying)

(defun bongo-point-before-next-line-satisfying (predicate &optional point)
  "Return the position of the next line satisfying PREDICATE.
If POINT is non-nil, the search starts after the line at POINT.
If POINT is nil, it starts after the current line.
If no matching line is found, return nil."
  (save-excursion
    (bongo-goto-point point)
    (when (not (bongo-last-line-p))
      (let (match)
        (while (and (not (eobp)) (not match))
          (forward-line)
          (when (funcall predicate)
            (setq match t)))
        (when match (point))))))

(defalias 'bongo-point-at-next-line-satisfying
  #'bongo-point-before-next-line-satisfying)

(defun bongo-point-after-next-line-satisfying (predicate &optional point)
  "Return the position after the next line satisfying PREDICATE.
This function works like `bongo-point-before-next-line-satisfying'."
  (let ((before-next (bongo-point-before-next-line-satisfying
                      predicate point)))
    (when before-next
      (bongo-point-at-eol before-next))))

(defun bongo-point-before-previous-object-line (&optional point)
  "Return the character position of the previous object line.
If POINT is non-nil, start before that line; otherwise,
  start before the current line.
If no object line is found before the starting line, return nil."
  (bongo-point-before-previous-line-satisfying 'bongo-object-line-p point))

(defalias 'bongo-point-at-previous-object-line
  #'bongo-point-before-previous-object-line)

(defun bongo-point-before-next-object-line (&optional point)
  "Return the character position of the next object line.
If POINT is non-nil, start after that line; otherwise,
  start after the current line.
If no object line is found after the starting line, return nil."
  (bongo-point-before-next-line-satisfying 'bongo-object-line-p point))

(defalias 'bongo-point-at-next-object-line
  #'bongo-point-before-next-object-line)

(defun bongo-point-after-next-object-line (&optional point)
  "Return the character position after the next object line.
This function works like `bongo-point-before-next-object-line'."
  (bongo-point-after-next-line-satisfying 'bongo-object-line-p point))

(defun bongo-backward-object-line ()
  "If possible, move point to the previous object line.
If there is no previous object line, move to the beginning of the buffer.
Return non-nil if point was moved to an object line."
  (let ((position (bongo-point-at-previous-object-line)))
    (prog1 (not (null position))
      (goto-char (or position (point-min))))))

(defun bongo-forward-object-line ()
  "If possible, move point to the next object line.
If there is no next object line, move to the end of the buffer.
Return non-nil if point was moved to an object line."
  (let ((position (bongo-point-at-next-object-line)))
    (prog1 (not (null position))
      (goto-char (or position (point-max))))))

(defun bongo-point-before-next-track-line (&optional point)
  "Return the character position of the next track line.
If POINT is non-nil, start after that line; otherwise,
  start after the current line.
If no track line is found after the starting line, return nil."
  (bongo-point-before-next-line-satisfying 'bongo-track-line-p point))

(defalias 'bongo-point-at-next-track-line
  #'bongo-point-before-next-track-line)

(defun bongo-point-after-section (&optional point)
  "Return the point after the section with its header on POINT."
  (unless (bongo-header-line-p point)
    (error "Point is not on a section header"))
  (save-excursion 
    (bongo-goto-point point)
    (let ((indentation (bongo-line-indentation)))
      (bongo-forward-object-line)
      (while (and (> (bongo-line-indentation) indentation)
                  (not (eobp)))
        (bongo-forward-object-line))
      (point))))

;; (defun bongo-field-dependencies (field)
;;   (or (assoc field bongo-field-dependencies) (list field)))

;; (defun bongo-fields-with-dependencies (fields)
;;   (remove-duplicates (mapcan 'bongo-field-dependencies fields)))

;; (defun bongo-filter-info (fields info)
;;   (bongo-filter-alist (bongo-fields-with-dependencies fields) info)))

(defun bongo-track-infoset (&optional point)
  "Return the infoset for the track at POINT.
You should use `bongo-line-infoset' most of the time."
  (unless (bongo-track-line-p point)
    (error "Point is not on a track line"))
  (bongo-infoset-from-file-name (bongo-line-file-name point)))

(defun bongo-header-infoset (&optional point)
  "Return the infoset for the header at POINT.
You should use `bongo-line-infoset' most of the time."
  (unless (bongo-header-line-p point)
    (error "Point is not on a header line"))
  (let ((next-track (bongo-point-at-next-track-line)))
    (if (null next-track)
        (error "Dangling header line")
      (bongo-filter-alist (bongo-line-fields)
                          (bongo-track-infoset next-track)))))

(defun bongo-line-infoset (&optional point)
  "Return the infoset for the line at POINT.
For track lines, the infoset is obtained by passing the file name to
  `bongo-file-name-parsing-function'.
For header lines, it is derived from the `bongo-fields' text property
  and the infoset of the nearest following track line."
    (cond
     ((bongo-track-line-p point) (bongo-track-infoset point))
     ((bongo-header-line-p point) (bongo-header-infoset point))))

(defun bongo-line-internal-infoset (&optional point)
  "Return the internal infoset for the line at POINT.
The internal infoset contains values of the internal fields only."
  (bongo-filter-alist (bongo-line-internal-fields point)
                      (bongo-line-infoset point)))

(defun bongo-line-field-value (field &optional point)
  "Return the value of FIELD for the line at POINT."
  (assoc field (bongo-line-infoset point)))

(defun bongo-line-field-values (fields &optional point)
  "Return the values of FIELDS for the line at POINT."
  (bongo-filter-alist fields (bongo-line-infoset point)))

(defun bongo-line-fields (&optional point)
  "Return the names of the fields defined for the line at POINT."
  (if (bongo-header-line-p point)
      (bongo-line-get-property 'bongo-fields point)
    (mapcar 'car (bongo-line-infoset point))))

(defun bongo-line-external-fields (&optional point)
  "Return the names of the fields external to the line at POINT."
  (bongo-line-get-property 'bongo-external-fields point))

(defun bongo-line-set-external-fields (fields &optional point)
  "Set FIELDS to be external to the line at POINT.
FIELDS should be a list of field names."
  (save-excursion
    (bongo-goto-point point)
    (bongo-line-set-property 'bongo-external-fields fields)
    (if (bongo-empty-header-line-p)
        (bongo-delete-line)
      (bongo-redisplay-line))))

(defun bongo-line-internal-fields (&optional point)
  "Return the names of the fields internal to the line at POINT."
  (set-difference (bongo-line-fields point)
                  (bongo-line-external-fields point)))

(defun bongo-line-indentation (&optional point)
  "Return the number of external fields of the line at POINT."
  (length (bongo-line-external-fields point)))

(defun bongo-line-indented-p (&optional point)
  (> (bongo-line-indentation point) 0))

(defun bongo-line-external-fields-proposal (&optional point)
  "Return the external fields proposal of the line at POINT.
This proposal is a list of field names that subsequent lines can
externalize if their field values match those of this line.

For track lines, this is always the same as the external field names.
For header lines, the internal field names are also added."
  (cond ((bongo-track-line-p point)
         (bongo-line-external-fields point))
        ((bongo-header-line-p point)
         (append (bongo-line-external-fields point)
                 (bongo-line-internal-fields point)))))

(defun bongo-line-indentation-proposal (&optional point)
  "Return the number of external fields proposed by the line at POINT.
See `bongo-line-external-fields-proposal'."
  (cond ((bongo-track-line-p point)
         (bongo-line-indentation point))
        ((bongo-header-line-p point)
         (+ (length (bongo-line-external-fields point))
            (length (bongo-line-internal-fields point))))))

(defun bongo-line-proposed-external-fields (&optional point)
  "Return the external fields proposed to the line at POINT.
This is nil for the first line, and equal to the external field names
proposal of the previous object line for all other lines."
  (if (bongo-first-object-line-p point) nil
    (bongo-line-external-fields-proposal
     (bongo-point-at-previous-object-line point))))

(defun bongo-line-proposed-indentation (&optional point)
  "Return the number of external fields proposed to the line at POINT.
See `bongo-line-proposed-external-fields'."
  (if (bongo-first-object-line-p point) 0
    (bongo-line-indentation-proposal
     (bongo-point-at-previous-object-line point))))

;; (defun bongo-line-relatively-outdented-p ()
;;   (< (bongo-line-indentation) (bongo-line-proposed-indentation)))

(defun bongo-line-file-name (&optional point)
  "Return the `bongo-file-name' text property of the file at POINT.
This will be nil for header lines and non-nil for track lines."
  (bongo-line-get-property 'bongo-file-name point))

(defun bongo-track-line-p (&optional point)
  "Return non-nil if the line at POINT is a track line."
  (not (null (bongo-line-file-name point))))

(defun bongo-header-line-p (&optional point)
  "Return non-nil if the line at POINT is a header line."
  (bongo-line-get-property 'bongo-header-p point))

(defun bongo-object-line-p (&optional point)
  "Return non-nil if the line at POINT is an object line.
Object lines are either track lines or header lines."
  (or (bongo-track-line-p point) (bongo-header-line-p point)))

(defun bongo-empty-header-line-p (&optional point)
  "Return non-nil if the line at POINT is an empty header line.
Empty header lines have no internal fields and are not supposed ever
to exist for long enough to be visible to the user."
  (and (bongo-header-line-p point)
       (null (bongo-line-internal-fields point))))


;;;; General convenience routines

;; (defmacro nor (&rest conditions)
;;   `(not (or ,@conditions)))

(defun bongo-shortest (a b)
  "Return the shorter of the lists A and B."
  (if (<= (length a) (length b)) a b))

(defun bongo-longest (a b)
  "Return the longer of the lists A and B."
  (if (>= (length a) (length b)) a b))

(defun bongo-equally-long-p (a b)
  "Return non-nil if the lists A and B have equal length."
  (= (length a) (length b)))

(defun bongo-set-equal-p (a b)
  "Return non-nil if A and B have equal elements.
The order of the elements is not significant."
  (null (set-exclusive-or a b)))

(defun bongo-alist-get (alist key)
  "Return the cdr of the element in ALIST whose car equals KEY.
If no such element exists, return nil."
  (cdr-safe (assoc key alist)))

(defun bongo-alist-put (alist key value)
  "Set the cdr of the element in ALIST whose car equals KEY to VALUE.
If no such element exists, add a new element to the start of ALIST.
This function destructively modifies ALIST and returns the new head.
If ALIST is a symbol, operate on the vaule of that symbol instead."
  (if (and (symbolp alist) (not (null alist)))
      (set alist (bongo-alist-put (symbol-value alist) key value))
    (let ((entry (assoc key alist)))
      (if entry (prog1 alist (setcdr entry value))
        (cons (cons key value) alist)))))

(defun bongo-filter-alist (keys alist)
  "Return a new list of each pair in ALIST whose car is in KEYS."
  (remove-if-not (lambda (pair)
                   (memq (car pair) keys)) alist))

(if (and (fboundp 'process-put) (fboundp 'process-get))
    (progn
      (defalias 'bongo-process-get #'process-get)
      (defalias 'bongo-process-put #'process-put))

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
    (bongo-set-process-plist
     process (plist-put (bongo-process-plist process)
                        property value))))

(defun bongo-delete-line (&optional point)
  "Delete the line at POINT."
  (let ((inhibit-read-only t))
    (delete-region (bongo-point-before-line point)
                   (bongo-point-after-line point))))

(defun bongo-clear-line (&optional point)
  "Remove all contents of the line at POINT."
  (let ((inhibit-read-only t))
    (save-excursion
      (bongo-goto-point point)
      ;; Avoid deleting the newline, because that would
      ;; cause the markers on this line to become mixed up
      ;; with those on the next line.
      (delete-region (point-at-bol) (point-at-eol))
      (unless (eobp)
        ;; Remove all text properties from the newline.
        (set-text-properties (point) (1+ (point)) nil)))))

(defun bongo-region-line-count (beg end)
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

(defun bongo-line-get-property (name &optional point)
  "Return the value of the text property NAME at POINT."
  (get-text-property (or point (point)) name))

(defvar bongo-line-semantic-properties
  '(bongo-file-name bongo-header-p bongo-fields bongo-external-fields)
  "The list of semantic text properties used in Bongo buffers.
When redisplaying lines, semantic text properties are preserved,
whereas all other text properties (e.g., `face') are discarded.")

(defun bongo-line-get-semantic-properties (&optional point)
  "Return the list of semantic text properties at POINT.
The value of `bongo-line-semantic-properties' determines which
text properties are considered \"semantic\" by this function."
  (let ((properties (text-properties-at (or point (point))))
        (semantic-properties nil))
    (while properties
      (when (member (car properties) bongo-line-semantic-properties)
        (setq semantic-properties
              (cons (car properties)
                    (cons (cadr properties)
                          semantic-properties))))
      (setq properties (cddr properties)))
    semantic-properties))

(defun bongo-line-set-property (name value &optional point)
  "Set the text property NAME to VALUE on the line at POINT.
The text property will be set for every character on the line at POINT,
including any terminating newline."
  (let ((inhibit-read-only t))
    (put-text-property (bongo-point-before-line point)
                       (bongo-point-after-line point)
                       name value)))

(defun bongo-line-set-properties (properties &optional point)
  "Set the text properties PROPERTIES on the line at POINT.
The text properties will be set for every character on the line at POINT,
including any terminating newline."
  (let ((inhibit-read-only t))
    (add-text-properties (bongo-point-before-line point)
                         (bongo-point-after-line point)
                         properties)))

(defun bongo-line-remove-property (name &optional point)
  "Remove the text property NAME from the line at POINT.
The text property will be removed from every character on the line at POINT,
including any terminating newline."
  (let ((inhibit-read-only t))
    (remove-text-properties (bongo-point-before-line point)
                            (bongo-point-after-line point)
                            (list name nil))))


;;;; Sectioning

(defun bongo-region-field-common-p (beg end field)
  "Return non-nil if FIELD is common between BEG and END.
FIELD should be the name of a field (i.e., a symbol).
A field is common in a region if all object lines inside
the region share the same value for the field."
  (save-excursion
    (let ((last-value nil)
          (common-p t))
      (goto-char beg)
      (when (not (bongo-object-line-p))
        (bongo-forward-object-line))
      (when (< (point) end)
        (setq last-value (bongo-line-field-value field))
        (bongo-forward-object-line)
        (while (and common-p (< (point) end))
          (if (equal last-value (bongo-line-field-value field))
              (bongo-forward-object-line)
            (setq common-p nil))))
      common-p)))

;; XXX: This will not work properly unless the fields are
;;      strictly hierarchical.
(defun bongo-region-common-fields (beg end)
  "Return the names of all fields that are common between BEG and END.
See `bongo-region-common-field-name-p'."
  (let ((fields (reverse bongo-fields))
        (common-fields nil))
    (while fields
      (if (bongo-region-field-common-p beg end (car fields))
          (when (null common-fields)
            (setq common-fields fields))
        (setq common-fields nil))
      (setq fields (cdr fields)))
    common-fields))

(defun bongo-common-fields-at-point (&optional point)
  "Return the names of all fields that are common at POINT.
A field is common at POINT if it is common in the region around
the object at POINT and either the previous or the next object."
  (save-excursion
    (bongo-goto-point point)
    (unless (bongo-object-line-p)
      (error "Point is not on an object line"))
    (let ((before-previous (bongo-point-before-previous-object-line))
          (after-next (bongo-point-after-next-object-line)))
      (bongo-longest
       (when before-previous
         (bongo-region-common-fields before-previous
                                     (bongo-point-after-line)))
       (when after-next
         (bongo-region-common-fields (bongo-point-before-line)
                                     after-next))))))

;; XXX: This will not work properly unless the fields are
;;      strictly hierarchical.
(defun bongo-region-fields-external-p (beg end fields)
  "Return non-nil if FIELDS are external between BEG and END.
Return nil if there is a field in FIELDS that is not external for
at least one line in the region."
  (save-excursion
    (let ((external-p t))
      (goto-char beg)
      (while (and (< (point) end) external-p)
        (when (< (bongo-line-indentation) (length fields))
          (setq external-p nil))
        (forward-line))
      external-p)))

;; (defun bongo-external-fields-in-region-equal-p (beg end)
;;   "In Bongo, return the fields that are external in the region.
;; The region delimiters BEG and END should be integers or markers.

;; Only the fields that are external for all objects throughout
;; the region are considered to be external ``in the region.''"
;;   (save-excursion
;;     (goto-char beg)
;;     (let* ((equal t)
;;            (fields (bongo-external-fields))
;;            (values (bongo-get fields)))
;;       (while (and (< (point) end) equal)
;;         (unless (equal (bongo-get fields) values)
;;           (setq equal nil))
;;         (forward-line))
;;       equal)))

;;; (defun bongo-external-fields-at-point-equal-to-previous-p (&optional point)
;;;   (if (bongo-point-at-first-line-p point)
;;;       (zerop (bongo-indentation-at-point point))
;;;     (bongo-external-fields-in-region-equal-p
;;;      (bongo-point-before-previous-line point)
;;;      (bongo-point-after-line point))))

(defun bongo-line-potential-external-fields (&optional point)
  "Return the fields of the line at POINT that could be external.
That is, return the names of the fields that are common between
  the line at POINT and the object line before that.
If the line at POINT is the first line, return nil."
  (unless (bongo-first-object-line-p point)
    (bongo-region-common-fields
     (bongo-point-before-previous-object-line point)
     (bongo-point-after-line point))))

(defun bongo-line-externalizable-fields (&optional point)
  "Return the externalizable fields of the line at POINT.
That is, return the names of all internal fields of the line at POINT
that could be made external without changing anything else."
  (set-difference (intersection
                   (bongo-line-proposed-external-fields point)
                   (bongo-line-potential-external-fields point))
                  (bongo-line-external-fields point)))

(defun bongo-line-redundant-header-p (&optional point)
  "Return non-nil if the line at POINT is a redundant header.
Redundant headers are headers whose internal fields are all externalizable."
  (and (bongo-header-line-p point)
       (bongo-set-equal-p (bongo-line-externalizable-fields point)
                          (bongo-line-internal-fields point))))

(defun bongo-backward-up-section ()
  (interactive)
  (let ((indentation (bongo-line-indentation)))
    (when (zerop indentation)
      (error "Already at the top level"))
    (bongo-backward-object-line)
    (while (>= (bongo-line-indentation) indentation)
      (unless (bongo-backward-object-line)
        (error "Broken playlist sectioning")))))

(defun bongo-maybe-forward-object-line ()
  (interactive)
  (if (bongo-object-line-p) t
    (bongo-forward-object-line)))

(defun bongo-maybe-backward-object-line ()
  (interactive)
  (if (bongo-object-line-p) t
    (bongo-backward-object-line)))

(defun bongo-forward-section ()
  (interactive)
  (when (bongo-maybe-forward-object-line)
    (cond
     ((bongo-track-line-p)
      (bongo-forward-object-line))
     ((bongo-header-line-p)
      (goto-char (bongo-point-after-section))))))

(defun bongo-maybe-insert-intermediate-header ()
  "Make sure that the current line has a suitable header.
If the first outer header is too specific, split it in two."
  (when (bongo-line-indented-p)
    (let ((external-fields (bongo-line-external-fields)))
      (save-excursion
        (bongo-backward-up-section)
        (unless (bongo-set-equal-p
                 (bongo-line-external-fields-proposal)
                 external-fields)
          (bongo-insert-header external-fields)
          (bongo-externalize-fields))))))

(defun bongo-externalize-fields ()
  "Externalize as many fields of the current line as possible.
This function may create a new section header, but only by splitting an
existing header into two (see `bongo-maybe-insert-intermediate-header')."
  (with-bongo-buffer
    (unless (zerop (bongo-line-proposed-indentation))
      (let ((fields (bongo-line-externalizable-fields)))
        (when (> (length fields) (bongo-line-indentation))
          (bongo-line-set-external-fields fields)
          (bongo-maybe-insert-intermediate-header))))))


;;;; Player types

(defvar bongo-player-types
  '((mpg123 (default-matcher . "\\.[mM][pP][23]$")
            (constructor . bongo-start-mpg123-player))))

(defvar bongo-preferred-player-types nil)

(defun bongo-file-name-matches-p (file-name matcher)
  (cond
   ((eq t matcher) t)
   ((stringp matcher) (string-match matcher file-name))
   ((symbolp matcher) (funcall matcher file-name))
   (t (error "Bad file name matcher: %s" matcher))))

(defun bongo-best-player-type-for-file (file-name)
  (let ((best-player-type nil))
    (let ((list bongo-preferred-player-types))
      (while (and list (null best-player-type))
        (let* ((player-type (bongo-alist-get bongo-player-types
                                             (cdar list)))
               (matcher (or (caar list)
                            (bongo-alist-get player-type
                                             'default-matcher))))
          (when (bongo-file-name-matches-p file-name matcher)
            (setq best-player-type player-type)))
        (setq list (cdr list))))
    (unless best-player-type
      (let ((list bongo-player-types))
        (while (and list (null best-player-type))
          (when (bongo-file-name-matches-p
                 file-name (bongo-alist-get (car list) 'default-matcher))
            (setq best-player-type (car list))))
          (setq list (cdr list))))
    best-player-type))



(defcustom bongo-next-action 'bongo-play-next
  "The function to call after the current track finishes playing."
  :type '(choice
          (const :tag "Stop playback" bongo-stop)
          (const :tag "Play the next track" bongo-play-next)
          (const :tag "Play the same track again" bongo-replay-current)
          (const :tag "Play the previous track" bongo-play-previous)
          (const :tag "Play a random track" bongo-play-random))
  :group 'bongo)

(make-variable-buffer-local 'bongo-next-action)

(defun bongo-perform-next-action ()
  (interactive)
  (when bongo-next-action
    (funcall bongo-next-action)))


;;;; Players

(defvar bongo-player nil
  "The currently active player for this buffer, or nil.")
(make-variable-buffer-local 'bongo-player)

(defvar bongo-player-started-functions nil
  "Abnormal hook run when a player is started.")
(defvar bongo-player-succeeded-functions nil
  "Abnormal hook run when a player exits normally.")
(defvar bongo-player-failed-functions nil
  "Abnormal hook run when a player exits abnormally.")
(defvar bongo-player-killed-functions nil
  "Abnormal hook run when a player recieves a fatal signal.")
(defvar bongo-player-finished-functions nil
  "Abnormal hook run when a player exits for whatever reason.")

(defun bongo-player-succeeded (player)
  "Run the hooks appropriate for when PLAYER has succeeded."
  (when (bongo-player-buffer player)
    (with-current-buffer (bongo-player-buffer player)
      (run-hook-with-args 'bongo-player-succeeded-functions player)
      (bongo-player-finished player))))

(defun bongo-player-failed (player)
  "Run the hooks appropriate for when PLAYER has failed."
  (let ((process (bongo-player-process player)))
    (message "Process `%s' exited abnormally with code %d"
             (process-name process) (process-exit-status process)))
  (when (bongo-player-buffer player)
    (with-current-buffer (bongo-player-buffer player)
      (run-hook-with-args 'bongo-player-failed-functions player)
      (bongo-player-finished player))))

(defun bongo-player-killed (player)
  "Run the hooks appropriate for when PLAYER was killed."
  (let ((process (bongo-player-process player)))
    (message "Process `%s' received fatal signal %s"
             (process-name process) (process-exit-status process)))
  (when (bongo-player-buffer player)
    (with-current-buffer (bongo-player-buffer player)
      (run-hook-with-args 'bongo-player-killed-functions player)
      (bongo-player-finished player))))

(defun bongo-player-finished (player)
  "Run the hooks appropriate for when PLAYER has finished.
Then perform the next action according to `bongo-next-action'.
You should not call this function directly."
  (when (bongo-player-buffer player)
    (with-current-buffer (bongo-player-buffer player)
      (run-hook-with-args 'bongo-player-finished-functions player)
      (bongo-perform-next-action))))

(defun bongo-play (file-name &optional player-type-name)
  "Start playing FILE-NAME and return the new player.
In Bongo mode, first stop the currently active player, if any.
This function calls `bongo-start-player'."
  (when (eq major-mode 'bongo-mode)
    (when bongo-player
      (bongo-player-stop bongo-player)))
  (let ((player (bongo-start-player file-name player-type-name)))
    (prog1 player
      (when (eq major-mode 'bongo-mode)
        (setq bongo-player player)))))

(defun bongo-start-player (file-name &optional player-type-name)
  "Start and return a new player for FILE-NAME.
If you don't specify PLAYER-TYPE-NAME, Bongo will try to find
the best player for FILE-NAME.
This function runs `bongo-player-started-functions'."
  (let* ((player-type (if player-type-name
                          (bongo-alist-get bongo-player-types
                                           player-type-name)
                        (bongo-best-player-type-for-file file-name)))
         (player (funcall (bongo-alist-get player-type 'constructor)
                          file-name)))
    (prog1 player
      (run-hook-with-args 'bongo-player-started-functions player))))

(defun bongo-player-type (player)
  "Return the player type of PLAYER (`mpg123', `mplayer', etc.)."
  (car player))

(defun bongo-player-get (player property)
  "Return the value of PLAYER's PROPERTY."
  (bongo-alist-get (cdr player) property))

(defun bongo-player-put (player property value)
  "Set PLAYER's PROPERTY to VALUE."
  (setcdr player (bongo-alist-put (cdr player) property value)))

(defun bongo-player-call (player method &rest arguments)
  "Call METHOD on PLAYER with extra ARGUMENTS."
  (apply (bongo-player-get player method) player arguments))

(defun bongo-player-process (player)
  "Return the process associated with PLAYER."
  (bongo-player-get player 'process))

(defun bongo-player-buffer (player)
  "Return the buffer associated with PLAYER."
  (bongo-player-get player 'buffer))

(defun bongo-player-file-name (player)
  "Return the name of the file played by PLAYER."
  (bongo-player-get player 'file-name))

(defun bongo-player-infoset (player)
  "Return the infoset for the file played by PLAYER."
  (bongo-infoset-from-file-name (bongo-player-file-name player)))

(defun bongo-player-show-infoset (player)
  "Display in the minibuffer what PLAYER is playing."
  (message (bongo-format-infoset (bongo-player-infoset player))))

(defun bongo-player-running-p (player)
  "Return non-nil if PLAYER's process is currently running."
  (eq 'run (process-status (bongo-player-process player))))

(defun bongo-player-explicitly-stopped-p (player)
  "Return non-nil if PLAYER was explicitly stopped."
  (bongo-player-get player 'explicitly-stopped))

(defun bongo-player-stop (player)
  "Tell PLAYER to stop playback completely.
When this function returns, PLAYER will no longer be usable."
  (bongo-player-put player 'explicitly-stopped t)
  (bongo-player-call player 'stop))

(defun bongo-player-pause/resume (player)
  "Tell PLAYER to toggle its paused state.
If PLAYER does not support pausing, signal an error."
  (bongo-player-call player 'pause/resume))

(defun bongo-player-seek-by (player n)
  "Tell PLAYER to seek to absolute position N.
If PLAYER does not support seeking, signal an error."
  (bongo-player-call player 'seek-by n))

(defun bongo-player-seek-to (player n)
  "Tell PLAYER to seek N units relative to the current position.
If PLAYER does not support seeking, signal an error."
  (bongo-player-call player 'seek-to n))


;;;; Default implementations of player features

(defun bongo-default-player-stop (player)
  "Delete the process associated with PLAYER."
  (delete-process (bongo-player-process player)))

(defun bongo-default-player-pause/resume (player)
  "Signal an error explaining that PLAYER does not support pausing."
  (error "Pausing is not supported for %s"
         (bongo-player-type player)))

(defun bongo-default-player-seek-by (player n)
  "Signal an error explaining that PLAYER does not support seeking."
  (error "Seeking is not supported for %s"
         (bongo-player-type player)))

(defun bongo-default-player-seek-to (player n)
  "Signal an error explaining that PLAYER does not support seeking."
  (error "Seeking is not supported for %s"
         (bongo-player-type player)))

(defun bongo-default-player-process-sentinel (process string)
  "If PROCESS has exited or been killed, run the appropriate hooks."
  (let ((status (process-status process))
        (player (bongo-process-get process 'bongo-player)))
    (cond
     ((eq status 'exit)
      (if (zerop (process-exit-status process))
          (bongo-player-succeeded player)
        (bongo-player-failed player)))
     ((eq status 'signal)
      (unless (bongo-player-explicitly-stopped-p player)
        (bongo-player-killed player))))))


;;;; The mpg123 player backend

(defgroup bongo-mpg123 nil
  "The mpg123 player backend."
  :group 'bongo)

(defcustom bongo-mpg123-program-name "mpg123"
  "The name of the mpg123-compatible executable."
  :type 'string
  :group 'bongo-mpg123)

(defcustom bongo-mpg123-device-type nil
  "The type of device (oss, alsa, esd, etc.) used by mpg123.
This corresponds to the `-o' option of mpg123."
  :type '(choice (const :tag "System default" nil)
                 (const :tag "ALSA" "alsa")
                 (const :tag "OSS" "oss")
                 (const :tag "Sun" "sun")
                 (const :tag "ESD" "esd")
                 (const :tag "ARTS" "arts")
                 (string :tag "Other (specify)"))
  :group 'bongo-mpg123)

(defcustom bongo-mpg123-device nil
  "The device (e.g., for ALSA, 1:0 or 2:1) used by mpg123.
This corresponds to the `-a' option of mpg123."
  :type '(choice (const :tag "System default" nil) string)
  :group 'bongo-mpg123)

(defcustom bongo-mpg123-interactive t
  "If non-nil, use the remote-control facility of mpg123.
Setting this to nil disables the pause and seek functionality."
  :type 'boolean
  :group 'bongo-mpg123)

(defun bongo-mpg123-is-mpg321-p ()
  "Return non-nil if the mpg123 program is actually mpg321."
  (string-match "^mpg321\\b" (shell-command-to-string
                              (concat bongo-mpg123-program-name
                                      " --version"))))

(defcustom bongo-mpg123-update-granularity
  (when (bongo-mpg123-is-mpg321-p) 30)
  "The number of frames to skip between each update from mpg321.
This corresponds to the mpg321-specific option `--skip-printing-frames'.
If your mpg123 does not support that option, set this variable to nil."
  :type '(choice (const :tag "None (lowest)" nil) integer)
  :group 'bongo-mpg123)

(defcustom bongo-mpg123-seek-granularity 150
  "The minimum number of frames to skip when seeking relatively.
This is used by `bongo-mpg123-seek-by'."
  :type 'integer
  :group 'bongo-mpg123)

(defcustom bongo-mpg123-extra-arguments nil
  "Extra command-line arguments to pass to mpg123.
These will come at the end or right before the file name."
  :type '(repeat string)
  :group 'bongo-mpg123)

(defun bongo-mpg123-process-filter (process string)
  (cond
   ((string-match "^@P 0$" string)
    (bongo-player-succeeded (bongo-process-get process 'bongo-player))
    (set-process-sentinel process nil)
    (delete-process process))))

(defun bongo-mpg123-player-interactive-p (player)
  "Return non-nil if PLAYER's process is interactive.
Interactive mpg123 processes support pausing and seeking."
  (bongo-alist-get player 'interactive-flag))

(defun bongo-mpg123-player-pause/resume (player)
  (if (bongo-mpg123-player-interactive-p player)
      (process-send-string (bongo-player-process player) "PAUSE\n")
    (error "This mpg123 process does not support pausing")))

(defun bongo-mpg123-player-seek-to (player position)
  (if (bongo-mpg123-player-interactive-p player)
      (process-send-string (bongo-player-process player)
                           (format "JUMP %d\n" position))
    (error "This mpg123 process does not support seeking")))

(defun bongo-mpg123-player-seek-by (player delta)
  (if (bongo-mpg123-player-interactive-p player)
      (process-send-string
       (bongo-player-process player)
       (format "JUMP %s%d\n" (if (< delta 0) "-" "+")
               (* bongo-mpg123-seek-granularity (abs delta))))
    (error "This mpg123 process does not support seeking")))

(defun bongo-start-mpg123-player (file-name)
  (let* ((process-connection-type nil) ; Use a pipe, not a PTY.
         (arguments (append
                     (when bongo-mpg123-device-type
                       (list "-o" bongo-mpg123-device-type))
                     (when bongo-mpg123-device
                       (list "-a" bongo-mpg123-device))
                     (when bongo-mpg123-update-granularity
                       (list "--skip-printing-frames"
                             (number-to-string
                              bongo-mpg123-update-granularity)))
                     bongo-mpg123-extra-arguments
                     (if bongo-mpg123-interactive
                         '("-R" "dummy") (list file-name))))
         (process (apply 'start-process "bongo-mpg123" nil
                         bongo-mpg123-program-name arguments))
         (player `(mpg123
                   (process . ,process)
                   (file-name . ,file-name)
                   (buffer . ,(current-buffer))
                   (stop . bongo-default-player-stop)
                   (interactive-flag . ,bongo-mpg123-interactive)
                   (pause/resume . bongo-mpg123-player-pause/resume)
                   (seek-by . bongo-mpg123-player-seek-by)
                   (seek-to . bongo-mpg123-player-seek-to))))
    (prog1 player
      (set-process-sentinel process 'bongo-default-player-process-sentinel)
      (bongo-process-put process 'bongo-player player)
      (if (not bongo-mpg123-interactive)
          (set-process-filter process 'ignore)
        (set-process-filter process 'bongo-mpg123-process-filter)
        (process-send-string process (format "LOAD %s\n" file-name))))))


;;;; Controlling playback

(defun bongo-active-track-position ()
  "Return the character position of the active track, or nil."
  (marker-position overlay-arrow-position))

(defun bongo-set-active-track-position (&optional point)
  "Make the track on the line at POINT be the active track."
  (move-marker overlay-arrow-position (bongo-point-before-line point)))

(defun bongo-unset-active-track-position ()
  "Make it so that no track is active in this buffer."
  (move-marker overlay-arrow-position nil))

(defun bongo-line-active-track-p (&optional point)
  "Return non-nil if the line at POINT is the active track."
  (when (bongo-active-track-position)
    (and (>= (bongo-active-track-position)
             (bongo-point-before-line point))
         (< (bongo-active-track-position)
            (bongo-point-after-line point)))))

;;; (defun bongo-playing-p ()
;;;   "Return non-nil if there is an active player for this buffer."
;;;   (not (null bongo-player)))

(defun bongo-play-line (&optional point)
  "Start playing the track on the line at POINT."
  (interactive)
  (if (not (bongo-track-line-p point))
      (error "No track at point")
    (bongo-set-active-track-position point)
    (let ((player (bongo-play (bongo-line-file-name point))))
      (bongo-line-set-property 'bongo-player player point))))

(defun bongo-replay-current ()
  (interactive)
  (when (null (bongo-active-track-position))
    (error "No active track"))
  (bongo-play-line (bongo-active-track-position)))

(defun bongo-play-next ()
  (interactive)
  (when (null (bongo-active-track-position))
    (error "No active track"))
  (let ((position (bongo-point-at-next-track-line
                   (bongo-active-track-position))))
    (if (null position)
        (error "No more tracks")
      (bongo-play-line position))))

(defun bongo-tracks-exist-p ()
  (let (tracks-exist)
    (save-excursion
      (goto-char (point-min))
      (while (and (not (eobp)) (not tracks-exist))
        (when (bongo-track-line-p)
          (setq tracks-exist t))
        (forward-line)))
    tracks-exist))

(defun bongo-play-random ()
  (interactive)
  (unless (bongo-tracks-exist-p)
    (error "No tracks"))
  (save-excursion
    (goto-char (1+ (random (point-max))))
    (while (not (bongo-track-line-p))
      (goto-char (1+ (random (point-max)))))
    (bongo-play-line)))

(defun bongo-stop ()
  (interactive)
  (when bongo-player
    (bongo-player-stop bongo-player))
  (bongo-unset-active-track-position))

(defun bongo-pause/resume ()
  (interactive)
  (if bongo-player
      (bongo-player-pause/resume bongo-player)
    (error "No active player")))

(defun bongo-seek-forward (&optional n)
  (interactive "p")
  (if bongo-player
      (bongo-player-seek-by bongo-player n)
    (error "No active player")))

(defun bongo-seek-backward (&optional n)
  (interactive "p")
  (if bongo-player
      (bongo-player-seek-by bongo-player (- n))
    (error "No active player")))


;;;; Inserting

(defun bongo-insert-line (&rest properties)
  "Insert a new line with PROPERTIES before the current line.
Externalize as many fields of the new line as possible and redisplay it.
Point is left immediately after the new line."
  (with-bongo-buffer
    (let ((inhibit-read-only t))
      (insert (apply 'propertize "\n" properties)))
    (forward-line -1)
    (bongo-externalize-fields)
    (if (bongo-empty-header-line-p)
        (bongo-delete-line)
      (bongo-redisplay-line)
      (forward-line))))

(defun bongo-insert-header (&optional fields)
  "Insert a new header line with internal FIELDS.
FIELDS defaults to the external fields of the current line."
  (bongo-insert-line 'bongo-header-p t 'bongo-fields
                     (or fields (bongo-line-external-fields))))

(defun bongo-insert-file (file-name)
  "Insert a new track line corresponding to FILE-NAME.
If FILE-NAME names a directory, call `bongo-insert-directory'."
  (interactive (list (expand-file-name
                      (read-file-name "Insert track: "
                                      default-directory nil t))))
  (if (file-directory-p file-name)
      (bongo-insert-directory file-name)
    (bongo-insert-line 'bongo-file-name file-name)))

(defun bongo-insert-directory (directory-name)
  "Insert a new track line for each file in DIRECTORY-NAME.
Only insert files whose names match `bongo-file-name-regexp'.
Do not examine subdirectories of DIRECTORY-NAME."
  (interactive (list (expand-file-name
                      (read-directory-name "Insert directory: "
                                           default-directory nil t))))
  (when (not (file-directory-p directory-name))
    (error "File is not a directory: %s" directory-name))
;;;   (when (file-exists-p (concat directory-name "/cover.jpg")))
  (dolist (file-name (directory-files directory-name t
                                      bongo-file-name-track-regexp))
    (bongo-insert-file file-name)))

(defun bongo-insert-directory-tree (directory-name)
  "Insert a new track line for each file below DIRECTORY-NAME.
Only insert files whose names match `bongo-file-name-regexp'.

This function descends each subdirectory of DIRECTORY-NAME recursively,
but actually uses `bongo-gnu-find-program' to find the files."
  (interactive (list (expand-file-name
                      (read-directory-name "Insert directory tree: "
                                           default-directory nil t))))
  (with-temp-buffer
    (apply 'call-process bongo-gnu-find-program nil t nil
           directory-name "-type" "f"
           "-iregex" bongo-file-name-track-regexp
           bongo-gnu-find-extra-arguments)
    (sort-lines nil (point-min) (point-max))
    (goto-char (point-min))
    (while (not (eobp))
      (bongo-insert-file (buffer-substring (point) (point-at-eol)))
      (forward-line))))


;;; Joining/splitting

(defun bongo-join-region (beg end &optional fields)
  "Externalize all fields that are common between BEG and END.
This function creates a new header if necessary.
If the lines cannot be joined, an error is signaled."
  (interactive "r")
  (when (null fields)
    (unless (setq fields (bongo-region-common-fields beg end))
      (error "Cannot join tracks: no common fields")))
  (when (= 0 (bongo-region-line-count beg end))
    (error "Cannot join tracks: region empty"))
  (when (bongo-region-fields-external-p beg end fields)
    (error "Cannot join tracks: already joined"))
  (when (= 1 (bongo-region-line-count beg end))
    (error "Cannot join tracks: need more than one"))
  (save-excursion
    (setq end (move-marker (make-marker) end))
    (goto-char beg)
    (beginning-of-line)
    (let ((indent (length fields)))
      (while (< (point) end)
        (when (< (bongo-line-indentation) indent)
          (bongo-line-set-external-fields fields))
        (bongo-forward-object-line)))
    (move-marker end nil)
;;;     (when (bongo-line-redundant-header-p)
;;;       (bongo-delete-line))
    (goto-char beg)
    (bongo-insert-header)))

(defun bongo-join ()
  (interactive)
  (if (and transient-mark-mode mark-active)
      (bongo-join-region (region-beginning) (region-end))
    (let ((fields (bongo-common-fields-at-point)))
      (when (null fields)
        (error "Cannot join tracks: no common fields"))
      (let ((values (bongo-line-field-values fields))
            (before (bongo-point-before-line))
            (after (bongo-point-after-line)))
        (save-excursion
          (while (and (bongo-backward-object-line)
                      (equal values (bongo-line-field-values fields)))
            (setq before (bongo-point-before-line))))
        (save-excursion
          (while (and (bongo-forward-object-line)
                      (equal values (bongo-line-field-values fields)))
            (setq after (bongo-point-after-line))))
        (bongo-join-region before after fields)))))

(defun bongo-split ()
  (interactive)
  (save-excursion
    (when (not (bongo-object-line-p))
      (bongo-backward-object-line))
    (when (not (bongo-object-line-p))
      (error "No bongo object here"))
    (when (bongo-track-line-p)
      (unless (bongo-line-indented-p)
        (error "Cannot split tracks: not joined"))
      (bongo-backward-up-section))
    (let ((fields (bongo-line-internal-fields))
          (end (move-marker (make-marker) (bongo-point-after-section))))
      (bongo-delete-line)
      (while (< (point) end)
        (let ((previous (point)))
          (bongo-forward-section)
          (bongo-line-set-external-fields
           (set-difference (bongo-line-external-fields previous)
                           fields) previous)))
      (move-marker end nil))))


;;;; Displaying

(defun bongo-redisplay-line ()
  "Redisplay the current line, preserving semantic text properties."
  (let ((inhibit-read-only t)
        (indentation (bongo-line-indentation))
        (infoset (bongo-line-internal-infoset))
        (header-p (bongo-header-line-p))
        (properties (bongo-line-get-semantic-properties)))
    (save-excursion
      (bongo-clear-line)
      (dotimes (_ indentation) (insert bongo-indentation-string))
      (let ((content (bongo-format-infoset infoset)))
        (insert (if (not header-p) content
                  (bongo-format-header content))))
      (bongo-line-set-properties properties)
;;       (bongo-line-set-property 'face (if header-p 'bongo-header
;;                                        'bongo-track))
      )))

(defun bongo-redisplay (&optional arg)
  "Redisplay every line in the entire buffer.
With prefix argument, remove all indentation and headers."
  (interactive "P")
  (save-excursion
    (with-bongo-buffer
      (message "Rendering playlist...")
      (goto-char (point-min))
      (while (not (eobp))
        (cond
         ((and arg (bongo-header-line-p))
          (bongo-delete-line)
          (bongo-maybe-forward-object-line))
         ((and arg (bongo-track-line-p))
          (bongo-line-set-external-fields nil)
          (bongo-forward-object-line))
         ((bongo-object-line-p)
          (bongo-redisplay-line)
          (bongo-forward-object-line))))
      (message "Rendering playlist...done"))))


;;; Killing/yanking

(defun bongo-kill-line (&optional arg)
  "In Bongo, kill the current line.
With prefix argument, kill that many lines from point.
See `kill-line'."
  (interactive "P")
  (let ((inhibit-read-only t))
    (cond
     ((bongo-track-line-p)
      (when (bongo-line-active-track-p)
        (bongo-unset-active-track-position))
      (let ((kill-whole-line t))
        (beginning-of-line)
        (kill-line arg)))
     ((bongo-header-line-p)
      (kill-region (bongo-point-before-line)
                   (bongo-point-after-section)))
     (t
      (kill-line arg)))
;;     (when (bongo-redundant-header-at-point-p)
;;       (bongo-delete-line))
    ))

(defun bongo-kill-region (&optional beg end)
  "In Bongo, kill the lines between point and mark.
See `kill-region'."
  (interactive "r")
  (setq end (move-marker (make-marker) end))
  (save-excursion
    (goto-char beg)
    (bongo-kill-line)
    (while (< (point) end)
      (append-next-kill)
      (bongo-kill-line)))
  (move-marker end nil))

(defun bongo-show (&optional arg)
  "Display what Bongo is playing in the minibuffer.
With prefix argument, insert the description at point."
  (interactive "P")
  (let (string)
    (with-bongo-buffer
      (let ((position (bongo-active-track-position)))
        (when (null position)
          (error "No track is currently playing"))
        (setq string (bongo-format-infoset
                      (bongo-line-infoset position)))))
    (if arg (insert string)
      (message string))))

(defun bongo-yank (&optional arg)
  "In Bongo, reinsert the last sequence of killed lines.
See `yank'."
  (interactive "P")
  (let ((inhibit-read-only t))
    (beginning-of-line)
    (yank arg)
    (let ((beg (region-beginning))
          (end (move-marker (make-marker) (region-end))))
      (save-excursion
        (goto-char beg)
        (when (not (bongo-object-line-p))
          (bongo-forward-object-line))
        (while (and (< (point) end))
          (let ((player (bongo-line-get-property 'bongo-player)))
            (when player
              (if (and (eq player bongo-player)
                       (null (bongo-active-track-position)))
                  (bongo-set-active-track-position (point-at-bol))
                (bongo-line-remove-property 'bongo-player))))
          (bongo-forward-object-line))
        ;; These headers will stay if they are needed,
        ;; or disappear automatically otherwise.
        (goto-char end)
        (bongo-insert-header)
        (goto-char beg)
        (bongo-insert-header)
        ;; In case the upper header does disappear,
        ;; we need to merge backwards to connect.
        (when (not (bongo-object-line-p))
          (bongo-forward-object-line))
        (when (< (point) end)
          (bongo-externalize-fields))
        (move-marker end nil)))))

;; XXX: This definitely does not work properly.
(defun bongo-yank-pop (&optional arg)
  "In Bongo, replace the just-yanked lines with different ones.
See `yank-pop'."
  (interactive "P")
  (let ((inhibit-read-only t))
    (yank-pop arg)
    (bongo-externalize-fields)))

;; XXX: This probably does not work properly.
(defun bongo-undo (&optional arg)
  "In Bongo, undo some previous changes.
See `undo'."
  (interactive "P")
  (let ((inhibit-read-only t))
    (undo arg)))



(defun bongo-quit ()
  "Quit Bongo by selecting some other buffer."
  (interactive)
  (bury-buffer))

(defun bongo-mode ()
  "Major mode for Bongo playlist buffers."
  (interactive)
  (kill-all-local-variables)
  (use-local-map bongo-mode-map)
  (setq buffer-read-only t)
  (setq major-mode 'bongo-mode)
  (setq mode-name "Bongo")
  (setq font-lock-defaults '())
  (set (make-local-variable 'overlay-arrow-position) (make-marker))
;;  (set (make-local-variable 'yank-excluded-properties) nil)
  (run-mode-hooks 'bongo-mode-hook))

(defvar bongo-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "\C-m" 'bongo-play-line)
    (define-key map "q" 'bongo-quit)
    (define-key map "g" 'bongo-redisplay)
    (define-key map "j" 'bongo-join)
    (define-key map "J" 'bongo-split)
    (define-key map "k" 'bongo-kill-line)
    (substitute-key-definition
     'kill-line 'bongo-kill-line map global-map)
    (define-key map "w" 'bongo-kill-region)
    (substitute-key-definition
     'kill-region 'bongo-kill-region map global-map)
    (define-key map "y" 'bongo-yank)
    (substitute-key-definition
     'yank 'bongo-yank map global-map)
    (substitute-key-definition
     'yank-pop 'bongo-yank-pop map global-map)
    (substitute-key-definition
     'undo 'bongo-undo map global-map)
    (define-key map " " 'bongo-pause/resume)
    (define-key map "s" 'bongo-stop)
    (define-key map "f" 'bongo-seek-forward)
    (define-key map "b" 'bongo-seek-backward)
    (define-key map "if" 'bongo-insert-file)
    (define-key map "id" 'bongo-insert-directory)
    (define-key map "it" 'bongo-insert-directory-tree)
    map))

(defmacro with-bongo-buffer (&rest body)
  "Execute the forms in BODY in some Bongo buffer.
If the current buffer is a Bongo buffer, don't switch buffers.
Otherwise, switch to the default Bongo buffer.
If no Bongo buffer exists at all, a default one will be created."
  (declare (indent 0) (debug t))
  `(save-current-buffer
     (unless (eq major-mode 'bongo-mode)
       (set-buffer (bongo-default-buffer)))
     ,@body))

(defvar bongo-default-buffer nil
  "The default Bongo buffer, or nil.
When executed from a buffer that is not in Bongo mode,
all Bongo commands will operate on this buffer instead.
This variable overrides `bongo-default-buffer-name'.")

(defun bongo-default-buffer ()
  "Return the default Bongo buffer, creating it if necessary.
If the variable `bongo-default-buffer' is non-nil, return that.
Otherwise, return the buffer named `bongo-default-buffer-name',
creating it if necessary."
  (or bongo-default-buffer
      (get-buffer bongo-default-buffer-name)
      (save-excursion
        (set-buffer (get-buffer-create bongo-default-buffer-name))
        (bongo-mode)
        (current-buffer))))

(defun bongo ()
  "Switch to the default Bongo buffer.
See `bongo-default-buffer'."
  (interactive)
  (switch-to-buffer (bongo-default-buffer)))

(provide 'bongo)
