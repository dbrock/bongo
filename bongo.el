;;; bongo.el --- buffer-oriented media player for Emacs
;; Copyright (C) 2005, 2006  Daniel Brockman
;; Copyright (C) 2005  Lars Öhrman

;; Author: Daniel Brockman <daniel@brockman.se>
;; URL: http://www.brockman.se/software/bongo/
;; Created: September 3, 2005
;; Updated: April 27, 2006

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
  "Buffer-oriented media player."
  :prefix "bongo-"
  :group 'multimedia
  :group 'applications)

(defvar bongo-shipped-backends
  `((mpg123
     (matcher "mp3" "mp2")
     (constructor . bongo-start-mpg123-player))
    (mplayer
     (matcher "ogg" "mp3" "wav" "wma" "rm"
              "mpg" "mpeg" "avi" "mov" "asf" "wmv")
     (constructor . bongo-start-mplayer-player))
    (ogg123
     (matcher "ogg")
     (constructor
      (program-name . "ogg123")
      (arguments file-name)))
    (timidity
     (matcher "mid" "midi")
     (constructor
      (program-name . "timidity")
      (arguments "--quiet" file-name))))
  "List of player backends shipped with Bongo.
Entries are of the same form as for `bongo-custom-backends'.

The `mplayer' and `mpg123' backends support pausing and seeking,
while `ogg123' and `timidity' do not.  Furthermore, mpg123 can
report the amount of time remaining, and usually has better seek
granularity than mplayer.")

(defcustom bongo-enabled-backends nil
  "List of names of enabled Bongo player backends.
See `bongo-shipped-backends' for the available backends.
This is not the place to define your own backends; custom
backends (see `bongo-custom-backends') are always enabled.

The `mplayer' and `mpg123' backends support pausing and seeking,
while `ogg123' and `timidity' do not.  Furthermore, mpg123 can
report the amount of time remaining, and usually has better seek
granularity than mplayer."
  ;; XXX: This variable is not really a hook, but you can't
  ;;      use the `:options' keyword with '(repeat symbol)
  ;;      for some reason and this is an okay workaround.
  :type 'hook
  :options (mapcar 'car bongo-shipped-backends)
  :link '(custom-group-link bongo-mplayer)
  :link '(custom-group-link bongo-mpg123)
  :group 'bongo)

(defcustom bongo-custom-backends nil
  "List of user-defined Bongo player backends.
Entries are of the following form:
   (NAME (matcher . MATCHER)
         (constructor . CONSTRUCTOR))

MATCHER can be t, nil, a string, a list, or a symbol;
  see `bongo-file-name-matches-p' for more information.
CONSTRUCTOR is a function that recieves one argument, FILE-NAME.
  It should immediately start a player for FILE-NAME.

For simple backends that do not merit a custom constructor,
you may be able use the following form instead:
   (NAME (matcher . MATCHER)
         (constructor
          (program-name PROGRAM-NAME)
          (arguments ARGUMENT... file-name ARGUMENT...)))

For example, to add support for TiMidity++, you could use this:
   (setq bongo-custom-backends
         '(timidity (matcher \"mid\" \"midi\")
                    (constructor (program-name \"timidity\")
                                 (arguments \"--quiet\" file-name))))"
  :type `(repeat
          (cons :tag "Backend"
                :value (timidity-example
                        (matcher "mid" "midi")
                        (constructor
                         (program-name . "timidity")
                         (arguments "--quiet" file-name)))
                (symbol :tag "Name")
                (alist
                 :format "%v"
                 :key-type symbol
                 :options
                 (((const :tag "Matcher" matcher)
                   (choice
                    (const :tag "Always match" t)
                    (const :tag "Never match" nil)
                    (repeat :tag "File extension" string)
                    (regexp :tag "File name (regular expression)")
                    (function :tag "Custom predicate")))
                  ((const :tag "Constructor" constructor)
                   (choice
                    (alist
                     :tag "Simple constructor"
                     :key-type symbol
                     :options
                     (((const :tag "Program name" program-name) string)
                      ((const :tag "Argument list" arguments)
                       (choice (const :tag "Just the file name" (file-name))
                               (repeat :tag "Some strings and the file name"
                                (choice string
                                        (const :tag "The file name"
                                               file-name)))))))
                    function))))))
  :group 'bongo)

(defun bongo-backends (&optional include-disabled)
  "Return an alist of available and enabled Bongo backends.
If INCLUDE-DISABLED is non-nil, also include disabled backends.

The return value is always the entirety of `bongo-custom-backends'
followed by some subset of `bongo-shipped-backends'."
  (append bongo-custom-backends
          (let (enabled-backends)
            (mapc (lambda (x)
                    (when (memq (car x) bongo-enabled-backends)
                      (setq enabled-backends (cons x enabled-backends))))
                  bongo-shipped-backends)
            enabled-backends)))

(defcustom bongo-preferred-backends nil
  "List of preferred Bongo player backends.
Entries are of the form (BACKEND-NAME . MATCHER).

For example, if you want to use the `mplayer' backend for all tracks
regardless of file name, you could use the following setting:

   (setq bongo-preferred-backends '((mplayer . t)))

BACKEND-NAME is the key for an entry in `bongo-custom-backends'
  or `bongo-shipped-backends'.
MATCHER, if non-nil, overrides the default matcher for the backend;
  see `bongo-file-name-matches-p' for more information."
  :type `(repeat
          (cons :format "%t\n%d%v"
                :tag "Preference:"
                :doc "Please note that recently added backends \
may not appear in the list."
                (choice :tag "Backend"
                        ,@(mapcar (lambda (x) `(const ,(car x)))
                                  (bongo-backends t))
                        (symbol :tag "Other backend"))
                (choice :tag "When"
                        (const :tag "Default condition for backend" nil)
                        (const :tag "Always preferred" t)
                        (radio :tag "Custom condition" :value ".*"
                               (regexp :tag "File name pattern")
                               (repeat :tag "File name extensions" string)
                               (function :tag "File name predicate")))))
  :link '(custom-group-link bongo-mplayer)
  :link '(custom-group-link bongo-mpg123)
  :group 'bongo)

(defcustom bongo-prefer-library-buffers t
  "If non-nil, prefer library buffers over playlist buffers.
This affects what kind of buffer is created by `\\[bongo]' when there
are no existing Bongo buffers.

Regardless of this setting, you can a specific type of Bongo buffer
using `\\[bongo-library]' or `\\[bongo-playlist]'.  To create a new one,
simply create a new buffer and then switch to a Bongo mode using
`\\[bongo-library-mode]' or `\\[bongo-playlist-mode]'.

If you set this variable to nil, you can happily use Bongo without ever
seeing a library buffer (that is, unless you create one yourself)."
  :type 'boolean
  :group 'bongo)

(defcustom bongo-avoid-interrupting-playback nil
  "If non-nil, Bongo will not interrupt playback unless forced.
This affects playlist commands like `bongo-play-random'; to avoid
interrupting playback, they will merely change the playback order.

Most such commands take a prefix argument, which forces them to
interrupt playback if they normally wouldn't, or asks them not to
if they normally would.  (That is, the prefix argument makes the
command act as if this variable was temporarily toggled.)"
  :type 'boolean
  :group 'bongo)

(defcustom bongo-default-playlist-buffer-name "*Bongo Playlist*"
  "The name of the default Bongo playlist buffer."
  :type 'string
  :group 'bongo)

(defcustom bongo-default-library-buffer-name "*Bongo Library*"
  "The name of the default Bongo library buffer."
  :type 'string
  :group 'bongo)

(defcustom bongo-next-action 'bongo-play-next-or-stop
  "The function to call after the current track finishes playing."
  :type '(choice
          (const :tag "Stop playback" bongo-stop)
          (const :tag "Play the next track" bongo-play-next-or-stop)
          (const :tag "Play the same track again" bongo-replay-current)
          (const :tag "Play the previous track" bongo-play-previous)
          (const :tag "Play a random track" bongo-play-random))
  :group 'bongo)
(make-variable-buffer-local 'bongo-next-action)

(defvar bongo-stored-next-action nil
  "The old value of `bongo-next-action'.
This variable is used by `bongo-play-queued'.")

(defgroup bongo-file-names nil
  "File names and file name parsing in Bongo.
If your files do not have nice names, but they do have nice tags,
you can use the `tree-from-tags.rb' tool (shipped with Bongo) to
create a hierarchy of nicely-named links to your files."
  :group 'bongo)

(defcustom bongo-file-name-field-separator " - "
  "String used to split track file names into fields.
For example, if your tracks are named like this,

   Frank Morton - 2004 - Frank Morton - 01 - Pojken på Tallbacksvägen

and your file name field separator is \" - \" (which is the default),
then the fields are \"Frank Morton\", \"2004\", \"Frank Morton\", \"01\",
and \"Pojken på Tallbacksvägen\".

When the the fields of a track's file name have been extracted,
they are used to build an infoset.

This is used by `bongo-default-infoset-from-file-name'."
  :type 'string
  :group 'bongo-file-names)

(defcustom bongo-file-name-album-year-regexp
  "^\\([0-9]\\{4\\}\\|'?[0-9]\\{2\\}\\)$"
  "Regexp matching album years.
This is used by `bongo-default-infoset-from-file-name'."
  :type 'regexp
  :group 'bongo-file-names)

(defcustom bongo-file-name-track-index-regexp "^[0-9]+$"
  "Regexp matching track indices.
This is used by `bongo-default-infoset-from-file-name'."
  :type 'regexp
  :group 'bongo-file-names)

(defcustom bongo-album-cover-file-names
  '("cover.jpg" "cover.jpeg" "cover.png"
    "front.jpg" "front.jpeg" "front.png"
    "album.jpg" "album.jpeg" "album.png")
  "File names of images that should be considered album covers.
See also `bongo-insert-album-covers'."
  :type '(repeat string)
  :group 'bongo-file-names)

(defgroup bongo-display nil
  "Display of Bongo playlist and library buffers."
  :group 'bongo)

(defcustom bongo-field-separator " —— "
  "String used to separate field values in track descriptions.
This is used by the function `bongo-default-format-field'."
  :type '(choice (const :tag " —— (Unicode dashes)" " —— ")
                 (const :tag " -- (ASCII dashes)" " -- ")
                 string)
  :group 'bongo-display)

(defcustom bongo-insert-album-covers t
  "Whether to put album cover images into playlists.
This is done by `bongo-insert-directory' and by
  `bongo-insert-directory-tree'.
See also `bongo-album-cover-file-names'."
  :type 'boolean
  :link '(custom-group-link bongo-file-names)
  :group 'bongo-display)

(defcustom bongo-insert-intermediate-headers nil
  "Whether to automatically insert intermediate headers.
This is best explained by an example.  Say you have the
following section,

   [Frank Morton —— Frank Morton (2004)]
       01. Pojken på Tallbacksvägen
       02. Kanske det blir så att jag måste gå

and you insert the following section immediately afterwards.

   [Frank Morton —— Jag såg en film om en gammal man (2005)]
       01. Det är så mysigt att vara två
       02. Labyrinten

If this variable is nil, the result will be as follows:

   [Frank Morton —— Frank Morton (2004)]
       01. Pojken på Tallbacksvägen
       02. Kanske det blir så att jag måste gå
   [Frank Morton —— Jag såg en film om en gammal man (2005)]
       01. Det är så mysigt att vara två
       02. Labyrinten

On the other hand, if it is non-nil, the result will be as follows:

   [Frank Morton]
     [Frank Morton (2004)]
       01. Pojken på Tallbacksvägen
       02. Kanske det blir så att jag måste gå
     [Jag såg en film om en gammal man (2005)]
       01. Det är så mysigt att vara två
       02. Labyrinten

Notice that an intermediate header ``[Frank Morton]'' was inserted."
  :type 'boolean
  :group 'bongo-display)

(defcustom bongo-album-format "%t (%y)"
  "Template for displaying albums in Bongo.
This is used by the function `bongo-default-format-field'.
%t means the album title.
%y means the album year."
  :type 'string
  :group 'bongo-display)

(defcustom bongo-track-format "%i. %t"
  "Template for displaying tracks in Bongo.
This is used by the function `bongo-default-format-field'.
%t means the track title.
%i means the track index."
  :type 'string
  :group 'bongo-display)

(defcustom bongo-expanded-header-format "[%s]"
  "Template for displaying header lines for expanded sections.
%s means the header line content."
  :type 'string
  :group 'bongo-display)

(defcustom bongo-collapsed-header-format "[%s ...]"
  "Template for displaying header lines for collapsed sections.
%s means the header line content."
  :type 'string
  :group 'bongo-display)

(defcustom bongo-indentation-string "  "
  "String prefixed to lines once for each level of indentation."
  :type 'string
  :group 'bongo-display)

(defgroup bongo-infosets nil
  "Structured track information in Bongo."
  :group 'bongo)

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
  :group 'bongo-infosets)

(defcustom bongo-infoset-from-file-name-function
  'bongo-default-infoset-from-file-name
  "Function used to convert file names into infosets."
  :type 'function
  :group 'bongo-file-names
  :group 'bongo-infosets)

(defcustom bongo-infoset-formatting-function 'bongo-default-format-infoset
  "Function used to convert an infoset into a string."
  :type 'function
  :group 'bongo-display
  :group 'bongo-infosets)

(defcustom bongo-field-formatting-function 'bongo-default-format-field
  "Function used to convert an info field into a string.
This is used by the function `bongo-default-format-infoset'."
  :type 'function
  :group 'bongo-display
  :group 'bongo-infosets)

(defgroup bongo-faces nil
  "Faces used by Bongo."
  :group 'bongo)

(defface bongo-comment
  '((t (:inherit font-lock-comment-face)))
  "Face used for comments in Bongo buffers."
  :group 'bongo-faces)

(defface bongo-artist
  '((t (:inherit font-lock-keyword-face)))
  "Face used for Bongo artist names."
  :group 'bongo-faces)

(defface bongo-album
  '((t (:inherit default)))
  "Face used for Bongo albums (year, title, and punctuation)."
  :group 'bongo-faces)

(defface bongo-album-title
  '((t (:inherit (font-lock-type-face bongo-album))))
  "Face used for Bongo album titles."
  :group 'bongo-faces)

(defface bongo-album-year
  '((t (:inherit bongo-album)))
  "Face used for Bongo album years."
  :group 'bongo-faces)

(defface bongo-track
  '((t (:inherit default)))
  "Face used for Bongo tracks (index, title, and punctuation)."
  :group 'bongo-faces)

(defface bongo-track-title
  '((t (:inherit (font-lock-function-name-face bongo-track))))
  "Face used for Bongo track titles."
  :group 'bongo-faces)

(defface bongo-track-index
  '((t (:inherit bongo-track)))
  "Face used for Bongo track indices."
  :group 'bongo-faces)


;;;; Infoset- and field-related functions

(defun bongo-format-header (content collapsed-flag)
  "Decorate CONTENT so as to make it look like a header.
If COLLAPSED-FLAG is non-nil, assume the section is collapsed.

This function uses `bongo-expanded-header-format'
and `bongo-collapsed-header-format'."
  (format (if collapsed-flag
              bongo-collapsed-header-format
            bongo-expanded-header-format) content))

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
  (require 'format-spec)
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
      (if (string-match bongo-file-name-track-index-regexp (nth 3 values))
          `((artist (name . ,(nth 0 values)))
            (album (year . ,(nth 1 values))
                   (title . ,(nth 2 values)))
            (track (index . ,(nth 3 values))
                   (title . ,(nth 4 values))))
        `((artist (name . ,(nth 0 values)))
          (album (year . ,(nth 1 values))
                 (title . ,(nth 2 values)))
          (track (title . ,(bongo-join-fields (nthcdr 3 values)))))))
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

(defun bongo-skip-invisible ()
  "Move point to the next visible character.
If point is already on a visible character, do nothing."
  (while (and (not (eobp)) (line-move-invisible-p (point)))
    (goto-char (next-char-property-change (point)))))

(defun bongo-point-at-bol (&optional point)
  "Return the first character position of the line at POINT.
If `line-move-ignore-invisible' is non-nil, ignore invisible text."
  (save-excursion
    (bongo-goto-point point)
    (if (not line-move-ignore-invisible)
        (point-at-bol)
      (move-beginning-of-line nil)
      (point))))

(defun bongo-point-at-eol (&optional point)
  "Return the last character position of the line at POINT.
If `line-move-ignore-invisible' is non-nil, ignore invisible text."
  (save-excursion
    (bongo-goto-point point)
    (if (not line-move-ignore-invisible)
        (point-at-eol)
      (move-end-of-line nil)
      (point))))

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

(defun bongo-backward-section (&optional n)
  "Move backward across N balanced expressions.
Here, a balanced expression is a track or a section."
  (interactive "p")
  (when (null n) (setq n 1))
  (if (< n 0)
      (bongo-forward-section (- n))
    (when line-move-ignore-invisible
      (bongo-skip-invisible))
    (bongo-maybe-forward-object-line)
    (while (and (> n 0) (not (bobp)))
      (let ((original-indentation (bongo-line-indentation)))
        (bongo-backward-object-line)
        (when (> (bongo-line-indentation) original-indentation)
          (bongo-backward-up-section)))
      (setq n (- n 1)))))

(defun bongo-forward-section (&optional n)
  "Move forward across N balanced expressions.
Here, a balanced expression is a track or a section.
This function is a suitable value for `forward-sexp-function'."
  (interactive "p")
  (when (null n) (setq n 1))
  (if (< n 0)
      (bongo-backward-section (- n))
    (when line-move-ignore-invisible
      (bongo-skip-invisible))
    (while (and (> n 0) (not (eobp)))
      (bongo-maybe-forward-object-line)
      (goto-char (if (bongo-header-line-p)
                     (bongo-point-after-section)
                   (bongo-point-after-line)))
      (setq n (- n 1)))))

(defun bongo-point-before-next-track-line (&optional point)
  "Return the character position of the next track line.
If POINT is non-nil, start after that line; otherwise,
  start after the current line.
If no track line is found after the starting line, return nil."
  (bongo-point-before-next-line-satisfying 'bongo-track-line-p point))

(defalias 'bongo-point-at-next-track-line
  #'bongo-point-before-next-track-line)

(defun bongo-point-before-previous-track-line (&optional point)
  "Return the character position of the previous track line.
If POINT is non-nil, start before that line; otherwise,
  start before the current line.
If no track line is found before the starting line, return nil."
  (bongo-point-before-previous-line-satisfying 'bongo-track-line-p point))

(defalias 'bongo-point-at-previous-track-line
  #'bongo-point-before-previous-track-line)

(defun bongo-point-after-section (&optional point)
  "Return the point after the section with its header on POINT."
  (unless (bongo-header-line-p point)
    (error "Point is not on a section header"))
  (save-excursion 
    (bongo-goto-point point)
    (let ((indentation (bongo-line-indentation))
          (after-last (bongo-point-after-line)))
      (bongo-forward-object-line)
      (while (and (> (bongo-line-indentation) indentation)
                  (not (eobp)))
        (setq after-last (bongo-point-after-line))
        (bongo-forward-object-line))
      after-last)))

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

;;; (defun bongo-line-relatively-outdented-p ()
;;;   (< (bongo-line-indentation) (bongo-line-proposed-indentation)))

(defun bongo-line-file-name (&optional point)
  "Return the `bongo-file-name' text property of the file at POINT.
This will be nil for header lines and non-nil for track lines."
  (bongo-line-get-property 'bongo-file-name point))

(defun bongo-track-line-p (&optional point)
  "Return non-nil if the line at POINT is a track line."
  (not (null (bongo-line-file-name point))))

(defun bongo-track-lines-exist-p ()
  "Return non-nil if the buffer contains any track lines.
This function does not care about the visibility of the lines."
  (let (track-lines-exist)
    (let ((line-move-ignore-invisible nil))
      (save-excursion
        (goto-char (point-min))
        (while (and (not (eobp)) (not track-lines-exist))
          (when (bongo-track-line-p)
            (setq track-lines-exist t))
          (forward-line))))
    track-lines-exist))

(defun bongo-header-line-p (&optional point)
  "Return non-nil if the line at POINT is a header line."
  (bongo-line-get-property 'bongo-header point))

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

(defun bongo-collapsed-header-line-p (&optional point)
  "Return non-nil if the line at POINT is a collapsed header line.
Collapsed header lines are header lines whose sections are collapsed."
  (and (bongo-header-line-p point)
       (bongo-line-get-property 'bongo-collapsed point)))


;;;; General convenience routines

(defsubst bongo-xor (a b)
  "Return non-nil if exactly one of A and B is nil."
  (if a (not b) b))

(defmacro bongo-until (test &rest body)
  "If TEST yields nil, evaluate BODY... and repeat.
The order of execution is thus TEST, BODY..., TEST, BODY..., TEST,
and so on, until TEST returns non-nil.
Return the final value of TEST.

\(fn TEST BODY...)"
  (declare (indent 1) (debug t))
  (let ((result (gensym)))
    `(let (,result)
       (while (unless (setq ,result ,test)
                (prog1 t
                  ,@body)))
       ,result)))

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

(defun bongo-set-contains-p (a b)
  "Return non-nil if all elements in B are also in A.
The order of the elements is not significant."
  (bongo-set-equal-p (union a b) a))

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
  "Return a new list of each pair in ALIST whose car is in KEYS.
Key comparisons are done with `eq'."
  (remove-if-not (lambda (pair)
                   (memq (car pair) keys)) alist))

(defun bongo-filter-plist (keys plist)
  "Return a new list of each property in PLIST whose name is in KEYS.
Key comparisons are done with `eq'."
  (let (new-plist)
    (while plist
      (when (memq (car plist) keys)
        (setq new-plist `(,(car plist) ,(cadr plist) ,@new-plist)))
      (setq plist (cddr plist)))
    new-plist))


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

(when (and (fboundp 'process-put) (fboundp 'process-get))
  (defalias 'bongo-process-get #'process-get)
  (defalias 'bongo-process-put #'process-put))


;;;; Line-oriented convenience routines

(defun bongo-ensure-final-newline ()
  "Make sure the last line in the current buffer ends with a newline.
Do nothing if the current buffer is empty."
  (or (= (point-min) (point-max))
      (= (char-before (point-max)) ?\n)
      (save-excursion
        (goto-char (point-max))
        (insert "\n"))))

(defun bongo-delete-line (&optional point)
  "Delete the line at POINT."
  (let ((inhibit-read-only t))
    (delete-region (bongo-point-before-line point)
                   (bongo-point-after-line point))))

(defun bongo-extract-line (&optional point)
  "Delete the line at POINT and return its content.
The content includes the final newline, if any."
  (prog1 (buffer-substring (bongo-point-before-line point)
                           (bongo-point-after-line point))
    (bongo-delete-line point)))

(defun bongo-clear-line (&optional point)
  "Remove all contents of the line at POINT."
  (let ((inhibit-read-only t))
    (bongo-ensure-final-newline)
    (save-excursion
      (bongo-goto-point point)
      ;; Avoid deleting the newline, because that would
      ;; cause the markers on this line to become mixed up
      ;; with those on the next line.
      (delete-region (point-at-bol) (point-at-eol))
      ;; Remove all text properties from the newline.
      (set-text-properties (point) (1+ (point)) nil))))

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

;;; XXX: Should rename these to `bongo-get-line-property', etc.

(defun bongo-line-get-property (name &optional point)
  "Return the value of the text property NAME on the line at POINT.
Actually only look at the terminating newline."
  (get-text-property (bongo-point-at-eol point) name))

(defvar bongo-line-semantic-properties
  (list 'bongo-file-name 'bongo-header 'bongo-collapsed
        'bongo-fields 'bongo-external-fields
        'bongo-player)
  "The list of semantic text properties used in Bongo buffers.
When redisplaying lines, semantic text properties are preserved,
whereas all other text properties (e.g., `face') are discarded.")

(defun bongo-line-get-semantic-properties (&optional point)
  "Return the list of semantic text properties on the line at POINT.
Actually only look at the terminating newline.

The value of `bongo-line-semantic-properties' determines which
text properties are considered \"semantic\" by this function."
  (bongo-filter-plist bongo-line-semantic-properties
                      (text-properties-at (bongo-point-at-eol point))))

(defun bongo-line-set-property (name value &optional point)
  "Set the text property NAME to VALUE on the line at POINT.
The text property will only be set for the terminating newline."
  (let ((inhibit-read-only t)
        (position (bongo-point-at-eol point)))
    (bongo-ensure-final-newline)
    (put-text-property position (1+ position) name value)))

(defun bongo-line-set-properties (properties &optional point)
  "Set the text properties PROPERTIES on the line at POINT.
The text properties will only be set for the terminating newline."
  (let ((inhibit-read-only t)
        (position (bongo-point-at-eol point)))
    (bongo-ensure-final-newline)
    (add-text-properties position (1+ position) properties)))

(defun bongo-line-remove-property (name &optional point)
  "Remove the text property NAME from the line at POINT.
The text properties will only be removed from the terminating newline."
  (let ((inhibit-read-only t)
        (position (bongo-point-at-eol point)))
    (bongo-ensure-final-newline)
    (remove-text-properties position (1+ position) (list name nil))))

(defun bongo-keep-text-properties (beg end keys)
  "Keep only some properties in text from BEG to END."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((properties (text-properties-at (point)))
               (kept-properties (bongo-filter-plist keys properties))
               (next (or (next-property-change (point)) (point-max))))
          (set-text-properties (point) next kept-properties)
          (goto-char next))))))


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
See `bongo-region-field-common-p'."
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

;;; (defun bongo-external-fields-in-region-equal-p (beg end)
;;;   "In Bongo, return the fields that are external in the region.
;;; The region delimiters BEG and END should be integers or markers.
;;;
;;; Only the fields that are external for all objects throughout
;;; the region are considered to be external ``in the region.''"
;;;   (save-excursion
;;;     (goto-char beg)
;;;     (let* ((equal t)
;;;            (fields (bongo-external-fields))
;;;            (values (bongo-get fields)))
;;;       (while (and (< (point) end) equal)
;;;         (unless (equal (bongo-get fields) values)
;;;           (setq equal nil))
;;;         (forward-line))
;;;       equal)))
;;;
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
  that could be made external without controversy.
This function respects `bongo-insert-intermediate-headers',
  in order to implement the correct semantics."
  (if bongo-insert-intermediate-headers
      (set-difference (intersection
                       (bongo-line-proposed-external-fields point)
                       (bongo-line-potential-external-fields point))
                      (bongo-line-external-fields point))
    (let ((potential (bongo-line-potential-external-fields point)))
      (save-excursion
        (bongo-backward-object-line)
        (let (fields)
          (while (and (null fields) (bongo-line-indented-p))
            (bongo-backward-up-section)
            (let ((proposal (bongo-line-external-fields-proposal)))
              (when (bongo-set-contains-p potential proposal)
                (setq fields proposal))))
          fields)))))

(defun bongo-line-redundant-header-p (&optional point)
  "Return non-nil if the line at POINT is a redundant header.
Redundant headers are headers whose internal fields are all externalizable."
  (and (bongo-header-line-p point)
       (bongo-set-equal-p (bongo-line-externalizable-fields point)
                          (bongo-line-internal-fields point))))

(defun bongo-down-section (&optional n)
  "Move to the first object line in the section.
Otherwise signal an error."
  (interactive "p")
  (when (null n) (setq n 1))
  (while (> n 0)
    (if (bongo-header-line-p)
        (bongo-forward-object-line)
      (error "No section here"))
    (setq n (- n 1))))

(defun bongo-backward-up-section (&optional n)
  "Move to the header line of this section.
With N, repeat that many times."
  (interactive "p")
  (when (null n) (setq n 1))
  (while (> n 0)
    (let ((indentation (bongo-line-indentation)))
      (when (zerop indentation)
        (error "Already at the top level"))
      (bongo-backward-object-line)
      (while (>= (bongo-line-indentation) indentation)
        (unless (bongo-backward-object-line)
          (error "Broken sectioning"))))
    (setq n (- n 1))))

(defun bongo-maybe-forward-object-line ()
  (interactive)
  (if (bongo-object-line-p) t
    (bongo-forward-object-line)))

(defun bongo-maybe-backward-object-line ()
  (interactive)
  (if (bongo-object-line-p) t
    (bongo-backward-object-line)))

(defun bongo-maybe-insert-intermediate-header ()
  "Make sure that the current line has a suitable header.
If the first outer header is too specific, split it in two."
  (when (bongo-line-indented-p)
    (let ((current (bongo-line-external-fields)))
      (save-excursion
        (bongo-backward-up-section)
        (let ((proposal (bongo-line-external-fields-proposal)))
          (unless (bongo-set-equal-p current proposal)
            (bongo-insert-header current)
            (bongo-externalize-fields)))))))

(defun bongo-externalize-fields ()
  "Externalize as many fields of the current line as possible.
This function may create a new section header, but only by splitting an
existing header into two (see `bongo-maybe-insert-intermediate-header')."
  (unless (zerop (bongo-line-proposed-indentation))
    (let ((fields (bongo-line-externalizable-fields)))
      (when (> (length fields) (bongo-line-indentation))
        (bongo-line-set-external-fields fields)
        (bongo-maybe-insert-intermediate-header)))))


;;;; Backends

(defun bongo-get-backend (name)
  "Return the backend whose name is NAME."
  (or (assoc bongo-custom-backends name)
      (assoc bongo-shipped-backends name)))

(defun bongo-backend-name (backend)
  "Return the name of BACKEND (`mpg123', `mplayer', etc.)."
  (car backend))

(defun bongo-backend-get (backend property)
  "Return the value of BACKEND's PROPERTY."
  (bongo-alist-get (cdr backend) property))

(defun bongo-backend-put (backend property value)
  "Set BACKEND's PROPERTY to VALUE."
  (bongo-alist-put (cdr backend) property value))

(defun bongo-backend-matcher (backend)
  "Return BACKEND's matcher."
  (bongo-backend-get backend 'matcher))

(defun bongo-backend-constructor (backend)
  "Return BACKEND's constructor."
  (bongo-backend-get backend 'constructor))

(defun bongo-file-name-matches-p (file-name matcher)
  "Return non-nil if FILE-NAME matches MATCHER.
The possible values of MATCHER are listed below.

If it is t, return non-nil immediately.
If it is a string, treat it as a regular expression;
  return non-nil if FILE-NAME matches MATCHER.
If it is a symbol, treat it as a function name;
  return non-nil if (MATCHER FILE-NAME) returns non-nil.
If it is a list, treat it as a set of file name extensions;
  return non-nil if the extension of FILE-NAME appears in MATCHER.
Otherwise, signal an error."
  (cond
   ((eq t matcher) t)
   ((stringp matcher) (string-match matcher file-name))
   ((symbolp matcher) (funcall matcher file-name))
   ((listp matcher)
    (let ((extension (file-name-extension file-name)))
      (let (match)
        (while (and matcher (not match))
          (if (equal (car matcher) extension)
              (setq match t)
            (setq matcher (cdr matcher))))
        match)))
   (t (error "Bad file name matcher: %s" matcher))))

;;; XXX: These functions need to be refactored.

(defun bongo-track-file-name-regexp ()
  "Return a regexp matching the names of playable files.
Walk `bongo-preferred-backends', `bongo-custom-backends', and
`bongo-shipped-backends' --- collecting file name regexps and file name
extensions --- and construct a regexp that matches all of the possibilities."
  (let ((available-backends (bongo-backends)))
    (when (null available-backends)
      (error (concat "No backends are enabled; please customize "
                     "`bongo-enabled-backends'")))
    (let ((extensions nil) (regexps nil))
      (let ((list bongo-preferred-backends))
        (while list
          (let ((backend (assoc (caar list) available-backends)))
            (when (null backend)
              (error "No such backend: `%s'" (caar list)))
            (let ((matcher (or (cdar list)
                               (bongo-backend-matcher backend))))
              (cond
               ((stringp matcher)
                (setq regexps (cons matcher regexps)))
               ((listp matcher)
                (setq extensions (append matcher extensions))))))
          (setq list (cdr list))))
      (let ((list available-backends))
        (while list
          (let ((matcher (bongo-backend-matcher (car list))))
            (cond
             ((stringp matcher)
              (setq regexps (cons matcher regexps)))
             ((listp matcher)
              (setq extensions (append matcher extensions)))))
          (setq list (cdr list))))
      (when extensions
        (let ((regexp (format ".*\\.%s$" (regexp-opt extensions t))))
          (setq regexps (cons regexp regexps))))
      (if (null regexps) "."
        (mapconcat 'identity regexps "\\|")))))

(defun bongo-best-backend-for-file (file-name)
  "Return a backend that can play the file FILE-NAME, or nil.
First search `bongo-preferred-backends', then `bongo-custom-backends',
then `bongo-shipped-backends'."
  (let ((available-backends (bongo-backends))
        (best-backend nil))
    (let ((list bongo-preferred-backends))
      (while (and list (null best-backend))
        (let ((backend (assoc (caar list) available-backends)))
          (when (null backend)
            (error "No such backend: `%s'" (caar list)))
          (let ((matcher (or (cdar list)
                             (bongo-backend-matcher backend))))
            (when (bongo-file-name-matches-p file-name matcher)
              (setq best-backend backend))))
        (setq list (cdr list))))
    (unless best-backend
      (let ((list available-backends))
        (while (and list (null best-backend))
          (let ((matcher (bongo-backend-matcher (car list))))
            (if (bongo-file-name-matches-p file-name matcher)
                (setq best-backend (car list))
              (setq list (cdr list)))))))
    best-backend))


;;;; Players

(defvar bongo-player nil
  "The currently active player for this buffer, or nil.")
(make-variable-buffer-local 'bongo-player)

(defcustom bongo-player-started-hook '(bongo-show)
  "Normal hook run when a Bongo player is started."
  :options '(bongo-show)
  :type 'hook
  :group 'bongo)

(defcustom bongo-player-finished-hook nil
  "Normal hook run when a Bongo player finishes."
  :options '((lambda () (bongo-show) (sit-for 2)))
  :type 'hook
  :group 'bongo)

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
  (when (buffer-live-p (bongo-player-buffer player))
    (with-current-buffer (bongo-player-buffer player)
      (run-hook-with-args 'bongo-player-succeeded-functions player)
      (bongo-player-finished player))))

(defun bongo-player-failed (player)
  "Run the hooks appropriate for when PLAYER has failed."
  (let ((process (bongo-player-process player)))
    (message "Process `%s' exited abnormally with code %d"
             (process-name process) (process-exit-status process)))
  (when (buffer-live-p (bongo-player-buffer player))
    (with-current-buffer (bongo-player-buffer player)
      (run-hook-with-args 'bongo-player-failed-functions player)
      (bongo-player-finished player))))

(defun bongo-player-killed (player)
  "Run the hooks appropriate for when PLAYER was killed."
  (let ((process (bongo-player-process player)))
    (message "Process `%s' received fatal signal %s"
             (process-name process) (process-exit-status process)))
  (when (buffer-live-p (bongo-player-buffer player))
    (with-current-buffer (bongo-player-buffer player)
      (run-hook-with-args 'bongo-player-killed-functions player)
      (bongo-player-finished player))))

(defun bongo-perform-next-action ()
  "Perform the next Bongo action, if any.
The next action is specified by `bongo-next-action'."
  (interactive)
  (when bongo-next-action
    (let ((bongo-avoid-interrupting-playback nil))
      (funcall bongo-next-action))))

(defun bongo-player-finished (player)
  "Run the hooks appropriate for when PLAYER has finished.
Then perform the next action according to `bongo-next-action'.
You should not call this function directly."
  (when (buffer-live-p (bongo-player-buffer player))
    (with-current-buffer (bongo-player-buffer player)
      (run-hook-with-args 'bongo-player-finished-functions player)
      (run-hooks 'bongo-player-finished-hook)
      (bongo-perform-next-action))))

(defun bongo-play (file-name &optional backend-name)
  "Start playing FILE-NAME and return the new player.
In Bongo mode, first stop the currently active player, if any.

BACKEND-NAME specifies which backend to use; if it is nil,
Bongo will try to find the best player for FILE-NAME.

This function runs `bongo-player-started-hook'."
  (when (bongo-buffer-p)
    (when bongo-player
      (bongo-player-stop bongo-player)))
  (let ((player (bongo-start-player file-name backend-name)))
    (prog1 player
      (when (bongo-buffer-p)
        (setq bongo-player player))
      (run-hooks 'bongo-player-started-hook))))

(defcustom bongo-player-process-priority nil
  "The desired scheduling priority of Bongo player processes.
If set to a non-nil value, `bongo-renice' will be used to alter
the scheduling priority after a player process is started."
  :type '(choice (const :tag "Default" nil)
                 (const :tag "Slightly higher (-5)" -5)
                 (const :tag "Much higher (-10)" -10)
                 (const :tag "Very much higher (-15)" -15)
                 integer)
  :group 'bongo)

(defcustom bongo-renice-command "sudo renice"
  "The shell command to use in place of the `renice' program.
It will get three arguments: the priority, \"-p\", and the PID."
  :type 'string
  :group 'bongo)

(defun bongo-renice (pid priority)
  "Alter the priority of PID (process ID) to PRIORITY.
The variable `bongo-renice-command' says what command to use."
  (call-process shell-file-name nil nil nil shell-command-switch
                (format "%s %d -p %d" bongo-renice-command
                        priority pid)))

(defun bongo-start-player (file-name &optional backend-name)
  "Start and return a new Bongo player for FILE-NAME.

BACKEND-NAME specifies which backend to use; if it is nil,
Bongo will try to find the best player for FILE-NAME.

This function runs `bongo-player-started-functions'.
See also `bongo-play'."
  (let ((backend (if backend-name
                     (bongo-alist-get (bongo-backends) backend-name)
                   (bongo-best-backend-for-file file-name))))
    (when (null backend)
      (error "Don't know how to play `%s'" file-name))
    (let* ((constructor (bongo-backend-constructor backend))
           (player (cond
                    ((symbolp constructor)
                     (funcall constructor file-name))
                    ((listp constructor)
                     (bongo-start-simple-player backend file-name))
                    (t (error "Invalid constructor for `%s' backend: %s"
                              (bongo-backend-name backend) constructor))))
           (process (bongo-player-process player)))
      (prog1 player
        (when (and process bongo-player-process-priority
                   (eq 'run (process-status process)))
          (bongo-renice (process-id process)
                        bongo-player-process-priority))
        (run-hook-with-args 'bongo-player-started-functions player)))))

(defun bongo-player-backend-name (player)
  "Return the name of PLAYER's backend (`mpg123', `mplayer', etc.)."
  (car player))

(defun bongo-player-backend (player)
  "Return PLAYER's backend object."
  (bongo-get-backend (bongo-player-backend-name player)))

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

(defun bongo-player-elapsed-time (player)
  "Return the number of seconds PLAYER has played so far.
If the player backend cannot report this, return nil."
  (or (bongo-player-get player 'elapsed-time)
      (when (bongo-player-get player 'get-elapsed-time)
        (bongo-player-call player 'get-elapsed-time))))

(defun bongo-player-total-time (player)
  "Return the total number of seconds PLAYER has and will use.
If the player backend cannot report this, return nil."
  (or (bongo-player-get player 'total-time)
      (when (bongo-player-get player 'get-total-time)
        (bongo-player-call player 'get-total-time))))


;;;; Default implementations of player features

(defun bongo-default-player-stop (player)
  "Delete the process associated with PLAYER."
  (delete-process (bongo-player-process player)))

(defun bongo-default-player-pause/resume (player)
  "Signal an error explaining that PLAYER does not support pausing."
  (error "Pausing is not supported for %s"
         (bongo-player-backend-name player)))

(defun bongo-default-player-seek-by (player n)
  "Signal an error explaining that PLAYER does not support seeking."
  (error "Seeking is not supported for %s"
         (bongo-player-backend-name player)))

(defun bongo-default-player-seek-to (player n)
  "Signal an error explaining that PLAYER does not support seeking."
  (error "Seeking is not supported for %s"
         (bongo-player-backend-name player)))

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

(defun bongo-start-simple-player (backend file-name)
  "This function is used by `bongo-start-player'."
  (let* ((process-connection-type nil)
         (backend-name (bongo-backend-name backend))
         (options (bongo-backend-constructor backend))
         (program-name
          (or (bongo-alist-get options 'program-name)
              (error "\
Missing option `program-name' in simple constructor \
for `%s' backend: %s" backend-name options)))
         (process
          (apply 'start-process
                 (format "bongo-%s" backend-name)
                 nil program-name
                 (mapcar (lambda (x)
                           (cond
                            ((stringp x) x)
                            ((eq x 'file-name) file-name)
                            (t (error "\
Invalid argument specifier in `arguments' option of simple \
constructor for `%s' backend" (car backend)))))
                         (bongo-alist-get options 'arguments))))
         (player `(,backend-name
                   (process . ,process)
                   (file-name . ,file-name)
                   (buffer . ,(current-buffer))
                   (stop . bongo-default-player-stop))))
    (prog1 player
      (set-process-sentinel process 'bongo-default-player-process-sentinel)
      (bongo-process-put process 'bongo-player player))))


;;;; The mpg123 backend

(defgroup bongo-mpg123 nil
  "The mpg123 backend to Bongo."
  :group 'bongo)

(defcustom bongo-mpg123-program-name "mpg123"
  "The name of the mpg123-compatible executable."
  :type 'string
  :group 'bongo-mpg123)

(defcustom bongo-mpg123-device-type nil
  "The type of device (oss, alsa, esd, etc.) to be used by mpg123.
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
  "The device (e.g., for ALSA, 1:0 or 2:1) to be used by mpg123.
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
This corresponds to the mpg321-specific option --skip-printing-frames.
If your mpg123 does not support that option, set this variable to nil."
  :type '(choice (const :tag "None (lowest)" nil) integer)
  :group 'bongo-mpg123)

(defcustom bongo-mpg123-seek-increment 150
  "The step size (in frames) to use for relative seeking.
This is used by `bongo-mpg123-seek-by'."
  :type 'integer
  :group 'bongo-mpg123)

(defcustom bongo-mpg123-extra-arguments nil
  "Extra command-line arguments to pass to mpg123.
These will come at the end or right before the file name."
  :type '(repeat string)
  :group 'bongo-mpg123)

;;; XXX: What happens if a record is split between two calls
;;;      to the process filter?
(defun bongo-mpg123-process-filter (process string)
  (let ((player (bongo-process-get process 'bongo-player)))
    (cond
     ((string-match "^@P 0$" string)
      (bongo-player-succeeded player)
      (set-process-sentinel process nil)
      (delete-process process))
     ((string-match "^@F .+ .+ \\(.+\\) \\(.+\\)$" string)
      (let* ((elapsed-time (string-to-number (match-string 1 string)))
             (total-time (+ elapsed-time (string-to-number
                                          (match-string 2 string)))))
        (bongo-player-put player 'elapsed-time elapsed-time)
        (bongo-player-put player 'total-time total-time))))))

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
               (* bongo-mpg123-seek-increment (abs delta))))
    (error "This mpg123 process does not support seeking")))

(defun bongo-mpg123-player-get-elapsed-time (player)
  (bongo-player-get player 'elapsed-time))

(defun bongo-mpg123-player-get-total-time (player)
  (bongo-player-get player 'total-time))

(defun bongo-start-mpg123-player (file-name)
  (let* ((process-connection-type nil)
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
                   (seek-to . bongo-mpg123-player-seek-to)
                   (seek-unit . frames))))
    (prog1 player
      (set-process-sentinel process 'bongo-default-player-process-sentinel)
      (bongo-process-put process 'bongo-player player)
      (if (not bongo-mpg123-interactive)
          (set-process-filter process 'ignore)
        (set-process-filter process 'bongo-mpg123-process-filter)
        (process-send-string process (format "LOAD %s\n" file-name))))))


;;;; The mplayer backend

(defgroup bongo-mplayer nil
  "The mplayer backend to Bongo."
  :group 'bongo)

(defcustom bongo-mplayer-program-name "mplayer"
  "The name of the mplayer executable."
  :type 'string
  :group 'bongo-mplayer)

(defcustom bongo-mplayer-audio-device nil
  "The audio device to be used by mplayer.
This corresponds to the `-ao' option of mplayer."
  :type '(choice (const :tag "System default" nil)
                 string)
  :group 'bongo-mplayer)

(defcustom bongo-mplayer-video-device nil
  "The video device to be used by mplayer.
This corresponds to the `-vo' option of mplayer."
  :type '(choice (const :tag "System default" nil)
                 string)
  :group 'bongo-mplayer)

(defcustom bongo-mplayer-interactive t
  "If non-nil, use the slave mode of mplayer.
Setting this to nil disables the pause and seek functionality."
  :type 'boolean
  :group 'bongo-mplayer)

(defcustom bongo-mplayer-extra-arguments nil
  "Extra command-line arguments to pass to mplayer.
These will come at the end or right before the file name."
  :type '(repeat string)
  :group 'bongo-mplayer)

(defcustom bongo-mplayer-seek-increment 5.0
  "The step size (in seconds) to use for relative seeking.
This is used by `bongo-mplayer-seek-by'."
  :type 'float
  :group 'bongo-mplayer)

(defun bongo-mplayer-player-interactive-p (player)
  "Return non-nil if PLAYER's process is interactive.
Interactive mplayer processes support pausing and seeking."
  (bongo-alist-get player 'interactive-flag))

(defun bongo-mplayer-player-pause/resume (player)
  (if (bongo-mplayer-player-interactive-p player)
      (process-send-string (bongo-player-process player) "pause\n")
    (error "This mplayer process does not support pausing")))

(defun bongo-mplayer-player-seek-to (player position)
  (if (bongo-mpg123-player-interactive-p player)
      (process-send-string
       (bongo-player-process player)
       (format "seek %f 2\n" position))
    (error "This mplayer process does not support seeking")))

(defun bongo-mplayer-player-seek-by (player delta)
  (if (bongo-mplayer-player-interactive-p player)
      (process-send-string
       (bongo-player-process player)
       (format "seek %f 0\n" (* bongo-mplayer-seek-increment delta)))
    (error "This mplayer process does not support seeking")))

(defun bongo-start-mplayer-player (file-name)
  (let* ((process-connection-type nil)
         (arguments (append
                     (when bongo-mplayer-audio-device
                       (list "-ao" bongo-mplayer-audio-device))
                     (when bongo-mplayer-video-device
                       (list "-vo" bongo-mplayer-video-device))
                     bongo-mplayer-extra-arguments
                     (if bongo-mplayer-interactive
                         (list "-quiet" "-slave" file-name)
                       (list file-name))))
         (process (apply 'start-process "bongo-mplayer" nil
                         bongo-mplayer-program-name arguments))
         (player `(mplayer
                   (process . ,process)
                   (file-name . ,file-name)
                   (buffer . ,(current-buffer))
                   (stop . bongo-default-player-stop)
                   (interactive-flag . ,bongo-mplayer-interactive)
                   (pause/resume . bongo-mplayer-player-pause/resume)
                   (seek-by . bongo-mplayer-player-seek-by)
                   (seek-to . bongo-mplayer-player-seek-to)
                   (seek-unit . seconds))))
    (prog1 player
      (set-process-sentinel process 'bongo-default-player-process-sentinel)
      (bongo-process-put process 'bongo-player player))))


;;;; DWIM commands

(defun bongo-dwim (&optional prefix-argument)
  "In Bongo, do what the user means to the object at point.

If point is on a header, collapse or expand the section below.
If point is on a track, the action is contingent on the mode:
  In Bongo Playlist mode, call `bongo-play-line'.
  In Bongo Library mode, call `bongo-insert-enqueue-line' and then,
    unless either `bongo-avoid-interrupting-playback' xor
    PREFIX-ARGUMENT is non-nil, call `bongo-play-next'.

If point is neither on a track nor on a header, do nothing."
  (interactive "P")
  (cond
   ((and (bongo-track-line-p) (bongo-library-buffer-p))
    (let ((position (if (bongo-playing-p)        
                        (bongo-insert-enqueue-line)
                      (bongo-append-enqueue-line))))
      (with-bongo-playlist-buffer
        (unless (and (bongo-playing-p)
                     (bongo-xor bongo-avoid-interrupting-playback
                                prefix-argument))
          (let ((bongo-avoid-interrupting-playback nil))
            (bongo-play-line position prefix-argument))))))
   ((and (bongo-track-line-p) (bongo-playlist-buffer-p))
    (bongo-play-line (point) prefix-argument))
   ((bongo-header-line-p)
    (bongo-toggle-collapsed))))

(defun bongo-mouse-dwim (event)
  "In Bongo, do what the user means to the object that was clicked on.
See `bongo-dwim'."
  (interactive "e")
  (let ((posn (event-end event)))
    (with-current-buffer (window-buffer (posn-window posn))
      (save-excursion
        (goto-char (posn-point posn))
        (bongo-dwim)))))


;;;; Controlling playback

(defun bongo-playing-p ()
  "Return non-nil if there is an active player for this buffer."
  (with-bongo-playlist-buffer
    (and (not (null bongo-player))
         (bongo-player-running-p bongo-player))))

(defvar bongo-active-track-marker nil
  "Marker pointing at the currently playing track, if any.")
(make-variable-buffer-local 'bongo-active-track-marker)

(defun bongo-active-track-position (&optional point)
  "Return the position of `bongo-active-track-marker'."
  (and bongo-active-track-marker
       (marker-position bongo-active-track-marker)))

(defun bongo-set-active-track (&optional point)
  "Make `bongo-active-track-marker' point to the line at POINT."
  (move-marker bongo-active-track-marker
               (bongo-point-before-line point)))

(defun bongo-unset-active-track ()
  "Make `bongo-active-track-marker' point nowhere."
  (move-marker bongo-active-track-marker nil))

(defun bongo-active-track-line-p (&optional point)
  "Return non-nil if the line at POINT is the active track."
  (when (bongo-active-track-position)
    (and (>= (bongo-active-track-position)
             (bongo-point-before-line point))
         (< (bongo-active-track-position)
            (bongo-point-after-line point)))))

(defvar bongo-queued-track-marker nil
  "Marker pointing at the queued track, if any.
This is used by `bongo-play-queued'.

The functions `bongo-set-queued-track' and `bongo-unset-queued-track'
  can properly manipulate this variable and its value.
If `bongo-avoid-interrupting-playback' is non-nil and a track is
  currently being played, `bongo-play-line' sets the queued track.")
(make-variable-buffer-local 'bongo-queued-track-marker)

(defun bongo-queued-track-position ()
  "Return the position of `bongo-queued-track-marker', or nil."
  (and bongo-queued-track-marker
       (marker-position bongo-queued-track-marker)))

(defvar bongo-queued-track-arrow-marker nil
  "Overlay arrow marker following `bongo-queued-track-marker'.
See also `overlay-arrow-variable-list'.")
(make-variable-buffer-local 'bongo-queued-track-arrow-marker)

(defcustom bongo-queued-track-arrow-type 'blinking-arrow
  "Type of overlay arrow used to indicate the queued track.
If nil, don't indicate the queued track using an overlay arrow.
If `arrow', use a static arrow.  If `blinking-arrow', use a
blinking arrow (see `bongo-queued-track-arrow-blink-frequency').
See `bongo-queued-track-arrow-marker'."
  :type '(choice (const :tag "None" nil)
                 (const :tag "Arrow" arrow)
                 (const :tag "Blinking arrow" blinking-arrow))
  :group 'bongo-display)

(defcustom bongo-queued-track-arrow-blink-frequency 1
  "Frequency (in Hertz) with which to blink the queued track arrow.
See `bongo-queued-track-arrow-type'."
  :type 'number
  :group 'bongo-display)

(defvar bongo-queued-track-arrow-timer nil
  "The timer that updates the blinking queued track arrow, or nil.
See `bongo-queued-track-arrow-type'.")
(make-variable-buffer-local 'bongo-queued-track-arrow-timer)

(defun bongo-queued-track-line-p (&optional point)
  "Return non-nil if POINT is on the queued track.
See `bongo-queued-track-marker'."
  (save-excursion
    (bongo-goto-point point)
    (when line-move-ignore-invisible
      (bongo-skip-invisible))
    (equal (bongo-queued-track-position)
           (point-at-bol))))

(defun bongo-unset-queued-track ()
  "Make `bongo-queued-track-marker' point nowhere.
In addition, set `bongo-next-action' to the value of
`bongo-stored-next-action' and set the latter to nil."
  (when bongo-queued-track-arrow-timer
    (cancel-timer bongo-queued-track-arrow-timer)
    (setq bongo-queued-track-arrow-timer nil))
  (move-marker bongo-queued-track-marker nil)
  (move-marker bongo-queued-track-arrow-marker nil)
  (setq bongo-next-action bongo-stored-next-action
        bongo-stored-next-action nil))

(defmacro with-point-at-bongo-track (point &rest body)
  "Execute BODY with point at the Bongo track line at POINT.
If there is no track at POINT, use the next track line.
If there is no next track line, signal an error."
  (declare (indent 1) (debug t))
  `(save-excursion
     (bongo-goto-point ,point)
     (when line-move-ignore-invisible
       (bongo-skip-invisible))
     (let ((line-move-ignore-invisible nil))
       (when (not (bongo-track-line-p))
         (bongo-goto-point (bongo-point-at-next-track-line)))
       (when (not (bongo-track-line-p))
        (error "No track at point"))
       ,@body)))

(defun bongo-set-queued-track (&optional point)
  "Make `bongo-queued-track-marker' point to the track at POINT.
In addition, unless `bongo-next-action' is already set to
`bongo-play-queued', set `bongo-stored-next-action' to the value
of `bongo-next-action' and set the latter to `bongo-play-queued'."
  (interactive "d")
  (with-point-at-bongo-track point
    (move-marker bongo-queued-track-marker (point-at-bol point))
    (unless (eq bongo-next-action 'bongo-play-queued)
      (setq bongo-stored-next-action bongo-next-action
            bongo-next-action 'bongo-play-queued))
    (if (null bongo-queued-track-arrow-type)
        (message "Queued track: %s" (bongo-format-infoset
                                     (bongo-line-infoset point)))
      (move-marker bongo-queued-track-arrow-marker
                   bongo-queued-track-marker)
      (when (eq bongo-queued-track-arrow-type 'blinking-arrow)
        (when bongo-queued-track-arrow-timer
          (cancel-timer bongo-queued-track-arrow-timer))
        (setq bongo-queued-track-arrow-timer
              (run-at-time
               (/ 1.0 bongo-queued-track-arrow-blink-frequency)
               (/ 1.0 bongo-queued-track-arrow-blink-frequency)
               'bongo-blink-queued-track-arrow))))))

(defun bongo-play-line (&optional point toggle-interrupt)
  "Start playing the track on the line at POINT.
If `bongo-avoid-interrupting-playback' is non-nil and a track is
  currently being played, call `bongo-set-queued-track' instead.
If TOGGLE-INTERRUPT (prefix argument if interactive) is non-nil,
  act as if `bongo-avoid-interrupting-playback' were reversed.
If there is no track on the line at POINT, signal an error."
  (interactive "d\nP")
  (with-point-at-bongo-track point
    (if (and (bongo-playing-p)
             (bongo-xor bongo-avoid-interrupting-playback
                        toggle-interrupt))
        ;; Something is being played and we should not
        ;; interrupt it.
        (if (bongo-queued-track-line-p)
            (bongo-unset-queued-track)
          (bongo-set-queued-track))
      ;; Nothing is being played or we should interrupt it.
      (bongo-set-active-track)
      (let ((player (bongo-play (bongo-line-file-name))))
        (bongo-line-set-property 'bongo-player player)))))

(defun bongo-blink-queued-track-arrow ()
  "Blink the overlay arrow indicating the queued track.
See `bongo-queued-track-arrow-marker'."
  (if (marker-position bongo-queued-track-arrow-marker)
      (move-marker bongo-queued-track-arrow-marker nil)
    (move-marker bongo-queued-track-arrow-marker
                 bongo-queued-track-marker)))

(defun bongo-play-queued ()
  "Play the track at `bongo-queued-track-marker'.
Then call `bongo-unset-queued-track'."
  (bongo-play-line (or (bongo-queued-track-position)
                       (error "No queued track")))
  (bongo-unset-queued-track))

(defun bongo-replay-current (&optional toggle-interrupt)
  "Play the current track from the start.
If `bongo-avoid-interrupting-playback' is non-nil,
  just set `bongo-next-action' to `bongo-replay-current'.
If TOGGLE-INTERRUPT (prefix argument if interactive) is non-nil,
  act as if `bongo-avoid-interrupting-playback' were reversed."
  (interactive "P")
  (with-bongo-playlist-buffer
    (if (not (bongo-xor bongo-avoid-interrupting-playback
                        toggle-interrupt))
        ;; We should interrupt playback, so play the current
        ;; track from the beginning.
        (let ((position (bongo-active-track-position))
              (bongo-avoid-interrupting-playback nil))
          (if position (bongo-play-line position)
            (error "No active track")))
      ;; We should not interrupt playback.
      (if (eq bongo-next-action 'bongo-replay-current)
          (message (concat "Switched to repeating playback "
                           "(prefix argument forces)."))
        (setq bongo-next-action 'bongo-replay-current)
        (message "Switched to repeating playback.")))))

(defun bongo-play-next (&optional toggle-interrupt)
  "Start playing the next track in the current Bongo buffer.
If `bongo-avoid-interrupting-playback' is non-nil,
  just set `bongo-next-action' to `bongo-play-next-or-stop'.
If TOGGLE-INTERRUPT (prefix argument if interactive) is non-nil,
  act as if `bongo-avoid-interrupting-playback' were reversed.
If there is no next track to play, signal an error."
  (interactive "P")
  (with-bongo-playlist-buffer
    (if (not (bongo-xor bongo-avoid-interrupting-playback
                        toggle-interrupt))
        ;; We should interrupt playback, so start playing
        ;; the next track immediately.
        (let ((line-move-ignore-invisible nil)
              (position (bongo-active-track-position))
              (bongo-avoid-interrupting-playback nil))
          (when (null position)
            (error "No active track"))
          (if (setq position (bongo-point-at-next-track-line position))
              (bongo-play-line position)
            (error "No next track")))
      ;; We should not interrupt playback.
      (if (eq bongo-next-action 'bongo-play-next-or-stop)
          (message (concat "Switched to sequential playback "
                           "(prefix argument forces)."))
        (setq bongo-next-action 'bongo-play-next-or-stop)
        (message "Switched to sequential playback.")))))

(defun bongo-play-next-or-stop (&optional toggle-interrupt)
  "Maybe start playing the next track in the current Bongo buffer.
If `bongo-avoid-interrupting-playback' is non-nil,
  just set `bongo-next-action' to `bongo-play-next-or-stop'.
If TOGGLE-INTERRUPT (prefix argument if interactive) is non-nil,
  act as if `bongo-avoid-interrupting-playback' were reversed.
If there is no next track to play, stop playback."
  (interactive "P")
  (with-bongo-playlist-buffer
    (if (not (bongo-xor bongo-avoid-interrupting-playback
                        toggle-interrupt))
        ;; We should interrupt playback, so start playing
        ;; the next track immediately.
        (let ((line-move-ignore-invisible nil)
              (position (bongo-active-track-position))
              (bongo-avoid-interrupting-playback nil))
          (when (null position)
            (error "No active track"))
          (let ((next-position (bongo-point-at-next-track-line position)))
            (if next-position
                (bongo-play-line next-position)
              (bongo-play-line position))))
      ;; We should not interrupt playback.
      (if (eq bongo-next-action 'bongo-play-next-or-stop)
          (message (concat "Switched to sequential playback "
                           "(prefix argument forces)."))
        (setq bongo-next-action 'bongo-play-next-or-stop)
        (message "Switched to sequential playback.")))))

(defun bongo-play-previous (&optional toggle-interrupt)
  "Start playing the previous track in the current Bongo buffer.
If `bongo-avoid-interrupting-playback' is non-nil,
  just set `bongo-next-action' to `bongo-play-previous'.
If TOGGLE-INTERRUPT (prefix argument if interactive) is non-nil,
  act as if `bongo-avoid-interrupting-playback' were reversed."
  (interactive "P")
  (with-bongo-playlist-buffer
    (if (not (bongo-xor bongo-avoid-interrupting-playback
                        toggle-interrupt))
        ;; We should interrupt playback, so start playing
        ;; the previous track immediately.
        (let ((line-move-ignore-invisible nil)
              (position (bongo-active-track-position))
              (bongo-avoid-interrupting-playback nil))
          (when (null position)
            (error "No active track"))
          (let ((previous-position
                 (bongo-point-at-previous-track-line position)))
            (if previous-position
                (bongo-play-line previous-position)
              (error "No previous track"))))
      ;; We should not interrupt playback.
      (if (eq bongo-next-action 'bongo-play-previous)
          (message (concat "Switched to reverse sequential playback "
                           "(prefix argument forces)."))
        (setq bongo-next-action 'bongo-play-previous)
        (message "Switched to reverse sequential playback.")))))

(defun bongo-play-random (&optional toggle-interrupt)
  "Start playing a random track in the current Bongo buffer.
If `bongo-avoid-interrupting-playback' is non-nil,
  just set `bongo-next-action' to `bongo-play-random'.
If TOGGLE-INTERRUPT (prefix argument if interactive) is non-nil,
  act as if `bongo-avoid-interrupting-playback' were reversed."
  (interactive "P")
  (with-bongo-playlist-buffer
    (if (not (bongo-xor bongo-avoid-interrupting-playback 
                        toggle-interrupt))
        ;; We should interrupt playback, so start playing a
        ;; random track immediately.
        (let ((line-move-ignore-invisible nil)
              (bongo-avoid-interrupting-playback nil))
          (unless (bongo-track-lines-exist-p)
            (error "Buffer contains no tracks"))
          (save-excursion
            (goto-char (1+ (random (point-max))))
            (bongo-play-line)))
      ;; We should not interrupt playback.
      (if (eq bongo-next-action 'bongo-play-random)
          (message (concat "Switched to random playback "
                           "(prefix argument forces)."))
        (setq bongo-next-action 'bongo-play-random)
        (message "Switched to random playback.")))))

(defun bongo-stop (&optional toggle-interrupt)
  "Permanently stop playback in the current Bongo buffer.
If `bongo-avoid-interrupting-playback' is non-nil,
  just set `bongo-next-action' to `bongo-stop'.
If TOGGLE-INTERRUPT (prefix argument if interactive) is non-nil,
  act as if `bongo-avoid-interrupting-playback' were reversed."
  (interactive "P")
  (with-bongo-playlist-buffer
    (if (not (bongo-xor bongo-avoid-interrupting-playback
                        toggle-interrupt))
        ;; We should interrupt playback.
        (progn
          (when bongo-player
            (bongo-player-stop bongo-player))
          (bongo-unset-active-track)
          (bongo-unset-queued-track))
      ;; We should not interrupt playback.
      (if (eq bongo-next-action 'bongo-stop)
          (message (concat "Playback will stop after the current track "
                           "(prefix argument forces)."))
        (setq bongo-next-action 'bongo-stop)
        (message "Playback will stop after the current track.")))))

(defun bongo-pause/resume ()
  "Pause or resume playback in the current Bongo buffer.
This functionality may not be available for all backends."
  (interactive)
  (with-bongo-playlist-buffer
    (if bongo-player
        (bongo-player-pause/resume bongo-player)
      (error "No active player"))))

(defun bongo-seek-forward (&optional n)
  "Seek N units forward in the currently playing track.
The time unit is currently backend-specific.
This functionality may not be available for all backends."
  (interactive "p")
  (with-bongo-playlist-buffer
    (if bongo-player
        (bongo-player-seek-by bongo-player n)
      (error "No active player"))))

(defun bongo-seek-backward (&optional n)
  "Seek N units backward in the currently playing track.
The time unit it currently backend-specific.
This functionality may not be available for all backends."
  (interactive "p")
  (with-bongo-playlist-buffer
    (if bongo-player
        (bongo-player-seek-by bongo-player (- n))
      (error "No active player"))))

(defun bongo-seek-to (position)
  "Seek to POSITION in the currently playing track.
The time unit is currently backend-specific.
This functionality may not be available for all backends."
  (interactive
   (with-bongo-playlist-buffer
     (if bongo-player
         (list
          (let ((unit (bongo-player-get bongo-player 'seek-unit)))
            (cond
             ((null unit)
              (error "This player does not support seeking"))
             ((eq unit 'frames)
              (read-number "Seek to (in frames): "))
             ((eq unit 'seconds)
              (let ((total-time (bongo-player-total-time bongo-player)))
                (bongo-until
                    (bongo-parse-time
                     (read-string
                      (if (null total-time)
                          "Seek to (in seconds or MM:SS): "
                        (format "Seek to (max %s): "
                                (bongo-format-seconds total-time)))))
                  (message "Please enter a number or HH:MM:SS.")
                  (sit-for 2)))))))
       (error "No active player"))))
  (with-bongo-playlist-buffer
    (if bongo-player
        (bongo-player-seek-to bongo-player position)
      (error "No active player"))))


;;;; Inserting

(defun bongo-insert-line (&rest properties)
  "Insert a new line with PROPERTIES before the current line.
Externalize as many fields of the new line as possible and redisplay it.
Point is left immediately after the new line."
  (let ((inhibit-read-only t))
    (insert (apply 'propertize "\n" properties)))
  (forward-line -1)
  (bongo-externalize-fields)
  (if (bongo-empty-header-line-p)
      (bongo-delete-line)
    (bongo-redisplay-line)
    (forward-line)))

(defun bongo-insert-header (&optional fields)
  "Insert a new header line with internal FIELDS.
FIELDS defaults to the external fields of the current line."
  (bongo-insert-line 'bongo-header t 'bongo-fields
                     (or fields (bongo-line-external-fields))))

(defun bongo-insert-file (file-name)
  "Insert a new track line corresponding to FILE-NAME.
If FILE-NAME names a directory, call `bongo-insert-directory'."
  (interactive (list (expand-file-name
                      (read-file-name "Insert track: "
                                      default-directory nil t
                                      (when (eq major-mode 'dired-mode)
                                        (dired-get-filename t))))))
  (if (file-directory-p file-name)
      (bongo-insert-directory file-name)
    (bongo-insert-line 'bongo-file-name file-name)
    (when (and (interactive-p) (not (bongo-buffer-p)))
      (message "Inserted track: %s"
               (bongo-format-infoset
                (bongo-infoset-from-file-name file-name))))))

(defun bongo-maybe-insert-album-cover (directory-name)
  "Insert the album cover in DIRECTORY-NAME, if one exists.
Album covers are files whose names are in `bongo-album-cover-file-names'."
  (let ((cover-file-name nil)
        (file-names bongo-album-cover-file-names))
    (while (and file-names (null cover-file-name))
      (let ((file-name (concat directory-name "/" (car file-names))))
        (when (file-exists-p file-name)
          (setq cover-file-name file-name)))
      (setq file-names (cdr file-names)))
    (when cover-file-name
      (let ((file-type-entry
             (assoc (downcase (file-name-extension cover-file-name))
                    '(("jpg" . jpeg) ("jpeg" . jpeg)
                      ("png" . png) ("gif" . gif)))))
        (when (null file-type-entry)
          (error "Unrecognized file name extension: %s" cover-file-name))
        (let ((cover-file-type (cdr file-type-entry))
              (inhibit-read-only t))
          (insert "\n")
          (insert (propertize "(cover image)" 'display
                              `(image :type ,cover-file-type
                                      :file ,cover-file-name)))
          (insert "\n"))))))

(defun bongo-insert-directory (directory-name)
  "Insert a new track line for each file in DIRECTORY-NAME.
Only insert files that can be played by some backend, as determined
by the file name (see `bongo-track-file-name-regexp').

If `bongo-insert-album-covers' is non-nil, then for each directory
that contains a file whose name is in `bongo-album-cover-file-names',
insert the image in that file before the directory contents.

Do not examine subdirectories of DIRECTORY-NAME."
  (interactive (list (expand-file-name
                      (read-directory-name
                       "Insert directory: " default-directory nil t
                       (when (eq major-mode 'dired-mode)
                         (when (file-directory-p (dired-get-filename))
                           (dired-get-filename t)))))))
  (with-bongo-buffer
    (when (not (file-directory-p directory-name))
      (error "File is not a directory: %s" directory-name))
    (when bongo-insert-album-covers
      (bongo-maybe-insert-album-cover directory-name))
    (let ((file-names (directory-files directory-name t
                                       (bongo-track-file-name-regexp))))
      (when (null file-names)
        (error "Directory contains no playable files"))
      (dolist (file-name file-names)
        (bongo-insert-file file-name))
      (when (and (interactive-p) (not (bongo-buffer-p)))
        (message "Inserted %d files." (length file-names))))))
  
(defun bongo-insert-directory-tree (directory-name)
  "Insert a new track line for each file below DIRECTORY-NAME.
Only insert files that can be played by some backend, as determined
by the file name (see `bongo-track-file-name-regexp').

If `bongo-insert-album-covers' is non-nil, then for each directory
that contains a file whose name is in `bongo-album-cover-file-names',
insert the image in that file before the directory contents.

This function descends each subdirectory of DIRECTORY-NAME recursively."
  (interactive (list (expand-file-name
                      (read-directory-name
                       "Insert directory tree: "
                       default-directory nil t
                       (when (eq major-mode 'dired-mode)
                         (when (file-directory-p (dired-get-filename))
                           (dired-get-filename t)))))))
  (with-bongo-buffer
    (when (not (file-directory-p directory-name))
      (error "File is not a directory: %s" directory-name))
    (when bongo-insert-album-covers
      (bongo-maybe-insert-album-cover directory-name))
    (let ((regexp (bongo-track-file-name-regexp))
          (file-names (directory-files directory-name t "^[^.]")))
      (dolist (file-name file-names)
        (if (file-directory-p file-name)
            (bongo-insert-directory-tree file-name)
          (when (string-match regexp file-name)
            (bongo-insert-file file-name)))))))

(defcustom bongo-gnu-find-program "find"
  "The name of the GNU find executable."
  :type 'string
  :group 'bongo)

(defcustom bongo-gnu-find-extra-arguments
  (when (and (executable-find bongo-gnu-find-program)
             (equal 0 (call-process bongo-gnu-find-program nil nil nil
                                    "-regextype" "emacs" "-prune")))
    '("-regextype" "emacs"))
  "Extra arguments to pass to GNU find."
  :type '(repeat string)
  :group 'bongo)

(defun bongo-insert-directory-tree-using-find (directory-name)
  "Insert a new track line for each file below DIRECTORY-NAME.
Only insert files that can be played by some backend, as determined
by the file name (see `bongo-track-file-name-regexp').

This function descends each subdirectory of DIRECTORY-NAME recursively,
using `bongo-gnu-find-program' to find the files."
  (interactive (list (expand-file-name
                      (read-directory-name
                       "Insert directory tree: "
                       default-directory nil t
                       (when (eq major-mode 'dired-mode)
                         (when (file-directory-p (dired-get-filename))
                           (dired-get-filename t)))))))
  (let ((file-count 0)
        (bongo-buffer (bongo-buffer)))
    (with-temp-buffer
      (apply 'call-process bongo-gnu-find-program nil t nil
             directory-name "-type" "f"
             "-iregex" (bongo-track-file-name-regexp)
             bongo-gnu-find-extra-arguments)
      (sort-lines nil (point-min) (point-max))
      (goto-char (point-min))
      (while (not (eobp))
        (with-current-buffer bongo-buffer
          (bongo-insert-file (buffer-substring (point) (point-at-eol))))
        (setq file-count (1+ file-count))
        (forward-line)))
    (when (zerop file-count)
      (error "Directory tree contains no playable files"))
    (when (and (interactive-p) (not (bongo-buffer-p)))
      (message "Inserted %d files." file-count))))


;;;; Collapsing and expanding

(defun bongo-collapse (&optional skip)
  "Collapse the section below the header line at point.
If point is not on a header line, collapse the section at point.

If SKIP is nil, leave point at the header line.
If SKIP is non-nil, leave point at the first object line after the section.
If point is neither on a header line nor in a section,
  and SKIP is nil, signal an error.
If called interactively, SKIP is always non-nil."
  (interactive "p")
  (when line-move-ignore-invisible
    (bongo-skip-invisible))
  (unless (bongo-header-line-p)
    (bongo-backward-up-section))
  (let ((line-move-ignore-invisible nil))
    (bongo-line-set-property 'bongo-collapsed t)
    (bongo-redisplay-line)
    (let ((end (bongo-point-after-section)))
      (forward-line 1)
      (let ((inhibit-read-only t))
        (put-text-property (point) end 'invisible t))
      (if (not skip)
          (forward-line -1)
        (goto-char end)
        (bongo-maybe-forward-object-line)))))

(defun bongo-expand (&optional skip)
  "Expand the section below the header line at point.

If SKIP is nil, leave point at the header line.
If SKIP is non-nil, leave point at the first object line after the section.
If point is not on a header line or the section below the header line
  is not collapsed, and SKIP is nil, signal an error.
If called interactively, SKIP is always non-nil."
  (interactive "p")
  (when line-move-ignore-invisible
    (bongo-skip-invisible))
  (unless (bongo-header-line-p)
    (error "Not on a header line"))
  (unless (bongo-collapsed-header-line-p)
    (error "This section is not collapsed"))
  (let ((start (point))
        (inhibit-read-only t)
        (line-move-ignore-invisible nil))
    (bongo-line-remove-property 'bongo-collapsed)
    (bongo-redisplay-line)
    (put-text-property (bongo-point-after-line)
                       (bongo-point-after-section)
                       'invisible nil)
    (let ((indentation (bongo-line-indentation)))
      (bongo-forward-object-line)
      (while (and (> (bongo-line-indentation) indentation)
                  (not (eobp)))
        (if (bongo-collapsed-header-line-p)
            (bongo-collapse t)
          (bongo-forward-object-line))))
    (when (not skip)
      (goto-char start))))

(defun bongo-toggle-collapsed ()
  "Collapse or expand the section below the header line at point.
If the section is expanded, collapse it; if it is collapsed, expand it.
If point is not on a header line, signal an error."
  (interactive)
  (unless (bongo-header-line-p)
    (error "Not on a header line"))
  (when line-move-ignore-invisible
    (bongo-skip-invisible))
  (if (bongo-collapsed-header-line-p)
      (bongo-expand)
    (bongo-collapse)))


;;;; Joining and splitting

(defun bongo-join-region (beg end &optional fields)
  "Join all tracks between BEG and END by externalizing FIELDS.
If FIELDS is nil, externalize all common fields between BEG and END.
If there are no common fields, or the fields are already external,
  or the region contains less than two lines, signal an error.
This function creates a new header if necessary."
  (interactive "r")
  (let ((line-move-ignore-invisible nil))
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
      (bongo-insert-header))))

(defun bongo-join (&optional skip)
  "Join the fields around point or in the region.
If Transient Mark mode is enabled, delegate to `bongo-join-region'.
Otherwise, find all common fields at point, and join all tracks around
point that share those fields.  (See `bongo-common-fields-at-point'.)

If SKIP is nil, leave point at the newly created header line.
If SKIP is non-nil, leave point at the first object line after
  the newly created section.
If there are no common fields at point and SKIP is nil, signal an error.
If called interactively, SKIP is always non-nil."
  (interactive "p")
  (if (and transient-mark-mode mark-active)
      (bongo-join-region (region-beginning) (region-end))
    (when line-move-ignore-invisible
      (bongo-skip-invisible))
    (let* ((line-move-ignore-invisible nil)
           (fields (bongo-common-fields-at-point)))
      (if (null fields)
          (if (not skip)
              (error "No common fields at point")
            (unless (bongo-last-object-line-p)
              (bongo-forward-object-line)))
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
          (setq after (move-marker (make-marker) after))
          (bongo-join-region before after fields)
          (when skip (goto-char after))
          (move-marker after nil)
          (bongo-maybe-forward-object-line))))))

(defun bongo-split (&optional skip)
  "Split the section below the header line at point.
If point is not on a header line, split the section at point.

If SKIP is nil, leave point at the first object in the section.
If SKIP is non-nil, leave point at the first object after the section.
If point is neither on a header line nor in a section,
  and SKIP is nil, signal an error.
If called interactively, SKIP is always non-nil."
  (interactive "p")
  (when (not (bongo-object-line-p))
    (bongo-backward-object-line))
  (when (not (bongo-object-line-p))
    (error "No bongo object here"))
  (when (and (bongo-track-line-p) (bongo-line-indented-p))
    (bongo-backward-up-section))
  (if (bongo-track-line-p)
      (if (not skip)
          (error "No section here")
        (unless (bongo-last-object-line-p)
          (bongo-forward-object-line)))
    (when (bongo-collapsed-header-line-p)
      (bongo-expand))
    (when line-move-ignore-invisible
      (bongo-skip-invisible))
    (let ((line-move-ignore-invisible nil))
      (let ((fields (bongo-line-internal-fields))
            (end (move-marker (make-marker) (bongo-point-after-section))))
        (bongo-delete-line)
        (let ((start (point)))
          (while (< (point) end)
            (let* ((previous (point))
                   (old-external (bongo-line-external-fields))
                   (new-external (set-difference old-external fields)))
              (bongo-forward-section)
              (bongo-line-set-external-fields new-external previous)))
          (move-marker end nil)
          (when (not skip)
            (goto-char start))
          (bongo-maybe-forward-object-line))))))


;;;; Displaying

(defun bongo-redisplay-line ()
  "Redisplay the current line, preserving semantic text properties."
  (let ((inhibit-read-only t)
        (indentation (bongo-line-indentation))
        (infoset (bongo-line-internal-infoset))
        (header (bongo-header-line-p))
        (collapsed (bongo-collapsed-header-line-p))
        (properties (bongo-line-get-semantic-properties)))
    (save-excursion
      (bongo-clear-line)
      (dotimes (_ indentation) (insert bongo-indentation-string))
      (let ((content (bongo-format-infoset infoset)))
        (insert (if (not header) content
                  (bongo-format-header content collapsed))))
      (bongo-line-set-properties properties)
;;;       (bongo-line-set-property 'face (if header 'bongo-header
;;;                                        'bongo-track))
      )))

(defun bongo-redisplay (&optional arg)
  "Redisplay every line in the entire buffer.
With prefix argument, remove all indentation and headers."
  (interactive "P")
  (unless (bongo-buffer-p)
    (error "Not a Bongo buffer"))
  (save-excursion
    (message "Rendering buffer...")
    (goto-char (point-min))
    (bongo-maybe-forward-object-line)
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
    (message "Rendering buffer...done")))

(defun bongo-recenter ()
  "Move point to the currently playing track and recenter.
If no track is currently playing, just call `recenter'."
  (interactive)
  (let ((original-window (selected-window))
        (window (get-buffer-window (bongo-playlist-buffer) t)))
    (when window
      (select-window window)
      (bongo-goto-point (or (bongo-active-track-position)
                            (bongo-queued-track-position)))
      (recenter)
      (select-window original-window))))

(defun bongo-parse-time (time)
  "Return the total number of seconds of TIME, or nil.
If TIME is a string of the form [[H:]M:]S[.F], where H, M, S and F
  may each be any number of digits, return 3600H + 60M + S.F.
If TIME is any other string, return nil."
  (when (string-match
         (concat "^\\(?:\\(?:\\([0-9]+\\):\\)?\\([0-9]+\\):\\)?"
                 "\\([0-9]+\\(?:\\.[0-9]+\\)?\\)$") time)
    (let ((hours (match-string 1 time))
          (minutes (match-string 2 time))
          (seconds (match-string 3 time)))
      (+ (if (null hours) 0
           (* 3600 (string-to-number hours)))
         (if (null minutes) 0
           (* 60 (string-to-number minutes)))
         (if (null seconds) 0
           (string-to-number seconds))))))

(defun bongo-format-seconds (n)
  "Return a user-friendly string representing N seconds.
If N < 3600, the string will look like \"mm:ss\".
Otherwise, it will look like \"hhh:mm:ss\", the first field
being arbitrarily long."
  (setq n (floor n))
  (let ((hours (/ n 3600))
        (minutes (% (/ n 60) 60))
        (seconds (% n 60)))
    (let ((result (format "%02d:%02d" minutes seconds)))
      (unless (zerop hours)
        (setq result (format "%d:%s" hours result)))
      result)))

(defun bongo-show (&optional insert-flag)
  "Display what Bongo is playing in the minibuffer.
If INSERT-FLAG (prefix argument if interactive) is non-nil,
  insert the description at point.
Return the description string."
  (interactive "P")
  (let (player infoset)
    (with-bongo-playlist-buffer
      (setq player bongo-player)
      (let ((position (bongo-active-track-position))
            (line-move-ignore-invisible nil))
        (when (null position)
          (error "No track is currently playing"))
        (setq infoset (bongo-line-infoset position))))
    (let ((elapsed-time (when player (bongo-player-elapsed-time player)))
          (total-time (when player (bongo-player-total-time player)))
          (description (bongo-format-infoset infoset)))
      (let ((string (if (not (and elapsed-time total-time))
                        description
                      (format "%s [%s/%s]" description
                              (bongo-format-seconds elapsed-time)
                              (bongo-format-seconds total-time)))))
        (prog1 string
          (if insert-flag
              (insert string)
            (message string)))))))


;;;; Killing/yanking

(defun bongo-kill-line ()
  "In Bongo, kill the current line.
If the current line is a header line, copy the whole section.
See also `bongo-copy-line-as-kill'."
  (interactive)
  (let ((inhibit-read-only t))
    (cond
     ((bongo-track-line-p)
      (when (bongo-active-track-line-p)
        (bongo-unset-active-track))
      (let ((kill-whole-line t))
        (beginning-of-line)
        (kill-line)))
     ((bongo-header-line-p)
      (save-excursion
        (beginning-of-line)
        (when line-move-ignore-invisible
          (bongo-skip-invisible))
        (kill-region (point) (bongo-point-after-section))))
     (t
      (kill-line)))
;;;     (when (bongo-redundant-header-at-point-p)
;;;       (bongo-delete-line))
    ))

(defun bongo-copy-line-as-kill (&optional skip)
  "In Bongo, save the current line as if killed, but don't kill it.
If the current line is a header line, copy the whole section.
If SKIP is non-nil, then move point to the next object line.
See also `bongo-kill-line'."
  (interactive "p")
  (when (eq last-command 'bongo-copy-line-as-kill)
    (append-next-kill))
  (copy-region-as-kill (bongo-point-before-line)
                       (if (bongo-header-line-p)
                           (bongo-point-after-section)
                         (bongo-point-after-line)))
  (when skip
    (bongo-forward-section)))

(defun bongo-kill-region (&optional beg end)
  "In Bongo, kill the lines between point and mark.
If the region ends inside a section, kill the whole section.
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

(defun bongo-clean-up-after-insertion (beg end)
  (let ((end (move-marker (make-marker) end))
        (line-move-ignore-invisible nil))
    (save-excursion
      (goto-char beg)
      (when (not (bongo-object-line-p))
        (bongo-forward-object-line))
      (while (and (< (point) end))
        (let ((player (bongo-line-get-property 'bongo-player)))
          (when player
            (if (and (eq player bongo-player)
                     (null (bongo-active-track-position)))
                (bongo-set-active-track (point-at-bol))
              (bongo-line-remove-property 'bongo-player))))
        (bongo-forward-object-line))
      ;; These headers will stay if they are needed,
      ;; or disappear automatically otherwise.
      (goto-char beg)
      (bongo-insert-header)
      (goto-char end)
      (unless (bongo-last-object-line-p)
        (bongo-insert-header))
      ;; In case the upper header does disappear,
      ;; we need to merge backwards to connect.
      (when (not (bongo-object-line-p))
        (bongo-forward-object-line))
      (when (< (point) end)
        (bongo-externalize-fields))
      (move-marker end nil))))

(defun bongo-insert (text)
  (let ((inhibit-read-only t))
    (beginning-of-line)
    (when line-move-ignore-invisible
      (bongo-skip-invisible))
    (let ((beg (point)))
      (insert text)
      (bongo-clean-up-after-insertion beg (point)))))

(defun bongo-yank (&optional arg)
  "In Bongo, reinsert the last sequence of killed lines.
See `yank'."
  (interactive "P")
  (let ((inhibit-read-only t))
    (beginning-of-line)
    (when line-move-ignore-invisible
      (bongo-skip-invisible))
    (yank arg)
    (bongo-clean-up-after-insertion
     (region-beginning) (region-end))))

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

(defun bongo-insert-enqueue-line (&optional skip)
  "Insert the current line immediately after the track being played.
In Bongo Playlist mode, insert into the current buffer.
In Bongo Library mode, insert into the playlist buffer
  \(see `bongo-playlist-buffer').
If point is on a section header, insert the whole section."
  (interactive "d")
  (let ((text (buffer-substring (bongo-point-before-line)
                                (if (bongo-header-line-p)
                                    (bongo-point-after-section)
                                  (bongo-point-after-line))))
        position)
    (with-bongo-playlist-buffer
      (save-excursion
        (if (bongo-active-track-position)
            (bongo-goto-point (bongo-point-after-line
                               (bongo-active-track-position)))
          (goto-char (point-min)))
        (setq position (point))
        (bongo-insert text)))
    (prog1 position
      (when skip
        (bongo-forward-section))
      (when (bongo-library-buffer-p)
        (let ((original-window (selected-window)))
          (select-window (display-buffer (bongo-playlist-buffer)))
          (goto-char position)
          (recenter)
          (select-window original-window))))))

(defun bongo-append-enqueue-line (&optional skip)
  "Append the current line to the Bongo playlist buffer.
In Bongo Playlist mode, append to the current buffer.
In Bongo Library mode, append to the playlist buffer
  \(see `bongo-playlist-buffer').
If point is on a section header, append the whole section."
  (interactive "d")
  (let ((text (buffer-substring (bongo-point-before-line)
                                (if (bongo-header-line-p)
                                    (bongo-point-after-section)
                                  (bongo-point-after-line))))
        position)
    (with-bongo-playlist-buffer
      (save-excursion
        (goto-char (point-max))
        (setq position (point))
        (bongo-insert text)))
    (prog1 position
      (when skip
        (bongo-forward-section))
      (when (bongo-library-buffer-p)
        (let ((original-window (selected-window)))
          (select-window (display-buffer (bongo-playlist-buffer)))
          (goto-char position)
          (recenter)
          (select-window original-window))))))


;;;; Serializing buffers

;;; (defun bongo-parse-header ()
;;;   "Parse a Bongo header.
;;; Leave point immediately after the header."
;;;   (let (pairs)
;;;     (while (looking-at "\\([a-zA-Z-]+\\): \\(.*\\)")
;;;       (setq pairs (cons (cons (intern (downcase (match-string 1)))
;;;                               (match-string 2))
;;;                         pairs))
;;;       (forward-line))
;;;     pairs))

(defvar bongo-library-magic-string
  "Content-Type: application/x-bongo-library\n"
  "The string that identifies serialized Bongo library buffers.
This string will inserted when serializing library buffers.")

(defvar bongo-playlist-magic-string
  "Content-Type: application/x-bongo-playlist\n"
  "The string that identifies serialized Bongo playlist buffers.
This string will inserted when serializing playlist buffers.")

(defvar bongo-magic-regexp
  "Content-Type: application/x-bongo\\(-library\\|-playlist\\)?\n"
  "Regexp that matches at the start of serialized Bongo buffers.
Any file whose beginning matches this regexp will be assumed to
be a serialized Bongo buffer.")

(add-to-list 'auto-mode-alist
             '("\\.bongo\\(-library\\)?$" . bongo-library-mode))
(add-to-list 'auto-mode-alist
             '("\\.bongo-playlist$" . bongo-playlist-mode))

(add-to-list 'format-alist
             (list 'bongo "Serialized Bongo library buffer"
                   bongo-library-magic-string 'bongo-decode
                   'bongo-encode t nil))
(add-to-list 'format-alist
             (list 'bongo "Serialized Bongo playlist buffer"
                   bongo-playlist-magic-string 'bongo-decode
                   'bongo-encode t nil))

(defun bongo-decode (beg end)
  "Convert a serialized Bongo buffer into the real thing.
Modify region between BEG and END; return the new end of the region.

This function is used when loading Bongo buffers from files.
You probably do not want to call this function directly;
instead, use high-level functions such as `find-file'."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (unless (looking-at bongo-magic-regexp)
        (error "Unrecognized format"))
      (let ((mode-tag (if (looking-at bongo-playlist-magic-string)
                          "-*- Bongo-Playlist -*-"
                        "-*- Bongo-Library -*-")))
        (bongo-delete-line)
        (while (not (eobp))
          (let ((start (point)))
            (condition-case nil
                (let ((object (read (current-buffer))))
                  (delete-region start (point))
                  (if (stringp object) (insert object)
                    (error "Unexpected object: %s" object)))
              (end-of-file
               (delete-region start (point-max))))))
        (save-restriction
          (widen)
          (goto-char beg)
          (let ((case-fold-match t))
            (when (and (bobp) (not (looking-at ".* -\\*- *\\w+ *-\\*-")))
              (insert-char #x20 (- fill-column (length mode-tag) 1))
              (insert mode-tag "\n")
              (forward-line -1)
              (put-text-property (point-at-bol) (point-at-eol)
                                 'face 'bongo-comment))))
        (point-max)))))

(defvar bongo-line-serializable-properties
  (list 'bongo-file-name 'bongo-fields 'bongo-external-fields
        'bongo-header 'bongo-collapsed)
  "List of serializable text properties used in Bongo buffers.
When a bongo Buffer is written to a file, only serializable text
properties are saved; all other text properties are discarded.")

(defun bongo-encode (beg end buffer)
  "Serialize part of a Bongo buffer into a flat representation.
Modify region between BEG and END; return the new end of the region.

This function is used when writing Bongo buffers to files.
You probably do not want to call this function directly;
instead, use high-level functions such as `save-buffer'."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (bongo-ensure-final-newline)
      (goto-char (point-min))
      (when (re-search-forward " *-\\*- *\\w+ *-\\*-\n?" nil t)
        (replace-match ""))
      (goto-char (point-min))
      (insert (if (bongo-playlist-buffer-p)
                  bongo-playlist-magic-string
                bongo-library-magic-string) "\n")
      (while (not (eobp))
        (bongo-keep-text-properties (point-at-bol) (point-at-eol)
                                    '(face display))
        (bongo-keep-text-properties (point-at-eol) (1+ (point-at-eol))
                                    bongo-line-serializable-properties)
        (prin1 (bongo-extract-line) (current-buffer))
        (insert "\n")))))


;;;; Typical user entry points

(defun bongo-mode ()
  "Common parent major mode for Bongo buffers.
Do not use this mode directly.  Instead, use Bongo Playlist mode (see
`bongo-playlist-mode') or Bongo Library mode (see `bongo-library-mode').

\\{bongo-mode-map}"
  (kill-all-local-variables)
  (set (make-local-variable 'forward-sexp-function)
       'bongo-forward-section)
  (use-local-map bongo-mode-map)
  (setq buffer-read-only t)
  (setq major-mode 'bongo-mode)
  (setq mode-name "Bongo")
  (setq buffer-file-format '(bongo))
  (run-mode-hooks 'bongo-mode-hook))

(defvar bongo-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "\C-m" 'bongo-dwim)
    (define-key map [mouse-2] 'bongo-mouse-dwim)
    (define-key map "q" 'bongo-quit)
    (define-key map "Q" 'bury-buffer)
    (define-key map "g" 'bongo-redisplay)
    (define-key map "h" 'bongo-switch-buffers)
    (define-key map "l" 'bongo-recenter)
    (define-key map "j" 'bongo-join)
    (define-key map "J" 'bongo-split)
    (define-key map "c" 'bongo-collapse)
    (define-key map "C" 'bongo-expand)
    (define-key map "k" 'bongo-copy-line-as-kill)
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
    (define-key map "p" 'bongo-play-previous)
    (define-key map "n" 'bongo-play-next)
    (define-key map "R" 'bongo-replay-current)
    (define-key map "r" 'bongo-play-random)
    (define-key map "E" 'bongo-insert-enqueue-line)
    (define-key map "e" 'bongo-append-enqueue-line)
    (define-key map "N" 'bongo-perform-next-action)
    (define-key map "f" 'bongo-seek-forward)
    (define-key map "b" 'bongo-seek-backward)
    (define-key map "S" 'bongo-seek-to)
    (when (require 'volume nil t)
      (define-key map "v" 'volume))
    (define-key map "if" 'bongo-insert-file)
    (define-key map "id" 'bongo-insert-directory)
    (define-key map "it" 'bongo-insert-directory-tree)
    map)
  "Keymap used in Bongo mode buffers.")

(define-derived-mode bongo-library-mode bongo-mode
  "Bongo Library"
  "Major mode for Bongo library buffers.
Contrary to playlist buffers, library buffers cannot directly
play tracks.  Instead, they are used to insert tracks into
playlist buffers.

\\{bongo-library-mode-map}"
    :group 'bongo :syntax-table nil :abbrev-table nil)

(define-derived-mode bongo-playlist-mode bongo-mode
  "Bongo Playlist"
  "Major mode for Bongo playlist buffers.
Playlist buffers are the most important feature of Bongo, as
they have the ability to play tracks.

\\{bongo-playlist-mode-map}"
  :group 'bongo :syntax-table nil :abbrev-table nil
  (setq bongo-active-track-marker (make-marker))
  (setq bongo-queued-track-marker (make-marker))
  (setq bongo-queued-track-arrow-marker (make-marker))
  (add-to-list 'overlay-arrow-variable-list
               'bongo-active-track-marker)
  (add-to-list 'overlay-arrow-variable-list
               'bongo-queued-track-arrow-marker))

(define-key bongo-playlist-mode-map "P" 'bongo-play-line)

(defmacro with-bongo-buffer (&rest body)
  "Execute the forms in BODY in some Bongo buffer.
The value returned is the value of the last form in BODY.

If the current buffer is not a Bongo buffer, switch to
the buffer returned by the function `bongo-buffer'."
  (declare (indent 0) (debug t))
  `(with-current-buffer
       (if (bongo-buffer-p)
           (current-buffer)
         (bongo-buffer))
     ,@body))

(defmacro with-bongo-library-buffer (&rest body)
  "Execute the forms in BODY in some Bongo library buffer.
The value returned is the value of the last form in BODY.

If the current buffer is not a library buffer, switch to
the buffer returned by the function `bongo-library-buffer'."
  (declare (indent 0) (debug t))
  `(with-current-buffer
       (if (bongo-library-buffer-p)
           (current-buffer)
         (bongo-library-buffer))
     ,@body))

(defmacro with-bongo-playlist-buffer (&rest body)
  "Execute the forms in BODY in some Bongo playlist buffer.
The value returned is the value of the last form in BODY.

If the current buffer is not a playlist buffer, switch to
the buffer returned by the function `bongo-playlist-buffer'."
  (declare (indent 0) (debug t))
  `(with-current-buffer
       (if (bongo-playlist-buffer-p)
           (current-buffer)
         (bongo-playlist-buffer))
     ,@body))

(defvar bongo-library-buffer nil
  "The default Bongo library buffer, or nil.
Bongo library commands will operate on this buffer when
executed from buffers that are not in Bongo Library mode.

This variable overrides `bongo-default-library-buffer-name'.
See the function `bongo-library-buffer'.")

(defvar bongo-playlist-buffer nil
  "The default Bongo playlist buffer, or nil.
Bongo playlist commands will operate on this buffer when
executed from buffers that are not in Bongo Playlist mode.

This variable overrides `bongo-default-playlist-buffer-name'.
See the function `bongo-playlist-buffer'.")

(defun bongo-buffer-p (&optional buffer)
  "Return non-nil if BUFFER is in Bongo mode.
If BUFFER is nil, test the current buffer instead."
  (with-current-buffer (or buffer (current-buffer))
    (or (eq 'bongo-playlist-mode major-mode)
        (eq 'bongo-library-mode major-mode))))

(defun bongo-library-buffer-p (&optional buffer)
  "Return non-nil if BUFFER is in Bongo Library mode.
If BUFFER is nil, test the current buffer instead."
  (with-current-buffer (or buffer (current-buffer))
    (eq 'bongo-library-mode major-mode)))

(defun bongo-playlist-buffer-p (&optional buffer)
  "Return non-nil if BUFFER is in Bongo Playlist mode.
If BUFFER is nil, test the current buffer instead."
  (with-current-buffer (or buffer (current-buffer))
    (eq 'bongo-playlist-mode major-mode)))

(defun bongo-buffer ()
  "Return an interesting Bongo buffer, creating it if necessary.
First try to find an existing Bongo buffer, using a strategy
similar to `bongo-library-buffer' and `bongo-playlist-buffer'.
If no Bongo buffer is found, create a new one.
This function respects the value of `bongo-prefer-library-buffers'."
  (or (if bongo-prefer-library-buffers
          (or bongo-library-buffer
              bongo-playlist-buffer)
        (or bongo-playlist-buffer
            bongo-library-buffer))
      (let (result (list (buffer-list)))
        (while (and list (not result))
          (when (bongo-buffer-p (car list))
            (setq result (car list)))
          (setq list (cdr list)))
        result)
      (let ((buffer (get-buffer-create
                     (if bongo-prefer-library-buffers
                         bongo-default-library-buffer-name
                       bongo-default-playlist-buffer-name))))
        (prog1 buffer
          (with-current-buffer buffer
            (if bongo-prefer-library-buffers
              (bongo-library-mode)
              (bongo-playlist-mode)))))))

(defun bongo-playlist-buffer ()
  "Return a Bongo playlist buffer.

If the variable `bongo-playlist-buffer' is non-nil, return that.
Otherwise, return the most recently selected Bongo playlist buffer.
If there is no buffer in Bongo Playlist mode, create one.  The name of
the new buffer will be the value of `bongo-default-playlist-buffer-name'."
  (or bongo-playlist-buffer
      (let (result (list (buffer-list)))
        (while (and list (not result))
          (when (bongo-playlist-buffer-p (car list))
            (setq result (car list)))
          (setq list (cdr list)))
        result)
      (let ((buffer (get-buffer-create
                     bongo-default-playlist-buffer-name)))
        (prog1 buffer
          (with-current-buffer buffer
            (bongo-playlist-mode))))))

(defun bongo-library-buffer ()
  "Return a Bongo library buffer.

If the variable `bongo-library-buffer' is non-nil, return that.
Otherwise, return the most recently selected Bongo library buffer.
If there is no buffer in Bongo Library mode, create one.  The name of
the new buffer will be the value of `bongo-default-library-buffer-name'."
  (or bongo-library-buffer
      (let (result (list (buffer-list)))
        (while (and list (not result))
          (when (bongo-library-buffer-p (car list))
            (setq result (car list)))
          (setq list (cdr list)))
        result)
      (let ((buffer (get-buffer-create
                     bongo-default-library-buffer-name)))
        (prog1 buffer
          (with-current-buffer buffer
            (bongo-library-mode))))))

(defun bongo-playlist ()
  "Switch to a Bongo playlist buffer.
See `bongo-playlist-buffer'."
  (interactive)
  (switch-to-buffer (bongo-playlist-buffer)))

(defun bongo-library ()
  "Switch to a Bongo library buffer.
See the function `bongo-library-buffer'."
  (interactive)
  (switch-to-buffer (bongo-library-buffer)))

(defvar bongo-stored-window-configuration nil
  "This is used by `bongo' and `bongo-quit'.")

(defun bongo-quit ()
  "Quit Bongo by selecting another buffer.
In addition, delete all windows except one.

This function stores the current window configuration in
`bongo-stored-window-configuration', which is used by \\[bongo]."
  (interactive)
  (setq bongo-stored-window-configuration
        (current-window-configuration))
  (delete-other-windows)
  (let ((buffer (current-buffer)) (count 0))
    (while (and (bongo-buffer-p buffer) (< count 10))
      (setq buffer (other-buffer buffer) count (+ count 1)))
    (switch-to-buffer buffer)))

(defun bongo-switch-buffers (&optional other-window)
  "Switch from a Bongo playlist to a Bongo library, or vice versa.
If OTHER-WINDOW (prefix argument if interactive) is non-nil,
  display the other buffer in another window."
  (interactive "P")
  (with-bongo-buffer
    (let* ((buffer (if (bongo-library-buffer-p)
                       (bongo-playlist-buffer)
                    (bongo-library-buffer)))
           (window (get-buffer-window buffer t)))
      (if window
          (select-window window)
        (if other-window
            (pop-to-buffer buffer)
          (switch-to-buffer buffer))))))

(defun bongo (&optional prefix-argument)
  "Switch to a Bongo buffer.
See the function `bongo-buffer'."
  (interactive "P")
  (when bongo-stored-window-configuration
    (set-window-configuration bongo-stored-window-configuration))
  (unless (bongo-buffer-p)
    (switch-to-buffer (bongo-buffer))))

(provide 'bongo)

;;; Local Variables:
;;; coding: utf-8
;;; time-stamp-format: "%:b %:d, %:y"
;;; time-stamp-start: ";; Updated: "
;;; time-stamp-end: "$"
;;; End:
