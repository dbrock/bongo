;;; bongo.el --- play music with Emacs

;; Copyright (C) 2005-2010, 2014  Daniel Brockman <daniel@brockman.se>
;; Copyright (C) 2006-2007  Daniel Jensen <daniel@bigwalter.net>
;; Copyright (C) 2005  Lars Öhrman <larohr@gmail.com>
;; Copyright (C) 2011  Jürgen Hötzel <juergen@archlinux.org>
;; Copyright (C) 1998, 2000-2005  Free Software Foundation, Inc.

;; Version: 1.1
;; Package-Requires: ((cl-lib "0.5") (emacs "24.1"))

;; This file is part of Bongo.

;; Bongo is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; Bongo is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with Bongo (see the file `COPYING'); if not,
;; write to the Free Software Foundation, 51 Franklin Street,
;; Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; Bongo is a flexible and usable media player for GNU Emacs.
;; For detailed documentation see the projects README file at
;; https://github.com/dbrock/bongo/

;;; Code:

(eval-when-compile
  (require 'rx))

(require 'dired)                        ; Required for dired integration
(require 'volume nil 'no-error)         ; Required for adjusting volume

(require 'pcase)
(require 'cl-lib)

;; We try to load this library so that we can later decide
;; whether to enable Bongo Last.fm mode by default.
(require 'lastfm-submit nil 'no-error)

(declare-function w32-get-clipboard-data "w32select.c")

(defgroup bongo nil
  "Buffer-oriented media player."
  :prefix "bongo-"
  :group 'multimedia
  :group 'applications)


;;;; Macro definitions

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

(defmacro with-temp-bongo-playlist-buffer (&rest body)
  "Execute the forms in BODY in a temporary Bongo playlist buffer.
The value returned is the value of the last form in BODY."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (bongo-playlist-mode)
     ,@body))

(defmacro with-temp-bongo-library-buffer (&rest body)
  "Execute the forms in BODY in a temporary Bongo library buffer.
The value returned is the value of the last form in BODY."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (bongo-library-mode)
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

(defmacro with-point-at-bongo-track (point &rest body)
  "Execute BODY with point at the Bongo track line at POINT.
If there is no track at POINT, use the next track line.
If there is no next track line, signal an error."
  (declare (indent 1) (debug t))
  `(progn
     (push-mark)
     (bongo-goto-point ,point)
     (when line-move-ignore-invisible
       (bongo-skip-invisible))
     (let ((line-move-ignore-invisible nil))
       (when (not (bongo-track-line-p))
         (bongo-goto-point (or (bongo-point-at-next-track-line)
                               (error "No track at point"))))
       ,@body)))

(defvar bongo-player-start-imminent nil
  "If non-nil, Bongo is just about to start playing some track.
For example, this will be non-nil while switching tracks.
See `with-imminent-bongo-player-start'.")

(defvar bongo-deferred-status-indicator-updates nil
  "List of deferred Bongo status indicator updates.
Entries are of the form (FUNCTION . ARGUMENTS).
When `bongo-player-start-imminent' is non-nil, all status
indicator updates should be deferred to prevent flicker.")

(defmacro with-imminent-bongo-player-start (&rest body)
  "Execute BODY with `bongo-player-start-imminent' bound to t.
Afterwards, unless `bongo-player-start-imminent' was already bound
to a non-nil value, perform all deferred status indicator updates.
See `bongo-deferred-status-indicator-updates'."
  (declare (indent 0) (debug t))
  `(progn
     (let ((bongo-player-start-imminent t))
       ,@body)
     (unless bongo-player-start-imminent
       (dolist (entry (nreverse bongo-deferred-status-indicator-updates))
         (apply (car entry) (cdr entry)))
       (setq bongo-deferred-status-indicator-updates nil))))

(defmacro bongo-ignore-movement-errors (&rest body)
  "Execute BODY; if a Bongo movement error occurs, return nil.
Otherwise, return the value of the last form in BODY."
  (declare (indent 0) (debug t))
  `(condition-case nil
       (progn ,@body)
     (bongo-movement-error nil)))

(defmacro bongo-until (test &rest body)
  "If TEST yields nil, evaluate BODY... and repeat.
The order of execution is thus TEST, BODY..., TEST, BODY..., TEST,
  and so on, until TEST returns non-nil.
Return the final value of TEST.

\(fn TEST BODY...)"
  (declare (indent 1) (debug t))
  (let ((result (cl-gensym)))
    `(let (,result)
       (while (unless (setq ,result ,test)
                (prog1 t
                  ,@body)))
       ,result)))



;;;; Global variables

(defvar bongo-title nil
  "Bound dynamically to the formatted album title or nil.")

(defvar bongo-year nil
  "Bound dynamically to the formatted album year or nil.")

(defvar bongo-album nil
  "Bound dynamically to the contents of album field or nil.")

(defvar bongo-infoset nil
  "Bound dynamically to the whole infoset or nil.")

(defvar bongo-target nil
  "Short for `bongo-infoset-formatting-target'.")

(defvar bongo-line nil
  "Short for `bongo-infoset-formatting-target-line'.")

(defvar bongo-index nil
  "Bound dynamically is bound to the formatted track index or nil.")

(defvar bongo-length nil
  "Bound dynamically is bound to the formatted track length or nil.")

(defvar bongo-track nil
  "Bound dynamically is bound to the contents of the `track' field.")

(defvar bongo-sprinkle-mode nil
  "Non-nil if `bongo-sprinke-mode' is on.

Declaring here so that it can be used in function appearing before the
minor mode definition.")


;;;; Commonly-used variables

(defvar bongo-backends '()
  "List of symbols naming available Bongo player backends.
The backend data for each entry is stored in the `bongo-backend'
property of the backend name symbol.")

(defvar bongo-backend-matchers '()
  "List of Bongo player backend matchers.
See `bongo-custom-backend-matchers' for more information.")

(defvar bongo-player nil
  "The currently active player for this buffer, or nil.
This variable is only used in Bongo mode buffers.")
(make-variable-buffer-local 'bongo-player)

(defvar bongo-seek-buffer nil
  "The current interactive Bongo Seek buffer, or nil.")

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



;;;; Customization variables

(defcustom bongo-enabled-backends nil
  "Dummy declaration."
  :group 'bongo)

(defcustom bongo-custom-backend-matchers nil
  "Dummy declaration."
  :group 'bongo)

(defun bongo-evaluate-backend-defcustoms ()
  "Define `bongo-enabled-backends' and `bongo-custom-backend-matchers'.
This should be done whenever `bongo-backends' changes, so that
the `defcustom' options can be updated."
  (custom-declare-variable 'bongo-enabled-backends
    `',(apply 'nconc
              (mapcar (lambda (backend-name)
                        (when (executable-find
                               (bongo-backend-program-name
                                (bongo-backend backend-name)))
                          (list backend-name)))
                      bongo-backends))
    "List of names of enabled Bongo player backends.
See `bongo-backends' for a list of available backends."
    :type `(list (set :inline t :format "%v"
                      ,@(mapcar (lambda (backend-name)
                                  `(const :tag ,(bongo-backend-pretty-name
                                                 backend-name)
                                          ,backend-name))
                                bongo-backends)))
    :set (lambda (name value)
           (set-default name value)
           (when (fboundp 'bongo-buffer-p)
             (dolist (buffer (if custom-local-buffer
                                 (list (current-buffer))
                               (buffer-list)))
               (when (bongo-buffer-p buffer)
                 (with-current-buffer buffer
                   (bongo-update-enabled-backends-list))))))
    :group 'bongo)
  (custom-reevaluate-setting 'bongo-enabled-backends)

  (custom-declare-variable 'bongo-custom-backend-matchers nil
    "List of custom Bongo player backend matchers.
Entries are rules of the form (BACKEND-NAME . MATCHER).

BACKEND-NAME is either `ignore' (which tells Bongo to ignore
  the matched files), or a symbol naming the backend to use.
MATCHER specifies which files the rule applies to;
  it is given to `bongo-file-name-matches-p'.

This option overrides `bongo-enabled-backends' in that disabled
backends will still be used if these rules say so.  In addition,
it always takes precedence over `bongo-backend-matchers'.

For example, let's say that you want to use VLC instead of
mpg123 to play MP3 files, use speexdec to play \".speex\" files
in addition to \".spx\" files, and ignore WAV files altogether.
Then you could use the following setting:

   (setq bongo-custom-backend-matchers
         '((vlc local-file \"mp3\")
           (speexdec local-file \"speex\")
           (ignore local-file \"wav\")))"
    :type
    `(repeat
      (cons :format "%v"
            (choice :tag "Backend"
                    (const :tag "Ignore matching files" ignore)
                    ,@(mapcar (lambda (backend-name)
                                (list 'const
                                      :tag (bongo-backend-pretty-name
                                            (bongo-backend backend-name))
                                      backend-name))
                              bongo-backends)
                    (symbol :tag "Other backend"))
            (cons :format "%v"
                  (repeat :tag "File types"
                          :value (local-file)
                          (choice :tag "Type"
                                  (const :tag "Local file" local-file)
                                  (string :tag "\
URI (specify scheme followed by a colon)")))
                  (choice :tag "Matcher"
                          (repeat :tag "File extensions" string)
                          (regexp :tag "File name (regexp)")
                          (function :tag "File name (predicate)")
                          (const :tag "All files" t)))))
    :group 'bongo))

(bongo-evaluate-backend-defcustoms)

(defcustom bongo-default-directory nil
  "Default directory for Bongo buffers, or nil.
If nil, use the value of `default-directory' when the buffer is created."
  :type '(choice (const :tag "None in particular" nil)
                 directory)
  :group 'bongo
  :group 'bongo-file-names)

(defcustom bongo-prefer-library-buffers t
  "If non-nil, prefer library buffers over playlist buffers.
This affects what kind of buffer is created by `\\[bongo]' when there
are no existing Bongo buffers.

Regardless of this setting, you can switch to a specific type of Bongo
buffer using the commands `\\[bongo-library]' and `\\[bongo-playlist]'.
To create a new library or playlist, create a new buffer and then switch to
the right mode using `\\[bongo-library-mode]' or `\\[bongo-playlist-mode]'.

If you set this variable to nil, you can happily use Bongo without ever
seeing a library buffer (unless you create one yourself, of course)."
  :type 'boolean
  :group 'bongo)

(defcustom bongo-display-playlist-after-enqueue t
  "Whether to display the playlist after enqueuing a track."
  :type 'boolean
  :group 'bongo)

(defcustom bongo-mark-played-tracks nil
  "Whether to mark all tracks that have been played.
Tracks marked as played are not selected for random playback.
Enabling Bongo Sprinkle mode sets this variable locally;
  see `bongo-sprinkle-mode' for more about that.
Played tracks are displayed in the face `bongo-played-track'.

To delete all played tracks, use `\\[bongo-flush-playlist]'.
To clear the mark for all tracks, use `\\[bongo-reset-playlist]'."
  :type 'boolean
  :group 'bongo)

(defcustom bongo-confirm-flush-playlist t
  "If non-nil, ask for confirmation before flushing the playlist.
This affects only the command `\\[bongo-flush-playlist]'.
In particular, `\\[bongo-erase-buffer]' and `\\[bongo-delete-played-tracks]'
never ask for confirmation, regardless of the value of this variable."
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
          (function-item :tag "Stop playback"
                         bongo-stop)
          (function-item :tag "Play the next track"
                         bongo-play-next-or-stop)
          (function-item :tag "Play the same track again"
                         bongo-replay-current)
          (function-item :tag "Play the previous track"
                         bongo-play-previous-or-stop)
          (function-item :tag "Play a random track"
                         bongo-play-random-or-stop))
  :group 'bongo)
(make-variable-buffer-local 'bongo-next-action)

(defvar bongo-stored-next-action nil
  "The old value of `bongo-next-action'.
This variable is used by `bongo-play-queued'.")

(defgroup bongo-file-names nil
  "File names and file name parsing in Bongo.
If your files do not have nice names but do have nice tags, then you
can use the `tree-from-tags.rb' tool (shipped with Bongo) to create a
hierarchy of nicely-named links to your files."
  :group 'bongo)

(defcustom bongo-file-name-field-separator " - "
  "String used to split track file names into fields.
For example, if your tracks are named like this,

   Frank Morton - 2004 - Frank Morton - 01 - Pojken på Tallbacksvägen.ogg

and your file name field separator is \" - \" (which is the default),
then the fields are \"Frank Morton\", \"2004\", \"Frank Morton\", \"01\",
and \"Pojken på Tallbacksvägen\".

When the the fields of a track's file name have been extracted,
they are used to build an infoset.

This is used by `bongo-default-infoset-from-file-name'."
  :type 'string
  :group 'bongo-file-names)

(defcustom bongo-file-name-album-year-regexp
  "\\`\\([0-9]\\{4\\}\\|'?[0-9]\\{2\\}\\)\\'"
  "Regexp matching album years.
This is used by `bongo-default-infoset-from-file-name'."
  :type 'regexp
  :group 'bongo-file-names)

(defcustom bongo-file-name-track-index-regexp "\\`[0-9]+\\'"
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

(defcustom bongo-update-references-to-renamed-files 'ask
  "Whether to search all Bongo buffers after renaming a file.
If nil, never search through any buffers after renaming a file.
If `ask', prompt the user every time.
If any other value, always perform the search.

You can rename a file from Bongo using `bongo-rename-line'."
  :type '(choice (const :tag "Never" nil)
                 (const :tag "Ask" ask)
                 (other :tag "Always" t))
  :group 'bongo
  :group 'bongo-file-names)

(defun bongo-format-string (format)
  "Short for (apply 'concat (mapcar 'eval FORMAT))."
  (apply 'concat (mapcar 'eval format)))

(defgroup bongo-display nil
  "Display of Bongo playlist and library buffers."
  :group 'bongo)

(defcustom bongo-field-separator
  (if (and (fboundp 'char-displayable-p)
           (char-displayable-p ?—))
      " —— " " -- ")
  "String used to separate field values in track descriptions.
This is used by the function `bongo-default-format-field'."
  :type '(choice (const :tag " —— (Unicode dashes)" " —— ")
                 (const :tag " -- (ASCII dashes)" " -- ")
                 string)
  :group 'bongo-display)

(defcustom bongo-insert-album-covers nil
  "Whether to put album cover images into Bongo buffers.
See also `bongo-album-cover-file-names'."
  :type 'boolean
  :link '(custom-group-link bongo-file-names)
  :group 'bongo-display)

(defcustom bongo-album-cover-size 200
  "Size in pixels to which album cover art should be scaled."
  :type 'integer
  :group 'bongo-display)

(defcustom bongo-join-inserted-tracks t
  "Whether to automatically join newly-inserted tracks.
This is done by repeatedly running `bongo-join'."
  :type 'boolean
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

(defcustom bongo-album-function
  'bongo-default-album
  "Function for displaying albums in Bongo.
Value is a funcion, evaluating to a string or nil.
When the function is evaluated,
 - `bongo-title' is bound to the formatted album title;
 - `bongo-year' is bound to the formatted album year or nil;
 - `bongo-album' is bound to the contents of the `album' field;
 - `bongo-infoset' is bound to the whole infoset;
 - `bongo-target' is short for `bongo-infoset-formatting-target';
 - `bongo-line' is short for `bongo-infoset-formatting-target-line'.

This variable is used by the function `bongo-default-format-field'."
  :type 'function
  :options '(bongo-default-album)
  :group 'bongo-display)

(defun bongo-default-album ()
  (concat
   bongo-title
   (when bongo-year
      (concat " (" bongo-year ")"))))

(defcustom bongo-display-track-lengths t
  "Whether to display track lengths in Bongo playlist buffers.
See also `bongo-track-length-column'."
  :type 'boolean
  :group 'bongo-display)

(defcustom bongo-track-length-column 60
  "Column at which to align track lengths in Bongo playlist buffers.
See also `bongo-display-track-lengths'."
  :type 'integer
  :group 'bongo-display)

(defcustom bongo-track-function
  'bongo-default-track
  "Function for displaying tracks in Bongo.
Value is a function, evaluating to a string or nil.
When the function is evaluated,
 - `bongo-title' is bound to the formatted track title;
 - `bongo-index' is bound to the formatted track index or nil;
 - `bongo-length' is bound to the formatted track length or nil;
 - `bongo-track' is bound to the contents of the `track' field;
 - `bongo-infoset' is bound to the whole infoset;
 - `bongo-target' is short for `bongo-infoset-formatting-target';
 - `bongo-line' is short for `bongo-infoset-formatting-target-line'.

This variable is used by the function `bongo-default-format-field'."
  :type 'function
  :options '()
  :group 'bongo-display)

(defcustom bongo-indentation-string "  "
  "String prefixed to lines once for each level of indentation."
  :type 'string
  :group 'bongo-display)

(defun bongo-default-track ()
  (concat
   (when bongo-index
     (concat bongo-index ". "))
   bongo-title
   (when (and bongo-display-track-lengths bongo-length
	      (bongo-playlist-buffer-p bongo-target))
     (concat (let ((other-fields-width
		    (with-temp-buffer
		      (insert (bongo-format-infoset
			       `((track (length . nil)
					,@bongo-track)
				 ,@bongo-infoset)))
		      (current-column)))
		   (indentation-width
		    (* (length bongo-indentation-string)
		       (bongo-line-indentation bongo-line))))
	       (make-string (max 0 (- bongo-track-length-column
				      indentation-width
				      other-fields-width)) 32))
	     " " bongo-length))))

(defcustom bongo-track-length-function
  'bongo-default-track-length
  "Function for displaying track lengths in Bongo.
Value is a Function, evaluating to a string or nil.
When the function is evaluated,
 - `bongo-length' is bound to the length of the track in seconds;
 - `bongo-track' is bound to the contents of the `track' field;
 - `bongo-infoset' is bound to the whole infoset;
 - `bongo-target' is short for `bongo-infoset-formatting-target';
 - `bongo-line' is short for `bongo-infoset-formatting-target-line'."
  :type 'function
  :options '(bongo-default-track-length)
  :group 'bongo-display)

(defun bongo-default-track-length ()
  (concat "[" (bongo-format-seconds bongo-length) "]"))

(defcustom bongo-action-format
  '("Action: " bongo-action-description)
  "Template for displaying action tracks in Bongo.
Value is a list of expressions, each evaluating to a string or nil.
The values of the expressions are concatenated.
When the expressions are evaluated,
 - `bongo-action-description' is bound to the action description;
 - `bongo-action-expression' is bound to the action expression;
 - `bongo-target' is short for `bongo-infoset-formatting-target';
 - `bongo-line' is short for `bongo-infoset-formatting-target-line'."
  :type '(repeat sexp)
  :group 'bongo-display)

(defcustom bongo-stream-format
  '((or bongo-uri-title bongo-stream-name bongo-uri)
    (when bongo-stream-genre
      (concat " (" bongo-stream-genre ")"))
    (when bongo-stream-part-title
      (concat ": " bongo-stream-part-title)))
  "Template for displaying stream tracks in Bongo.
Value is a list of expressions, each evaluating to a string or nil.
The values of the expressions are concatenated.
When the expressions are evaluated,
 - `bongo-uri' is bound to the URI of the stream;
 - `bongo-uri-title' is bound to the URI title or nil;
 - `bongo-stream-name' is bound to the stream name or nil;
 - `bongo-stream-genre' is bound to the stream genre or nil;
 - `bongo-stream-part-title' is bound to the stream part title;
 - `bongo-target' is short for `bongo-infoset-formatting-target';
 - `bongo-line' is short for `bongo-infoset-formatting-target-line'."
  :type '(repeat sexp)
  :group 'bongo-display)

(defcustom bongo-track-line-format
  '((bongo-format-infoset bongo-internal-infoset))
  "Template for displaying track lines in Bongo.
Value is a list of expressions, each evaluating to a string or nil.
The values of the expressions are concatenated.
When the expressions are evaluated,
 - `bongo-internal-infoset' is bound to the internal infoset;
 - `bongo-infoset' is bound to the whole infoset;
 - `bongo-target' is short for `bongo-infoset-formatting-target';
 - `bongo-line' is short for `bongo-infoset-formatting-target-line'."
  :type '(repeat sexp)
  :group 'bongo-display)

(defcustom bongo-display-header-icons t
  "Whether to display icons for header lines in Bongo buffers."
  :type 'boolean
  :group 'bongo-display)

(defcustom bongo-expanded-header-icon "expanded-header-icon.png"
  "File name of icon to use for header lines of expanded sections.
If nil, do not use any icon."
  :type '(choice file (const :tag "None" nil))
  :group 'bongo-display)

(defcustom bongo-collapsed-header-icon "collapsed-header-icon.png"
  "File name of icon to use for header lines of collapsed sections.
If nil, do not use any icon."
  :type '(choice file (const :tag "None" nil))
  :group 'bongo-display)

(defcustom bongo-expanded-header-format "[%s]"
  "Template for displaying header lines for expanded sections.
%s means the header line content.
This variable is only used when not displaying header icons."
  :type 'string
  :group 'bongo-display)

(defcustom bongo-collapsed-header-format "[%s ...]"
  "Template for displaying header lines for collapsed sections.
%s means the header line content.
This variable is only used when not displaying header icons."
  :type 'string
  :group 'bongo-display)

(defcustom bongo-section-header-line-format
  '((if (and bongo-display-header-icons (display-images-p))
        (bongo-format-infoset bongo-internal-infoset)
      (format (if bongo-collapsed
                  bongo-collapsed-header-format
                bongo-expanded-header-format)
              (bongo-format-infoset bongo-internal-infoset))))
  "Template for displaying header lines in Bongo.
Value is a list of expressions, each evaluating to a string or nil.
The values of the expressions are concatenated.
When the expressions are evaluated,
 - `bongo-internal-infoset' is bound to the internal infoset;
 - `bongo-infoset' is bound to the whole infoset;
 - `bongo-collapsed' is non-nil if the section is collapsed;
 - `bongo-target' is short for `bongo-infoset-formatting-target';
 - `bongo-line' is short for `bongo-infoset-formatting-target-line'.
The values of the expressions are concatenated."
  :type '(repeat sexp)
  :group 'bongo-display)

(defgroup bongo-track-icons nil
  "Display of track icons in Bongo buffers."
  :group 'bongo-display)

(defcustom bongo-display-track-icons t
  "Whether to display icons for track lines in Bongo buffers."
  :type 'boolean
  :group 'bongo-track-icons
  :group 'bongo-display)

(defcustom bongo-unknown-local-file-track-icon
  "unknown-local-file-track-icon.png"
  "File name of icon to use for unknown local file tracks."
  :type '(choice file (const :tag "None" nil))
  :group 'bongo-track-icons)

(defcustom bongo-local-audio-file-track-icon
  "local-audio-file-track-icon.png"
  "File name of icon to use for local audio file tracks.
If nil, use the same icon as for unknown local file tracks."
  :type '(choice file (const :tag "\
Same as for unknown local file tracks" nil))
  :group 'bongo-track-icons)

(defcustom bongo-local-video-file-track-icon
  "local-video-file-track-icon.png"
  "File name of icon to use for video file tracks.
If nil, use the same icon as for unknown file tracks."
  :type '(choice file (const :tag "\
Same as for unknown local file tracks" nil))
  :group 'bongo-track-icons)

(defcustom bongo-audio-cd-track-icon "audio-cd-track-icon.png"
  "File name of icon to use for audio CD tracks.
If nil, use the same icon as for local audio file tracks."
  :type '(choice file (const :tag "\
Same as for local audio file tracks" nil))
  :group 'bongo-track-icons)

(defcustom bongo-uri-track-icon "uri-track-icon.png"
  "File name of icon to use for URI tracks.
If nil, do not use any icon at all."
  :type '(choice file (const :tag "None" nil))
  :group 'bongo-track-icons)

(defcustom bongo-action-track-icon "action-track-icon.png"
  "File name of icon to use for action tracks.
If nil, do not use any icon at all."
  :type '(choice file (const :tag "None" nil))
  :group 'bongo-track-icons)

(defcustom bongo-currently-playing-track-icon nil
  "File name of icon to use for currently playing tracks.
If nil, use the same icon as for other tracks."
  :type '(choice file (const :tag "Same as for other tracks" nil))
  :group 'bongo-track-icons)

(defcustom bongo-played-track-icon nil
  "File name of icon to use for played track lines.
If nil, use the same icon as for unplayed tracks."
  :type '(choice file (const :tag "Same as for other tracks" nil))
  :group 'bongo-track-icons)

(defconst bongo-images-directory
  (or (expand-file-name "images" (file-name-directory load-file-name))
      (error "Please use `load-file' to load bongo.el")))

(defun bongo-find-image (file-name &optional face)
  (let ((image-load-path (cons bongo-images-directory
                               (and (boundp 'image-load-path)
                                    image-load-path))))
    (find-image
     (list (list :ascent 'center
                 :file file-name
                 :type (image-type-from-file-name file-name)
                 :foreground (face-foreground
                              (or face 'default) nil 'default)
                 :background (face-background
                              (or face 'default) nil 'default))))))

(defun bongo-make-image-string (image)
  "Return a string with IMAGE in its `display' property."
  (propertize " " 'display image))

(defun bongo-make-image-placeholder-string (image)
  "Return a blank string taking up as much space as IMAGE would."
  (let ((size (image-size image t)))
    (propertize " " 'display `(space :width (,(car size))
                                     :height (,(cdr size))))))

(defun bongo-line-icon-string ()
  "Return the string to use as an icon for the current line."
  (let ((file-name
         (cond ((and (bongo-track-line-p) bongo-display-track-icons)
                (cond ((and (bongo-currently-playing-track-line-p)
                            bongo-currently-playing-track-icon)
                       bongo-currently-playing-track-icon)
                      ((and (bongo-played-track-line-p)
                            bongo-played-track-icon)
                       bongo-played-track-icon)
                      ((bongo-audio-cd-track-line-p)
                       bongo-audio-cd-track-icon)
                      ((bongo-uri-track-line-p)
                       bongo-uri-track-icon)
                      ((bongo-action-track-line-p)
                       bongo-action-track-icon)
                      ((bongo-local-audio-file-track-line-p)
                       (or bongo-local-audio-file-track-icon
                           bongo-unknown-local-file-track-icon))
                      ((bongo-local-video-file-track-line-p)
                       (or bongo-local-video-file-track-icon
                           bongo-unknown-local-file-track-icon))
                      ((bongo-local-file-track-line-p)
                       bongo-unknown-local-file-track-icon)))
               ((and (bongo-header-line-p) bongo-display-header-icons)
                (if (bongo-collapsed-header-line-p)
                    bongo-collapsed-header-icon
                  bongo-expanded-header-icon)))))
    (when file-name
      (bongo-make-image-string (bongo-find-image file-name)))))

(defgroup bongo-header-line nil
  "Display of header lines in Bongo playlist buffers."
  :group 'bongo
  :group 'bongo-display)

(defcustom bongo-header-line-mode t
  "Whether to display header lines in Bongo playlist buffers."
  :type 'boolean
  :initialize 'custom-initialize-default
  :set 'custom-set-minor-mode
  :group 'bongo-header-line)

(defcustom bongo-header-line-playing-string "Playing:"
  "String to display in the header line when a track is playing."
  :type 'string
  :group 'bongo-header-line)

(defcustom bongo-header-line-paused-string "Paused: "
  "String to display in the header line when a track is paused."
  :type 'string
  :group 'bongo-header-line)

(defun bongo-header-line-playback-status ()
  "Return the string to use for header line playback status."
  (when (bongo-playing-p)
    (if (bongo-paused-p)
        bongo-header-line-paused-string
      bongo-header-line-playing-string)))

(defcustom bongo-header-line-function
  'bongo-default-header-line-function
  "Function for Bongo playlist header lines."
  :type 'function
  :options '(bongo-default-header-line-function)
  :group 'bongo-header-line)

(defun bongo-default-header-line-function ()
  "Return String of Playback status and Track description"
  (concat (bongo-header-line-playback-status) " "
    (bongo-formatted-infoset)))

(defvar bongo-header-line-string nil
  "Bongo header line string.
Value is derived from `bongo-header-line-function'.
The name of this variable should go in `header-line-format'.")
(make-variable-buffer-local 'bongo-header-line-string)
(put 'bongo-header-line-string 'risky-local-variable t)

(defun bongo-update-header-line-string (&rest dummy)
  "Update `bongo-header-line-string' using `bongo-header-line-format'.
If Bongo is not playing anything, set the header line string to nil.
Accept DUMMY arguments to ease hook usage."
  (when (bongo-buffer-p)
    (let ((new-header-line-format header-line-format)
          (new-bongo-header-line-string bongo-header-line-string))
      (when (null new-header-line-format)
        (setq new-header-line-format '("")))
      (if bongo-header-line-mode
          (add-to-list 'new-header-line-format
            'bongo-header-line-string t)
        (setq new-header-line-format
              (remq 'bongo-header-line-string new-header-line-format)))
      (setq new-bongo-header-line-string
            (when (bongo-playing-p)
              (funcall bongo-header-line-function)))
      (when (or (equal new-header-line-format '(""))
                (and (equal new-header-line-format
                            '("" bongo-header-line-string))
                     (null new-bongo-header-line-string)))
        (setq new-header-line-format nil))
      (if (not bongo-player-start-imminent)
          (setq header-line-format new-header-line-format
                bongo-header-line-string new-bongo-header-line-string)
        (add-to-list 'bongo-deferred-status-indicator-updates
          (list 'set 'header-line-format
                new-header-line-format))
        (add-to-list 'bongo-deferred-status-indicator-updates
          (list 'set 'bongo-header-line-string
                new-bongo-header-line-string))))))

(defun bongo-header-line-mode (argument &optional called-interactively-p)
  "Toggle display of Bongo mode line indicator on or off.
With ARGUMENT equal to `toggle', or interactively
  with no prefix argument, toggle the mode.
With zero or negative ARGUMENT, turn the mode off.
With any other ARGUMENT, turn the mode on.
When called interactively, CALLED-INTERACTIVELY-P is non-nil."
  ;; Use `toggle' rather than (if mode 0 1) so that using
  ;; `repeat-command' still does the toggling correctly.
  (interactive (list (or current-prefix-arg 'toggle)
                     'called-interactively-p))
  (setq bongo-header-line-mode
        (if (eq argument 'toggle)
            (not bongo-header-line-mode)
          (> (prefix-numeric-value argument) 0)))
  (when called-interactively-p
    (customize-mark-as-set 'bongo-header-line-mode))
  (when (called-interactively-p 'interactive)
    (message "Bongo header line mode %s."
             (if bongo-header-line-mode
                 "enabled" "disabled")))
  bongo-header-line-mode)

(defgroup bongo-mode-line nil
  "Display of Bongo mode line indicators."
  :group 'bongo
  :group 'bongo-display)

(defcustom bongo-display-playback-mode-indicator t
  "Display playback mode indicators in playlist buffer mode lines.
These indicate which playback mode is in effect.
However, nothing is shown for normal in-order playback.
To change playback mode, try (for example) `\\[bongo-random-playback-mode]'."
  :type 'boolean
  :group 'bongo-mode-line
  :group 'bongo-display)

(defcustom bongo-mode-line-indicator-mode t
  "Display a Bongo playback status indicator in the global mode line.
See `bongo-mode-line-indicator-format'."
  :type 'boolean
  :initialize 'custom-initialize-default
  :set 'custom-set-minor-mode
  :group 'bongo-mode-line)

(defvar bongo-player-times-last-updated 0
  "Time in seconds when player output was last updated because of
  player times changed")

(defun bongo-hyphen-padded-mode-line-p ()
  "Return non-nil if the mode line is padded with hyphens.
That is, if `mode-line-format' ends with a string ending with \"%-\"."
  (and (listp mode-line-format)
       (let ((last (car (last mode-line-format))))
         (and (stringp last)
              (string-match "%-$" last)))))

(defun bongo-mode-line-pad-string ()
  "Return the string to use for padding in the mode line.
This is either \"-\" or \" \", depending on the return value of
the function `bongo-hyphen-padded-mode-line-p'."
  (if (bongo-hyphen-padded-mode-line-p) "-" " "))

(defcustom bongo-mode-line-indicator-function
  'bongo-default-mode-line-indicator-function
  "Function for the Bongo mode line indicator."
  :type 'function
  :options '(bongo-default-mode-line-indicator-function)
  :group 'bongo-mode-line)

(defun bongo-default-mode-line-indicator-function ()
  (concat (bongo-mode-line-pad-string)
	  (when (bongo-hyphen-padded-mode-line-p) "[")
	  (bongo-mode-line-volume-button)
      (when (featurep 'volume) " ")
	  (bongo-mode-line-backward/previous-button)
	  (bongo-mode-line-pause/resume-button)
	  (bongo-mode-line-start/stop-button)
	  (bongo-mode-line-forward/next-button)
	  (when (bongo-playing-p) " ")
	  (when (bongo-playing-p)
	    (cond ((and (bongo-elapsed-time) (bongo-total-time))
		   (format "%d%%" (/ (* 100.0 (bongo-elapsed-time))
				     (bongo-total-time))))
		  ((bongo-elapsed-time)
		   (bongo-format-seconds (bongo-elapsed-time)))))
	  (when (bongo-hyphen-padded-mode-line-p) "]")
	  (bongo-mode-line-pad-string)
	  (when (bongo-hyphen-padded-mode-line-p)
	    (bongo-mode-line-pad-string))))

(defcustom bongo-mode-line-indicator-parent 'global-mode-string
  "List variable in which to put the Bongo mode line indicator.
Value is the name of a variable whose value is a list.
If nil, `bongo-mode-line-indicator-string' is not put anywhere."
  :type '(choice (const :tag "None" nil) variable)
  :group 'bongo-mode-line)

(defcustom bongo-mode-line-icon-color
  (face-foreground 'mode-line nil 'default)
  "Color of Bongo mode line icons."
  :type 'string
  :group 'bongo-mode-line)

(defcustom bongo-mode-line-playing-string "Playing"
  "Fallback string for the Bongo [Pause] button icon."
  :type 'string
  :group 'bongo-mode-line)

(defcustom bongo-mode-line-paused-string "Paused"
  "Fallback string for the Bongo [Resume] button icon."
  :type 'string
  :group 'bongo-mode-line)

(defvar bongo-mode-line-pause-icon-18
  '`(image :type xpm :ascent center :data ,(concat "/* XPM */
static char *pause_18[] = {
/* width  height  number of colors  number of characters per pixel */
\" 18     18      2                 1\",
/* colors */
\"# c " bongo-mode-line-icon-color  "\",
\". c None\",
/* pixels */
\"..................\",
\"..................\",
\"..................\",
\"...####....####...\",
\"...####....####...\",
\"...####....####...\",
\"...####....####...\",
\"...####....####...\",
\"...####....####...\",
\"...####....####...\",
\"...####....####...\",
\"...####....####...\",
\"...####....####...\",
\"...####....####...\",
\"...####....####...\",
\"..................\",
\"..................\",
\"..................\"
};"))
  "Bongo [Pause] button icon (18 pixels tall).")

(defvar bongo-mode-line-pause-icon-11
  '`(image :type xpm :ascent center :data ,(concat "/* XPM */
static char *pause_11[] = {
/* width  height  number of colors  number of characters per pixel */
\" 10     11      2                 1\",
/* colors */
\"# c " bongo-mode-line-icon-color  "\",
\". c None\",
/* pixels */
\"..........\",
\"..........\",
\"..##..##..\",
\"..##..##..\",
\"..##..##..\",
\"..##..##..\",
\"..##..##..\",
\"..##..##..\",
\"..##..##..\",
\"..........\",
\"..........\"};"))
  "Bongo [Pause] button icon (11 pixels tall).")

(defvar bongo-mode-line-resume-icon-18
  '`(image :type xpm :ascent center :data ,(concat "/* XPM */
static char *resume_18[] = {
/* width  height  number of colors  number of characters per pixel */
\" 18     18      2                 1\",
/* colors */
\"# c " bongo-mode-line-icon-color  "\",
\". c None\",
/* pixels */
\"..................\",
\"..................\",
\"......##..........\",
\"......###.........\",
\"......####........\",
\"......#####.......\",
\"......######......\",
\"......#######.....\",
\"......########....\",
\"......########....\",
\"......#######.....\",
\"......######......\",
\"......#####.......\",
\"......####........\",
\"......###.........\",
\"......##..........\",
\"..................\",
\"..................\"
};"))
  "Bongo [Resume] button icon (18 pixels tall).")

(defvar bongo-mode-line-resume-icon-11
  '`(image :type xpm :ascent center :data ,(concat "/* XPM */
static char *resume_11[] = {
/* width  height  number of colors  number of characters per pixel */
\" 10     11      2                 1\",
/* colors */
\"# c " bongo-mode-line-icon-color  "\",
\". c None\",
/* pixels */
\"..........\",
\"...#......\",
\"...##.....\",
\"...###....\",
\"...####...\",
\"...#####..\",
\"...####...\",
\"...###....\",
\"...##.....\",
\"...#......\",
\"..........\"
};"))
  "Bongo [Resume] button icon (11 pixels tall).")

(defvar bongo-mode-line-stop-icon-18
  '`(image :type xpm :ascent center :data ,(concat "/* XPM */
static char *stop_18[] = {
/* width  height  number of colors  number of characters per pixel */
\" 18     18      2                 1\",
/* colors */
\"# c " bongo-mode-line-icon-color  "\",
\". c None\",
/* pixels */
\"..................\",
\"..................\",
\"..................\",
\"...############...\",
\"...############...\",
\"...############...\",
\"...############...\",
\"...############...\",
\"...############...\",
\"...############...\",
\"...############...\",
\"...############...\",
\"...############...\",
\"...############...\",
\"...############...\",
\"..................\",
\"..................\",
\"..................\"
};"))
  "Bongo [Stop] button icon (18 pixels tall).")

(defvar bongo-mode-line-stop-icon-11
  '`(image :type xpm :ascent center :data ,(concat "/* XPM */
static char *stop_11[] = {
/* width  height  number of colors  number of characters per pixel */
\" 10     11      2                 1\",
/* colors */
\"# c " bongo-mode-line-icon-color  "\",
\". c None\",
/* pixels */
\"..........\",
\"..........\",
\"..######..\",
\"..######..\",
\"..######..\",
\"..######..\",
\"..######..\",
\"..######..\",
\"..######..\",
\"..........\",
\"..........\"};"))
  "Bongo [Stop] button icon (11 pixels tall).")

(defvar bongo-mode-line-previous-icon-18
  '`(image :type xpm :ascent center :data ,(concat "/* XPM */
static char *previous_18[] = {
/* width  height  number of colors  number of characters per pixel */
\" 20     18      2                 1\",
/* colors */
\"# c " bongo-mode-line-icon-color  "\",
\". c None\",
/* pixels */
\"....................\",
\"....................\",
\"....................\",
\"....................\",
\"....###.......##....\",
\"....###......###....\",
\"....###.....####....\",
\"....###....#####....\",
\"....###...######....\",
\"....###...######....\",
\"....###....#####....\",
\"....###.....####....\",
\"....###......###....\",
\"....###.......##....\",
\"....................\",
\"....................\",
\"....................\",
\"....................\"
};"))
  "Bongo [Previous] button icon (18 pixels tall)")

(defvar bongo-mode-line-previous-icon-11
  '`(image :type xpm :ascent center :data ,(concat "/* XPM */
static char *previous_11[] = {
/* width  height  number of colors  number of characters per pixel */
\" 11     11      2                 1\",
/* colors */
\"# c " bongo-mode-line-icon-color  "\",
\". c None\",
/* pixels */
\"...........\",
\"...........\",
\"..##....#..\",
\"..##...##..\",
\"..##..###..\",
\"..##.####..\",
\"..##..###..\",
\"..##...##..\",
\"..##....#..\",
\"...........\",
\"...........\"
};"))
  "Bongo [Previous] button icon (11 pixels tall).")

(defvar bongo-mode-line-next-icon-18
  '`(image :type xpm :ascent center :data ,(concat "/* XPM */
static char *next_18[] = {
/* width  height  number of colors  number of characters per pixel */
\" 20     18      2                 1\",
/* colors */
\"# c " bongo-mode-line-icon-color  "\",
\". c None\",
/* pixels */
\"....................\",
\"....................\",
\"....................\",
\"....................\",
\"....##.......###....\",
\"....###......###....\",
\"....####.....###....\",
\"....#####....###....\",
\"....######...###....\",
\"....######...###....\",
\"....#####....###....\",
\"....####.....###....\",
\"....###......###....\",
\"....##.......###....\",
\"....................\",
\"....................\",
\"....................\",
\"....................\"
};"))
  "Bongo [Next] button icon (18 pixels tall)")

(defvar bongo-mode-line-next-icon-11
  '`(image :type xpm :ascent center :data ,(concat "/* XPM */
static char *next_11[] = {
/* width  height  number of colors  number of characters per pixel */
\" 11     11      2                 1\",
/* colors */
\"# c " bongo-mode-line-icon-color  "\",
\". c None\",
/* pixels */
\"...........\",
\"...........\",
\"..#....##..\",
\"..##...##..\",
\"..###..##..\",
\"..####.##..\",
\"..###..##..\",
\"..##...##..\",
\"..#....##..\",
\"...........\",
\"...........\"
};"))
  "Bongo [Next] button icon (11 pixels tall).")

(defvar bongo-mode-line-backward-icon-18
  '`(image :type xpm :ascent center :data ,(concat "/* XPM */
static char *backward_18[] = {
/* width  height  number of colors  number of characters per pixel */
\" 20     18      2                 1\",
/* colors */
\"# c " bongo-mode-line-icon-color  "\",
\". c None\",
/* pixels */
\"....................\",
\"....................\",
\"....................\",
\"....................\",
\"....................\",
\"......##......##....\",
\".....###.....###....\",
\"....####....####....\",
\"...#####...#####....\",
\"...#####...#####....\",
\"....####....####....\",
\".....###.....###....\",
\"......##......##....\",
\"....................\",
\"....................\",
\"....................\",
\"....................\",
\"....................\"
};"))
  "Bongo [Rewind] button icon (18 pixels tall).")

(defvar bongo-mode-line-backward-icon-11
  '`(image :type xpm :ascent center :data ,(concat "/* XPM */
static char *backward_11[] = {
/* width  height  number of colors  number of characters per pixel */
\" 11     11      2                 1\",
/* colors */
\"# c " bongo-mode-line-icon-color  "\",
\". c None\",
/* pixels */
\"...........\",
\"...........\",
\"...........\",
\"....#...#..\",
\"...##..##..\",
\"..###.###..\",
\"...##..##..\",
\"....#...#..\",
\"...........\",
\"...........\",
\"...........\"
};"))
  "Bongo [Rewind] button icon (11 pixels tall).")

(defvar bongo-mode-line-forward-icon-18
  '`(image :type xpm :ascent center :data ,(concat "/* XPM */
static char *forward_18[] = {
/* width  height  number of colors  number of characters per pixel */
\" 20     18      2                 1\",
/* colors */
\"# c " bongo-mode-line-icon-color  "\",
\". c None\",
/* pixels */
\"....................\",
\"....................\",
\"....................\",
\"....................\",
\"....................\",
\"....##......##......\",
\"....###.....###.....\",
\"....####....####....\",
\"....#####...#####...\",
\"....#####...#####...\",
\"....####....####....\",
\"....###.....###.....\",
\"....##......##......\",
\"....................\",
\"....................\",
\"....................\",
\"....................\",
\"....................\"
};"))
  "Bongo [Fast-forward] button icon (18 pixels tall).")

(defvar bongo-mode-line-forward-icon-11
  '`(image :type xpm :ascent center :data ,(concat "/* XPM */
static char *forward_11[] = {
/* width  height  number of colors  number of characters per pixel */
\" 11     11      2                 1\",
/* colors */
\"# c " bongo-mode-line-icon-color  "\",
\". c None\",
/* pixels */
\"...........\",
\"...........\",
\"...........\",
\"..#...#....\",
\"..##..##...\",
\"..###.###..\",
\"..##..##...\",
\"..#...#....\",
\"...........\",
\"...........\",
\"...........\"
};"))
  "Bongo [Fast-forward] button icon (11 pixels tall).")

(defvar bongo-mode-line-volume-icon-18
  '`(image :type xpm :ascent center :data ,(concat "/* XPM */
static char *volume_18[] = {
/* width  height  number of colors  number of characters per pixel */
\" 20     18      2                 1\",
/* colors */
\"# c " bongo-mode-line-icon-color  "\",
\". c None\",
/* pixels */
\"....................\",
\"....................\",
\"........#...........\",
\".......##....##.....\",
\"......###.....##....\",
\".....####...#..##...\",
\"..#######...##.##...\",
\"..#######.#..##.##..\",
\"..#######..#.##.##..\",
\"..#######..#.##.##..\",
\"..#######.#..##.##..\",
\"..#######...##.##...\",
\".....####...#..##...\",
\"......###.....##....\",
\".......##....##.....\",
\"........#...........\",
\"....................\",
\"....................\"
};"))
  "Bongo [Volume] button icon (18 pixels tall).")

(defvar bongo-mode-line-volume-icon-11
  '`(image :type xpm :ascent center :data ,(concat "/* XPM */
static char *volume_11[] = {
/* width  height  number of colors  number of characters per pixel */
\" 11     11      2                 1\",
/* colors */
\"# c " bongo-mode-line-icon-color  "\",
\". c None\",
/* pixels */
\"...........\",
\"...........\",
\".....#.....\",
\"....##.#...\",
\"..####..#..\",
\"..#####.#..\",
\"..####..#..\",
\"....##.#...\",
\".....#.....\",
\"...........\",
\"...........\"
};"))
  "Bongo [Volume] button icon (11 pixels tall).")

(defvar bongo-mode-line-start-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map [mode-line mouse-1]
        (lambda (e)
          (interactive "e")
          (bongo-start)))
      (define-key map [mode-line mouse-3]
        (lambda (e)
          (interactive "e")
          (if (bongo-buffer-p)
              (bongo-switch-buffers)
            (bongo)))))))

(defvar bongo-mode-line-pause/resume-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map [mode-line mouse-1]
        (lambda (e)
          (interactive "e")
          (bongo-pause/resume)))
      (define-key map [mode-line mouse-3]
        (lambda (e)
          (interactive "e")
          (if (bongo-buffer-p)
              (bongo-switch-buffers)
            (bongo)))))))

(defvar bongo-mode-line-stop-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map [mode-line mouse-1]
        (lambda (e)
          (interactive "e")
          (bongo-stop)))
      (define-key map [mode-line mouse-3]
        (lambda (e)
          (interactive "e")
          (if (bongo-buffer-p)
              (bongo-switch-buffers)
            (bongo)))))))

(defvar bongo-mode-line-previous-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map [mode-line mouse-1]
        (lambda (e)
          (interactive "e")
          (bongo-play-previous)))
      (define-key map [mode-line mouse-3]
        (lambda (e)
          (interactive "e")
          (bongo-play-previous))))))

(defvar bongo-mode-line-next-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map [mode-line mouse-1]
        (lambda (e)
          (interactive "e")
          (bongo-play-next)))
      (define-key map [mode-line mouse-3]
        (lambda (e)
          (interactive "e")
          (bongo-play-next))))))

(defvar bongo-mode-line-backward-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map [mode-line mouse-1]
        (lambda (e)
          (interactive "e")
          (bongo-seek-backward-10)))
      (define-key map [mode-line mouse-3]
        (lambda (e)
          (interactive "e")
          (if (bongo-buffer-p)
              (bongo-switch-buffers)
            (bongo)))))))

(defvar bongo-mode-line-forward-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map [mode-line mouse-1]
        (lambda (e)
          (interactive "e")
          (bongo-seek-forward-10)))
      (define-key map [mode-line mouse-3]
        (lambda (e)
          (interactive "e")
          (if (bongo-buffer-p)
              (bongo-switch-buffers)
            (bongo)))))))

(defvar bongo-mode-line-backward/previous-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map [mode-line mouse-1]
        (lambda (e)
          (interactive "e")
          (bongo-seek-backward-10)))
      (define-key map [mode-line mouse-3]
        (lambda (e)
          (interactive "e")
          (bongo-previous))))))

(defvar bongo-mode-line-forward/next-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map [mode-line mouse-1]
        (lambda (e)
          (interactive "e")
          (bongo-seek-forward-10)))
      (define-key map [mode-line mouse-3]
        (lambda (e)
          (interactive "e")
          (bongo-next))))))

(defvar bongo-mode-line-volume-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map [mode-line mouse-1]
        (lambda (e)
          (interactive "e")
          (if volume-buffer
              (volume-quit)
            (volume))))
      (define-key map [mode-line mouse-4]
        (lambda (e)
          (interactive "e")
          (volume-raise)))
      (define-key map [mode-line mouse-5]
        (lambda (e)
          (interactive "e")
          (volume-lower))))))

(defun bongo-face-height (face-name)
  "Return the height of the font used for FACE-NAME, or nil.
If running without a window system, signal an error."
  (catch 'return
    (aref (or (font-info (or (face-font face-name)
                             (throw 'return nil)))
              (throw 'return nil)) 3)))

(defun bongo-face-width (face-name)
  "Return the width of the font used for FACE-NAME, or nil.
If running without a window system, signal an error."
  (catch 'return
    (aref (or (font-info (or (face-font face-name)
                             (throw 'return nil)))
              (throw 'return nil)) 2)))

(defun bongo-mode-line-icon-size ()
  "Return the size to use for mode line icons."
  (if (>= (or (bongo-face-height 'mode-line) 0) 18) 18 11))

(defun bongo-mode-line-start-button ()
  "Return the string to use as [Start] button in the mode line."
  (when (and window-system (not (bongo-playing-p)))
    (let ((icon-size (bongo-mode-line-icon-size)))
      (concat
       (when (>= emacs-major-version 22)
         (propertize " " 'display '(space :width (1))))
       (propertize " "
                   'display (cond ((= icon-size 18)
                                   (eval bongo-mode-line-resume-icon-18))
                                  ((= icon-size 11)
                                   (eval bongo-mode-line-resume-icon-11)))
                   'help-echo
                   (let ((position (bongo-point-at-current-track-line)))
                     ;; We can't put the <mouse-3> text at
                     ;; the bottom because it'll truncate.
                     (concat "mouse-3: display Bongo buffers\nmouse-1: "
                             (if position
                                 (concat "play "
                                         (bongo-format-infoset
                                          (bongo-line-infoset position)))
                               "start playback")))
                   'local-map bongo-mode-line-start-map
                   'mouse-face 'highlight)
       (when (>= emacs-major-version 22)
         (propertize " " 'display '(space :width (1))))))))

(defun bongo-mode-line-stop-button ()
  "Return the string to use as [Stop] button in the mode line."
  (when (and window-system (bongo-playing-p))
    (let ((icon-size (bongo-mode-line-icon-size)))
      (concat
       (when (>= emacs-major-version 22)
         (propertize " " 'display '(space :width (1))))
       (propertize " "
                   'display (cond ((= icon-size 18)
                                   (eval bongo-mode-line-stop-icon-18))
                                  ((= icon-size 11)
                                   (eval bongo-mode-line-stop-icon-11)))
                   'help-echo (concat "mouse-1: stop playback")
                   'local-map bongo-mode-line-stop-map
                   'mouse-face 'highlight)
       (when (>= emacs-major-version 22)
         (propertize " " 'display '(space :width (1))))))))

(defun bongo-mode-line-start/stop-button ()
  "Return the string to use as [Start] or [Stop] button."
  (or (bongo-mode-line-start-button)
      (bongo-mode-line-stop-button)))

(defun bongo-mode-line-pause/resume-button ()
  "Return the string to use as [Pause] or [Resume] button."
  (when (and (bongo-playing-p) (bongo-pausing-supported-p))
    (if window-system
        (let ((icon-size (bongo-mode-line-icon-size)))
          (concat
           (when (>= emacs-major-version 22)
             (propertize " " 'display '(space :width (1))))
           (propertize
            " "
            'display (if (bongo-paused-p)
                         (cond ((= icon-size 18)
                                (eval bongo-mode-line-resume-icon-18))
                               ((= icon-size 11)
                                (eval bongo-mode-line-resume-icon-11)))
                       (cond ((= icon-size 18)
                              (eval bongo-mode-line-pause-icon-18))
                             ((= icon-size 11)
                              (eval bongo-mode-line-pause-icon-11))))
            'help-echo (concat (if (bongo-paused-p)
                                   "mouse-1: resume "
                                 "mouse-1: pause ")
                               (bongo-format-infoset
                                (bongo-player-infoset bongo-player)))
            'local-map bongo-mode-line-pause/resume-map
            'mouse-face 'highlight)
           (when (>= emacs-major-version 22)
             (propertize " " 'display '(space :width (1))))))
      (if (bongo-paused-p)
          bongo-mode-line-paused-string
        bongo-mode-line-playing-string))))

(defun bongo-mode-line-previous-button ()
  "Return the string to use as [Previous] button in the mode line."
  (when (and window-system (bongo-point-at-current-track-line))
    (let ((icon-size (bongo-mode-line-icon-size)))
      (concat
       (when (>= emacs-major-version 22)
         (propertize " " 'display '(space :width (1))))
       (propertize " "
                   'display (cond ((= icon-size 18)
                                   (eval bongo-mode-line-previous-icon-18))
                                  ((= icon-size 11)
                                   (eval bongo-mode-line-previous-icon-11)))
                   'help-echo
                   (let ((position (bongo-point-at-previous-track-line
                                    (bongo-point-at-current-track-line))))
                     (if position
                         (concat "mouse-1: play "
                                 (bongo-format-infoset
                                  (bongo-line-infoset position)))
                       "No previous track"))
                   'local-map bongo-mode-line-previous-map
                   'mouse-face 'highlight)
       (when (>= emacs-major-version 22)
         (propertize " " 'display '(space :width (1))))))))

(defun bongo-mode-line-next-button ()
  "Return the string to use as [Next] button in the mode line."
  (when (and window-system (bongo-point-at-current-track-line))
    (let ((icon-size (bongo-mode-line-icon-size)))
      (concat
       (when (>= emacs-major-version 22)
         (propertize " " 'display '(space :width (1))))
       (propertize " "
                   'display (cond ((= icon-size 18)
                                   (eval bongo-mode-line-next-icon-18))
                                  ((= icon-size 11)
                                   (eval bongo-mode-line-next-icon-11)))
                   'help-echo
                   (let ((position (bongo-point-at-next-track-line
                                    (bongo-point-at-current-track-line))))
                     (if position
                         (concat "mouse-1: play "
                                 (bongo-format-infoset
                                  (bongo-line-infoset position)))
                       "No next track"))
                   'local-map bongo-mode-line-next-map
                   'mouse-face 'highlight)
       (when (>= emacs-major-version 22)
         (propertize " " 'display '(space :width (1))))))))

(defun bongo-mode-line-backward-button ()
  "Return the string to use as [Backward] button in the mode line."
  (when (and window-system (bongo-playing-p) (bongo-seeking-supported-p))
    (let ((icon-size (bongo-mode-line-icon-size)))
      (concat
       (when (>= emacs-major-version 22)
         (propertize " " 'display '(space :width (1))))
       (propertize " "
                   'display (cond ((= icon-size 18)
                                   (eval bongo-mode-line-backward-icon-18))
                                  ((= icon-size 11)
                                   (eval bongo-mode-line-backward-icon-11)))
                   'help-echo "mouse-1: rewind 10 seconds"
                   'local-map bongo-mode-line-backward-map
                   'mouse-face 'highlight)
       (when (>= emacs-major-version 22)
         (propertize " " 'display '(space :width (1))))))))

(defun bongo-mode-line-forward-button ()
  "Return the string to use as [Forward] button in the mode line."
  (when (and window-system (bongo-playing-p) (bongo-seeking-supported-p))
    (let ((icon-size (bongo-mode-line-icon-size)))
      (concat
       (when (>= emacs-major-version 22)
         (propertize " " 'display '(space :width (1))))
       (propertize " "
                   'display (cond ((= icon-size 18)
                                   (eval bongo-mode-line-forward-icon-18))
                                  ((= icon-size 11)
                                   (eval bongo-mode-line-forward-icon-11)))
                   'help-echo "mouse-1: fast-forward 10 seconds"
                   'local-map bongo-mode-line-forward-map
                   'mouse-face 'highlight)
       (when (>= emacs-major-version 22)
         (propertize " " 'display '(space :width (1))))))))

(defun bongo-mode-line-backward/previous-button ()
  "Return the string to use as [Rewind/Previous] button."
  (when (and window-system
             ;; This condition could be made optional.
             (bongo-playing-p))
    (let ((icon-size (bongo-mode-line-icon-size)))
      (concat
       (when (>= emacs-major-version 22)
         (propertize " " 'display '(space :width (1))))
       (propertize " "
                   'display
                   (if (bongo-playing-p)
                       (cond ((= icon-size 18)
                              (eval bongo-mode-line-backward-icon-18))
                             ((= icon-size 11)
                              (eval bongo-mode-line-backward-icon-11)))
                     (cond ((= icon-size 18)
                            (eval bongo-mode-line-previous-icon-18))
                           ((= icon-size 11)
                            (eval bongo-mode-line-previous-icon-11))))
                   'help-echo
                   (let ((position
                          (and (bongo-point-at-current-track-line)
                               (bongo-point-at-previous-track-line
                                (bongo-point-at-current-track-line)))))
                     (concat (when (and (bongo-playing-p)
                                        (bongo-seeking-supported-p))
                               "mouse-1: rewind 10 seconds")
                             (if position
                                 (concat (if (bongo-playing-p)
                                             "\nmouse-3: "
                                           "mouse-1: ")
                                         "play "
                                         (bongo-format-infoset
                                          (bongo-line-infoset position)))
                               (unless (bongo-playing-p)
                                 "No previous track"))))
                   'local-map (if (bongo-playing-p)
                                  bongo-mode-line-backward/previous-map
                                bongo-mode-line-backward-map)
                   'mouse-face 'highlight)
       (when (>= emacs-major-version 22)
         (propertize " " 'display '(space :width (1))))))))

(defun bongo-mode-line-forward/next-button ()
  "Return the string to use as [Fast-forward/Next] button."
  (when (and window-system
             ;; This condition could be made optional.
             (bongo-playing-p))
    (let ((icon-size (bongo-mode-line-icon-size)))
      (concat
       (when (>= emacs-major-version 22)
         (propertize " " 'display '(space :width (1))))
       (propertize " "
                   'display
                   (if (bongo-playing-p)
                       (cond ((= icon-size 18)
                              (eval bongo-mode-line-forward-icon-18))
                             ((= icon-size 11)
                              (eval bongo-mode-line-forward-icon-11)))
                     (cond ((= icon-size 18)
                            (eval bongo-mode-line-next-icon-18))
                           ((= icon-size 11)
                            (eval bongo-mode-line-next-icon-11))))
                   'help-echo
                   (let ((position
                          (and (bongo-point-at-current-track-line)
                               (bongo-point-at-next-track-line
                                (bongo-point-at-current-track-line)))))
                     (concat (when (and (bongo-playing-p)
                                        (bongo-seeking-supported-p))
                               "mouse-1: fast-forward 10 seconds")
                             (if position
                                 (concat (if (bongo-playing-p)
                                             "\nmouse-3: "
                                           "mouse-1: ")
                                         "play "
                                         (bongo-format-infoset
                                          (bongo-line-infoset position)))
                               (unless (bongo-playing-p)
                                 "No next track"))))
                   'local-map (if (bongo-playing-p)
                                  bongo-mode-line-forward/next-map
                                bongo-mode-line-next-map)
                   'mouse-face 'highlight)
       (when (>= emacs-major-version 22)
         (propertize " " 'display '(space :width (1))))))))

(defun bongo-mode-line-volume-button ()
  "Return the string to use as [Volume] button in the mode line."
  (when (and window-system (featurep 'volume))
    (let ((icon-size (bongo-mode-line-icon-size)))
      (concat
       (when (>= emacs-major-version 22)
         (propertize " " 'display '(space :width (1))))
       (propertize " "
                   'display (cond ((= icon-size 18)
                                   (eval bongo-mode-line-volume-icon-18))
                                  ((= icon-size 11)
                                   (eval bongo-mode-line-volume-icon-11)))
                   'help-echo (concat "mouse-1: open volume control\n"
                                      "mouse-4: raise volume\n"
                                      "mouse-5: lower volume")
                   'local-map bongo-mode-line-volume-map
                   'mouse-face 'highlight)
       (when (>= emacs-major-version 22)
         (propertize " " 'display '(space :width (1))))))))

(defvar bongo-mode-line-indicator-string nil
  "Bongo mode line indicator string.
Value is derived from `bongo-mode-line-indicator-function'.
The name of this variable should go in, e.g., `global-mode-string'.")
(put 'bongo-mode-line-indicator-string 'risky-local-variable t)

(defun bongo-update-mode-line-indicator-string (&rest dummy)
  "Update `bongo-mode-line-indicator-string'.
Evalutate elements of `bongo-mode-line-indicator-format' and store
  the resulting string in `bongo-mode-line-indicator-string'.
Accept DUMMY arguments to ease hook usage."
  (when (bongo-buffer-p)
    (let ((new-value (funcall bongo-mode-line-indicator-function)))
      (if (not bongo-player-start-imminent)
          (setq bongo-mode-line-indicator-string new-value)
        (add-to-list 'bongo-deferred-status-indicator-updates
          (list 'set 'bongo-mode-line-indicator-string new-value))))))

(defun bongo-mode-line-indicator-mode
  (argument &optional called-interactively-p)
  "Toggle display of Bongo mode line indicator on or off.
With ARGUMENT equal to `toggle', or interactively
  with no prefix argument, toggle the mode.
With zero or negative ARGUMENT, turn the mode off.
With any other ARGUMENT, turn the mode on.
When called interactively, CALLED-INTERACTIVELY-P is non-nil."
  ;; Use `toggle' rather than (if mode 0 1) so that using
  ;; `repeat-command' still does the toggling correctly.
  (interactive (list (or current-prefix-arg 'toggle)
                     'called-interactively-p))
  (setq bongo-mode-line-indicator-mode
        (if (eq argument 'toggle)
            (not bongo-mode-line-indicator-mode)
          (> (prefix-numeric-value argument) 0)))
  (when called-interactively-p
    (customize-mark-as-set 'bongo-mode-line-indicator-mode))
  (when bongo-mode-line-indicator-parent
    (if (not bongo-mode-line-indicator-mode)
        (set bongo-mode-line-indicator-parent
             (remq 'bongo-mode-line-indicator-string
                   (symbol-value bongo-mode-line-indicator-parent)))
      (when (null (symbol-value bongo-mode-line-indicator-parent))
        (set bongo-mode-line-indicator-parent '("")))
      (add-to-list bongo-mode-line-indicator-parent
        'bongo-mode-line-indicator-string 'append)))
  (when (called-interactively-p 'interactive)
    (message "Bongo mode line indicator mode %s."
             (if bongo-mode-line-indicator-mode
                 "enabled" "disabled")))
  bongo-mode-line-indicator-mode)

(defgroup bongo-infosets nil
  "Structured track information in Bongo."
  :group 'bongo)

(defcustom bongo-potential-external-fields '(artist album)
  "The fields that may be used to join tracks into sections.
Currently, this list needs to be completely ordered, starting with
the most general field and ending with the most specific field."
  :type '(repeat symbol)
  :group 'bongo-infosets)

(defcustom bongo-infoset-from-file-name-function
  'bongo-default-infoset-from-file-name
  "Function used to convert file names into infosets.
This function should be chosen so that the following identity holds:
   (equal (file-name-sans-extension
           (file-name-nondirectory FILE-NAME))
          (bongo-file-name-from-infoset
           (bongo-infoset-from-file-name FILE-NAME)))

Good functions are `bongo-default-infoset-from-file-name'
and `bongo-simple-infoset-from-file-name'.

See also `bongo-file-name-from-infoset-function'."
  :type 'function
  :options '(bongo-default-infoset-from-file-name
             bongo-simple-infoset-from-file-name)
  :group 'bongo-file-names
  :group 'bongo-infosets)

(defcustom bongo-file-name-from-infoset-function
  'bongo-default-file-name-from-infoset
  "Function used to represent an infoset as a file name.
This function should be chosen so that the following identity holds:
   (equal (file-name-sans-extension
           (file-name-nondirectory FILE-NAME))
          (bongo-file-name-from-infoset
           (bongo-infoset-from-file-name FILE-NAME)))

If the infoset cannot be represented as a file name, the
function should signal an error.  To satisfy the above
identity, this must not be the case for any infoset that
`bongo-infoset-from-file-name' can generate.

The default function cannot represent infosets that contain
general but not specific data.  For example, it cannot
represent ((artist (name \"Foo\"))), because a file name
containing only \"Foo\" would be interpreted as containing
only a track title.

See also `bongo-infoset-from-file-name-function'."
  :type 'function
  :group 'bongo-file-names
  :group 'bongo-infosets)

(defcustom bongo-file-name-part-from-field-function
  'bongo-default-file-name-part-from-field
  "Function used to represent an info field as part of a file name.
This is used by `bongo-default-file-name-from-infoset'."
  :type 'function
  :group 'bongo-file-names
  :group 'bongo-infosets)

(defcustom bongo-infoset-formatting-function
  'bongo-default-format-infoset
  "Function used to represent an infoset as a user-friendly string."
  :type 'function
  :group 'bongo-display
  :group 'bongo-infosets)

(defcustom bongo-field-formatting-function
  'bongo-default-format-field
  "Function used to represent an info field as a user-friendly string.
This is used by `bongo-default-format-infoset'."
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

(defface bongo-warning
  '((t (:inherit font-lock-warning-face)))
  "Face used for warnings in Bongo buffers."
  :group 'bongo-faces)

(defface bongo-artist
  '((t (:inherit font-lock-keyword-face)))
  "Face used for Bongo artist names."
  :group 'bongo-faces)

(defface bongo-album '((t nil))
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

(defface bongo-track '((t nil))
  "Face used for Bongo tracks (index, title, length, and punctuation)."
  :group 'bongo-faces)

(defface bongo-track-title
  '((t (:inherit (font-lock-function-name-face bongo-track))))
  "Face used for Bongo track titles."
  :group 'bongo-faces)

(defface bongo-track-index
  '((t (:inherit bongo-track)))
  "Face used for Bongo track indices."
  :group 'bongo-faces)

(defface bongo-track-length
  '((t (:inherit bongo-track)))
  "Face used for Bongo track lengths."
  :group 'bongo-faces)

(defface bongo-played-track
  '((t (:strike-through "#808080" :inherit bongo-track)))
  "Face used for already played Bongo tracks."
  :group 'bongo-faces)

(defface bongo-elapsed-track-part
  '((t (:strike-through "#808080")))
  "Face used for the elapsed part of the currently playing Bongo track."
  :group 'bongo-faces)

(defface bongo-currently-playing-track
  '((t (:weight bold :inherit bongo-track)))
  "Face used for the currently playing Bongo track."
  :group 'bongo-faces)

(defface bongo-action-track '((t nil))
  "Face used for Bongo action tracks."
  :group 'bongo-faces)

(defface bongo-action-track-description
  '((t (:weight bold :inherit (font-lock-builtin-face
                               bongo-action-track))))
  "Face used for Bongo action track descriptions."
  :group 'bongo-faces)

(defface bongo-action-track-expression
  '((t (:inherit bongo-action-track)))
  "Face used for Bongo action track expressions."
  :group 'bongo-faces)


;;;; Infoset- and field-related functions

(defun bongo-format-infoset (infoset)
  "Represent INFOSET as a user-friendly string.
This function just calls `bongo-infoset-formatting-function'."
  (funcall bongo-infoset-formatting-function infoset))

(defvar bongo-infoset-formatting-target 'unspecified
  "Target of the current infoset formatting.
When an infoset is formatted for display in a Bongo buffer,
  this variable is dynamically bound to the target buffer.
When there is no particular target, the value is `unspecified'.
This value can be used by the formatting function.")

(defvar bongo-infoset-formatting-target-line nil
  "Target line of the current infoset formatting, or nil.
When an infoset is formatted for display on a line in a Bongo buffer,
  this variable is dynamically bound to the buffer position of the line.
The buffer itself appears in `bongo-infoset-formatting-target'.
When the formatting target is not a Bongo buffer line, the value is nil.
This value can be used by the formatting function.")

(defun bongo-default-format-infoset (infoset)
  "Format INFOSET by calling `bongo-format-field' on each field.
Only the first instance of each field with a given name is considered.
Dynamically bind `bongo-infoset' to INFOSET while formatting.
Return the concatenation of the obtained formatted field values
separated by `bongo-field-separator'."
  (let* ((bongo-infoset infoset)
         (bongo-target bongo-infoset-formatting-target)
         (bongo-line bongo-infoset-formatting-target-line)
         (processed-fields nil)
         (formatted-fields
          (apply 'nconc
                 (mapcar (lambda (field)
                           (unless (or (memq (car field) processed-fields)
                                       (null (cdr field))
                                       (eq (cdr field) 'unknown)
                                       (eq (cdr field) 'unbound))
                             (push (car field) processed-fields)
                             (list (bongo-format-field field))))
                         infoset))))
    (mapconcat 'identity formatted-fields bongo-field-separator)))

(defun bongo-file-name-from-infoset (infoset)
  "Represent INFOSET as a file name, if possible.
If INFOSET cannot be represented as a file name, signal an error.
This function just calls `bongo-file-name-from-infoset-function'.
See the documentation for that variable for more information."
  (funcall bongo-file-name-from-infoset-function infoset))

(defun bongo-default-file-name-from-infoset (infoset)
  "Represent INFOSET as a file name, if possible.
This function calls `bongo-file-name-part-from-field' on
each field and separates the obtained field values using
`bongo-file-name-field-separator'."
  ;; Signal an error if the infoset cannot be represented.
  (mapconcat 'bongo-file-name-part-from-field infoset
             bongo-file-name-field-separator))

(defun bongo-join-fields (values)
  (mapconcat 'identity values bongo-field-separator))

(defun bongo-join-file-name-fields (values)
  (mapconcat 'identity values bongo-file-name-field-separator))

(defun bongo-format-field (field)
  (funcall bongo-field-formatting-function field))

(defun bongo-default-format-field (field)
  (let ((type (car field))
        (data (cdr field)))
    (cl-case type
     ((artist)
      (propertize (bongo-alist-get data 'name) 'face 'bongo-artist))
     ((album)
      ;; These variables are used in `bongo-album-format',
      ;; so their names are significant.
      (let* ((bongo-title
              (propertize (bongo-alist-get data 'title)
                          'face 'bongo-album-title))
             (bongo-year
              (when (bongo-alist-get data 'year)
                (propertize (bongo-alist-get data 'year)
                            'face 'bongo-album-year)))
             (bongo-artist data))
        (funcall bongo-album-function)))
     ((track)
      ;; These variables are used in `bongo-track-format',
      ;; so their names are significant.
      (let* ((bongo-title
              (propertize (bongo-alist-get data 'title)
                          'face 'bongo-track-title))
             (bongo-index
              (when (bongo-alist-get data 'index)
                (propertize (bongo-alist-get data 'index)
                            'face 'bongo-track-index)))
             (bongo-track data)
             (bongo-length
              (bongo-alist-get data 'length))
             (length-string
              (when bongo-length
                (funcall bongo-track-length-function)))
             (bongo-length
              (when length-string
                (propertize length-string 'face 'bongo-track-length))))
        (funcall bongo-track-function)))
     ((stream)
      ;; These variables are used in `bongo-stream-format',
      ;; so their names are significant.
      (let ((bongo-uri
             (propertize (bongo-alist-get data 'uri)
                         'face 'bongo-album-title))
            (bongo-uri-title
             (when (bongo-alist-get data 'uri-title)
               (propertize (bongo-alist-get data 'uri-title)
                           'face 'bongo-album-title)))
            (bongo-stream-name
             (when (bongo-alist-get data 'name)
               (propertize (bongo-alist-get data 'name)
                           'face 'bongo-album-title)))
            (bongo-stream-genre
             (when (bongo-alist-get data 'genre)
               (bongo-alist-get data 'genre)))
            (bongo-stream-part-title
             (when (bongo-alist-get data 'part-title)
               (propertize (bongo-alist-get data 'part-title)
                           'face 'bongo-track-title))))
        (bongo-format-string bongo-stream-format)))
     ((action)
      ;; These variables are used in `bongo-action-format',
      ;; so their names are significant.
      (let ((bongo-action-expression data)
            (bongo-action-description
             (let ((description-specifier
                    (when (listp data)
                      (get (car data) 'bongo-action-description))))
               (cond ((null description-specifier)
                      (bongo-facify (with-temp-buffer
                                      (emacs-lisp-mode)
                                      (insert (prin1-to-string data))
                                      (let ((font-lock-verbose nil))
                                        (font-lock-fontify-buffer))
                                      (buffer-string))
                                    'bongo-action-track-expression))
                     ((stringp description-specifier)
                      (bongo-facify-copy description-specifier
                                         'bongo-action-track-description))
                     ((functionp description-specifier)
                      (bongo-facify-copy (apply description-specifier data)
                                         'bongo-action-track-description))
                     (t (error (concat "Invalid action description "
                                       "specifier: `%S'")
                               description-specifier))))))
        (bongo-format-string bongo-action-format))))))

(defun bongo-file-name-part-from-field (field)
  "Represent FIELD as part of a file name.
This is used by `bongo-default-file-name-from-infoset'."
  (funcall bongo-file-name-part-from-field-function field))

(defun bongo-default-file-name-part-from-field (field)
  (let ((type (car field))
        (data (cdr field)))
    (cl-case type
     ((artist) data)
     ((album)
      (let ((title (bongo-alist-get data 'title))
            (year (bongo-alist-get data 'year)))
        (if (null year) title
          (bongo-join-file-name-fields (list year title)))))
     ((track)
      (let ((title (bongo-alist-get data 'title))
            (index (bongo-alist-get data 'index)))
        (if (null index) title
          (bongo-join-file-name-fields (list index title))))))))

(defun bongo-infoset-from-file-name (file-name)
  (funcall bongo-infoset-from-file-name-function file-name))

(defun bongo-uri-scheme (file-name)
  "Return the URI scheme of FILE-NAME, or nil if it has none.
To avoid treating Microsoft Windows and DOS drive letters as URI schemes,
we require that URI schemes be at least two characters long."
  (when (string-match (eval-when-compile
                        (rx (and string-start
                                 (submatch
                                  (any "a-zA-Z")
                                  (one-or-more
                                   (or (any "a-zA-Z0-9$_@.&!*\"'(),")
                                       (and "%" (repeat 2 hex-digit)))))
                                 ":")))
                      file-name)
    (match-string 1 file-name)))

(defun bongo-uri-p (file-name)
  "Return non-nil if FILE-NAME is a URI.
As a special case, return nil if FILE-NAME is nil."
  (and file-name (not (null (bongo-uri-scheme file-name)))))

(defun bongo-unescape-uri (uri)
  "Replace all occurences of `%HH' in URI by the character HH."
  (with-temp-buffer
    (insert uri)
    (goto-char (point-min))
    (while (re-search-forward
            (eval-when-compile
              (rx (and "%" (submatch (repeat 2 hex-digit)))))
            nil 'no-error)
      (replace-match (char-to-string
                      (string-to-number (match-string 1) 16))))
    (buffer-string)))

(defun bongo-default-infoset-from-file-name (file-name)
  (let ((track-length-part
         (when (and (boundp 'bongo-track-length) bongo-track-length)
           `((length . ,bongo-track-length)))))
    (if (bongo-uri-p file-name)
        `((artist . unknown)
          (album . unknown)
          (track (title . ,(bongo-unescape-uri file-name))
                 ,@track-length-part))
      (let* ((base-name (file-name-sans-extension
                         (file-name-nondirectory file-name)))
             (values (split-string base-name bongo-file-name-field-separator)))
        (when (> (length values) 5)
          (let ((fifth-and-rest (nthcdr 4 values)))
            (setcar fifth-and-rest (bongo-join-fields fifth-and-rest))
            (setcdr fifth-and-rest nil)))
        (cond ((= 5 (length values))
               (if (string-match bongo-file-name-track-index-regexp
                                 (nth 3 values))
                   `((artist (name . ,(nth 0 values)))
                     (album (year . ,(nth 1 values))
                            (title . ,(nth 2 values)))
                     (track (index . ,(nth 3 values))
                            (title . ,(nth 4 values))
                            ,@track-length-part))
                 `((artist (name . ,(nth 0 values)))
                   (album (year . ,(nth 1 values))
                          (title . ,(nth 2 values)))
                   (track (title . ,(bongo-join-fields
                                     (nthcdr 3 values)))
                          ,@track-length-part))))
              ((and (= 4 (length values))
                    (string-match bongo-file-name-track-index-regexp
                                  (nth 2 values)))
               `((artist (name . ,(nth 0 values)))
                 (album (title . ,(nth 1 values)))
                 (track (index . ,(nth 2 values))
                        (title . ,(nth 3 values))
                        ,@track-length-part)))
              ((and (= 4 (length values))
                    (string-match bongo-file-name-album-year-regexp
                                  (nth 1 values)))
               `((artist (name . ,(nth 0 values)))
                 (album (year . ,(nth 1 values))
                        (title . ,(nth 2 values)))
                 (track (title . ,(nth 3 values))
                        ,@track-length-part)))
              ((= 4 (length values))
               `((artist (name . ,(nth 0 values)))
                 (album (title . ,(nth 1 values)))
                 (track (title . ,(bongo-join-fields
                                   (nthcdr 2 values)))
                        ,@track-length-part)))
              ((= 3 (length values))
               `((artist (name . ,(nth 0 values)))
                 (album (title . ,(nth 1 values)))
                 (track (title . ,(nth 2 values))
                        ,@track-length-part)))
              ((= 2 (length values))
               `((artist (name . ,(nth 0 values)))
                 (album . unknown)
                 (track (title . ,(nth 1 values))
                        ,@track-length-part)))
              ((= 1 (length values))
               `((artist . unknown)
                 (album . unknown)
                 (track (title . ,(nth 0 values))
                        ,@track-length-part))))))))

(defun bongo-simple-infoset-from-file-name (file-name)
  `((track (title . ,(file-name-sans-extension
                      (file-name-nondirectory
                       (if (bongo-uri-p file-name)
                           (bongo-unescape-uri file-name)
                         file-name)))))))

(defun bongo-double-alist-get (alist-1 key-1 key-2)
  (let ((alist-2 (bongo-alist-get alist-1 key-1)))
    (when (listp alist-2)
      (bongo-alist-get alist-2 key-2))))

(defun bongo-infoset-artist-name (infoset)
  (bongo-double-alist-get infoset 'artist 'name))
(defun bongo-infoset-album-year (infoset)
  (bongo-double-alist-get infoset 'album 'year))
(defun bongo-infoset-album-title (infoset)
  (bongo-double-alist-get infoset 'album 'title))
(defun bongo-infoset-track-index (infoset)
  (bongo-double-alist-get infoset 'track 'index))
(defun bongo-infoset-track-title (infoset)
  (bongo-double-alist-get infoset 'track 'title))


;;;; Basic point-manipulation routines

(defun bongo-goto-point (point)
  "Set point to POINT, if POINT is non-nil.
POINT may be a number, a marker or nil."
  (when point (goto-char point)))

(defun bongo-before-invisible-text-p (&optional point)
  "Return non-nil if the character after POINT is invisible.
Return nil if POINT is at the last buffer position.
See `buffer-invisibility-spec'."
  (save-excursion
    (bongo-goto-point point)
    (and (not (eobp))
         (let ((property (get-char-property (point) 'invisible)))
           (if (eq buffer-invisibility-spec t)
               property
             (or (memq property buffer-invisibility-spec)
                 (assq property buffer-invisibility-spec)))))))

(defun bongo-after-invisible-text-p (&optional point)
  "Return non-nil if the character before POINT is invisible.
Return nil if POINT is at the first buffer position.
See `buffer-invisibility-spec'."
  (save-excursion
    (bongo-goto-point point)
    (and (not (bobp))
         (bongo-before-invisible-text-p (1+ (point))))))

(defun bongo-skip-invisible ()
  "Move point to the next visible character.
If point is already on a visible character, do nothing."
  (while (bongo-before-invisible-text-p)
    (goto-char (next-single-char-property-change (point) 'invisible))))

(defun bongo-point-at-bol (&optional point)
  "Return the first character position of the line at POINT.
If `line-move-ignore-invisible' is non-nil, ignore invisible text.
For lines that start with invisible text, return the position of
the first visible character on the line."
  (save-excursion
    (bongo-goto-point point)
    (if (not line-move-ignore-invisible)
        (point-at-bol)
      (while (progn (skip-chars-backward "^\n")
                    (bongo-after-invisible-text-p))
        (goto-char (previous-single-char-property-change
                    (point) 'invisible)))
      (bongo-skip-invisible)
      (point))))

(defun bongo-point-at-eol (&optional point)
  "Return the last character position of the line at POINT.
If `line-move-ignore-invisible' is non-nil, ignore invisible text.
Always return the position of the newline character, even for lines
that contain invisible text immediately before the newline."
  (save-excursion
    (bongo-goto-point point)
    (if (not line-move-ignore-invisible)
        (point-at-eol)
      (while (progn (skip-chars-forward "^\n")
                    (bongo-before-invisible-text-p))
        (goto-char (next-single-char-property-change
                    (point) 'invisible)))
      (point))))

(defun bongo-beginning-of-line (&optional point)
  "Move to the beginning of the line at POINT.
See `bongo-point-at-bol'."
  (goto-char (bongo-point-at-bol point)))

(defun bongo-end-of-line (&optional point)
  "Move to the end of the line at POINT.
See `bongo-point-at-eol'."
  (goto-char (bongo-point-at-eol point)))

(defun bongo-first-line-p (&optional point)
  "Return non-nil if POINT is on the first line."
  (= (bongo-point-at-bol point) (point-min)))

(defun bongo-last-line-p (&optional point)
  "Return non-nil if POINT is on the last line.
An empty line at the end of the buffer doesn't count."
  (>= (1+ (bongo-point-at-eol point)) (point-max)))

(defun bongo-first-object-line-p (&optional point)
  "Return non-nil if POINT is on the first object line.
See also `bongo-point-at-previous-object-line'."
  (null (bongo-point-at-previous-object-line point)))

(defun bongo-last-object-line-p (&optional point)
  "Return non-nil if POINT is on the last object line.
See also `bongo-point-at-next-object-line'."
  (null (bongo-point-at-next-object-line point)))

(defalias 'bongo-point-before-line
  'bongo-point-at-bol)

(defun bongo-point-after-line (&optional point)
  "Return the first character position after the line at POINT.
In the normal case, for lines that end with newlines, the point
after a line is the same as the point before the next line."
  (let ((eol (bongo-point-at-eol point)))
    (if (= eol (point-max)) eol (1+ eol))))

(defun bongo-point-at-bol-forward (&optional point)
  "Return the position of the first line beginning after or at POINT.
If POINT is at the beginning of a line, just return POINT.
Otherwise, return the first character position after the line at POINT."
  (or point (setq point (point)))
  (if (= point (bongo-point-at-bol point))
      point
    (bongo-point-after-line point)))

(defun bongo-point-before-previous-line (&optional point)
  "Return the first position of the line before the one at POINT.
If the line at POINT is the first line, return nil."
  (unless (bongo-first-line-p point)
    (bongo-point-at-bol (1- (bongo-point-at-bol point)))))

(defun bongo-point-before-next-line (&optional point)
  "Return the first position of the line after the one at POINT.
If the line at POINT is the last line, return nil."
  (unless (bongo-last-line-p point)
    (1+ (bongo-point-at-eol point))))

(defalias 'bongo-point-at-previous-line
  'bongo-point-before-previous-line)

(defalias 'bongo-point-at-next-line
  'bongo-point-before-next-line)

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
        (when match
          (point))))))

(defalias 'bongo-point-at-previous-line-satisfying
  'bongo-point-before-previous-line-satisfying)

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
          (forward-line 1)
          (when (funcall predicate)
            (setq match t)))
        (when match
          (point))))))

(defalias 'bongo-point-at-next-line-satisfying
  'bongo-point-before-next-line-satisfying)

(defun bongo-point-after-next-line-satisfying (predicate &optional point)
  "Return the position after the next line satisfying PREDICATE.
This function works like `bongo-point-before-next-line-satisfying'.
Start searching at POINT, or at point if POINT is nil."
  (let ((before-next (bongo-point-before-next-line-satisfying
                      predicate point)))
    (when before-next
      (bongo-point-at-eol before-next))))

(defun bongo-point-at-first-line-satisfying (predicate &optional point)
  "Return the position of the first line satisfying PREDICATE.
If POINT is non-nil, the search starts at the line at POINT.
If POINT is nil, it starts at the first line in the buffer.
If no matching line is found, return nil."
  (save-excursion
    (when (null point)
      (goto-char (point-min)))
    (if (funcall predicate)
        (point)
      (bongo-point-at-next-line-satisfying predicate))))

(defvar bongo-random-number-generator-seeded nil
  "Non-nil if Bongo has seeded the random number generator.")

(defun bongo-point-at-random-line-satisfying (predicate)
  "Return the position of a random line satisfying PREDICATE.
If there are no lines that satisfy PREDICATE, loop forever."
  (unless bongo-random-number-generator-seeded
    (random t)
    (setq bongo-random-number-generator-seeded t))
  (save-excursion
    (while (progn (goto-char (point-min))
                  (forward-line (1- (random (count-lines (point-max)
                                                         (point-min)))))
                  (not (funcall predicate))))
    (point)))

(defun bongo-point-at-random-track-line ()
  "Return the position of a random track line.
If there are no track lines, loop forever."
  (bongo-point-at-random-line-satisfying 'bongo-track-line-p))

(defun bongo-point-before-previous-object-line (&optional point)
  "Return the character position of the previous object line.
If POINT is non-nil, start before that line; otherwise,
  start before the current line.
If no object line is found before the starting line, return nil."
  (bongo-point-before-previous-line-satisfying 'bongo-object-line-p point))

(defalias 'bongo-point-at-previous-object-line
  'bongo-point-before-previous-object-line)

(defun bongo-point-before-next-object-line (&optional point)
  "Return the character position of the next object line.
If POINT is non-nil, start after that line; otherwise,
  start after the current line.
If no object line is found after the starting line, return nil."
  (bongo-point-before-next-line-satisfying 'bongo-object-line-p point))

(defalias 'bongo-point-at-next-object-line
  'bongo-point-before-next-object-line)

(defun bongo-point-after-next-object-line (&optional point)
  "Return the character position after the next object line.
This function works like `bongo-point-before-next-object-line'.
Start searching at POINT, or at point if POINT is nil."
  (bongo-point-after-next-line-satisfying 'bongo-object-line-p point))

(put 'bongo-no-previous-object
     'error-conditions
     '(error bongo-movement-error bongo-no-previous-object))
(put 'bongo-no-previous-object
     'error-message
     "No previous section or track")

(defun bongo-previous-object-line (&optional no-error)
  "Move point to the previous object line, if possible.
If NO-ERROR is non-nil, return non-nil if and only if point was moved.
If NO-ERROR is not given or nil, and there is no previous object line,
signal `bongo-no-previous-object'."
  (interactive "p")
  (let ((position (bongo-point-at-previous-object-line)))
    (if position
        (prog1 'point-moved
          (goto-char position))
      (unless no-error
        (signal 'bongo-no-previous-object nil)))))

(put 'bongo-no-next-object
     'error-conditions
     '(error bongo-movement-error bongo-no-next-object))
(put 'bongo-no-next-object
     'error-message
     "No next section or track")

(defun bongo-next-object-line (&optional no-error)
  "Move point to the next object line, if possible.
If NO-ERROR is non-nil, return non-nil if and only if point was moved.
If NO-ERROR is not given or nil, and there is no next object line,
signal `bongo-no-next-object'."
  (interactive "p")
  (let ((position (bongo-point-at-next-object-line)))
    (if position
        (prog1 'point-moved
          (goto-char position))
      (unless no-error
        (signal 'bongo-no-next-object nil)))))

(defun bongo-snap-to-object-line (&optional no-error)
  "Move point to the next object line unless it is already on one.
If point was already on an object line, return `point-not-moved'.
If point was moved to the next object line, return `point-moved'.
If there is no next object line, signal `bongo-no-next-object'.
If NO-ERROR is non-nil, return nil instead of signalling an error."
  (interactive)
  (if (bongo-object-line-p)
      'point-not-moved
    (bongo-next-object-line no-error)))

(put 'bongo-no-previous-header-line
     'error-conditions
     '(error bongo-movement-error bongo-no-previous-header-line))
(put 'bongo-no-previous-header-line
     'error-message
     "No previous header line")

(defun bongo-previous-header-line (&optional n)
  "Move N header lines backward.
With negative N, move forward instead."
  (interactive "p")
  (if (< n 0)
      (bongo-next-header-line (- n))
    (dotimes (dummy n)
      (let ((position (bongo-point-at-previous-line-satisfying
                       'bongo-header-line-p)))
        (if position
            (goto-char position)
          (signal 'bongo-no-previous-header-line nil))))))

(put 'bongo-no-next-header-line
     'error-conditions
     '(error bongo-movement-error bongo-no-next-header-line))
(put 'bongo-no-next-header-line
     'error-message
     "No next header line")

(defun bongo-next-header-line (&optional n)
  "Move N header lines forward.
With negative N, move backward instead."
  (interactive "p")
  (if (< n 0)
      (bongo-previous-header-line (- n))
    (dotimes (dummy n)
      (let ((position (bongo-point-at-next-line-satisfying
                       'bongo-header-line-p)))
        (if position
            (goto-char position)
          (signal 'bongo-no-next-header-line nil))))))

(defun bongo-backward-expression (&optional n)
  "Move backward across one section, track, or stretch of text.
With prefix argument N, do it that many times.
With negative argument -N, move forward instead."
  (interactive "p")
  (or n (setq n 1))
  (if (< n 0)
      (bongo-forward-expression (- n))
    (catch 'done
      (dotimes (dummy n)
        (if (= (point) (point-min))
            (throw 'done nil)
          (goto-char (or (if (bongo-object-line-p)
                             (or (bongo-point-before-previous-object)
                                 (bongo-point-at-previous-object-line))
                           (bongo-point-after-line
                            (bongo-point-at-previous-object-line)))
                         (point-min))))))))

(defun bongo-forward-expression (&optional n)
  "Move forward across one section, track, or stretch of text.
With prefix argument N, do it that many times.
With negative argument -N, move backward instead.
This function is a suitable value for `forward-sexp-function'."
  (interactive "p")
  (or n (setq n 1))
  (if (< n 0)
      (bongo-backward-expression (- n))
    (catch 'done
      (dotimes (dummy n)
        (if (= (point) (point-max))
            (throw 'done nil)
          (goto-char (or (if (bongo-object-line-p)
                             (bongo-point-after-object)
                           (bongo-point-before-next-object-line))
                         (point-max))))))))

(defun bongo-previous-object (&optional no-error n)
  "Move to the previous object (either section or track).
With prefix argument N, do it that many times.
With negative prefix argument -N, move forward instead.
If there is no previous object, signal `bongo-no-previous-object'.
If NO-ERROR is non-nil, move to the beginning of the buffer instead.
Return non-nil only if the move was successful."
  (interactive "p")
  (or n (setq n 1))
  (if (< n 0)
      (bongo-next-object no-error (- n))
    (let ((result t))
      (dotimes (dummy n result)
        (goto-char (or (bongo-point-at-previous-object)
                       (if no-error
                           (prog1 (point-min)
                             (setq result nil))
                         (signal 'bongo-no-previous-object nil))))))))

(defun bongo-next-object (&optional no-error n)
  "Move to the next object (either section or track).
With prefix argument N, do it that many times.
With negative prefix argument -N, move backward instead.
If there is no next object, signal `bongo-no-next-object'.
If NO-ERROR is non-nil, move to end of the buffer instead.
Return non-nil only if the move was successful."
  (interactive (list nil (prefix-numeric-value current-prefix-arg)))
  (or n (setq n 1))
  (if (< n 0)
      (bongo-previous-object no-error (- n))
    (let ((result t))
      (dotimes (dummy n result)
        (goto-char (or (bongo-point-at-next-object)
                       (if no-error
                           (prog1 (point-max)
                             (setq result nil))
                         (signal 'bongo-no-next-object nil))))))))

(defun bongo-point-before-next-track-line (&optional point)
  "Return the character position of the next track line.
If POINT is non-nil, start after that line; otherwise,
  start after the current line.
If no track line is found after the starting line, return nil."
  (bongo-point-before-next-line-satisfying 'bongo-track-line-p point))

(defalias 'bongo-point-at-next-track-line
  'bongo-point-before-next-track-line)

(defun bongo-point-before-previous-track-line (&optional point)
  "Return the character position of the previous track line.
If POINT is non-nil, start before that line; otherwise,
  start before the current line.
If no track line is found before the starting line, return nil."
  (bongo-point-before-previous-line-satisfying 'bongo-track-line-p point))

(defalias 'bongo-point-at-previous-track-line
  'bongo-point-before-previous-track-line)

(defun bongo-point-after-object (&optional point)
  "Return the character position after the object at POINT.
By object is meant either section or track.
If there are no sections or tracks at POINT, return nil."
  (save-excursion
    (bongo-goto-point point)
    (when line-move-ignore-invisible
      (bongo-skip-invisible))
    (when (bongo-snap-to-object-line 'no-error)
      (let ((indentation (bongo-line-indentation)))
        (let ((after-last nil))
          (bongo-ignore-movement-errors
            (while (progn
                     (setq after-last (bongo-point-after-line))
                     (bongo-next-object-line)
                     (> (bongo-line-indentation) indentation))))
          after-last)))))

(defun bongo-point-at-next-object (&optional point)
  "Return the character position of the object after the one at POINT.
By object is meant either section or track.
If there are no sections or tracks after the one at POINT, return nil."
  (save-excursion
    (bongo-goto-point point)
    (when line-move-ignore-invisible
      (bongo-skip-invisible))
    (if (bongo-object-line-p)
        (let ((indentation (bongo-line-indentation)))
          (goto-char (bongo-point-after-object))
          (when (and (bongo-snap-to-object-line 'no-error)
                     (= (bongo-line-indentation) indentation))
            (point)))
      (when (bongo-snap-to-object-line 'no-error)
        (point)))))

(defun bongo-point-before-previous-object (&optional point)
  "Return the character position of the object previous to POINT.
By object is meant either section or track.
If there are no sections or tracks before POINT, return nil."
  (save-excursion
    (bongo-goto-point point)
    (when line-move-ignore-invisible
      (bongo-skip-invisible))
    (let ((indentation (if (bongo-snap-to-object-line 'no-error)
                           (bongo-line-indentation)
                         0)))
      (bongo-ignore-movement-errors
        (while (progn
                 (bongo-previous-object-line)
                 (> (bongo-line-indentation) indentation)))
        (when (= (bongo-line-indentation) indentation)
          (bongo-point-before-line))))))

(defalias 'bongo-point-at-previous-object
  'bongo-point-before-previous-object)

(defun bongo-point-at-first-track-line ()
  "Return the character position of the first track line, or nil."
  (save-excursion
    (goto-char (point-min))
    (if (bongo-track-line-p)
        (bongo-point-at-bol)
      (bongo-point-at-next-track-line))))

(defun bongo-point-at-last-track-line ()
  "Return the character position of the last track line, or nil."
  (save-excursion
    (goto-char (point-max))
    (if (bongo-track-line-p)
        (bongo-point-at-bol)
      (bongo-point-at-previous-track-line))))

(defun bongo-uri-track-infoset (&optional point)
  "Return the infoset for the URI track at POINT.
You should use `bongo-line-infoset' most of the time."
  (let ((file-name
         (bongo-line-file-name point))
        (uri-title
         (bongo-line-get-property 'bongo-uri-title point))
        (stream-name
         (bongo-line-get-property 'bongo-stream-name point))
        (stream-genre
         (bongo-line-get-property 'bongo-stream-genre point))
        (stream-part-title
         (let ((player (bongo-line-get-property 'bongo-player point)))
           (and player (bongo-player-running-p player)
                (bongo-player-get player 'stream-part-title)))))
    (cond ((or stream-name stream-genre stream-part-title)
           `((artist . unknown)
             (album . unknown)
             (stream (uri . ,file-name)
                     (uri-title . ,uri-title)
                     (name . ,stream-name)
                     (genre . ,stream-genre)
                     (part-title . ,stream-part-title))))
          (uri-title
           (let ((track-length
                  (bongo-line-get-property 'bongo-track-length point)))
             `((artist . unknown)
               (album . unknown)
               (track (title . ,uri-title)
                      ,@(when track-length
                          `((length . ,track-length)))))))
          (t
           (bongo-file-track-infoset point)))))

(defun bongo-file-track-infoset (&optional point)
  "Return the infoset for the file track at POINT.
You should use `bongo-line-infoset' most of the time."
  (let ((bongo-track-length
         (bongo-line-get-property 'bongo-track-length point)))
    (bongo-infoset-from-file-name (bongo-line-file-name point))))

(defun bongo-action-track-infoset (&optional point)
  "Return the infoset for the action track at POINT.
You should use `bongo-line-infoset' most of the time."
  `((action . ,(bongo-line-action point))))

(defun bongo-track-infoset (&optional point)
  "Return the infoset for the track at POINT.
You should use `bongo-line-infoset' most of the time."
  (unless (bongo-track-line-p point)
    (error "Point is not on a track line"))
  (or (bongo-line-get-property 'bongo-infoset point)
      (cond ((bongo-uri-track-line-p point)
             (bongo-uri-track-infoset point))
            ((bongo-file-track-line-p point)
             (bongo-file-track-infoset point))
            ((bongo-action-track-line-p point)
             (bongo-action-track-infoset point)))))

(defun bongo-header-infoset (&optional point)
  "Return the infoset for the header at POINT.
You should use `bongo-line-infoset' most of the time."
  (save-excursion
    (bongo-goto-point point)
    (unless (bongo-header-line-p)
      (error "Point is not on a header line"))
    (let* ((line-move-ignore-invisible nil)
           (fields (bongo-line-fields))
           (indentation (bongo-line-indentation)))
      (while (and (bongo-next-object-line 'no-error)
                  (> (bongo-line-indentation) indentation)
                  (or (not (bongo-track-line-p))
                      (bongo-action-track-line-p))))
      (if (and (> (bongo-line-indentation) indentation)
               (bongo-track-line-p))
          (bongo-filter-alist fields (bongo-track-infoset))
        (mapcar (lambda (field)
                  (cons field 'unbound))
                fields)))))

(defun bongo-line-infoset (&optional point)
  "Return the infoset for the line at POINT.
For header lines, derive the infoset from the `bongo-fields' text
  property and the infoset of the nearest following track line.
For track lines, first check the `bongo-infoset' text property:  If the
  value is non-nil, return it; otherwise, construct the infoset as follows.
For action track lines, construct the infoset as ((action . ACTION)),
  where ACTION is the value of the `bongo-action' text property.
For file track lines, construct the infoset by passing the file name to
  the value of `bongo-file-name-parsing-function'."
    (cond ((bongo-track-line-p point)
           (bongo-track-infoset point))
          ((bongo-header-line-p point)
           (bongo-header-infoset point))))

(defun bongo-line-internal-infoset (&optional point)
  "Return the internal infoset for the line at POINT.
The internal infoset contains values of the internal fields only."
  (bongo-filter-alist (bongo-line-internal-fields point)
                      (bongo-line-infoset point)))

(defun bongo-line-field-value (field &optional point)
  "Return the value of FIELD for the line at POINT."
  (cdr (assoc field (bongo-line-infoset point))))

(defun bongo-line-field-values (fields &optional point)
  "Return the values of FIELDS for the line at POINT."
  (mapcar 'cdr (bongo-filter-alist fields (bongo-line-infoset point))))

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
  (bongo-set-difference (bongo-line-fields point)
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
  (catch 'return
    (bongo-line-external-fields-proposal
     (or (bongo-point-at-previous-object-line point)
         (throw 'return nil)))))

(defun bongo-line-proposed-indentation (&optional point)
  "Return the number of external fields proposed to the line at POINT.
See `bongo-line-proposed-external-fields'."
  (catch 'return
    (bongo-line-indentation-proposal
     (or (bongo-point-at-previous-object-line point)
         (throw 'return 0)))))

;;; (defun bongo-line-relatively-outdented-p ()
;;;   (< (bongo-line-indentation) (bongo-line-proposed-indentation)))

(defun bongo-line-file-name (&optional point)
  "Return the `bongo-file-name' text property of the line at POINT."
  (bongo-line-get-property 'bongo-file-name point))

(defun bongo-line-action (&optional point)
  "Return the `bongo-action' text property of the line at POINT."
  (bongo-line-get-property 'bongo-action point))

(defun bongo-file-track-line-p (&optional point)
  "Return non-nil if the line at POINT is a file track line."
  (bongo-line-file-name point))

(defun bongo-uri-track-line-p (&optional point)
  "Return non-nil if the line at POINT is a URI track line."
  (and (bongo-file-track-line-p point)
       (bongo-uri-p (bongo-line-file-name point))))

(defun bongo-audio-cd-uri-p (file-name)
  "Return non-nil if FILE-NAME is an audio CD (CDDA) URI."
  (equal (bongo-uri-scheme file-name) "cdda"))

(defun bongo-audio-cd-track-line-p (&optional point)
  "Return non-nil if the line at POINT is an audio CD track line."
  (and (bongo-file-track-line-p point)
       (bongo-audio-cd-uri-p (bongo-line-file-name point))))

(defun bongo-local-file-track-line-p (&optional point)
  "Return non-nil if the line at POINT is a local file track line."
  (and (bongo-file-track-line-p point)
       (not (bongo-uri-track-line-p point))))

(defcustom bongo-audio-file-name-extensions
  `("669"
    "aac",
    "amf"
    "apun"
    "au"
    "dsm"
    "far"
    "flac"
    "g18"
    "g36"
    "gdm"
    "imf"
    "it"
    "mdz"
    "med"
    "mid"
    "midi"
    "mka"
    "mod"
    "mp2"
    "mp3"
    "mtm"
    "ogg"
    "okt"
    "r36"
    "ra"
    "rcp"
    "rmi"
    "s3m"
    "spx"
    "stm"
    "stx"
    "ult"
    "umx"
    "uni"
    "vqf"
    "wav"
    "wma"
    "xm")
  "List of file name extensions of audio files."
  :type '(repeat string)
  :group 'bongo-file-names)

(defun bongo-audio-file-name-p (file-name)
  "Return non-nil if FILE-NAME has an audio file name extension."
  (member (file-name-extension file-name)
          bongo-audio-file-name-extensions))

(defun bongo-local-audio-file-track-line-p (&optional point)
  "Return non-nil if the line at POINT is an audio file track line."
  (and (bongo-local-file-track-line-p point)
       (bongo-audio-file-name-p (bongo-line-file-name point))))

(defcustom bongo-video-file-name-extensions
  `("asf"
    "avi"
    "flv"
    "mkv"
    "mov"
    "mp4"
    "mpeg"
    "mpg"
    "ogm"
    "qt"
    "rm"
    "rmvb"
    "ts"
    "vob"
    "webm"
    "wmv")
  "List of file name extensions of video files."
  :type '(repeat string)
  :group 'bongo-file-names)

(defun bongo-video-file-name-p (file-name)
  "Return non-nil if FILE-NAME has a video file name extension."
  (member (file-name-extension file-name)
          bongo-video-file-name-extensions))

(defun bongo-local-video-file-track-line-p (&optional point)
  "Return non-nil if the line at POINT is an video file track line."
  (and (bongo-local-file-track-line-p point)
       (bongo-video-file-name-p (bongo-line-file-name point))))

(defun bongo-action-track-line-p (&optional point)
  "Return non-nil if the line at POINT is an action track line."
  (bongo-line-action point))

(defun bongo-track-line-p (&optional point)
  "Return non-nil if the line at POINT is a track line."
  (cl-case major-mode
    ((bongo-playlist-mode bongo-library-mode)
     (or (bongo-file-track-line-p point)
         (bongo-action-track-line-p point)))
    (dired-mode
     (save-excursion
       (bongo-goto-point point)
       (ignore-errors
         (bongo-backend-for-file (dired-get-file-for-visit)))))))

(defun bongo-currently-playing-track-line-p (&optional point)
  "Return non-nil if the line at POINT is currently playing."
  (and (bongo-current-track-line-p point)
       (bongo-playing-p)))

(defun bongo-played-track-line-p (&optional point)
  "Return non-nil if the line at POINT is a played track line."
  (and (bongo-track-line-p point)
       (bongo-line-get-property 'bongo-played point)))

(defun bongo-unplayed-track-line-p (&optional point)
  "Return non-nil if the line at POINT is an unplayed track line."
  (and (bongo-track-line-p point)
       (null (bongo-action-track-line-p))
       (null (bongo-line-get-property 'bongo-played point))))

(defun bongo-mark-line-as-played (&optional point)
  "Mark the track line at POINT as played."
  (bongo-line-set-property 'bongo-played t point)
  (bongo-redisplay-line point)
  (when bongo-sprinkle-mode
    (bongo-sprinkle-until-saturated)))

(defun bongo-mark-current-track-line-as-played ()
  "Mark the current track line as played.
If there is no current track line, do nothing."
  (catch 'abort
    (let ((line-move-ignore-invisible nil))
      (bongo-mark-line-as-played
       (or (bongo-point-at-current-track-line)
           (throw 'abort nil))))))

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

(defun bongo-empty-section-p (&optional point)
  "Return non-nil if the line at POINT is an empty section.
That is, the header line of a section that has no content."
  (and (bongo-header-line-p point)
       (catch 'return
         (let ((line-move-ignore-invisible nil))
           (not (> (bongo-line-indentation
                    (or (bongo-point-at-next-object-line point)
                        (throw 'return t)))
                   (bongo-line-indentation point)))))))


;;;; General convenience routines

(defsubst bongo-xor (a b)
  "Return non-nil if exactly one of A and B is nil."
  (if a (not b) b))

(defun bongo-shortest (a b)
  "Return the shorter of the lists A and B."
  (if (<= (length a) (length b)) a b))

(defun bongo-longest (a b)
  "Return the longer of the lists A and B."
  (if (>= (length a) (length b)) a b))

(defun bongo-equally-long-p (a b)
  "Return non-nil if the lists A and B have equal length."
  (= (length a) (length b)))

(defun bongo-set-union (&rest sets)
  "Return the set-theoretic union of the items in SETS.
Comparisons are done with `eq'.  Order is *not* preserved."
  (let (result)
    (dolist (set sets result)
      (dolist (entry set)
        (unless (memq entry result)
          (push entry result))))))

(defun bongo-set-intersection (a b)
  "Return the items in A that are also in B.
Comparisons are done with `eq'.  Order is preserved."
  (let (result)
    (dolist (entry a (nreverse result))
      (when (memq entry b)
        (push entry result)))))

(defun bongo-set-exclusive-or (a b)
  "Return the items that appear in either A or B but not both.
Comparisons are done with `eq'.  Order is *not* preserved."
  (let (result)
    (dolist (set (list a b) result)
      (dolist (entry set)
        (when (bongo-xor (memq entry a) (memq entry b))
          (push entry result))))))

(defun bongo-set-difference (a b)
  "Return the items in A that are not also in B.
Comparisons are done with `eq'.  Order is preserved."
  (let (result)
    (dolist (entry a (nreverse result))
      (unless (memq entry b)
        (push entry result)))))

(defun bongo-set-equal-p (a b)
  "Return non-nil if A and B have equal elements.
Comparisons are done with `eq'.  Element order is not significant."
  (null (bongo-set-exclusive-or a b)))

(defun bongo-subset-p (a b)
  "Return non-nil if all elements in B are also in A.
Comparisons are done with `eq'.  Element order is not significant."
  (bongo-set-equal-p (bongo-set-union a b) a))

(defun bongo-alist-get (alist key)
  "Return the cdr of the element in ALIST whose car equals KEY.
If no such element exists, return nil."
  (cdr (assoc key alist)))

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

;; This function is used by `define-bongo-backend'.
(eval-and-compile
(defun bongo-plist-get-all (plist property)
  "Return a list of all values in PLIST corresponding to PROPERTY."
  (let ((result nil))
    (while plist
      (when (eq (car plist) property)
        (push (cadr plist) result))
      (setq plist (cddr plist)))
    (nreverse result))))

(defun bongo-filter-alist (keys alist)
  "Return a new list of each pair in ALIST whose car is in KEYS.
Key comparisons are done with `eq'.  Order is preserved."
  (let (result)
    (dolist (entry alist (nreverse result))
      (when (memq (car entry) keys)
        (push entry result)))))

(defun bongo-filter-plist (keys plist)
  "Return a new list of each property in PLIST whose name is in KEYS.
Key comparisons are done with `eq'.  Order is *not* preserved."
  (let ((result nil))
    (while plist
      (when (memq (car plist) keys)
        (setq result (cons (car plist) (cons (cadr plist) result))))
      (setq plist (cddr plist)))
    result))

(defun bongo-region-active-p ()
  "Return non-nil if the region is active."
  (and transient-mark-mode mark-active))


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

(defun bongo-line-string (&optional point)
  "Return the contents of the line at POINT.
The contents includes the final newline, if any."
  (buffer-substring (bongo-point-before-line point)
                    (bongo-point-after-line point)))

(defun bongo-extract-line (&optional point)
  "Delete the line at POINT and return its content.
The content includes the final newline, if any."
  (prog1 (bongo-line-string point)
    (bongo-delete-line point)))

(defun bongo-clear-line (&optional point)
  "Remove all contents of the line at POINT."
  (let ((inhibit-read-only t))
    (with-silent-modifications
      (bongo-ensure-final-newline)
      (save-excursion
        (bongo-goto-point point)
        ;; Avoid deleting the newline, because that would
        ;; cause the markers on this line to become mixed up
        ;; with those on the next line.
        (delete-region (point-at-bol) (point-at-eol))
        ;; Remove all text properties from the newline.
        (set-text-properties (point) (1+ (point)) nil)))))

(defun bongo-region-line-count (beg end)
  "Return the number of lines between BEG and END.
If BEG and END are the same, return 0.
If they are distinct but on the same line, return 1."
  (save-excursion
    (goto-char beg)
    (let ((result 0))
      (while (< (point) end)
        (setq result (1+ result))
        (forward-line))
      result)))

(defun bongo-count-lines-satisfying (predicate &optional beg end)
  "Return the total number of lines that satisfy PREDICATE.
If BEG and END are non-nil, only count lines in that region."
  (or beg (setq beg (point-min)))
  (or end (setq end (point-max)))
  (let ((result 0))
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (when (funcall predicate)
          (setq result (+ result 1)))
        (forward-line 1)))
    result))


;;;; Text properties

(defun bongo-line-get-property (name &optional point)
  "Return the value of the text property NAME on the line at POINT.
Actually only look at the terminating newline."
  (get-text-property (bongo-point-at-eol point) name))

(defvar bongo-line-semantic-properties
  ;; When changing this, consider also changing
  ;; `bongo-line-serializable-properties'.
  (list 'bongo-file-name 'bongo-action 'bongo-backend
        'bongo-infoset 'bongo-uri-title 'bongo-track-length
        'bongo-stream-name 'bongo-stream-genre
        'bongo-fields 'bongo-external-fields
        'bongo-header 'bongo-collapsed
        'bongo-marked 'bongo-reference-counted-marker
        'bongo-player 'bongo-played)
  "List of semantic text properties used in Bongo buffers.
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
  (with-silent-modifications
    (let ((inhibit-read-only t)
          (position (bongo-point-at-eol point)))
      (bongo-ensure-final-newline)
      (put-text-property position (1+ position) name value))))
(put 'bongo-line-set-property 'lisp-indent-function 1)

(defun bongo-line-set-properties (properties &optional point)
  "Set the text properties PROPERTIES on the line at POINT.
The text properties will only be set for the terminating newline."
  (with-silent-modifications
    (let ((inhibit-read-only t)
          (position (bongo-point-at-eol point)))
      (bongo-ensure-final-newline)
      (add-text-properties position (1+ position) properties))))

(defun bongo-line-remove-property (name &optional point)
  "Remove the text property NAME from the line at POINT.
The text properties will only be removed from the terminating newline."
  (with-silent-modifications
    (let ((inhibit-read-only t)
          (position (bongo-point-at-eol point)))
      (bongo-ensure-final-newline)
      (remove-text-properties position (1+ position) (list name nil)))))

(defun bongo-keep-text-properties (beg end keys)
  "Keep only some properties in the text between BEG and END.
Remove any property that is not in KEYS."
  (with-silent-modifications
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((properties (text-properties-at (point)))
                 (kept-properties (bongo-filter-plist keys properties))
                 (next (or (next-property-change (point)) (point-max))))
            (set-text-properties (point) next kept-properties)
            (goto-char next)))))))


;;;; Sectioning

(defvar bongo-bind-field-values nil
  "If non-nil, `unbound' field values are joinable.
This variable should only be bound environmentally.
See `bongo-joinable-field-values-p'.")

(defun bongo-joinable-field-values-p (a b)
  "Return non-nil if A and B are joinable field values.
Field values are joinable if they are `equal', except that
 - `unknown' is not joinable with anything (including itself),
 - `unbound' is joinable with anything non-`unknown' if and
     only if `bongo-bind-field-values' is non-nil, and
 - nil is joinable with everything."
  (or (null a) (null b)
      (and bongo-bind-field-values
           (or (and (eq a 'unbound)
                    (not (eq b 'unknown)))
               (and (eq b 'unbound)
                    (not (eq a 'unknown)))))
      (and (not (eq a 'unknown))
           (equal a b))))

(defun bongo-region-joinable-on-field-p (beg end field)
  "Return non-nil if all lines in the region are joinable on FIELD.
FIELD is a symbol naming a field (for example, `artist' or `title').
Two lines are joinable on FIELD if their values for FIELD are joinable.
If there is less than two object lines between BEG and END, return nil.
Also return nil if the value for FIELD is nil throughout the region.
See `bongo-joinable-field-values-p' about what it means for two field
values to be joinable."
  (save-excursion
    (let ((result 'maybe))
      (goto-char beg)
      (bongo-ignore-movement-errors
        (bongo-snap-to-object-line)
        (let ((last (bongo-line-field-value field)))
          (while (progn (bongo-next-object-line)
                        (and (< (point) end) result))
            (let ((current (bongo-line-field-value field)))
              (if (bongo-joinable-field-values-p last current)
                  (when (or last current)
                    (setq result t))
                (setq result nil))))))
      (eq result t))))

;; XXX: This will not work properly unless the list of
;;      potential external fields is completely ordered.
(defun bongo-potential-external-fields-in-region (beg end)
  "Return the names of the fields that could join the region.
That is, the fields on which the region between BEG and END is joinable.
See `bongo-region-joinable-on-field-p' about what it means for a region
to be joinable on a field."
  (let ((fields (reverse bongo-potential-external-fields))
        (joining-fields nil))
    (while fields
      (if (bongo-region-joinable-on-field-p beg end (car fields))
          (when (null joining-fields)
            (setq joining-fields fields))
        (setq joining-fields nil))
      (setq fields (cdr fields)))
    joining-fields))

(defun bongo-potential-external-fields-at-point (&optional point)
  "Return the names of the fields that could join tracks around POINT.
That is, find the largest subset of `bongo-potential-external-fields'
such that some region around POINT is joinable on all those fields.
See `bongo-potential-external-fields-in-region'."
  (let ((interesting-line-p
         (lambda ()
           (catch 'return
             (dolist (field (bongo-line-fields) nil)
               (when (and (bongo-line-field-value field)
                          (memq field bongo-potential-external-fields))
                 (throw 'return t)))))))
    (save-excursion
      (bongo-goto-point point)
      (let ((before-previous
             (bongo-point-before-previous-line-satisfying
              interesting-line-p))
            (after-next
             (bongo-point-after-next-line-satisfying
              interesting-line-p)))
        (if (funcall interesting-line-p)
            (bongo-longest
             (when before-previous
               (bongo-potential-external-fields-in-region
                before-previous (bongo-point-after-line)))
             (when after-next
               (bongo-potential-external-fields-in-region
                (bongo-point-before-line) after-next)))
          (when (and before-previous after-next)
            (bongo-potential-external-fields-in-region
             before-previous after-next)))))))

;;; XXX: This will not work properly unless the list of
;;;      potential external fields is completely ordered.
;;;
;;; This function appears to be unused, and since its
;;; implementation is not general, I commented it out.
;;;
;;; (defun bongo-fields-external-in-region-p (beg end fields)
;;;   "Return non-nil if FIELDS are external between BEG and END.
;;; FIELDS is a list of symbols naming fields (for example, `artist').
;;; More precisely, return nil if and only if there is a field in FIELDS
;;; that is not external for some line in the region."
;;;   (save-excursion
;;;     (let ((result t))
;;;       (goto-char beg)
;;;       (while (and (< (point) end) result)
;;;         (when (< (bongo-line-indentation) (length fields))
;;;           (setq result nil))
;;;         (forward-line))
;;;       result)))

(defun bongo-line-potential-external-fields (&optional point)
  "Return the field names of the line at POINT that could be external.
That is, return the largest subset of `bongo-potential-external-fields'
that could join the line at POINT with the previous object line.
If the line at POINT is the first line, return nil."
  (catch 'return
    (bongo-potential-external-fields-in-region
     (or (bongo-point-before-previous-object-line point)
         (throw 'return nil))
     (bongo-point-after-line point))))

(defun bongo-line-externalizable-fields (&optional point)
  "Return the externalizable fields of the line at POINT.
That is, return the names of all internal fields of the line at POINT
  that could be made external without controversy.
This function respects `bongo-insert-intermediate-headers',
  in order to implement the correct semantics."
  (if bongo-insert-intermediate-headers
      (bongo-set-difference (bongo-set-intersection
                             (bongo-line-proposed-external-fields point)
                             (bongo-line-potential-external-fields point))
                            (bongo-line-external-fields point))
    ;; We are looking for an already existing header line, above the
    ;; current line, such that the proposed external fields below the
    ;; existing header line is a subset of the potential external
    ;; fields of the current line.  If such a header line exists, then
    ;; the externalizable fields of the current line is equal to the
    ;; proposed external fields of the existing header line.
    (let ((potential (bongo-line-potential-external-fields point)))
      (bongo-ignore-movement-errors
        (save-excursion
          ;; We begin the search on the previous line.
          (bongo-previous-object-line)
          ;; If this is a header line, it might be the one we are
          ;; looking for.
          (or (and (bongo-header-line-p)
                   (let ((proposal (bongo-line-external-fields-proposal)))
                     (and (bongo-subset-p potential proposal) proposal)))
              ;; If not, continue the search by backing up to the parent
              ;; header line while there still is one.
              (let (fields)
                (while (and (null fields) (bongo-line-indented-p))
                  (bongo-backward-up-section)
                  (let ((proposal (bongo-line-external-fields-proposal)))
                    (when (bongo-subset-p potential proposal)
                      (setq fields proposal))))
                fields)))))))

(defun bongo-down-section (&optional n)
  "Move to the first object line in the section at point.
With N, repeat that many times.
If there are not enough sections at point, signal an error."
  (interactive "p")
  (or n (setq n 1))
  (while (> n 0)
    (bongo-snap-to-object-line)
    (if (bongo-header-line-p)
        (let ((indentation (bongo-line-indentation)))
          (unless (and (bongo-next-object-line 'no-error)
                       (> (bongo-line-indentation) indentation))
            (error "Empty section")))
      (error "No section here"))
    (setq n (- n 1))))

(defun bongo-backward-up-section (&optional n)
  "Move to the header line of this section.
With N, repeat that many times."
  (interactive "p")
  (or n (setq n 1))
  (while (> n 0)
    (let ((indentation (bongo-line-indentation)))
      (when (zerop indentation)
        (error "Already at the top level"))
      (while (progn (bongo-previous-object-line)
                    (>= (bongo-line-indentation) indentation))))
    (setq n (- n 1))))

(defun bongo-maybe-insert-intermediate-header ()
  "Make sure that the current line has a suitable header.
If the first outer header is too specific, split it in two."
  (when (bongo-line-indented-p)
    (let ((current (bongo-line-external-fields)))
      (save-excursion
        (bongo-backward-up-section)
        (let ((proposal (bongo-line-external-fields-proposal)))
          (unless (bongo-set-equal-p current proposal)
            ;; The new header will not display properly
            ;; until the header below is indented correctly,
            ;; so call `bongo-externalize-fields' on the old
            ;; header before calling `bongo-redisplay-line'
            ;; on the new header.
            (let ((header-line-position (point)))
              (bongo-insert-header current)
              (let ((bongo-bind-field-values t))
                (bongo-externalize-fields))
              (bongo-redisplay-line header-line-position))))))))

(defun bongo-externalize-fields ()
  "Externalize as many fields of the current line as possible.
This function may create a new section header, but only by splitting an
existing header into two (see `bongo-maybe-insert-intermediate-header')."
  (unless (zerop (bongo-line-proposed-indentation))
    (let ((fields (bongo-line-externalizable-fields)))
      (when (> (length fields) (bongo-line-indentation))
        (bongo-line-set-external-fields fields)
        (bongo-maybe-insert-intermediate-header)))))


;;;; Markings

;;; Each track line in Bongo is either marked or unmarked.
;;; The set of marked track lines is called the `marking'.
;;; Many commands default to operating on the marked track
;;; lines whenever the buffer has at least one.
;;;
;;; Marked track lines have non-nil `bongo-marked' properties,
;;; and the values of their `bongo-reference-counted-marker'
;;; properties appear in `bongo-marking', which is a list of
;;; pairs (MARKER . REFERENCE-COUNT) such that each MARKER
;;; points either nowhere (in which case the track line to
;;; which it refers is currently unavailable --- for example,
;;; it may be killed), or to the start of a marked track line.
;;;
;;; The `bongo-marking' list facilitates quickly walking
;;; over all marked track lines, but the double bookkeeping
;;; increases complexity.  (Remember to update both the text
;;; property and the global list.)
;;;
;;; Contrary to earlier versions, marks on killed tracks do
;;; persist when yanking the tracks back, provided that the
;;; same marking is still in effect in the buffer.
;;;
;;; There is another list `bongo-killed-marking', which does
;;; not necessarily hold markers pointing to currently marked
;;; track lines; instead, it stores an inactive marking that
;;; can be restored at a later time.  Most commands operating
;;; on the marked tracks kill the current marking afterwards.
;;;
;;; [In the future, this feature may be extended to a stack.]

(defgroup bongo-track-marks nil
  "Track marks in Bongo."
  :group 'bongo)

(defcustom bongo-track-mark-icon-file-name "track-mark-icon.png"
  "File name of icon to use for track marks in Bongo."
  :type '(choice file (const :tag "None" nil))
  :group 'bongo-track-marks
  :group 'bongo-display)

(defcustom bongo-track-mark-icon-string "*"
  "String to use for track marks in Bongo."
  :type 'string
  :group 'bongo-track-marks
  :group 'bongo-display)

(defun bongo-track-mark-icon-string ()
  "Return the string to use as a marker icon for the current line."
  (let ((image (and bongo-track-mark-icon-file-name (display-images-p)
                    (bongo-find-image bongo-track-mark-icon-file-name
                                      'bongo-marked-track-line))))
    (if image
        (if (bongo-marked-track-line-p)
            (bongo-make-image-string image)
          (bongo-make-image-placeholder-string image))
      (if (bongo-marked-track-line-p)
          bongo-track-mark-icon-string
        (make-string (length bongo-track-mark-icon-string) ? )))))

(defcustom bongo-track-mark-function
  'bongo-default-track-mark
  "Function for displaying track marks in Bongo.
Value is a Function, evaluating to a string or nil.
The values of the expressions are concatenated."
  :type 'function
  :options '(bongo-default-track-mark)
  :group 'bongo-display
  :group 'bongo-track-marks)

(defun bongo-default-track-mark ()
  (concat (bongo-track-mark-icon-string) " "))

(defface bongo-marked-track '((t nil))
  "Face used for marked Bongo tracks."
  :group 'bongo-track-marks
  :group 'bongo-faces)

(defface bongo-marked-track-line
  '((t (:inherit fringe)))
  "Face used for lines of marked Bongo tracks."
  :group 'bongo-track-marks
  :group 'bongo-faces)

(defvar bongo-marking nil
  "List of reference-counted markers pointing at marked track lines.
Reference-counted markers are pairs (MARKER . REFERENCE-COUNT).")
(make-variable-buffer-local 'bongo-marking)

(defun bongo-marked-track-line-p (&optional point)
  "Return non-nil if the line at POINT is a marked track line."
  (bongo-line-get-property 'bongo-marked point))

(defun bongo-unmarked-track-line-p (&optional point)
  "Return non-nil if the line at POINT is an unmarked track line."
  (and (bongo-track-line-p point)
       (not (bongo-marked-track-line-p point))))

(defun bongo-reference-marker (reference-counted-marker)
  "Increase the reference count of REFERENCE-COUNTED-MARKER.
Return REFERENCE-COUNTED-MARKER."
  (prog1 reference-counted-marker
    (setcdr reference-counted-marker
            (+ (cdr reference-counted-marker) 1))))

(defun bongo-unreference-marker (reference-counted-marker)
  "Decrease the reference count of REFERENCE-COUNTED-MARKER.
If the reference count drops to zero, make the marker point nowhere.
Return REFERENCE-COUNTED-MARKER."
  (prog1 reference-counted-marker
    (setcdr reference-counted-marker
            (- (cdr reference-counted-marker) 1))
    (when (zerop (cdr reference-counted-marker))
      (move-marker (car reference-counted-marker) nil))))

(defun bongo-line-reference-counted-marker (&optional point)
  "Return the reference-counted marker for the line at POINT, if any.
The reference-counted marker is a pair (MARKER . REFERENCE-COUNT)."
  (bongo-line-get-property 'bongo-reference-counted-marker point))

(defun bongo-line-marker (&optional point)
  "Return the marker for the line at POINT, if any."
  (car (bongo-line-reference-counted-marker point)))

(defun bongo-reference-line-marker (&optional point)
  "Increase the reference count of the marker for the line at POINT.
Return the reference-counted marker, creating it if necessary.
The reference-counted marker is a pair (MARKER . REFERENCE-COUNT)."
  (let ((reference-counted-marker
         (bongo-line-reference-counted-marker point)))
    (if reference-counted-marker
        (bongo-reference-marker reference-counted-marker)
      (let* ((marker (move-marker (make-marker)
                                  (bongo-point-at-bol point)))
             (reference-counted-marker (cons marker 1)))
        (prog1 reference-counted-marker
          (bongo-line-set-property 'bongo-reference-counted-marker
            reference-counted-marker point))))))

(defun bongo-unreference-line-marker (&optional point)
  "Decrease the reference count of the marker for line at POINT.
If the reference count drops to zero, make the marker point nowhere
and remove the `bongo-reference-counted-marker' property of the line.
Return the reference-counted marker, or signal an error if none exists.
The reference-counted marker is a pair (MARKER . REFERENCE-COUNT)."
  (let ((reference-counted-marker
         (bongo-line-reference-counted-marker point)))
    (when (= (cdr reference-counted-marker) 1)
      (bongo-line-remove-property 'bongo-reference-counted-marker point))
    (bongo-unreference-marker reference-counted-marker)))

(defun bongo-mark-line (&optional point)
  "Mark the track or section at POINT.
Marking a section just marks all tracks in that section."
  (cond ((bongo-header-line-p point)
         (bongo-mark-region (bongo-point-before-next-line point)
                            (bongo-point-after-object point)))
        ((bongo-unmarked-track-line-p point)
         (let ((buffer-undo-list t))
           (add-to-list 'bongo-marking
             (bongo-reference-line-marker point))
           (bongo-line-set-property 'bongo-marked t point)
           (bongo-redisplay-line point))
         (when (listp buffer-undo-list)
           (push (list 'apply 'bongo-unmark-line
                       (bongo-point-at-bol point))
                 buffer-undo-list))
         (let ((line-move-ignore-invisible nil))
           (when (get-text-property (bongo-point-at-eol point) 'invisible)
             (save-excursion
               (bongo-goto-point point)
               (while (bongo-line-indented-p)
                 (bongo-backward-up-section)
                 (bongo-expand))))))))

(defun bongo-mark-line-forward (&optional n)
  "Mark the next N tracks or sections.
Marking a section just marks all tracks in that section.
Leave point after the marked tracks."
  (interactive "p")
  (or n (setq n 1))
  (if (and n (< n 0))
      (bongo-mark-line-backward (- n))
    (dotimes (dummy n)
      (bongo-snap-to-object-line)
      (bongo-mark-line)
      (goto-char (bongo-point-after-object)))))

(defun bongo-mark-line-backward (&optional n)
  "Mark the previous N tracks or sections.
Marking a section just marks all tracks in that section.
Leave point at the topmost affected track."
  (interactive "p")
  (or n (setq n 1))
  (if (< n 0)
      (bongo-mark-line-forward (- n))
    (dotimes (dummy n)
      (bongo-previous-object)
      (bongo-mark-line))))

(defun bongo-mark-region (beg end)
  "Mark all tracks in the region."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (progn (bongo-snap-to-object-line 'no-error)
                  (< (point) end))
      (bongo-mark-line)
      (goto-char (bongo-point-after-object)))))

(defun bongo-mark-forward (&optional n)
  "Mark the next N tracks or sections.
If N is nil and the region is active, mark the region.
Otherwise, if N is nil, mark the track at point."
  (interactive "P")
  (cond ((not (null n))
         (bongo-mark-line-forward (prefix-numeric-value n)))
        ((bongo-region-active-p)
         (bongo-mark-region (region-beginning) (region-end)))
        (t
         (bongo-mark-line-forward))))

(defun bongo-mark-backward (&optional n)
  "Mark the previous N tracks or sections.
If N is nil and the region is active, mark the region.
Otherwise, if N is nil, mark the track at point."
  (interactive "p")
  (bongo-mark-forward (and n (- (prefix-numeric-value n)))))

(defun bongo-unmark-line (&optional point)
  "Unmark the track or section at POINT.
Unmarking a section unmarks all tracks in that section."
  (cond ((bongo-header-line-p point)
         (bongo-unmark-region (bongo-point-before-next-line point)
                              (bongo-point-after-object point)))
        ((bongo-marked-track-line-p point)
         (let ((buffer-undo-list t))
           (let ((marker (bongo-unreference-line-marker point)))
             (setq bongo-marking (delq marker bongo-marking)))
           (bongo-line-remove-property 'bongo-marked point)
           (bongo-redisplay-line point))
         (when (listp buffer-undo-list)
           (push (list 'apply 'bongo-mark-line
                       (bongo-point-at-bol point))
                 buffer-undo-list)))))

(defun bongo-unmark-line-forward (&optional n)
  "Unmark the next N tracks or sections.
Unmarking a section unmarks all tracks in that section.
Leave point after the affected tracks."
  (interactive "p")
  (or n (setq n 1))
  (if (< n 0)
      (bongo-unmark-line-backward (- n))
    (dotimes (dummy n)
      (bongo-snap-to-object-line)
      (bongo-unmark-line)
      (goto-char (bongo-point-after-object)))))

(defun bongo-unmark-line-backward (&optional n)
  "Unmark the previous N tracks or sections.
Unmarking a section unmarks all tracks in that section.
Leave point at the topmost affected track."
  (interactive "p")
  (or n (setq n 1))
  (if (< n 0)
      (bongo-unmark-line-forward (- n))
    (dotimes (dummy n)
      (bongo-previous-object)
      (bongo-unmark-line))))

(defun bongo-unmark-region (beg end)
  "Unmark all tracks in the region."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (progn (bongo-snap-to-object-line 'no-error)
                  (< (point) end))
      (bongo-unmark-line)
      (goto-char (bongo-point-after-object)))))

(defun bongo-unmark-forward (&optional n)
  "Unmark the next N tracks or sections, or the region.
If N is non-nil, unmark the next N tracks or sections.
Otherwise, if the region is active, unmark the region.
Otherwise, just unmark the track at point."
  (interactive "P")
  (cond ((not (null n))
         (bongo-unmark-line-forward (prefix-numeric-value n)))
        ((bongo-region-active-p)
         (bongo-unmark-region (region-beginning) (region-end)))
        (t
         (bongo-unmark-line-forward))))

(defun bongo-unmark-backward (&optional n)
  "Unmark the previous N tracks or sections, or the region.
If N is non-nil, unmark the previous N tracks or sections.
Otherwise, if the region is active, unmark the region.
Otherwise, just unmark the previous track."
  (interactive "P")
  (cond ((not (null n))
         (bongo-unmark-line-forward (- (prefix-numeric-value n))))
        ((bongo-region-active-p)
         (bongo-unmark-region (region-beginning) (region-end)))
        (t
         (bongo-unmark-line-backward))))

(defvar bongo-killed-marking nil
  "Killed marking that can be restored with `bongo-yank-marking'.")
(make-variable-buffer-local 'bongo-killed-marking)

(defun bongo-yank-marking ()
  "Restore the killed marking from `bongo-killed-marking'.
Discard the current marking."
  (interactive)
  (bongo-unmark-all)
  (dolist (reference-counted-marker bongo-killed-marking)
    (when (marker-position (car reference-counted-marker))
      (bongo-mark-line (car reference-counted-marker)))))

(defun bongo-kill-marking ()
  "Kill the current marking and store it in `bongo-killed-marking'.
Discard the old value of `bongo-killed-marking'."
  (interactive)
  (let ((markers bongo-marking)
        (line-move-ignore-invisible nil))
    (setq bongo-marking nil)
    (dolist (marker markers)
      (when (marker-position (car marker))
        (bongo-reference-marker marker)
        (bongo-unmark-line (car marker))))
    (dolist (marker bongo-killed-marking)
      (bongo-unreference-marker marker))
    (setq bongo-killed-marking markers)))

(defun bongo-toggle-marking ()
  "Kill the current marking, if any, or restore the killed one.
See `bongo-kill-marking' and `bongo-yank-marking'."
  (interactive)
  (if bongo-marking
      (bongo-kill-marking)
    (bongo-yank-marking)))

(defun bongo-mark-all ()
  "Mark all tracks in the current buffer."
  (interactive)
  (bongo-mark-region (point-min) (point-max)))

(defun bongo-unmark-all ()
  "Unmark all tracks in the current buffer."
  (interactive)
  (let (bongo-killed-marking)
    (bongo-kill-marking)
    (mapc 'bongo-unreference-marker bongo-killed-marking)))

(defun bongo-mark-track-lines-satisfying (predicate)
  "Mark all track lines satisfying PREDICATE.
Return the number of newly-marked tracks."
  (let ((count 0)
        (line-move-ignore-invisible nil))
    (save-excursion
      (goto-char (point-min))
      (while (and (not (eobp))
                  (bongo-snap-to-object-line 'no-error))
        (when (and (bongo-unmarked-track-line-p)
                   (funcall predicate))
          (bongo-mark-line)
          (setq count (+ count 1)))
        (goto-char (bongo-point-after-line))))
    count))

(defun bongo-unmark-track-lines-satisfying (predicate)
  "Unmark all track lines satisfying PREDICATE.
Return the number of newly-unmarked tracks."
  (let ((count 0)
        (line-move-ignore-invisible nil))
    (save-excursion
      (dolist (reference-counted-marker bongo-marking)
        (when (marker-position (car reference-counted-marker))
          (goto-char (car reference-counted-marker))
          (when (funcall predicate)
            (bongo-unmark-line)
            (setq count (+ count 1))))))
    count))

(defun bongo-mark-by-regexp (regexp key-function)
  "Mark all track lines for which KEY-FUNCTION's value matches REGEXP.
Do not mark lines for which KEY-FUNCTION returns nil.
Return the number of newly-marked tracks."
  (let* ((previous-marking bongo-marking)
         (count (bongo-mark-track-lines-satisfying
                 (lambda ()
                   (let ((key (funcall key-function)))
                     (and key (string-match regexp key)))))))
    (if previous-marking
        (if (zerop count)
            (message "Marked no additional tracks.")
          (message "Marked %d additional track%s." count
                   (if (= count 1) "" "s")))
      (if (zerop count)
          (message "No matching tracks.")
        (message "Marked %d track%s." count
                 (if (= count 1) "" "s"))))
    count))

(defun bongo-unmark-by-regexp (regexp key-function)
  "Unmark all track lines for which KEY-FUNCTION's value matches REGEXP.
Do not unmark lines for which KEY-FUNCTION returns nil.
Return the number of newly-unmarked tracks."
  (if (null bongo-marking)
      (message "No marked tracks.")
    (let ((count (bongo-unmark-track-lines-satisfying
                  (lambda ()
                    (let ((key (funcall key-function)))
                      (and key (string-match regexp key)))))))
      (if (zerop count)
          (message "No matching marked tracks.")
        (message "Unmarked %d track%s." count
                 (if (= count 1) "" "s")))
      count)))

(defvar bongo-regexp-history nil
  "History list for `bongo-read-regexp'.")

(defun bongo-read-regexp (prompt &optional default-value)
  "Read a regexp from the minibuffer, prompting with string PROMPT.
See `read-string' for the meaning of DEFAULT-VALUE."
  (read-string prompt nil 'bongo-regexp-history default-value))

(defun bongo-mark-by-formatted-infoset-regexp (regexp)
  "Mark all lines whose formatted infoset matches REGEXP.
Return the number of newly-marked tracks."
  (interactive (list (bongo-read-regexp "Mark by regexp: ")))
  (bongo-mark-by-regexp regexp (lambda ()
                                 (bongo-format-infoset
                                  (bongo-line-infoset)))))

(defun bongo-mark-by-artist-name-regexp (regexp)
  "Mark all lines whose artist name matches REGEXP.
Return the number of newly-marked tracks."
  (interactive
   (list (bongo-read-regexp "Mark by artist name (regexp): ")))
  (bongo-mark-by-regexp regexp (lambda ()
                                 (bongo-infoset-artist-name
                                  (bongo-line-infoset)))))

(defun bongo-mark-by-album-title-regexp (regexp)
  "Mark all lines whose album title matches REGEXP.
Return the number of newly-marked tracks."
  (interactive
   (list (bongo-read-regexp "Mark by album title (regexp): ")))
  (bongo-mark-by-regexp regexp (lambda ()
                                 (bongo-infoset-album-title
                                  (bongo-line-infoset)))))

(defun bongo-mark-by-album-year-regexp (regexp)
  "Mark all lines whose album year matches REGEXP.
Return the number of newly-marked tracks."
  (interactive
   (list (bongo-read-regexp "Mark by album year (regexp): ")))
  (bongo-mark-by-regexp regexp (lambda ()
                                 (bongo-infoset-album-year
                                  (bongo-line-infoset)))))

(defun bongo-mark-by-track-index-regexp (regexp)
  "Mark all lines whose track index matches REGEXP.
Return the number of newly-marked tracks."
  (interactive
   (list (bongo-read-regexp "Mark by track index (regexp): ")))
  (bongo-mark-by-regexp regexp (lambda ()
                                 (bongo-infoset-track-index
                                  (bongo-line-infoset)))))

(defun bongo-mark-by-track-title-regexp (regexp)
  "Mark all lines whose track title matches REGEXP.
Return the number of newly-marked tracks."
  (interactive
   (list (bongo-read-regexp "Mark by track title (regexp): ")))
  (bongo-mark-by-regexp regexp (lambda ()
                                 (bongo-infoset-track-title
                                  (bongo-line-infoset)))))

(defun bongo-unmark-by-formatted-infoset-regexp (regexp)
  "Unmark all lines whose formatted infoset matches REGEXP.
Return the number of newly-unmarked tracks."
  (interactive (list (bongo-read-regexp "Unmark by regexp: ")))
  (bongo-unmark-by-regexp regexp (lambda ()
                                   (bongo-format-infoset
                                    (bongo-line-infoset)))))

(defun bongo-unmark-by-artist-name-regexp (regexp)
  "Unmark all lines whose artist name matches REGEXP.
Return the number of newly-unmarked tracks."
  (interactive
   (list (bongo-read-regexp "Unmark by artist name (regexp): ")))
  (bongo-unmark-by-regexp regexp (lambda ()
                                   (bongo-infoset-artist-name
                                    (bongo-line-infoset)))))

(defun bongo-unmark-by-album-title-regexp (regexp)
  "Unmark all lines whose album title matches REGEXP.
Return the number of newly-unmarked tracks."
  (interactive
   (list (bongo-read-regexp "Unmark by album title (regexp): ")))
  (bongo-unmark-by-regexp regexp (lambda ()
                                   (bongo-infoset-album-title
                                    (bongo-line-infoset)))))

(defun bongo-unmark-by-album-year-regexp (regexp)
  "Unmark all lines whose album year matches REGEXP.
Return the number of newly-unmarked tracks."
  (interactive
   (list (bongo-read-regexp "Unmark by album year (regexp): ")))
  (bongo-unmark-by-regexp regexp (lambda ()
                                   (bongo-infoset-album-year
                                    (bongo-line-infoset)))))

(defun bongo-unmark-by-track-index-regexp (regexp)
  "Unmark all lines whose track index matches REGEXP.
Return the number of newly-unmarked tracks."
  (interactive
   (list (bongo-read-regexp "Unmark by track index (regexp): ")))
  (bongo-unmark-by-regexp regexp (lambda ()
                                   (bongo-infoset-track-index
                                    (bongo-line-infoset)))))

(defun bongo-unmark-by-track-title-regexp (regexp)
  "Unmark all lines whose track title matches REGEXP.
Return the number of newly-unmarked track lines."
  (interactive
   (list (bongo-read-regexp "Unmark by track title (regexp): ")))
  (bongo-unmark-by-regexp regexp (lambda ()
                                   (bongo-infoset-track-title
                                    (bongo-line-infoset)))))


;;;; The prefix/region/marking convention

(defun bongo-universal-prefix/region/marking-object-command (&optional n)
  "In Bongo, force the prefix/region/marking convention for a command.
That is, read a key sequence and execute the command bound to that sequence
some number of times, each time with point at a different track or section.
If given a prefix argument N, execute the command once for each of the
  next N tracks or sections.
Otherwise, if the region is active, execute the command once for each
  track or section in the region.
Otherwise, if there are any marked tracks, execute the command once for
  each marked track.
Otherwise, just execute the command on the track or section at point.
In all cases (and this is an important detail), execute the command
without any prefix argument, active region or marking.
See `bongo-universal-prefix/region/marking-track-command' for a similar
utility that operates only on tracks (not sections)."
  (interactive "P")
  (let ((command (key-binding (read-key-sequence nil t))))
    (when (eq command 'keyboard-quit)
      (keyboard-quit))
    (save-excursion
      (cond (n
             (setq n (prefix-numeric-value n))
             (let ((mark-active nil)
                   (marking bongo-marking))
               (when marking
                 ;; XXX: This discards the killed marking
                 ;;      as an unfortunate side-effect.
                 (bongo-kill-marking))
               (if (< n 0)
                   (dotimes (dummy (- n))
                     (bongo-previous-object)
                     (save-excursion
                       (command-execute command)))
                 (dotimes (dummy n)
                   (bongo-snap-to-object-line)
                   (save-excursion
                     (command-execute command))
                   (goto-char (bongo-point-after-object))))
               (when marking
                 (bongo-yank-marking))))
            ((bongo-region-active-p)
             (deactivate-mark)
             (let ((marking bongo-marking)
                   (end (move-marker (make-marker) (region-end))))
               (when marking
                 ;; XXX: This discards the killed marking
                 ;;      as an unfortunate side-effect.
                 (bongo-kill-marking))
               (goto-char (region-beginning))
               (while (and (bongo-snap-to-object-line 'no-error)
                           (< (point) end))
                 (command-execute command)
                 (goto-char (bongo-point-after-object)))
               (move-marker end nil)
               (when marking
                 (bongo-yank-marking))))
            (bongo-marking
             (let ((mark-active nil)
                   (marking bongo-marking))
               (bongo-kill-marking)
               (dolist (marker (reverse marking))
                 (catch 'abort
                   (goto-char (or (marker-position (car marker))
                                  (throw 'abort nil)))
                   (command-execute command)))))
            (t
             (let ((mark-active nil)
                   (marking bongo-marking))
               (when marking
                 ;; XXX: This discards the killed marking
                 ;;      as an unfortunate side-effect.
                 (bongo-kill-marking))
               (bongo-snap-to-object-line)
               (command-execute command)
               (when marking
                 (bongo-yank-marking))))))))

(defun bongo-universal-prefix/region/marking-track-command (&optional n)
  "In Bongo, force the prefix/region/marking convention for a command.
That is, read a key sequence and execute the command bound to that sequence
some number of times, each time with point at a different track.
If given a prefix argument N, execute the command once for each of the
  next N tracks.
Otherwise, if the region is active, execute the command once for each
  track in the region.
Otherwise, if there are any marked tracks, execute the command once for
  each marked track.
Otherwise, just execute the command on the track at point.
In all cases (and this is an important detail), execute the command
without any prefix argument, active region or marking.
Unlike `bongo-universal-prefix/region/marking-object-command',
ignore all section structure (i.e., operate only on tracks)."
  (interactive "P")
  (let ((command (key-binding (read-key-sequence nil t))))
    (when (eq command 'keyboard-quit)
      (keyboard-quit))
    (save-excursion
      (cond (n
             (setq n (prefix-numeric-value n))
             (let ((mark-active nil)
                   (marking bongo-marking))
               (when marking
                 ;; XXX: This discards the killed marking
                 ;;      as an unfortunate side-effect.
                 (bongo-kill-marking))
               (if (< n 0)
                   (dotimes (dummy (- n))
                     (goto-char (or (bongo-point-at-previous-track-line)
                                    (signal 'bongo-no-previous-object nil)))
                     (save-excursion
                       (command-execute command)))
                 (dotimes (dummy n)
                   (unless (bongo-track-line-p)
                     (goto-char (or (bongo-point-at-next-track-line)
                                    (signal 'bongo-no-next-object nil))))
                   (save-excursion
                     (command-execute command))
                   (forward-line 1)))
               (when marking
                 (bongo-yank-marking))))
            ((bongo-region-active-p)
             (deactivate-mark)
             (let ((marking bongo-marking)
                   (end (move-marker (make-marker) (region-end))))
               (when marking
                 ;; XXX: This discards the killed marking
                 ;;      as an unfortunate side-effect.
                 (bongo-kill-marking))
               (goto-char (region-beginning))
               (while (and (or (bongo-track-line-p)
                               (goto-char (or (bongo-point-at-next-track-line)
                                              (point-max))))
                           (< (point) end))
                 (command-execute command)
                 (forward-line 1))
               (move-marker end nil)
               (when marking
                 (bongo-yank-marking))))
            (bongo-marking
             (let ((mark-active nil)
                   (marking bongo-marking))
               (bongo-kill-marking)
               (dolist (marker (reverse marking))
                 (catch 'abort
                   (goto-char (or (marker-position (car marker))
                                  (throw 'abort nil)))
                   (command-execute command)))))
            (t
             (let ((mark-active nil)
                   (marking bongo-marking))
               (when marking
                 ;; XXX: This discards the killed marking
                 ;;      as an unfortunate side-effect.
                 (bongo-kill-marking))
               (unless (bongo-track-line-p)
                 (goto-char (or (bongo-point-at-next-track-line)
                                (signal 'bongo-no-next-object nil))))
               (command-execute command)
               (when marking
                 (bongo-yank-marking))))))))


;;;; Backends

(defun bongo-backend (backend-name)
  "Return the backend called BACKEND-NAME.
If BACKEND-NAME is not a symbol, just return it."
  (if (symbolp backend-name)
      (get backend-name 'bongo-backend)
    backend-name))

(defun bongo-backend-name (backend)
  "Return the name of BACKEND."
  (car (bongo-backend backend)))

(defun bongo-backend-pretty-name (backend)
  "Return BACKEND's pretty name."
  (or (bongo-backend-get backend 'pretty-name)
      (symbol-name (bongo-backend-name backend))))

(defun bongo-backend-get (backend property)
  "Return the value of BACKEND's PROPERTY."
  (bongo-alist-get (cdr (bongo-backend backend)) property))

(defun bongo-backend-put (backend property value)
  "Set BACKEND's PROPERTY to VALUE."
  (setcdr (bongo-backend backend)
          (bongo-alist-put (cdr (bongo-backend backend))
                           property value)))
(put 'bongo-backend-put 'lisp-indent-function 2)

(defun bongo-backend-push (backend property element)
  "Push ELEMENT to the list stored in BACKEND's PROPERTY."
  (bongo-backend-put backend property
    (cons element (bongo-backend-get backend property))))
(put 'bongo-backend-push 'lisp-indent-function 2)

(defun bongo-backend-constructor (backend)
  "Return BACKEND's constructor."
  (bongo-backend-get backend 'constructor))

(defun bongo-backend-program-name (backend)
  "Return BACKEND's program name."
  (let ((program-name (bongo-backend-get backend 'program-name)))
    (if (symbolp program-name)
        (symbol-value program-name)
      program-name)))

(defun bongo-backend-program-arguments (backend)
  "Return BACKEND's program argument list."
  (bongo-backend-get backend 'program-arguments))

(defun bongo-backend-file-name-transformers (backend)
  "Return BACKEND's file name transformers."
  (bongo-backend-get backend 'file-name-transformers))

(defun bongo-transform-file-name (file-name transformer)
  "Transform FILE-NAME according to TRANSFORMER.
If TRANSFORMER is a function, simply apply it to FILE-NAME.
If TRANSFORMER is a pair (REGEXP . REPLACEMENT), replace any matches
  of REGEXP in FILE-NAME by REPLACEMENT.
If TRANSFORMER is a pair ((REGEXP . N) . REPLACEMENT), do the same,
  except replace the Nth sumbatch of REGEXP for each match."
  (cond ((functionp transformer)
         (funcall transformer file-name))
        ((consp transformer)
         (if (stringp (car transformer))
             (replace-regexp-in-string (car transformer)
                                       (cdr transformer)
                                       file-name 'fixed-case)
           (replace-regexp-in-string (caar transformer)
                                     (cdr transformer)
                                     file-name 'fixed-case
                                     nil (cdar transformer))))
        (t (error "Malformed file name transformer: %S"
                  transformer))))

(defun bongo-file-name-matches-p (file-name matcher)
  "Return non-nil if FILE-NAME matches MATCHER.
MATCHER is of the form (TYPE-MATCHER . VALUE-MATCHER),
where TYPE-MATCHER is either `local-file' or a string
of the form \"URI-SCHEME:\", or a list of such atoms.
The possible values of VALUE-MATCHER are listed below.

If it is t, return non-nil immediately.
If it is a string, treat it as a regular expression;
  return non-nil if FILE-NAME matches VALUE-MATCHER.
If it is a symbol, treat it as a function name;
  return non-nil if (VALUE-MATCHER FILE-NAME) returns non-nil.
If it is a list of strings, treat it as a set of file name extensions;
  return non-nil if the extension of FILE-NAME appears in VALUE-MATCHER.
Otherwise, signal an error."
  (let ((type-matcher (car matcher))
        (value-matcher (cdr matcher)))
    (when (let* ((uri-scheme (bongo-uri-scheme file-name))
                 (needed-type-matcher
                  (if uri-scheme
                      (concat uri-scheme ":")
                    'local-file)))
            (or (equal type-matcher needed-type-matcher)
                (and (listp type-matcher)
                     (member needed-type-matcher type-matcher))))
      (cond ((eq value-matcher t) t)
            ((stringp value-matcher)
             (string-match value-matcher file-name))
            ((symbolp value-matcher)
             (funcall value-matcher file-name))
            ((and (listp value-matcher) (stringp (car value-matcher)))
             (let ((actual-extension
                    (downcase (or (file-name-extension file-name) ""))))
               (catch 'match
                 (dolist (extension value-matcher nil)
                   (when (string-equal extension actual-extension)
                     (throw 'match t))))))
            (t (error "Malformed file name matcher: %S"
                      value-matcher))))))

(defun bongo-backend-matchers ()
  "Return a list of all backend matchers in order of priority."
  (append bongo-custom-backend-matchers
          (apply 'nconc
                 (mapcar (lambda (matcher)
                           (when (memq (car matcher) bongo-enabled-backends)
                             (list matcher)))
                         bongo-backend-matchers))))

(defun bongo-backend-for-file (file-name)
  "Return the name of the backend to use for playing FILE-NAME."
  (let ((backend-name nil))
    (let ((matchers (bongo-backend-matchers)))
      (while (and matchers (null backend-name))
        (if (and (bongo-file-name-matches-p file-name (cdar matchers))
                 (bongo-backend (caar matchers)))
            (setq backend-name (caar matchers))
          (setq matchers (cdr matchers)))))
    (unless (eq backend-name 'ignore)
      backend-name)))

(defun bongo-completing-read (&rest args)
  (if (and (boundp 'ido-mode) ido-mode)
      ;; XXX: This shouldn't be necessary.
      (let ((ido-merged-indicator nil))
        (apply 'ido-completing-read args))
    (apply 'completing-read args)))

(defun bongo-set-backend-for-track (backend &optional point)
  "Specify that BACKEND is to be used for playing the track at POINT."
  (interactive
   (let* ((backends
           (cons (cons "(auto)" "nil")
                 (mapcar (lambda (backend)
                           (cons (bongo-backend-pretty-name backend)
                                 ;; Putting symbols in this
                                 ;; alist causes trouble.
                                 (symbol-name backend)))
                         bongo-backends)))
          (current-backend
           (car (rassoc (bongo-line-get-property 'bongo-backend)
                        backends)))
          (completion-ignore-case t))
     (list (intern (cdr (assoc (bongo-completing-read
                                (format (concat "Backend for playing this "
                                                "track (default `%s'): ")
                                        (if current-backend
                                            (bongo-backend-pretty-name
                                             current-backend)
                                          "(auto)"))
                                backends nil t nil nil current-backend)
                               backends))))))
  (if backend
      (bongo-line-set-property 'bongo-backend backend point)
    (bongo-line-remove-property 'bongo-backend point)))


;;;; Last.fm

(define-minor-mode bongo-lastfm-mode
  "Toggle Bongo Last.fm mode in the current buffer.
In Bongo Last.fm mode, information about played tracks is automatically
sumbitted to Last.fm (using `lastfm-submit').

Interactively with no prefix argument, toggle the mode.
With zero or negative ARG, turn the mode off.
With any other ARG, turn the mode on.

You can use Bongo Global Last.fm mode (see `bongo-global-lastfm-mode')
to automatically enable Bongo Last.fm mode in Bongo playlist buffers."
  :lighter " Last.fm"
  (if (bongo-playlist-buffer-p)
      (when (bongo-playing-p)
        (if bongo-lastfm-mode
            (bongo-start-lastfm-timer bongo-player)
          (bongo-cancel-lastfm-timer bongo-player)))
    (let ((value bongo-lastfm-mode))
      (kill-local-variable 'bongo-lastfm-mode)
      (when (not (null value))
        (error "Bongo Last.fm mode can only be enabled in playlists")))))

(defun bongo-turn-on-lastfm-mode-if-applicable ()
  (when (bongo-playlist-buffer-p)
    (bongo-lastfm-mode 1)))

(define-global-minor-mode bongo-global-lastfm-mode
  bongo-lastfm-mode bongo-turn-on-lastfm-mode-if-applicable
  :initialize 'custom-initialize-default
  :init-value (and (boundp 'lastfmsubmit-program-name)
                   lastfmsubmit-program-name
                   (not (null (executable-find lastfmsubmit-program-name))))
  :group 'bongo)

(defun bongo-lastfm-submit (infoset length)
  "Submit song information to Last.fm using `lastfm-submit'.
INFOSET must contain at least the artist name and the track title.
LENGTH is the track length in seconds."
  (require 'lastfm-submit)
  (let ((artist-name (bongo-infoset-artist-name infoset))
        (track-title (bongo-infoset-track-title infoset))
        (formatted-infoset (bongo-format-infoset infoset)))
    (if (or (null length) (null artist-name) (null track-title))
        (error "Cannot submit to Last.fm due to missing %s: %s"
               (cond ((null artist-name) "artist name")
                     ((null track-title) "track title")
                     ((null length) "track length"))
               formatted-infoset)
      (lastfm-submit artist-name track-title
                     (number-to-string (round length))
                     (bongo-infoset-album-title infoset))
      (message "Submitted to Last.fm: %s" formatted-infoset))))

(defun bongo-lastfm-submit-player (player)
  "Submit PLAYER's track to Last.fm, if not submitted already.
This function looks at and may change PLAYER's `lastfm-submitted' property.
See `bongo-lastfm-submit'."
  ;; In a perfect world, the `lastfm-sumbitted' property
  ;; would not be necessary.  We use it as a precaution in
  ;; case we miss cancelling the Last.fm timer.  That is a
  ;; serious bug in itself, but there is no reason to submit
  ;; the same song over and over just because we have a bug.
  (unless (bongo-player-get player 'lastfm-sumbitted)
    (bongo-lastfm-submit (bongo-player-infoset player)
                         (bongo-player-total-time player))
    (bongo-player-put player 'lastfm-sumbitted t)))

(defun bongo-lastfm-submit-current ()
  "Sumbit the currently playing track to Last.fm."
  (interactive)
  (with-bongo-playlist-buffer
    (if (bongo-playing-p)
        (bongo-lastfm-submit-player bongo-player)
      (error "No active player"))))

(defun bongo-cancel-lastfm-timer (player)
  (when (bongo-player-get player 'lastfm-timer)
    (cancel-timer (bongo-player-get player 'lastfm-timer))
    (bongo-player-put player 'lastfm-timer nil)))

(defun bongo-lastfm-tick (player)
  ;; The Audioscrobbler website says that each song should
  ;; be submitted ``when it is 50% or 240 seconds complete,
  ;; whichever comes first.''
  (when (or (>= (bongo-player-elapsed-time player)
                (/ (bongo-player-total-time player) 2.0))
            (>= (bongo-player-elapsed-time player) 240))
    (when (or (null (bongo-player-buffer player))
              (with-current-buffer (bongo-player-buffer player)
                bongo-lastfm-mode))
      (bongo-lastfm-submit-player player))
    (bongo-cancel-lastfm-timer player)))

(defun bongo-start-lastfm-timer (player)
  (unless (bongo-player-get player 'lastfm-timer)
    (when (and (bongo-player-elapsed-time player)
               (bongo-player-total-time player)
               ;; ``Songs with a duration of less than 30
               ;; seconds should not be submitted,'' says the
               ;; Audioscrobbler website.
               (>= (bongo-player-total-time player) 30))
      (bongo-player-put player 'lastfm-timer
        (run-with-timer 1 1 'bongo-lastfm-tick player)))))

(defun bongo-restart-lastfm-timer (player)
  (bongo-cancel-lastfm-timer player)
  (bongo-start-lastfm-timer player))


;;;; Sprinkling

(defcustom bongo-sprinkle-amount 5
  "Minimum number of unplayed tracks in Bongo Sprinkle mode."
  :type 'integer
  :group 'bongo)

(defun bongo-sprinkle-until-saturated ()
  "Make sure the playlist has some minimum number of unplayed tracks.
Specifically, enqueue random tracks using `bongo-sprinkle' until the
playlist contains at least `bongo-sprinkle-amount' unplayed tracks."
  (interactive)
  (with-bongo-playlist-buffer
    (bongo-sprinkle (- bongo-sprinkle-amount
                       (bongo-count-lines-satisfying
                        'bongo-unplayed-track-line-p)))))

(defun bongo-sprinkle (&optional n)
  "Enqueue some random tracks to the end of the playlist.
With prefix argument N, enqueue that many random tracks.

The random tracks are taken from the library buffer named
  by the variable `bongo-library-buffer'.
If `bongo-library-buffer' is nil, the tracks are taken from
  the nearest library buffer.
If there are no library buffers, the tracks are taken from
  the nearest playlist buffer other than the current one.
If there are no other playlist buffers, as a last resort,
  the tracks are taken from the current playlist buffer.

If the buffer found by the algorithm described above does
not contain any track lines, just signal an error."
  (interactive "p")
  (with-bongo-playlist-buffer
    (dotimes (dummy n)
      (let ((bongo-playlist-buffer (current-buffer)))
        (with-current-buffer
            (or bongo-library-buffer
                (bongo-recent-library-buffer)
                (bongo-recent-playlist-buffer)
                (current-buffer))
          (if (bongo-track-lines-exist-p)
              (save-excursion
                (goto-char (bongo-point-at-random-track-line))
                (let ((bongo-inhibit-recenter-after-enqueue t))
                  (bongo-append-enqueue-line)))
            (error (concat "Recently selected library or playlist "
                           "buffer contains no track lines"))))))))


;;;; Players

(defcustom bongo-player-started-hook '(bongo-show)
  "Normal hook run when a Bongo player is started.
This hook is only run for players started in Bongo buffers."
  :options '(bongo-show)
  :type 'hook
  :group 'bongo)

(defvar bongo-player-started-functions nil
  "Abnormal hook run when a player is started.")

(defun bongo-fringe-icon-size ()
  "Return the size to use for fringe icons."
  (if (null window-system)
      ;; On Multi-TTY Emacs, `window-system' is a frame-local
      ;; variable, so default to the smallest size.
      11
    (if (>= (or (bongo-face-height 'fringe) 0) 18) 18 11)))

(defvar bongo-playing-track-marker nil
  "Marker pointing at the currently playing track, if any.
As soon as the track is paused or stopped, this marker is set to
point to nowhere, and another marker assumes its role instead.")
(make-variable-buffer-local 'bongo-playing-track-marker)
(put 'bongo-playing-track-marker 'overlay-arrow-bitmap
       (cl-ecase (bongo-fringe-icon-size)
         (11 'bongo-playing-11)
         (18 'bongo-playing-18)))

(defvar bongo-paused-track-marker nil
  "Marker pointing at the currently paused track, if any.
As soon as the track is unpaused or stopped, this marker is set to
point to nowhere, and another marker assumes its role instead.")
(make-variable-buffer-local 'bongo-paused-track-marker)
(put 'bongo-paused-track-marker 'overlay-arrow-bitmap
       (cl-ecase (bongo-fringe-icon-size)
         (11 'bongo-paused-11)
         (18 'bongo-paused-18)))

(defvar bongo-stopped-track-marker nil
  "Marker pointing at the last stopped track, if any.
As soon as another track starts playing, this marker is set to
point to nowhere.")
(make-variable-buffer-local 'bongo-stopped-track-marker)
(put 'bongo-stopped-track-marker 'overlay-arrow-bitmap
     (cl-ecase (bongo-fringe-icon-size)
       (11 'bongo-stopped-11)
       (18 'bongo-stopped-18)))

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

(defun bongo-play-file (file-name &optional backend)
  "Start playing FILE-NAME using BACKEND and return the new player.
If BACKEND is omitted or nil, Bongo will try to find the best player
  backend for FILE-NAME (using `bongo-backend-for-file').
This function runs `bongo-player-started-functions'."
  (let* ((backend (or (and backend (bongo-backend backend))
                      (bongo-backend-for-file file-name)
                      (error "Don't know how to play `%s'" file-name)))
         (constructor (bongo-backend-constructor backend))
         (transformers (bongo-backend-get backend 'file-name-transformers))
         (extra-arguments nil))
    (dolist (transformer transformers)
      (let ((transformed-file-name
             (bongo-transform-file-name file-name transformer)))
        (when transformed-file-name
          (setq file-name transformed-file-name)
          (when (consp file-name)
            (setq extra-arguments (nconc (cdr file-name) extra-arguments))
            (setq file-name (car file-name))))))
    (let* ((player (funcall constructor file-name extra-arguments))
           (process (bongo-player-process player)))
      (prog1 player
        (when (and bongo-player-process-priority
                   process (eq 'run (process-status process)))
          (bongo-renice (process-id process)
                        bongo-player-process-priority))
        (when bongo-lastfm-mode
          (bongo-player-put player 'lastfm-timer
            (run-with-timer 5 nil 'bongo-restart-lastfm-timer player)))
        (run-hook-with-args 'bongo-player-started-functions player)))))

(defcustom bongo-player-finished-hook nil
  "Normal hook run when a Bongo player in Bongo mode finishes.
This hook is only run for players started in Bongo buffers."
  :options '((lambda () (bongo-show) (sit-for 2)))
  :type 'hook
  :group 'bongo)

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
  (save-current-buffer
    (when (buffer-live-p (bongo-player-buffer player))
      (set-buffer (bongo-player-buffer player)))
    (run-hook-with-args 'bongo-player-succeeded-functions player)
    (bongo-player-finished player)))

(defun bongo-player-failed (player)
  "Run the hooks appropriate for when PLAYER has failed."
  (save-current-buffer
    (when (buffer-live-p (bongo-player-buffer player))
      (set-buffer (bongo-player-buffer player)))
    (run-hook-with-args 'bongo-player-failed-functions player)
    (bongo-player-finished player)))

(defun bongo-player-killed (player)
  "Run the hooks appropriate for when PLAYER was killed."
  (let ((process (bongo-player-process player)))
    (message "Process `%s' received fatal signal %s"
             (process-name process) (process-exit-status process)))
  (save-current-buffer
    (when (buffer-live-p (bongo-player-buffer player))
      (set-buffer (bongo-player-buffer player)))
    (run-hook-with-args 'bongo-player-killed-functions player)
    (bongo-player-finished player)))

(defun bongo-perform-next-action ()
  "Perform the next Bongo action, if any.
The next action is specified by `bongo-next-action'."
  (interactive)
  (with-bongo-playlist-buffer
    (when bongo-next-action
      (funcall bongo-next-action))))

(defun bongo-player-finished (player)
  "Run the hooks appropriate for when PLAYER has finished.
Then perform the next action according to `bongo-next-action'.
You should not call this function directly."
  (save-current-buffer
    (when (buffer-live-p (bongo-player-buffer player))
      (set-buffer (bongo-player-buffer player)))
    (run-hook-with-args 'bongo-player-finished-functions player)
    (when (bongo-buffer-p)
      (when bongo-mark-played-tracks
        (bongo-mark-current-track-line-as-played))
      (run-hooks 'bongo-player-finished-hook))
    (bongo-player-stopped player)
    (when (bongo-buffer-p)
      (bongo-perform-next-action))))

(defcustom bongo-player-explicitly-stopped-hook nil
  "Normal hook run after a Bongo player is explicitly stopped.
This hook is only run for players started in Bongo buffers."
  :type 'hook
  :group 'bongo)

(defvar bongo-player-explicitly-stopped-functions nil
  "Abnormal hook run after a Bongo player is explicitly stopped.")

(defun bongo-player-explicitly-stopped (player)
  "Run the hooks appropriate for when PLAYER was explicitly stopped."
  (save-current-buffer
    (when (buffer-live-p (bongo-player-buffer player))
      (set-buffer (bongo-player-buffer player)))
    (run-hook-with-args 'bongo-player-explicitly-stopped-functions player)
    (when (bongo-buffer-p)
      (run-hooks 'bongo-player-explicitly-stopped-hook))
    (bongo-player-stopped player)))

(defvar bongo-player-stopped-functions nil
  "Abnormal hook run when a player exits or is stopped.")

(defcustom bongo-player-stopped-hook nil
  "Normal hook run after a Bongo player exits or is stopped.
This hook is only run for players started in Bongo buffers."
  :type 'hook
  :group 'bongo)

(defun bongo-player-stopped (player)
  "Run the hooks appropriate for when PLAYER exited or was stopped."
  (save-current-buffer
    (when (buffer-live-p (bongo-player-buffer player))
      (set-buffer (bongo-player-buffer player)))
    (bongo-cancel-lastfm-timer player)
    (run-hook-with-args 'bongo-player-stopped-functions player)
    (when (bongo-buffer-p)
      (bongo-set-current-track-marker bongo-stopped-track-marker)
      (catch 'abort
        (bongo-redisplay-line (or (bongo-point-at-current-track-line)
                                  (throw 'abort nil))))
      (when bongo-header-line-mode
        (bongo-update-header-line-string))
      (when bongo-mode-line-indicator-mode
        (bongo-update-mode-line-indicator-string))
      (run-hooks 'bongo-player-stopped-hook))
    (when (bufferp bongo-seek-buffer)
      (bongo-seek-redisplay))))

(defcustom bongo-player-paused/resumed-hook nil
  "Normal hook run after a Bongo player is paused or resumed.
This hook is only run for players started in Bongo buffers."
  :type 'hook
  :group 'bongo)

(defvar bongo-player-paused/resumed-functions nil
  "Abnormal hook run after a Bongo player is paused or resumed.")

(defun bongo-player-paused/resumed (player)
  "Run the hooks appropriate for when PLAYER has paused or resumed."
  (save-current-buffer
    (when (buffer-live-p (bongo-player-buffer player))
      (set-buffer (bongo-player-buffer player)))
    (run-hook-with-args 'bongo-player-paused/resumed-functions player)
    (when (bongo-buffer-p)
      (bongo-set-current-track-marker (if (bongo-paused-p)
                                          bongo-paused-track-marker
                                        bongo-playing-track-marker))
      (when bongo-header-line-mode
        (bongo-update-header-line-string))
      (when bongo-mode-line-indicator-mode
        (bongo-update-mode-line-indicator-string))
      (run-hooks 'bongo-player-paused/resumed-hook))))

(defcustom bongo-player-sought-hook nil
  "Normal hook run after a Bongo player seeks.
This hook is only run for players started in Bongo buffers."
  :type 'hook
  :group 'bongo)

(defvar bongo-player-sought-functions nil
  "Abnormal hook run after a Bongo player seeks.")

(defun bongo-player-sought (player new-elapsed-time)
  "Run the hooks appropriate for when PLAYER has sought.
NEW-ELAPSED-TIME is the new value of the `elapsed-time' property."
  (save-current-buffer
    (when (buffer-live-p (bongo-player-buffer player))
      (set-buffer (bongo-player-buffer player)))
    (bongo-cancel-lastfm-timer player)
    (bongo-player-put player 'elapsed-time
      (max 0 (if (bongo-player-total-time player)
                 (min new-elapsed-time
                      (bongo-player-total-time player ))
               new-elapsed-time)))
    (bongo-player-times-changed player)
    (bongo-player-put player 'last-seek-time (current-time))
    (run-hook-with-args 'bongo-player-sought-functions player)
    (when (bongo-buffer-p)
      (run-hooks 'bongo-player-sought-hook))))

(defvar bongo-player-times-changed-functions nil
  "Abnormal hook run after one of the times of a Bongo player changes.
By ``one of the times'' is meant elapsed time or total time.")

(defun bongo-player-times-changed (player)
  "Run the hooks for when one of the times of PLAYER has changed."
  (let ((current-seconds (cl-second (current-time))))
    (when (> current-seconds bongo-player-times-last-updated)
      (setq bongo-player-times-last-updated current-seconds)
      (save-current-buffer
	(when (buffer-live-p (bongo-player-buffer player))
	  (set-buffer (bongo-player-buffer player)))
	(run-hook-with-args 'bongo-player-times-changed-functions player)
	(when (bongo-buffer-p)
	  (when bongo-header-line-mode
	    (bongo-update-header-line-string))
	  (when bongo-mode-line-indicator-mode
	    (bongo-update-mode-line-indicator-string))
	  (catch 'abort
	    ;; While `save-excursion' is good for making sure
	    ;; that point stays on the same line in all cases,
	    ;; it cannot bring point back to the original column
	    ;; (because of how `bongo-redisplay-line' works).
	    (let* ((line-move-ignore-invisible nil)
		   (point (when (bongo-current-track-line-p)
			    (point))))
	      (save-excursion
		(goto-char (or (bongo-point-at-current-track-line)
			       (throw 'abort nil)))
		(bongo-line-set-property 'bongo-track-length
		  (bongo-player-total-time player))
		(bongo-redisplay-line))
	      (when point
		(goto-char point)))))
	(when (bufferp bongo-seek-buffer)
	  (bongo-seek-redisplay))))))

(defcustom bongo-player-metadata-changed-hook '(bongo-show)
  "Normal hook run when a Bongo player receives new metadata."
  :options '(bongo-show)
  :type 'hook
  :group 'bongo)

(defvar bongo-player-metadata-changed-functions nil
  "Abnormal hook run when a Bongo player receives new metadata.")

(defun bongo-player-metadata-changed (player)
  "Take actions appropriate for when PLAYER's metadata changed.
Changing metadata is provided by some Internet radio streams.
This function runs the hooks `bongo-player-metadata-changed-hook'
  and `bongo-player-metadata-changed-functions'.
The following metadata properties are currently used:
  `stream-name'        - The name of the stream.
  `stream-genre'       - The genre of the stream.
  `stream-part-title'  - The title of the part that is
                         currently being streamed."
  (save-current-buffer
    (when (buffer-live-p (bongo-player-buffer player))
      (set-buffer (bongo-player-buffer player)))
    (run-hook-with-args 'bongo-player-metadata-changed-functions player)
    (when (bongo-buffer-p)
      (save-excursion
        (goto-char (bongo-point-at-current-track-line))
        (bongo-line-set-property 'bongo-stream-name
          (bongo-player-get player 'stream-name))
        (bongo-line-set-property 'bongo-stream-genre
          (bongo-player-get player 'stream-genre))
        (bongo-player-put player 'infoset (bongo-line-infoset))
        (when bongo-header-line-mode
          (bongo-update-header-line-string))
        (when bongo-mode-line-indicator-mode
          (bongo-update-mode-line-indicator-string))
        (bongo-redisplay-line))
      (run-hooks 'bongo-player-metadata-changed-hook))))

(defun bongo-player-backend-name (player)
  "Return the name of PLAYER's backend."
  (car player))

(defun bongo-player-backend (player)
  "Return PLAYER's backend object."
  (bongo-backend (bongo-player-backend-name player)))

(defun bongo-player-get (player property)
  "Return the value of PLAYER's PROPERTY."
  (bongo-alist-get (cdr player) property))

(defun bongo-player-put (player property value)
  "Set PLAYER's PROPERTY to VALUE."
  (setcdr player (bongo-alist-put (cdr player) property value)))
(put 'bongo-player-put 'lisp-indent-function 2)

(defun bongo-player-push (player property element)
  "Push ELEMENT to the list stored in PLAYER's PROPERTY."
  (bongo-player-put player property
    (cons element (bongo-player-get player property))))
(put 'bongo-player-push 'lisp-indent-function 2)

(defun bongo-player-pop (player property)
  "Remove and return the head of PLAYER's PROPERTY."
  (let ((first-cell (bongo-player-get player property)))
    (prog1 (car first-cell)
      (bongo-player-put player property (cdr first-cell)))))

(defun bongo-player-shift (player property)
  "Remove and return the last element of PLAYER's PROPERTY."
  (let ((first-cell (bongo-player-get player property)))
    (if (null (cdr first-cell))
        (bongo-player-pop player property)
      (let ((penultimate-cell (last first-cell 2)))
        (prog1 (cadr penultimate-cell)
          (setcdr penultimate-cell nil))))))

(defun bongo-player-call (player method &rest arguments)
  "Call METHOD on PLAYER with extra ARGUMENTS."
  (apply (bongo-player-get player method) player arguments))

(defun bongo-player-call-with-default (player method default &rest arguments)
  "Call METHOD on PLAYER with extra ARGUMENTS.
If PLAYER has no property called METHOD, use DEFAULT instead."
  (apply (or (bongo-player-get player method) default) player arguments))

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
  (or (bongo-player-get player 'infoset)
      (bongo-infoset-from-file-name (bongo-player-file-name player))))

(defun bongo-player-show-infoset (player)
  "Display in the minibuffer what PLAYER is playing."
  (message (bongo-format-infoset (bongo-player-infoset player))))

(defun bongo-player-running-p (player)
  "Return non-nil if PLAYER's process is currently running."
  (bongo-player-call-with-default
   player 'running-p 'bongo-default-player-running-p))

(defun bongo-player-explicitly-stopped-p (player)
  "Return non-nil if PLAYER was explicitly stopped."
  (bongo-player-get player 'explicitly-stopped))

(defun bongo-player-stop (player)
  "Tell PLAYER to stop playback completely.
When this function returns, PLAYER will no longer be usable."
  (bongo-player-put player 'explicitly-stopped t)
  (bongo-player-call-with-default
   player 'stop 'bongo-default-player-stop))

(defun bongo-player-interactive-p (player)
  "Return non-nil if PLAYER's process is interactive.
Interactive processes may support pausing and seeking."
  (bongo-player-call-with-default
   player 'interactive-p 'bongo-default-player-interactive-p))

(defun bongo-player-pausing-supported-p (player)
  "Return non-nil if PLAYER supports pausing."
  (bongo-player-call-with-default
   player 'pausing-supported-p 'bongo-default-player-pausing-supported-p))

(defun bongo-player-paused-p (player)
  "Return non-nil if PLAYER is paused."
  (bongo-player-call-with-default
   player 'paused-p 'bongo-default-player-paused-p))

(defun bongo-player-pause-signal (player)
  "Return the value of PLAYER's backend's `pause-signal' property."
  (bongo-backend-get (bongo-player-backend player) 'pause-signal))

(defun bongo-player-pause/resume (player)
  "Tell PLAYER to toggle its paused state.
If PLAYER does not support pausing, signal an error."
  (bongo-player-call-with-default
   player 'pause/resume 'bongo-default-player-pause/resume))

(defun bongo-player-seeking-supported-p (player)
  "Return non-nil if PLAYER supports seeking."
  (bongo-player-call-with-default
   player 'seeking-supported-p 'bongo-default-player-seeking-supported-p))

(defun bongo-player-seek-by (player n)
  "Tell PLAYER to seek to absolute position N.
If PLAYER does not support seeking, signal an error."
  (bongo-player-call-with-default
   player 'seek-by 'bongo-default-player-seek-by n))

(defun bongo-player-seek-to (player n)
  "Tell PLAYER to seek N units relative to the current position.
If PLAYER does not support seeking, signal an error."
  (bongo-player-call-with-default
   player 'seek-to 'bongo-default-player-seek-to n))

(defun bongo-player-elapsed-time (player)
  "Return the number of seconds PLAYER has played so far.
If the player backend cannot report this, return nil.
The return value is always a floating point number or nil."
  (let ((value (bongo-player-call-with-default
                player 'get-elapsed-time
                'bongo-default-player-get-elapsed-time)))
    (and value (>= value 0) (float value))))

(defun bongo-player-total-time (player)
  "Return the total number of seconds PLAYER has and will use.
If the player backend cannot report this, return nil.
The return value is always a floating point number or nil."
  (let ((value (bongo-player-call-with-default
                player 'get-total-time
                'bongo-default-player-get-total-time)))
    ;; In most cases, zero means "I don't know".
    (and value (> value 0) (float value))))

(defun bongo-player-update-elapsed-time (player elapsed-time)
  "Set PLAYER's `elapsed-time' property to ELAPSED-TIME,
unless PLAYER's last seek happened less than N seconds ago, where N
is the value of PLAYER's `time-update-delay-after-seek' property."
  (let ((delay (bongo-player-get player 'time-update-delay-after-seek)))
    (when (or (null delay) (zerop delay)
              (let ((time (bongo-player-get player 'last-seek-time)))
                (or (null time)
                    (time-less-p (seconds-to-time delay)
                                 (subtract-time (current-time) time)))))
      (bongo-player-put player 'elapsed-time elapsed-time))))

(defun bongo-player-update-total-time (player total-time)
  "Set PLAYER's `total-time' property to TOTAL-TIME."
  (bongo-player-put player 'total-time total-time))


;;;; Default implementations of player features

(defun bongo-default-player-stop (player)
  "Delete the process associated with PLAYER, if any.
Then call `bongo-player-explicitly-stopped'."
  (let ((process (bongo-player-process player)))
    (when process
      (delete-process process)))
  (bongo-player-explicitly-stopped player))

(defun bongo-default-player-running-p (player)
  "Return non-nil if PLAYER has a running or stopped process."
  (let ((process (bongo-player-process player)))
    (and process (memq (process-status process) '(run stop)) t)))

(defun bongo-default-player-interactive-p (player)
  "Return the value of PLAYER's `interactive' property."
  (bongo-player-get player 'interactive))

(defun bongo-default-player-pausing-supported-p (player)
  "Return the value of PLAYER's `pausing-supported' property."
  (bongo-player-get player 'pausing-supported))

(defun bongo-default-player-paused-p (player)
  "Return non-nil if PLAYER has a non-nil `paused' property,
or a stopped (but continuable) process."
  (or (bongo-player-get player 'paused)
      (let ((process (bongo-player-process player)))
        (and process (eq (process-status process) 'stop)))))

(defun bongo-default-player-pause/resume (player)
  "Pause/resume PLAYER using SIGSTOP/SIGCONT.
Some signal other that SIGSTOP may be used for pausing if
the player's backend has a non-nil `pause-signal' property."
  (if (bongo-player-paused-p player)
      ;; We can't use `signal-process' with `cont' here,
      ;; because that won't change the process status.
      ;; This could be seen as a bug in Emacs.
      (continue-process (bongo-player-process player))
    ;; We _can_ use `signal-process' with `stop', however,
    ;; because Emacs will update the process status upon
    ;; handling the SIGCHLD sent when a child is stopped.
    ;;
    ;; Some players pause upon receiving a SIGTSTP signal,
    ;; which gives them an opportunity to prepare first.
    ;; If they go ahead and stop themselves after preparing,
    ;; we should be nice and let them.  That's why this code
    ;; does not assume that it should always send SIGSTOP.
    (signal-process (bongo-player-process player)
                    (bongo-player-pause-signal player))))

(defun bongo-default-player-seeking-supported-p (player)
  "Return the value of PLAYER's `seeking-supported' property."
  (bongo-player-get player 'seeking-supported))

(defun bongo-default-player-seek-by (player n)
  "Tell PLAYER to seek by N seconds (backwards if N is negative).
If PLAYER does not support seeking, signal an error."
  (bongo-player-seek-to player
    (+ (or (bongo-player-elapsed-time player) 0) n)))

(defun bongo-default-player-seek-to (player n)
  "Signal an error explaining that PLAYER does not support seeking.
This is a dummy implementation, so N is ignored."
  (error "Seeking is not supported for %s"
         (bongo-player-backend-name player)))

(defun bongo-default-player-get-elapsed-time (player)
  "Return the value of PLAYER's `elapsed-time' property."
  (bongo-player-get player 'elapsed-time))

(defun bongo-default-player-get-total-time (player)
  "Return the value of PLAYER's `total-time' property."
  (bongo-player-get player 'total-time))

(defun bongo-default-player-process-sentinel (process string)
  "If PROCESS has exited or been killed, run the appropriate hooks.
STRING is ignored; the process status of PROCESS is used instead."
  (let ((status (process-status process))
        (player (process-get process 'bongo-player)))
    (cond ((eq status 'exit)
           (if (zerop (process-exit-status process))
               (bongo-player-succeeded player)
             (message "Process `%s' exited abnormally with code %d"
                      (process-name process)
                      (process-exit-status process))
             (bongo-player-failed player)))
          ((eq status 'signal)
           (unless (bongo-player-explicitly-stopped-p player)
             (bongo-player-killed player)))
          ((memq status '(run stop))
           (bongo-player-paused/resumed player)))))


;;;; Backends

(defun bongo-evaluate-program-argument (argument)
  ;; Lists returned by this function will be destroyed by
  ;; the `nconc' in `bongo-evaluate-program-argument'.
  (cond ((stringp argument) (list argument))
        ((symbolp argument)
         (let ((value (symbol-value argument)))
           (if (listp value) (copy-sequence value) (list value))))
        ((listp argument)
         (let ((value (eval argument)))
           (if (listp value) (copy-sequence value) (list value))))
        (t (error "Invalid program argument specifier: `%s'" argument))))

(defun bongo-evaluate-program-arguments (arguments)
  (apply 'nconc (mapcar 'bongo-evaluate-program-argument arguments)))

(defun bongo-start-simple-player
  (backend file-name &optional extra-arguments)
  (let* ((process-connection-type nil)
         (backend (bongo-backend backend))
         (backend-name (bongo-backend-name backend))
         ;; These dynamically-bound variables are used in
         ;; the simple constructor argument list.
         (bongo-file-name file-name)
         (bongo-extra-arguments extra-arguments)
         (process (apply 'start-process
                         (format "bongo-%s" backend-name) nil
                         (bongo-backend-program-name backend)
                         (bongo-evaluate-program-arguments
                          (bongo-backend-program-arguments backend))))
         (player (list backend-name
                       (cons 'process process)
                       (cons 'file-name file-name)
                       (cons 'buffer (current-buffer))
                       (cons 'pausing-supported t))))
    (prog1 player
      (set-process-sentinel process 'bongo-default-player-process-sentinel)
      (process-put process 'bongo-player player))))

(defun bongo-action-player-run (player)
  (bongo-player-put player 'running t)
  (unwind-protect
      (eval (bongo-player-get player 'expression))
    (bongo-player-put player 'running nil))
  (unless (bongo-player-explicitly-stopped-p player)
    (bongo-player-succeeded player)))

(defun bongo-action-player-stop (player)
  (bongo-player-put player 'running nil)
  (bongo-player-explicitly-stopped player))

(defun bongo-action-player-running-p (player)
  (bongo-player-get player 'running))

(defun bongo-start-action-player (expression)
  (let ((player (list 'action
                      (cons 'expression expression)
                      (cons 'buffer (current-buffer))
                      (cons 'stop 'bongo-action-player-stop)
                      (cons 'running-p 'bongo-action-player-running-p))))
    (prog1 player
      (run-with-timer 0 nil 'bongo-action-player-run player))))

(defmacro define-bongo-backend (name &rest options)
  (let ((options options))
    (while options
      (unless (memq (car options)
                    (list :program-name-variable
                          :extra-program-arguments-variable
                          :pretty-name
                          :constructor
                          :program-name
                          :program-arguments
                          :extra-program-arguments
                          :pause-signal
                          :matcher
                          :file-name-transformer))
        (error "Unsupported keyword `%S' for `define-bongo-backend'"
               (car options)))
      (setq options (cddr options))))
  (let* ((group-name
          (intern (format "bongo-%s" name)))
         (program-name-variable
          (or (eval (plist-get options :program-name-variable))
              (intern (format "bongo-%s-program-name" name))))
         (extra-program-arguments-variable
          (or (eval (plist-get options :extra-program-arguments-variable))
              (intern (format "bongo-%s-extra-arguments" name))))
         (pretty-name
          (or (eval (plist-get options :pretty-name))
              (symbol-name name)))
         (constructor
          (or (eval (plist-get options :constructor))
              (intern (format "bongo-start-%s-player" name))))
         (program-name
          (or (eval (plist-get options :program-name))
              (symbol-name name)))
         (program-arguments
          (or (eval (plist-get options :program-arguments))
              (list extra-program-arguments-variable
                    'bongo-extra-arguments 'bongo-file-name)))
         (extra-program-arguments
          (eval (plist-get options :extra-program-arguments)))
         (pause-signal
          (or (eval (plist-get options :pause-signal)) 'SIGSTOP))
         (matcher-expressions
          (bongo-plist-get-all options :matcher))
         (file-name-transformers
          (mapcar 'eval (bongo-plist-get-all
                         options :file-name-transformer))))
    ;; The special element used to be called `file-name'.
    (when (memq 'file-name program-arguments)
      (error (concat "Use `bongo-file-name' rather than "
                     "`file-name' in program argument list")))
    (when (memq 'extra-arguments program-arguments)
      (error (concat "Use `bongo-extra-arguments' rather than "
                     "`extra-arguments' in program argument list")))
    `(progn
       (defgroup ,group-name nil
         ,(format "The %s backend to Bongo." pretty-name)
         :prefix ,(format "bongo-%s-" name)
         :group 'bongo)

       ,@(when program-name-variable
           `((defcustom ,program-name-variable ',program-name
               ,(format "The name of the `%s' executable." program-name)
               :type 'string
               :group ',group-name)))

       ,@(when (and (not (null extra-program-arguments-variable))
                    (member extra-program-arguments-variable
                            program-arguments))
           `((defcustom ,extra-program-arguments-variable
               ',extra-program-arguments
               ,(format "Extra command-line arguments to pass to `%s'."
                        program-name)
               :type '(repeat (choice string variable sexp))
               :group ',group-name)))

       ,@(when (null (plist-get options :constructor))
           `((defun ,constructor (file-name &optional extra-arguments)
               (bongo-start-simple-player ',name file-name extra-arguments))))

       ,@(mapcar (lambda (matcher-expression)
                   `(add-to-list 'bongo-backend-matchers
                      (cons ',name ,matcher-expression) t))
                 matcher-expressions)

       (put ',name 'bongo-backend
            (list ',name
                  (cons 'constructor ',constructor)
                  (cons 'program-name ',(or program-name-variable
                                            program-name))
                  (cons 'program-arguments ',program-arguments)
                  (cons 'pretty-name ',pretty-name)
                  (cons 'pause-signal ',pause-signal)
                  (cons 'file-name-transformers
                        ',file-name-transformers)))
       (add-to-list 'bongo-backends ',name t)
       (bongo-evaluate-backend-defcustoms))))


;;;; The mpg123 backend

(define-bongo-backend mpg123
  :constructor 'bongo-start-mpg123-player
  ;; We define this variable manually so that we can get
  ;; some other customization variables to appear before it.
  :extra-program-arguments-variable nil
  :matcher '(local-file "mp3" "mp2"))

(defcustom bongo-mpg123-audio-driver nil
  "Audio driver (\"esd\", \"alsa\", etc.) to be used by mpg123.
This corresponds to the `-o' option of mpg123."
  :type '(choice (const :tag "System default" nil)
                 (const :tag "\
esd (the Enlightened Sound Daemon)" "esd")
                 (const :tag "\
alsa (the Advanced Linux Sound Architecture)" "alsa")
                 (const :tag "\
alsa09 (ALSA version 0.9)" "alsa")
                 (const :tag "\
arts (the analog real-time synthesiser)" "arts")
                 (const :tag "\
sun (the Sun audio system)" "sun")
                 (const :tag "\
oss (the Linux Open Sound System)" "oss")
                 (string :tag "Other audio driver"))
  :group 'bongo-mpg123)

(defcustom bongo-mpg123-audio-device nil
  "Audio device (e.g., for ALSA, \"1:0\") to be used by mpg123.
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
  (and (bongo-mpg123-is-mpg321-p) 30)
  "Number of frames to skip between each update from mpg321.
This corresponds to the mpg321-specific option --skip-printing-frames.
If your mpg123 does not support that option, set this variable to nil."
  :type '(choice (const :tag "None (lowest)" nil) integer)
  :group 'bongo-mpg123)

(defcustom bongo-mpg123-seek-increment 150
  "Step size (in frames) to use for relative seeking.
This variable is no longer used."
  :type 'integer
  :group 'bongo-mpg123)

(defcustom bongo-mpg123-time-update-delay-after-seek 1
  "Number of seconds to delay time updates from mpg123 after seeking.
Such delays may prevent jerkiness in the visual seek interface."
  :type 'number
  :group 'bongo-mpg123)

(defcustom bongo-mpg123-extra-arguments nil
  "Extra command-line arguments to pass to `mpg123'.
These will come at the end or right before the file name, if any."
  :type '(repeat (choice string variable sexp))
  :group 'bongo-mpg123)

(defun bongo-mpg123-player-pause/resume (player)
  (if (not (bongo-player-interactive-p player))
      (bongo-default-player-pause/resume player)
    (process-send-string (bongo-player-process player) "PAUSE\n")
    (bongo-player-put player 'paused
      (not (bongo-player-get player 'paused)))
    (bongo-player-paused/resumed player)))

(defun bongo-seconds-to-mp3-frames (seconds)
  (round (* seconds 38.3)))

(defun bongo-mpg123-player-seek-to (player seconds)
  (when (not (bongo-player-interactive-p player))
    (error (concat "This mpg123 process is not interactive "
                   "and so does not support seeking")))
  (process-send-string
   (bongo-player-process player)
   (format "JUMP %d\n"
           (bongo-seconds-to-mp3-frames (max seconds 0))))
  (bongo-player-sought player seconds))

;;; XXX: What happens if a record is split between two calls
;;;      to the process filter?
(defun bongo-mpg123-parse-output (string)
  "Return a list of (STATUS &rest PARAMETERS) from mpg123 process output string"
  (let* ((valid-lines (cl-remove-if
		      ;; remove unparsable lines
		      (lambda (line)
			(or (< (length line) 3)
			    (not (or (string= (substring line 0 2) "@F")
				     (string= "@P 0" line)))))
		      (split-string string "[\n\r]+")))
	 ;; map to a list of (STATUS &rest PARAMETERS)
	 (parsed-output
	  (mapcar (lambda (line)
		    (if (string= "@P 0" line)
			(list 'P 0)
		      (let* ((elapsed-time (string-to-number (nth 3 (split-string line))))
			     (total-time (+ elapsed-time (string-to-number
							  (nth 4 (split-string line))))))
			;; we can round the elapsed time, because the frontend only uses whole seconds
			(list 'F (round elapsed-time) (round total-time)))))
		  valid-lines)))
    (delete-dups parsed-output)))

(defun bongo-mpg123-process-filter (process string)
  (condition-case condition
      (let ((player (process-get process 'bongo-player)))
        (dolist (event (bongo-mpg123-parse-output string))
	  (cl-case (car event)
	    ('F
	     (let* ((elapsed-time (nth 1 event))
		    (total-time (nth 2 event)))
	       (bongo-player-update-elapsed-time player elapsed-time)
	       (bongo-player-update-total-time player total-time)
	       (bongo-player-times-changed player)))
	    ('P
	     (bongo-player-succeeded player)
	     (set-process-sentinel process nil)
	     (delete-process process)))))
    ;; Getting errors in process filters is not fun, so stop.
    (error (bongo-stop)
	   (signal (car condition) (cdr condition)))))

(defun bongo-start-mpg123-player (file-name &optional extra-arguments)
  (let* ((process-connection-type nil)
         (arguments (append
                     (when bongo-mpg123-audio-driver
                       (list "-o" bongo-mpg123-audio-driver))
                     (when bongo-mpg123-audio-device
                       (list "-a" bongo-mpg123-audio-device))
                     (when bongo-mpg123-update-granularity
                       (list "--skip-printing-frames"
                             (number-to-string
                              bongo-mpg123-update-granularity)))
                     (bongo-evaluate-program-arguments
                      bongo-mpg123-extra-arguments)
                     extra-arguments
                     (if bongo-mpg123-interactive
                         '("-R" "dummy")
                       (list file-name))))
         (process (apply 'start-process "bongo-mpg123" nil
                         bongo-mpg123-program-name arguments))
         (player
          (list 'mpg123
                (cons 'process process)
                (cons 'file-name file-name)
                (cons 'buffer (current-buffer))
                (cons 'interactive bongo-mpg123-interactive)
                (cons 'pausing-supported t)
                (cons 'seeking-supported bongo-mpg123-interactive)
                (cons 'time-update-delay-after-seek
                      bongo-mpg123-time-update-delay-after-seek)
                (cons 'paused nil)
                (cons 'pause/resume 'bongo-mpg123-player-pause/resume)
                (cons 'seek-to 'bongo-mpg123-player-seek-to)
                (cons 'seek-unit 'seconds))))
    (prog1 player
      (set-process-sentinel process 'bongo-default-player-process-sentinel)
      (process-put process 'bongo-player player)
      (if (not bongo-mpg123-interactive)
          (set-process-filter process 'ignore)
        (set-process-filter process 'bongo-mpg123-process-filter)
        (process-send-string process (format "LOAD %s\n" file-name))))))


;;;; The VLC backend

(define-bongo-backend vlc
  :pretty-name "VLC"
  :constructor 'bongo-start-vlc-player

  ;; We define this variable manually so that we can get
  ;; some other customization variables to appear before it.
  :extra-program-arguments-variable nil

  ;; Play generic URLs and files if the file extension
  ;; matches that of some potentially supported format.
  :matcher '((local-file "file:" "http:" "https:" "ftp:")
             "669"
             "aac"
             "asf"
             "au"
             "avi"
             "flac"
             "flv"
             "it"
             "m4a"
             "m4v"
             "mdz"
             "mid"
             "midi"
             "mka"
             "mkv"
             "mod"
             "mov"
             "mp2"
             "mp3"
             "mp4"
             "mpeg"
             "mpg"
             "mtm"
             "ogg"
             "ogm"
             "ra"
             "rm"
             "rmi"
             "rmvb"
             "s3m"
             "stm"
             "ts"
             "umx"
             "vob"
             "vqf"
             "wav"
             "webm"
             "wma"
             "wmv"
             "xm")

  ;; Play special media URIs regardless of the file name.
  :matcher '(("mms:" "udp:" "dvd:" "vcd:" "cdda:") . t)

  ;; Play all HTTP URLs (necessary for many streams).
  ;; XXX: This is not a good long-term solution.  (But it
  ;;      would be good to keep this matcher as a fallback
  ;;      if we could somehow declare that more specific
  ;;      matchers should be tried first.)
  :matcher '(("http:" "https:") . t)

  ;; VLC fails to report time information for CD tracks
  ;; played using the `vlc cdda://@1' syntax.  The bug
  ;; does not manifest for `vlc cdda:// --cdda-track 1',
  ;; which we use instead as a workaroud.  See Bug#404645
  ;; reported against VLC in Debian.
  :file-name-transformer
  (lambda (file-name)
    (when (string-match (eval-when-compile
                          (rx (and string-start "cdda://"
                                   (submatch (zero-or-more anything))
                                   "@" (submatch (one-or-more digit))
                                   (submatch (zero-or-more anything))
                                   string-end)))
                        file-name)
      (list (concat "cdda://" (match-string 1 file-name)
                    (match-string 3 file-name))
            "--cdda-track" (match-string 2 file-name)))))

(defcustom bongo-vlc-interactive t
  "If non-nil, use the remote control interface of VLC.
Setting this to nil disables the pause and seek functionality."
  :type 'boolean
  :group 'bongo-vlc)

(defcustom bongo-vlc-initialization-period 0.2
  "Number of seconds to wait before querying VLC for time information.
If this number is too low, there might be a short period of time right
after VLC starts playing a track during which Bongo thinks that the total
track length is unknown."
  :type 'number
  :group 'bongo-vlc)

(defcustom bongo-vlc-time-update-delay-after-seek 1
  "Number of seconds to delay time updates from VLC after seeking.
Such delays may prevent jerkiness in the visual seek interface."
  :type 'number
  :group 'bongo-vlc)

(defcustom bongo-vlc-extra-arguments nil
  "Extra command-line arguments to pass to `vlc'.
These will come at the end or right before the file name, if any."
  :type '(repeat (choice string variable sexp))
  :group 'bongo-vlc)

(defun bongo-vlc-player-pause/resume (player)
  (if (bongo-player-interactive-p player)
      (process-send-string (bongo-player-process player) "pause\n")
    (bongo-default-player-pause/resume player)))

(defun bongo-vlc-player-seek-to (player seconds)
  (when (not (bongo-player-interactive-p player))
    (error (concat "This VLC process is not interactive "
                   "and so does not support seeking")))
  (process-send-string (bongo-player-process player)
                       (format "seek %d\n" (max seconds 0)))
  (bongo-player-sought player seconds))

(defun bongo-vlc-player-stop-timer (player)
  (let ((timer (bongo-player-get player 'timer)))
    (when timer
      (cancel-timer timer)
      (bongo-player-put player 'timer nil))))

(defun bongo-vlc-player-tick (player)
  (cond ((not (bongo-player-running-p player))
         (bongo-vlc-player-stop-timer player))
        ((and (not (bongo-player-paused-p player))
              (null (nthcdr 4 (bongo-player-get player 'pending-queries))))
         (let ((process (bongo-player-process player)))
           (process-send-string process "get_time\n")
           (bongo-player-push player 'pending-queries 'time)
           (when (null (bongo-player-total-time player))
             (process-send-string process "get_length\n")
             (bongo-player-push player 'pending-queries 'length))))))

(defun bongo-vlc-player-start-timer (player)
  (bongo-vlc-player-stop-timer player)
  (let ((timer (run-with-timer bongo-vlc-initialization-period
                               1 'bongo-vlc-player-tick player)))
    (bongo-player-put player 'timer timer)))

;;; XXX: What happens if a record is split between two calls
;;;      to the process filter?
(defun bongo-vlc-process-filter (process string)
  (condition-case condition
      (let ((player (process-get process 'bongo-player)))
        (with-temp-buffer
          (insert string)
          (goto-char (point-min))
          (let (stream-name stream-genre stream-part-title)
            (while (not (eobp))
              (cond ((looking-at
                      (eval-when-compile
                        (rx (and line-start
                                 "Remote control interface initialized."))))
                     (when (null (bongo-player-get player 'timer))
                       (bongo-vlc-player-start-timer player)))
                    ((looking-at
                      (eval-when-compile
                        (rx (and line-start
                                 "status change:"
                                 (zero-or-more (or space "("))
                                 "play state:"
                                 (zero-or-more space)
                                 (submatch (one-or-more digit))
                                 (zero-or-more (or space ")"))
                                 line-end))))
                     (cl-case (string-to-number (match-string 1))
                       ((1 3)
                        (bongo-player-put player 'paused nil)
                        (bongo-player-paused/resumed player))
                       ((2 4)
                        (bongo-player-put player 'paused t)
                        (bongo-player-paused/resumed player))))
                    ((looking-at
                      (eval-when-compile
                        (rx (and line-start
                                 (optional
                                  (and "[" (zero-or-more digit) "]"))
                                 (zero-or-more space)
                                 "main input debug:"
                                 (zero-or-more space)
                                 "- 'Title' = '"
                                 (submatch (zero-or-more not-newline))
                                 "'"
                                 line-end))))
                     (setq stream-name (match-string 1)))
                    ((looking-at
                      (eval-when-compile
                        (rx (and line-start
                                 (optional
                                  (and "[" (zero-or-more digit) "]"))
                                 (zero-or-more space)
                                 "main input debug:"
                                 (zero-or-more space)
                                 "- 'Genre' = '"
                                 (submatch (zero-or-more not-newline))
                                 "'"
                                 line-end))))
                     (setq stream-genre (match-string 1)))
                    ((looking-at
                      (eval-when-compile
                        (rx (and line-start
                                 (optional
                                  (and "[" (zero-or-more digit) "]"))
                                 (zero-or-more space)
                                 "main input debug:"
                                 (zero-or-more space)
                                 "- 'Now Playing' = '"
                                 (submatch (zero-or-more not-newline))
                                 "'"
                                 line-end))))
                     (setq stream-part-title (match-string 1)))
                    ((looking-at
                      (eval-when-compile
                        (rx (and line-start
                                 (optional
                                  (and "[" (zero-or-more digit) "]"))
                                 (zero-or-more space)
                                 "main playlist: nothing to play"
                                 line-end))))
                     (process-send-string process "quit\n"))
                    ((looking-at
                      (eval-when-compile
                        (rx (and line-start
                                 (submatch (one-or-more digit))
                                 (zero-or-more space)
                                 line-end))))
                     (when (bongo-player-get player 'pending-queries)
                       (let ((value (string-to-number (match-string 1))))
                         (cl-ecase (bongo-player-shift player 'pending-queries)
                           (time
                            (bongo-player-update-elapsed-time player value)
                            (bongo-player-times-changed player))
                           (length
                            (bongo-player-update-total-time player value)
                            (bongo-player-times-changed player)))))))
              (forward-line))
            (when stream-name
              (bongo-player-put player 'stream-name stream-name))
            (when stream-genre
              (bongo-player-put player 'stream-genre stream-genre))
            (when stream-part-title
              (bongo-player-put player 'stream-part-title stream-part-title))
            (when (or stream-name stream-genre stream-part-title)
              (bongo-player-metadata-changed player)))))
    ;; Getting errors in process filters is not fun, so stop.
    (error (bongo-stop)
           (signal (car condition) (cdr condition)))))

(defun bongo-start-vlc-player (file-name &optional extra-arguments)
  (let* ((process-connection-type nil)
         (arguments (append
                     (when bongo-vlc-interactive
                       (append (list "-I" "oldrc" "--rc-fake-tty"
                                     "--play-and-stop" "--play-and-exit")
                               (when (bongo-uri-p file-name)
                                 (list "-vv"))
                               (when (eq window-system 'w32)
                                 (list "--rc-quiet"))))
                     (bongo-evaluate-program-arguments
                      bongo-vlc-extra-arguments)
                     extra-arguments
                     (list file-name)))
         (process (apply 'start-process "bongo-vlc" nil
                         bongo-vlc-program-name arguments))
         (player
          (list 'vlc
                (cons 'process process)
                (cons 'file-name file-name)
                (cons 'buffer (current-buffer))
                (cons 'interactive bongo-vlc-interactive)
                (cons 'pausing-supported t)
                (cons 'seeking-supported bongo-vlc-interactive)
                (cons 'time-update-delay-after-seek
                      bongo-vlc-time-update-delay-after-seek)
                (cons 'paused nil)
                (cons 'pause/resume 'bongo-vlc-player-pause/resume)
                (cons 'seek-to 'bongo-vlc-player-seek-to)
                (cons 'seek-unit 'seconds))))
    (prog1 player
      (set-process-sentinel process 'bongo-default-player-process-sentinel)
      (process-put process 'bongo-player player)
      (when bongo-vlc-interactive
        (set-process-filter process 'bongo-vlc-process-filter)))))



;;;; The MPlayer backend

(define-bongo-backend mplayer
  :constructor 'bongo-start-mplayer-player
  :pretty-name "MPlayer"

  ;; We define this variable manually so that we can get
  ;; some other customization variables to appear before it.
  :extra-program-arguments-variable nil

  ;; Play generic URLs and files if the file extension
  ;; matches that of some potentially supported format.
  :matcher '((local-file "file:" "http:" "ftp:")
             "ogg" "flac" "mp3" "mka" "wav" "wma"
             "mpg" "mpeg" "vob" "avi" "ogm" "mp4" "mkv"
             "mov" "asf" "wmv" "rm" "rmvb" "ts")

  ;; Play special media URIs regardless of the file name.
  :matcher '(("mms:" "mmst:" "rtp:" "rtsp:" "udp:" "unsv:"
              "dvd:" "vcd:" "tv:" "dvb:" "mf:" "cdda:" "cddb:"
              "cue:" "sdp:" "mpst:" "tivo:") . t)

  ;; Play all HTTP URLs (necessary for many streams).
  ;; XXX: This is not a good long-term solution.  (But it
  ;;      would be good to keep this matcher as a fallback
  ;;      if we could somehow declare that more specific
  ;;      matchers should be tried first.)
  :matcher '(("http:") . t)

  ;; Transform CDDA URIs into the right syntax for mplayer.
  :file-name-transformer
  (cons (eval-when-compile
          (rx (and string-start
                   (submatch (and (or "cdda" "cddb") "://"))
                   ;; Device file name.
                   (optional (submatch (one-or-more (not (any "@")))))
                   ;; Track number.
                   (optional (and "@" (submatch (zero-or-more anything))))
                   string-end)))
        "\\1\\3/\\2"))

(defun bongo-mplayer-available-drivers (type)
  (unless (memq type '(audio video))
    (error "Invalid device type"))
  (when (executable-find bongo-mplayer-program-name)
    (let ((result nil))
      (with-temp-buffer
        (let ((process-environment (cons "LC_ALL=en_US.UTF-8" process-environment)))
          (call-process bongo-mplayer-program-name nil t nil
                        (cl-ecase type
                          (audio "-ao")
                          (video "-vo"))
                        "help"))
        (goto-char (point-min))
        (search-forward (concat "Available " (cl-ecase type
                                               (audio "audio")
                                               (video "video"))
                                " output drivers:\n"))
        (while (looking-at
                (eval-when-compile
                  (rx (and line-start
                           (one-or-more space)
                           (submatch (one-or-more word))
                           (one-or-more space)
                           (submatch (zero-or-more not-newline))
                           line-end))))
          (setq result (cons (cons (match-string 1)
                                   (match-string 2))
                             result))
          (forward-line)))
      (reverse result))))

(defcustom bongo-mplayer-audio-driver nil
  "Audio driver to be used by mplayer.
This corresponds to the `-ao' option of mplayer."
  :type `(choice (const :tag "System default" nil)
                 ,@(mapcar (lambda (entry)
                             `(const :tag ,(concat (car entry)
                                                   " (" (cdr entry) ")")))
                           (bongo-mplayer-available-drivers 'audio))
                 (string :tag "Other audio driver"))
  :group 'bongo-mplayer)

(defcustom bongo-mplayer-video-driver nil
  "Video driver to be used by mplayer.
This corresponds to the `-vo' option of mplayer."
  :type `(choice (const :tag "System default" nil)
                 ,@(mapcar (lambda (entry)
                             `(const :tag ,(concat (car entry)
                                                   " (" (cdr entry) ")")))
                           (bongo-mplayer-available-drivers 'video))
                 (string :tag "Other video driver"))
  :group 'bongo-mplayer)

(defcustom bongo-mplayer-interactive t
  "If non-nil, use the slave mode of mplayer.
Setting this to nil disables the pause and seek functionality."
  :type 'boolean
  :group 'bongo-mplayer)

(defcustom bongo-mplayer-time-update-delay-after-seek 1
  "Number of seconds to delay time updates from mplayer after seeking.
Such delays may prevent jerkiness in the visual seek interface."
  :type 'number
  :group 'bongo-mplayer)

(defcustom bongo-mplayer-extra-arguments nil
  "Extra command-line arguments to pass to `mplayer'.
These will come at the end or right before the file name, if any."
  :type '(repeat (choice string variable sexp))
  :group 'bongo-mplayer)

(defun bongo-mplayer-player-pause/resume (player)
  (when (not (bongo-player-interactive-p player))
    (error (concat "This mplayer process is not interactive "
                   "and so does not support pausing")))
  (process-send-string (bongo-player-process player) "pause\n")
  (bongo-player-put player 'paused
    (not (bongo-player-get player 'paused)))
  (bongo-player-paused/resumed player))

(defun bongo-mplayer-player-seek-to (player seconds)
  (when (not (bongo-player-interactive-p player))
    (error (concat "This mplayer process is not interactive "
                   "and so does not support seeking")))
  (process-send-string (bongo-player-process player)
                       (format "seek %f 2\n" (max seconds 0)))
  (bongo-player-sought player seconds))

(defun bongo-mplayer-player-start-timer (player)
  (bongo-mplayer-player-stop-timer player)
  (let ((timer (run-with-timer 0 1 'bongo-mplayer-player-tick player)))
    (bongo-player-put player 'timer timer)))

(defun bongo-mplayer-player-stop-timer (player)
  (let ((timer (bongo-player-get player 'timer)))
    (when timer
      (cancel-timer timer)
      (bongo-player-put player 'timer nil))))

(defun bongo-mplayer-player-tick (player)
  (cond ((not (bongo-player-running-p player))
         (bongo-mplayer-player-stop-timer player))
        ((not (bongo-player-paused-p player))
         (let ((process (bongo-player-process player)))
           (process-send-string
            process "pausing_keep get_time_pos\n")
           (when (null (bongo-player-total-time player))
             (process-send-string
              process "pausing_keep get_time_length\n"))))))

;;; XXX: What happens if a record is split between two calls
;;;      to the process filter?
(defun bongo-mplayer-process-filter (process string)
  (condition-case condition
      (let ((player (process-get process 'bongo-player)))
        (with-temp-buffer
          (insert string)
          (goto-char (point-min))
          (while (not (eobp))
            (cond ((looking-at "^ANS_TIME_POSITION=\\(.+\\)$")
                   (bongo-player-update-elapsed-time
                    player (string-to-number (match-string 1)))
                   (bongo-player-times-changed player))
                  ((looking-at "^ANS_LENGTH=\\(.+\\)$")
                   (bongo-player-update-total-time
                    player (string-to-number (match-string 1)))
                   (bongo-player-times-changed player)))
            (forward-line))))
    ;; Getting errors in process filters is not fun, so stop.
    (error (bongo-stop)
           (signal (car condition) (cdr condition)))))

(defun bongo-start-mplayer-player (file-name &optional extra-arguments)
  (let* ((process-connection-type nil)
         (arguments (append
                     (when bongo-mplayer-audio-driver
                       (list "-ao" bongo-mplayer-audio-driver))
                     (when bongo-mplayer-video-driver
                       (list "-vo" bongo-mplayer-video-driver))
                     (when bongo-mplayer-interactive
                       (list "-quiet" "-slave"))
                     (bongo-evaluate-program-arguments
                      bongo-mplayer-extra-arguments)
                     extra-arguments
                     (list file-name)))
         (process (apply 'start-process "bongo-mplayer" nil
                         bongo-mplayer-program-name arguments))
         (player
          (list 'mplayer
                (cons 'process process)
                (cons 'file-name file-name)
                (cons 'buffer (current-buffer))
                (cons 'interactive bongo-mplayer-interactive)
                (cons 'pausing-supported bongo-mplayer-interactive)
                (cons 'seeking-supported bongo-mplayer-interactive)
                (cons 'time-update-delay-after-seek
                      bongo-mplayer-time-update-delay-after-seek)
                (cons 'paused nil)
                (cons 'pause/resume 'bongo-mplayer-player-pause/resume)
                (cons 'seek-to 'bongo-mplayer-player-seek-to)
                (cons 'seek-unit 'seconds))))
    (prog1 player
      (set-process-sentinel process 'bongo-default-player-process-sentinel)
      (process-put process 'bongo-player player)
      (when bongo-mplayer-interactive
        (set-process-filter process 'bongo-mplayer-process-filter)
        (bongo-mplayer-player-start-timer player)))))



;;;; mpv backend

(defcustom bongo-mpv-time-update-delay-after-seek 1
  "Number of seconds to delay time updates from mpv after seeking.
Such delays may prevent jerkiness in the visual seek interface."
  :type 'number
  :group 'bongo-mpv)

(defcustom bongo-mpv-initialization-period 0.2
  "Number of seconds to wait before querying MPV for time information.
We can't call out MPV immediately since the socket might not be ready"
  :type 'number
  :group 'bongo-vlc)

(defvar bongo-mpv-remote-option 'unknown
  "The command line option to be used with mpv to start remote control.

This is calculated when needed and cached, we cannot hardcode the value since
the a different option is used in newer versions of mpv")

(defun bongo--mpv-get-remote-option ()
  "Get the command line option for starting mpv's remote control."
  (with-temp-buffer
    (when (executable-find "mpv")
      (insert (shell-command-to-string "mpv --list-options"))
      (goto-char (point-min))
      (save-match-data
        (when (search-forward-regexp "\\(--input-ipc-server\\|--input-unix-socket\\)" nil t)
          (match-string 0))))))

(defun bongo--mpv-connect-to-socket (player)
  "Establish connection with mpv's remote interface for the PLAYER."
  (let ((socket (make-network-process :name "bongo-mpv"
                                      :buffer nil
                                      :family 'local
                                      :service (bongo-player-get player 'socket-file)
                                      :filter 'bongo--mpv-socket-filter)))
    (bongo-player-put player 'socket socket)
    (process-put socket 'bongo-player player)
    socket))

(defun bongo--mpv-player-process-sentinel (process string)
  "Process sentinel for mpv PROCESS, close the socket after mpv exits.

STRING is simply passed to the default."
  (let ((status (process-status process))
        (player (process-get process 'bongo-player)))
    (when (memq status '(exit signal))
      (when (bongo-player-get player 'socket)
        (delete-process (bongo-player-get player 'socket))))
    (bongo-default-player-process-sentinel process string)))

(defun bongo--run-mpv-command (player request-id command &rest args)
  "For mpv instance associated with PLAYER run COMMAND with ARGS.

REQUEST-ID is used to identity the response in the socket filter. See also
`bongo--mpv-socket-filter'.

It connects to mpv's remote interface if the connection has not being already
created, we defer the socket creation, rather than creating it right after
starting mpv because it takes sometime for the socket to be ready."
  (let ((socket (bongo-player-get player 'socket)))
    (unless socket
      (setq socket (bongo--mpv-connect-to-socket player)))
    (process-send-string socket
                         (concat (json-encode `(("command" . (,command ,@args))
                                                ("request_id" . ,request-id)))
                                 "\n"))))

(defun bongo--mpv-socket-filter (process output)
  "Filter for socket connection with mpv.

PROCESS is the socket which returned the OUTPUT."
  (let ((player (process-get process 'bongo-player)))
    (dolist (parsed-response (mapcar #'json-read-from-string
                                     (split-string output "\n" t)))
      ;; Events are treated differently from normal responses
      (if (assoc 'event parsed-response)
          (pcase (bongo-alist-get parsed-response 'event)
            (`"pause" (progn
                        (bongo-player-put player 'paused t)
                        (bongo-player-paused/resumed player)))
            (`"unpause" (progn
                          (bongo-player-put player 'paused nil)
                          (bongo-player-paused/resumed player))))
        ;; Use request-id to identify the type of response
        (pcase (bongo-alist-get parsed-response 'request_id)
          (`"time-pos" (progn
                         (bongo-player-update-elapsed-time player
                                                           (bongo-alist-get parsed-response
                                                                            'data))
                         (bongo-player-times-changed player)))
          (`"duration" (progn
                         (bongo-player-update-total-time player
                                                         (bongo-alist-get parsed-response
                                                                          'data))
                         (bongo-player-times-changed player)))
          (`"metadata" (let* ((data (bongo-alist-get parsed-response 'data))
                              (album (bongo-alist-get data 'album))
                              (title (bongo-alist-get data 'title))
                              (genre (bongo-alist-get data 'genre)))
                         (bongo-player-put player 'metadata-fetched t)
                         (when album
                           (bongo-player-put player 'stream-name album))
                         (when title
                           (bongo-player-put player 'stream-part-title title))
                         (when genre
                           (bongo-player-put player 'stream-genre genre))
                         (when (or album title genre)
                           (bongo-player-metadata-changed player)))))))))

(defun bongo-mpv-player-stop-timer (player)
  "Stop timer for the PLAYER."
  (let ((timer (bongo-player-get player 'timer)))
    (when timer
      (cancel-timer timer)
      (bongo-player-put player 'timer nil))))

(defun bongo-mpv-player-tick (player)
  "Update elapsed time for PLAYER.

Also fetch metadata and length of track if not fetched already."
  (if (or (not (bongo-player-running-p player))
          (and (bongo-player-get player 'socket)
               (not (equal (process-status (bongo-player-get player 'socket))
                           'open))))
      (bongo-mpv-player-stop-timer player)
    (bongo--run-mpv-command player "time-pos" "get_property" "time-pos")
    (when (null (bongo-player-total-time player))
      (bongo--run-mpv-command player "duration" "get_property" "duration"))
    (unless (bongo-player-get player 'metadata-fetched)
      (bongo--run-mpv-command player "metadata" "get_property" "metadata"))))

(defun bongo-mpv-player-start-timer (player)
  "Start tick timer for PLAYER."
  (bongo-mpv-player-stop-timer player)
  (let ((timer (run-with-timer bongo-mpv-initialization-period
                               0.1
                               'bongo-mpv-player-tick
                               player)))
    (bongo-player-put player 'timer timer)))

(defun bongo-compose-remote-option (socket-file)
  "Get the command line argument for starting mpv's remote interface at SOCKET-FILE."
  (when (equal bongo-mpv-remote-option 'unknown)
    (setq bongo-mpv-remote-option (bongo--mpv-get-remote-option)))
  (list bongo-mpv-remote-option socket-file))

(defun bongo-mpv-player-pause/resume (player)
  "Play/pause mpv PLAYER."
  (if (bongo-player-paused-p player)
      (bongo--run-mpv-command player
                             "pause"
                             "set_property_string"
                             "pause"
                             "no")
    (bongo--run-mpv-command player
                           "pause"
                           "set_property"
                           "pause"
                           t)))

(defun bongo-mpv-player-seek-to (player seconds)
  "Seek mpv PLAYER by given SECONDS."
  (bongo--run-mpv-command player "seek" "seek" seconds "absolute"))

(defun bongo-start-mpv-player (file-name &optional extra-arguments)
  "Play FILE-NAME with mpv, EXTRA-ARGUMENTS are passed to mpv."
  (let* ((process-connection-type nil)
         (socket-file (expand-file-name "bongo-mpv.socket" temporary-file-directory))
         (arguments (append
                     (bongo-compose-remote-option socket-file)
                     (bongo-evaluate-program-arguments bongo-mpv-extra-arguments)
                     extra-arguments
                     (list file-name)))
         (process (apply 'start-process "bongo-mpv" nil
                         bongo-mpv-program-name arguments))
         (player
          (list 'mpv
                (cons 'process process)
                (cons 'file-name file-name)
                (cons 'buffer (current-buffer))
                (cons 'interactive t)
                (cons 'pausing-supported t)
                (cons 'seeking-supported t)
                (cons 'time-update-delay-after-seek
                      bongo-mpv-time-update-delay-after-seek)
                (cons 'paused nil)
                (cons 'pause/resume 'bongo-mpv-player-pause/resume)
                (cons 'seek-to 'bongo-mpv-player-seek-to)
                (cons 'seek-unit 'seconds)
                (cons 'socket-file socket-file))))
    (prog1 player
      (set-process-sentinel process 'bongo--mpv-player-process-sentinel)
      (process-put process 'bongo-player player)
      (bongo-mpv-player-start-timer player))))

(define-bongo-backend mpv
  :constructor 'bongo-start-mpv-player
  :extra-program-arguments '("--no-audio-display")
  ;; TODO: The matchers below are just copied from MPlayer's config, since
  ;; mpv was forked off MPlayer
  :matcher '((local-file "file:" "http:" "ftp:")
             "ogg" "flac" "mp3" "mka" "wav" "wma"
             "mpg" "mpeg" "vob" "avi" "ogm" "mp4" "mkv"
             "mov" "asf" "wmv" "rm" "rmvb" "ts")

  :matcher '(("mms:" "mmst:" "rtp:" "rtsp:" "udp:" "unsv:"
              "dvd:" "vcd:" "tv:" "dvb:" "mf:" "cdda:" "cddb:"
              "cue:" "sdp:" "mpst:" "tivo:") . t)

  :matcher '(("http:") . t))


;;;; Simple backends

(define-bongo-backend ogg123
  :pause-signal 'SIGTSTP
  :matcher '(local-file "ogg" "flac"))

(define-bongo-backend speexdec
  :matcher '(local-file "spx"))

(define-bongo-backend timidity
  :pretty-name "TiMidity"
  :extra-program-arguments '("--quiet")
  :matcher '(local-file "mid" "midi" "mod" "rcp" "r36" "g18" "g36"))

(define-bongo-backend mikmod
  :pretty-name "MikMod"
  :extra-program-arguments '("-q" "-P" "1" "-X")
  :matcher `(local-file
             . ,(eval-when-compile
                  (rx (and "." (or "669" "amf" "dsm" "far" "gdm" "imf"
                                   "it" "med" "mod" "mtm" "okt" "s3m"
                                   "stm" "stx" "ult" "uni" "apun" "xm")
                           (optional
                            (and "." (or "zip" "lha" "lhz" "zoo" "gz" "bz2"
                                         "tar" "tar.gz" "tar.bz2" "rar")))
                           string-end)))))

(define-bongo-backend afplay
  :matcher '(local-file "mp3"))


;;;; Audio CD and CDDB support

(defgroup bongo-audio-cd nil
  "Audio CD playback and CDDB support in Bongo."
  :group 'bongo)

(defcustom bongo-cd-device nil
  "Default CD device used by Bongo, or nil for system default."
  :type '(choice string (const :tag "System default" nil))
  :group 'bongo-audio-cd)

(defcustom bongo-libcddb-cddb-query-program-name "cddb_query"
  "Name of the `cddb_query' executable (distributed with libcddb)."
  :type 'string
  :group 'bongo-audio-cd)

(defcustom bongo-cdtool-cdir-program-name "cdir"
  "Name of the `cdir' executable (distributed with cdtool)."
  :type 'string
  :group 'bongo-audio-cd)

(defcustom bongo-cdtool-cdown-program-name "cdown"
  "Name of the `cdown' executable (distributed with cdtool)."
  :type 'string
  :group 'bongo-audio-cd)

(defcustom bongo-cdda-info-function
  (cond ((executable-find bongo-libcddb-cddb-query-program-name)
         'bongo-libcddb-cdda-info)
        ((executable-find bongo-cdtool-cdir-program-name)
         'bongo-cdtool-cdda-info))
  "Function used to find basic information about audio CDs.
This function should behave like `bongo-cdda-info'."
  :type '(choice
          (function-item :tag "Use `cddb_query' from the libcddb package"
                         bongo-libcddb-cdda-info)
          (function-item :tag "Use `cdir' from the cdtool package"
                         bongo-cdtool-cdda-info)
          (const :tag "Disable finding basic audio CD information" nil))
  :group 'bongo-audio-cd)

(defcustom bongo-use-cddb t
  "If non-nil, look up CDDB information about audio CDs."
  :type 'boolean
  :group 'bongo-audio-cd)

(defcustom bongo-cddb-server nil
  "Host name of the CDDB server to use."
  :type '(choice (const :tag "Unspecified" nil)
                 (const "freedb.org")
                 string)
  :group 'bongo-audio-cd)

(defcustom bongo-cddb-server-port nil
  "Port number of the CDDB server to use."
  :type '(choice (const :tag "Unspecified")
                 (const 888)
                 integer)
  :group 'bongo-audio-cd)

(defcustom bongo-cddb-info-function
  (cond ((executable-find bongo-libcddb-cddb-query-program-name)
         'bongo-libcddb-cddb-info)
        ((executable-find bongo-cdtool-cdown-program-name)
         'bongo-cdtool-cddb-info))
  "Function used to find CDDB information about audio CDs.
The function should behave like `bongo-cddb-info'."
  :type '(choice
          (function-item :tag "Use `cddb_query' from the libcddb package"
                         bongo-libcddb-cddb-info)
          (function-item :tag "Use `cdown' from the cdtool package"
                         bongo-cdtool-cddb-info)
          (const :tag "Disable finding CDDB information" nil))
  :group 'bongo-audio-cd)

(defun bongo-cdda-info (&optional device omit-lengths)
  "Find the track count and track lengths of an audio CD.
Return (TRACK-COUNT . TRACK-LENGTHS), or nil on failure.
If OMIT-LENGTHS is non-nil, TRACK-LENGTHS will be nil.
Optional argument DEVICE overrides `bongo-cd-device'."
  (cond (bongo-cdda-info-function
         (funcall bongo-cdda-info-function device omit-lengths))
        (bongo-cddb-info-function
         (let ((cddb-info (funcall bongo-cddb-info-function
                                   device omit-lengths)))
           (when cddb-info
             (cons (cadr cddb-info) (mapcar 'cdr (cddr cddb-info))))))))

(defun bongo-cdda-track-count (&optional device)
  "Find the track count of an audio CD.
Return nil if the track count could not be found for some reason.
Optional argument DEVICE overrides `bongo-cd-device'."
  (cond (bongo-cdda-info-function
         (car (bongo-cdda-info device 'omit-lengths)))
        (bongo-cddb-info-function
         (cadr (bongo-cddb-info device 'omit-lengths)))))

(defun bongo-cddb-info (&optional device omit-lengths)
  "Find CDDB information about an audio CD.
Return ((ARTIST-NAME ALBUM-TITLE ALBUM-YEAR) TRACK-COUNT . TRACKS),
Entries in TRACKS are of the form (TRACK-TITLE . TRACK-LENGTH).
If OMIT-LENGTHS is non-nil, TRACK-LENGTH will be nil for all tracks.
Return nil if the audio CD could not be read or some other error occured.
Optional argument DEVICE overrides `bongo-cd-device'."
  (when bongo-cddb-info-function
    (funcall bongo-cddb-info-function device)))

(defun bongo-libcddb-cdda-info (&optional device omit-lengths)
  "Use `cddb_query' to find the track count on the audio CD in DEVICE.
Track lengths are not retrieved; OMIT-LENGTHS is ignored.
This function is a suitable value for `bongo-cdda-info-function'."
  (with-temp-buffer
    (apply 'call-process bongo-libcddb-cddb-query-program-name nil t nil
           (nconc (when (or device bongo-cd-device)
                    (list "-i" (or device bongo-cd-device)))
                  (list "calc")))
    (goto-char (point-min))
    (when (re-search-forward "^CD contains \\(.+\\) track" nil t)
      (let ((count (string-to-number (match-string 1))))
        ;; Return nil for non-audio CDs.
        (when (search-forward "CD disc ID is" nil t)
          (cons count nil))))))

(defun bongo-libcddb-cddb-info (&optional device omit-lengths)
  "Use `cddb_query' to find CDDB information about the CD in DEVICE.
Return nil if there is no audio CD in DEVICE.
If OMIT-LENGTHS is non-nil, do not include track lengths in the result.
Retrieving track lengths takes almost no extra time for this function.
This function is a suitable value for `bongo-cddb-info-function'."
  (with-current-buffer (get-buffer-create " *CDDB query*")
    (erase-buffer)
    (let ((coding-system-for-read 'utf-8))
      (apply 'call-process bongo-libcddb-cddb-query-program-name nil t nil
             (nconc (when (or device bongo-cd-device)
                      (list "-i" (or device bongo-cd-device)))
                    (when bongo-cddb-server
                      (list "-s" bongo-cddb-server))
                    (when bongo-cddb-server-port
                      (list "-p" (number-to-string bongo-cddb-server-port)))
                    (list "read")))
      (goto-char (point-min))
      (when (re-search-forward "^CD contains \\(.+\\) track" nil t)
        (let ((track-count (string-to-number (match-string 1)))
              artist-name album-title album-year tracks)
          (save-excursion
            (when (re-search-forward "^Artist:\\s-*\\(.+\\)$" nil t)
              (setq artist-name (match-string 1))))
          (save-excursion
            (when (re-search-forward "^Title:\\s-*\\(.+\\)$" nil t)
              (setq album-title (match-string 1))))
          (save-excursion
            (when (re-search-forward "^Year:\\s-*\\(.+\\)$" nil t)
              (setq album-year (match-string 1))))
          (while (re-search-forward (concat "^\\s-+\\[[0-9]+\\] '\\(.+\\)'"
                                            "\\s-+.*(\\(.+\\))") nil t)
            (push (cons (match-string 1)
                        (bongo-parse-time (match-string 2)))
                  tracks))
          (cons (list artist-name album-title album-year)
                (cons track-count (nreverse tracks))))))))

(defun bongo-cdtool-cdda-info (&optional device omit-lengths)
  "Use `cdir' to find track information about the CD in DEVICE.
Return nil if there is no audio CD in DEVICE.
If OMIT-LENGTHS is non-nil, do not include track lengths in the result.
Retrieving track lengths takes almost no extra time for this function.
This function is a suitable value for `bongo-cdda-info-function'."
  (with-temp-buffer
    (apply 'call-process bongo-cdtool-cdir-program-name nil t nil "-n"
           (when (or device bongo-cd-device)
             (list "-d" (or device bongo-cd-device))))
    (goto-char (point-min))
    (when (re-search-forward "\\<in \\(.+\\) tracks\\>" nil t)
      (let ((track-count (string-to-number (match-string 1)))
            (track-lengths nil))
        (unless (and (= track-count 1)
                     (save-excursion
                       (search-forward "[DATA]" nil t)))
          (unless omit-lengths
            (while (re-search-forward "^\\s-+\\([0-9:.]+\\)" nil t)
              (push (bongo-parse-time (match-string 1)) track-lengths)))
          (cons track-count track-lengths))))))

(defun bongo-cdtool-cddb-info (&optional device omit-lengths)
  "Use `cdown' to find CDDB information about the audio CD in DEVICE.
Return nil if there is no audio CD in DEVICE.
If OMIT-LENGTHS is non-nil, do not include track lengths in the result.
Retrieving track lengths takes some extra time for this function.
This function is a suitable value for `bongo-cddb-info-function'.
Note that this function cannot report album release year information."
  (with-current-buffer (get-buffer-create " *CDDB query*")
    (erase-buffer)
    (insert "1\n")           ; Always pick first alternative.
    (apply 'call-process-region (point-min) (point)
           bongo-cdtool-cdown-program-name t t nil
           (nconc (when (or device bongo-cd-device)
                    (list "-d" (or device bongo-cd-device)))
                  (when bongo-cddb-server
                    (list "-H" bongo-cddb-server))
                  (when bongo-cddb-server-port
                    (list "-P" (number-to-string bongo-cddb-server-port)))))
    (goto-char (point-min))
    (let (track-count artist-name album-title tracks)
      (when (re-search-forward "^tracks \\([0-9]+\\)" nil t)
        (setq track-count (string-to-number (match-string 1))))
      (when (re-search-forward "^cdname \\(.+\\)$" nil t)
        (setq album-title (match-string 1)))
      (when (re-search-forward "^artist \\(.+\\)$" nil t)
        (setq artist-name (match-string 1)))
      (let (track-titles)
        (while (re-search-forward "^track \\(.+\\)$" nil t)
          (push (match-string 1) track-titles))
        (setq track-titles (nreverse track-titles))
        (let ((track-lengths (unless omit-lengths
                               (cdr (bongo-cdtool-cdda-info device)))))
          (dotimes (dummy track-count)
            (push (cons (car track-titles)
                        (unless omit-lengths
                          (car track-lengths)))
                  tracks)
            (setq track-titles (cdr track-titles))
            (unless omit-lengths
              (setq track-lengths (cdr track-lengths))))))
      (cons (list artist-name album-title nil)
            (cons track-count tracks)))))

(defun bongo-read-cd-device-name ()
  "Prompt the user for a CD device name.
Return a CD device name or nil."
  (let ((file-name
         (read-file-name (format "CD device name (default %s): "
                                 (if bongo-cd-device
                                     (format "`%s'" bongo-cd-device)
                                   "unspecified"))
                         "/dev/" bongo-cd-device t)))
    (unless (string-equal file-name "")
      (expand-file-name file-name))))

(defun bongo-insert-cd-track (track-index &optional cddb-info device)
  "Insert a new track line corresponding to an audio CD track.
Prefix argument TRACK-INDEX is the index of the CD track.
Optional argument CDDB-INFO is a CDDB info structure for the CD, as
  returned by `bongo-cddb-info', used to set the metadata for the track.
If CDDB-INFO is nil but `bongo-use-cddb' is non-nil, call `bongo-cddb-info'
  to obtain a CDDB info structure for the CD.
Optional argument DEVICE overrides `bongo-cd-device'.
With \\[universal-argument] as prefix argument, \
prompt for the CD device to use."
  (interactive
   (let ((device (or (when (consp current-prefix-arg)
                       (bongo-read-cd-device-name))
                     bongo-cd-device)))
     (list (if (integerp current-prefix-arg)
               current-prefix-arg
             (let* ((track-count (bongo-cdda-track-count device))
                    (range-string (when track-count
                                    (format " (1-%d)" track-count))))
               (read-number (format "Audio CD track number%s: "
                                    (or range-string "")))))
           nil device)))
  (when (and (null cddb-info) bongo-use-cddb)
    (setq cddb-info (bongo-cddb-info device)))
  (when (null device)
    (setq device bongo-cd-device))
  (cl-destructuring-bind ((artist-name album-title album-year)
                          track-count . tracks)
      (or cddb-info (list (list nil nil nil) nil))
    (cl-destructuring-bind (track-title . track-length)
        (or (nth (- track-index 1) tracks) (cons nil nil))
      (bongo-insert-line
       'bongo-file-name
       (format "cdda://%s@%d" (or device "") track-index)
       'bongo-infoset
       (nconc `((artist (name . ,(or artist-name "Audio CD"))))
              (when (or artist-name album-title device)
                `((album (title
                          . ,(cond ((and artist-name album-title)
                                    (format "%s (%s)" album-title
                                            (if device
                                                (format "`%s'" device)
                                              "CD")))
                                   (album-title album-title)
                                   (t (format "Device: `%s'" device))))
                         ,@(when album-year
                             `((year . ,album-year))))))
              `((track (title . ,(or track-title
                                     (format "Track %d" track-index)))
                       (index . ,(format "%02d" track-index))
                       ,@(when track-length
                           `((length . ,track-length))))))))))

(defun bongo-insert-cd (&optional device)
  "Insert a new track line for each track on an audio CD.
Optional argument DEVICE overrides `bongo-cd-device'.
With \\[universal-argument] as prefix argument, prompt for CD device to use.
With a numerical prefix argument, insert only that particular track."
  (interactive
   (list (when (consp current-prefix-arg)
           (bongo-read-cd-device-name))))
  (when (null device)
    (setq device bongo-cd-device))
  (if (integerp current-prefix-arg)
      (bongo-insert-cd-track current-prefix-arg nil device)
    (if (null bongo-cdda-info-function)
        (progn
          (message (concat "Cannot find track count of audio CD; "
                           "please customize `bongo-cdda-info-function'."))
          (bongo-insert-uri
           (format "cdda://%s" (or device ""))))
      (let* ((cdda-info (bongo-cdda-info device))
             (track-count (car cdda-info)))
        (if (null track-count)
            (error (concat "Cannot read audio CD"
                           (when device
                             (format " in `%s'" device))))
          (with-bongo-buffer
            (let ((beginning (point))
                  (cddb-info (if bongo-use-cddb
                                 (bongo-cddb-info device)
                               (cons (list nil nil nil)
                                     (cons track-count
                                           (mapcar (lambda (length)
                                                     (cons nil length))
                                                   (cdr cdda-info)))))))
              (dotimes (n track-count)
                (bongo-insert-cd-track (+ n 1) cddb-info device))
              (bongo-maybe-join-inserted-tracks beginning (point))))
          (when (and (called-interactively-p 'interactive) (not (bongo-buffer-p)))
            (message "Inserted %d tracks." track-count)))))))


;;;; DWIM commands

(defun bongo-dwim (&optional n)
  "In Bongo, do what the user means to the object at point.
If point is on a header, collapse or expand the section below.
If point is on a playlist track, just start playing the track.
If point is on a library track, enqueue the track in the playlist
  and then immediately start playing it.
With numerical prefix argument N, play or enqueue the next N tracks
  or sections (even if point is on a header).
With \\[universal-argument] as prefix argument, play or enqueue-play \
the track at point
  after explicitily specifying the backend to use.
With \\[universal-argument] as prefix argument on a header, \
just start playing the section.
With \\[universal-argument] \\[universal-argument] as prefix argument \
on a header, start playing the section
  after explicitly specifying the backend to use.
In other words, add an extra \\[universal-argument] when point is \
on a header and you want
  to play the section instead of collapsing/expanding it.
If point is neither on a track nor on a header, do nothing."
  (interactive "P")
  (cond ((or n (bongo-track-line-p))
         (if (not (consp n))
             (bongo-play-lines n)
           (when (or (bongo-track-line-p)
                     (> (prefix-numeric-value n) 4))
             (call-interactively 'bongo-set-backend-for-track))
           (bongo-play-lines)))
        ((bongo-header-line-p)
         (bongo-toggle-collapsed))))

(defun bongo-mouse-dwim (event)
  "In Bongo, do what the user means to the object that was clicked on.
EVENT is generated by Emacs when this function is called interactively.
See `bongo-dwim', which this function delegates to."
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

(defun bongo-formatted-infoset ()
  "Return the formatted infoset of the active player, or nil."
  (with-bongo-playlist-buffer
    (when bongo-player
      (bongo-format-infoset
       (bongo-player-infoset bongo-player)))))

(defun bongo-pausing-supported-p ()
  "Return non-nil if the active player supports pausing."
  (with-bongo-playlist-buffer
    (and (bongo-playing-p)
         (bongo-player-pausing-supported-p bongo-player))))

(defun bongo-paused-p ()
  "Return non-nil if the active player is paused."
  (with-bongo-playlist-buffer
    (and (bongo-playing-p)
         (bongo-player-paused-p bongo-player))))

(defun bongo-seeking-supported-p ()
  "Return non-nil if the active player supports seeking."
  (with-bongo-playlist-buffer
    (and (bongo-playing-p)
         (bongo-player-seeking-supported-p bongo-player))))

(defun bongo-elapsed-time ()
  "Return the number of seconds played so far of the current track.
Return nil if the active player cannot report this."
  (with-bongo-playlist-buffer
    (when bongo-player
      (bongo-player-elapsed-time bongo-player))))

(defun bongo-remaining-time ()
  "Return the number of seconds remaining of the current track.
Return nil if the active player cannot report this."
  (let ((elapsed-time (bongo-elapsed-time))
        (total-time (bongo-total-time)))
    (when (and elapsed-time total-time)
      (- total-time elapsed-time))))

(defun bongo-total-time ()
  "Return the length of the currently playing track in seconds.
Return nil if the active player cannot report this."
  (with-bongo-playlist-buffer
    (when bongo-player
      (bongo-player-total-time bongo-player))))

(defvar bongo-current-track-marker nil
  "Marker pointing at the current track line, if any.
The current track line is the line of the currently playing track,
or that of the last played track if no track is currently playing.")
(make-variable-buffer-local 'bongo-current-track-marker)

(defun bongo-point-at-current-track-line ()
  (when bongo-current-track-marker
    (let ((position (marker-position bongo-current-track-marker))
          (line-move-ignore-invisible nil))
      (and (bongo-track-line-p position) position))))

(defun bongo-set-current-track-marker (marker)
  (unless (eq marker bongo-current-track-marker)
    (move-marker marker (bongo-point-at-current-track-line))
    (when bongo-current-track-marker
      (move-marker bongo-current-track-marker nil))
    (setq bongo-current-track-marker marker)))

(defun bongo-set-current-track-position (&optional position)
  (move-marker bongo-current-track-marker (or position (point))))

(defun bongo-unset-current-track-position ()
  (move-marker bongo-current-track-marker nil))

(defun bongo-current-track-line-p (&optional point)
  "Return non-nil if the line at POINT is the current track line."
  (and (not (null (bongo-point-at-current-track-line)))
       (>= (bongo-point-at-current-track-line)
           (bongo-point-before-line point))
       (< (bongo-point-at-current-track-line)
          (bongo-point-after-line point))))

(defun bongo-fringe-bitmap-from-strings (strings)
  (vconcat (mapcar (lambda (string)
                     (string-to-number
                      (replace-regexp-in-string
                       "#" "1" (replace-regexp-in-string "\\." "0" string)) 2))
                   strings)))

(when (fboundp 'define-fringe-bitmap)
  (define-fringe-bitmap 'bongo-playing-11
    (bongo-fringe-bitmap-from-strings
     '("........"
       ".#......"
       ".##....."
       ".###...."
       ".####..."
       ".#####.."
       ".####..."
       ".###...."
       ".##....."
       ".#......"
       "........")))

  (define-fringe-bitmap 'bongo-playing-18
    (bongo-fringe-bitmap-from-strings
     '("................"
       "................"
       "....##.........."
       "....###........."
       "....####........"
       "....#####......."
       "....######......"
       "....#######....."
       "....########...."
       "....########...."
       "....#######....."
       "....######......"
       "....#####......."
       "....####........"
       "....###........."
       "....##.........."
       "................"
       "................"))
    18 16)

  (define-fringe-bitmap 'bongo-paused-11
    (bongo-fringe-bitmap-from-strings
     '("........"
       ".##..##."
       ".##..##."
       ".##..##."
       ".##..##."
       ".##..##."
       ".##..##."
       ".##..##."
       "........")))

  (define-fringe-bitmap 'bongo-paused-18
    (bongo-fringe-bitmap-from-strings
     '("................"
       "................"
       "..####....####.."
       "..####....####.."
       "..####....####.."
       "..####....####.."
       "..####....####.."
       "..####....####.."
       "..####....####.."
       "..####....####.."
       "..####....####.."
       "..####....####.."
       "..####....####.."
       "..####....####.."
       "..####....####.."
       "..####....####.."
       "................"
       "................"))
    18 16)

  (define-fringe-bitmap 'bongo-stopped-18
    (bongo-fringe-bitmap-from-strings
     '("................"
       "................"
       "................"
       "..############.."
       "..############.."
       "..############.."
       "..############.."
       "..############.."
       "..############.."
       "..############.."
       "..############.."
       "..############.."
       "..############.."
       "..############.."
       "..############.."
       "................"
       "................"
       "................"))
    18 16)

  (define-fringe-bitmap 'bongo-stopped-11
    (bongo-fringe-bitmap-from-strings
     '("........"
       ".######."
       ".######."
       ".######."
       ".######."
       ".######."
       ".######."
       ".######."
       "........"))))

(defvar bongo-queued-track-marker nil
  "Marker pointing at the queued track, if any.
This is used by `bongo-play-queued'.

The functions `bongo-set-queued-track-position' and
`bongo-unset-queued-track-position' can properly manipulate this
variable and its value.")
(make-variable-buffer-local 'bongo-queued-track-marker)

(defun bongo-point-at-queued-track-line ()
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
    (equal (bongo-point-at-queued-track-line)
           (bongo-point-at-bol))))

(defun bongo-unset-queued-track-position ()
  "Make `bongo-queued-track-marker' point nowhere.
In addition, set `bongo-next-action' to the value of
`bongo-stored-next-action' and set the latter to nil."
  (when bongo-queued-track-arrow-timer
    (cancel-timer bongo-queued-track-arrow-timer)
    (setq bongo-queued-track-arrow-timer nil))
  (when (bongo-point-at-queued-track-line)
    (setq bongo-next-action bongo-stored-next-action)
    (setq bongo-stored-next-action nil))
  (move-marker bongo-queued-track-marker nil)
  (move-marker bongo-queued-track-arrow-marker nil))

(defun bongo-set-queued-track-position (&optional point)
  "Make `bongo-queued-track-marker' point to the track at POINT.
In addition, unless `bongo-next-action' is already set to
`bongo-play-queued', set `bongo-stored-next-action' to the value
of `bongo-next-action' and set the latter to `bongo-play-queued'."
  (interactive)
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

(defun bongo-play-line (&optional point)
  "Start playing the track on the line at POINT.
If there is no track at POINT, play the first track after POINT or
signal an error if there is no track after POINT."
  (interactive)
  (cond ((bongo-playlist-buffer-p)
         (with-point-at-bongo-track point
           (with-imminent-bongo-player-start
             (when bongo-player
               (bongo-player-stop bongo-player))
             (bongo-set-current-track-position)
             (let ((player
                    (if (bongo-action-track-line-p)
                        (bongo-start-action-player
                         (bongo-line-action))
                      (bongo-play-file
                       (bongo-line-file-name)
                       (bongo-line-get-property 'bongo-backend)))))
               (bongo-player-put player 'infoset (bongo-line-infoset))
               (setq bongo-player player)
               (bongo-line-set-property 'bongo-player player)
               (bongo-set-current-track-marker bongo-playing-track-marker)
               (when bongo-header-line-mode
                 (bongo-update-header-line-string))
               (when bongo-mode-line-indicator-mode
                 (bongo-update-mode-line-indicator-string))
               (run-hooks 'bongo-player-started-hook)
               (bongo-redisplay-line)))))
        ((bongo-library-buffer-p)
         (save-excursion
           (bongo-goto-point point)
           (bongo-play-lines)))
        (t (error "Not a Bongo buffer"))))

(defun bongo-play-lines (&optional n)
  "Start playing the next N tracks or sections.
If N is nil, just start playing the track at point.
Otherwise, start playing the track at point and stop after N tracks.
In Bongo Library mode, enqueue and play in the nearest playlist."
  (interactive "P")
  (cond ((bongo-playlist-buffer-p)
         (when (not (null n))
           (bongo-stop (prefix-numeric-value n)))
         (bongo-play-line))
        ((bongo-library-buffer-p)
         (let ((position (save-excursion
                           (if (bongo-playing-p)
                               (bongo-insert-enqueue-lines
                                (prefix-numeric-value n))
                             (bongo-append-enqueue-lines
                              (prefix-numeric-value n))))))
           (with-bongo-playlist-buffer
             (bongo-play-line position))))
        (t (error "Not a Bongo buffer"))))

(defun bongo-play-region (beg end)
  "Start playing the tracks and sections between BEG and END.
That is, start playing the first track after BEG and stop
  playback at the first track after END.
In Bongo Library mode, enqueue and play in the nearest playlist."
  (interactive "r")
  (cond ((bongo-library-buffer-p)
         (let ((position (if (bongo-playing-p)
                             (bongo-insert-enqueue-region beg end)
                           (bongo-append-enqueue-region beg end))))
           (with-bongo-playlist-buffer
             (bongo-play-line position))))
        ((bongo-playlist-buffer-p)
         (save-excursion
           (goto-char (bongo-point-at-bol-forward end))
           (bongo-insert-line 'bongo-action '(bongo-stop)))
         (bongo-play-line beg))
        (t (error "Not a Bongo buffer"))))

(defun bongo-play-marked ()
  "Start playing the marked tracks.
In Bongo Library mode, enqueue and play in the nearest playlist.
In Bongo Playlist mode, this is not implemented, so signal an error."
  (interactive)
  (cond ((bongo-library-buffer-p)
         (let ((position (if (bongo-playing-p)
                             (bongo-insert-enqueue-marked)
                           (bongo-append-enqueue-marked))))
           (with-bongo-playlist-buffer
             (bongo-play-line position))))
        ((bongo-playlist-buffer-p)
         (error "Intra-playlist enqueuing is not yet supported"))
        (t
         (error "Not a Bongo buffer"))))

(defun bongo-play (&optional n)
  "Start playing the marked tracks, or the region, or N objects.
In Bongo Library mode, enqueue and play in the nearest playlist."
  (interactive "P")
  (cond ((bongo-library-buffer-p)
         (let ((position (if (bongo-playing-p)
                             (bongo-insert-enqueue
                              (and n (prefix-numeric-value n)))
                           (bongo-append-enqueue
                            (and n (prefix-numeric-value n))))))
           (with-bongo-playlist-buffer
             (bongo-play-line position))))
        ((bongo-playlist-buffer-p)
         (if bongo-marking
             (error "Intra-playlist enqueuing is not yet supported")
           (cond ((not (null n))
                  (bongo-play-lines (prefix-numeric-value n)))
                 ((bongo-region-active-p)
                  (bongo-play-region (region-beginning) (region-end)))
                 (t
                  (bongo-play-lines)))))
        (t (error "Not a Bongo buffer"))))

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
  (bongo-play-line (or (bongo-point-at-queued-track-line)
                       (error "No queued track")))
  (bongo-unset-queued-track-position))

(defun bongo-repeating-playback-mode (&optional default)
  "Switch to repeating playback mode in the nearest playlist buffer.
In repeating playback mode, the current track is played over and over.
With prefix argument DEFAULT, make repeating playback the default mode.
This function sets the buffer-local or global value of `bongo-next-action'."
  (interactive "P")
  (if (not default)
      (with-bongo-playlist-buffer
        (setq bongo-next-action 'bongo-replay-current)
        (message "Switched to repeating playback mode."))
    (setq-default bongo-next-action 'bongo-replay-current)
    (message "Repeating playback is now the default mode."))
  (force-mode-line-update))

(defun bongo-replay-current (&optional argument)
  "Play the current track in the nearest playlist from the start.
With \\[universal-argument] as prefix ARGUMENT, just switch to \
repeating playback mode."
  (interactive "P")
  (if (consp argument)
      (bongo-repeating-playback-mode)
    (with-bongo-playlist-buffer
      (bongo-play-line (or (bongo-point-at-current-track-line)
                           (error "No current track"))))))

(put 'bongo-replay-current
     'bongo-playback-mode-indicator "repeat")

(defun bongo-progressive-playback-mode (&optional default)
  "Switch to progressive playback mode in the nearest playlist buffer.
In progressive playback mode, tracks are played in order (top-to-bottom).
With prefix argument DEFAULT, make progressive playback the default mode.
This function sets the buffer-local or global value of `bongo-next-action'."
  (interactive "P")
  (if (not default)
      (with-bongo-playlist-buffer
        (setq bongo-next-action 'bongo-play-next-or-stop)
        (message "Switched to progressive playback mode."))
    (setq-default bongo-next-action 'bongo-play-next-or-stop)
    (message "Progressive playback is now the default mode."))
  (force-mode-line-update))

(put 'bongo-progressive-playback-mode 'bongo-action-description
     "Start playing tracks in order (downwards)")

(defun bongo-play-next (&optional n)
  "Start playing the next track in the nearest Bongo playlist buffer.
If there is no next track to play, signal an error.
With numerical prefix argument N, skip that many tracks.
With \\[universal-argument] as prefix argument, just switch to \
progressive playback mode.
With \\[universal-argument] \\[universal-argument] as prefix argument, \
insert an action track at point."
  (interactive "P")
  (if (consp n)
      (if (> (prefix-numeric-value n) 4)
          (bongo-insert-line 'bongo-action '(bongo-progressive-playback-mode))
        (bongo-progressive-playback-mode))
    (if (< (prefix-numeric-value n) 0)
        (bongo-play-previous (- (prefix-numeric-value n)))
      (with-imminent-bongo-player-start
        (bongo-stop)
        (when bongo-mark-played-tracks
          (bongo-mark-current-track-line-as-played))
        (bongo-next n)
        (bongo-start)))))

(defun bongo-play-next-or-stop (&optional n)
  "Maybe start playing the next track in the nearest playlist buffer.
If there is no next track to play, just stop playback.
With numerical prefix argument N, skip that many tracks.
With \\[universal-argument] as prefix argument, just switch to \
progressive playback.
With \\[universal-argument] \\[universal-argument] as prefix argument, \
insert an action track at point."
  (interactive "P")
  (condition-case nil
      (bongo-play-next n)
    (error (bongo-stop))))

(put 'bongo-play-next
     'bongo-playback-mode-indicator "")
(put 'bongo-play-next-or-stop
     'bongo-playback-mode-indicator "")

(defun bongo-next (&optional n)
  "Make the next track current in the nearest playlist buffer.
If there is no next track, signal an error.
With prefix argument N, skip that many tracks."
  (interactive "p")
  (if (bongo-playing-p)
      (bongo-play-next n)
    (with-bongo-playlist-buffer
      (let ((line-move-ignore-invisible nil))
        (save-excursion
          (goto-char (or (bongo-point-at-current-track-line)
                         (bongo-point-at-first-track-line)
                         (error "No tracks in playlist")))
          (dotimes (dummy (prefix-numeric-value n))
            (goto-char (or (bongo-point-at-next-track-line)
                           (error "No next track"))))
          (bongo-set-current-track-position))))))

(defun bongo-regressive-playback-mode (&optional default)
  "Switch to regressive playback mode in the nearest playlist buffer.
In regressive playback mode, tracks are played in reverse order.
With prefix argument DEFAULT, make regressive playback the default mode.
This function sets the buffer-local or global value of `bongo-next-action'."
  (interactive "P")
  (if (not default)
      (with-bongo-playlist-buffer
        (setq bongo-next-action 'bongo-play-previous-or-stop)
        (message "Switched to regressive playback mode."))
    (setq-default bongo-next-action 'bongo-play-previous-or-stop)
    (message "Regressive playback is now the default mode."))
  (force-mode-line-update))

(put 'bongo-regressive-playback-mode 'bongo-action-description
     "Start playing tracks in reverse order (upwards)")

(defun bongo-play-previous (&optional n)
  "Start playing the previous track in the nearest playlist buffer.
If there is no previous track to play, signal an error.
With numerical prefix argument N, skip that many tracks.
With \\[universal-argument] as prefix argument, just switch to \
regressive playback mode.
With \\[universal-argument] \\[universal-argument] as prefix argument, \
insert an action track at point."
  (interactive "P")
  (if (consp n)
      (if (> (prefix-numeric-value n) 4)
          (bongo-insert-line 'bongo-action '(bongo-regressive-playback-mode))
        (bongo-regressive-playback-mode))
    (if (< (prefix-numeric-value n) 0)
        (bongo-play-next (- (prefix-numeric-value n)))
      (with-imminent-bongo-player-start
        (bongo-stop)
        (when bongo-mark-played-tracks
          (bongo-mark-current-track-line-as-played))
        (bongo-previous n)
        (bongo-start)))))

(defun bongo-play-previous-or-stop (&optional n)
  "Maybe start playing the previous track in the playlist buffer.
If there is no previous track to play, just stop playback.
With numerical prefix argument N, skip that many tracks.
With \\[universal-argument] as prefix argument, just switch to \
regressive playback.
With \\[universal-argument] \\[universal-argument] as prefix argument, \
insert an action track at point."
  (interactive "P")
  (condition-case nil
      (bongo-play-previous n)
    (error (bongo-stop))))

(put 'bongo-play-previous
     'bongo-playback-mode-indicator "reverse")
(put 'bongo-play-previous-or-stop
     'bongo-playback-mode-indicator "reverse")

(defun bongo-previous (&optional n)
  "Make the previous track current in the nearest playlist buffer.
If there is no previous track, signal an error.
With prefix argument N, skip that many tracks."
  (interactive "p")
  (if (bongo-playing-p)
      (bongo-play-previous n)
    (with-bongo-playlist-buffer
      (let ((line-move-ignore-invisible nil))
        (save-excursion
          (goto-char (or (bongo-point-at-current-track-line)
                         (bongo-point-at-last-track-line)
                         (error "No tracks in playlist")))
          (dotimes (dummy (prefix-numeric-value n))
            (goto-char (or (bongo-point-at-previous-track-line)
                           (error "No previous track"))))
          (bongo-set-current-track-position))))))

(defun bongo-random-playback-mode (&optional default)
  "Switch to random playback mode in the nearest playlist buffer.
In progressive playback mode, tracks are played in random order.
With prefix argument DEFAULT, make random playback the default mode.
This function sets the buffer-local or global value of `bongo-next-action'."
  (interactive "P")
  (if (not default)
      (with-bongo-playlist-buffer
        (setq bongo-next-action 'bongo-play-random-or-stop)
        (message "Switched to random playback mode."))
    (setq-default bongo-next-action 'bongo-play-random-or-stop)
    (message "Random playback is now the default mode."))
  (force-mode-line-update))

(put 'bongo-random-playback-mode 'bongo-action-description
     "Start playing tracks in random order")

(defun bongo-randomly-playable-track-line-p (&optional point)
  "Return non-nil for randomly playable track lines.
That is, if the track is an appropriate choice during random playback.
Inspect the line at POINT; or the one at point, if POINT is nil.
Currently, the only non-randomly-playable tracks are action tracks."
  (and (bongo-track-line-p point)
       (not (bongo-action-track-line-p point))))

(defun bongo-unplayed-randomly-playable-track-line-p (&optional point)
  "Return non-nil for unplayed and randomly playable track lines.
Inspect the line at POINT; or the one at point, if POINT is nil.
See `bongo-randomly-playable-track-line-p' and `bongo-played-track-line-p'."
  (and (bongo-randomly-playable-track-line-p point)
       (not (and bongo-mark-played-tracks
                 (bongo-currently-playing-track-line-p)))
       (not (bongo-played-track-line-p))))

(defun bongo-play-random (&optional argument)
  "Start playing a random track in the nearest Bongo playlist buffer.
If there are no randomly playable tracks, signal an error.
Randomly playable tracks are unplayed non-action tracks.
With \\[universal-argument] as prefix ARGUMENT, just switch to \
random playback mode.
With \\[universal-argument] \\[universal-argument] as prefix ARGUMENT, \
insert an action track at point."
  (interactive "P")
  (with-bongo-playlist-buffer
    (if (consp argument)
        (if (> (prefix-numeric-value argument) 4)
            (bongo-insert-line 'bongo-action '(bongo-random-playback-mode))
          (bongo-random-playback-mode))
      (let ((line-move-ignore-invisible nil))
        (cond ((null (bongo-point-at-first-line-satisfying
                      'bongo-randomly-playable-track-line-p))
               (error "No randomly playable tracks in playlist"))
              ((null (bongo-point-at-first-line-satisfying
                      'bongo-unplayed-randomly-playable-track-line-p))
               (error (concat "No unplayed tracks in playlist; use "
                              "`M-x bongo-reset-playlist' to reset")))
              (t (bongo-play-line
                  (bongo-point-at-random-line-satisfying
                   'bongo-unplayed-randomly-playable-track-line-p))))))))

(defun bongo-play-random-or-stop (&optional argument)
  "Start playing a random track in the nearest Bongo playlist buffer.
If there are no randomly playable tracks, just stop playback.
Randomly playable tracks are unplayed non-action tracks.
With \\[universal-argument] as prefix ARGUMENT, just switch to \
random playback mode.
With \\[universal-argument] \\[universal-argument] as prefix ARGUMENT, \
insert an action track at point."
  (interactive "P")
  (condition-case nil
      (bongo-play-random argument)
    (error (bongo-stop))))

(put 'bongo-play-random
     'bongo-playback-mode-indicator "random")
(put 'bongo-play-random-or-stop
     'bongo-playback-mode-indicator "random")

;;;###autoload
(defun bongo-start (&optional called-interactively-p)
  "Start playing the current track in the nearest playlist buffer.
If there is no current track, perform the action appropriate for the current
  playback mode (for example, for regressive playback, play the last track).
However, if something is already playing, do nothing.
When called interactively and the current track is a stop action track,
  continue playback as if the action track had finished playing.
CALLED-INTERACTIVELY-P is non-nil when called interactively."
  (interactive (list 'called-interactively-p))
  (with-bongo-playlist-buffer
    (unless (bongo-playing-p)
      (let ((position (bongo-point-at-current-track-line))
            (line-move-ignore-invisible nil))
        (cond ((null position)
               (cl-case bongo-next-action
                 ((bongo-play-next bongo-play-next-or-stop)
                  (bongo-play-line (bongo-point-at-first-track-line)))
                 ((bongo-play-previous bongo-play-previous-or-stop)
                  (bongo-play-line (bongo-point-at-last-track-line)))
                 (t
                  (bongo-perform-next-action))))
              ((and called-interactively-p
                    (bongo-action-track-line-p position)
                    (equal (bongo-line-action position)
                           '(bongo-stop)))
               (bongo-perform-next-action))
              (t
               (bongo-play-line position)))))))

(defun bongo-start/stop-playback-mode (&optional default)
  "Switch to start/stop playback mode in the nearest playlist buffer.
In start/stop playback mode, playback stops after each track.
With prefix argument DEFAULT, make start/stop playback the default mode.
This function sets the buffer-local or global value of `bongo-next-action'."
  (interactive "P")
  (if (not default)
      (with-bongo-playlist-buffer
        (setq bongo-next-action 'bongo-stop)
        (message "Switched to start/stop playback mode."))
    (setq-default bongo-next-action 'bongo-stop)
    (message "Start/stop playback is now the default mode."))
  (force-mode-line-update))

(defun bongo-stop (&optional n)
  "Permanently stop playback in the nearest Bongo playlist buffer.
With numerical prefix argument N, stop playback after N tracks.
With \\[universal-argument] as prefix argument, just switch to \
start/stop playback.
With \\[universal-argument] \\[universal-argument] as prefix argument, \
stop when playback reaches point."
  (interactive "P")
  (with-bongo-playlist-buffer
    (if (or (null n) (zerop (prefix-numeric-value n)))
        (when bongo-player
          (bongo-player-stop bongo-player))
      (if (consp n)
          (if (> (prefix-numeric-value n) 4)
              (bongo-insert-line 'bongo-action '(bongo-stop))
            (bongo-start/stop-playback-mode))
        (let ((line-move-ignore-invisible nil))
          (save-excursion
            (goto-char (or (bongo-point-at-current-track-line)
                           (bongo-point-at-first-track-line)
                           (error "No tracks in playlist")))
            (catch 'done
              (if (> n 0)
                  (dotimes (dummy n)
                    (let ((position (bongo-point-at-next-track-line)))
                      (if position
                          (goto-char position)
                        (goto-char (bongo-point-after-line))
                        (throw 'done nil))
                      (when (and (bongo-action-track-line-p)
                                 (equal (bongo-line-action) '(bongo-stop)))
                        (bongo-delete-line))))
                (dotimes (dummy (- -1 n))
                  (goto-char (or (bongo-point-at-previous-track-line)
                                 (throw 'done nil))))))
            (bongo-insert-line 'bongo-action '(bongo-stop))))))))

(put 'bongo-stop 'bongo-playback-mode-indicator "stop")
(put 'bongo-stop 'bongo-action-description
     (lambda (bongo-stop &optional n)
       (if (integerp n)
           (format "Stop playback after %d track%s"
                   n (if (= n 1) "" "s"))
         "Stop playback")))

;;;###autoload
(defun bongo-start/stop (&optional argument called-interactively-p)
  "Start or stop playback in the nearest Bongo playlist buffer.
With prefix ARGUMENT, call `bongo-stop' even if already stopped.
CALLED-INTERACTIVELY-P is non-nil when called interactively."
  (interactive (list current-prefix-arg 'called-interactively-p))
  (if (or argument (bongo-playing-p))
      (bongo-stop argument)
    (bongo-start called-interactively-p)))

(defun bongo-pause/resume ()
  "Pause or resume playback in the nearest Bongo playlist buffer.
This functionality may not be available for all backends."
  (interactive)
  (with-bongo-playlist-buffer
    (if bongo-player
        (bongo-player-pause/resume bongo-player)
      (error "No active player"))))

(defun bongo-minibuffer-seek-string ()
  (save-window-excursion
    (with-temp-buffer
      (let ((bongo-seek-buffer (current-buffer)))
        (select-window (minibuffer-window))
        (bongo-seek-redisplay)
        (buffer-string)))))

(defun bongo-seek-forward (&optional n)
  "Seek N units forward in the currently playing track.
The time unit is currently backend-specific.
This functionality may not be available for all backends."
  (interactive "p")
  (let ((seeking-interactively (eq major-mode 'bongo-seek-mode)))
    (with-bongo-playlist-buffer
      (if (null bongo-player)
          (error "No active player")
        (bongo-player-seek-by bongo-player n)
        (unless seeking-interactively
          (let ((message-log-max nil))
            (with-temp-message (bongo-minibuffer-seek-string)
              (sit-for 2))))))))

(defun bongo-seek-backward (&optional n)
  "Seek N units backward in the currently playing track.
The time unit it currently backend-specific.
This functionality may not be available for all backends."
  (interactive "p")
  (let ((seeking-interactively (eq major-mode 'bongo-seek-mode)))
    (with-bongo-playlist-buffer
     (if (null bongo-player)
         (error "No active player")
       (bongo-player-seek-by bongo-player (- n))
       (unless seeking-interactively
         (let ((message-log-max nil))
           (with-temp-message (bongo-minibuffer-seek-string)
             (sit-for 2))))))))

(defun bongo-seek-to (position)
  "Seek to POSITION (seconds) in the currently playing track.
This functionality may not be available for all backends."
  (interactive
   (with-bongo-playlist-buffer
     (if bongo-player
         (list
          (let ((unit (bongo-player-get bongo-player 'seek-unit)))
            (cond ((null unit)
                   (error "This player does not support seeking"))
                  ((eq unit 'frames)
                   (read-number "Seek to (in frames): "))
                  ((eq unit 'seconds)
                   (let ((total-time
                          (bongo-player-total-time bongo-player)))
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
  (let ((seeking-interactively (eq major-mode 'bongo-seek-mode)))
    (with-bongo-playlist-buffer
      (if (null bongo-player)
          (error "No active player")
        (bongo-player-seek-to bongo-player position)
        (unless seeking-interactively
          (let ((message-log-max nil))
            (with-temp-message (bongo-minibuffer-seek-string)
              (sit-for 2))))))))


;;;; Interactive seeking

(defcustom bongo-seek-electric-mode t
  "Run Bongo Seek electrically, in the echo area.
Electric mode saves some space, but uses its own command loop."
  :type 'boolean
  :group 'bongo)

(defvar bongo-seeking-electrically nil
  "Non-nil in the dynamic scope of electric `bongo-seek'.
That is, when `bongo-seek-electric-mode' is non-nil.")

(defface bongo-seek-bar '((t nil))
  "Face used for the indicator bar in Bongo Seek mode."
  :group 'bongo-faces)

(defface bongo-filled-seek-bar
  '((t (:inverse-video t :bold t :inherit bongo-seek-bar)))
  "Face used for the filled part of the indicator bar."
  :group 'bongo-faces)

(defface bongo-unfilled-seek-bar
  '((t (:inherit bongo-seek-bar)))
  "Face used for the unfilled part of the indicator bar."
  :group 'bongo-faces)

(defface bongo-seek-message '((t nil))
  "Face used for messages in Bongo Seek mode."
  :group 'bongo-faces)

(defun bongo-seek-quit ()
  "Quit Bongo Seek mode."
  (interactive)
  (if bongo-seek-electric-mode
      (throw 'bongo-seek-done nil)
    (ignore-errors
      (while (get-buffer-window bongo-seek-buffer)
        (delete-window (get-buffer-window bongo-seek-buffer))))
    (kill-buffer bongo-seek-buffer)
    (setq bongo-seek-buffer nil)))

(defun bongo-seek-forward-3 (&optional n)
  "Seek 3 N seconds forward in the currently playing track."
  (interactive "p")
  (bongo-seek-forward (* 3 (or n 1))))

(defun bongo-seek-backward-3 (&optional n)
  "Seek 3 N seconds backward in the currently playing track."
  (interactive "p")
  (bongo-seek-backward (* 3 (or n 1))))

(defun bongo-seek-forward-10 (&optional n)
  "Seek 10 N seconds forward in the currently playing track."
  (interactive "p")
  (bongo-seek-forward (* 10 (or n 1))))

(defun bongo-seek-backward-10 (&optional n)
  "Seek 10 N seconds backward in the currently playing track."
  (interactive "p")
  (bongo-seek-backward (* 10 (or n 1))))

(defun bongo-seek-forward-60 (&optional n)
  "Seek N minutes forward in the currently playing track."
  (interactive "p")
  (bongo-seek-forward (* 60 (or n 1))))

(defun bongo-seek-backward-60 (&optional n)
  "Seek N minutes backward in the currently playing track."
  (interactive "p")
  (bongo-seek-backward (* 60 (or n 1))))

(defvar bongo-seek-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "\C-b" 'bongo-seek-backward)
    (define-key map "\C-f" 'bongo-seek-forward)
    (define-key map "b" 'bongo-seek-backward)
    (define-key map "f" 'bongo-seek-forward)
    (define-key map "B" 'bongo-seek-backward-3)
    (define-key map "F" 'bongo-seek-forward-3)
    (define-key map [left] 'bongo-seek-backward-3)
    (define-key map [right] 'bongo-seek-forward-3)
    (define-key map "\M-b" 'bongo-seek-backward-10)
    (define-key map "\M-f" 'bongo-seek-forward-10)
    (define-key map "\M-B" 'bongo-seek-backward-10)
    (define-key map "\M-F" 'bongo-seek-forward-10)
    (define-key map [(control left)] 'bongo-seek-backward-10)
    (define-key map [(control right)] 'bongo-seek-forward-10)
    (define-key map [(meta left)] 'bongo-seek-backward-10)
    (define-key map [(meta right)] 'bongo-seek-forward-10)
    (define-key map [up] 'bongo-seek-forward-10)
    (define-key map [down] 'bongo-seek-backward-10)
    (define-key map [(control up)] 'bongo-seek-forward-60)
    (define-key map [(control down)] 'bongo-seek-backward-60)
    (define-key map [(meta up)] 'bongo-seek-forward-60)
    (define-key map [(meta down)] 'bongo-seek-backward-60)
    (define-key map "\M-\C-b" 'bongo-seek-backward-60)
    (define-key map "\M-\C-f" 'bongo-seek-forward-60)
    (define-key map [?\C-\M-\S-b] 'bongo-seek-backward-60)
    (define-key map [?\C-\M-\S-f] 'bongo-seek-forward-60)
    (define-key map [(control meta left)] 'bongo-seek-backward-60)
    (define-key map [(control meta right)] 'bongo-seek-forward-60)
    (define-key map "a" 'bongo-replay-current)
    (define-key map "e" 'bongo-play-next)
    (define-key map "\C-a" 'bongo-replay-current)
    (define-key map "\C-e" 'bongo-perform-next-action)
    (define-key map [home] 'bongo-replay-current)
    (define-key map [end] 'bongo-perform-next-action)
    (define-key map "p" 'bongo-play-previous)
    (define-key map "n" 'bongo-play-next)
    (define-key map "\C-p" 'bongo-play-previous)
    (define-key map "\C-n" 'bongo-play-next)
    (define-key map " " 'bongo-pause/resume)
    (define-key map "\C-c\C-a" 'bongo-replay-current)
    (define-key map "\C-c\C-e" 'bongo-perform-next-action)
    (define-key map "\C-c\C-p" 'bongo-play-previous)
    (define-key map "\C-c\C-n" 'bongo-play-next)
    (define-key map "\C-c\C-r" 'bongo-play-random)
    (define-key map "\C-c\C-s" 'bongo-start/stop)
    (define-key map "\C-l" 'bongo-seek-redisplay)
    (define-key map "l" 'bongo-seek-redisplay)
    (define-key map "\C-g" 'bongo-seek-quit)
    (define-key map "\C-m" 'bongo-seek-quit)
    (define-key map "q" 'bongo-seek-quit)
    (define-key map "s" 'bongo-seek-quit)
    (define-key map [escape escape] 'bongo-seek-quit)
    map)
  "Keymap for Bongo Seek mode.")

(defun bongo-seek-mode ()
  "Major mode for interactively seeking in Bongo tracks.

\\{bongo-seek-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'bongo-seek-mode)
  (setq mode-name "Bongo Seek")
  (use-local-map bongo-seek-mode-map)
  (setq buffer-undo-list t)
  (run-mode-hooks 'bongo-seek-mode-hook))

(defvar bongo-seek-redisplaying nil
  "Non-nil in the dynamic scope of `bongo-seek-redisplay'.")

(defun bongo-seek-status-string (width)
  "Return string of length WIDTH indicating the current track position."
  (with-temp-buffer
    (when (and (bongo-playing-p)
               (bongo-elapsed-time))
      (insert (bongo-format-seconds (bongo-elapsed-time)))
      (insert " "))
    (let* ((end-string (when (and (bongo-playing-p)
                                  (bongo-remaining-time))
                         (concat " -" (bongo-format-seconds
                                       (bongo-remaining-time)))))
           (bar-start (point))
           (available-width (- width bar-start (length end-string)))
           (bar-width (if (and (bongo-playing-p)
                               (bongo-elapsed-time)
                               (bongo-total-time))
                          (round
                           (* (min 1 (/ (float (bongo-elapsed-time))
                                        (bongo-total-time)))
                              available-width))
                        available-width))
           (label (if (and (bongo-playing-p)
                           (bongo-elapsed-time)
                           (bongo-total-time))
                      (format " %d%% %s"
                              (/ (* (bongo-elapsed-time) 100.0)
                                 (bongo-total-time))
                              (if (bongo-paused-p) "(paused) " ""))
                    (cond ((not (bongo-playing-p))
                           " (no currently playing track) ")
                          ((null (bongo-total-time))
                           " (track length not available) ")
                          ((null (bongo-elapsed-time))
                           " (elapsed time not available) "))))
           (label-width (length label)))
      (insert-char ?\  available-width)
      (goto-char
       (+ bar-start
          (if (< bar-width label-width)
              (1+ bar-width)
            (/ (1+ (- bar-width label-width)) 2))))
      (delete-char label-width)
      (insert label)
      (put-text-property bar-start (+ bar-start bar-width)
                         'face (if (and (bongo-playing-p)
                                        (bongo-elapsed-time)
                                        (bongo-total-time))
                                   'bongo-filled-seek-bar
                                 'bongo-seek-message))
      (put-text-property (+ bar-start bar-width)
                         (+ bar-start available-width)
                         'face 'bongo-unfilled-seek-bar)
      (when end-string
        (goto-char (point-max))
        (insert end-string))
      (goto-char (+ bar-start bar-width)))
    (buffer-string)))

(defun bongo-seek-redisplay ()
  "Update the Bongo Seek buffer to reflect the current track position."
  (interactive)
  (unless bongo-seek-redisplaying
    (let ((bongo-seek-redisplaying t))
      (let ((inhibit-read-only t))
        (set-buffer bongo-seek-buffer)
        (when (and bongo-seeking-electrically
                   (current-message))
          (sit-for 2)
          (message nil))
        (delete-region (point-min) (point-max))
        (insert (bongo-seek-status-string (window-width)))))))

;; This function was based on the function `calculator' from
;; calculator.el, which is copyrighted by the FSF.
(defun bongo-seek ()
  "Interactively seek in the current Bongo track."
  (interactive)
  (setq bongo-seek-buffer (get-buffer-create "*Bongo Seek*"))
  (if bongo-seek-electric-mode
      (unwind-protect
          (save-window-excursion
            (require 'electric)
            (message nil)
            (let ((echo-keystrokes 0)
                  (garbage-collection-messages nil)
                  (bongo-seeking-electrically t))
              (set-window-buffer (minibuffer-window) bongo-seek-buffer)
              (select-window (minibuffer-window))
              (let ((old-local-map (current-local-map))
                    (old-global-map (current-global-map)))
                (use-local-map nil)
                (use-global-map bongo-seek-mode-map)
                (setq major-mode 'bongo-seek-mode)
                (unwind-protect
                    (progn
                      (bongo-seek-redisplay)
                      (run-hooks 'bongo-seek-mode-hook)
                      (catch 'bongo-seek-done
                        (Electric-command-loop
                         'bongo-seek-done
                         ;; Avoid `noprompt' due to
                         ;; a bug in electric.el.
                         '(lambda () 'noprompt)
                         nil
                         (lambda (x y) (bongo-seek-redisplay)))))
                  (use-local-map old-local-map)
                  (use-global-map old-global-map)))))
        (when bongo-seek-buffer
          (kill-buffer bongo-seek-buffer)
          (setq bongo-seek-buffer nil)))
    (cond
     ((null (get-buffer-window bongo-seek-buffer))
      (let ((window-min-height 2)
            (split-window-keep-point nil))
        (select-window
         (split-window-vertically
          (if (and (fboundp 'face-attr-construct)
                   (plist-get (face-attr-construct 'modeline) :box))
              -3 -2)))
        (switch-to-buffer bongo-seek-buffer)))
     ((not (eq (current-buffer) bongo-seek-buffer))
      (select-window (get-buffer-window bongo-seek-buffer))))
    (bongo-seek-mode)
    (setq buffer-read-only t)
    (bongo-seek-redisplay)))


;;;; Inserting

(defun bongo-insert-line (&rest properties)
  "Insert a new line with PROPERTIES before the current line.
Externalize as many fields of the new line as possible and redisplay it.
Point is left immediately after the new line."
  ;; XXX: Should we really bind this here?
  (let ((line-move-ignore-invisible t))
    (goto-char (bongo-point-at-bol)))
  (let ((inhibit-read-only t))
    (insert-before-markers (apply 'propertize "\n" properties)))
  (forward-line -1)
  (bongo-externalize-fields)
  (if (bongo-empty-header-line-p)
      (bongo-delete-line)
    (bongo-redisplay-line)
    (forward-line)
    (unless (or (bongo-header-line-p)
                (catch 'return
                  (>= (bongo-line-indentation)
                      (bongo-line-indentation
                       (or (bongo-point-at-next-object-line)
                           (throw 'return t))))))
      (bongo-insert-header))))

(defun bongo-insert-header (&optional fields)
  "Insert a new header line with internal fields FIELDS.
FIELDS defaults to the external fields of the current line."
  (bongo-insert-line 'bongo-header t 'bongo-fields
                     (or fields (bongo-line-external-fields))))

(defcustom bongo-insert-whole-directory-trees 'ask
  "Whether to insert directory trees recursively.
This controls how `\\[bongo-insert-file]' inserts directories.
If nil, only insert files immediately contained in the top directory.
If `ask', prompt the user every time.
If any other value, insert the whole directory tree."
  :type '(choice (const :tag "No" nil)
                 (const :tag "Ask" ask)
                 (other :tag "Yes" t))
  :group 'bongo)

(defun bongo-insert-file (file-name)
  "Insert FILE-NAME into the current Bongo buffer.
If FILE-NAME is the name of a directory, insert its contents;
  if `bongo-insert-whole-directory-trees' is `ask', prompt the
  user interactively about whether to insert subdirectories.
If FILE-NAME is the name of a playlist file, insert its contents.
Otherwise, just insert a file track corresponding to FILE-NAME.

Interactively, expand wildcards and insert all matching files
unless `find-file-wildcards' is set to nil."
  ;; It would be good if this function could insert URIs,
  ;; but then `read-file-name' could not be used to do the
  ;; completion since it interprets double slash as special
  ;; syntax for discarding whatever comes before.
  (interactive
   (list (let ((file-string
                (read-file-name
                 (if (memq bongo-insert-whole-directory-trees '(nil ask))
                     "Insert file or directory: "
                   "Insert file or directory tree: ")
                 default-directory nil nil
                 (when (eq major-mode 'dired-mode)
                   (dired-get-filename t)))))
           (cond ((string-match "\\`/:" file-string)
                  (expand-file-name (substring file-string 2)))
                 ((null find-file-wildcards)
                  (expand-file-name file-string))
                 (t
                  (or (file-expand-wildcards file-string t)
                      (and (file-exists-p file-string)
                           (expand-file-name file-string))))))))
  (cond ((null file-name)
         (error "No matching files found"))
        ((consp file-name)
         (if (null (cdr file-name))
             (bongo-insert-file (car file-name))
           (with-bongo-buffer
             (let ((beginning (point)))
               (mapc 'bongo-insert-file file-name)
               (bongo-maybe-join-inserted-tracks beginning (point))))))
        ((file-directory-p file-name)
         (if (if (eq bongo-insert-whole-directory-trees 'ask)
                 (y-or-n-p (format "Insert whole directory tree (`%s')? "
                                   file-name))
               bongo-insert-whole-directory-trees)
             (bongo-insert-directory-tree file-name)
           (bongo-insert-directory file-name)))
        ((bongo-playlist-file-p file-name)
         (bongo-insert-playlist-contents file-name))
        (t
         (with-bongo-buffer
           (bongo-insert-line 'bongo-file-name file-name))
         (when (and (called-interactively-p 'interactive) (not (bongo-buffer-p)))
           (message "Inserted track: %s"
                    (bongo-format-infoset
                     (bongo-infoset-from-file-name file-name)))))))

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
          (insert (propertize " " 'display
                              (find-image (list (list
                                                 :type cover-file-type
                                                 :scale 16
                                                 :max-width bongo-album-cover-size
                                                 :max-height bongo-album-cover-size
                                                 :file cover-file-name)))))
          (insert "\n"))))))

(defun bongo-maybe-join-inserted-tracks (beg end)
  "Maybe run `bongo-join' repeatedly from BEG to END.
Only do it if `bongo-join-inserted-tracks' is non-nil."
  (when bongo-join-inserted-tracks
    (unless (markerp end)
      (setq end (move-marker (make-marker) end)))
    (goto-char beg)
    (bongo-ignore-movement-errors
      (bongo-snap-to-object-line)
      (while (< (point) end)
        (bongo-join 'skip)))))

(defun bongo-insert-directory (directory-name)
  "Insert a new track line for each playable file in DIRECTORY-NAME.
Only insert files that can be played by some backend, as determined
by the matchers returned by the function `bongo-backend-matchers'.

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
  (when (null (bongo-backend-matchers))
    (error "No backends are enabled; customize `bongo-enabled-backends'"))
  (with-bongo-buffer
    (when (not (file-directory-p directory-name))
      (error "File is not a directory: %s" directory-name))
    (when bongo-insert-album-covers
      (bongo-maybe-insert-album-cover directory-name))
    (let ((file-names (directory-files directory-name t "\\`[^.]")))
      (when (null file-names)
        (error "Directory contains no playable files"))
      (let ((beginning (point)))
        (dolist (file-name file-names)
          (when (bongo-backend-for-file file-name)
            (bongo-insert-file file-name)))
        (bongo-maybe-join-inserted-tracks beginning (point)))
      (when (and (called-interactively-p 'interactive) (not (bongo-buffer-p)))
        (message "Inserted %d files." (length file-names))))))

(defvar bongo-insert-directory-tree-total-file-count nil
  "The total number of files to be inserted.
This variable is bound by `bongo-insert-directory-tree'.")

(defvar bongo-insert-directory-tree-current-file-count nil
  "The number of files inserted so far.
This variable is bound by `bongo-insert-directory-tree'
and modified by `bongo-insert-directory-tree-1'.")

(defun bongo-insert-directory-tree-1 (directory-name)
  "Insert a new track line for each playable file below DIRECTORY-NAME.
This is a helper function for `bongo-insert-directory-tree'."
  (when bongo-insert-album-covers
    (bongo-maybe-insert-album-cover directory-name))
  (let ((file-names (directory-files directory-name t "\\`[^.]")))
    (let ((bongo-inside-insert-directory-tree t))
      (dolist (file-name file-names)
        (if (file-directory-p file-name)
            (bongo-insert-directory-tree-1 file-name)
          (when (bongo-backend-for-file file-name)
            (bongo-insert-file file-name))
          (unless (zerop bongo-insert-directory-tree-total-file-count)
            (when (zerop (% bongo-insert-directory-tree-current-file-count 10))
              (message "Inserting directory tree...%d%%"
                       (/ (* 100 bongo-insert-directory-tree-current-file-count)
                          bongo-insert-directory-tree-total-file-count))))
          (setq bongo-insert-directory-tree-current-file-count
                (+ 1 bongo-insert-directory-tree-current-file-count)))))))

(defun bongo-insert-directory-tree (directory-name)
  "Insert a new track line for each playable file below DIRECTORY-NAME.
Recursively descend each subdirectory of DIRECTORY-NAME.
Join inserted tracks afterwards if `bongo-join-inserted-tracks' is non-nil.
Only insert files that can be played by some backend, as determined by
  the matchers returned by the function `bongo-backend-matchers'.
If `bongo-insert-album-covers' is non-nil, then for each directory
  that contains a file whose name is in `bongo-album-cover-file-names',
  insert the image in that file before the directory contents."
  (interactive (list (expand-file-name
                      (read-directory-name
                       "Insert directory tree: "
                       default-directory nil t
                       (when (eq major-mode 'dired-mode)
                         (when (file-directory-p (dired-get-filename))
                           (dired-get-filename t)))))))
  (when (null (bongo-backend-matchers))
    (error "No backends are enabled; customize `bongo-enabled-backends'"))
  (when (not (file-directory-p directory-name))
    (error "File is not a directory: %s" directory-name))
  (message "Inserting directory tree...")
  (with-bongo-buffer
    (let ((beginning (point))
          (bongo-insert-directory-tree-current-file-count 0)
          (bongo-insert-directory-tree-total-file-count
           (with-temp-buffer
             (insert directory-name)
             (call-process-region
              (point-min) (point-max) "sh" t t nil
              "-c" "xargs -0i find {} -type f -o -type l | wc -l")
             (string-to-number (buffer-string)))))
      (bongo-insert-directory-tree-1 directory-name)
      (bongo-maybe-join-inserted-tracks beginning (point))))
  (message "Inserting directory tree...done"))

(defun bongo-get-x-selection (&optional type)
  "Return the value of the X Windows selection TYPE, if it exists.
If no value exists for the given selection type, return nil.
TYPE defaults to `PRIMARY'.  Use `CLIPBOARD' on Microsoft Windows."
  (when (and (fboundp 'x-selection-exists-p)
             (x-selection-exists-p type))
    (if (eq window-system 'w32)
        (w32-get-clipboard-data)
      (x-get-selection type))))

(defun bongo-get-x-selection-uri ()
  "Return the URI in the X Windows selection, if any.
See `bongo-get-x-selection'."
  (let ((value (or (let ((primary (bongo-get-x-selection)))
                     (and (bongo-uri-p primary) primary))
                   (let ((clipboard (bongo-get-x-selection 'CLIPBOARD)))
                     (and (bongo-uri-p clipboard) clipboard)))))
    (prog1 value
      (when value
        ;; The string may have `read-only' properties on it,
        ;; and other properties may cause problems as well.
        (set-text-properties 0 (length value) nil value)))))

(defun bongo-insert-uri (uri &optional title)
  "Insert a new track line corresponding to URI.
Optional argument TITLE specifies a custom title for the URI."
  (interactive
   (let* ((default-uri (bongo-get-x-selection-uri))
          (uri (read-string (concat "Insert URI"
                                    (when default-uri
                                      (format " (default `%s')"
                                              default-uri))
                                    ": ")
                            nil nil default-uri))
          (title (read-string (format "Title (default `%s'): " uri)
                              nil nil uri)))
     (list uri title)))
  (with-bongo-buffer
    (apply 'bongo-insert-line 'bongo-file-name uri
           (when (and title (not (equal title "")))
             (list 'bongo-uri-title title))))
  (when (and (called-interactively-p 'interactive) (not (bongo-buffer-p)))
    (message "Inserted URI: %s"
             (bongo-format-infoset
              (bongo-infoset-from-file-name uri)))))

(defun bongo-insert-m3u-playlist-contents (file-name)
  "Insert the contents of M3U playlist FILE-NAME."
  (interactive "fInsert contents of M3U playlist file: ")
  (when bongo-insert-album-covers
    (bongo-maybe-insert-album-cover (file-name-directory file-name)))
  (let ((beginning (with-bongo-buffer (point))))
    (with-temp-buffer
      (let ((coding-system-for-read
             (if (string-equal (file-name-extension file-name) "m3u8")
                 'utf-8
               coding-system-for-read)))
        (let* ((absolute-file-name (car (insert-file-contents file-name)))
               (default-directory (file-name-directory absolute-file-name)))
          (goto-char (point-min))
          (while (not (eobp))
            (unless (char-equal ?# (char-after (point)))
              (bongo-insert-file
               (expand-file-name
                (buffer-substring (point) (point-at-eol)))))
            (forward-line 1)))))
    (with-bongo-buffer
      (bongo-maybe-join-inserted-tracks beginning (point)))))

(defun bongo-insert-pls-playlist-contents (file-name)
  "Insert the contents of PLS playlist FILE-NAME."
  (interactive "fInsert contents of PLS playlist file: ")
  (when bongo-insert-album-covers
    (bongo-maybe-insert-album-cover (file-name-directory file-name)))
  (let ((beginning (with-bongo-buffer (point))))
    (with-temp-buffer
      (let* ((absolute-file-name (car (insert-file-contents file-name)))
             (default-directory (file-name-directory absolute-file-name)))
        (goto-char (point-min))
        (when (not (looking-at "^\\[playlist\\]$"))
          (error "File does not appear to be a PLS playlist"))
        (forward-line 1)
        (let ((i 1))
          (catch 'bongo-done
            (while t
              (if (null (re-search-forward
                         (format "^file%d=\\(.*\\)$" i) nil t))
                  (throw 'bongo-done nil)
                (let ((entry-file-name (match-string 1))
                      (entry-title
                       (and (re-search-forward
                             (format "^title%d=\\(.*\\)" i) nil t)
                            (not (string-equal "" (match-string 1)))
                            (match-string 1))))
                  (if (bongo-uri-p entry-file-name)
                      (bongo-insert-uri entry-file-name entry-title)
                    (bongo-insert-file
                     (expand-file-name entry-file-name))))
                (setq i (+ i 1))))))))
    (with-bongo-buffer
      (bongo-maybe-join-inserted-tracks beginning (point)))))

(defvar bongo-playlist-file-name-extensions
  '("pls" "playlist" "m3u" "m3u8")
  "List of file name extensions of playlist files.")

(defun bongo-playlist-file-p (file-name)
  "Return non-nil if FILE-NAME appears to be a playlist file.
Currently, only PLS and M3U playlists are supported."
  (let ((extension (file-name-extension file-name))
        (case-fold-search t))
    (and extension
         (string-match (regexp-opt bongo-playlist-file-name-extensions)
                       extension))))

(defun bongo-insert-playlist-contents (file-name)
  "Insert the contents of playlist FILE-NAME.
If the first line in the file is `[playlist]', then it is
  assumed to be a PLS playlist.
Otherwise, it is assumed to be an M3U playlist."
  (interactive "fInsert contents of playlist file: ")
  (if (with-temp-buffer
        (insert-file-contents file-name nil 0 (length "[playlist]\r\n"))
        (goto-char (point-min))
        (looking-at "^\\[playlist\\]$"))
      (bongo-insert-pls-playlist-contents file-name)
    (bongo-insert-m3u-playlist-contents file-name)))

(defun bongo-insert-action (action)
  "Insert a new action track line corresponding to ACTION."
  (interactive "xInsert action: ")
  (with-bongo-buffer
    (bongo-insert-line 'bongo-action action)))

(defvar bongo-insertion-command-alist
  '(("Action" . bongo-insert-action)
    ("CD tracks" . bongo-insert-cd)
    ("Directory" . bongo-insert-directory)
    ("Directory tree" . bongo-insert-directory-tree)
    ("File" . bongo-insert-file)
    ("Playlist contents" . bongo-insert-playlist-contents)
    ("URI" . bongo-insert-uri))
  "Alist of insertion commands for `bongo-insert-special'.")

(defun bongo-insert-special ()
  "Prompt for something to insert into the current Bongo buffer.
See `bongo-insertion-command-alist'."
  (interactive)
  (let* ((completion-ignore-case t)
         (type (completing-read "Type of thing to insert: "
                                bongo-insertion-command-alist nil t))
         (command (cdr (assoc type bongo-insertion-command-alist))))
    (when command
      (call-interactively command))))


;;;; Drag-and-drop support

(defun bongo-enable-dnd-support ()
  "Install the Bongo drag-and-drop handler for the current buffer."
  (interactive)
  (set (make-local-variable 'dnd-protocol-alist)
       '(("" . bongo-dnd-insert-uri)))
  (when (called-interactively-p 'interactive)
    (message "Bongo drag-and-drop support enabled")))

(defun bongo-disable-dnd-support ()
  "Remove the Bongo drag-and-drop handler for the current buffer."
  (interactive)
  (kill-local-variable 'dnd-protocol-alist)
  (when (called-interactively-p 'interactive)
    (message "Bongo drag-and-drop support disabled")))

(defcustom bongo-dnd-support t
  "Whether to enable drag-and-drop support in Bongo buffers.
Setting this variable normally affects only new Bongo buffers,
  but setting it through Custom also affects existing buffers.
To manually enable or disable Bongo drag-and-drop support, use
  `bongo-enable-dnd-support' and `bongo-disable-dnd-support'."
  :type 'boolean
  :initialize 'custom-initialize-default
  :set (lambda (name value)
         (dolist (buffer (if custom-local-buffer
                             (list (current-buffer))
                           (buffer-list)))
           (when (bongo-buffer-p buffer)
             (with-current-buffer buffer
               (if value
                   (bongo-enable-dnd-support)
                 (bongo-disable-dnd-support)))))
         (set-default name value))
  :group 'bongo)

(defcustom bongo-dnd-destination 'before-point
  "Where to insert items dragged and dropped into Bongo buffers.
If `before-point' or `after-point', insert dropped items before or after
  the line at point (or, if `mouse-yank-at-point' is nil, at the position
  of the mouse pointer).
If `end-of-buffer' or anything else, append to the end of the buffer."
  :type '(choice (const :tag "Insert before line at point (or mouse)"
                        before-point)
                 (const :tag "Insert after line at point (or mouse)"
                        after-point)
                 (other :tag "Append to end of buffer"
                        end-of-buffer))
  :group 'bongo)

(defun bongo-dnd-insert-uri (uri &optional action)
  "Insert URI at the current drag and drop destination.
If URI names a local file, insert it as a local file name.
If URI is not actually a URI, do nothing.
ACTION is ignored."
  (when (bongo-uri-p uri)
    (let* ((local-file-uri (dnd-get-local-file-uri uri))
           (local-file-name
            (or (when local-file-uri
                  ;; Due to a bug, `dnd-get-local-file-name'
                  ;; always returns nil without MUST-EXIST.
                  (dnd-get-local-file-name local-file-uri 'must-exist))
                (dnd-get-local-file-name uri 'must-exist))))
      (goto-char (cl-case bongo-dnd-destination
                   (before-point (bongo-point-before-line))
                   (after-point (bongo-point-after-line))
                   (otherwise (point-max))))
      (if (not local-file-name)
          (bongo-insert-uri uri)
        (bongo-insert-file local-file-name)
        (when (eq bongo-dnd-destination 'after-point)
          (bongo-previous-object-line))))))


;;;; Collapsing and expanding

(defun bongo-collapse (&optional skip)
  "Collapse the section below the header line at point.
If point is not on a header line, collapse the section at point.
If there is no section at point, do nothing.

If SKIP is nil, leave point at the header line.
If SKIP is non-nil, leave point at the first object line
  after the section.
If called interactively, SKIP is always non-nil."
  (interactive "p")
  (with-silent-modifications
    (when line-move-ignore-invisible
      (bongo-skip-invisible))
    (bongo-snap-to-object-line)
    (unless (bongo-header-line-p)
      (when (bongo-line-indented-p)
        (bongo-backward-up-section)))
    (if (not (bongo-header-line-p))
        (when skip
          (or (bongo-next-object 'no-error)
              (goto-char (bongo-point-after-object))))
      (let ((line-move-ignore-invisible nil)
            (inhibit-read-only t))
        (bongo-line-set-property 'bongo-collapsed t)
        (bongo-redisplay-line)
        (save-excursion
          (forward-line 1)
          ;; There is a bug in Emacs that causes invisible text
          ;; with a `display' property to become visible, but
          ;; only if it is right next to visible text.
          ;; Therefore, we have to make sure that the invisible
          ;; block of text starts with a character that does not
          ;; have a `display' property.  This character is later
          ;; removed by `bongo-expand'.
          (unless (get-text-property (point) 'bongo-invisibility-padding)
            (insert (propertize " " 'invisible t
                                'bongo-invisibility-padding t))))
        (let ((end (bongo-point-after-object)))
          (forward-line 1)
          (put-text-property (point) end 'invisible t)
          (if (not skip)
              (forward-line -1)
            (goto-char end)
            (bongo-snap-to-object-line 'no-error)))))))

(defun bongo-expand (&optional skip)
  "Expand the section below the header line at point.
If point is not on a header line, expand the section at point.
If there is no section at point, do nothing.

If SKIP is nil, leave point at the header line.
If SKIP is non-nil, leave point at the first object line
  after the section.
If called interactively, SKIP is always non-nil."
  (interactive "p")
  (with-silent-modifications
    (when line-move-ignore-invisible
      (bongo-skip-invisible))
    (bongo-snap-to-object-line)
    (unless (bongo-header-line-p)
      (when (bongo-line-indented-p)
        (bongo-backward-up-section)))
    (if (not (bongo-header-line-p))
        (when skip
          (or (bongo-next-object 'no-error)
              (goto-char (bongo-point-after-object))))
      (let ((header-line-position (point))
            (inhibit-read-only t)
            (line-move-ignore-invisible nil))
        (bongo-line-remove-property 'bongo-collapsed)
        (bongo-redisplay-line)
        (put-text-property (bongo-point-after-line)
                           (bongo-point-after-object)
                           'invisible nil)
        (save-excursion
          ;; See the comment in `bongo-collapse'.
          (forward-line 1)
          (when (get-text-property (point) 'bongo-invisibility-padding)
            (delete-char 1)))
        (let ((indentation (bongo-line-indentation)))
          (bongo-ignore-movement-errors
            (bongo-next-object-line)
            (while (> (bongo-line-indentation) indentation)
              (if (not (bongo-collapsed-header-line-p))
                  (bongo-next-object-line)
                (bongo-collapse 'skip)
                (bongo-snap-to-object-line 'no-error)))))
        (when (not skip)
          (goto-char header-line-position))))))

(defun bongo-toggle-collapsed ()
  "Collapse or expand the section at point.
If point is on a header line, operate on the section below point.
Otherwise, if point is in a section, operate on the section around point.
If point is neither on a header line nor in a section, signal an error."
  (interactive)
  (when line-move-ignore-invisible
    (bongo-skip-invisible))
  (condition-case nil
      (when (not (bongo-header-line-p))
        (bongo-backward-up-section))
    (error (error "No section here")))
  (if (bongo-collapsed-header-line-p)
      (bongo-expand)
    (bongo-collapse)))


;;;; Joining and splitting

(defun bongo-join-region (beg end &optional fields)
  "Join all tracks between BEG and END by externalizing FIELDS.
If FIELDS is nil, externalize as many fields as possible.
If there are no externalizable fields, signal an error.
This function creates a new header if necessary."
  (interactive "r")
  (let ((line-move-ignore-invisible nil))
    (when (null fields)
      (setq fields (bongo-potential-external-fields-in-region beg end)))
    (when (null fields)
      (error "Cannot join region: no common fields"))
    (save-excursion
      (setq end (move-marker (make-marker) end))
      (goto-char beg)
      (beginning-of-line)
      (let ((indent (length fields)))
        (bongo-ignore-movement-errors
          (while (< (point) end)
            (when (< (bongo-line-indentation) indent)
              (bongo-line-set-external-fields fields))
            (bongo-next-object-line))))
      (move-marker end nil)
      (goto-char beg)
      (bongo-insert-header))))

(defun bongo-join (&optional skip)
  "Join the tracks around point or in the region.
If the region is active, delegate to `bongo-join-region'.
Otherwise, join the tracks around point by externalizing as many
fields as possible.  (See `bongo-potential-external-fields-at-point'.)

If SKIP is nil, leave point at the newly created header line.
If SKIP is non-nil, leave point at the first object line after
  the newly created section.
If no tracks could be joined and SKIP is nil, signal an error.
When called interactively, SKIP is always non-nil."
  (interactive "p")
  (if (bongo-region-active-p)
      (bongo-join-region (region-beginning) (region-end))
    (when line-move-ignore-invisible
      (bongo-skip-invisible))
    (let* ((line-move-ignore-invisible nil)
           (fields (bongo-potential-external-fields-at-point)))
      (if (null fields)
          (if skip
              (progn (bongo-snap-to-object-line)
                     (or (bongo-next-object-line 'no-error)
                         (bongo-forward-expression)))
            (error "No common fields at point"))
        (when (bongo-action-track-line-p)
          (condition-case nil
              (bongo-next-object-line)
            (bongo-no-next-object
             (bongo-previous-object-line))))
        (let (;; Avoid the name `values', as the debugger
              ;; will interfere with that.
              (field-values (bongo-line-field-values fields))
              (before (bongo-point-before-line))
              (after (bongo-point-after-line)))
          (save-excursion
            (while (and (bongo-previous-object-line 'no-error)
                        (or (bongo-action-track-line-p)
                            (equal field-values
                                   (bongo-line-field-values fields))))
              (setq before (bongo-point-before-line))))
          (save-excursion
            (while (and (bongo-next-object-line 'no-error)
                        (or (bongo-action-track-line-p)
                            (equal field-values
                                   (bongo-line-field-values fields))))
              (setq after (bongo-point-after-line))))
          (setq after (move-marker (make-marker) after))
          (bongo-join-region before after fields)
          (when skip
            (goto-char after))
          (move-marker after nil)
          (bongo-snap-to-object-line 'no-error))))))

(defun bongo-split (&optional skip)
  "Split the section below the header line at point.
If point is not on a header line, split the section at point.

If SKIP is nil, leave point at the first object in the section.
If SKIP is non-nil, leave point at the first object after the section.
If there is no section at point and SKIP is nil, signal an error.
When called interactively, SKIP is always non-nil."
  (interactive "p")
  (when (not (bongo-object-line-p))
    (or (bongo-previous-object-line 'no-error)
        (error "No section or track here")))
  (when (and (bongo-track-line-p)
             (bongo-line-indented-p))
    (bongo-backward-up-section))
  (if (bongo-track-line-p)
      (if skip
          (or (bongo-next-object-line 'no-error)
              (bongo-forward-expression))
        (error "No section here"))
    (when (bongo-collapsed-header-line-p)
      (bongo-expand))
    (when line-move-ignore-invisible
      (bongo-skip-invisible))
    (let ((line-move-ignore-invisible nil))
      (let ((fields (bongo-line-internal-fields))
            (end (move-marker (make-marker) (bongo-point-after-object))))
        (bongo-delete-line)
        (let ((start (point)))
          (while (< (point) end)
            (let* ((previous (point))
                   (old-external
                    (bongo-line-external-fields))
                   (new-external
                    (bongo-set-difference old-external fields)))
              (goto-char (or (bongo-point-at-next-object) end))
              (bongo-line-set-external-fields new-external previous)))
          (move-marker end nil)
          (when (not skip)
            (goto-char start))
          (bongo-snap-to-object-line 'no-error))))))


;;;; Displaying

(defvar bongo-facify-below-existing-faces nil
  "When non-nil, existing faces take priority over new faces.
When nil, new faces take priority over any existing faces.
This variable controls the behavior of `bongo-facify-in-object'.")

(defun bongo-facify-in-object (beg end object &rest new-faces)
  "Add NEW-FACES to the `face' property between BEG and END in OBJECT.
For each character between BEG and END in OBJECT, if the value
  of the `face' property is a list, append NEW-FACES to the old
  value and make that the new value; if the value is a symbol,
  treat it as if it were a singleton list.
Return OBJECT, which may be a string or a buffer."
  (prog1 object
    (let ((index beg))
      (while (and index (< index end))
        (let* ((next-index (next-single-property-change index 'face object))
               (segment-end (min (or next-index end) end))
               (old-face-property (get-text-property index 'face object))
               (old-faces (if (listp old-face-property)
                              old-face-property
                            (list old-face-property)))
               (faces (if bongo-facify-below-existing-faces
                          (append old-faces new-faces)
                        (append new-faces old-faces))))
          (put-text-property index segment-end 'face faces object)
          (setq index next-index))))))

(defun bongo-facify-string (string &rest new-faces)
  "Add NEW-FACES to the `face' property of STRING.
This function calls `bongo-facify-in-object'."
  (apply 'bongo-facify-in-object 0 (length string) string new-faces))

(defalias 'bongo-facify 'bongo-facify-string)

(defun bongo-facify-copy (string &rest new-faces)
  "Make a copy of STRING and add NEW-FACES to the `face' property.
Return the newly-made copy, which has NEW-FACES on it."
  (apply 'bongo-facify (copy-sequence string) new-faces))

(defun bongo-facify-region (beg end &rest new-faces)
  "Add NEW-FACES to the `face' property of text between BEG and END.
This function calls `bongo-facify-in-object' on the current buffer."
  (apply 'bongo-facify-in-object beg end (current-buffer) new-faces))

(defun bongo-facify-current-line (&rest new-faces)
  "Add NEW-FACES to the `face' property of the current line.
This function calls `bongo-facify-region' on the current line,
including the terminating newline character."
  (apply 'bongo-facify-region
         (bongo-point-before-line)
         (bongo-point-after-line)
         new-faces))

(defun bongo-pop-up-context-menu (event)
  "Pop up a context menu at position EVENT."
  (interactive "@e")
  (let* ((line-move-ignore-invisible nil)
         (posn (event-end event))
         (region/marking (or (bongo-region-active-p) bongo-marking))
         (n-tracks-in-region
          (if (not (bongo-region-active-p)) 0
            (bongo-count-lines-satisfying
             'bongo-track-line-p (region-beginning) (region-end))))
         (region-menu
          (when (>= n-tracks-in-region 1)
            `(["----" bongo-region-tracks-separator]
              ("Tracks in Region"
               ,@(when (bongo-library-buffer-p)
                   `([,(format "Enqueue and Play %d Track%s"
                               n-tracks-in-region
                               (if (= n-tracks-in-region 1) "" "s"))
                      bongo-play]
                     [,(format "Enqueue %d Track%s"
                               n-tracks-in-region
                               (if (= n-tracks-in-region 1) "" "s"))
                      bongo-insert-enqueue]
                     [,(format "Enqueue %d Track%s at End"
                               n-tracks-in-region
                               (if (= n-tracks-in-region 1) "" "s"))
                      bongo-append-enqueue]
                     ["----" bongo-region-tracks-separator-1]))
               [,(format "Copy %d Track%s" n-tracks-in-region
                         (if (= n-tracks-in-region 1) "" "s"))
                bongo-copy-forward]
               [,(format "Cut %d Track%s" n-tracks-in-region
                         (if (= n-tracks-in-region 1) "" "s"))
                bongo-kill]))))
         (n-tracks-in-killed-marking
          (let ((result 0))
            (dolist (marker bongo-killed-marking result)
              (when (marker-position (car marker))
                (setq result (+ result 1))))))
         (n-marked-tracks
          (let ((result 0))
            (dolist (marker bongo-marking result)
              (when (marker-position (car marker))
                (setq result (+ result 1))))))
         (marking-menu
          (cond ((>= n-marked-tracks 1)
                 `(["----" bongo-marking-tracks-separator]
                   ("Marked Tracks"
                    ,@(when (bongo-library-buffer-p)
                        `([,(format "Enqueue and Play %d Track%s"
                                    n-marked-tracks
                                    (if (= n-marked-tracks 1) "" "s"))
                           ,(if (bongo-region-active-p)
                                'bongo-play-marked
                              'bongo-play)]
                          [,(format "Enqueue %d Track%s"
                                    n-marked-tracks
                                    (if (= n-marked-tracks 1) "" "s"))
                           ,(if (bongo-region-active-p)
                                'bongo-insert-enqueue-marked
                              'bongo-insert-enqueue)]
                          [,(format "Enqueue %d Track%s at End"
                                    n-marked-tracks
                                    (if (= n-marked-tracks 1) "" "s"))
                           ,(if (bongo-region-active-p)
                                'bongo-append-enqueue-marked
                              'bongo-append-enqueue)]
                          ["----" bongo-marking-tracks-separator-1]))
                    [,(format "Copy %d Track%s" n-marked-tracks
                              (if (= n-marked-tracks 1) "" "s"))
                     ,(if (bongo-region-active-p)
                          'bongo-copy-marked
                        'bongo-copy-forward)]
                    [,(format "Cut %d Track%s" n-marked-tracks
                              (if (= n-marked-tracks 1) "" "s"))
                     ,(if (bongo-region-active-p)
                          'bongo-kill-marked
                        'bongo-kill)]
                    ["----" bongo-marking-tracks-separator-2]
                    ["Unmark All Tracks" bongo-kill-marking]
                    ,@(when (and (>= n-tracks-in-killed-marking 1)
                                 (not (equal (reverse bongo-marking)
                                             bongo-killed-marking)))
                        `([,(format "Restore Earlier Marking of %d Track%s"
                                    n-tracks-in-killed-marking
                                    (if (= n-tracks-in-killed-marking 1)
                                        "" "s"))
                           bongo-yank-marking])))))
                ((>= n-tracks-in-killed-marking 1)
                 `(["----" bongo-marking-tracks-separator]
                   [,(format "Remark %d Track%s"
                               n-tracks-in-killed-marking
                               (if (= n-tracks-in-killed-marking 1) "" "s"))
                      bongo-toggle-marking]))))
         (playback-menu-content
          (when (bongo-playing-p)
            `(["Pause/Resume" bongo-pause/resume
               :style toggle
               :selected (bongo-paused-p)
               :help "\
Temporarily stop playback (this does not kill the backend player)."]
              ["Stop Playback" bongo-stop
               :help "\
Permanently stop playback (this kills the backend player)"]
              ["Play Track Again" bongo-replay-current
               :help "\
Play the current track from the beginning."]
              ["Seek in Track..." bongo-seek
               :help "\
Fast-forward or rewind the track."]
              ["----" bongo-playback-separator-1]
              ["Play Next Track" bongo-play-next
               :active (with-bongo-playlist-buffer
                         (bongo-point-at-next-track-line
                          (bongo-point-at-current-track-line)))]
              ["Play Previous Track" bongo-play-next
               :active (with-bongo-playlist-buffer
                         (bongo-point-at-previous-track-line
                          (bongo-point-at-current-track-line)))]
              ["Play Random Track" bongo-play-random
               :active (with-bongo-playlist-buffer
                         (bongo-track-lines-exist-p))]))))
    (with-current-buffer (window-buffer (posn-window posn))
      (save-excursion
        (goto-char (posn-point posn))
        (popup-menu
         (if (and (bongo-object-line-p)
                  (get-text-property (point) 'follow-link))
             (if (bongo-currently-playing-track-line-p)
                 `("Playing Bongo Track"
                   ,@playback-menu-content
                   ["----" bongo-separator-1]
                   ["Copy Track"
                    ,(if region/marking
                         'bongo-copy-line
                       'bongo-copy-forward)]
                   ["Cut Track"
                    ,(if region/marking
                         'bongo-kill-line
                       'bongo-kill)]
                   ,@region-menu
                   ,@marking-menu)
               `(,(cond ((bongo-local-file-track-line-p)
                         "Bongo File Track")
                        ((bongo-uri-track-line-p)
                         "Bongo URI Track")
                        ((bongo-action-track-line-p)
                         "Bongo Action Track")
                        ((bongo-header-line-p)
                         "Bongo Section"))
                 [,(if (bongo-library-buffer-p)
                       (if (bongo-action-track-line-p)
                           "Enqueue and Perform Action"
                         "Enqueue and Play")
                     (cond ((bongo-action-track-line-p)
                            "Perform Action")
                           ((bongo-header-line-p)
                            "Play Contents")
                           (t
                            "Play")))
                  ,(if (bongo-header-line-p)
                       (if region/marking
                           'bongo-play-lines
                         'bongo-play)
                     'bongo-dwim)]
                 ,@(when (bongo-library-buffer-p)
                     `(["Enqueue"
                        ,(if region/marking
                             'bongo-insert-enqueue-line
                           'bongo-insert-enqueue)]
                       ["Enqueue at End"
                        ,(if region/marking
                             'bongo-append-enqueue-line
                           'bongo-append-enqueue)]))
                 ,@(when (and (bongo-track-line-p)
                              (bongo-playlist-buffer-p)
                              bongo-mark-played-tracks)
                     `(["Played" bongo-mark-line-as-played]))
                 ["----" bongo-separator-1]
                 ["Copy"
                  ,(if region/marking
                       'bongo-copy-line
                     'bongo-copy-forward)]
                 ["Cut"
                  ,(if region/marking
                       'bongo-kill-line
                     'bongo-kill)]
                 ,@(when (bongo-track-line-p)
                     `(["Marked" ,(if (bongo-marked-track-line-p)
                                      'bongo-unmark-forward
                                    'bongo-mark-forward)
                        :style toggle
                        :selected (bongo-marked-track-line-p)]))
                 ,@(when (bongo-header-line-p)
                     `(["Collapsed" bongo-dwim
                        :style toggle
                        :selected (bongo-collapsed-header-line-p)]))
                 ["----" bongo-separator-2]
                 ,@(when (bongo-track-line-p)
                     `([,(cond ((bongo-local-file-track-line-p)
                                "Rename File...")
                               ((bongo-uri-track-line-p)
                                "Change URI or Title...")
                               ((bongo-action-track-line-p)
                                "Edit Action...")
                               (t
                                "Rename Track..."))
                        bongo-rename-line]))
                 ,@(when (save-excursion
                           (while (bongo-header-line-p)
                             (bongo-down-section))
                           (bongo-local-file-track-line-p))
                     `(["Open Dired" bongo-dired-line]))
                 ,@region-menu
                 ,@marking-menu
                 ,@(when playback-menu-content
                     `(["----" bongo-playback-separator-1]
                       ["----" bongo-playback-separator-2]
                       ("Current Playback" ,@playback-menu-content)))))
           `("Bongo"
             ,@playback-menu-content
             ,@region-menu
             ,@marking-menu))
         event)))))

(defun bongo-current-column (&optional point window)
  (let ((posn (posn-at-point point window)))
    (and posn (car (posn-col-row posn)))))

(defcustom bongo-display-inline-playback-progress nil
  "Whether to display playback progress inline in the playlist buffer.
This is done using the face `bongo-elapsed-track-part'.
Enabling this may considerably slow down interactive seeking."
  :type 'boolean
  :group 'bongo)

(defun bongo-redisplay-line (&optional point)
  "Redisplay the line at POINT, preserving semantic text properties."
  (with-silent-modifications
    (save-excursion
      (bongo-goto-point point)
      (let* ((inhibit-read-only t)
             (line-move-ignore-invisible nil)
             (invisible (bongo-line-get-property 'invisible)))
        (let ((properties (bongo-line-get-semantic-properties)))
          (bongo-clear-line)
          (bongo-line-set-properties properties))
        (insert (funcall bongo-track-mark-function))
        (dotimes (dummy (bongo-line-indentation))
          (insert bongo-indentation-string))
        (let ((icon-string (bongo-line-icon-string)))
          (when icon-string
            (insert icon-string " ")))
        (let* ((bongo-infoset-formatting-target
                (current-buffer))
               (bongo-infoset-formatting-target-line
                (bongo-point-before-line))
               (bongo-infoset
                (bongo-line-infoset))
               (bongo-internal-infoset
                (bongo-filter-alist (bongo-line-internal-fields)
                                    bongo-infoset))
               (header (bongo-header-line-p))
               (content
                (propertize (if header
                                (let ((bongo-collapsed
                                       (bongo-collapsed-header-line-p)))
                                  (bongo-format-string
                                   bongo-section-header-line-format))
                              (bongo-format-string bongo-track-line-format))
                            'follow-link t 'mouse-face 'highlight)))
          (when (not header)
            (cond ((bongo-currently-playing-track-line-p)
                   (bongo-facify content 'bongo-currently-playing-track))
                  ((bongo-played-track-line-p)
                   (bongo-facify content 'bongo-played-track)))
            (cond ((bongo-marked-track-line-p)
                   (bongo-facify content 'bongo-marked-track))))
          (insert content))
        (when (bongo-marked-track-line-p)
          (let ((bongo-facify-below-existing-faces t))
            (bongo-facify-current-line 'bongo-marked-track-line)))
        (when (and bongo-display-inline-playback-progress
                   (bongo-currently-playing-track-line-p)
                   (bongo-elapsed-time)
                   (bongo-total-time))
          (let ((windows (get-buffer-window-list (current-buffer))))
            (let (smallest-window)
              (dolist (window windows)
                (when (and (posn-at-point point window)
                           (or (null smallest-window)
                               (< (window-width window)
                                  (window-width smallest-window))))
                  (setq smallest-window window)))
              (when smallest-window
                (let ((point (point)))
                  (save-window-excursion
                    (select-window smallest-window)
                    (goto-char point)
                    (let* ((middle (floor (* (window-width)
                                             (/ (bongo-elapsed-time)
                                                (bongo-total-time))))))
                      (goto-char (point-at-bol))
                      (while (let ((column (bongo-current-column)))
                               (and column (< column middle)))
                        (if (eolp)
                            (insert " ")
                          (forward-char 1)))
                      (bongo-facify-region (point-at-bol) (point)
                                           'bongo-elapsed-track-part))))))))
        (when invisible
          (put-text-property (bongo-point-before-line)
                             (bongo-point-after-line)
                             'invisible t))))))

(defun bongo-redisplay-region (beg end)
  "Redisplay the Bongo objects in the region between BEG and END."
  (interactive "r")
  (let ((target-string (if (and (= beg (point-min))
                                (= end (point-max)))
                           "buffer" "region"))
        (line-move-ignore-invisible nil)
        (end-marker (move-marker (make-marker) end)))
    (save-excursion
      (when (called-interactively-p 'interactive)
        (message "Rendering %s..." target-string))
      (goto-char beg)
      (bongo-ignore-movement-errors
        (bongo-snap-to-object-line)
        (while (< (point) end-marker)
          (when (called-interactively-p 'interactive)
            (message "Rendering %s...%d%%" target-string
                     (/ (* 100 (point)) (point-max))))
          (bongo-redisplay-line)
          (bongo-next-object-line)))
      (when (called-interactively-p 'interactive)
        (message "Rendering %s...done" target-string)))))

(defun bongo-redisplay ()
  "Redisplay the current Bongo buffer.
If the region is active, redisplay just the objects in the region."
  (interactive)
  (if (bongo-region-active-p)
      (bongo-redisplay-region (region-beginning) (region-end))
    (bongo-redisplay-region (point-min) (point-max))))

(defun bongo-recenter ()
  "Move point to the currently playing track and recenter.
If no track is currently playing, just call `recenter'."
  (interactive)
  (let ((original-window (selected-window))
        (window (get-buffer-window (bongo-playlist-buffer) t)))
    (when window
      (select-window window)
      (bongo-goto-point (or (bongo-point-at-current-track-line)
                            (bongo-point-at-queued-track-line)))
      (recenter)
      (select-window original-window))))

(defvar bongo-time-regexp
  (eval-when-compile
    (rx (and string-start
             (optional
              (and (optional
                    ;; Hours.
                    (and (submatch (one-or-more digit)) ":"))
                   ;; Minutes.
                   (and (submatch (one-or-more digit))) ":"))
             ;; Seconds.
             (submatch (one-or-more digit)
                       (optional (and "." (one-or-more digit))))
             string-end)))
  "Regular expression matching a [[H:]M:]S[.F] time string.
There are three submatches: hours, minutes, and seconds.")

(defun bongo-parse-time (time)
  "Return the total number of seconds of TIME, or nil.
If TIME is a string of the form [[H:]M:]S[.F], where H, M, S and F
  may each be any number of digits, return 3600H + 60M + S.F.
If TIME is any other string, return nil."
  (when (string-match bongo-time-regexp time)
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
  being arbitrarily long.
If N is nil, just return nil."
  (when n
    (setq n (floor n))
    (let ((hours (/ n 3600))
          (minutes (% (/ n 60) 60))
          (seconds (% n 60)))
      (let ((result (format "%02d:%02d" minutes seconds)))
        (unless (zerop hours)
          (setq result (format "%d:%s" hours result)))
        result))))

;;;###autoload
(defun bongo-show (&optional insert-flag)
  "Display what Bongo is playing in the minibuffer.
If INSERT-FLAG (prefix argument if interactive) is non-nil,
  insert the description at point.
Return the description string."
  (interactive "P")
  (let* ((player (with-bongo-playlist-buffer
                   (or bongo-player
                       (error "No currently playing track"))))
         (elapsed-time (and player (bongo-player-elapsed-time player)))
         (total-time (and player (bongo-player-total-time player)))
         (description (bongo-format-infoset
                       (bongo-player-infoset player)))
         (string (if (not (and elapsed-time total-time))
                     description
                   (format "%s [%s/%s]" description
                           (bongo-format-seconds elapsed-time)
                           (bongo-format-seconds total-time)))))
    (prog1 string
      (if insert-flag
          (insert string)
        (message "%s" string)))))


;;;; Killing and yanking commands

(defun bongo-kill-line (&optional point)
  "In Bongo, kill the section, track, or line of text at POINT.
If the line at POINT is a header line, kill the whole section.
If the line at POINT is a track line, kill the track.
Otherwise, just kill the line as `kill-line' would."
  (interactive)
  (save-excursion
    (bongo-goto-point point)
    (let ((inhibit-read-only t))
      (cond ((bongo-track-line-p)
             (when (bongo-line-marker)
               (move-marker (bongo-line-marker) nil))
             (when (bongo-current-track-line-p)
               (bongo-unset-current-track-position))
             (when (bongo-queued-track-line-p)
               ;; Use a text property to communicate with
               ;; `bongo-clean-up-after-insertion'.
               (bongo-line-set-property 'bongo-queued-track-flag t)
               (bongo-unset-queued-track-position)
               (bongo-line-remove-property 'bongo-queued-track-flag))
             (kill-region (bongo-point-before-line)
                          (bongo-point-after-line))
             (when bongo-sprinkle-mode
               (bongo-sprinkle-until-saturated)))
            ((bongo-header-line-p)
             (save-excursion
               (beginning-of-line)
               (when line-move-ignore-invisible
                 (bongo-skip-invisible))
               (let ((line-move-ignore-invisible nil))
                 (kill-region (point) (bongo-point-after-object)))))
            (t
             (kill-line)))
      (while (and (bongo-previous-object-line 'no-error)
                  (bongo-empty-section-p))
        (delete-region (bongo-point-before-line)
                       (bongo-point-after-line))))))

(defun bongo-kill-region (&optional beg end)
  "In Bongo, kill all lines in the region between BEG and END.
If the region ends inside a section, kill that whole section."
  (interactive "r")
  (setq end (move-marker (make-marker) end))
  (save-excursion
    (goto-char beg)
    (while (progn (bongo-kill-line)
                  (< (point) end))
      (append-next-kill)))
  (move-marker end nil))

(defun bongo-kill-marked ()
  "In Bongo, kill all marked track lines and kill the marking."
  (interactive)
  (let ((marking (reverse bongo-marking)))
    (bongo-kill-marking)
    (while (and marking (null (marker-position (caar marking))))
      (setq marking (cdr marking)))
    (when marking
      (let ((line-move-ignore-invisible nil))
        (bongo-kill-line (caar marking))
        (dolist (reference-counted-marker (cdr marking))
          (when (marker-position (car reference-counted-marker))
            (append-next-kill)
            (bongo-kill-line (car reference-counted-marker))))))))

(defun bongo-kill (&optional n)
  "In Bongo, kill N objects, or the region, or the marked tracks.
If N is non-nil, kill the next N tracks or sections.
Otherwise, if the region is active, kill the region.
Otherwise, if there are any marked track lines, kill those.
Otherwise, just kill the track or section at point."
  (interactive "P")
  (cond ((not (null n))
         (dotimes (dummy (prefix-numeric-value n))
           (bongo-kill-line)))
        ((bongo-region-active-p)
         (bongo-kill-region (region-beginning) (region-end)))
        (bongo-marking
         (bongo-kill-marked))
        (t
         (bongo-kill-line))))

(defun bongo-copy-line (&optional point)
  "In Bongo, copy the track, section, or line of text at POINT.
If the line at POINT line is a track line, copy the track.
If the line at POINT is a header line, copy the whole section.
Otherwise, just copy the line of text at POINT.
Return the character position of the end of the copied text."
  (interactive)
  (let ((end (if (bongo-object-line-p point)
                 (bongo-point-after-object point)
               (bongo-point-after-line point))))
    (prog1 end
      ;; `filter-buffer-substring-functions' has been deprecated as of Emacs 24.4
      ;; in favor of `filter-buffer-substring-function', however we are still using
      ;; it since `filter-buffer-substring-function' is not available on versions
      ;; below Emacs 24.4 (we support Emacs v24.1 onwards)
      (let ((filter-buffer-substring-functions
             (cons (lambda (string)
                     (prog1 (setq string (copy-sequence string))
                       (remove-text-properties
                        0 (length string)
                        ;; When modifying this list, consider also
                        ;; modifying the one in `bongo-enqueue-text'.
                        (list 'invisible nil
                              'bongo-marker nil
                              'bongo-reference-counted-marker nil)
                        string)))
                   filter-buffer-substring-functions)))
        (copy-region-as-kill (bongo-point-before-line point) end)))))

(defun bongo-copy-line-forward (&optional n)
  "In Bongo, copy the next N tracks or sections or lines of text."
  (interactive "p")
  (or n (setq n 1))
  (if (< n 0)
      (progn (when (eq this-command 'bongo-copy-line-forward)
               (setq this-command 'bongo-copy-line-backward))
             (bongo-copy-line-backward (- n)))
    (when (> n 0)
      (when (eq last-command 'bongo-copy-line-forward)
        (append-next-kill))
      (goto-char (bongo-copy-line))
      (dotimes (dummy (- n 1))
        (append-next-kill)
        (goto-char (bongo-copy-line))))))

(defun bongo-previous-object-or-line ()
  "Move to the previous object or to the previous line of text.
If the previous line is an object line, move to the previous object;
otherwise, just move to the previous line of text."
  (when (bongo-first-line-p)
    (signal 'bongo-no-previous-object nil))
  (if (save-excursion
        (forward-line -1)
        (bongo-object-line-p))
      (bongo-previous-object)
    (forward-line -1)))

(defun bongo-copy-line-backward (&optional n)
  "In Bongo, copy the previous N tracks or sections or lines of text."
  (interactive "p")
  (or n (setq n 1))
  (if (< n 0)
      (progn (when (eq this-command 'bongo-copy-line-backward)
               (setq this-command 'bongo-copy-line-forward))
             (bongo-copy-line-forward (- n)))
    (when (> n 0)
      (bongo-previous-object-or-line)
      (when (eq last-command 'bongo-copy-line-backward)
        (append-next-kill))
      (bongo-copy-line)
      (dotimes (dummy (- n 1))
        (append-next-kill)
        (bongo-previous-object-or-line)
        (bongo-copy-line)))))

(defalias 'bongo-copy-region 'kill-ring-save)

(defun bongo-copy-marked ()
  "In Bongo, copy all marked track lines and kill the marking."
  (interactive)
  (let ((marking (reverse bongo-marking)))
    (bongo-kill-marking)
    (while (and marking (null (marker-position (caar marking))))
      (setq marking (cdr marking)))
    (when marking
      (let ((line-move-ignore-invisible nil))
        (bongo-copy-line (caar marking))
        (dolist (reference-counted-marker (cdr marking))
          (when (marker-position (car reference-counted-marker))
            (append-next-kill)
            (bongo-copy-line (car reference-counted-marker))))))))

(defun bongo-copy-forward (&optional n)
  "In Bongo, copy N objects, or the region, or the marked tracks.
If N is non-nil, copy the next N tracks or sections.
Otherwise, if the region is active, copy the region.
Otherwise, if there are any marked tracks, copy those.
Otherwise, just copy the track or section at point.
Leave point after the copied text."
  (interactive "P")
  (cond (n
         (bongo-copy-line-forward (prefix-numeric-value n)))
        ((bongo-region-active-p)
         (bongo-copy-region (region-beginning) (region-end)))
        (bongo-marking
         (bongo-copy-marked))
        (t
         (when (eq this-command 'bongo-copy-forward)
           (setq this-command 'bongo-copy-line-forward))
         (bongo-copy-line-forward))))

(defun bongo-copy-backward (&optional n)
  "In Bongo, copy N objects, or the region, or the marked tracks.
If N is non-nil, copy the previous N tracks or sections.
Otherwise, if the region is active, copy the region.
Otherwise, if there are any marked tracks, copy those.
Otherwise, just copy the track or section at point.
Leave point before the copied text."
  (interactive "P")
  (if n
      (bongo-copy-forward (- (prefix-numeric-value n)))
    (when (eq this-command 'bongo-copy-backward)
      (setq this-command 'bongo-copy-forward))
    (bongo-copy-forward)))

(defun bongo-clean-up-after-insertion (beg end)
  (let ((end (move-marker (make-marker) end))
        (line-move-ignore-invisible nil))
    (save-excursion
      (goto-char beg)
      (bongo-ignore-movement-errors
        (bongo-snap-to-object-line)
        (while (< (point) end)
          (let ((player (bongo-line-get-property 'bongo-player)))
            (when player
              (if (and (eq player bongo-player)
                       (null (bongo-point-at-current-track-line)))
                  (bongo-set-current-track-position (point-at-bol))
                (bongo-line-remove-property 'bongo-player))))
          (let ((marker (bongo-line-reference-counted-marker)))
            (when marker
              (if (marker-position (car marker))
                  (bongo-line-remove-property
                   'bongo-reference-counted-marker)
                (move-marker (car marker) (bongo-point-at-bol))
                (let ((marked-flag (memq marker bongo-marking))
                      (marked-property-flag
                       (bongo-line-get-property 'bongo-marked)))
                  (when (not (eq marked-flag marked-property-flag))
                    (bongo-line-set-property 'bongo-marked marked-flag)
                    (bongo-redisplay-line))))))
          (unless (bongo-point-at-queued-track-line)
            ;; See `bongo-kill-line' for the origin of these
            ;; temporary-text-property messages.
            (when (bongo-line-get-property 'bongo-queued-track-flag)
              (bongo-line-remove-property 'bongo-queued-track-flag)
              (bongo-set-queued-track-position)))
          (let ((new-external-fields nil))
            ;; Internalize all null fields to avoid creating
            ;; a section joined on null fields.
            (dolist (field (bongo-line-external-fields))
              (when (not (null (bongo-line-field-value field)))
                (push field new-external-fields)))
            ;; Externalize all null fields that are external
            ;; on the next line to avoid splitting a section
            ;; for the purpose of internalizing null fields.
            ;;
            ;; (If we wanted, we could externalize all the
            ;; proposed external null fields instead; that
            ;; would also effect reasonable behavior.)
            (catch 'abort
              (dolist (field (bongo-line-external-fields
                              (or (bongo-point-at-next-object-line)
                                  (throw 'abort nil))))
                (when (null (bongo-line-field-value field))
                  (push field new-external-fields))))
            (bongo-line-set-external-fields new-external-fields))
          (bongo-next-object-line)))
      (goto-char beg)
      (when (and (bongo-snap-to-object-line 'no-error)
                 (< (point) end))
        (bongo-insert-header))
      (goto-char end)
      (when (bongo-snap-to-object-line 'no-error)
        (bongo-insert-header))
      (move-marker end nil))))

(defun bongo-yank (&optional argument)
  "In Bongo, reinsert the last sequence of killed lines.
See `yank' for the meaning of ARGUMENT."
  (interactive "P")
  (let ((inhibit-read-only t))
    (beginning-of-line)
    (when line-move-ignore-invisible
      (bongo-skip-invisible))
    ;; This trick causes the yanked text to be inserted
    ;; before all markers at the beginning of the line.
    (insert-before-markers " ")
    (backward-char 1)
    (let ((yank-excluded-properties
           (remq 'invisible yank-excluded-properties)))
      (yank argument))
    (save-excursion
      (goto-char (region-end))
      (delete-char 1))
    (bongo-clean-up-after-insertion (region-beginning) (region-end))))

;; XXX: This definitely does not work properly.
(defun bongo-yank-pop (&optional argument)
  "In Bongo, replace the just-yanked lines with different ones.
See `yank-pop' for the meaning of ARGUMENT."
  (interactive "P")
  (let ((inhibit-read-only t))
    (yank-pop argument)
    (bongo-externalize-fields)))

;; XXX: This probably does not work properly.
(defun bongo-undo (&optional argument)
  "In Bongo, undo some previous changes.
See `undo' for the meaning of ARGUMENT."
  (interactive "P")
  (let ((inhibit-read-only t))
    (undo argument)))


;;;; Enqueuing commands

(defvar bongo-inhibit-recenter-after-enqueue nil
  "If non-nil, do not recenter the playlist after enqueueing.")

(defun bongo-enqueue-text (mode text &optional maybe-display-playlist)
  "Insert TEXT into the Bongo playlist.
If MODE is `insert', insert TEXT just below the current track.
If MODE is `append', append TEXT to the end of the playlist.

If MAYBE-DISPLAY-PLAYLIST is non-nil, maybe display the playlist;
see `bongo-display-playlist-after-enqueue'."
  (let ((insertion-point
         (with-current-buffer (bongo-playlist-buffer)
           (save-excursion
             (goto-char
              (cl-ecase mode
                (insert (or (and (bongo-point-at-current-track-line)
                                 (bongo-point-after-line
                                  (bongo-point-at-current-track-line)))
                            (bongo-point-at-first-track-line)
                            (point-max)))
                (append (point-max))))
             (prog1 (point)
               (remove-text-properties
                0 (length text)
                ;; When modifying this list, consider also
                ;; modifying the one in `bongo-copy-line'.
                (list 'invisible nil
                      'bongo-collapsed nil
                      'bongo-marked nil
                      'bongo-reference-counted-marker nil)
                text)
               (let ((beg (point))
                     (inhibit-read-only t))
                 (insert text)
                 (bongo-redisplay-region beg (point))
                 (bongo-clean-up-after-insertion beg (point))))))))
    (prog1 insertion-point
      (when (and (bongo-library-buffer-p)
                 (or (get-buffer-window (bongo-playlist-buffer))
                     (and maybe-display-playlist
                          bongo-display-playlist-after-enqueue)))
        (let ((original-window (selected-window)))
          (select-window (display-buffer (bongo-playlist-buffer)))
          (unless bongo-inhibit-recenter-after-enqueue
            (goto-char insertion-point)
            (recenter))
          (select-window original-window))))))

;;; These functions operate on all tracks in a given region.

(defun bongo-enqueue-region (mode beg end &optional maybe-display-playlist)
  "Insert the tracks between BEG and END into the Bongo playlist.
If MODE is `insert', insert the tracks just below the current track.
If MODE is `append', append the tracks to the end of the playlist.

If MAYBE-DISPLAY-PLAYLIST is non-nil, maybe display the playlist;
see `bongo-display-playlist-after-enqueue'."
  (let* ((original-buffer (current-buffer))
         (text (with-temp-buffer
                 ;; This is complicated because we want to remove the
                 ;; `bongo-external-fields' property from all tracks
                 ;; and headers before enqueuing them, but we want to
                 ;; keep the property for everything *within* sections.
                 (let ((temp-buffer (current-buffer))
                       (line-move-ignore-invisible nil))
                   (set-buffer original-buffer)
                   (goto-char (bongo-point-before-line beg))
                   (while (< (point) end)
                     (let ((point-before-first-line (point))
                           (point-after-first-line (bongo-point-after-line)))
                       (when (and (prog1 (bongo-object-line-p)
                                    (bongo-forward-expression))
                                  (> (point) end))
                         (goto-char end))
                       (when (> (point) end)
                         (goto-char (bongo-point-at-bol-forward end)))
                       (let ((first-line
                              (buffer-substring point-before-first-line
                                                point-after-first-line))
                             (other-lines
                              (buffer-substring point-after-first-line
                                                (point))))
                         (remove-text-properties
                          0 (length first-line)
                          (list 'bongo-external-fields nil)
                          first-line)
                         (with-current-buffer temp-buffer
                           (insert first-line)
                           (insert other-lines)))))
                   (with-current-buffer temp-buffer
                     (buffer-string))))))
    (bongo-enqueue-text mode text maybe-display-playlist)))

(defun bongo-insert-enqueue-region (beg end &optional maybe-display-playlist)
  "Insert the region between BEG and END just below the current track.

If MAYBE-DISPLAY-PLAYLIST is non-nil, maybe display the playlist;
see `bongo-display-playlist-after-enqueue'."
  (interactive (list (region-beginning) (region-end)
                     'maybe-display-playlist))
  (bongo-enqueue-region 'insert beg end maybe-display-playlist))

(defun bongo-append-enqueue-region (beg end &optional maybe-display-playlist)
  "Append the region between BEG and END to the end of the playlist.

If MAYBE-DISPLAY-PLAYLIST is non-nil, maybe display the playlist;
see `bongo-display-playlist-after-enqueue'."
  (interactive (list (region-beginning) (region-end)
                     'maybe-display-playlist))
  (bongo-enqueue-region 'append beg end maybe-display-playlist))

;;; These functions operate on an explicitly specified line.

(defun bongo-enqueue-line (mode &optional point maybe-display-playlist)
  "Insert the track or section at POINT into the Bongo playlist.
If MODE is `insert', insert just below the current track.
If MODE is `append', append to the end of the playlist.
Return the playlist position of the newly-inserted text.

If MAYBE-DISPLAY-PLAYLIST is non-nil, maybe display the playlist;
see `bongo-display-playlist-after-enqueue'."
  (save-excursion
    (bongo-goto-point point)
    (bongo-enqueue-lines mode 1 maybe-display-playlist)))

(defun bongo-insert-enqueue-line (&optional point maybe-display-playlist)
  "Insert the track or section at POINT just below the current track.
Return the playlist position of the newly-inserted text.

If MAYBE-DISPLAY-PLAYLIST is non-nil, maybe display the playlist;
see `bongo-display-playlist-after-enqueue'."
  (bongo-enqueue-line 'insert point maybe-display-playlist))

(defun bongo-append-enqueue-line (&optional point maybe-display-playlist)
  "Append the track or section at POINT to the Bongo playlist buffer.
Return the playlist position of the newly-inserted text.

If MAYBE-DISPLAY-PLAYLIST is non-nil, maybe display the playlist;
see `bongo-display-playlist-after-enqueue'."
  (bongo-enqueue-line 'append point maybe-display-playlist))

;;; These functions operate on a given number of tracks or
;;; sections right after point.

(defun bongo-enqueue-lines (mode &optional n maybe-display-playlist)
  "Insert the next N tracks or sections into the Bongo playlist.
Leave point immediately after the enqueued tracks and sections.
If MODE is `insert', insert just below the current track.
If MODE is `append', append to the end of the playlist.
Return the playlist position of the newly-inserted text.

If MAYBE-DISPLAY-PLAYLIST is non-nil, maybe display the playlist;
see `bongo-display-playlist-after-enqueue'."
  (or n (setq n 1))
  (if (eq major-mode 'dired-mode)
      (bongo-dired-enqueue-lines mode n maybe-display-playlist)
    (when line-move-ignore-invisible
      (bongo-skip-invisible))
    (let ((line-move-ignore-invisible nil))
      (let ((beg (point))
            (end (dotimes (dummy (abs n) (point))
                   (bongo-goto-point
                    (if (> n 0)
                        (bongo-point-after-object)
                      (bongo-point-before-previous-object))))))
        (bongo-enqueue-region mode (min beg end) (max beg end)
                              maybe-display-playlist)))))

(defun bongo-insert-enqueue-lines (&optional n maybe-display-playlist)
  "Insert the next N tracks or sections just below the current track.
Leave point immediately after the enqueued tracks and sections.
Return the playlist position of the newly-inserted text.

If MAYBE-DISPLAY-PLAYLIST is non-nil, maybe display the playlist;
see `bongo-display-playlist-after-enqueue'."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     'maybe-display-playlist))
  (bongo-enqueue-lines 'insert n maybe-display-playlist))

(defun bongo-append-enqueue-lines (&optional n maybe-display-playlist)
  "Append the next N tracks or sections to the Bongo playlist buffer.
Leave point immediately after the enqueued tracks and sections.
Return the playlist position of the newly-inserted text.

If MAYBE-DISPLAY-PLAYLIST is non-nil, maybe display the playlist;
see `bongo-display-playlist-after-enqueue'."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     'maybe-display-playlist))
  (bongo-enqueue-lines 'append n maybe-display-playlist))

;;; These functions operate on the marked tracks.

(defun bongo-enqueue-marked (mode &optional maybe-display-playlist)
  "Insert the marked tracks into the playlist and kill the marking.
If MODE is `insert', insert just below the current track.
If MODE is `append', append to the end of the playlist.
Return the playlist position of the newly-inserted text.

If MAYBE-DISPLAY-PLAYLIST is non-nil, maybe display the playlist;
see `bongo-display-playlist-after-enqueue'."
  (save-excursion
    (let ((marking (reverse bongo-marking)))
      (bongo-kill-marking)
      (while (and marking (null (marker-position (caar marking))))
        (setq marking (cdr marking)))
      (when marking
        (let ((line-move-ignore-invisible nil))
          (prog1 (bongo-enqueue-line
                  mode (caar marking) maybe-display-playlist)
            (dolist (reference-counted-marker (cdr marking))
              (when (marker-position (car reference-counted-marker))
                (bongo-enqueue-line
                 mode (car reference-counted-marker))))))))))

(defun bongo-insert-enqueue-marked (&optional maybe-display-playlist)
  "Insert the marked tracks just below the current track.

If MAYBE-DISPLAY-PLAYLIST is non-nil, maybe display the playlist;
see `bongo-display-playlist-after-enqueue'."
  (interactive (list 'maybe-display-playlist))
  (bongo-enqueue-marked 'insert maybe-display-playlist))

(defun bongo-append-enqueue-marked (&optional maybe-display-playlist)
  "Insert the marked tracks just below the current track.

If MAYBE-DISPLAY-PLAYLIST is non-nil, maybe display the playlist;
see `bongo-display-playlist-after-enqueue'."
  (interactive (list 'maybe-display-playlist))
  (bongo-enqueue-marked 'append maybe-display-playlist))

;;; These functions follow the p/r/m convention.

(defun bongo-enqueue (mode &optional n maybe-display-playlist)
  "In Bongo, enqueue N objects, or the region, or the marked tracks.
Enqueuing a track or section copies it into the nearest playlist.
If MODE is `insert', insert just below the current track.
If MODE is `append', append to the end of the playlist.

This command follows the prefix/region/marking (p/r/m) convention:
 - If N is non-nil, enqueue the next N tracks or sections.
 - Otherwise, if the region is active, enqueue the region.
 - Otherwise, if there are any marked tracks, enqueue those.
 - Otherwise, just enqueue the track or section at point.

If MAYBE-DISPLAY-PLAYLIST is non-nil, maybe display the playlist;
see `bongo-display-playlist-after-enqueue'."
  (cond (n
         (bongo-enqueue-lines mode n maybe-display-playlist))
        ((bongo-region-active-p)
         (bongo-enqueue-region mode (region-beginning) (region-end)
                               maybe-display-playlist))
        (bongo-marking
         (bongo-enqueue-marked mode maybe-display-playlist))
        (t
         (bongo-enqueue-lines mode 1 maybe-display-playlist))))

(defun bongo-append-enqueue (&optional n maybe-display-playlist)
  "Append-enqueue N objects, or the region, or the marked tracks.
Append-enqueuing something copies it to the end of the nearest playlist.

This command follows the prefix/region/marking (p/r/m) convention:
 - If N is non-nil, enqueue the next N tracks or sections.
 - Otherwise, if the region is active, enqueue the region.
 - Otherwise, if there are any marked tracks, enqueue those.
 - Otherwise, just enqueue the track or section at point.

If MAYBE-DISPLAY-PLAYLIST is non-nil, maybe display the playlist;
see `bongo-display-playlist-after-enqueue'."
  (interactive (list current-prefix-arg 'maybe-display-playlist))
  (bongo-enqueue 'append (and n (prefix-numeric-value n))
                 maybe-display-playlist))

(defun bongo-insert-enqueue (&optional n maybe-display-playlist)
  "Insert-enqueue N objects, or the region, or the marked tracks.
Insert-enqueuing something copies it into the nearest playlist
just below the current track.

This command follows the prefix/region/marking (p/r/m) convention:
 - If N is non-nil, enqueue the next N tracks or sections.
 - Otherwise, if the region is active, enqueue the region.
 - Otherwise, if there are any marked tracks, enqueue those.
 - Otherwise, just enqueue the track or section at point.

If MAYBE-DISPLAY-PLAYLIST is non-nil, maybe display the playlist;
see `bongo-display-playlist-after-enqueue'."
  (interactive (list current-prefix-arg 'maybe-display-playlist))
  (bongo-enqueue 'insert (and n (prefix-numeric-value n))
                 maybe-display-playlist))


;;;; Miscellaneous commands

(defun bongo-transpose-forward ()
  "Transpose forward the section or track at point."
  (interactive)
  (bongo-snap-to-object-line)
  (let ((beg (bongo-point-before-line))
        (mid (bongo-point-at-next-object)))
    (when (null mid)
      (error 'bongo-no-next-object nil))
    (let ((end (bongo-point-after-object mid)))
      (let ((inhibit-read-only t))
        (transpose-regions beg mid mid end)))))

(defun bongo-transpose-backward ()
  "Transpose backward the section or track at point."
  (interactive)
  (bongo-snap-to-object-line)
  (save-excursion
    (bongo-previous-object)
    (bongo-transpose-forward)))

(defun bongo-delete-empty-sections ()
  "Delete all empty sections from the current Bongo buffer."
  (let ((inhibit-read-only t)
        (line-move-ignore-invisible nil))
    (save-excursion
      (goto-char (point-min))
      (bongo-ignore-movement-errors
        (while (bongo-snap-to-object-line)
          (if (not (bongo-empty-section-p))
              (bongo-next-object-line)
            (bongo-delete-line)))))))

(defun bongo-delete-played-tracks ()
  "Delete all played tracks from the Bongo playlist."
  (interactive)
  (with-bongo-playlist-buffer
    (let ((inhibit-read-only t)
          (line-move-ignore-invisible nil))
      (save-excursion
        (goto-char (point-min))
        (bongo-ignore-movement-errors
          (while (bongo-snap-to-object-line)
            (if (or (not (bongo-played-track-line-p))
                    (bongo-currently-playing-track-line-p))
                (bongo-next-object-line)
              (bongo-delete-line))))
        (bongo-delete-empty-sections)))))

(defun bongo-erase-buffer ()
  "Delete the entire contents of the current Bongo buffer.
However, if some track is currently playing, do not delete that."
  (interactive)
  (let ((inhibit-read-only t)
        (currently-playing-track
         (and (bongo-playing-p)
              (bongo-line-string
               (bongo-point-at-current-track-line)))))
    (erase-buffer)
    (when currently-playing-track
      (remove-text-properties 0 (length currently-playing-track)
                              '(bongo-external-fields nil)
                              currently-playing-track)
      (insert currently-playing-track))
    (goto-char (point-max))))

(defun bongo-flush-playlist (&optional delete-all)
  "Delete all played tracks from the Bongo playlist.
With prefix argument DELETE-ALL, clear the entire playlist."
  (interactive "P")
  (with-bongo-playlist-buffer
    (if delete-all
        (when (or (not bongo-confirm-flush-playlist)
                  (y-or-n-p "Clear the entire playlist? "))
          (bongo-erase-buffer))
      (when (or (not bongo-confirm-flush-playlist)
                (y-or-n-p "Delete all played tracks from the playlist? "))
        (bongo-delete-played-tracks)))))

(defun bongo-reset-playlist ()
  "Mark all tracks as unplayed and unset the current track marker.
However, only unset the current track marker if nothing is playing."
  (interactive)
  (with-bongo-playlist-buffer
    (unless (bongo-playing-p)
      (bongo-unset-current-track-position))
    (let ((inhibit-read-only t)
          (line-move-ignore-invisible nil))
      (save-excursion
        (goto-char (point-min))
        (while (bongo-snap-to-object-line 'no-error)
          (when (bongo-line-get-property 'bongo-played)
            (bongo-line-remove-property 'bongo-played)
            (bongo-redisplay-line))
          (forward-line 1))))))

(defun bongo-rename-line ()
  "Interactively rename the track at point.
If there is no track at point, signal an error.
This function dispatches to one of the following functions:
 - `bongo-rename-local-file-track'
 - `bongo-rename-uri-track'
 - `bongo-rename-action-track'."
  (interactive)
  (cond ((bongo-local-file-track-line-p)
         (call-interactively 'bongo-rename-local-file-track))
        ((bongo-uri-track-line-p)
         (call-interactively 'bongo-rename-uri-track))
        ((bongo-action-track-line-p)
         (call-interactively 'bongo-rename-action-track))
        (t (error "No track at point"))))

(defun bongo-rename-local-file-track (new-name &optional point)
  "Rename the file corresponding to the track at POINT to NEW-NAME.
This function uses `bongo-update-references-to-renamed-files'."
  (interactive
   (when (bongo-local-file-track-line-p)
     (list (read-from-minibuffer "Rename file to: "
                                 (bongo-line-file-name)))))
  (with-point-at-bongo-track point
    (when (not (bongo-local-file-track-line-p))
      (error "No local file track at point"))
    (let ((old-name (bongo-line-file-name)))
      (rename-file old-name new-name)
      (if (if (eq bongo-update-references-to-renamed-files 'ask)
              (y-or-n-p (concat "Search all Bongo buffers and update "
                                "references to the renamed file? "))
            bongo-update-references-to-renamed-files)
          (dolist (buffer (buffer-list))
            (when (bongo-buffer-p buffer)
              (set-buffer buffer)
              (goto-char (point-min))
              (bongo-ignore-movement-errors
                (while (bongo-snap-to-object-line)
                  (when (string-equal (bongo-line-file-name) old-name)
                    (bongo-line-set-property 'bongo-file-name new-name)
                    (bongo-redisplay-line))
                  (bongo-next-object-line)))))
        (bongo-line-set-property 'bongo-file-name new-name)
        (bongo-redisplay-line)))))

(defun bongo-rename-uri-track (new-uri &optional new-title point)
  "Retarget and optionally retitle the URI track at POINT.
NEW-URI is the new URI; NEW-TITLE, if non-nil, is the new title."
  (interactive
   (when (bongo-uri-track-line-p)
     (list (read-from-minibuffer
            "Change URI to: " (bongo-line-file-name))
           (read-from-minibuffer
            "Change URI title to: "
            (or (bongo-line-get-property 'bongo-uri-title)
                (bongo-line-get-property 'bongo-stream-name))))))
  (with-point-at-bongo-track point
    (when (not (bongo-uri-track-line-p))
      (error "No URI track at point"))
    (bongo-line-set-property 'bongo-file-name new-uri)
    (when new-title
      (if (equal new-title "")
          (bongo-line-remove-property 'bongo-uri-title)
        (bongo-line-set-property 'bongo-uri-title new-title)))
    (bongo-redisplay-line)))

(defun bongo-rename-action-track (new-action &optional point)
  "Change the action of the action track at POINT.
NEW-ACTION is the new action (a function or an expression)."
  (interactive (when (bongo-action-track-line-p)
                 (list (read-from-minibuffer "Change action to: "
                                             (prin1-to-string
                                              (bongo-line-action))
                                             nil 'read))))
  (with-point-at-bongo-track point
    (when (not (bongo-action-track-line-p))
      (error "No action track at point"))
    (bongo-line-set-property 'bongo-action new-action)
    (bongo-redisplay-line)))

(defun bongo-dired-line (&optional point)
  "Open a Dired buffer containing the track at POINT."
  (interactive)
  (save-excursion
    (bongo-goto-point point)
    (bongo-snap-to-object-line)
    (dired (file-name-directory
            (save-excursion
              (while (bongo-header-line-p)
                (bongo-down-section))
              (if (bongo-local-file-track-line-p)
                  (bongo-line-file-name)
                (error "No local file track here")))))
    (bongo-dired-library-mode 1)))


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
             `("\\.bongo$"
               . ,(if bongo-prefer-library-buffers
                      'bongo-library-mode
                    'bongo-playlist-mode)))
(add-to-list 'auto-mode-alist
             '("\\.bongo-library$" . bongo-library-mode))
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

(defun bongo-update-images (string)
  "Update paths to images in the STRING's display property.

The image's location changes whenever MELPA updates leaving bongo buffers saved
with older version with wrong image paths, this function updates the image paths
to point to (potentially) new location."
  (let* ((start (if (get-text-property 0 'display string)
                    0
                  (next-single-property-change 0 'display string)))
         (end (length string)))
    (while start
      (let* ((display (get-text-property start 'display string))
             (face-prop (get-text-property start 'face string))
             ;; Does not handle all the possible cases of faces
             ;; See https://www.gnu.org/software/emacs/manual/html_node/elisp/Special-Properties.html#Special-Properties
             ;; But Bongo seems to be using only list of faces and face name
             (face-cand (if (listp face-prop) (car face-prop) face-prop))
             (face (and (facep face-cand) face-cand)))
        (when (equal (car display) 'image)
          (plist-put (cdr display)
                     :file (expand-file-name (file-name-nondirectory (plist-get (cdr display) :file))
                                             bongo-images-directory))
          (plist-put (cdr display)
                     :background (face-background (or face 'default) nil 'default))
          (plist-put (cdr display)
                     :foreground (face-foreground (or face 'default) nil 'default))))
      (setq start (text-property-not-all (1+ start) end 'display nil string)))
    string))

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
      (bongo-delete-line)
      (let ((original-buffer-size (- end beg))
            (iteration 0))
        (while (not (eobp))
          (when (zerop (% (setq iteration (+ iteration 1)) 500))
            (message "Reading Bongo buffer...%d%%"
                     (/ (* 100.0 (- original-buffer-size
                                    (- (point-max) (point))))
                        original-buffer-size)))
          (let ((start (point)))
            (condition-case nil
                (let ((object (read (current-buffer))))
                  (delete-region start (point))
                  (if (stringp object)
                      (insert (bongo-update-images object))
                    (error "Unexpected object: %s" object)))
              (end-of-file
               (delete-region start (point-max)))))))
      (message "Reading Bongo buffer...done")
      (point-max))))

(defvar bongo-line-serializable-properties
  ;; When changing this, consider also changing
  ;; `bongo-line-semantic-properties'.
  (list 'bongo-file-name 'bongo-action 'bongo-backend
        'bongo-infoset 'bongo-uri-title 'bongo-track-length
        'bongo-stream-name 'bongo-stream-genre
        'bongo-fields 'bongo-external-fields
        'bongo-header 'bongo-collapsed)
  "List of serializable text properties used in Bongo buffers.
When a bongo Buffer is written to a file, only serializable text
properties are saved; all other text properties are discarded.")

(defun bongo-encode (beg end buffer)
  "Serialize part of BUFFER into a flat representation.
Modify region between BEG and END; return the new end of the region.

This function is used when writing Bongo buffers to files.
You probably do not want to call this function directly;
instead, use high-level functions such as `save-buffer'."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (bongo-ensure-final-newline)
      (goto-char (point-min))
      (insert (if (bongo-playlist-buffer-p)
                  bongo-playlist-magic-string
                bongo-library-magic-string) "\n")
      (while (not (eobp))
        (bongo-keep-text-properties (point-at-eol) (1+ (point-at-eol))
                                    bongo-line-serializable-properties)
        (prin1 (bongo-extract-line) (current-buffer))
        (insert "\n"))
      (point-max))))


;;;; Bongo Dired Library mode

(defun bongo-dired-enqueue-lines (mode &optional n maybe-display-playlist)
  (let ((dired-buffer (current-buffer)))
    (with-temp-bongo-library-buffer
      (bongo-insert-file (with-current-buffer dired-buffer
                           (dired-get-file-for-visit)))
      (bongo-enqueue-region mode (point-min) (point-max)
                            maybe-display-playlist))))

(defun bongo-dired-append-enqueue-lines (&optional n maybe-display-playlist)
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     'maybe-display-playlist))
  (bongo-dired-enqueue-lines 'append n maybe-display-playlist))

(defun bongo-dired-insert-enqueue-lines (&optional n maybe-display-playlist)
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     'maybe-display-playlist))
  (bongo-dired-enqueue-lines 'insert n maybe-display-playlist))

(defun bongo-dired-play-line ()
  (interactive)
  (let ((point (bongo-dired-insert-enqueue-lines)))
    (with-bongo-playlist-buffer
      (bongo-play-line point))))

(defun bongo-dired-dwim ()
  (interactive)
  (if (ignore-errors
        (bongo-backend-for-file (dired-get-file-for-visit)))
      (bongo-dired-play-line)
    (let ((dired-mode-hook
           (cons (lambda ()
                   (bongo-dired-library-mode 1))
                 (and (boundp 'dired-mode-hook)
                      (if (and (listp dired-mode-hook)
                               (not (eq (car dired-mode-hook) 'lambda)))
                          dired-mode-hook
                        (list dired-mode-hook))))))
      (dired-find-file))))

(defvar bongo-dired-library-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map "e" 'bongo-dired-append-enqueue-lines)
      (define-key map "E" 'bongo-dired-insert-enqueue-lines)
      (define-key map "h" 'bongo-switch-buffers)
      (define-key map "q" 'bongo-quit)
      (define-key map "\C-m" 'bongo-dired-dwim)
      (define-key map " " 'bongo-pause/resume)
      (define-key map "\C-c\C-a" 'bongo-replay-current)
      (define-key map "\C-c\C-e" 'bongo-perform-next-action)
      (define-key map "\C-c\C-p" 'bongo-play-previous)
      (define-key map "\C-c\C-n" 'bongo-play-next)
      (define-key map "\C-c\C-r" 'bongo-play-random)
      (define-key map "\C-c\C-s" 'bongo-start/stop))))

(define-minor-mode bongo-dired-library-mode
  "Use a Dired buffer as a Bongo library.

\\{bongo-dired-library-mode-map}"
 :lighter " Bongo Library"
 :keymap bongo-dired-library-mode-map)


;;;; Typical user entry points

(defvar bongo-mode-hook nil
  "Hook run when entering Bongo mode.
This is run for both playlist and library buffers.")

(defcustom bongo-xmms-refugee-mode nil
  "Bind the `z', `x', `c', `v' and `b' keys as XMMS does.
These keys form a little playback control panel on QWERTY keyboards.
The bindings are `previous', `start', `pause/resume', `stop', and `next'.
This causes Bongo to move the usual bindings of the affected keys to their
uppercase counterparts.  For example, `\\[volume]' becomes bound to `V'.

You should call `bongo-redefine-keys' after changing this variable.
However, setting it through Custom does this automatically."
  :type 'boolean
  :set (lambda (variable value)
         (custom-set-default variable value)
         (bongo-redefine-keys))
  ;; Avoid redefining the keys when loading Bongo.
  :initialize 'custom-initialize-default
  :group 'bongo)

(defvar bongo-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (suppress-keymap map)))
  "Keymap used in Bongo playlist and library buffers.")

(defun bongo-redefine-keys ()
  "Define the usual keys in `bongo-mode-map'."
  (interactive)
  (let ((map bongo-mode-map))
    (define-key map "\C-m" 'bongo-dwim)
    (define-key map [mouse-2] 'bongo-mouse-dwim)
    (define-key map [mouse-3] 'bongo-pop-up-context-menu)
    (define-key map "q" 'bongo-quit)
    (define-key map "Q" 'bury-buffer)
    (define-key map "h" 'bongo-switch-buffers)
    (define-key map "H" 'bongo-list-buffers)
    (define-key map "l" 'bongo-recenter)
    (define-key map "$" 'bongo-toggle-collapsed)
    (define-key map "p" 'previous-line)
    (define-key map "n" 'next-line)
    (substitute-key-definition
     'backward-paragraph 'bongo-previous-header-line map global-map)
    (substitute-key-definition
     'forward-paragraph 'bongo-next-header-line map global-map)
    (define-key map "\M-p" 'bongo-previous-header-line)
    (define-key map "\M-n" 'bongo-next-header-line)
    (if bongo-xmms-refugee-mode
        (define-key map "C" 'bongo-copy-forward)
      (define-key map "c" 'bongo-copy-forward))
    (define-key map "k" 'bongo-kill)
    (substitute-key-definition
     'kill-line 'bongo-kill-line map global-map)
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
    (define-key map "\C-c\C-m" 'bongo-play)
    (define-key map "\C-c\C-a" 'bongo-replay-current)
    (define-key map "\C-c\C-e" 'bongo-perform-next-action)
    (define-key map "\C-c\C-p" 'bongo-play-previous)
    (define-key map "\C-c\C-n" 'bongo-play-next)
    (define-key map "\C-c\C-r" 'bongo-play-random)
    (define-key map "\C-c\C-s" 'bongo-start/stop)
    (define-key map "P" 'bongo-previous)
    (define-key map "N" 'bongo-next)
    (define-key map "s" 'bongo-seek)
    (define-key map [?\C-\S-b] 'bongo-seek-backward)
    (define-key map [?\C-\S-f] 'bongo-seek-forward)
    (define-key map "b" 'bongo-seek-backward)
    (define-key map "f" 'bongo-seek-forward)
    (define-key map "B" 'bongo-seek-backward-3)
    (define-key map "F" 'bongo-seek-forward-3)
    (define-key map [(shift left)] 'bongo-seek-backward-3)
    (define-key map [(shift right)] 'bongo-seek-forward-3)
    (define-key map "\M-B" 'bongo-seek-backward-10)
    (define-key map "\M-F" 'bongo-seek-forward-10)
    (define-key map [(control shift left)] 'bongo-seek-backward-10)
    (define-key map [(control shift right)] 'bongo-seek-forward-10)
    (define-key map [(meta shift left)] 'bongo-seek-backward-10)
    (define-key map [(meta shift right)] 'bongo-seek-forward-10)
    (define-key map [?\C-\M-\S-b] 'bongo-seek-backward-60)
    (define-key map [?\C-\M-\S-f] 'bongo-seek-forward-60)
    (define-key map [(control meta shift left)] 'bongo-seek-backward-60)
    (define-key map [(control meta shift right)] 'bongo-seek-forward-60)
    (define-key map "i" 'bongo-insert-file)
    (define-key map "I" 'bongo-insert-special)
    (define-key map "g" 'bongo-play)
    (define-key map "e" 'bongo-append-enqueue)
    (define-key map "E" 'bongo-insert-enqueue)
    (define-key map "t" 'bongo-transpose-forward)
    (define-key map "T" 'bongo-transpose-backward)
    (define-key map "~" 'bongo-flush-playlist)
    (define-key map "R" 'bongo-reset-playlist)
    (define-key map "S" 'bongo-sprinkle)
    (define-key map "&"
      'bongo-universal-prefix/region/marking-object-command)
    (define-key map "\M-&"
      'bongo-universal-prefix/region/marking-track-command)
    (define-key map "m" 'bongo-mark-forward)
    (define-key map "u" 'bongo-unmark-forward)
    (define-key map "\177" 'bongo-unmark-backward)
    (substitute-key-definition
     'backward-delete-char 'bongo-unmark-backward map global-map)
    (define-key map "**" 'bongo-toggle-marking)
    (define-key map "*k" 'bongo-kill-marking)
    (define-key map "U" 'bongo-kill-marking)
    (define-key map "*y" 'bongo-yank-marking)
    (define-key map "%m" 'bongo-mark-by-formatted-infoset-regexp)
    (define-key map "%u" 'bongo-unmark-by-formatted-infoset-regexp)
    (define-key map "%a" 'bongo-mark-by-artist-name-regexp)
    (define-key map "%b" 'bongo-mark-by-album-title-regexp)
    (define-key map "%y" 'bongo-mark-by-album-year-regexp)
    (define-key map "%i" 'bongo-mark-by-track-index-regexp)
    (define-key map "%t" 'bongo-mark-by-track-title-regexp)
    (define-key map "%A" 'bongo-unmark-by-artist-name-regexp)
    (define-key map "%B" 'bongo-unmark-by-album-title-regexp)
    (define-key map "%Y" 'bongo-unmark-by-album-year-regexp)
    (define-key map "%I" 'bongo-unmark-by-track-index-regexp)
    (define-key map "%T" 'bongo-unmark-by-track-title-regexp)
    (define-key map "r" 'bongo-rename-line)
    (define-key map "d" 'bongo-dired-line)
    (if bongo-xmms-refugee-mode
        (define-key map "V" 'volume)
      (define-key map "v" 'volume))
    (define-key map [(shift up)] 'volume-raise)
    (define-key map [(control shift up)] 'volume-raise-10)
    (define-key map [(meta shift up)] 'volume-raise-10)
    (define-key map [(control meta shift up)] 'volume-raise-50)
    (define-key map [(shift down)] 'volume-lower)
    (define-key map [(control shift down)] 'volume-lower-10)
    (define-key map [(meta shift down)] 'volume-lower-10)
    (define-key map [(control meta shift down)] 'volume-lower-50)
    (when bongo-xmms-refugee-mode
      (define-key map "z" 'bongo-previous)
      (define-key map "x" 'bongo-start)
      (define-key map "c" 'bongo-pause/resume)
      (define-key map "v" 'bongo-stop)
      ;; This replaces the rewind binding, but that's okay.
      (define-key map "b" 'bongo-next))
    (when (not bongo-xmms-refugee-mode)
      ;; Delete leftover XMMS bindings, as it may be
      ;; confusing if only some of them work.
      (when (eq (lookup-key map "z") 'bongo-previous)
        (define-key map "z" nil))
      (when (eq (lookup-key map "x") 'bongo-start)
        (define-key map "x" nil))
      (when (eq (lookup-key map "b") 'bongo-next)
        (define-key map "b" nil))
      (when (eq (lookup-key map "V") 'volume)
        (define-key map "V" nil)))
    (let ((menu-map (make-sparse-keymap "Bongo")))
      (define-key menu-map [bongo-quit]
        '("Quit Bongo" . bongo-quit))
      (define-key menu-map [bongo-menu-separator-6]
        '("----" . nil))
      (define-key menu-map [bongo-customize]
        '("Customize Bongo..." . (lambda ()
                                   (interactive)
                                   (customize-group 'bongo))))
      (define-key menu-map [bongo-menu-separator-5]
        '("----" . nil))
      (define-key menu-map [bongo-flush-playlist]
        '(menu-item "Flush Played Tracks" bongo-flush-playlist
          :visible (and (bongo-playlist-buffer-p)
                        bongo-mark-played-tracks)))
      (define-key menu-map [bongo-sprinkle-mode]
        '(menu-item "Automatic Sprinkling" bongo-sprinkle-mode
          :button (:toggle . bongo-sprinkle-mode)
          :visible (bongo-playlist-buffer-p)))
      (define-key menu-map [bongo-sprinkle]
        '(menu-item "Sprinkle a Random Track" bongo-sprinkle
          :visible (bongo-playlist-buffer-p)))
      (define-key menu-map [bongo-menu-separator-4]
        '(menu-item "----" nil
          :visible (bongo-playlist-buffer-p)))
      ;; Remember that these are listed in reverse order.
      (define-key menu-map [bongo-insert-other]
        '("Insert Other..." . bongo-insert-special))
      (define-key menu-map [bongo-insert-action]
        '("Insert Action Track..." . bongo-insert-action))
      (define-key menu-map [bongo-insert-playlist-contents]
        '("Insert Contents of M3U/PLS Playlist..."
          . bongo-insert-playlist-contents))
      (define-key menu-map [bongo-insert-cd]
        '("Insert CD Tracks..." . bongo-insert-cd))
      (define-key menu-map [bongo-insert-uri]
        '("Insert URI..." . bongo-insert-uri))
      (define-key menu-map [bongo-insert-directory-tree]
        '("Insert Directory Tree..." . bongo-insert-directory-tree))
      (define-key menu-map [bongo-insert-directory]
        '("Insert Directory..." . bongo-insert-directory))
      (define-key menu-map [bongo-insert-file]
        '("Insert File or Directory..." . bongo-insert-file))
      (define-key menu-map [bongo-menu-separator-3]
        '("----" . nil))
      (define-key menu-map [bongo-play-random-track]
        '("Play Random Track" . bongo-play-random))
      (define-key menu-map [bongo-play-previous-track]
        '("Play Previous Track" . bongo-play-previous))
      (define-key menu-map [bongo-play-next-track]
        '("Play Next Track" . bongo-play-next))
      (define-key menu-map [bongo-replay-current-track]
        '("Play Current Track from Start" . bongo-replay-current))
      (define-key menu-map [bongo-menu-separator-2]
        '("----" . nil))
      (define-key menu-map [bongo-change-volume]
        '(menu-item "Change the Audio Volume..." volume
          :enable (featurep 'volume)))
      (define-key menu-map [bongo-stop]
        '(menu-item "Stop Playback" bongo-start/stop
          :enable (bongo-playing-p)
          :visible (bongo-playing-p)))
      (define-key menu-map [bongo-seek-interactively]
        '(menu-item "Seek Forward or Backward..." bongo-seek
          :enable (bongo-seeking-supported-p)
          :visible (bongo-playing-p)))
      (define-key menu-map [bongo-pause/resume]
        '(menu-item "Pause Playback" bongo-pause/resume
          :enable (bongo-pausing-supported-p)
          :button (:toggle . (bongo-paused-p))
          :visible (bongo-playing-p)))
      (define-key menu-map [bongo-start]
        '(menu-item "Start Playback" bongo-start/stop
          :visible (not (bongo-playing-p))))
      (define-key menu-map [bongo-menu-separator-1]
        '("----" . nil))
      (define-key menu-map [bongo-switch-to-library]
        '(menu-item "Switch to Library" bongo-switch-buffers
          :visible (bongo-playlist-buffer-p)))
      (define-key menu-map [bongo-switch-to-playlist]
        '(menu-item "Switch to Playlist" bongo-switch-buffers
          :visible (bongo-library-buffer-p)))
      (define-key map [menu-bar bongo]
        (cons "Bongo" menu-map)))))

(bongo-redefine-keys)

(defun bongo-confirm-player-stop ()
  "If the current buffer has a player running stop it before killing the buffer."
  (or (not bongo-player)
      (when (yes-or-no-p "This buffer has an associated player running, stop it?")
        (bongo-player-stop bongo-player)
        t)))

(defun bongo-mode ()
  "Common parent major mode for Bongo buffers.
Do not use this mode directly.  Instead, use Bongo Playlist mode (see
`bongo-playlist-mode') or Bongo Library mode (see `bongo-library-mode').

\\{bongo-mode-map}"
  (or (memq major-mode (list 'fundamental-mode
                             'bongo-mode
                             'bongo-playlist-mode
                             'bongo-library-mode))
      (yes-or-no-p (format "Really switch from %s mode to Bongo mode? "
                           mode-name))
      (keyboard-quit))
  (kill-all-local-variables)
  (set (make-local-variable 'forward-sexp-function)
       'bongo-forward-section)
  (set (make-local-variable 'line-move-ignore-invisible) t)
  (use-local-map bongo-mode-map)
  (setq buffer-read-only t)
  (setq major-mode 'bongo-mode)
  (setq mode-name "Bongo")
  (setq buffer-file-format '(bongo))
  (when bongo-default-directory
    (setq default-directory bongo-default-directory))
  (when bongo-dnd-support
    (bongo-enable-dnd-support))
  (add-hook 'kill-buffer-query-functions 'bongo-confirm-player-stop t t)
  (run-mode-hooks 'bongo-mode-hook))

(define-derived-mode bongo-library-mode bongo-mode "Library"
  "Major mode for Bongo library buffers.
Library buffers are used to insert tracks into playlist buffers.
They cannot directly play tracks, and they are not essential to Bongo.

You may create a new Bongo library buffer by simply creating an empty buffer
and running `\\[bongo-library-mode]'.  If you do not yet have any library
buffers, `\\[bongo-library]' will create one for you.  If you do not have
any Bongo buffers at all, `\\[bongo]' will create a library buffer for you
if `bongo-prefer-library-buffers' is non-nil (the default).

\\{bongo-library-mode-map}"
    :group 'bongo :syntax-table nil :abbrev-table nil)

(put 'bongo-library-mode 'mode-class 'special)

(define-derived-mode bongo-playlist-mode bongo-mode "Playlist"
  "Major mode for Bongo playlist buffers.
Playlist buffers are essential to Bongo, because you use them to play tracks.

You may create a new Bongo playlist buffer by simply creating an empty buffer
and running `\\[bongo-playlist-mode]'.  If you do not yet have any playlist
buffers, `\\[bongo-playlist]' will create one for you.  If you do not have
any Bongo buffers at all, `\\[bongo]' will create a playlist buffer for you
if `bongo-prefer-library-buffers' is nil.

\\{bongo-playlist-mode-map}"
  :group 'bongo :syntax-table nil :abbrev-table nil
  (setq mode-line-process
        '(:eval (when bongo-display-playback-mode-indicator
                  (let ((description
                         (get bongo-next-action
                              'bongo-playback-mode-indicator)))
                    (when (functionp description)
                      (setq description (funcall description)))
                    (when (null description)
                      (setq description "custom"))
                    (if (equal description "")
                        ""
                      `("[" ,description "]"))))))
  (setq bongo-stopped-track-marker (make-marker))
  (setq bongo-playing-track-marker (make-marker))
  (setq bongo-paused-track-marker (make-marker))
  (setq bongo-current-track-marker bongo-stopped-track-marker)
  (when (and window-system (>= emacs-major-version 22))
    (catch 'abort
      (setq left-fringe-width
            (* 2 (or (bongo-face-width 'fringe)
                     (throw 'abort nil))))))
  (setq bongo-queued-track-marker (make-marker))
  (setq bongo-queued-track-arrow-marker (make-marker))
  (when (boundp 'overlay-arrow-variable-list)
    (add-to-list 'overlay-arrow-variable-list
      'bongo-stopped-track-marker)
    (add-to-list 'overlay-arrow-variable-list
      'bongo-playing-track-marker)
    (add-to-list 'overlay-arrow-variable-list
      'bongo-paused-track-marker)
    (add-to-list 'overlay-arrow-variable-list
      'bongo-queued-track-arrow-marker)))

(put 'bongo-playlist-mode 'mode-class 'special)

(define-minor-mode bongo-sprinkle-mode
  "Minor mode for automatic sprinkling of Bongo playlists.
This mode can only be enabled in Bongo playlist buffers.
When it is enabled, Bongo automatically adds random tracks
to the playlist to keep it populated with unplayed tracks.

In Bongo Sprinkle mode, whenever a track is played or one or
more unplayed tracks are killed, Bongo appends enough random
tracks to the playlist to ensure that `bongo-sprinkle-amount'
unplayed tracks are present at any time.\\<bongo-mode-map>

To move unplayed tracks around without causing any sprinkling,
use the `\\[bongo-transpose-forward]' and \
`\\[bongo-transpose-backward]' commands.

To manually sprinkle the buffer, use the `\\[bongo-sprinkle]' command.
For example, `5 \\[bongo-sprinkle]' will append five random tracks,
and `\\[universal-argument] \\[universal-argument] \\[bongo-sprinkle]' \
will append 16 random tracks.

The documentation for `bongo-sprinkle' describes how Bongo
decides on the buffer from which to take the random tracks."
  :lighter " Sprinkle"
  (if (not bongo-sprinkle-mode)
      ;; Ideally, deactivating Sprinkle mode would restore
      ;; the previous state of this variable, but doing that
      ;; right is surprisingly difficult, so just screw it;
      ;; we can do that when we actually need it done.
      (kill-local-variable 'bongo-mark-played-tracks)
    (unless (bongo-playlist-buffer-p)
      (error "Bongo Sprinkle mode can only be used in playlist buffers"))
    (set (make-local-variable 'bongo-mark-played-tracks) t)
    (bongo-sprinkle-until-saturated)))

(defun bongo-buffer-p (&optional buffer)
  "Return non-nil if BUFFER is in Bongo mode.
If BUFFER is nil, test the current buffer instead.
If BUFFER is neither nil nor a buffer, return nil."
  (when (or (null buffer) (bufferp buffer))
    (with-current-buffer (or buffer (current-buffer))
      (or (eq major-mode 'bongo-playlist-mode)
          (eq major-mode 'bongo-library-mode)
          (and (eq major-mode 'dired-mode)
               bongo-dired-library-mode)))))

(defun bongo-library-buffer-p (&optional buffer)
  "Return non-nil if BUFFER is in Bongo Library mode.
If BUFFER is nil, test the current buffer instead.
If BUFFER is neither nil nor a buffer, return nil."
  (when (or (null buffer) (bufferp buffer))
    (with-current-buffer (or buffer (current-buffer))
      (or (eq 'bongo-library-mode major-mode)
          (and (eq 'dired-mode major-mode)
               bongo-dired-library-mode)))))

(defun bongo-playlist-buffer-p (&optional buffer)
  "Return non-nil if BUFFER is in Bongo Playlist mode.
If BUFFER is nil, test the current buffer instead.
If BUFFER is neither nil nor a buffer, return nil."
  (when (or (null buffer) (bufferp buffer))
    (with-current-buffer (or buffer (current-buffer))
      (eq 'bongo-playlist-mode major-mode))))

(defun bongo-embolden-quoted-substrings (string)
  "Return a copy of STRING with each quoted `SUBSTRING' emboldened."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (re-search-forward "\\(`\\)\\(.*?\\)\\('\\)" nil 'noerror)
      (replace-match (concat (match-string 1)
                             (bongo-facify (match-string 2) 'bold)
                             (match-string 3))))
    (buffer-string)))

(defvar bongo-logo
  (bongo-find-image "bongo-logo.pbm" 'bongo-comment))

(defun bongo-insert-comment-text (text)
  (let ((inhibit-read-only t))
    (insert (bongo-facify-copy text 'bongo-comment))))

(defun bongo-insert-warning-text (text)
  (let ((inhibit-read-only t))
    (insert (bongo-facify-copy text 'bongo-warning))))

(defun bongo-update-enabled-backends-list ()
  (let* ((beg (next-single-property-change
               (point-min) 'bongo-enabled-backends-list))
         (end (when beg
                (next-single-property-change
                 beg 'bongo-enabled-backends-list))))
    (when (and beg end)
      (save-excursion
        (let ((inhibit-read-only t))
          (delete-region beg end)
          (goto-char beg)
          (if (null bongo-enabled-backends)
              (bongo-insert-warning-text
               (propertize "(none)" 'bongo-enabled-backends-list t))
            (bongo-insert-comment-text
             (propertize
              (mapconcat
               (lambda (backend-name)
                 (bongo-facify-copy
                  (bongo-backend-pretty-name backend-name) 'bold))
               bongo-enabled-backends ", ")
              'bongo-enabled-backends-list t))))))))

(defun bongo-insert-enabled-backends-comment ()
  (bongo-insert-comment-text
   (format "\
  Enabled backends: %s\n"
           (propertize "dummy" 'bongo-enabled-backends-list t)))
  (bongo-update-enabled-backends-list)
  (bongo-insert-comment-text "\
  To modify this list, customize `bongo-enabled-backends'.\n\n")
  (when (fboundp 'help-xref-button)
    (let ((inhibit-read-only t))
      (save-excursion
        (search-backward "customize")
        (replace-match (bongo-facify (match-string 0) 'underline))
        (help-xref-button 0 'help-customize-variable
                          'bongo-enabled-backends))))
  (bongo-insert-comment-text "\
  Bongo is free software licensed under the GNU GPL.
  Report bugs to <bongo-devel@nongnu.org>.\n\n"))

(defun bongo-default-library-buffer ()
  (or (get-buffer bongo-default-library-buffer-name)
      (let ((buffer (get-buffer-create bongo-default-library-buffer-name)))
        (prog1 buffer
          (with-current-buffer buffer
            (bongo-library-mode)
            (when (and window-system bongo-logo)
              (let ((inhibit-read-only t))
                (insert "\n  ")
                (insert-image bongo-logo "[Bongo logo]")
                (insert "\n")))
            (when bongo-prefer-library-buffers
              (bongo-insert-comment-text "
  Welcome to Bongo, the buffer-oriented media player!\n"))
            (bongo-insert-comment-text
             (bongo-embolden-quoted-substrings "
  This is a Bongo library buffer.  It's empty now, but in a
  few moments it could hold your entire media collection ---
  or just the parts that you are currently interested in.

  To insert local media files or directories, use `i'.
  To insert the URL of a media file or stream, use `I u RET'.
  To insert other things, use `I TAB' to list possibilities.

  To enqueue tracks in the nearest playlist buffer, use `e'.
  To hop to the nearest playlist buffer, use `h'.\n\n"))
            (when bongo-prefer-library-buffers
              (bongo-insert-enabled-backends-comment)))))))

(defun bongo-default-playlist-buffer ()
  (or (get-buffer bongo-default-playlist-buffer-name)
      (let ((buffer (get-buffer-create bongo-default-playlist-buffer-name)))
        (prog1 buffer
          (with-current-buffer buffer
            (bongo-playlist-mode)
            (when (and window-system bongo-logo)
              (let ((inhibit-read-only t))
                (insert "\n  ")
                (insert-image bongo-logo "[Bongo logo]")
                (insert "\n")))
            (when (not bongo-prefer-library-buffers)
              (bongo-insert-comment-text "
  Welcome to Bongo, the buffer-oriented media player!\n"))
            (bongo-insert-comment-text
             (bongo-embolden-quoted-substrings "
  This is a Bongo playlist buffer.  It holds things that are
  about to be played, and things that have already been played.

  To start playing a track, use `RET'; to stop, use `C-c C-s'.
  To play the previous or next track, use `C-c C-p' or `C-c C-n'.
  To pause, use `SPC', and to fast-forward or rewind, use `s'.

  You can use `i' and `I' to insert things directly into playlists,
  but enqueuing (using `e') from libraries is often more convenient.
  Use `h' to hop to a library buffer (creating one if necessary).\n\n"))
            (when (not bongo-prefer-library-buffers)
              (bongo-insert-enabled-backends-comment)))))))

(defun bongo-buffer ()
  "Return an interesting Bongo buffer, creating it if necessary.
First try to find an existing Bongo buffer, using a strategy similar to the
function `bongo-library-buffer' and the function `bongo-playlist-buffer'.
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
      (if bongo-prefer-library-buffers
          (bongo-default-library-buffer)
        (bongo-default-playlist-buffer))))

(defun bongo-recent-playlist-buffer ()
  "Return the most recently selected Bongo playlist buffer, if any.
The current buffer is excluded from consideration."
  (catch 'return
    (dolist (buffer (buffer-list))
      (when (and (bongo-playlist-buffer-p buffer)
                 (not (eq buffer (current-buffer))))
        (throw 'return buffer)))))

(defun bongo-playlist-buffer ()
  "Return a Bongo playlist buffer.
If the variable `bongo-playlist-buffer' is non-nil, return that.
Otherwise, return the most recently selected Bongo playlist buffer.
If there is no buffer in Bongo Playlist mode, create one.  The name of
the new buffer will be the value of `bongo-default-playlist-buffer-name'."
  (or bongo-playlist-buffer
      (and (bongo-playlist-buffer-p)
           (current-buffer))
      (bongo-recent-playlist-buffer)
      (bongo-default-playlist-buffer)))

(defun bongo-recent-library-buffer ()
  "Return the most recently selected Bongo library buffer, if any.
The current buffer is excluded from consideration."
  (catch 'return
    (dolist (buffer (buffer-list))
      (when (and (bongo-library-buffer-p buffer)
                 (not (eq buffer (current-buffer))))
        (throw 'return buffer)))))

(defun bongo-library-buffer ()
  "Return a Bongo library buffer.
If the variable `bongo-library-buffer' is non-nil, return that.
Otherwise, return the most recently selected Bongo library buffer.
If there is no buffer in Bongo Library mode, create one.  The name of
the new buffer will be the value of `bongo-default-library-buffer-name'."
  (or bongo-library-buffer
      (and (bongo-library-buffer-p)
           (current-buffer))
      (bongo-recent-library-buffer)
      (bongo-default-library-buffer)))

;;;###autoload
(defun bongo-playlist ()
  "Switch to a Bongo playlist buffer.
See the function `bongo-playlist-buffer'."
  (interactive)
  (switch-to-buffer (bongo-playlist-buffer)))

;;;###autoload
(defun bongo-library ()
  "Switch to a Bongo library buffer.
See the function `bongo-library-buffer'."
  (interactive)
  (switch-to-buffer (bongo-library-buffer)))

(defun bongo-quit ()
  "Quit Bongo by deleting all windows and selecting a non-Bongo buffer."
  (interactive)
  (let ((first-buffer (current-buffer)))
    (while (and (bongo-buffer-p)
                (progn (bury-buffer)
                       (not (eq first-buffer (current-buffer))))))))

(defun bongo-buffers ()
  (let (result)
    (dolist (buffer (buffer-list) result)
      (when (bongo-buffer-p buffer)
        (push buffer result)))))

(defun bongo-switch-to-buffer ()
  (interactive)
  (let* ((buffer-name
          (bongo-completing-read "Switch to Bongo buffer: "
                                 (mapcar 'buffer-name (bongo-buffers))))
         (buffer (get-buffer buffer-name)))
    (cond (buffer
           (switch-to-buffer buffer))
          ((y-or-n-p (format "No buffer matching `%s', create one? "
                             buffer-name))
           (if (prog1 (y-or-n-p (format "Make `%s' a library buffer? "
                                        buffer-name))
                 (switch-to-buffer buffer-name))
               (bongo-library-mode)
             (bongo-playlist-mode))))))

;;;###autoload
(defun bongo-switch-buffers (&optional prompt)
  "In Bongo, switch from a playlist to a library, or vice versa.
With prefix argument PROMPT, prompt for the buffer to switch to."
  (interactive "P")
  (if prompt
      (bongo-switch-to-buffer)
    (let* ((buffer (if (bongo-playlist-buffer-p)
                       (bongo-library-buffer)
                     (bongo-playlist-buffer)))
           (window (get-buffer-window buffer)))
      (if window
          (select-window window)
        (switch-to-buffer buffer)))))

(defun bongo-list-buffers ()
  (interactive)
  (if (featurep 'ibuffer)
      (ibuffer nil "*Bongo Ibuffer*"
               '((predicate . (bongo-buffer-p))) nil nil
               '(("Playlists" (predicate . (bongo-playlist-buffer-p)))
                 ("Libraries" (predicate . (bongo-library-buffer-p)))))
    (switch-to-buffer (list-buffers-noselect nil (bongo-buffers)))))

;;;###autoload
(defun bongo ()
  "Switch to a Bongo buffer.
See the function `bongo-buffer'."
  (interactive)
  (unless (bongo-buffer-p)
    (switch-to-buffer (bongo-buffer))))

(custom-reevaluate-setting 'bongo-header-line-mode)
(custom-reevaluate-setting 'bongo-mode-line-indicator-mode)
(custom-reevaluate-setting 'bongo-global-lastfm-mode)

;;; Local Variables:
;;; coding: utf-8
;;; checkdoc-arguments-in-order-flag: nil
;;; checkdoc-permit-comma-termination-flag: t
;;; checkdoc-symbol-words: ("command-line")
;;; End:

(provide 'bongo)
;;; bongo.el ends here
