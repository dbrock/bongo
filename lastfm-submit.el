;;; lastfm-submit.el --- submit information to Last.fm
;; Copyright (C) 2006  Daniel Brockman <daniel@brockman.se>

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

;;; Commentary:

;; This is a thin frontend to the `lastfmsumbit' tool,
;; which is included in the `lastfmsubmitd' distribution,
;; available at the following URL:

;; <http://www.red-bean.com/~decklin/software/lastfmsubmitd/>

;; Please note that you may have to be in the `lastfm' group
;; to run the `lastfmsubmit' program, and that you may have
;; to relogin for such a group change to take effect.

;;; Code:

(defcustom lastfmsubmit-program-name
  (or (executable-find "lastfmsubmit")
      ;; Debian puts it here.
      (executable-find "/usr/lib/lastfmsubmitd/lastfmsubmit"))
  "The name of the `lastfmsubmit' executable.
Note that you may have to be in the `lastfm' group to run this program,
and that adding yourself to a group normally requires that you re-login."
  :type 'string
  :group 'multimedia
  :group 'external)

(defun lastfm-submit (artist title length &optional album mbid time)
  "Submit TITLE by ARTIST to Last.fm using the `lastfmsubmit' tool.
ARTIST is the name of the artist and TITLE is the name of the track.
LENGTH is the length of the track, either as a number of seconds
  or formatted as \"HOURS:MINUTES:SECONDS\" (HOURS may be omitted).
ALBUM is either nil or the name of the album on which the track appears.
MBID is either nil or the MusicBrainz ID of the track.
TIME is either nil or the time at which the track was played, formatted
  in UTC as \"%Y-%m-%d %H:%M:%S\" (see `format-time-string').
See also `lastfmsubmit-program-name'."
  (when (numberp length)
    (setq length (number-to-string (round length))))
  (unless (string-match "^\\(\\([0-9]+:\\)?[0-9]+:\\)?[0-9]+$" length)
    (error "Badly formed track length: %s" length))
  (with-temp-buffer
    (let ((status (apply 'call-process lastfmsubmit-program-name nil t nil
                         "--artist" artist "--title" title "--length" length
                         (append (when album (list "--album" album))
                                 (when mbid (list "--mbid" mbid))))))
      (unless (equal status 0)
        (with-output-to-temp-buffer "*lastfmsumbit*"
          (princ (buffer-string)))
        (if (integerp status)
            (error "lastfmsubmit failed with exit code %s" status)
          (error "lastfmsubmit failed: %s" status))))))

;;; Local Variables:
;;; coding: utf-8
;;; End:

(provide 'lastfm-submit)
;;; lastfm-submit.el ends here.
