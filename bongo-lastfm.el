;;; bongo-lastfm.el --- submit information to Last.fm from Bongo
;; Copyright (C) 2006  Daniel Brockman

;; Author: Daniel Brockman <daniel@brockman.se>
;; URL: http://www.brockman.se/software/bongo/bongo-lastfm.el
;; Created: May 24, 2006
;; Updated: September 19, 2006

;; This file is part of Bongo.

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

;; This file requires the `lastfm-submit' library,
;; available at the following URL:

;; <http://www.brockman.se/software/lastfm-submit.el>

;;; Code:

(require 'lastfm-submit)

(defun bongo-lastfm-cancel-timer (player)
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
    (bongo-lastfm-cancel-timer player)))

(defun bongo-lastfm-player-started-1 (player)
  (when (and (bongo-player-elapsed-time player)
             (bongo-player-total-time player)
             ;; ``Songs with a duration of less than 30
             ;; seconds should not be submitted,'' says the
             ;; Audioscrobbler website.
             (>= (bongo-player-total-time player) 30))
    (let ((timer (run-with-timer 1 1 'bongo-lastfm-tick player)))
      (bongo-player-put player 'lastfm-timer timer))))

(defun bongo-lastfm-player-started (player)
  (let ((timer (run-with-timer 5 nil 'bongo-lastfm-player-started-1 player)))
    (bongo-player-put player 'lastfm-timer timer)))

(defun bongo-lastfm-player-disrupted (player)
  (bongo-lastfm-cancel-timer player))

(define-minor-mode bongo-lastfm-mode
  "Toggle Bongo Last.fm mode in the current buffer.
Interactively, with no prefix argument, toggle the mode.
With universal prefix ARG, turn mode on.
With zero or negative ARG, turn mode off.
In Bongo Last.fm mode, song information is automatically
sumbitted to Last.fm (using `lastfm-submit')."
  :lighter " Last.fm"
  (when (not (bongo-playlist-buffer-p))
    (setq bongo-lastfm-mode nil)
    (error "Bongo Last.fm mode can only be enabled in Bongo playlists"))
  (if bongo-lastfm-mode
      (progn
        (add-hook 'bongo-player-started-functions
                  'bongo-lastfm-player-started nil 'local)
        (add-hook 'bongo-player-sought-functions
                  'bongo-lastfm-player-disrupted nil 'local)
        (add-hook 'bongo-player-stopped-functions
                  'bongo-lastfm-player-disrupted nil 'local))
    (remove-hook 'bongo-player-started-functions
                 'bongo-lastfm-player-started 'local)
    (remove-hook 'bongo-player-sought-functions
                 'bongo-lastfm-player-disrupted 'local)
    (remove-hook 'bongo-player-stopped-functions
                 'bongo-lastfm-player-disrupted 'local)))

(defun bongo-lastfm-submit (infoset length)
  "Submit song information to Last.fm using `lastfm-submit'."
  (when (null length)
    (error "Can't submit track to Last.fm: missing song length"))
  (lastfm-submit (bongo-infoset-artist-name infoset)
                 (bongo-infoset-track-title infoset)
                 (number-to-string (round length))
                 (bongo-infoset-album-title infoset))
  (message "Submitted to Last.fm: %s"
           (bongo-format-infoset infoset)))

(defun bongo-lastfm-submit-player (player)
  "Submit PLAYER's song information to Last.fm.
See `bongo-lastfm-submit'."
  (bongo-lastfm-submit (bongo-infoset-from-file-name
                        (bongo-player-file-name player))
                       (bongo-player-total-time player)))

(defun bongo-lastfm-submit-current ()
  "Sumbit the currently playing track to Last.fm."
  (interactive)
  (with-bongo-playlist-buffer
    (if (bongo-playing-p)
        (bongo-lastfm-submit-player bongo-player)
      (error "No active track"))))

;;; Local Variables:
;;; coding: utf-8
;;; time-stamp-format: "%:b %:d, %:y"
;;; time-stamp-start: ";; Updated: "
;;; time-stamp-end: "$"
;;; End:

(provide 'bongo-lastfm)
;;; bongo-lastfm.el ends here.
