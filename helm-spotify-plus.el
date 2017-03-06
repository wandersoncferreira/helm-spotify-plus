;;; helm-spotify-plus.el --- Control Spotify with Helm.

;; Copyright (C)
;; Author: Wanderson Ferreira <https://github.com/wandersoncferreira> and Luis Moneda <https://github.com/lgmoneda>
;; Package: helm-spotify-plus
;; Package-Requires: ((emacs "24.4"))
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Makes it easier to browse Spotify API from Emacs.
;;; Code:


;;; API Reference: https://developer.spotify.com/technologies/web-api/
(require 'url)
(require 'json)
(require 'helm)
(require 'multi)
(require 'subr-x)

      
(defun helm-spotify-plus-alist-get (symbols alist)
  "Look up the value for the chain of SYMBOLS in ALIST."
  (if symbols
      (helm-spotify-plus-alist-get (cdr symbols)
		 (assoc (car symbols) alist))
    (cdr alist)))

(defmulti helm-spotify-plus-play-href (href)
  "Get the Spotify app to play the object with the given HREF."
  system-type)

(defmulti-method helm-spotify-plus-play-href 'darwin
  "Play app in OS systems."
  (href)
  (shell-command (format "osascript -e 'tell application %S to play track %S'"
			 "Spotify"
			 href)))

(defmulti-method helm-spotify-plus-play-href 'gnu/linux
  "Play app in Linux systems."
  (href)
  (shell-command "dbus-send  --print-reply --session --type=method_call --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Pause")
  (shell-command (format "dbus-send --session --type=method_call --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.OpenUri \"string:%s\""
			 href)))


;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spotify controllers ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun helm-spotify-plus-action (action)
  "Send a given ACTION to dbus."
  (shell-command
   (format "dbus-send --session --type=method_call --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.%s" action)))

(defun helm-spotify-plus-next ()
  "Play the next song."
  (interactive)
  (spotify-action "Next"))

(defun helm-spotify-plus-pause ()
  "Pause the current song."
  (interactive)
  (spotify-action "Pause"))

(defun helm-spotify-plus-play ()
  "Play a song."
  (interactive)
  (spotify-action "Play"))

(defun helm-spotify-plus-previous ()
  "Plays previous song."
  (interactive)
  (spotify-action "Previous"))

(defun helm-spotify-plus-toggle-play-pause ()
  "Toggle between play and pause song."
  (interactive)
  (spotify-action "PlayPause"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End of spotify controllers definition. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti-method helm-spotify-plus-play-href 'windows-nt
  "Play song at windows systems."
  (href)
  (shell-command (format "explorer %S" href)))

(defmulti-method-fallback helm-spotify-plus-play-href
  "Fallback method if nothing is found."
  (href)
  (message "Sorry, helm-spotify does not support playing tracks on %S." system-type))

(defun helm-spotify-plus-play-track (track)
  "Get the Spotify app to play the TRACK."
  (helm-spotify-plus-play-href (helm-spotify-plus-alist-get '(uri) track)))



(defun helm-spotify-plus-play-album (track)
  "Get the Spotify app to play the album for this TRACK."
  (let ((album-uri (helm-spotify-plus-alist-get '(album uri) track)))
    (helm-spotify-plus-play-href album-uri)))


;; magic numbers!
(defvar number-of-pages 5
  "Magic number to control the number of pages of the request.")

(defvar limit-per-request 50
  "Magic number to control the limit of candidates that Spotify API allows per request.")

(defvar helm-candidate-number-limit (* number-of-pages limit-per-request)
  "Magic number to control the helm candidate numer limit.")


(defun helm-spotify-plus-improved-search-formatted (search-term)
  "Improved version of the out spotify-search formatted using the SEARCH-TERM."
  (let ((final-list '()))
    (dotimes (counter number-of-pages final-list)
      (setq final-list (append final-list (helm-spotify-plus-search-formatted-helper search-term counter))))))


(defun helm-spotify-plus-search-formatted-helper (search-term counter)
  "Helper function to format the output due to SEARCH-TERM and COUNTER."
  (mapcar (lambda (track)
	    (cons (helm-spotify-plus-format-track track) track))
	  (helm-spotify-plus-alist-get '(tracks items) (helm-spotify-plus-artist-track-search search-term counter))))

(defun helm-spotify-plus-artist-track-search (search-term counter)
  "Function to get the current match between the SEARCH-TERM and amount of requests defined by COUNTER."
  (let ((offset (* limit-per-request counter)))
    (cond
     
     ((and (string-match "a:" search-term) (string-match "t:" search-term)) ;both the artist and track name are available
      (let ((artist-name (helm-spotify-plus-split-string "a" search-term))
            (track-name (helm-spotify-plus-split-string "t" search-term))
            (new-url (format "https://api.spotify.com/v1/search?q=%s&type=track&%s&type=artist&limit=%s&offset=%d" track-name artist-name limit-per-request offset)))
        (helm-spotify-plus-request new-url)))
     
     ((string-match "a:" search-term)	;only the artist name was given
      (let ((artist-name (helm-spotify-plus-split-string "a" search-term))
            (new-url (format "https://api.spotify.com/v1/search?q=%s&type=artist&limit=%s&offset=%d" artist-name limit-per-request offset)))
        (helm-spotify-plus-request new-url)))
     
     ((string-match "t:" search-term)	; only the track name was given
      (let ((track-name (helm-spotify-plus-split-string "t" search-term))
            (new-url (format "https://api.spotify.com/v1/search?q=%s&type=track&limit=%s&offset=%d" track-name limit-per-request offset)))
        (helm-spotify-plus-request new-url)))
     
     (t					;Else case... do a regular search for the track name
      (let ((new-url (format "https://api.spotify.com/v1/search?q=%s&type=track&limit=%s&offset=%d" search-term limit-per-request offset)))
        (helm-spotify-plus-request new-url))))))

(defun helm-spotify-plus-split-string (letter search-term)
  "Function to split based in the LETTER using the SEARCH-TERM."
  (let* ((delimiter (format ".*%s:" letter))
	 (name-tmp (car (cdr (split-string search-term delimiter))))
	 (name (car (split-string name-tmp " [a-z]:"))))
    (string-trim name)))

(defun helm-spotify-plus-request (a-url)
  "Function to request an json given a correct A-URL."
  (with-current-buffer
      (url-retrieve-synchronously a-url)
    (goto-char url-http-end-of-headers)
    (json-read)))

(defun helm-spotify-plus-decode-string-utg8 (string)
  "Function to decode the STRING due to the errors in some symbols."
  (decode-coding-string (string-make-unibyte string) 'utf-8)
  )


(defun helm-spotify-plus-format-track (track)
  "Given a TRACK, return a a formatted string suitable for display."
  (let ((track-name   (helm-spotify-plus-decode-string-utg8 (helm-spotify-plus-alist-get '(name) track)))
	(track-length (/ (helm-spotify-plus-alist-get '(duration_ms) track) 1000))
	(album-name  (helm-spotify-plus-decode-string-utg8 (helm-spotify-plus-alist-get '(album name) track)))
	(artist-names (mapcar (lambda (artist)
				(helm-spotify-plus-decode-string-utg8 (helm-spotify-plus-alist-get '(name) artist)))
			      (helm-spotify-plus-alist-get '(artists) track))))
    (format "%s (%dm%0.2ds)\n%s - %s"
	    track-name
	    (/ track-length 60) (mod track-length 60)
	    (mapconcat 'identity artist-names "/")
	    album-name)))


(defun helm-spotify-plus-search (search-term)
  "Improved SEARCH-TERM."
  (helm-spotify-plus-improved-search-formatted search-term))


(defun helm-spotify-plus-actions-for-track (actions track)
  "Return a list of helm ACTIONS available for this TRACK."
  `((,(format "Play Track - %s" (helm-spotify-plus-decode-string-utg8 (helm-spotify-plus-alist-get '(name) track)))       . helm-spotify-plus-play-track)
    (,(format "Play Album - %s" (helm-spotify-plus-decode-string-utg8 (helm-spotify-plus-alist-get '(album name) track))) . helm-spotify-plus-play-album)
    ("Show Track Metadata" . pp)))


(defun helm-spotify-plus-get-search-string ()
  "Function to require an input string for the user."
  (read-string "Enter the (partial/full) name of an Track: "))

;;;###autoload
(defun helm-spotify-plus ()
  "Brind up a custom PROMPT asking for the name of the Artist to perform the search and them all the candidates ready to be narrowed."
  (interactive)
  (helm :sources (helm-build-sync-source "Spotify"
		   :init (setq search-string (helm-spotify-plus-get-search-string))
		   :candidates (helm-spotify-plus-search search-string)
		   :multiline t
		   :action-transformer
		   (lambda (actions track)
		     (helm-spotify-plus-actions-for-track actions track)))
	:buffer "*helm-spotify*"))

(provide 'helm-spotify-plus)
;;; helm-spotify-plus.el ends here
