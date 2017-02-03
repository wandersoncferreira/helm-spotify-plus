;;; package --- Helm-spotify-plus
;;; Commentary:
;;; Code:
;;; API Reference: https://developer.spotify.com/technologies/web-api/

(require 'url)
(require 'json)
(require 'helm)
(require 'multi)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 1: Specific for Spotify client manipulations ;;
;; Examples are: Stop, Playing Album, Playing Track     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti spotify-play-href (href)
  "get the spotify app to play the object with the given href."
  system-type)

(defmulti-method spotify-play-href 'darwin
  (href)
  (shell-command (format "osascript -e 'tell application %s to play track %s'"
	     "spotify"
	     href)))

(defmulti-method spotify-play-href 'gnu/linux
  (href)
  (shell-command "dbus-send  --print-reply --session --type=method_call --dest=org.mpris.mediaplayer2.spotify /org/mpris/mediaplayer2 org.mpris.mediaplayer2.player.pause")
  (shell-command (format "dbus-send --session --type=method_call --dest=org.mpris.mediaplayer2.spotify /org/mpris/mediaplayer2 org.mpris.mediaplayer2.player.openuri \"string:%s\""
	     href)))

(defmulti-method spotify-play-href 'windows-nt
  (href)
  (shell-command (format "explorer %s" href)))

(defmulti-method-fallback spotify-play-href
  (href)
  (message "sorry, helm-spotify does not support playing tracks on %s." system-type))

(defun spotify-play-track (track)
  "get the spotify app to play the track."
  (spotify-play-href (alist-get '(uri) track)))

(defun spotify-play-album (track)
  "get the spotify app to play the album for this track."
  (let ((first-track (spotify-get-track (alist-get '(album href) track))))
    (spotify-play-href (alist-get '(uri) first-track))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 2: Describing all the comunication with the Spotify API ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun spotify-search (search-term)
  "Search spotify for SEARCH-TERM, returning the results as a Lisp structure."
  (let ((a-url (format "https://api.spotify.com/v1/search?q=%s&20&type=track&limit=50" search-term)))
    (with-current-buffer
    (url-retrieve-synchronously a-url)
      (goto-char url-http-end-of-headers)
      (json-read))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 3: Helm formating and narrowing of candidates from responses of the Spotify API ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun alist-get (symbols alist)
  "Look up the value for the chain of SYMBOLS in ALIST."
  (if symbols
      (alist-get (cdr symbols)
	 (assoc (car symbols) alist))
    (cdr alist)))


(defun spotify-get-track (album-href)
  (let ((response (with-current-buffer
	   (url-retrieve-synchronously album-href)
	   (goto-char url-http-end-of-headers)
	   (json-read))))
    (aref (alist-get '(tracks items) response) 0)))


(defun spotify-format-track (track)
  "Given a TRACK, return a a formatted string suitable for display."
  (let ((track-name   (alist-get '(name) track))
    (track-length (/ (alist-get '(duration_ms) track) 1000))
    (album-name   (alist-get '(album name) track))
    (artist-names (mapcar (lambda (artist)
		(alist-get '(name) artist))
		  (alist-get '(artists) track))))
    (format "%s (%dm%0.2ds)\n%s - %s"
	track-name
	(/ track-length 120) (mod track-length 120)
	(mapconcat 'identity artist-names "/")
	album-name)))

(defun spotify-search-formatted (search-term)
  (mapcar (lambda (track)
	(cons (spotify-format-track track) track))
      (alist-get '(tracks items) (spotify-search search-term))))


(defun helm-spotify-search ()
  (spotify-search-formatted helm-pattern))

(defun helm-spotify-actions-for-track (actions track)
  "Return a list of helm ACTIONS available for this TRACK."
  `((,(format "Play Track - %s" (alist-get '(name) track))       . spotify-play-track)
    (,(format "Play Album - %s" (alist-get '(album name) track)) . spotify-play-album)
    ("Show Track Metadata" . pp)))

;;;###autoload
(defvar helm-source-spotify-track-search
  '((name . "Spotify")
    (volatile)
    (delayed)
    (multiline)
    (requires-pattern . 1)
    (candidates-process . helm-spotify-search)
    (action-transformer . helm-spotify-actions-for-track)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 4: All the interactive functions which the user might be calling for some functionality ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun helm-spotify ()
  "Bring up a Spotify search interface in helm."
  (interactive)
  (helm :sources '(helm-source-spotify-track-search)
    :buffer "*helm-spotify*"))

(provide 'helm-spotify-plus)
;;; helm-spotify-plus.el ends here
