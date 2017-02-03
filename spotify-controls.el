;;; package --- Only for spotify client related activities
;;; Commentary:
;;; Code:

(defmulti spotify-play-href (href)
  "Get the Spotify app to play the object with the given HREF."
  system-type)

(defmulti-method spotify-play-href 'darwin
  (href)
  (shell-command (format "osascript -e 'tell application %S to play track %S'"
             "Spotify"
             href)))

(defmulti-method spotify-play-href 'gnu/linux
  (href)
  (shell-command "dbus-send  --print-reply --session --type=method_call --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Pause")
  (shell-command (format "dbus-send --session --type=method_call --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.OpenUri \"string:%s\""
             href)))

(defmulti-method spotify-play-href 'windows-nt
  (href)
  (shell-command (format "explorer %S" href)))

(defmulti-method-fallback spotify-play-href
  (href)
  (message "Sorry, helm-spotify does not support playing tracks on %S." system-type))

(defun spotify-play-track (track)
  "Get the Spotify app to play the TRACK."
  (spotify-play-href (alist-get '(uri) track)))

(defun spotify-play-album (track)
  "Get the Spotify app to play the album for this TRACK."
  (let ((first-track (spotify-get-track (alist-get '(album href) track))))
    (spotify-play-href (alist-get '(uri) first-track))))



(provide 'spotify-control)
;;; spotify-control.el ends here
