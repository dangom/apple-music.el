;;; apple-music.el --- Search for songs in Apple Music

;; Copyright (C) 2018 Daniel Gomez

;; Author: Daniel Gomez <d.gomez at posteo dot org>
;; Created: 2018-04-03
;; Version: 0.01
;; Keywords: music, itunes, apple

;; This file is not part of GNU Emacs.

;;; Copyright Notice:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;; Please see "Readme.org" for detail introductions.

;;; Code:
(require 'json)

(defvar am-apple-store-region "nl"
  "Country of your Apple account.")

;; TODO Clean search-term so that spaces get converted to + in the address, and
;; TODO search-term gets converetd to _ in the filename.
;; TODO Don't redownload file if it already exists.
(defun am/get-apple-results-from (search-term)
  "Search the apple store for a given SEARCH-TERM"
  (let ((address (concat "https://itunes.apple.com/"
                         am-apple-store-region
                         "/search?term="
                         (replace-regexp-in-string " " "+" search-term)))
        (filename (concat "/tmp/emacs-am-"
                          (replace-regexp-in-string " " "_" search-term) ".txt")))
    (unless (file-exists-p filename)
      (url-copy-file address filename))
    filename))


(defun am/applescript-quote-string (argument)
  "Quote a string for passing as a string to AppleScript.
This function taken from https://gist.github.com/jrblevin/cacbaf7b34b042bb308b"
  (if (or (not argument) (string-equal argument ""))
      "\"\""
    ;; Quote using double quotes, but escape any existing quotes or
    ;; backslashes in the argument with backslashes.
    (let ((result "")
          (start 0)
          end)
      (save-match-data
        (if (or (null (string-match "[^\"\\]" argument))
                (< (match-end 0) (length argument)))
            (while (string-match "[\"\\]" argument start)
              (setq end (match-beginning 0)
                    result (concat result (substring argument start end)
                                   "\\" (substring argument end (1+ end)))
                    start (1+ end))))
        (concat "\"" result (substring argument start) "\"")))))


(defun am/trackname-tracklink-assoc (results)
  "From a list of association lists, grab the trackName and trackViewUrl
and combine them in a new association list."
  (mapcar #'(lambda (x) (cons
                         (concat
                          (when (cdr (assoc 'kind x))
                            (concat
                             (capitalize (cdr (assoc 'kind x)))
                             ": "))
                          (cdr (assoc 'trackName x))
                          " by "
                          (cdr (assoc 'artistName x)))
                         (cdr (assoc 'trackViewUrl x))))
          results))


(defun am/http-to-itms (link)
  "Apple returns trackViewUrl as http links. We want to open them in iTunes,
so we change them to itms."
  (replace-regexp-in-string "http" "itms" link nil 'literal))


(defun am/open-song-on-itunes (song-url)
  "Calls applescript to activate iTunes and open the itms SONG-URL."
  (do-applescript
   (format "tell application \"iTunes\"
           open location %s
           end tell"
           (am/applescript-quote-string
            (am/http-to-itms song-url)))))


(defun am/search-apple-music ()
  "Use Helm to select a music match and play it on iTunes."
  (interactive)
  (let* ((json-file (am/get-apple-results-from
                     (read-string "Search Apple Music for: ")))
         (results (json-read-file json-file))
         (track-and-links (am/trackname-tracklink-assoc (cdadr results)))
         (track (completing-read "Choose an entry: " (mapcar 'car track-and-links))))
    (am/open-song-on-itunes (cdr (assoc track track-and-links)))))

(provide 'apple-music)

;; apple-music.el ends here
