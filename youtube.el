;; -*- lexical-binding: t -*-

;; NOTE: elquery returns results in reverse order, so it is necessary
;; to reverse the results to get them in order.

(require 'elquery)
(require 'json)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'time-date)
(require 'url)

(eval-when-compile
  (require 'cl-macs)
  (require 'dash))

(cl-defstruct (youtube-video (:constructor youtube--video-create))
  (id nil :read-only t)
  (title nil :read-only t)
  (author nil :read-only t)
  (date nil :read-only t)
  (views nil :read-only t)
  (description nil :read-only t)
  (thumbnail nil :read-only t)
  (duration nil :read-only t)
  (badges nil :read-only t))

(defun youtube-video-url (video)
  (format "https://www.youtube.com/watch?v=%s" (youtube-video-id video)))

(defun youtube-video-date-string (video)
  (youtube-video-date video))

(defun youtube-video-views-string (video)
  (--> video
    (youtube-video-views video)
    (number-to-string it)
    (nreverse it)
    (seq-partition it 3)
    (seq-map #'nreverse it)
    (nreverse it)
    (string-join it ",")))

(defun youtube-video-duration-string (video)
  (let ((seconds (youtube-video-duration video)))
    (format-seconds
     (if (< seconds 3600) "%m:%.2s" "%h:%.2m:%.2s")
     seconds)))

(cl-defstruct (youtube-author (:constructor youtube--author-create))
  (url nil :read-only t)
  (name nil :read-only t)
  (verified nil :read-only t))

(cl-defstruct (youtube-streams (:constructor youtube--streams-create))
  (audio nil :read-only t)
  (video nil :read-only t)
  (audio-video nil :read-only t))

(cl-defstruct (youtube-stream (:constructor youtube--stream-create))
  (url nil :read-only t)
  (mimetype nil :read-only t))

(defun youtube-search (query callback)
  (url-retrieve
   ;; TODO: Escape query
   ;; TODO: Use pbj=1 instead of spf=navigate, it used by the polymer (material design) interface
   (format "https://www.youtube.com/results?search_query=%s&spf=navigate" query)
   (lambda (status callback)
     (goto-char url-http-end-of-headers)
     (->> (json-read)
       (youtube--scrape-search-body)
       (funcall callback)))
   (list callback))
  nil)

(defun youtube--scrape-search-body (body)
  (-some-> body
    (seq-elt 1) (map-elt 'body) (map-elt 'content)
    (elquery-read-string)
    (youtube--scrape-search-content)))

(defun youtube--scrape-search-content (content)
  ;; Total results:
  ;; NOTE: could overflow on 32-bit machines
  ;; (-some->>
  ;;  content
  ;;  (elquery-$ ".num-results") (car)
  ;;  (elquery-text)
  ;;  (youtube--scrape-integer))
  (->> content
    (elquery-$ ".yt-lockup-video") (nreverse)
    (seq-remove (lambda (video) (elquery-$ ".yt-badge-ad" video)))
    (seq-map #'youtube--scrape-search-video)))

(defun youtube--scrape-search-video (video)
  (let* ((thumbnail (->> video (elquery-$ ".yt-lockup-thumbnail") (car)))
         (thumbnail-img (->> thumbnail (elquery-$ "img") (car)))
         (duration (->> thumbnail (elquery-$ ".video-time") (car)))
         (title (->> video (elquery-$ ".yt-lockup-title a") (car)))
         (author (->> video (elquery-$ ".yt-lockup-byline a") (car)))
         (meta (->> video (elquery-$ ".yt-lockup-meta li")))
         (views (-> meta (seq-elt 0)))
         (date (-> meta (seq-elt 1)))
         (description (->> video (elquery-$ ".yt-lockup-description") (car)))
         (badges (->> video (elquery-$ ".yt-lockup-badges .yt-badge") (nreverse))))
    (youtube--video-create
     :id (-> video (elquery-data "context-item-id"))
     :title (-some-> title (elquery-text))
     :author (-some-> author youtube--scrape-search-author)
     :date (-some-> date (elquery-text) (youtube--scrape-relative-date))
     ;; NOTE: views could overflow on 32-bit machines (eg. Gangnam Style)
     :views (-some-> views (elquery-text) (youtube--scrape-integer))
     :description (-some-> description (elquery-text))
     :thumbnail
     (-some->
         (or (-some-> thumbnail-img (elquery-data "thumb"))
             (-some-> thumbnail-img (elquery-prop "src")))
       (url-expand-file-name))
     :duration (-some-> duration (elquery-text) (youtube--scrape-duration))
     :badges (->> badges (seq-map #'elquery-text)))))

(defun youtube--scrape-search-author (author)
  (let ((verified (->> author (elquery-$ ".yt-channel-title-icon-verified") (car))))
    (youtube--author-create
     :url (-some-> author (elquery-prop "href") (url-expand-file-name))
     :name (-> author (elquery-text))
     :verified (if verified t))))

(defun youtube-streams (id callback)
  (error "unimplemented"))

(defun youtube--scrape-integer (integer)
  (save-match-data
    (if (string-match "[0-9,]+" integer)
        (->> (match-string 0 integer)
          (replace-regexp-in-string "," "")
          (string-to-number)))))

(defun youtube--scrape-duration (duration)
  (seq-let (second minute hour)
      (--> duration
        (split-string it ":")
        (nreverse it)
        (seq-map #'string-to-number it))
    (+ second
       (* minute 60)
       (* (or hour 0) 3600))))

(defun youtube--scrape-relative-date (date)
  ;; TODO: Parse into a date object
  date)

(provide 'youtube)
