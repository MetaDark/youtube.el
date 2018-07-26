;; -*- lexical-binding: t -*-

;; NOTE: elquery returns results in reverse order, so it is necessary
;; to reverse the results to get them in order.

(require 'deferred)
(require 'elquery)
(require 'json)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'time-date)
(require 'url-expand)
(require 'url-util)

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
  (thumbnail-url nil :read-only t)
  (duration-seconds nil :read-only t)
  (badges nil :read-only t))

(defun youtube-video-url (video)
  (-> "https://www.youtube.com/watch"
    (youtube--url-with-query
     `((v ,(youtube-video-id video))))))

(defun youtube-video-date-string (video)
  (youtube-video-date video))

(defun youtube-video-views-string (video)
  (-some--> (youtube-video-views video)
    (number-to-string it)
    (nreverse it)
    (seq-partition it 3)
    (seq-map #'nreverse it)
    (nreverse it)
    (string-join it ",")))

(defun youtube-video-thumbnail-image (video)
  (-some-> (youtube-video-thumbnail-url video)
    (youtube--http-get-image)))

(defun youtube-video-duration-string (video)
  (-some--> (youtube-video-duration-seconds video)
    (format-seconds (if (< it 3600) "%m:%.2s" "%h:%.2m:%.2s") it)))

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

(defun youtube-search (query)
  ;; TODO: Use pbj=1 instead of spf=navigate, it used by the
  ;; polymer (material design) interface
  (let ((url (-> "https://www.youtube.com/results"
               (youtube--url-with-query
                `((search_query ,query)
                  (spf "navigate"))))))
    (deferred:$
      (deferred:url-retrieve url)
      (deferred:nextc it
        (lambda (buf)
          (with-current-buffer buf
            (if-let ((videos (-> (json-read)
                               (youtube--scrape-search-body url))))
                videos
              (error "failed to scrape youtube search results"))))))))

(defun youtube--scrape-search-body (body url)
  (-some-> body
    (seq-elt 1) (map-elt 'body) (map-elt 'content)
    (elquery-read-string)
    (youtube--scrape-search-content url)))

(defun youtube--scrape-search-content (content url)
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
    (seq-map (lambda (video) (youtube--scrape-search-video video url)))))

(defun youtube--scrape-search-video (video url)
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
     :author (-some-> author (youtube--scrape-search-author url))
     :date (-some-> date (elquery-text) (youtube--scrape-relative-date))
     ;; NOTE: views could overflow on 32-bit machines (eg. Gangnam Style)
     :views (-some-> views (elquery-text) (youtube--scrape-integer))
     :description (-some-> description (elquery-text))
     :thumbnail-url
     (-some->
         (or (-some-> thumbnail-img (elquery-data "thumb"))
             (-some-> thumbnail-img (elquery-prop "src")))
       (url-expand-file-name url))
     :duration-seconds (-some-> duration (elquery-text) (youtube--scrape-duration-seconds))
     :badges (->> badges (seq-map #'elquery-text)))))

(defun youtube--scrape-search-author (author url)
  (let ((verified (->> author (elquery-$ ".yt-channel-title-icon-verified") (car))))
    (youtube--author-create
     :url (-some-> author (elquery-prop "href") (url-expand-file-name url))
     :name (-> author (elquery-text))
     :verified (if verified t))))

(defun youtube-streams (id)
  (error "unimplemented"))

(defun youtube--scrape-integer (integer)
  (save-match-data
    (if (string-match "[0-9,]+" integer)
        (->> (match-string 0 integer)
          (replace-regexp-in-string "," "")
          (string-to-number)))))

(defun youtube--scrape-duration-seconds (duration)
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

(defun youtube--url-with-query (url query)
  (let ((query-string (url-build-query-string query)))
    (if (string-empty-p query-string) url
      (concat url "?" query-string))))

(defun youtube--http-get-image (image)
  (deferred:$
    (deferred:url-retrieve image)
    (deferred:nextc it
      (lambda (buf)
        (with-current-buffer buf
          ;; TODO: Parse content type / extension from HTTP response
          (--> (buffer-substring-no-properties
                (search-forward "\n\n") (point-max))
            (create-image it 'jpeg t)))))))

(provide 'youtube)
