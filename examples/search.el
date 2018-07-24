;; -*- lexical-binding: t -*-

(require 'seq)
(require 'subr-x)
(require 'url-http)
(require 'youtube)

(defmacro lazy-insert-image (image &optional string area slice)
  `(let ((string ,(or string " "))
         (area ,area)
         (slice ,slice)
         (image-buffer (current-buffer))
         (image-point (point)))
     (insert string)
     ,(append
       image
       `((lambda (image)
           (with-current-buffer image-buffer
             (save-excursion
               (let ((inhibit-read-only t))
                 ;; NOTE: This will get messed up if anything is
                 ;; inserted before image-point during the load
                 (goto-char image-point)
                 (delete-char (length string))
                 (insert-image image string area slice)))))))))

(youtube-search
 "rick astley"
 (lambda (videos)
   (let ((search-results (generate-new-buffer "youtube search results")))
     (with-current-buffer search-results
       (seq-doseq (video videos)
         (lazy-insert-image (youtube-video-thumbnail-image video))
         (newline)
         (youtube-video-duration-string video)
         (insert (youtube-video-title video))
         (newline)
         (let ((author (youtube-video-author video)))
           (insert-text-button (youtube-author-name author)
                               'action (lambda (x) (browse-url (youtube-author-url author))))
           (insert (if (youtube-author-verified author) " ✓" ""))
           (newline))
         (insert (youtube-video-views-string video) " views"
                 " • "
                 (youtube-video-date-string video))
         (newline)
         (insert (youtube-video-description video))
         (newline)
         (when-let ((badges (youtube-video-badges video)))
           (insert (string-join badges " • "))
           (newline))
         (insert-text-button (youtube-video-url video)
                             'action (lambda (x) (browse-url (youtube-video-url video))))
         (newline)
         (newline))
       (beginning-of-buffer))
     (view-buffer-other-window search-results))))
