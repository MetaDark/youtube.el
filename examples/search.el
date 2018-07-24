;; -*- lexical-binding: t -*-

(require 'seq)
(require 'subr-x)
(require 'url-http)
(require 'youtube)

(eval-when-compile
  (require 'dash))

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

(defun youtube-search-interactive (query)
  (interactive "M")
  (youtube-search
   query
   (lambda (videos)
     (let ((search-results (get-buffer "youtube search results")))
       (with-current-buffer search-results
         (let ((inhibit-read-only t))
           (seq-doseq (video videos)

             (lazy-insert-image (youtube-video-thumbnail-image video))
             (newline)

             (let ((video (youtube-video-title video)))
               (insert video)
               (newline))

             (let* ((author (youtube-video-author video))
                    (name (youtube-author-name author))
                    (url (youtube-author-url author))
                    (verified (youtube-author-verified author)))
               (insert-text-button name 'action (lambda (x) (browse-url url)))
               (insert (if verified " ✓" ""))
               (newline))

             (when-let ((meta (list
                               (-some-> (youtube-video-views-string video) (concat " views"))
                               (youtube-video-date-string video))))
               (insert (string-join (seq-filter #'identity meta) " • "))
               (newline))

             (when-let (description (-some-> (youtube-video-description video)))
               (insert description)
               (newline))

             (when-let ((badges (youtube-video-badges video)))
               (insert (string-join badges " • "))
               (newline))

             (let ((url (youtube-video-url video)))
               (insert-text-button url 'action (lambda (x) (start-process (concat "mpv " url) nil "mpv" url)))
               (newline))

             (newline)))
         (goto-char (point-min))
         (read-only-mode))
       (switch-to-buffer search-results)))))
