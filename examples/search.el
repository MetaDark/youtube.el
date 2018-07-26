;; -*- lexical-binding: t -*-

(require 'seq)
(require 'subr-x)
(require 'url-http)
(require 'youtube)

(eval-when-compile
  (require 'dash))

(defun deferred:excursion (d callback)
  (let ((saved-buffer (current-buffer))
        (saved-marker (point-marker))
        (saved-inhibit-read-only inhibit-read-only))
    (deferred:$ d
      (deferred:nextc it
        (lambda (result)
          (with-current-buffer saved-buffer
            (save-mark-and-excursion
              (let ((inhibit-read-only saved-inhibit-read-only))
                (goto-char saved-marker)
                (funcall callback result)))))))))

(defun youtube-search-interactive (query)
  (interactive "MSearch Query: ")
  (deferred:$
    (youtube-search query)
    (deferred:nextc it
      (lambda (videos)
        (let ((search-results (get-buffer-create "youtube search results")))
          (with-current-buffer search-results
            (let ((inhibit-read-only t))
              (erase-buffer)
              (seq-doseq (video videos)
                (deferred:excursion (youtube-video-thumbnail-image video)
                  (lambda (image)
                    (insert-image image)
                    (newline)))

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
          (switch-to-buffer search-results))))))

(youtube-search-interactive "rick astley")
