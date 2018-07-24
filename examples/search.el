(require 'seq)
(require 'youtube)

(youtube-search
 "rick astley"
 (lambda (videos)
   (let ((search-results (generate-new-buffer "search results")))
     (with-current-buffer search-results
       (seq-doseq (video videos)
         (insert (youtube-video-title video))
         (newline)
         (insert (youtube-author-name (youtube-video-author video)))
         (newline)
         (insert (youtube-video-views-string video) "views " (youtube-video-date-string video))
         (newline)
         (insert (youtube-video-description video))
         (newline)
         (seq-doseq (badge (youtube-video-badges video))
           (insert badge " "))))
     (switch-to-buffer-other-window search-results))))
