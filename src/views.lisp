(in-package #:nmebious)

(add-template-directory (asdf:system-relative-pathname 'nmebious "templates/"))

(defparameter +mebious.html+ (compile-template* "mebious.html"))

(defun render-board (board &key error)
  (let* ((text-posts (select-posts *text-display-count* 0 "text" board))
         (file-posts (select-posts *file-display-count*  0 "file" board))
         (stylized-text-posts (map 'list #'stylize-text-post text-posts))
         (stylized-file-posts (map 'list #'stylize-file-post file-posts)))
    (render-template* +mebious.html+ nil
                      :text-posts stylized-text-posts
                      :file-posts stylized-file-posts
                      :active-board board
                      :boards *boards*
                      :error error
                      :uploads-web-path *uploads-web-path*)))
