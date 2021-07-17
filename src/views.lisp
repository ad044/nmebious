(in-package #:nmebious)

(add-template-directory (asdf:system-relative-pathname 'nmebious "templates/"))

(defparameter +mebious.html+ (compile-template* "mebious.html"))
(defparameter +404.html+ (compile-template* "404.html"))

(defun render-board (board &key error)
  (let* ((text-posts (select-posts *text-display-count* 0 :type "text" :board board))
         (file-posts (select-posts *file-display-count*  0 :type "file" :board board))
         (stylized-text-posts (map 'list #'stylize-text-post text-posts))
         (stylized-file-posts (map 'list #'stylize-file-post file-posts)))
    (print board)
    (render-template* +mebious.html+ nil
                      :text-posts stylized-text-posts
                      :file-posts stylized-file-posts
                      :active-board board
                      :boards *boards*
                      :error error
                      :uploads-web-path *uploads-web-path*)))


(defun render-404 ()
  (render-template* +404.html+ nil))
