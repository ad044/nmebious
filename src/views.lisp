(in-package #:nmebious)

(add-template-directory (asdf:system-relative-pathname 'nmebious "templates/"))

(defparameter +mebious.html+ (compile-template* "mebious.html"))
(defparameter +error.html+ (compile-template* "error.html"))
(defparameter +about.html+ (compile-template* "about.html"))
(defparameter +preferences.html+ (compile-template* "preferences.html"))
(defparameter +auth.html+ (compile-template* "auth.html"))
(defparameter +panel-posts.html+ (compile-template* "panel-posts.html"))
(defparameter +panel-bans.html+ (compile-template* "panel-bans.html"))
(defparameter +panel-api-keys.html+ (compile-template* "panel-api-keys.html"))

;; -----------------------------------------------------------------------------
;;; Color Constructor
;; -----------------------------------------------------------------------------

(defstruct color
  (red   0 :type (integer 0 256))
  (blue  0 :type (integer 0 256))
  (green 0 :type (integer 0 256)))

(defun hex-to-color (hex)
  (make-color :red   (/ (parse-integer (subseq hex 1 3) :radix 16) 255)
              :green (/ (parse-integer (subseq hex 3 5) :radix 16) 255)
              :blue  (/ (parse-integer (subseq hex 5 7) :radix 16) 255)))

(defun on-colors (f rgb)
  (funcall f (color-blue rgb) (color-red rgb) (color-green rgb)))



;; -----------------------------------------------------------------------------
;;; Text Stuff
;; -----------------------------------------------------------------------------

(defun get-font ()
  (nth (random (length *fonts*)) *fonts*))

(defun gen-color (hue &optional (sat (random-in-range 0 100)))
  (let* ((lum (random-in-range 20 100)))
    (format nil "hsl(~A, ~A%, ~A%)" hue sat lum)))

(defun hex-to-hsl (hex)
  (let* ((rgb (hex-to-color hex))
         (max (on-colors #'max rgb))
         (min (on-colors #'min rgb))
         (average (/ (+ max min) 2)))
    (flet ((color-difference (col-1 col-2 offset)
             (let ((difference (- max min)))
               (+ (/ (- col-1 col-2) difference)
                  offset))))
      (if (eql max min)
          (list 0 0 (* 100 average))
          (let* ((s (if (> average 0.5)
                        (- 2 max min)
                        (/ (- max min) (+ max min))))
                 (h (/ (cond ((eql max (color-red rgb))
                              (color-difference (color-green rgb)
                                                (color-blue rgb)
                                                (if (< (color-green rgb) (color-blue rgb))
                                                    6
                                                    0)))
                             ((eql max (color-green rgb))
                              (color-difference (color-blue rgb)
                                                (color-red rgb)
                                                2))
                             ;; blue must be the max
                             (t
                              (color-difference (color-red rgb)
                                                (color-green rgb)
                                                4)))
                       6)))
            (list (round (* 360 h)) (round (* s 100)) (round (* 100 average))))))))

(defun text-style (board)
  (let* ((color (let* ((board-color-hex (color-for-board board)))
                  (if board-color-hex
                      (let* ((board-color-hsl (hex-to-hsl board-color-hex)))
                        ;; if achromatic
                        (if (and (eql 0 (first board-color-hsl))
                                 (eql 0 (second board-color-hsl)))
                            (gen-color 0 0)
                            (gen-color (first board-color-hsl))))
                      (gen-color 120))))
         (font-size (random-in-range 0.8 2.0))
         (left (random-in-range 0.1 40.0))
         (font-family (let* ((font (get-font)))
                        (if (string= font "Arial")
                            font
                            (format nil "~A, Arial" font)))))
    (format nil
            "color: ~A; font-family: ~A; font-size: ~Aem; left: ~A%"
            color font-family font-size left)))

(defun corrupt (text)
  (let* ((corruptions (pairlis '(#\a #\e #\i #\o #\u #\y #\s)
                               '((#\á #\ã #\à #\@)
                                 (#\è #\ë #\ê)
                                 (#\ï #\î #\1)
                                 (#\ø #\ò #\ô)
                                 (#\ü #\ù)
                                 (#\ÿ)
                                 (#\$)))))
    (map 'string #'(lambda (char)
                     (let* ((corruptions-for-character (cassoc char corruptions)))
                       (if (and corruptions-for-character
                                (eql (random 2)
                                     1))
                           (nth (random (length corruptions-for-character)) corruptions-for-character)
                           char)))
         text)))

(defun stylize-text-post (post)
  (acons :data (corrupt (cassoc :data post)) (acons :style (text-style (cassoc :board post)) post)))

;; File stuff
(defun file-style ()
  (let* ((z-index (- (random-in-range 1 10)))
         (left (random-in-range 0.1 50.0))
         (opacity (random-in-range 0.5 1.0))
         (top (random-in-range 7.0 50.0)))
    (format nil
            "z-index: ~A; left: ~A%; opacity: ~A; top: ~A%" z-index left opacity top)))

(defun stylize-file-post (post)
  (acons :style (file-style) post))

;; Rendering
(defun render-board (board &key error (page 0))
  ;; we get 1 extra post for each type to determine whether or not there is a next page
  ;; this is hacky, but its the most optimal solution i can think of currently.
  ;; if you have a better idea, feel free to shoot a pr.
  (let* ((text-posts (select-posts (+ 1 *text-display-count*) (* page *text-display-count*) :type "text" :board board))
         (file-posts (select-posts (+ 1 *file-display-count*) (* page *file-display-count*) :type "file" :board board))
         (text-posts-next-page-p (> (length text-posts) *text-display-count*))
         (file-posts-next-page-p (> (length file-posts) *file-display-count*))
         (stylized-text-posts (map 'list #'stylize-text-post (if text-posts-next-page-p
                                                                 (without-last text-posts)
                                                                 text-posts)))
         (stylized-file-posts (map 'list #'stylize-file-post (if file-posts-next-page-p
                                                                 (without-last file-posts)
                                                                 file-posts))))
    (if (or  (> (length text-posts) 0)
             (> (length file-posts) 0)
             (eql page 0))
        (render-template* +mebious.html+ nil
                          :text-posts stylized-text-posts
                          :file-posts stylized-file-posts
                          :active-board board
                          :single-board-p (single-board-p)
                          :user-prefs (parse-user-preferences)
                          :board-names (unless (single-board-p)
                                         (alist-keys *boards*))
                          :board-data (cassoc board *boards* :test #'string=)
                          :csrf-token (session-csrf-token)
                          :next-page (when (or
                                            file-posts-next-page-p
                                            text-posts-next-page-p)
                                       (+ page 1))
                          :prev-page (when (> page 0)
                                       (- page 1))
                          :error error)
        (render-error-page "This page does not exist." 404))))

(defun render-error-page (msg code)
  (setf (return-code*) code)
  (render-template* +error.html+ nil
                    :error msg))

(defun render-about-page ()
  (render-template* +about.html+ nil
                    :api-requires-key *api-requires-key*
                    :boards *boards*
                    :accepted-mime-types *accepted-mime-types*
                    :max-file-size (write-to-string  *max-file-size*)
                    :pagination-enabled-p *pagination-on-default-frontend-enabled-p*))

(defun render-preferences-page ()
  (let* ((user-prefs (parse-user-preferences))
         (render-prefs (map 'list
                            #'(lambda (pref)
                                (let* ((keyword-pref (car pref)))
                                  (car (acons keyword-pref (acons :current
                                                                  (cassoc (string-downcase (car pref)) user-prefs :test #'string=)
                                                                  (cassoc keyword-pref *web-user-preferences*))
                                              nil))))
                            *web-user-preferences*)))
    (render-template* +preferences.html+ nil
                      :preferences render-prefs)))

(defun render-admin-auth-page (&key error)
  (render-template* +auth.html+ nil
                    :csrf-token (session-csrf-token)
                    :error error))

(defun render-admin-panel-posts (&optional (page 0))
  (let* ((text-posts (select-posts-with-ip 21 (* page 20) :type "text"))
         (file-posts (select-posts-with-ip 21 (* page 20) :type "file")))
    (render-template* +panel-posts.html+ nil
                      :csrf-token (session-csrf-token)
                      :text-posts text-posts
                      :file-posts file-posts
                      :prev-page (when (> page
                                          0)
                                   (- page 1))
                      :next-page (when (or (> (length text-posts)
                                              20)
                                           (> (length file-posts)
                                              20))
                                   (+ page 1)))))

(defun render-admin-panel-bans ()
  (render-template* +panel-bans.html+ nil
                    :csrf-token (session-csrf-token)
                    :bans (get-banned-users)))

(defun render-admin-panel-api-keys ()
  (render-template* +panel-api-keys.html+ nil
                    :csrf-token (session-csrf-token)
                    :api-keys (get-api-keys)))
