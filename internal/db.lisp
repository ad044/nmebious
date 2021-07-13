(in-package #:nmebious)

(defun setup-db ()
  (query (:create-table (:if-not-exists 'post)
                        ((id :type serial :primary-key t)
                         (board :type string)
                         (ip-hash :type string)
                         (submission-date :type timestamp)
                         (type :type string)
                         (checksum :type string)
                         (data :type string))))
  (query (:create-table (:if-not-exists 'ban)
                        ((ip-hash :type string)))))

(defun insert-post (data checksum type board ip-hash)
  (query  (:insert-rows-into 'post
           :columns 'submission-date 'board 'type 'data 'ip-hash 'checksum
           :values `((,(format-timestring nil (now))
                      ,board
                      ,type
                      ,data
                      ,ip-hash
                      ,checksum))
           :returning 'id)))

(defun select-posts (count offset &optional type board)
  (query (sql-compile  `(:limit (:order-by (:select 'id 'type 'data 'submission-date 'board
                                             :from 'post
                                             :where (:and ,(if type `(:= 'type ,type) t)
                                                          ,(if board `(:= 'board ,board) t)))
                                           (:desc 'id))
                                ,count
                                ,offset))
         :alists))

(defun post-duplicate-p (checksum ip-hash limit board)
  (query  (:select  (:exists  (:limit (:order-by (:select 1
                                                   :from 'post
                                                   :where (:and  (:= 'checksum checksum)
                                                                 (:= 'ip-hash ip-hash)
                                                                 (:= 'board board)))
                                                 (:desc 'id))
                                      limit)))
          :single))

(defun delete-all-from-user (ip-hash)
  (query (:delete-from 'post
          :where (:= 'ip-hash ip-hash))))

(defun exists-p (val table col)
  (query (:select '*
          :from table
           :where (:= col val))))

(defun banned-p (ip-hash)
  (exists-p ip-hash 'ban 'ip-hash))

(defun ban (ip-hash)
  (query (:insert-into 'ban :set 'ip-hash ip-hash)))

(defun unban (ip-hash)
  (query (:delete-from 'ban
          :where (:= 'ip-hash ip-hash))))

