(in-package #:nmebious)

(defun insert-post (data checksum type board ip-hash)
  (query  (:insert-rows-into 'post
           :columns 'submission-date 'board 'type 'data 'ip-hash 'checksum
           :values `((,(format-timestring nil (now))
                      ,board
                      ,type
                      ,data
                      ,ip-hash
                      ,checksum))
           :returning 'id)
          :single))

(defun select-posts (count offset &key type board)
  (query (sql-compile  `(:limit (:order-by (:select 'id 'type 'data 'submission-date 'board
                                             :from 'post
                                             :where (:and ,(if type `(:= 'type ,type) t)
                                                          ,(if board `(:= 'board ,board) t)))
                                           (:desc 'id))
                                ,count
                                ,offset))
         :alists))

(defun select-posts-with-ip (count offset &key type board)
  (query (sql-compile  `(:limit (:order-by (:select '*
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

(defun delete-post (id)
  (query (:delete-from 'post
          :where (:= 'id id))))

(defun get-banned-users ()
  (query (:select '*
           :from 'ban)
         :alists))

(defun get-api-keys ()
  (query (:select '*
           :from 'api-key)
         :alists))

(defun exists-p (val table col)
  (query (:select '*
           :from table
           :where (:= col val))))

(defun api-key-valid-p (api-key)
  (exists-p api-key 'api-key 'key))

(defun banned-p (ip-hash)
  (exists-p ip-hash 'ban 'ip-hash))

(defun ban (ip-hash)
  (unless (exists-p ip-hash 'ban 'ip-hash)
    (query (:insert-into 'ban :set 'ip-hash ip-hash))))

(defun unban (ip-hash)
  (query (:delete-from 'ban
          :where (:= 'ip-hash ip-hash))))

(defun remove-api-key (api-key)
  (query (:delete-from 'api-key
          :where (:= 'key api-key))))

(defun add-api-key (api-key)
  (query (:insert-into 'api-key :set 'key api-key)))

(defun create-api-key ()
  (query (:insert-into 'api-key :set 'key (hex (random-data 32)))))

(defun admin-lookup (username)
  (query (:select '*
           :from 'admin
           :where (:= username 'username))
         :alists))

(defun register-admin-user (user pass)
  (let* ((salt (random-data 32))
         (pass-hash (hash-pass pass salt)))
    (unless (exists-p user 'admin 'username)
      (query  (:insert-rows-into 'admin
               :columns 'username 'password 'salt
               :values `((,user
                          ,pass-hash
                          ,(hex salt))))))))
