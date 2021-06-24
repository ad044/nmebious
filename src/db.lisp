(in-package #:nmebious)

(defun setup-db ()
  (query "CREATE TABLE IF NOT EXISTS post (
                post_id                     SERIAL PRIMARY KEY,
                board                       TEXT NOT NULL,
                ip_hash                     TEXT NOT NULL,
                submission_date             TIMESTAMP NOT NULL)")
  (query "CREATE TABLE IF NOT EXISTS text_post (
                text_data                   TEXT NOT NULL
          ) INHERITS (post)")
  (query "CREATE TABLE IF NOT EXISTS file_post (
                filename                    TEXT NOT NULL,
                checksum                    TEXT NOT NULL
          ) INHERITS (post)")
  (query "CREATE TABLE IF NOT EXISTS ban (
                ip_hash                     TEXT NOT NULL
          )"))

(defun insert-text-row (board text-data ip-hash)
  (query  (:insert-rows-into 'text-post
           :columns 'submission-date 'board 'text-data 'ip-hash
           :values `((,(format-timestring nil (now))
                      ,board
                      ,text-data
                      ,ip-hash))
           :returning 'post-id)))

(defun insert-file-row (board filename checksum ip-hash)
  (query  (:insert-rows-into 'file-post
           :columns 'submission-date 'board 'filename 'checksum 'ip-hash
           :values `((,(format-timestring nil (now))
                      ,board
                      ,filename
                      ,checksum
                      ,ip-hash))
           :returning 'post-id)))

(defun select-posts-from-table (table count col &optional board offset)
  (if board
      (query (:limit (:order-by (:select 'post-id col 'submission-date 'board :from table :where (:= 'board board))
                                (:desc 'post-id))
                     count
                     offset)
             :alists)
      (query (:limit (:order-by (:select 'post-id col 'submission-date 'board :from table)
                                (:desc 'post-id))
                     count
                     offset)
             :alists)))

(defun select-posts (count &key table board offset)
  (cond ((eql table 'text-post)
         (pairlis '(txt) (list (select-posts-from-table table count 'text-data board offset))))
        ((eql table 'file-post)
         (pairlis '(file) (list (select-posts-from-table table count 'filename board offset))))
        ((null table)
         (pairlis '(file txt)
                  (list (select-posts-from-table 'file-post count 'filename board offset)
                        (select-posts-from-table 'text-post count 'text-data board offset))))))

(defun exists-p (val table col)
  (query (:select '*
          :from table
          :where (:= col val))))

(defun exists-with-limit-p (val ip-hash table col limit board)
  (query (:limit (:order-by (:select '*
                             :from table
                             :where (:and  (:= col val)
                                           (:= 'ip-hash ip-hash)
                                           (:= 'board board)))
                            (:desc 'post-id))
                 limit)))

(defun delete-all-from-user (ip-hash)
  (query (:delete-from 'post
          :where (:= 'ip-hash ip-hash))))

(defun banned-p (ip-hash)
  (exists-p ip-hash 'ban 'ip-hash))

(defun ban (ip-hash)
  (query (:insert-into 'ban :set 'ip-hash ip-hash)))

(defun unban (ip-hash)
  (query (:delete-from 'ban
          :where (:= 'ip-hash ip-hash))))

(defun file-duplicate-p (checksum ip-hash limit board)
  (exists-with-limit-p checksum ip-hash'file-post 'checksum limit board))

(defun text-duplicate-p (text-data ip-hash limit board)
  (exists-with-limit-p text-data ip-hash 'text-post 'text-data limit board))
