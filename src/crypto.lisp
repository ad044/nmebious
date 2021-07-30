(in-package #:nmebious)

;; Used only once (for hashing the admin password) and stored in memory.
;; Regenerated upon re-evaluation, restarts, etc.
(defparameter *admin-pass-salt* (random-data 32))

(defparameter *argon2-kdf* (make-kdf :argon2d :block-count 15000))

(defun hmac-sha256 (secret text)
  (hex (hmac-sha256-bytes secret text)))

(defun hash-ip (ip)
  (hmac-sha256 *hmac-secret* ip))

(defun hmac-sha256-bytes (secret text)
  (let ((hmac (make-hmac (ascii-string-to-byte-array secret) :sha256)))
    (update-hmac hmac (ascii-string-to-byte-array text))
    (hmac-digest hmac)))

(defun hash-admin-pass (pass)
  (hex (derive-key *argon2-kdf*
                   (ascii-string-to-byte-array pass)
                   *admin-pass-salt*
                   2
                   32)))
