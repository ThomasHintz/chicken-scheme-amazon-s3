; author: Thomas Hintz
; email: t@thintz.com
; license: bsd

(load "amazon-s3.scm")

(use test srfi-1)
(use amazon-s3)

(define *b* "chicken-scheme-test-bucket")

(test-group "Amazon S3"
            (test "Bucket Exists 1" #f (bucket-exists? *b*))
            (test-assert "Create Bucket" (create-bucket! *b*))
            (test "Bucket Exists 2" #t (bucket-exists? *b*))
            (test-assert "List Buckets" (list-buckets)) ; should test this more specifically...
            (test "List Bucket Objects 1" '() (list-objects *b*))
            (test-assert "Put Object" (put-object! *b* "key" "value"))
            (test "List Bucket Objects 2" '("key") (list-objects *b*))
            (test-assert "Delete Object" (delete-object! *b* "key"))
            (test-assert "Delete Bucket" (delete-bucket! *b*)))

(test-exit)