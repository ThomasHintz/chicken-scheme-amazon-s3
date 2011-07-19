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
            (test "List Bucket Objects" '() (list-objects *b*))
            (test-assert "Delete Bucket" (delete-bucket! *b*)))

(test-exit)