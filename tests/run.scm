; author: Thomas Hintz
; email: t@thintz.com
; license: bsd

(use test srfi-1 amazon-s3)

(define *b* "chicken-scheme-test-bucket")

(test-group "Amazon S3"
            (test "Bucket Exists 1" #f (bucket-exists? *b*))
            (test-assert "Create Bucket" (create-bucket! *b*))
            (test "Bucket Exists 2" #t (bucket-exists? *b*))
            (test-assert "List Buckets" (list-buckets)) ; should test this more specifically...
            (test "List Bucket Objects 1" '() (list-objects *b*))
            (test-assert "Put Object" (put-object! *b* "key" (lambda () "value") (string-length "value") "text/plain"))
            (test "List Bucket Objects 2" '("key") (list-objects *b*))
            (test-assert "Delete Object" (delete-object! *b* "key"))
            (test-assert "Put String" (put-string! *b* "string" "res-string"))
            (test "Get String" "res-string" (get-string *b* "string"))
            (test-assert "Delete Object 2" (delete-object! *b* "string"))
            (test-assert "Put Sexp" (put-sexp! *b* "sexp" '(+ 1 2 3)))
            (test "Get Sexp" 6 (eval (get-sexp *b* "sexp")))
            (test-assert "Delete Object 3" (delete-object! *b* "sexp"))
            (test-assert "Delete Bucket" (delete-bucket! *b*)))

(test-exit)