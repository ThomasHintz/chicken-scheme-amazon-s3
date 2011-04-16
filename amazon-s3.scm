(use base64 sha1 srfi-1 http-client uri-common intarweb json)
(load "../hmac/hmac.scm")

(define secret-access-key (make-parameter ""))

(define (make-aws-authorization verb resource #!key (date #f) (amz-headers '()) (content-md5 #f) (content-type #f))
  (let* ((can-amz-headers (sort (map (lambda (header)
                                       `(,(string-downcase (car header)) . ,(cdr header)))
                                     amz-headers)
                                (lambda (v1 v2)
                                  (string<? (car v1) (car v2)))))
         (can-string (with-output-to-string
                       (lambda ()
                         (display (string-upcase verb))
                         (newline)
                         (if content-md5 (display content-md5) (display ""))
                         (newline)
                         (if content-type (display content-type) (display ""))
                         (newline)
                         (if date (display date) (display ""))
                         (newline)
                         (display (fold (lambda (e o)
                                        (string-append o (sprintf "~a:~a~%" (car e) (cdr e))))
                                      ""
                                      can-amz-headers))
                         (display resource))))
         (hmac-sha1 (base64-encode ((sha1-hmac (secret-access-key)) can-string))))
    (values hmac-sha1 can-string)))


(secret-access-key "")
(define sig (make-aws-authorization "GET" "/test-bucket-keep-the-records/test-ktr" date: "1297806701"))

(define (get-test)
  (handle-exceptions
   exn
   ((condition-property-accessor 'client-error 'body) exn)
   (with-input-from-request
    (make-request method: 'GET
                  uri: (uri-reference "http://s3.amazonaws.com/test-bucket-keep-the-records/test-ktr"))
    `((AWSAccessKeyId . "AKIAJ4BAYHGF254QF7DQ")
      (Expires . "1297806701")
      (Signature . ,sig))
    json-read)))