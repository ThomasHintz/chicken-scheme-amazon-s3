; author: Thomas Hintz
; email: t@thintz.com
; license: bsd

(module amazon-s3
  (;; params
   access-key secret-key https

              *last-sig*

   list-objects list-buckets bucket-exists? create-bucket! delete-bucket!)
              
   ;; procs
   ;get-object put-object delete-object)

(import scheme chicken srfi-1 extras srfi-13 data-structures ports posix)
(use base64 sha1 http-client uri-common intarweb srfi-19 hmac ssax sxpath)

; needed to make intarweb work with Amazon's screwy authorization header
(define (aws-param-subunparser params)
 (sprintf "~A:~A" (alist-ref 'access-key params)
                  (alist-ref 'signed-secret params)))

(authorization-param-subunparsers
 `((aws . ,aws-param-subunparser) . ,(authorization-param-subunparsers)))

;;; params

(define (intarweb-date date) (string->time (date->string date "~a ~b ~d ~T ~Y GMT")))
(define (sig-date date) (date->string date "~a, ~d ~b ~Y ~T GMT"))

(define access-key (make-parameter ""))
(define secret-key (make-parameter ""))
(define https (make-parameter #f))

;;; helper methods

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
         (hmac-sha1 (base64-encode ((hmac (secret-key) (sha1-primitive)) can-string))))
    (set! *last-sig* can-string)
    (values hmac-sha1 can-string)))

(define *last-sig* #f)
(define amazon-ns (make-parameter '(x . "http://s3.amazonaws.com/doc/2006-03-01/")))

(define (aws-headers bucket path verb)
  (let ((n (current-date 0)))
    (headers `((date #(,(intarweb-date n) ()))
               (authorization #(aws ((access-key . ,(access-key))
                                     (signed-secret .
                                                    ,(make-aws-authorization
                                                      verb (string-append "/" (if bucket (string-append bucket "/") ""))
                                                      date: (sig-date n)
                                                      content-type: "application/x-www-form-urlencoded")))))))))

(define (aws-request bucket path verb)
  (make-request
   method: (string->symbol verb)
   uri: (uri-reference (string-append "http" (if (https) "s" "") "://" (if bucket (string-append bucket ".") "")
                                      "s3.amazonaws.com"))
   headers: (aws-headers bucket "" verb)))

(define (aws-xml-parser path ns)
  (lambda () 
     ((sxpath path)
      (ssax:xml->sxml (current-input-port) ns))))

(define (perform-aws-request bucket path sxpath-path verb #!key
                             (ns '((x . "http://s3.amazonaws.com/doc/2006-03-01/")))
                             (no-xml #f))
  ;(handle-exceptions
  ; exn
  ; ((condition-property-accessor 'client-error 'body) exn)
  (with-input-from-request
   (aws-request bucket path verb)
   '()
   (if no-xml
       read-string
       (aws-xml-parser sxpath-path ns))))

;;; api

(define (list-objects bucket)
  (perform-aws-request bucket "" '(x:ListBucketResult x:Contents x:Key *text*) "GET"))

(define (list-buckets)
  (perform-aws-request #f "" '(x:ListAllMyBucketsResult x:Buckets x:Bucket x:Name *text*) "GET"))

; probably should use something faster than list-objects for the test...
(define (bucket-exists? bucket)
  (handle-exceptions
   exn
   (if (string=? (call-with-input-string ((condition-property-accessor 'client-error 'body) exn)
                               (lambda (p)
                                 (first ((sxpath '(Error Code *text*)) (ssax:xml->sxml p '())))))
                 "NoSuchBucket")
       #f
       (abort exn))
   (list-objects bucket)
   #t))

(define (create-bucket! bucket)
  (perform-aws-request bucket "" '() "PUT" no-xml: #t))


(define (delete-bucket! bucket)
  (perform-aws-request bucket "" '() "DELETE" no-xml: #t))

)