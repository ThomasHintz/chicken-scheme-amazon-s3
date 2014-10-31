; author: Thomas Hintz
; email: t@thintz.com
; license: bsd

(module amazon-s3
  (;; debugging
   *last-sig*

   ;; params
   access-key secret-key https url

   ;; procs
   list-objects list-buckets bucket-exists? create-bucket! delete-bucket! get-object put-object! delete-object!
   put-string! put-sexp! put-file! get-string get-sexp get-file

   ;; macros
   with-bucket)

(import scheme chicken srfi-1 extras srfi-13 data-structures ports posix)
(use base64 sha1 http-client uri-common intarweb srfi-19 hmac ssax sxpath)

; needed to make intarweb work with Amazon's screwy authorization header
(define (aws-param-subunparser params)
 (sprintf "~A:~A" (alist-ref 'access-key params)
                  (alist-ref 'signed-secret params)))

(authorization-param-subunparsers
 `((aws . ,aws-param-subunparser) . ,(authorization-param-subunparsers)))


;;; params
(define (sig-date date) (date->string date))

(define access-key (make-parameter ""))
(define secret-key (make-parameter ""))
(define https (make-parameter #f))
(define url (make-parameter "s3.amazonaws.com"))

;;; helper methods

(define (assert-404 exn)
  (if (string=? ((condition-property-accessor 'exn 'message) exn)
                 "Client error: 404 Not Found")
       #f
       (abort exn)))

(define (make-aws-authorization verb resource #!key (amz-headers '()) (content-md5 #f) (content-type #f))
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
                         (newline) ;;; Always rely on x-amz-date
                         (newline)
                         (display (fold (lambda (e o)
                                          (string-append o (sprintf "~a:~a~%" (car e) (car (cdr e)))))
                                        ""
                                        can-amz-headers))
                         (display resource))))

         (hmac-sha1 (base64-encode ((hmac (secret-key) (sha1-primitive)) can-string))))

    (set! *last-sig* can-string)
    (values hmac-sha1 can-string)))

(define *last-sig* #f)
(define amazon-ns (make-parameter '(x . "http://s3.amazonaws.com/doc/2006-03-01/")))

(define (aws-headers bucket path verb content-type content-length)
  (let ((display-date (sig-date (current-date 0))))
    (headers `((x-amz-date ,display-date)
               (authorization #(aws ((access-key . ,(access-key))
                                     (signed-secret .
                                                    ,(make-aws-authorization
                                                      verb
                                                      (string-append "/"
                                                                     (if bucket (string-append bucket "/") "")
                                                                     (if path path ""))
                                                      content-type: content-type
                                                      amz-headers: `(("x-amz-date" ,(string-append "\"" display-date "\""))))
                                                    ))))
               (content-type ,(string->symbol content-type))
               (content-length ,content-length)))))

(define (aws-request bucket path verb #!key no-auth (content-type "") (content-length 0))
  (make-request
   method: (string->symbol verb)
   uri: (uri-reference (string-append "http"
                                      (if (https) "s" "")
                                      "://"
                                      (if bucket (string-append bucket ".") "")
                                      (url)
                                      (if path (string-append "/" path) "")))
   headers: (if no-auth (headers '()) (aws-headers bucket path verb content-type content-length))))

(define (aws-xml-parser path ns)
  (lambda () 
     ((sxpath path)
      (ssax:xml->sxml (current-input-port) ns))))

(define (perform-aws-request #!key
                             (bucket #f)
                             (path #f)
                             (sxpath '())
                             (body "")
                             (verb "GET")
                             (ns '((x . "http://s3.amazonaws.com/doc/2006-03-01/")))
                             (no-xml #f)
                             (no-auth #f)
			     (reader-thunk read-string)
                             (content-type "application/x-www-form-urlencoded")
                             (content-length 0))
  (with-input-from-request
   (aws-request bucket path verb no-auth: no-auth content-type: content-type content-length: content-length)
   body
   (if no-xml
       reader-thunk
       (aws-xml-parser sxpath ns))))

(define (read-byte-file path . port)
  (lambda ()
    (let ((file (open-input-file path)))
      (letrec ((read-next
		(lambda ()
		  (let ((b (read-byte file)))
		    (if (eof-object? b)
			#t
			(begin 
			  (if (> (length port) 0)
			      (write-byte b (car port))
			      (write-byte b))
			  (read-next)))))))
	(read-next))
      (close-input-port file))))

(define (write-byte-file path . port)
  (lambda ()
    (let ((file (open-output-file path)))
      (letrec ((read-next
		(lambda ()
		  (let ((b (if (> (length port) 0)
			       (read-byte (car port))
			       (read-byte))))
		    (if (eof-object? b)
			#t
			(begin 
			  (write-byte b file)
			  (read-next)))))))
	(read-next))
      (close-output-port file))))

;;; api

; broken and deprecated
; next version will have parameterized keywords so this
; won't be necessary
(define-syntax with-bucket
  (syntax-rules ()
    ((with-bucket bucket (func p1 ...))
     (func bucket p1 ...))
    ((with-bucket bucket exp body ...)
     (begin (print "I am deprecated.")
            (with-bucket bucket exp)
            (with-bucket bucket body ...)))))

(define (list-buckets)
  (perform-aws-request sxpath: '(x:ListAllMyBucketsResult x:Buckets x:Bucket x:Name *text*)))

(define (bucket-exists? bucket)
  (handle-exceptions
   exn
   (assert-404 exn)
   (perform-aws-request bucket: bucket verb: "HEAD" no-xml: #t)
   #t))

(define (create-bucket! bucket)
  (perform-aws-request bucket: bucket verb: "PUT" no-xml: #t))

(define (delete-bucket! bucket)
  (perform-aws-request bucket: bucket verb: "DELETE" no-xml: #t))

(define (list-objects bucket)
  (perform-aws-request bucket: bucket sxpath: '(x:ListBucketResult x:Contents x:Key *text*)))

(define (put-object! bucket key object-thunk object-length object-type)
  (perform-aws-request bucket: bucket path: key verb: "PUT" content-type: object-type body: object-thunk
                       content-length: object-length no-xml: #t))

(define (put-string! bucket key string)
  (put-object! bucket key (lambda () (display string)) (string-length string) "text/plain"))

(define (put-sexp! bucket key sexp)
  (let-values (((res request-uri response) (put-string! bucket key (->string sexp))))
    (values res request-uri response)))

(define (put-file! bucket key file-path)
  (put-object! bucket key (read-byte-file file-path) (file-size file-path) "binary/octet-stream"))

(define (get-object bucket key)
  (perform-aws-request bucket: bucket path: key no-xml: #t))

(define (get-string bucket key)
  (perform-aws-request bucket: bucket path: key no-xml: #t))

(define (get-sexp bucket key)
  (let-values (((string request-uri response) (get-string bucket key)))
               (values (call-with-input-string string read) request-uri response)))

(define (get-file bucket key file-path)
  (perform-aws-request bucket: bucket path: key no-xml: #t reader-thunk: (write-byte-file file-path)))

(define (delete-object! bucket key)
  (perform-aws-request bucket: bucket path: key no-xml: #t verb: "DELETE"))

)
