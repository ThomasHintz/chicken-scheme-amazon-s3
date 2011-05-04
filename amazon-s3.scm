; author: Thomas Hintz
; email: t@thintz.com
; license: bsd

(module amazon-s3
  (;; params
   access-key secret-key https

              *last-sig*

   list-objects)
              
   ;; procs
   ;bucket-exists? create-bucket delete-bucket list-buckets list-objects
   ;get-object put-object delete-object)

(import scheme chicken srfi-1 extras srfi-13 data-structures ports posix)
(use base64 sha1 http-client uri-common intarweb srfi-19 hmac ssax)

; needed to make intarweb work with Amazon's screwy authorization header
(header-unparsers
 (alist-update! 'authorization
                (lambda (v) (list (vector-ref (first v) 0)))
                (header-unparsers)))

(define (intarweb-date date) (string->time (date->string date "~a ~b ~d ~T ~Y GMT")))
(define (sig-date date) (date->string date "~a, ~d ~b ~Y ~T GMT"))

(define access-key (make-parameter ""))
(define secret-key (make-parameter ""))
(define https (make-parameter #f))

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

(define (list-objects bucket)
;  (handle-exceptions
;   exn
;   ((condition-property-accessor 'client-error 'body) exn)
  (with-input-from-request
   (make-request
    method: 'GET
    uri: (uri-reference (string-append "http" (if (https) "s" "") "://" bucket ".s3.amazonaws.com"))
    headers: (let ((n (current-date 0)))
               (headers `((date #(,(intarweb-date n) ()))
                          (authorization ,(string-append "AWS " (access-key) ":"
                                                         (make-aws-authorization
                                                          "GET" (string-append "/" bucket "/")
                                                          date: (sig-date n)
                                                          content-type: "application/x-www-form-urlencoded")))))))
    '()
    (lambda () (ssax:xml->sxml (current-input-port) '()))))
   




;(define *the-date* #f)
;(define *date-as-date* (current-date -4))
;(define (update-date)
;  (set! *date-as-date* (current-date -4))
;  (set! *the-date* (date->string *date-as-date* "~a, ~d ~b ~Y ~T GMT")))
;(define (sig) (make-aws-authorization "GET" "/test-bucket-keep-the-records" date: *the-date* content-type: "application/x-www-form-urlencoded"))

;(define (get-test)
;  (update-date)
;  (handle-exceptions
;   exn
;   ((condition-property-accessor 'client-error 'body) exn)
;   (with-input-from-request
;    (make-request method: 'GET
;                  uri: (uri-reference "http://s3.amazonaws.com/test-bucket-keep-the-records")
;                  headers: (headers `((date #(,(string->time (date->string *date-as-date* "~a ~b ~d ~T ~Y GMT")) ()))
;                                      (authorization ,(string-append "" (sig))))))
;    '()
;    read-string)))
)

;(update-date)
;(define r (make-request uri: (uri-reference "")
;                        port: (current-output-port)
;                        headers: (headers `((date #(,(string->time (date->string *date-as-date* "~a ~b ~d ~T ~Y ~z")) ()))
;                                            (authorization ,(string-append "" (sig)))))))
;(header-unparsers
; (alist-update! 'authorization
;                (lambda (v) (list (vector-ref (first v) 0)))
;                (header-unparsers)))
;(write-request r)