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
(use base64 sha1 http-client uri-common intarweb srfi-19 hmac ssax sxpath)

; needed to make intarweb work with Amazon's screwy authorization header
(define (aws-param-subunparser params)
 (sprintf "~A:~A" (alist-ref 'access-key params)
                  (alist-ref 'signed-secret params)))

(authorization-param-subunparsers
 `((aws . ,aws-param-subunparser) . ,(authorization-param-subunparsers)))

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
  ;(handle-exceptions
  ; exn
  ; ((condition-property-accessor 'client-error 'body) exn)
  (with-input-from-request
   (make-request
    method: 'GET
    uri: (uri-reference (string-append "http" (if (https) "s" "") "://" bucket ".s3.amazonaws.com"))
    headers: (let ((n (current-date 0)))
               (headers `((date #(,(intarweb-date n) ()))
                          (authorization #(aws ((access-key . ,(access-key))
                                                (signed-secret .
                                                         ,(make-aws-authorization
                                                          "GET" (string-append "/" bucket "/")
                                                          date: (sig-date n)
                                                          content-type: "application/x-www-form-urlencoded")))))))))
   '()
   (lambda () 
     ((sxpath '(x:ListBucketResult x:Contents x:Key *text*))
      (ssax:xml->sxml (current-input-port) '((x . "http://s3.amazonaws.com/doc/2006-03-01/")))))))
)