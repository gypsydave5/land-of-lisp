(defpackage web-server
  (:use #:common-lisp #:socket)
  (:export #:serve))
(in-package #:web-server)

(coerce (list #\6 #\4) 'string)         ;; "64"
(parse-integer "64")                    ;; 64
(parse-integer "FF" :radix 16)          ;; 255
(parse-integer "G" :junk-allowed t)     ;; NIL

;; HTTP decoding
(defun http-char (c1 c2 &optional (default #\Space))
  (let ((code (parse-integer
               (coerce (list c1 c2) 'string)
               :radix 16
               :junk-allowed t)))
    (if code
        (code-char code)
        default)))

(defun decode-param (s)
  (labels ((f (lst)
             (when lst (case (car lst)
                         (#\% (cons (http-char (cadr lst) (caddr lst))
                                    (f (cdddr lst))))
                         (#\+ (cons #\space (f (cdr lst))))
                         (otherwise (cons (car lst)
                                          (f (cdr lst))))))))
    (coerce (f (coerce s 'list)) 'string)))

;; Request Parameter decoding
(position #\= "0123=567") ;; => 4
(string-upcase "Hello") ;; => "HELLO"
(intern "HELLO")
;; => HELLO
;;    :INTERNAL
(cons (intern "HELLO") (intern "WORLD")) ;; => (HELLO . WORLD)
(subseq "Hello, World" 0 5) ;; => "Hello"

(defun parse-params (s) (let* ((i1 (position #\= s))
         (i2 (position #\& s)))
    (cond (i1
           (cons (cons (intern (string-upcase (subseq s 0 i1)))
                       (decode-param (subseq s (1+ i1) i2)))
                 (and i2 (parse-params (subseq s (1+ i2))))))
          ((equal s "") nil)
          (t s))))

(parse-params "name=bob&age=25&gender=male")
;; => ((NAME . "bob") (AGE . "25") (GENDER . "male"))

;; NB - both above better with TCR

(position #\space "012 456 890") ;; => 3
(position #\space "012 456 890" :from-end t) ;; => 7
(position #\? "01234") ;; => NIL

;; Headers
(defun parse-url (s)
  (let* ((url (subseq s
                      (+ 2 (position #\space s))
                      (position #\space s :from-end t)))
         (x (position #\? url)))
    (if x
        (cons (subseq url 0 x) (parse-params (subseq url (1+ x))))
        (cons url '()))))

(parse-url "GET /fun/cats HTTP/1.1")
;; => ("fun/cats")
(parse-url "GET /fun/cats?skip=10&limit=20 HTTP/1.1")
;; => ("fun/cats" (SKIP . "10") (LIMIT . "20"))

;; rest of the headers - as an alist, from a stream
(defun get-header (stream)
  (let* ((s (read-line stream))
         (h (let ((i (position #\: s)))
              (when i
                (cons (intern (string-upcase (subseq s 0 i)))
                      (subseq s (+ i 2)))))))
    (when h
      (cons h (get-header stream)))))

(get-header (make-string-input-stream
             "foo: 1
bar: abc, 123

"))
;; => ((FOO . "1") (BAR . "abc, 123"))

(assoc 'foo '((foo . 1) (bar . 2) (baz . 3)))
;; => (FOO . 1)
(cdr (assoc 'foo '((foo . "foo") (bar . "bar"))))
;; => "foo"
(parse-integer "11")
;; => 11
;;    2
(make-string 11)
;; => "           "
(defparameter *five-byte-string* (make-string 5))
;; => *FIVE-BYTE-STRING*
*five-byte-string*
;; => "     "
(read-sequence *five-byte-string* (make-string-input-stream "0123456789"))
;; => 5
*five-byte-string*
;; => "01234"

(defun get-content-params (stream header)
  (let ((length (cdr (assoc 'content-length header))))
    (when length
      (let ((content (make-string (parse-integer length))))
        (read-sequence content stream)
        (parse-params content)))))

(get-content-params (make-string-input-stream "foo=1&bar=me+you") '((content-length . "16")))
;; => ((FOO . "1") (BAR . "me you"))

;; Serve
(defun serve (request-handler)
  (let ((socket (socket-server 8080)))
    (unwind-protect
         (loop (with-open-stream (stream (socket-accept socket))
                 (let* ((url (parse-url (read-line stream)))
                        (path (car url))
                        (header (get-header stream))
                        (params (append (cdr url)
                                        (get-content-params stream header)))
                        (*standard-output* stream))
                   (funcall request-handler path header params))))
      (socket-server-close socket))))

(defun response-header () "" "HTTP/1.1 200 OK
Content-Type: text/html

")

(defun dummy-handler (a b c)
  (princ (response-header))
  (princ "<html><h1>This is a test</h1></html>"))
