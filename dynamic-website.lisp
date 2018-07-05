(load :web-server)
(defpackage #:dynamic-website
  (:use #:common-lisp)
  (:import-from #:web-server #:serve))
(in-package #:dynamic-website)

(defun hello-request-handler (path header params)
  (if (equal path "greeting")
      (let ((name (assoc 'name params)))
        (if (not name)
            (princ (header (html-wrapper "title" "<form>What is your name?<input name='name' />
</form>")))
            (princ (header (html-wrapper "title" (format nil "Nice to meet you, ~a!" (cdr name)))))))
      (return404)))

(defun header (body) (concatenate 'string
                                  "HTTP/1.1 200 OK
Content-Type: text/html

" body))

(defun return404 () (princ "HTTP/1.1 404 NOT FOUND
Content-Type: text/html

<h1>Sorry... I don't know that page.</h1>"))

(defun html-wrapper (page-title content) (concatenate 'string "<html>
  <head>
    <meta charset=\"utf-8\" />
    <meta name=\"viewport\" content=\"width=device-width\" />
    <title>" page-title "</title>
  </head>
  <body>
" content "
</body>
</html>"))

(serve #'hello-request-handler)
