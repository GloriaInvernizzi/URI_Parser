;;; -*- Mode: Lisp -*-

;; urilib_tester.lisp
(load "C:/Users/gloria/OneDrive/Desktop/UNI/URI_Parser/Lisp/urilib-parse.lisp")

(defun test-urilib-parse ()
  "Esegue il comando urilib-parse su una lista di URI e stampa i risultati."
  (setq uris '( "http://example.com"
               "http:192.168.19.2"
               "http://123.345.55.5"
               "http://123.123.12.123"
               "http:/"
               "http:path/to/file.pdf"
               "http:?query"
               "http:#fragment"
               "http:/path/to/file.pdf"
               "http:?query#fragment"
               "http://?query#fragment"
               "mailto:gloria@example.it"
               "mailto:gloria"
               "mailto"
               "news:host"
               "news:123.126.34.5"
               "news:122.344.3.2"
               "tel:+3934776623131"
               "zos://example.com/q.s.d.2.1.2.3.(12345678910)"
               "zos://example.com/abcd.12(123)"
               "zos:abcid44(12345678910)"
               "https://example.com:8080/path?query=1#fragment"
               "ftp://ftp.example.com"
               "mailto:user@example.com"
               "http://"
               ""))
    (dolist (uri uris)
      (format t "~%Input: ~a~%" uri)
      (handler-case
          (let ((result (urilib-parse uri)))
            (format t "Output: ~a~%" result))
        (error (e)
          (format t "Errore: ~a~%" e)))))


;; urilib_tester.lisp ends here