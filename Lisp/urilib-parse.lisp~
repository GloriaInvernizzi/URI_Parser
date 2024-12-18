;;;; Pomi Beatrice 914386
;;;; Invernizzi Gloria MATRICOLA

;;;; -*- Mode: Lisp -*-

;;;; urilib-parse.lisp

;;;; RFC3986 (https://datatracker.ietf.org/doc/html/rfc3986)

;;; Struct contenente tutti i campi che compongono un URI.
(defstruct uri-struct schema userinfo host port path query fragment)

;;; Ognuna di queste funzioni rappresenta l'estrazione di un singolo
;;; componente dall'URISTRUCT.
(defun urilib-scheme (uri) (uri-struct-schema uri))
(defun urilib-userinfo (uri) (uri-struct-userinfo uri))
(defun urilib-host (uri) (uri-struct-host uri))
(defun urilib-port (uri) (uri-struct-port uri))
(defun urilib-path (uri) (uri-struct-path uri))
(defun urilib-query (uri) (uri-struct-query uri))
(defun urilib-fragment (uri) (uri-struct-fragment uri))

;;; Struct per contenere i componenti dell'authority.
(defstruct authority-struct userinfo host port)

;;; Metodo di debug per stampare l'URI passato in input sullo stream
;;; di destinazione. Nel caso in cui stream non fosse passato alla
;;; funzione, l'output verrà stampato sullo stream corrente.
(defun urilib-display (uri &optional (stream T))
  (format stream "Schema:~13T~S~%" (urilib-scheme uri))
  (format stream "Userinfo:~13T~S~%" (urilib-userinfo uri))
  (format stream "Host:~13T~S~%" (urilib-host uri))
  (format stream "Port:~13T~D~%" (urilib-port uri))
  (format stream "Path:~13T~S~%" (urilib-path uri))
  (format stream "Query:~13T~S~%" (urilib-query uri))
  (format stream "Fragment:~13T~S" (urilib-fragment uri))
  (if (not (equal stream T))
      (close stream)
      T))

;;; Metodo principale per la scomposizione dei componenti dell'URI,
;;; a partire dal parsing dello Schema, che restituisce infine
;;; una URISTRUCT popolata con i suoi componenti.
(defun urilib-parse (uri)
  (if (stringp uri)
      (progn
	(setq schema (extract-schema (coerce uri 'list)))
	(if (null after) ; caso "solo Schema"
	    (make-uri-struct
	     :schema (coerce schema 'string)
	     :userinfo NIL
	     :host NIL
	     :port 80
	     :path NIL
	     :query NIL
	     :fragment NIL)
	    (if (special-schema-p (coerce schema 'string)) ; caso "sintassi speciale"
		(extract-special-uri (coerce schema 'string) after)
		(progn
	          (setq authority (extract-authority after))
		  (make-uri-struct
		   :schema (coerce schema 'string)
		   :userinfo (if (equal
				  (authority-struct-userinfo authority)
				  NIL)
				 NIL
				 (coerce
				  (authority-struct-userinfo authority)
				  'string))
		   :host (coerce
			  (authority-struct-host authority)
			  'string)
		   :port (parse-integer (coerce
					 (authority-struct-port authority)
					 'string))
		   :path (if (contains-separator after "/")
			     (coerce (extract-path after) 'string)
			     NIL)
		   :query (if (contains-separator after "?")
			      (coerce (extract-query after) 'string)
			      NIL)
		   :fragment (if (contains-separator after "#")
				 (coerce
				  (extract-fragment after)
				  'string)
				 NIL))))))
      NIL))

;;; Estrazione dello Schema
(defun extract-schema (chars)
  (cond ((null chars) (error "Schema is not valid"))
	((string= (first chars) ":")
	 (defparameter after (rest chars))
	 NIL)
	(T (if (identificatorep (first chars))
	       (append
		(list (first chars))
		(extract-schema (rest chars)))
	       (error "invalid schema character")))))

;;; Predicato per definire la presenza di uno Schema caratterizzato
;;; da "sintassi speciale"
(defun special-schema-p (schema)
  (or (string= schema "mailto")
      (string= schema "news")
      (string= schema "tel")
      (string= schema "fax")
      (string= schema "zos")))

;;; Estrazione dei componenti dell'Authority che vengono restituiti
;;; all'interno di un AUTHORITY-STRUCT.
(defun extract-authority (chars)
  ;; Nel primo caso l'Authority è presente
  (cond ((and (string= (first chars) "/")
	      (string= (second chars) "/"))
	 (progn
	   (setq authority (extract-authority-chars
			    (rest (rest chars))))
	   (make-authority-struct
	    :userinfo (if (contains-separator authority "@")
			  (extract-userinfo authority)
			  NIL)
	    :host (extract-host authority)
	    :port (if (contains-separator authority ":")
		      (progn
		        (setq port (extract-port authority))
			(if (null port)
			    (error "invalid port")
			    port))
		      "80"))))
        ;; Nel secondo caso l'Authority non è presente
        ((and (string= (first chars) "/")
	      (not (string= (second chars) "/")))
	 (progn
	   (defparameter after chars)
	   (make-authority-struct
	    :userinfo NIL
	    :host NIL
	    :port "80")))
	(T (error "authority not recognized"))))

;;; Estrazione ricorsiva dei caratteri che compongono l'Authority.
(defun extract-authority-chars (chars)
  (cond ((or (string= (first chars) "/")
	     (string= (first chars) "?")
	     (string= (first chars) "#"))
	 (progn
	   (defparameter after chars)
	   NIL))
	((null chars)
	 (progn
	   (defparameter after NIL)
	   NIL))
	(T (append (list (first chars))
		   (extract-authority-chars (rest chars))))))

;;; Funzione di utility che determina la presenza o meno di un certo
;;; carattere separatore passato in input.
(defun contains-separator (chars separator)
  (cond ((null chars) NIL)
	((string= (first chars) separator) T)
	(T (contains-separator (rest chars) separator))))

;;; Estrazione ricorsiva dei caratteri che compongono lo Userinfo.
(defun extract-userinfo (chars)
  (cond ((null chars) NIL)
	((string= (first chars) "@") NIL)
	(T (if (identificatorep (first chars))
	       (append (list (first chars))
		       (extract-userinfo (rest chars)))
	       (error "invalid userinfo character")))))

;;; Estrazione ricorsiva dei caratteri che compongono l'Host.
(defun extract-host (chars)
  (cond ((contains-separator chars "@")
	 (extract-host (rest chars)))
	((string= (first chars) ":") NIL)
	((null chars) NIL)
	(T (if (identificatorep (first chars))
	       (append (list (first chars))
		       (extract-host (rest chars)))
	       (error "invalid host character")))))

;;; Estrazione ricorsiva dei caratteri che compongono Port.
(defun extract-port (chars)
  (cond ((contains-separator chars ":")
	 (extract-port (rest chars)))
	((null chars) NIL)
	(T (if (numberp (digit-char-p (first chars)))
	       (append (list (first chars))
		       (extract-port (rest chars)))
	       (error "invalid port character")))))

;;; Estrazione ricorsiva dei caratteri che compongono il Path,
;;; con controllo sull'esistenza di "/" come primo carattere.
(defun extract-path (chars)
  (cond ((string= (first chars) "/")
	 (extract-path-chars (rest chars)))))

(defun extract-path-chars (chars)
  (cond ((null chars) NIL)
	((or (string= (first chars) "?")
	     (string= (first chars) "#"))
	 (progn
	   (defparameter after chars)
	   NIL))
	(T (if (or (identificatorep (first chars))
		   (string= (first chars) "/"))
	       (append (list (first chars))
		       (extract-path-chars (rest chars)))
	       (error "invalid path character")))))

;;; Estrazione ricorsiva dei caratteri che compongono la Query.
(defun extract-query (chars)
  (cond ((string= (first chars) "?")
	 (extract-query-chars (rest chars)))))

(defun extract-query-chars (chars)
  (cond ((null chars) NIL)
	((string= (first chars) "#")
	 (progn
	   (defparameter after chars)
	   NIL))
	(T (if (caratterip (first chars))
	       (append (list (first chars))
		       (extract-query-chars (rest chars)))
	       (error "invalid query character")))))

;;; Estrazione ricorsiva dei caratteri che compongono il Fragment.
(defun extract-fragment (chars)
  (cond ((string= (first chars) "#")
	 (extract-fragment-chars (rest chars)))))

(defun extract-fragment-chars (chars)
  (cond ((null chars)
	 (progn
	   (defparameter after NIL)
	   NIL))
	(T (if (caratterip (first chars))
	       (append (list (first chars))
		       (extract-fragment-chars (rest chars)))
	       (error "invalid fragment character")))))

;;; Parsing di URI nel caso di Schema caratterizzati da "sintassi speciali".
(defun extract-special-uri (schema chars)
  (cond ((string= schema "mailto") ; parsing mailto
	 (if (contains-separator chars "@")
	     (make-uri-struct
	      :schema schema
	      :userinfo (coerce (extract-userinfo chars) 'string)
	      :host (coerce (extract-host chars) 'string)
	      :port "80")
	     (make-uri-struct
	      :schema schema
	      :userinfo (coerce (extract-userinfo chars) 'string)
	      :port "80")))
	((string= schema "news") ; parsing news
	 (make-uri-struct
	  :schema schema
	  :host (if (or (contains-separator chars "@")
			(contains-separator chars ":"))
		    (error "invalid host")
		    (coerce (extract-host chars) 'string))
	  :port 80))
	((or (string= schema "tel") (string= schema "fax")) ; parsing tel e fax
	 (make-uri-struct
	  :schema schema
	  :userinfo (if (contains-separator chars "@")
			(error "invalid userinfo")
			(coerce (extract-userinfo chars) 'string))
	  :port 80))
	((string= schema "zos") ; parsing zos
	 (progn
	   (setq authority (extract-authority after))
	   (make-uri-struct
	    :schema (coerce schema 'string)
	    :userinfo (if (equal
			   (authority-struct-userinfo authority)
			   NIL)
			  NIL
			  (coerce
			   (authority-struct-userinfo authority)
			   'string))
	    :host (coerce
		   (authority-struct-host authority)
		   'string)
	    :port (parse-integer (coerce
				  (authority-struct-port authority)
				  'string))
	    :path (if (contains-separator after "/")
		      (coerce (extract-zos-path after) 'string)
		      (error "missing path"))
	    :query (if (contains-separator after "?")
		       (coerce (extract-query after) 'string)
		       NIL)
	    :fragment (if (contains-separator after "#")
			  (coerce
			   (extract-fragment after)
			   'string)
			  NIL))))))

;;; Parsing del Path di un URI che corrisponde allo Schema "zos".
(defun extract-zos-path (chars)
  (cond ((or (and (contains-separator chars "(")
		  (not (contains-separator chars ")")))
	     (and (not (contains-separator chars "("))
		  (contains-separator chars ")")))
	 (error "invalid sequence"))
        ((and (contains-separator chars "(")
	      (contains-separator chars ")"))
	 (progn
	   (setq id44-chars (id44 (rest chars)))
	   (setq id8-chars (id8 after))
	   (cond ((or (< (length id44-chars) 1)
		      (< (length id8-chars) 1))
		  (error "invalid sequence")))
	   (append id44-chars '(#\() id8-chars '(#\)))))
	(T (progn
	     (setq id44-chars (id44 (rest chars)))
	     (cond ((< (length id44-chars) 1)
		    (error "invalid sequence")))
	     id44-chars))))

;;; Estrazione ricorsiva dei caratteri di Id44.
(defun id44 (chars)
  (cond ((null chars) NIL)
	((or (string= (first chars) "(")
	     (string= (first chars) "?")
	     (string= (first chars) "#"))
	 NIL)
	((or (alphanumericp (first chars)) (string= (first chars) "."))
	 (progn
	   (defparameter after (rest chars))
	   (append (list (first chars)) (id44 (rest chars)))))
	(T (error "invalid id44 character"))))

;;; Estrazione ricorsiva dei caratteri di Id8.
(defun id8 (chars)
  (cond ((contains-separator chars "(")
	 (id8 (rest chars)))
	((string= (first chars) ")")
	 (progn
	   (defparameter after (rest chars))
	   NIL))
	((null chars) (error "invalid id8"))
	((alphanumericp (first chars))
	 (progn
	   (defparameter after (rest chars))
	   (append (list (first chars)) (id8 (rest chars)))))
	(T (error "invalid id8 character"))))

;;; Viene stabilito se il carattere passato in input corrisponde
;;; ad uno dei caratteri accettati dalla specifica corrente.
(defun caratterip (char)
  (or (alphanumericp char)
      (string= char " ")
      (string= char "-")
      (string= char ".")
      (string= char "_")
      (string= char "~")
      (string= char ":")
      (string= char "/")
      (string= char "?")
      (string= char "#")
      (string= char "[")
      (string= char "]")
      (string= char "@")
      (string= char "!")
      (string= char "$")
      (string= char "&")
      (string= char "'")
      (string= char "(")
      (string= char ")")
      (string= char "*")
      (string= char "+")
      (string= char ",")
      (string= char ";")
      (string= char "=")))

;;; Tramite questo subset di caratteri, usato in alcuni componenti
;;; dell'URI, viene stabilito se il carattere passato in input
;;; corrisponde ad uno dei caratteri accettati dalla specifica corrente.
(defun identificatorep (char)
  (or (alphanumericp char)
      (string= char " ")
      (string= char "-")
      (string= char ".")
      (string= char "_")
      (string= char "~")
      (string= char "[")
      (string= char "]")
      (string= char "!")
      (string= char "$")
      (string= char "&")
      (string= char "'")
      (string= char "(")
      (string= char ")")
      (string= char "*")
      (string= char "+")
      (string= char ",")
      (string= char ";")
      (string= char "=")))

;;;; urilib-parse.lisp ends here.
