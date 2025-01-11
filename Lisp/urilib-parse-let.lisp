;; -*- Mode: Lisp -*-

;; urilib-parse.lisp

;; Pomi Beatrice 914386
;; Invernizzi Gloria 910243

;; RFC3986 (https://datatracker.ietf.org/doc/html/rfc3986)

;; Struct contenente tutti i campi che compongono un URI.
(defstruct uri-struct schema userinfo host port path query fragment)

;; Ognuna di queste funzioni rappresenta l'estrazione di un singolo
;; componente dall'URISTRUCT.

(defun urilib-scheme (uri) (uri-struct-schema uri))
(defun urilib-userinfo (uri) (uri-struct-userinfo uri))
(defun urilib-host (uri) (uri-struct-host uri))
(defun urilib-port (uri) (uri-struct-port uri))
(defun urilib-path (uri) (uri-struct-path uri))
(defun urilib-query (uri) (uri-struct-query uri))
(defun urilib-fragment (uri) (uri-struct-fragment uri))

;; Struct per contenere i componenti dell'authority.
(defstruct authority-struct userinfo host port)

;; Metodo di debug per stampare l'URI passato in input sullo stream
;; di destinazione. Nel caso in cui stream non fosse passato alla
;; funzione, l'output verr� stampato sullo stream corrente.

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

;; Metodo principale per la scomposizione dei componenti dell'URI,
;; a partire dal parsing dello Schema, che restituisce infine
;; una URISTRUCT popolata con i suoi componenti.
(defun urilib-parse (uri)
  (when (stringp uri)
    ;; Converto la stringa in lista di caratteri
    (let* ((uri-chars (coerce uri 'list)))
      ;; Otteniamo lo schema e "after"
      (multiple-value-bind (schema after) (extract-schema uri-chars)
        (if (not (valid-schema-p (coerce schema 'string)))
            (error "Unrecognized schema"))
        ;; Caso "solo Schema"
        (if (null after)
            (make-uri-struct
             :schema (coerce schema 'string)
             :userinfo NIL
             :host NIL
             :port (get-port-from-schema schema)
             :path NIL
             :query NIL
             :fragment NIL)
          ;; Caso "sintassi speciale"
          (if (special-schema-p (coerce schema 'string))
              (extract-special-uri (coerce schema 'string) after)
            ;; Authority e rimanente URI
            (multiple-value-bind (authority after-authority)
                (extract-authority after schema)
              (let* ((userinfo
                      (if (equal (authority-struct-userinfo authority) NIL)
                          NIL
                        (coerce
                         (authority-struct-userinfo authority) 'string)))
                     (host (coerce
                            (authority-struct-host authority) 'string))
                     (port (parse-integer
                            (coerce
                             (authority-struct-port authority) 'string))))
                ;; estrazione del Path
                (multiple-value-bind (path after-path)
                    (extract-path after-authority)
                  ;; Estrazione della Query
                  (multiple-value-bind (query after-query)
                      (extract-query after-path)
                    ;; Estrazione del Fragment
                    (let* ((fragment (extract-fragment after-query)))
                      (make-uri-struct
                       :schema (coerce schema 'string)
                       :userinfo userinfo
                       :host host
                       :port port
                       :path (when path (coerce path 'string))
                       :query (when query (coerce query 'string))
                       :fragment (when fragment
                                   (coerce fragment 'string))))))))))))))

;; Estrazione dello schema
(defun extract-schema (chars)
  (cond
   ((null chars) (error "Schema is not valid"))
   ; Controlla se il primo carattere è ":"
   ((char= (first chars) #\:)
   ; Restituisce NIL come schema e il resto dei caratteri
    (values nil (rest chars)))
   (t (if (identificatorep (first chars))
          (multiple-value-bind (schema rest)
              (extract-schema (rest chars))
            (values (cons (first chars) schema) rest))
        (error "Invalid schema character")))))

;; Estrazione authority
(defun extract-authority (chars schema)
  (cond
   ;; Caso in cui l'Authority è presente
   ((and (string= (first chars) "/")
         (string= (second chars) "/"))
    (if (or
         (null (third chars))
         (string= (third chars) "?")
         (string= (third chars) "#"))
        (error "Undefined authority"))
    (multiple-value-bind (authority after)
    ; Estrazione authority e caratteri rimanenti
        (extract-authority-chars (rest (rest chars)))
      (let* ((userinfo (if (contains-separator authority "@")
                           (extract-userinfo authority)
                         NIL))
             (host (extract-host authority))
             (port (if (contains-separator authority ":")
                       (let ((extracted-port (extract-port authority)))
                         (if (null extracted-port)
                             (error "invalid port")
                           extracted-port))
                     (get-port-from-schema schema))))
        (values (make-authority-struct
                 :userinfo userinfo
                 :host host
                 :port port) after))))
   ;; Caso schema speciale "news"
   ((string= (coerce schema 'string) "news")
    (multiple-value-bind (authority after)
        (extract-authority-chars chars)
      (values (make-authority-struct
               :userinfo NIL
               :host (extract-host authority)
               :port 80) after)))
   ;; Caso schema speciale "mailto"
   ((string= (coerce schema 'string) "mailto")
    (multiple-value-bind (authority after)
        (extract-authority-chars chars)
      (values (make-authority-struct
               :userinfo (extract-userinfo authority)
               :host (when (contains-separator authority "@")
                           (extract-host authority))
               :port 80) after)))
   ;; Caso in cui l'Authority non � presente
   ((or (and (string= (first chars) "/")
             (not (string= (second chars) "/")))
        (string= (first chars) "?")
        (string= (first chars) "#")
        (alpha-char-p (first chars)))
    (let ((after chars))
      (values (make-authority-struct
               :userinfo NIL
               :host NIL
               :port (get-port-from-schema schema)) after)))
   ;; Caso di errore
   (T (error "Authority not recognized"))))

;; Estrazione ricorsiva dei caratteri che compongono l'Authority.
;; Ritorna anche il restante URI
(defun extract-authority-chars (chars)
  (cond
   ;; ".." nell'authority non � accettato
   ((and (string= (first chars) ".")
         (string= (second chars) "."))
    (error "Invalid host syntax"))
   ;; Se troviamo uno dei separatori che termina l'authority
   ((or (string= (first chars) "/")
        (string= (first chars) "?")
        (string= (first chars) "#"))
    (values NIL chars))
   ;; La lista di caratteri � vuota
   ((null chars)
    (values NIL NIL))
   
   ;; Aggiungiamo il primo carattere all'authority e esegui il passo ricorsivo
   (T
    (multiple-value-bind (authority after)
        ;; Esamino i restanti caratteri
        (extract-authority-chars (rest chars))
      (values (cons (first chars) authority) after)))))

;; Funzione di utility che determina la presenza o meno di un certo
;; carattere separatore passato in input.
(defun contains-separator (chars separator)
  (cond ((null chars) NIL)
	((string= (first chars) separator) T)
	(T (contains-separator (rest chars) separator))))

;; Estrazione ricorsiva dei caratteri che compongono lo Userinfo.
(defun extract-userinfo (chars)
  (cond ((null chars) NIL)
	((string= (first chars) "@") NIL)
	(T (if (identificatorep (first chars))
	       (append (list (first chars))
		       (extract-userinfo (rest chars)))
             (error "invalid userinfo character")))))

;; Verifica se l'host � un indirizzo IP valido
;; Analizza Ip ottetto per ottetto, ottetti al pi� di tre cifre separati da  '.'
(defun valid-ipv4-p (chars)
  (let* ((first (parse-octet chars))
         (after-octet1 (when (not (null first))
                         (nthcdr (cdr first) chars))))
    ;; Se � valido il primo ottetto analizzo il successivo
    ;; Ottetti separati da '.'
    (when (and first (string= (first after-octet1) "."))
      (let* ((second (parse-octet (rest after-octet1)))
             (after-octet2 (when (not (null second))
                             (nthcdr (cdr second) (rest after-octet1)))))
        (when (and second (string= (first after-octet2) "."))
          (let* ((third (parse-octet (rest after-octet2)))
                 (after-octet3 (when (not (null third))
                                 (nthcdr (cdr third) (rest after-octet2)))))
            (when (and third
                       (string= (first after-octet3) "."))
              (let* ((fourth
                      (parse-octet (rest after-octet3))))
                (when (not (null fourth))
                  (format nil "~a.~a.~a.~a"
                          (car first)
                          (car second)
                          (car third)
                          (car fourth)))))))))))

(defun parse-octet (chars)
; Prende il primo carattere
  (let* ((digit1 (digit-char-p (first chars))))
    (when digit1
      ;; Se il primo carattere � una cifra analizza gli altri caratteri
      ;; Valido se la lista ha pi� di due caratteri e il secondo � un numero
      ;; digit2 è la conversione del secondo carattere in numero, se è valido
      (let* ((digit2 (and
                      (rest chars)
                      ;; la AND restituisce la conversione char -> integer
                      (digit-char-p (second chars))))
             (digit3 (and
                      digit2
                      (rest (rest chars))
                      (digit-char-p (third chars)))))
        (let ((value (cond
                      ((and digit3) (+ (* 100 (or digit1 0))
                                       (* 10 (or digit2 0))
                                       (or digit3 0)))
                      ((and digit2) (+ (* 10 (or digit1 0))
                                       (or digit2 0)))
                      ((and digit1) (or digit1 0))
                      (T NIL))))
          (cond
           ; Ottetto con 3 cifre
           ((and digit3 (<= value 255)) (cons value 3))
           ; Ottetto con 2 cifre
           ((and digit2 (<= value 255)) (cons value 2))
           ; Ottetto con 1 cifra
           ((and (not (null value)) (<= value 255)) (cons value 1))
           ; Valore non valido
           (t nil)))))))

;; Riconosce stringhe che iniziano con una lettera oppure indirizzi IPv4 validi
(defun extract-host (chars)
  (cond
   ;; Caso: la stringa inizia con una lettera
   ((and (not (null chars)) (alpha-char-p (first chars)))
    (extract-host-ricorsiva chars))
   ;; Caso: la stringa rappresenta un indirizzo IPv4 valido
   ((valid-ipv4-p chars)
    (valid-ipv4-p chars))
   (T (error "invalid host"))))

;; Estrazione ricorsiva dei caratteri che compongono l'Host.
(defun extract-host-ricorsiva (chars)
  (cond ((and (string= (first chars) "@") (alpha-char-p (second chars)))
         (extract-host-ricorsiva (rest chars)) )
        ((contains-separator chars "@")
	 (extract-host-ricorsiva (rest chars)))
	((string= (first chars) ":") NIL)
	((null chars) NIL)
	(T (if (identificatorep (first chars))
	       (append (list (first chars))
		       (extract-host-ricorsiva (rest chars)))
             (error "invalid host character")))))

;; Estrazione ricorsiva dei caratteri che compongono Port
(defun extract-port (chars)
  (cond ((contains-separator chars ":")
	 (extract-port (rest chars)))
	((null chars) NIL)
	(T (if (numberp (digit-char-p (first chars)))
	       (append (list (first chars))
		       (extract-port (rest chars)))
             (error "invalid port character")))))

;; Estrazione ricorsiva dei caratteri che compongono il Path,
;; con controllo sull'esistenza di "/" come primo carattere
(defun extract-path (chars)
  (cond ((null chars) (values NIL NIL))
        ((string= (first chars) "/")
         (extract-path-chars (rest chars)))
        ((or (string= (first chars) "?") (string= (first chars) "#"))
         (values NIL chars))
        ((alpha-char-p (first chars))
         (extract-path-chars chars))))

;; Estrae ricorsivamente i caratteri del path, restituisce il path e il resto
(defun extract-path-chars (chars)
  (cond
   ((null chars)
    (values NIL NIL))
   ;; Se si incontra un separatore di path per query ? o fragment #
   ((or (string= (first chars) "?")
        (string= (first chars) "#"))
    (values NIL chars))
   ;; Caso generale verifica i caratteri e continua la ricorsione
   (T
    (if (or (identificatorep (first chars))
            (string= (first chars) "/"))
        (multiple-value-bind (path rest)
            (extract-path-chars (rest chars))
          ;; Restituisce il path e l'uri rimanente
          (values (cons (first chars) path) rest))
      (error "Invalid path character")))))

;; Estrazione ricorsiva dei caratteri che compongono la Query
(defun extract-query (chars)
  (cond ((null chars) (values NIL NIL))
        ((string= (first chars) "#")
         (values NIL chars))
        ((string= (first chars) "?")
	 (extract-query-chars (rest chars)))))

(defun extract-query-chars (chars)
  (cond ((null chars) (values NIL NIL))
        ;; Se si incontra il separatore per fragment
	((string= (first chars) "#")
	 (values NIL chars))
        ;; Caso generale chiamata ricorsiva
	(T (if (or (identificatorep (first chars))
                   (string= (first chars) "="))
               (multiple-value-bind (query rest)
                   (extract-query-chars (rest chars))
                 ;; Restituisce la query e l'uri rimanente
                 (values (cons (first chars) query) rest))
             (error "Invalid query character")))))

;; Estrazione ricorsiva dei caratteri che compongono il Fragment
(defun extract-fragment (chars)
  (cond ((string= (first chars) "#")
	 (extract-fragment-chars (rest chars)))))

(defun extract-fragment-chars (chars)
  (cond ((null chars) NIL)
	(T (if (identificatorep (first chars))
	       (append (list (first chars))
		       (extract-fragment-chars (rest chars)))
             (error "invalid fragment character")))))

;; Parsing di URI nel caso di Schema caratterizzati da "sintassi speciali"
(defun extract-special-uri (schema chars)
  (cond
   ;; Parsing mailto
   ((string= schema "mailto")
    (multiple-value-bind (authority)
        (extract-authority chars schema)
      (make-uri-struct
       :schema schema
       :userinfo (when (authority-struct-userinfo authority)
                   (coerce (authority-struct-userinfo authority) 'string))
       :host (when (authority-struct-host authority)
               (coerce (authority-struct-host authority) 'string))
       :port 80)))
   ;; Parsing news
   ((string= schema "news")
    (multiple-value-bind (authority)
        (extract-authority chars schema)
      (let ((host (authority-struct-host authority)))
        (when (or (null host)
                  (contains-separator (coerce host 'list) "@")
                  (contains-separator (coerce host'list) ":"))
          (error "invalid host"))
        (make-uri-struct
         :schema schema
         :host (coerce host 'string)
         :port 80))))
   ;; Parsing tel e fax
   ((or (string= schema "tel") (string= schema "fax"))
    (make-uri-struct
     :schema schema
     :userinfo (cond ((contains-separator chars "@")
                      (error "invalid userinfo"))
                     ((string= (first chars) "+")
                      (coerce (append '(#\+)
                                      (extract-userinfo (rest chars)))
                              'string))
                     (T (coerce (extract-userinfo chars) 'string)))
     :port 80))
   ;; Parsing zos
   ((string= schema "zos")
    (multiple-value-bind (authority after-authority)
        (extract-authority chars schema)
      (let* ((userinfo
              (when (authority-struct-userinfo authority)
                (coerce (authority-struct-userinfo authority) 'string)))
             (host (when (authority-struct-host authority)
                     (coerce (authority-struct-host authority) 'string)))
             (port (if (authority-struct-port authority)
                       (parse-integer
                        (coerce (authority-struct-port authority) 'string))
                     NIL)))
        ;; Estrazione del Path
        (multiple-value-bind (path after-path)
            (extract-zos-path after-authority)
          ;; Estrazione della Query
          (multiple-value-bind (query after-query)
              (extract-query after-path)
            ;; Estrazione del Fragment
            (let* ((fragment (extract-fragment after-query)))
              (make-uri-struct
               :schema (coerce schema 'string)
               :userinfo userinfo
               :host host
               :port port
               :path (when path (coerce path 'string))
               :query (when query (coerce query 'string))
               :fragment (when fragment
                           (coerce fragment 'string)))))))))))

;; Estrazione ricorsiva dei caratteri che compongono il Path,
;; con controllo sull'esistenza di "/" come primo carattere.
(defun extract-zos-path (chars)
  (cond ((null chars) (values NIL NIL))
        ((string= (first chars) "/")
         (extract-zos-path-chars (rest chars)))
        ((alpha-char-p (first chars))
         (extract-zos-path-chars chars))))

;; Parsing del Path di un URI che corrisponde allo Schema "zos".
(defun extract-zos-path-chars (chars)
  (cond
   ((null chars)
    (values NIL NIL))
   ((or (and (contains-separator chars "(")
             (not (contains-separator chars ")")))
        (and (not (contains-separator chars "("))
             (contains-separator chars ")")))
    (error "invalid sequence"))
   ((and (contains-separator chars "(")
         (contains-separator chars ")"))
    (multiple-value-bind (id44-chars rest-after-id44) (id44 chars)
      (multiple-value-bind (id8-chars rest-after-id8) (id8 rest-after-id44)
        (cond ((or (< (length id44-chars) 1)
                   (> (length id44-chars) 44)
                   (< (length id8-chars) 1)
                   (> (length id8-chars) 8))
               (error "invalid sequence")))
        (values (append id44-chars '(#\() id8-chars '(#\)))
                rest-after-id8))))
   ((or (string= (first chars) "#") (string= (first chars) "?"))
    (values NIL chars))
   (T (multiple-value-bind (id44-chars rest-after-id44) (id44 chars)
        (when (< (length id44-chars) 1)
          (error "invalid sequence"))
        (values id44-chars rest-after-id44)))))

;; Controllo sintassi id44
(defun id44 (chars)
  (cond
   ((null chars)
    (values NIL chars))
   ;; primo carattere deve essere una letter
   ((alpha-char-p (first chars))
    (id44-chars chars))
   (T (error "Invalid Id44 chars"))))

;; Estrazione ricorsiva dei caratteri di Id44
(defun id44-chars (chars)
  (cond
   ((null chars)
    (values NIL chars))
   ((or (string= (first chars) "(")
        (string= (first chars) "?")
        (string= (first chars) "#"))
    (values NIL chars))
   ((or (alphanumericp (first chars)) (string= (first chars) "."))
    (multiple-value-bind (id44 after-id44) (id44-chars (rest chars))
      (values (append (list (first chars)) id44) after-id44)))
   (T (error "invalid id44 character"))))

;; Controllo sintassi id8
(defun id8 (chars)
  (cond
   ((null chars)
    (values NIL chars))
   ;; primo carattere deve essere una letter
   ((alpha-char-p (first chars))
    (id8-chars chars))
   (T (error "Invalid Id8 chars"))))

;; Estrazione ricorsiva dei caratteri di Id8.
(defun id8-chars (chars)
  (cond
   ((contains-separator chars "(")
    (id8 (rest chars)))
   ((string= (first chars) ")")
    (values NIL (rest chars)))
   ((null chars)
    (error "invalid id8"))
   ((alphanumericp (first chars))
    (multiple-value-bind (id8 after-id8) (id8-chars (rest chars))
      (values (append (list (first chars)) id8) after-id8)))
   (T (error "invalid id8 character"))))


;; Viene stabilito se il carattere passato in input corrisponde
;; ad uno dei caratteri accettati dalla specifica corrente.
(defun identificatorep (char)
  (or (alphanumericp char)
      (string= char ".")
      (string= char "_")
      (string= char "-")))

;; Predicato per definire se schema ha "sintassi speciale"
(defun special-schema-p (schema)
  (or (string= schema "mailto")
      (string= schema "news")
      (string= schema "tel")
      (string= schema "fax")
      (string= schema "zos")))

;; Predicato per stabilire se lo schema � valido
(defun valid-schema-p (schema)
  (or (string= schema "http")
      (string= schema "https")
      (string= schema "ftp")
      (special-schema-p schema)))

;; Porta di default secondo gli schemi standard
(defun get-port-from-schema (schema)
  (cond ((string= (coerce schema 'string) "https")
         "443")
        ((string= (coerce schema 'string) "ftp")
         "21")
        (T "80")))

;; urilib-parse.lisp ends here.
