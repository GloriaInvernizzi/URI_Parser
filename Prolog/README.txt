# Pomi Beatrice 914386
# Invernizzi Gloria 910243

# URI Parser

Un URI (Uniform Resource Identifier) è una stringa che identifica una risorsa 
univocamente, le cui caratteristiche sono descritte dalla RFC3986 
(https://datatracker.ietf.org/doc/html/rfc3986). 
Per poter fare in modo che la risorsa venga correttamente riconosciuta e 
raggiunta, si utilizzano diversi componenti variabili che vanno a comporre un 
URI.

Il componente primario di un URI è lo **schema**, che stabilisce la sintassi 
che deve essere implementata; mediante schema diversi è quindi possibile 
identificare risorse di natura diversa, stabilendo quali componenti utilizzare 
e quale sintassi specifica dovrà seguire il singolo componente.

I componenti di un URI sono:

- Schema
- Userinfo
- Host
- Port
- Path
- Query
- Fragment

Questa libreria, realizzata in Prolog, che adotta il **paradigma di 
programmazione logica**, mira al riconoscimento di un URI secondo una sintassi 
semplificata, ed alla scomposizione di questo nei componenti che lo compongono.

### Utilizzo

La libreria viene eseguita tramite il predicato urilib_parse/2, che restituisce 
un URI scomposto a partire da una stringa contenente un URI in formato 
testuale; per fare ciò, viene prima riconosciuto lo schema utilizzato tramite 
il predicato schema/3, poi vengono impiegati diversi predicati, ognuno in grado 
di eseguire il parse di un singolo componente. 

A seconda dello schema utilizzato, se tra quelli speciali o quelli generali, 
vengono applicate regole sintattiche differenti, fino ad arrivare alla 
scomposizione della stringa ed alla restituzione del risultato.

Sono disponibili due predicati, urilib_display/1 e urilib_display/2, in grado 
di stampare su uno stream di destinazione. Nel caso di urilib_display/2, 
lo stream deve essere passato come argomento, mentre nel caso di 
urilib_display/1 viene usato lo stream corrente per richiamare urilib_display/2.