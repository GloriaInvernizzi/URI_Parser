# Pomi Beatrice 914386
# Invernizzi Gloria 910243

# URI Parser

Un URI (Uniform Resource Identifier) è una stringa che identifica una risorsa univocamente, le cui caratteristiche sono descritte dalla [RFC3986](https://datatracker.ietf.org/doc/html/rfc3986). Per poter fare in modo che la risorsa venga correttamente riconosciuta e raggiunta, si utilizzano diversi componenti variabili che vanno a comporre un URI.

Il componente primario di un URI è lo schema, che stabilisce la sintassi che deve essere implementata; mediante schema diversi è quindi possibile identificare risorse di natura diversa, stabilendo quali componenti utilizzare e quale sintassi specifica dovrà seguire il singolo componente.

I componenti di un URI sono:

- Schema
- Userinfo
- Host
- Port
- Path
- Query
- Fragment

Questa libreria, realizzata in Common Lisp, adotta il paradigma di programmazione funzionale e mira al riconoscimento di un URI secondo una sintassi semplificata, ed alla scomposizione di questo nei componenti che lo compongono.

### Utilizzo

La libreria viene eseguita tramite la funzione URILIB-PARSE, che restituisce un URI-STRUCT che contiene i componenti di un URI scomposto, a partire da una stringa contenente un URI in formato testuale; per fare ciò, viene prima riconosciuto lo schema utilizzato tramite la funzione EXTRACT-SCHEMA, poi vengono impiegati diversi predicati, ognuno in grado di eseguire il parse di un singolo componente.

A seconda dello schema utilizzato, se tra quelli speciali o quelli generali, vengono applicate regole sintattiche differenti, fino ad arrivare alla scomposizione della stringa ed alla restituzione del risultato.

È disponibile una funzione per ogni componente dell'URI presente in URI-STRUCT, in grado di restituire il singolo componente dallo struct:

- URILIB-SCHEME
- URILIB-USERINFO
- URILIB-HOST
- URILIB-PORT
- URILIB-PATH
- URILIB-QUERY
- URILIB-FRAGMENT

È disponibile la funzione URILIB-DISPLAY, in grado di stampare su uno stream di destinazione. È possibile passare a questa funzione uno stream, in quel caso l'output verrà stampato sullo stream in input. Nel caso non venisse specificato uno stream, allora l'output verrà stampato sullo stream corrente.