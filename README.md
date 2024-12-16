# URI parser
Programming languages project consist in developing two libraries to process URI strings and convert them into data structures: one in Prolog and the other in either Common Lisp or Julia.

URI structure based on simplified version of RFC 3986.
URI are divided into **scheme, authority, path, query, and fragment**

## Group participants 
- Pomi Beatrice 914386
- Invernizzi Gloria 910243

# Schemi
## - ZOS
Lo schema **zos** descrive i nomi di data-sets su mainframes IBM. In questo caso il campo path ha una struttura diversa, che dovete controllare. 
Gli altri campi (userinfo, host, port, query, fragment), sono da riconoscere normalmente. 
path ::= <id44> ['(' <id8> ')' ]
id44 ::= ( < caratteri alfanumerici> | ‘.’)+
id8 ::= (< caratteri alfanumerici>)+

dove la lunghezza di **id44 è al massimo 44 e quella di id8 è al massimo 8.** Inoltre, id44 e id8 devono iniziare con un carattere alfabetico; id44 non può terminare con un ‘.’