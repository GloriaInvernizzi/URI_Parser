%%%% -*- Mode: Prolog -*-
%%%% urilib-parse.pl

%%%% Pomi Beatrice 914386
%%%% Invernizzi Gloria 910243

%%%% RFC3986 (https://datatracker.ietf.org/doc/html/rfc3986)

%   urilib_parse(URIString, URI)
urilib_parse(URIString, URI) :-
    % Trasforma la stringa URISTRING in una lista, ad ogni carattere associa
    % il codice ascii e mette il risultato in Codes
    string(URIString),
    % atoms_code è una funzione valida per coerce in prolog (casting)
    atom_codes(URIString, Codes),
    schema(Codes, SchemaCodes, AfterSchema),
    % trasforma lista in string, inverso, incognita in questo caso è Scheme
    atom_codes(Schema, SchemaCodes),
    % parser URI based to uri scheme structure
    parse_uri_with_schema(Schema, AfterSchema, URI).

%   schema(Codes, Schema, After)
%   Definition of the scheme (http, https, ftp, mailto, file, zos ...)

% base
schema([C | Codes], Schema, After) :-
    C = 58, % Codice ASCII per :
    !,
    % Scheme è tutto quello che c'è prima di : after è tutto quello che c'è dopo
    Schema = [],
    After = Codes.

% induttivo
schema([C | Codes], Schema, After) :-
    identificatore(C), % if C is an acetpet char
    !,
    schema(Codes, S, After),
    Schema = [C | S].

schema(Schema) :- Schema = 'http'.
schema(Schema) :- Schema = 'https'.
schema(Schema) :- Schema = 'ftp'.

%   parse_uri_with_schema(Schema, AfterSchema, URI)
%
%   A partire dallo Schema passato in input, tramite l'applicazione delle
%   regole sintattiche associate vengono estratti i componenti dell'URI
%   mancanti da quello che si trova dopo lo Schema, passato come
%   AfterSchema.

%   Uno Schema seguito dal nulla è un URI valido.
parse_uri_with_schema(Schema, [], URI) :-
    schema(Schema),
    !,
    URI = uri(Schema, [], [], 80, [], [], []).

%   Parse di un URI contenente solo Userinfo secondo il formato dello Schema
%   mailto.
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    Schema = 'mailto',
    plain_userinfo(AfterSchema, U, []),
    !,
    atom_codes(Userinfo, U),
    URI = uri('mailto', Userinfo, [], 80, [], [], []).

%   Parse di un URI contenente Userinfo e Host secondo il formato dello Schema
%   mailto.
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    Schema = 'mailto',
    !,
    plain_userinfo(AfterSchema, U, AfterUserinfo),
    host(AfterUserinfo, H, []),
    atom_codes(Userinfo, U),
    atom_codes(Host, H),
    URI = uri('mailto', Userinfo, Host, 80, [], [], []).

%   Parse di un URI contenente solo Userinfo secondo il formato degli Schema
%   tel e fax.
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    Schema = 'tel',
    !,
    plain_userinfo(AfterSchema, U, []),
    atom_codes(Userinfo, U),
    URI = uri(Schema, Userinfo, [], 80, [], [], []).

parse_uri_with_schema(Schema, AfterSchema, URI) :-
    Schema = 'fax',
    !,
    plain_userinfo(AfterSchema, U, []),
    atom_codes(Userinfo, U),
    URI = uri(Schema, Userinfo, [], 80, [], [], []).

%   Parse di un URI contenente solo l'Host secondo il formato dello Schema news.
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    Schema = 'news',
    host(AfterSchema, H, []),
    !,
    atom_codes(Host, H),
    URI = uri('news', [], Host, 80, [], [], []).

parse_uri_with_schema(Schema, AfterSchema, URI) :-
    Schema = 'news',
    !,
    ip(AfterSchema, H, []),
    atom_codes(Host, H),
    URI = uri('news', [], Host, 80, [], [], []).

%   Parse di un URI senza Path, Fragment e Query secondo il formato
%   dello Schema zos.
%   parse_uri_with_schema(String, List, struct)
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    Schema = 'zos',
    authority(Schema, AfterSchema, Userinfo, Host, Port, []),
    !,
    URI = uri(Schema, Userinfo, Host, Port, [], [], []).

%   Parse di un URI senza Path secondo il formato dello Schema zos.
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    Schema = 'zos',
    authority(Schema, AfterSchema, Userinfo, Host, Port, AfterAuthority),
    path(AfterAuthority, [], AfterPath),
    query(AfterPath, QueryCodes, AfterQuery),
    fragment(AfterQuery, FragmentCodes, []),
    !,
    atom_codes(Query, QueryCodes),
    atom_codes(Fragment, FragmentCodes),
    URI = uri(Schema, Userinfo, Host, Port, [], Query, Fragment).

%   Parse di un URI senza Path e Fragment secondo il formato
%   dello Schema zos.
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    Schema = 'zos',
    authority(Schema, AfterSchema, Userinfo, Host, Port, AfterAuthority),
    path(AfterAuthority, [], AfterPath),
    query(AfterPath, QueryCodes, []),
    !,
    atom_codes(Query, QueryCodes),
    URI = uri(Schema, Userinfo, Host, Port, [], Query, []).

%   Parse di un URI senza Path e Query secondo il formato
%   dello Schema zos.
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    Schema = 'zos',
    authority(Schema, AfterSchema, Userinfo, Host, Port, AfterAuthority),
    path(AfterAuthority, [], AfterPath),
    fragment(AfterPath, FragmentCodes, []),
    !,
    atom_codes(Fragment, FragmentCodes),
    URI = uri(Schema, Userinfo, Host, Port, [], [], Fragment).

%   Parse di un URI senza Fragment secondo il formato dello Schema zos.
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    Schema = 'zos',
    authority(Schema, AfterSchema, Userinfo, Host, Port, AfterAuthority),
    path(AfterAuthority, PathCodes, AfterPath),
    zos_path(PathCodes),
    query(AfterPath, QueryCodes, []),
    !,
    atom_codes(Path, PathCodes),    % traformate in stringhe
    atom_codes(Query, QueryCodes),  % trasformate in stringhe
    URI = uri(Schema, Userinfo, Host, Port, Path, Query, []).

%   Parse di un URI senza Query secondo il formato dello Schema zos.
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    Schema = 'zos',
    authority(Schema, AfterSchema, Userinfo, Host, Port, AfterAuthority),
    path(AfterAuthority, PathCodes, AfterPath),
    zos_path(PathCodes),
    fragment(AfterPath, FragmentCodes, []),
    !,
    atom_codes(Path, PathCodes),
    atom_codes(Fragment, FragmentCodes),
    URI = uri(Schema, Userinfo, Host, Port, Path, [], Fragment).

%   Parse di un URI senza Query e Fragment secondo il formato dello Schema zos.
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    Schema = 'zos',
    authority(Schema, AfterSchema, Userinfo, Host, Port, AfterAuthority),
    path(AfterAuthority, PathCodes, []),
    zos_path(PathCodes),
    !,
    atom_codes(Path, PathCodes),
    URI = uri(Schema, Userinfo, Host, Port, Path, [], []).

%   Parse di un URI completo secondo il formato dello Schema zos.
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    Schema = 'zos',
    !,
    authority(Schema, AfterSchema, Userinfo, Host, Port, AfterAuthority),
    path(AfterAuthority, PathCodes, AfterPath),
    zos_path(PathCodes),
    query(AfterPath, QueryCodes, AfterQuery),
    fragment(AfterQuery, FragmentCodes, []),
    atom_codes(Path, PathCodes),
    atom_codes(Query, QueryCodes),
    atom_codes(Fragment, FragmentCodes),
    URI = uri(Schema, Userinfo, Host, Port, Path, Query, Fragment).

%   Parse di un URI senza Path secondo il formato generico.
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    schema(Schema),
    authority(Schema, AfterSchema, Userinfo, Host, Port, AfterAuthority),
    path(AfterAuthority, [], AfterPath),
    query(AfterPath, QueryCodes, AfterQuery),
    fragment(AfterQuery, FragmentCodes, []),
    !,
    atom_codes(Query, QueryCodes),
    atom_codes(Fragment, FragmentCodes),
    URI = uri(Schema, Userinfo, Host, Port, [], Query, Fragment).

%   Parse di un URI senza Fragment secondo il formato generico.
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    schema(Schema),
    authority(Schema, AfterSchema, Userinfo, Host, Port, AfterAuthority),
    path(AfterAuthority, PathCodes, AfterPath),
    query(AfterPath, QueryCodes, []),
    !,
    atom_codes(Path, PathCodes),
    atom_codes(Query, QueryCodes),
    URI = uri(Schema, Userinfo, Host, Port, Path, Query, []).

%   Parse di un URI senza Query secondo il formato generico.
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    schema(Schema),
    authority(Schema, AfterSchema, Userinfo, Host, Port, AfterAuthority),
    path(AfterAuthority, PathCodes, AfterPath),
    fragment(AfterPath, FragmentCodes, []),
    !,
    atom_codes(Path, PathCodes),
    atom_codes(Fragment, FragmentCodes),
    URI = uri(Schema, Userinfo, Host, Port, Path, [], Fragment).

%   Parse di un URI senza Query e Fragment secondo il formato generico.
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    schema(Schema),
    authority(Schema, AfterSchema, Userinfo, Host, Port, AfterAuthority),
    path(AfterAuthority, PathCodes, []),
    !,
    atom_codes(Path, PathCodes),
    URI = uri(Schema, Userinfo, Host, Port, Path, [], []).

%   Parse di un URI senza Path e Fragment secondo il formato generico.
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    schema(Schema),
    authority(Schema, AfterSchema, Userinfo, Host, Port, AfterAuthority),
    path(AfterAuthority, [], AfterPath),
    query(AfterPath, QueryCodes, []),
    !,
    atom_codes(Query, QueryCodes),
    URI = uri(Schema, Userinfo, Host, Port, [], Query, []).

%   Parse di un URI senza Path e Query secondo il formato generico.
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    schema(Schema),
    authority(Schema, AfterSchema, Userinfo, Host, Port, AfterAuthority),
    path(AfterAuthority, [], AfterPath),
    fragment(AfterPath, FragmentCodes, []),
    !,
    atom_codes(Fragment, FragmentCodes),
    URI = uri(Schema, Userinfo, Host, Port, [], [], Fragment).

%   Parse di un URI senza Path, Query e Fragment secondo il formato generico,
%   terminato dal carattere "/".
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    schema(Schema),
    authority(Schema, AfterSchema, Userinfo, Host, Port, AfterAuthority),
    path(AfterAuthority, [], []),
    !,
    URI = uri(Schema, Userinfo, Host, Port, [], [], []).

%   Parse di un URI senza Path, Query e Fragment secondo il formato generico.
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    schema(Schema),
    authority(Schema, AfterSchema, Userinfo, Host, Port, []),
    !,
    URI = uri(Schema, Userinfo, Host, Port, [], [], []).

%   Parse di un URI completo secondo il formato generico.
parse_uri_with_schema(Schema, AfterSchema, URI) :-
    schema(Schema),
    authority(Schema, AfterSchema, Userinfo, Host, Port, AfterAuthority),
    path(AfterAuthority, PathCodes, AfterPath),
    query(AfterPath, QueryCodes, AfterQuery),
    fragment(AfterQuery, FragmentCodes, []),
    !,
    atom_codes(Path, PathCodes),
    atom_codes(Query, QueryCodes),
    atom_codes(Fragment, FragmentCodes),
    URI = uri(Schema, Userinfo, Host, Port, Path, Query, Fragment).


%   authority(Codes, Userinfo, Host, Port, After)
%
%   I componenti dell'Authority vengono estratti dai codici passati in input.

%   Parse di Authority completa.
authority(_, [C1, C2 | Chars], Userinfo, Host, Port, After) :-
    C1 = 47, % Codice ASCII per /
    C2 = 47, % Codice ASCII per /
    userinfo(Chars, U, AfterUserinfo),
    ip(AfterUserinfo, H, AfterHost),
    port(AfterHost, P, After),
    !,
    atom_codes(Userinfo, U),
    atom_codes(Host, H),
    number_codes(Port, P).

authority(_, [C1, C2 | Chars], Userinfo, Host, Port, After) :-
    C1 = 47, % Codice ASCII per /
    C2 = 47, % Codice ASCII per /
    userinfo(Chars, U, AfterUserinfo),
    host(AfterUserinfo, H, AfterHost),
    port(AfterHost, P, After),
    !,
    atom_codes(Userinfo, U),
    atom_codes(Host, H),
    number_codes(Port, P).

%   Parse di Authority senza Userinfo.
authority(_, [C1, C2 | Chars], Userinfo, Host, Port, After) :-
    C1 = 47, % Codice ASCII per /
    C2 = 47, % Codice ASCII per /
    ip(Chars, H, AfterHost),
    port(AfterHost, P, After),
    !,
    Userinfo = [],
    atom_codes(Host, H),
    number_codes(Port, P).

authority(_, [C1, C2 | Chars], Userinfo, Host, Port, After) :-
    C1 = 47, % Codice ASCII per /
    C2 = 47, % Codice ASCII per /
    host(Chars, H, AfterHost),
    port(AfterHost, P, After),
    !,
    Userinfo = [],
    atom_codes(Host, H),
    number_codes(Port, P).

%   Parse di Authority senza Port, sostituita quindi da quella di default.
authority(Schema, [C1, C2 | Chars], Userinfo, Host, Port, After) :-
    C1 = 47, % Codice ASCII per /
    C2 = 47, % Codice ASCII per /
    userinfo(Chars, U, AfterUserinfo),
    ip(AfterUserinfo, H, After),
    !,
    atom_codes(Userinfo, U),
    atom_codes(Host, H),
    port(Schema, Port).

authority(Schema, [C1, C2 | Chars], Userinfo, Host, Port, After) :-
    C1 = 47, % Codice ASCII per /
    C2 = 47, % Codice ASCII per /
    userinfo(Chars, U, AfterUserinfo),
    host(AfterUserinfo, H, After),
    !,
    atom_codes(Userinfo, U),
    atom_codes(Host, H),
    port(Schema, Port).

%   Parse di Authority senza Userinfo e Port. La Port viene sostituita quindi
%   con quella di default.
authority(Schema, [C1, C2 | Chars], Userinfo, Host, Port, After) :-
    C1 = 47, % Codice ASCII per /
    C2 = 47, % Codice ASCII per /
    ip(Chars, H, After),
    !,
    Userinfo = [],
    atom_codes(Host, H),
    port(Schema, Port).

authority(Schema, [C1, C2 | Chars], Userinfo, Host, Port, After) :-
    C1 = 47, % Codice ASCII per /
    C2 = 47, % Codice ASCII per /
    host(Chars, H, After),
    !,
    Userinfo = [],
    atom_codes(Host, H),
    port(Schema, Port).

%   Parse di Authority non presente, quindi si passa direttamente al
%   Path che ha il separatore /.
authority(Schema,[C1, C2 | Chars], Userinfo, Host, Port, After) :-
    C1 = 47,  % Codice ASCII per /
    C2 \= 47, % Codice ASCII per /
    !,
    Userinfo = [],
    Host = [],
    port(Schema, Port),
    After = [C1, C2 | Chars].

%   Parse di Authority non presente, quindi si passa direttamente al
%   Path che non ha il separatore /.
authority(Schema, [C | Chars], Userinfo, Host, Port, After) :-
    identificatore(C),
    !,
    Userinfo = [],
    Host = [],
    port(Schema, Port),
    After = [C | Chars].

%   userinfo(Codes, Userinfo, After)
%
%   Lo Userinfo viene estratto dai codici passati in input.

userinfo([C | Codes], Userinfo, After) :-
    C = 64, % Codice ASCII per @
    !,
    Userinfo = [],
    After = Codes.

userinfo([C | Codes], Userinfo, After) :-
    identificatore(C),
    !,
    userinfo(Codes, U, After),
    Userinfo = [C | U].

%   plain_userinfo(Codes, Userinfo, After)
%
%   Lo Userinfo viene estratto dai codici passati in input. In questo caso
%   lo Userinfo può essere l'unico componente presente all'interno di un
%   URI

plain_userinfo([], Userinfo, After) :-
    Userinfo = [],
    After = [].

plain_userinfo([C | Codes], Userinfo, After) :-
    C = 64, % Codice ASCII per @
    !,
    Userinfo = [],
    After = Codes.

plain_userinfo([C | Codes], Userinfo, After) :-
    identificatore(C),
    !,
    plain_userinfo(Codes, U, After),
    Userinfo = [C | U].

%   host(Codes, Host, After)
%
%   L'Host viene estratto dai codici passati in input.
%   Fondamentale che il primo carattere sia una lettera

host([First | Codes], Host, After) :-
    is_letter(First),
    !,
    identificatore_host(Codes, P, After),
    Host = [First | P].

%   identificatore_host(Codes, Host, After)
%
%   Verifica che la restante parte del Codes (tolto il primo carattere)
%   sia una stringa valida

identificatore_host([], Host, After) :-
    Host = [],
    After = [].

identificatore_host([C | Codes], Host, After) :-
    C = 58,  % Codice ASCII per :
    !,
    Host = [],
    After = [C | Codes].

identificatore_host([C | Codes], Host, After) :-
    C = 47, % Codice ASCII per /
    !,
    Host = [],
    After = [C | Codes].

identificatore_host([C | Codes], Host, After) :-
    C = 46, % Codice ASCII per .
    Codes = [C2 | _],
    is_letter(C2),
    !,
    identificatore_host(Codes, P, After),
    Host = [C | P].

identificatore_host([C | Codes], Host, After) :-
    carattere(C),
    !,
    identificatore_host(Codes, P, After),
    Host = [C | P].

%   ip(Codes, Host, After)
%
%   L'Host viene estratto dai codici passati in input, se il formato dell'Host
%   corrisponde a quello di un indirizzo IP.
%   46 è il codice ASCII per .
% 192.168.156.12 ==> ipv4, 32 bit === 4 byte, 4 ottetti di bit
% ogni ottetto separato da un punto

ip(Codes, Host, After):-
    octet(Codes, FirstOctet, [46 | AfterFirstOctet]),
    octet(AfterFirstOctet, SecondOctet, [46 | AfterSecondOctet]),
    octet(AfterSecondOctet, ThirdOctet, [46 | AfterThirdOctet]),
    octet(AfterThirdOctet, FourthOctet, After),
    append(FirstOctet, [46], FirstAppend),
    append(SecondOctet, [46], SecondAppend),
    append(ThirdOctet, [46], ThirdAppend),
    append(FirstAppend, SecondAppend, FourthAppend),
    append(FourthAppend, ThirdAppend, FifthAppend),
    append(FifthAppend, FourthOctet, Host).

%   octet(Codes, Octet, After)
%
%   Vengono estratti e convertiti gli otto bit che compongono un ottetto
%   dai codici passati in input.

%   Ottetto composto da tre cifre.
octet([A, B, C | After], Octet, After):-
    is_digit(A),
    is_digit(B),
    is_digit(C),
    number_codes(Number, [A, B, C]),
    Number =< 255, % 2^8 ==> 0-255
    Number >= 0,
    Octet = [A, B, C],
    !.

%   Ottetto composto da due cifre.
octet([A, B | After], Octet, After):-
    is_digit(A),
    is_digit(B),
    number_codes(Number, [A, B]),
    Number =< 255,
    Number >= 0,
    Octet = [A, B],
    !.

%   Ottetto composto da una cifra.
octet([A | After], Octet, After):-
    is_digit(A),
    number_codes(Number, [A]),
    Number =< 255,
    Number >= 0,
    Octet = [A],
    !.

%   port(Codes, Port, After)
%
%   Viene estratta Port dai codici passati in input.

parse_port([], Port, After) :-
    Port = [],
    After = [].

parse_port([C | Codes], Port, After) :-
    C = 47, % Codice ASCII per /
    !,
    Port = [],
    After = [C | Codes].

parse_port([C | Codes], Port, After) :-
    is_digit(C),
    !,
    parse_port(Codes, P, After),
    Port = [C | P].

port([C | Codes], Port, After) :-
    C = 58, % Codice ASCII per :
    !,
    parse_port(Codes, Port, After),
    Port \= [].

port(Schema, Port) :-
    Schema = 'https',
    Port = 443.

port(Schema, Port) :-
    Schema = 'ftp',
    Port = 21.

port(_, Port) :-
    Port = 80.


%   path(Codes, Path, After)
%
%   Viene estratto il Path dai codici passati in input.

parse_path([], Path, After) :-
    Path = [],
    After = [].

parse_path([C | Codes], Path, After) :-
    C = 63,  % Codice ASCII per ?
    !,
    Path = [],
    After = [C | Codes].

parse_path([C | Codes], Path, After) :-
    C = 35, % Codice ASCII per #
    !,
    Path = [],
    After = [C | Codes].

parse_path([C1, C2 | Codes], Path, After) :-
    C1 = 47,  % Codice ASCII per /
    C2 = 47,  % Controllo di non avere // nel path
    !,
    Path = [],
    After = [C1, C2 | Codes].

parse_path([C | Codes], Path, After) :-
    identificatore(C),
    !,
    parse_path(Codes, P, After),
    Path = [C | P].

% Parse del Path che inizia con /
path([C | Codes], Path, After) :-
    C = 47,  % Codice ASCII per /
    Codes = [C2 | _ ],
    C2 \= 47,  % Controllo di non avere // all'inizio del path
    !,
    parse_path(Codes, Path, After).

% Parse del Path che non inizia con /
path(Codes, Path, After) :-
    Codes = [C | _],
    C \= 58,   % Controllo di non avere : all'inizio del path
    parse_path(Codes, Path, After).

%   zos_path(Codes)
%
%   Viene estratto il Path dai codici passati in input secondo il formato
%   dello Schema zos.

%   Parse del Path contenente solo Id44 secondo il formato dello Schema zos.
zos_path([C | Codes]) :-
    is_alpha(C),
    id44([C | Codes], Id44Codes, []),
    !,
    length(Id44Codes, Id44Length),
    Id44Length =< 44,
    last(Id44Codes, LastId44Character),
    LastId44Character \= 46. % . in ASCII

%   Parse del Path contenente Id44 e Id8 secondo il formato dello Schema zos.
zos_path([C | Codes]) :-
    is_alpha(C),
    id44([C | Codes], Id44Codes, AfterId44Codes),
    length(Id44Codes, Id44Length),
    Id44Length =< 44,
    last(Id44Codes, LastId44Character),
    LastId44Character \= 46,        % . in ASCII non può essere x lo schema zos
    id8(AfterId44Codes, Id8Codes),
    length(Id8Codes, Id8Length),
    Id8Length =< 8.

%   id44(Codes, Id44, After)
%
%   Viene estratto l'Id44 dai codici passati in input che compongono un Path,
%   secondo il formato dello Schema zos.

% mette in id44 tutti i caratteri + . che ci sono tra lo schema e il carattere (
id44([], Id44, After) :-
    Id44 = [],
    After = [].

id44([C | Codes], Id44, After) :-
    C = 40, % Codice ASCII per (
    !,
    Id44 = [],
    After = [C | Codes].

id44([C | Codes], Id44, After) :-
    is_alnum(C),
    !,
    id44(Codes, I, After),
    Id44 = [C | I].

id44([C | Codes], Id44, After) :-
    C = 46, % Codice ASCII per .
    !,
    id44(Codes, I, After),
    Id44 = [C | I].

%   id8(Codes, Id44, After)
%
%   Viene estratto l'Id8 dai codici passati in input che compongono un Path,
%   secondo il formato dello Schema zos.

parse_id8([C | []], Id8) :-
    C = 41, % Codice ASCII per )
    !,
    Id8 = [].

parse_id8([C | Codes], Id8) :-
    is_alnum(C),
    !,
    parse_id8(Codes, I),
    Id8 = [C | I]. % Id8 contiene tutti i caratteri compresi tra le ()
id8([C1, C2 | Codes], Id8) :-
    C1 = 40, % Codice ASCII per (
    is_alnum(C2),
    !,
    parse_id8([C2 | Codes], Id8).

%   query(Codes, Query, After)
%
%   Viene estratto il componente Query dai codici passati in input.

parse_query([], Query, After) :-
    Query = [],
    After = [].

parse_query([C | Codes], Query, After) :-
    C = 35, % Codice ASCII per #
    !,
    Query = [],
    After = [C | Codes].

parse_query([C | Codes], Query, After) :-
    identificatore(C),
    !,
    parse_query(Codes, Q, After),
    Query = [C | Q].

query([C | Codes], Query, After) :-
    C = 63, % Codice ASCII per ?
    !,
    parse_query(Codes, Query, After).

%   fragment(Codes, Fragment, After)
%
%   Viene estratto il Fragment dai codici passati in input.

parse_fragment([], Fragment, After) :-
    Fragment = [],
    After = [].

parse_fragment([C | Codes], Fragment, After) :-
    carattere(C),
    !,
    parse_fragment(Codes, F, After),
    Fragment = [C | F].

fragment([C | Codes], Fragment, After) :-
    C = 35, % Codice ASCII per #
    !,
    parse_fragment(Codes, Fragment, After).

%   urilib_display(URI)
%   urilib_display(URI, Stream)
%   sullo stream corrente.

urilib_display(URI) :-
    current_output(Stream),
    urilib_display(URI, Stream).

urilib_display(URI, Stream) :-
    URI = uri(Schema, Userinfo, Host, Port, Path, Query, Fragment),
    format(Stream, 'Schema = ~w\n', [Schema]),
    format(Stream, 'Userinfo = ~w\n', [Userinfo]),
    format(Stream, 'Host = ~w\n', [Host]),
    format(Stream, 'Port = ~w\n', [Port]),
    format(Stream, 'Path = ~w\n', [Path]),
    format(Stream, 'Query = ~w\n', [Query]),
    format(Stream, 'Fragment = ~w\n', [Fragment]),
    close(Stream).

%   is_letter(Carattere)
%
%   Viene stabilito se il carattere passata in input sia una lettera
%   maiuscola o minuscola tramite il suo codice ASCII

is_letter(C) :- C >= 65, C =< 90.  % Maiuscole (A-Z)
is_letter(C) :- C >= 97, C =< 122. % Minuscole (a-z)

%   identificatore(Carattere)
%
%   Viene stabilito se il carattere passato in input corrisponde ad uno dei
%   caratteri accettati dalla specifica corrente.

identificatore(C) :- carattere(C).
identificatore(C) :- C = 46.  % .
identificatore(C) :- C = 47.  % /
identificatore(C) :- C = 63.  % ?
identificatore(C) :- C = 40.  % (
identificatore(C) :- C = 41.  % )
identificatore(C) :- C = 35.  % #
identificatore(C) :- C = 43.  % +
identificatore(C) :- C = 45.  % -
identificatore(C) :- C = 61.  % =

%   carattere(Carattere)
%
%   Viene stabilito se il carattere passato in input corrisponde ad uno dei
%   caratteri accettati dalla specifica corrente.

carattere(C) :- is_alnum(C).
carattere(C) :- C = 95. % _

%%%% urilib-parse.pl ends here










