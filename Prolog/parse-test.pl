:- consult('urilib-parse.pl').

% Lista estesa di URL (validi e non validi specificati nei commenti)
url_list([
    % Validi
    "http://192.168.0.236",                       % Valido
    "https://example.com",                        % Valido
    "ftp://ftp.example.com",                      % Valido
    "http://[::1]:8080",                          % Valido
    "https://www.example.com:443/path?query=param#fragment", % Valido
    "http://user:pass@host.com:8080/path",        % Valido
    "mailto:user@example.com",                    % Valido
    "file:///C:/path/to/file",                    % Valido

    % Non validi
    "invalid_url",                                % Non valido
    "http://",                                    % Non valido
    "http://:8080",                               % Non valido
    "http://example..com",                        % Non valido
    "htp:/example.com",                           % Non valido
    "http://[::1]:",                              % Non valido
    "http://example.com:-80",                     % Non valido
    "https://example.com/<>",                     % Non valido
    ""                                           % Non valido
]).

% Funzione per testare ogni URL
test :-
    url_list(URLs),
    test_urls(URLs).

% Itera su ogni URL e tenta di analizzarlo
test_urls([]).
test_urls([URL | Rest]) :-
    (   urilib_parse(URL, URI)
    ->  format("accettato: ~w -> Parsed URI: ~w~n", [URL, URI])
    ;   format("rifiutato: ~w~n", [URL])
    ),
    test_urls(Rest).

% Fine del file
