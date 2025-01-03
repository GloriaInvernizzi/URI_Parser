:- consult('urilib-parse.pl').

% Lista estesa di URL (validi e non validi specificati nei commenti)
url_list([
    % Validi
    "http:192.168.19.2",                          % Valido
    "http://192.168.0.236",                       % Valido
    "http://123.123.12.123",                      % Valido
    "https://example.com",                        % Valido
    "ftp://ftp.example.com",                      % Valido
    "https://www.example.com:443/path?query=param#fragment", % Valido
    "mailto:user@example.com",                    % Valido
    "tel:+3934776623131",                         % Valido

    "http:?query#fragment",                       % Valido
    "scheme:?query",                              % Valido
    "scheme:#fragment",                           % Valido
    "scheme:",                                    % Valido
    "scheme:path/to/resource",                    % Valido
    "scheme:path/to/resource.pdf",                % Valido
    "scheme:/",                                   % Valido
    "scheme:/path/to/file.pdf",                   % Valido

    "http://user:1/path",                         % Valido
    "http://user/path",                           % Valido
    "http://user",                                % Valido
    "http://ip:3",                                % Valido
    "http://user:580",                            % Valido
    "htp:/example.com",                           % Valido
    "zos://blabla.com:100",                       % Valido
    "zos://blabla.com:100/#fragment",             % Valido
    "zos://blabla.com:100/?query",                % Valido
    "zos://blabla.com:100/?q#f",                  % Valido
    "zos://blabla.com:100/path.p",                % Valido
    "zos://blabla.com:100/p#f",                   % Valido
    "zos://blabla.com:100/p?q",                   % Valido
    "zos://blabla.com:100/p?q#f",                 % Valido
    "zos://example.com/abcd.12(123)",             % Valido
    "mailto:gloria@example.it",                   % Valido
    "mailto:gloria",                              % Valido
    "news:host",                                  % Valido
    "news:123.126.34.5",                          % Valido

    % Non validi
    "invalid_url",                                   % Non valido
    "http://",                                       % Non valido
    "http://:8080",                                  % Non valido
    "http://example..com",                           % Non valido
    "http://[::1]:",                                 % Non valido
    "http://example.com:-80",                        % Non valido
    "https://example.com/<>",                        % Non valido
    "",                                              % Non valido
    "http://[::1]:8080",                             % Non valido
    "file:///C:/path/to/file",                       % Non valido
    "http://user:p/path",                            % Non valido
    "http://?query#fragment",                        % Non valido
    "http://123.345.55.5",                           % Non valido
    "http://user:pass@host.com:8080/path",           % Non valido
    "zos:abcid44(12345678910)",                      % Non valido
    "zos://example.com/q.s.d.2.1.2.3.(12345678910)", % Non valido
    "mailto",                                        % Non valido
    "news:122.344.3.2"                               % Non valido

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
