:- use_module(library(xpath)).
:- use_module(library(http/http_open)).
:- use_module(library(sgml)).

github_page('https://github.com/SWI-Prolog').
github_page('https://github.com/SWI-Prolog?page=2').
github_page('https://github.com/SWI-Prolog?page=3').
github_page('https://github.com/SWI-Prolog?page=4').

repository(R) :-
        github_page(Page),
        load_html(Page, DOM, []),
        xpath(DOM, //a(contains(@itemprop, 'codeRepository'),@href), R0),
        atomic_list_concat(['https://github.com',R0], R).

%?- repository(R), 
%@ R = 'https://github.com/SWI-Prolog/packages-http' ;
%@ R = 'https://github.com/SWI-Prolog/swipl-devel' ;
%@ R = 'https://github.com/SWI-Prolog/distro-debian' ;
%@ R = 'https://github.com/SWI-Prolog/packages-sgml' ;
%@ R = 'https://github.com/SWI-Prolog/packages-zlib' ;
%@ R = 'https://github.com/SWI-Prolog/packages-xpce' ;
%@ R = 'https://github.com/SWI-Prolog/packages-windows' .


repository_issue(R, I) :-
        atomic_list_concat([R,'/issues'], Issues),
        load_html(Issues, DOM, []),
        xpath(DOM, //a(contains(@class, 'issue-title-link'),text), I).


%?- repository(R), repository_issue(R, I).

%?- repository_issue('https://github.com/SWI-Prolog/packages-http', I).
%@ R = element(a, [href='/SWI-Prolog/packages-http', itemprop='name codeRepository'], ['        packages-http']) .
        
        

