/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Fetch issues related to SWI-Prolog and its packages from github.com.

   Written Jan 2016 by Markus Triska (triska@metalevel.at)

   Public domain code.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- module(issues,
          [repository/1,
           repository_issue/3]).

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

%?- repository(R).
%@ R = 'https://github.com/SWI-Prolog/packages-http' ;
%@ R = 'https://github.com/SWI-Prolog/swipl-devel' .


repository_issue(R, Text, Link) :-
        atomic_list_concat([R,'/issues'], Issues),
        load_html(Issues, DOM, []),
        xpath(DOM, //a(contains(@class, 'issue-title-link')), Issue),
        xpath(Issue, /self(text), Text),
        xpath(Issue, /self(@href), Link0),
        atomic_list_concat(['https://github.com',Link0], Link).


%?- repository_issue('https://github.com/SWI-Prolog/packages-http', Text, Href).
%@ Text = '      --debug=topic yields error with thread_httpd\n    ',
%@ Href = 'https://github.com/SWI-Prolog/packages-http/issues/32' .

