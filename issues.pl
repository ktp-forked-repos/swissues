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


swi_repositories_page('https://github.com/SWI-Prolog').
swi_repositories_page('https://github.com/SWI-Prolog?page=2').
swi_repositories_page('https://github.com/SWI-Prolog?page=3').
swi_repositories_page('https://github.com/SWI-Prolog?page=4').

% first clauses: testing with sample repositories
% repository('https://github.com/SWI-Prolog/packages-http').
% repository('https://github.com/SWI-Prolog/swipl-devel').
repository(R) :-
        swi_repositories_page(Page),
        catch((http_open(Page, Stream, [connection('Keep-alive')]),
              load_html(stream(Stream), DOM, [])),
              _,
              false),
        xpath(DOM, //a(contains(@itemprop, 'codeRepository'),@href), R0),
        atomic_list_concat(['https://github.com',R0], R).

%?- repository(R).
%@ R = 'https://github.com/SWI-Prolog/packages-http' ;
%@ R = 'https://github.com/SWI-Prolog/swipl-devel' .


%?- repository(R), portray_clause(R), repository_issue(R, T, L), format("~w ~w\n", [T,L]), false.

repository_issue(R, Text, Link) :-
        atomic_list_concat([R,'/issues'], Issues),
        catch((http_open(Issues, Stream, [connection('Keep-alive')]),
               load_html(stream(Stream), DOM, [])),
              Exception,
              true),
        (   nonvar(Exception) ->
            Text = error(Exception)
        ;   xpath(DOM, //a(contains(@class, 'issue-title-link')), Issue),
            xpath(Issue, /self(text), Text0),
            Text = text(Text0),
            xpath(Issue, /self(@href), Link0),
            atomic_list_concat(['https://github.com',Link0], Link)
        ).


%?- repository(R).
%?- repository_issue('https://github.com/SWI-Prolog/packages-http', Text, Href).
%@ Text = '      --debug=topic yields error with thread_httpd\n    ',
%@ Href = 'https://github.com/SWI-Prolog/packages-http/issues/32' .

