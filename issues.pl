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
:- use_module(library(clpfd)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Declarative debugging
   ---------------------

   This definition allows me to conveniently generalize away goals.
   When Github changes its layout and an empty list of issues is produced,
   we can use this to quickly detect the cause of the failure.

   For more information about declarative debugging, please see:

              https://www.metalevel.at/prolog/debugging
              =========================================

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- op(920,fy, *).

*_.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Hard-coded list of repository overview pages.

   Issues are collected for each repository mentioned on these pages.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

swi_repositories_page('https://github.com/SWI-Prolog').
swi_repositories_page('https://github.com/SWI-Prolog?page=2').
swi_repositories_page('https://github.com/SWI-Prolog?page=3').
swi_repositories_page('https://github.com/SWI-Prolog?page=4').

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Sample repositories for testing.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

% repository('https://github.com/SWI-Prolog/packages-http').
% repository('https://github.com/SWI-Prolog/swipl-devel').

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   On backtracking, yield each repository that occurs on the pages.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

repository(R) :-
        swi_repositories_page(Page),
        catch((http_open(Page, Stream, [connection('Keep-alive'),timeout(2)]),
              load_html(stream(Stream), DOM, [])),
              _,
              false),
        xpath(DOM, //a(contains(@itemprop, 'codeRepository'),@href), R0),
        atomic_list_concat(['https://github.com',R0], R).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   For a given repository page, yield the text and link for each issue.

   We perform 3 attempts to fetch the issues. This is necessary because
   Github sometimes does not respond.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

repository_issue(R, Text, Link) :-
        repository_issue_(3, R, Text, Link).

repository_issue_(AttemptsLeft0, R, Text, Link) :-
        atomic_list_concat([R,'/issues'], Issues),
        catch((http_open(Issues, Stream, [connection('Keep-alive'),timeout(2)]),
               load_html(stream(Stream), DOM, [])),
              Exception,
              true),
        (   nonvar(Exception) ->
            (   AttemptsLeft0 #=< 0 ->
                Text = error(Exception)
            ;   AttemptsLeft #= AttemptsLeft0 - 1,
                sleep(5), % wait 5 seconds until next attempt
                repository_issue_(AttemptsLeft, R, Text, Link)
            )
        ;   dom_issue_text_link(DOM, Text, Link)
        ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Extract the text and link for each issue from the DOM.

   If the Github layout changes, then this predicate must be adapted.
   So far, this has occurred once every few months.

   In such cases, declarative debugging (see above) can be used to
   quickly determine which of the goals fails.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

dom_issue_text_link(DOM, Text, Link) :-
        xpath(DOM, //a(contains(@class, 'h4')), Issue),
        xpath(Issue, /self(text), Text0),
        Text = text(Text0),
        xpath(Issue, /self(@href), Link0),
        atomic_list_concat(['https://github.com',Link0], Link).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   For testing:

?- load_html(issues, DOM, []),
   issues:dom_issue_text_link(DOM, Text, Link),
   portray_clause(Text),
   false.

?- repository(R).
%@ R = 'https://github.com/SWI-Prolog/packages-ssl' ;
%@ R = 'https://github.com/SWI-Prolog/plweb-www' .

?- repository_issue('https://github.com/SWI-Prolog/packages-http', Text, Href).
%@ Text = '      --debug=topic yields error with thread_httpd\n    ',
%@ Href = 'https://github.com/SWI-Prolog/packages-http/issues/32' .


?- repository(R),
   portray_clause(R),
   repository_issue(R, T, L),
   format("~w ~w\n", [T,L]),
   false.

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
