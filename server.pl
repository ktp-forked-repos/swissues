/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   HTTP server displaying issues related to SWI-Prolog from github.com.

   Written Jan 2016 by Markus Triska (triska@metalevel.at)

   Public domain code.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_unix_daemon)).
:- use_module(library(http/http_log)).
:- use_module(library(http/html_write)).

:- initialization http_daemon.

:- http_handler(/, handle_request, [prefix]).

:- use_module(issues).

handle_request(_Request) :-
        findall(Repository-issue(Text,Link),
                (   repository(Repository),
                    repository_issue(Repository, Text, Link)),
                Pairs),
        group_pairs_by_key(Pairs, RepIssues),
        reply_html_page(title('SWI-Prolog issues'),
                        [\repository_issues(RepIssues)]).

repository_issues([]) --> [].
repository_issues([Repository-Issues|RIs]) -->
        html(h3(Repository)),
        html_begin(ul),
        html(\issues(Issues)),
        html_end(ul),
        repository_issues(RIs).

issues([]) --> [].
issues([issue(Text,Link)|Is]) -->
        html(li(a([href=Link],[Text]))),
        issues(Is).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
?- reply_html_page(title(test),
   [\repository_issues([a-[issue(x,y),issue(v,w)]])]).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
