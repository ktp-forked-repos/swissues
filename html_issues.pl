/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Create an HTML file from issues related to SWI-Prolog and its packages.

   Written Jan 2016 by Markus Triska (triska@metalevel.at)

   Public domain code.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- use_module(library(http/html_write)).
:- use_module(issues).

%?- make_html.
%@ true.


make_html :-
        findall(Repository-issue(Text,Link),
                (   repository(Repository),
                    repository_issue(Repository, Text, Link)),
                Pairs0),
        keysort(Pairs0, Pairs),
        group_pairs_by_key(Pairs, RepIssues),
        issues_html(RepIssues, Hs),
        setup_call_cleanup(open('swi-issues.html', write, Stream),
                           print_html(Stream, Hs),
                           close(Stream)).

issues_html(RepIssues, Hs) :-
        phrase(page([title('SWI-Prolog issues'),
                     style(' a { text-decoration: none; }')],
                    [br([]),h2(center('SWI-Prolog Issues')),br([]),
                     div([style='padding-left: 5%'],
                         ['Compiled with ',
                          a([href='https://github.com/triska/swissues'],
                            b('swissues')),'.',
                          br([]),br([]),
                          'Date: ', \today,
                          br([]),br([]),
                          \repository_issues(RepIssues)]),
                     hr([]),
                     div([style='text-align: center'],
                         ['Powered by ',
                          i([a([href='https://github.com/triska/proloxy'],
                              'Proloxy')])])]), Hs).

today -->
        { get_time(Now), format_time(atom(A), "%h %d", Now) },
        html(b(A)).

repository_issues([]) --> [].
repository_issues([Repository-Issues|RIs]) -->
        { path_segments_atom(_/Package, Repository) },
        html(h3(a([href=Repository],Package))),
        html_begin(ul),
        html(\issues(Issues)),
        html_end(ul),
        repository_issues(RIs).

issues([]) --> [].
issues([issue(Text,Link)|Is]) -->
        html(\link_to(Text, Link)),
        issues(Is).

link_to(error(Exception), _) --> html(li(b("~q"-[Exception]))).
link_to(text(Text), Link)    --> html(li(a([href=Link],[Text]))).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
?- issues_html(['a/test1'-[issue(text(x),y),issue(error(v),w)]], Hs),
   print_html(Hs).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
