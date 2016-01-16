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
                Pairs),
        group_pairs_by_key(Pairs, RepIssues),
        phrase(page([title('SWI-Prolog issues'),
                     style(' a { text-decoration: none; }')],
                    [br([]),h2(center('SWI-Prolog Issues')),br([]),
                     div([style='padding-left: 5%'],
                         ['Compiled with ',
                          a([href='https://github.com/triska/swi-issues'],
                            'SWI-Prolog'),
                          \repository_issues(RepIssues)]),
                     hr([]),
                     div([style='text-align: center'],
                         ['Powered by ',
                          i([a([href='https://github.com/triska/proloxy'],
                              'Proloxy')])])]), Hs),
        setup_call_cleanup(open('swi-issues.html', write, Stream),
                           print_html(Stream, Hs),
                           close(Stream)).

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
?- reply_html_page([title(test),style('body { padding-left: 5% }')],
   [\repository_issues(['a/test1'-[issue(text(x),y),issue(error(v),w)]])]).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
