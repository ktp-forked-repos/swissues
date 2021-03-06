## Reason about issues related to SWI-Prolog and its packages

The Prolog programs in this repository reason about the
SWI-Prolog&nbsp;(+&nbsp;packages) **issues** that are filed on
[GitHub](https://github.com/SWI-Prolog).

Browse the collection of SWI-Prolog issues, updated twice a day:

[**https://www.metalevel.at/swissues**](https://www.metalevel.at/swissues)

[**issues.pl**](issues.pl) lets you obtain the repositories and issues on
the toplevel. This is very easy to do, using a combination of
SWI-Prolog's
[HTTP](http://eu.swi-prolog.org/pldoc/man?section=httpopen),
[HTML](http://eu.swi-prolog.org/pldoc/doc/swi/library/sgml.pl) and
[XPATH](http://eu.swi-prolog.org/pldoc/doc/swi/library/xpath.pl)&nbsp;libraries.

In principle, the repositories can be easily extracted from a github
overview page with:

    overview_repository(Page, R) :-
            load_html(Page, DOM, []),
            xpath(DOM, //a(contains(@itemprop, 'codeRepository'),@href), R0),
            atomic_list_concat(['https://github.com',R0], R).

Complications only arise because github does not answer very reliably
on multiple requests, so keep-alive connections and timeouts are used.

If the github page structure changes, then such snippets must be adapted.

Example invocation:

    $ swipl issues.pl

Sample query and its result:

<pre>
?- repository(R), repository_issue(R, Text, Link).
R = 'https://github.com/SWI-Prolog/packages-http',
<b>Text = text('      --debug=topic yields error with thread_httpd\n    '),</b>
Link = 'https://github.com/SWI-Prolog/packages-http/issues/32' ;
R = 'https://github.com/SWI-Prolog/packages-http',
<b>Text = text('      HEAD method must not return a message-body in the response\n    '),</b>
Link = 'https://github.com/SWI-Prolog/packages-http/issues/31' ;
R = 'https://github.com/SWI-Prolog/packages-http',
<b>Text = text('      Better approach towards throwing HTTP errors\n    '),</b>
Link = 'https://github.com/SWI-Prolog/packages-http/issues/20' .
</pre>

[**html_issues.pl**](html_issues.pl) uses
[library(http/html_write)](http://eu.swi-prolog.org/pldoc/man?section=htmlwrite)
to create the file&nbsp;`swi-issues.html` when `make_html/0` is
invoked. The HTML&nbsp;file displays the collected information about
all issues.

To refresh `swi-issues.html`, use for example:

    $ swipl -g make_html -t halt html_issues.pl
