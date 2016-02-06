#!/bin/bash

# to be used as a cronjob

/usr/local/bin/swipl -g make_html -t halt html_issues.pl
