#!/bin/bash

# to be used as a cronjob

cd ~/swi-issues
/usr/local/bin/swipl -g make_html -t halt html_issues.pl
