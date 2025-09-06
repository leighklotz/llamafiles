#!/bin/bash -x

PMT='Please write a project_history.md file for the git project below. Use the git log output as source of truth, and break into major sections and themes. Use full month/day/year for dates. Use ideas and terms from the README. Identify major features and subsystems and document their history. Group conmits together by theme instead of explaining each commit. Finish with an overall narrative of development themes, subsystems, and features. Cover the entire time range of the git log.'
pm="$(printf "%s" "${PMT}" | md5sum | awk '{print $1}')"

#(bashblock cat README.md; bashblock git log --reverse) | help.sh "${PMT}" > m-r--$pm-$$.md
(bashblock cat README.md; bashblock git log) | help.sh "${PMT}" > m-r--$pm-$$.md

