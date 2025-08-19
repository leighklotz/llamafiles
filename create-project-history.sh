#!/bin/bash -x

#(bashblock cat README.md; bashblock git log --reverse) | help.sh "write a project_history.md file for the llamafiles project. Use the git log output as source if truth, and breakninto major sections and themes. Use ideqs and terms from the README. Folloenthe timelinr, stsrting Jan 5, 2024" > m-r-$$.md

#PMT="write a project_history.md file for the llamafiles project. Use the git log output as source if truth, and breakninto major sections and themes. Use ideqs and terms from the README. Folloenthe timelinr, stsrting Jan 5, 2024"
PMT='Please write a project_history.md file for the llamafiles project. Use the git log output as source of truth, and break into major sections and themes. Use ideas and terms from the README. Identify major features and subsystems and document their history. Group conmits together by theme instead of explaining each commit. Finish with an overall narrative of development themes, subsystems, and features.'
pm="$(printf "%s" "${PMT}" | md5sum | awk '{print $1}')"

(bashblock cat README.md; bashblock git log --reverse) | help.sh "${PMT}" > m-r--$pm-$$.md

