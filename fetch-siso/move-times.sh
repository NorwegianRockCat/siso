#!/bin/sh

# Grep everything up
grep -E -e 'ID 10[0-9][0-9] now using .* \(index [0-3]\)' -e 'Move finished time to complete:' *.log >> timings.txt 

# Do some clean-up (make sure items have 6-items per movement). Save it as timings-clean.txt 

# now format it up.
awk -f times-to-columns.awk timings-clean.txt  > timings-tidy.tsv
