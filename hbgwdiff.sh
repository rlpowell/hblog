#!/bin/bash

# If you make major changes here, consider making them to
# ~/bin/gwdiff as well, although they do not serve exactly the same
# function.

echo "You can pipe the wdiff output below through colordiff --difftype=wdiff for color."
echo "BEGIN wdiffs for $1"
wdiff -n $2 $5 | (grep --color=never -C 5 '[{[][+-]' || echo "No wdiffs found.")
echo "END wdiffs for $1"
