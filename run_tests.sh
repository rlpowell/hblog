stack test --test-arguments "$*"
#stack test --trace --test-arguments "$*"



# stack runghc --trace test/Spec.hs -- "$*"




# set -e
# rm -rf .stack-work
# ln -s /tmp/hbsw  .stack-work
# if [ "$1" ]
# then
#   echo "$*" | stack --no-keep-going ghci --no-keep-going :hblog-test
# else
#   stack ghci :hblog-test
# fi
