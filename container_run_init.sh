set -e
set -x

~/src/containers/container_run_init_start.sh

TERM=xterm-256color tmux -2 -u new-session -d -A -s main -c /home/rlpowell/src/hblog

stack install pandoc

exec ~/src/containers/container_run_init_end.sh
