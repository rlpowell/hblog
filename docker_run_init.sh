set -e
set -x

/tmp/docker_run_init_start.sh

TERM=xterm-256color tmux -2 -u new-session -d -A -s main -c /home/rlpowell/src/hblog

# Make sure we've got dirs and perms we expect
mkdir -p ~/.stack ~/.local ~/.stack-work-hblog ~/.stack-work-pandoc ~/.stack-work-pandoc-citeproc ~/.stack-work-hakyll
sudo chown -R rlpowell ~/.stack/ ~/.local/ ~/src/ ~/.stack-work-hblog/ ~/.stack-work-pandoc/ ~/.stack-work-pandoc-citeproc/ ~/.stack-work-hakyll/

exec /tmp/docker_run_init_end.sh
