set -e
set -x

DIR="$(pwd)"

TERM=xterm-256color tmux -2 -u new-session -d -A -s main -c /home/rlpowell/src/hblog
mkdir -p ~/.vimtmp

# Make sure we've got dirs and perms we expect
mkdir -p ~/.stack ~/.local ~/.stack-work-hblog ~/.stack-work-pandoc ~/.stack-work-pandoc-citeproc ~/.stack-work-hakyll
sudo chown -R rlpowell ~/.stack/ ~/.local/ ~/src/ ~/.stack-work-hblog/ ~/.stack-work-pandoc/ ~/.stack-work-pandoc-citeproc/ ~/.stack-work-hakyll/

# Get our ~/src and ~/Docs and whatever else directories, from vrici
syncthing 2>&1 | tee /tmp/syncthing.log &
disown
sleep 5
syncthing-inotify 2>&1 | tee /tmp/syncthing-inotify.log &
disown
sleep 5

exec sudo /usr/sbin/sshd -D
