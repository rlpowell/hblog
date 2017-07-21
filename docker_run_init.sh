set -x

DIR="$(pwd)"
check_mount () {
  mount=$1
  cd $mount
  if ! df -h . | grep -q '^/dev/[sv]d'
  then
    mkdir -p $mount
    cd $mount
    ls -ld $mount
    df -h .
    echo "BADNESS: $mount is not a shared mount.  Aborting."
    exit 1
  fi
}

TERM=xterm-256color tmux -2 -u new-session -d -A -s main -c /home/rlpowell/src/hblog
mkdir -p ~/.vimtmp
# These probably won't help if the mounts aren't already setup, but
# they should make the next run work
mkdir -p ~/.stack ~/.local ~/src/hblog/.stack-work ~/src/pandoc/.stack-work ~/src/pandoc-citeproc/.stack-work ~/src/hakyll/.stack-work 
sudo chown -R rlpowell ~/.stack/ ~/.local/ ~/src/

check_mount ~/.stack
check_mount ~/.local
check_mount ~/src/hblog/.stack-work
check_mount ~/src/pandoc/.stack-work
check_mount ~/src/pandoc-citeproc/.stack-work
check_mount ~/src/hakyll/.stack-work

cd /dropbox/src

/dropbox/src/docker_dropbox_initial_sync.rb "$(basename "$DIR")"/sync.yaml

sudo /usr/sbin/sshd

exec /dropbox/src/docker_dropbox_sync.rb "$(basename "$DIR")"/sync.yaml
