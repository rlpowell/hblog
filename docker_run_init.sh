check_mount () {
  mount=$1
  cd $mount
  if ! df -h . | grep -q '^/dev/sd'
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
mkdir -p ~/.stack ~/.local ~/src/hblog/.stack-work
sudo chown -R rlpowell ~/.stack/ ~/.local/ ~/src/hblog/.stack-work/
check_mount ~/.stack
check_mount ~/.local
check_mount ~/src/hblog/.stack-work
# rm /home/rlpowell/src/hblog/.stack-work
# rm -rf /home/rlpowell/src/hblog/.stack-work
# ln -s /tmp/hbsw /home/rlpowell/src/hblog/.stack-work
sudo /usr/sbin/sshd -D -e
