sudo docker kill hblog
sudo docker rm hblog
sudo docker build -t rlpowell/hblog .
sudo docker run --name hblog -v /var/run/docker.sock:/var/run/docker.sock -v /home/rlpowell/Dropbox:/dropbox:z --volumes-from stackstore -p 0.0.0.0:2224:22 -p 0.0.0.0:8084:8084 -d -t -i rlpowell/hblog bash -c "TERM=xterm-256color tmux -2 -u new-session -d -A -s main -c /home/rlpowell/src/hblog ; rm /home/rlpowell/src/hblog/.stack-work ; rm -rf /home/rlpowell/src/hblog/.stack-work ; ln -s /tmp/ hbsw /home/rlpowell/src/hblog/.stack-work ; sudo /usr/sbin/sshd -D -e"
echo Now connect with SSH
