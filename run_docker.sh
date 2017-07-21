set -x
"$(dirname $0)"/stop_docker.sh
sudo docker rm hblog
sudo docker build -t rlpowell/hblog .
sudo docker run --name hblog -v /var/run/docker.sock:/var/run/docker.sock -v ~/Dropbox:/dropbox:z --volumes-from volume--stack--global --volumes-from volume--local--hblog --volumes-from volume--stack-work--hblog --volumes-from volume--stack-work--pandoc --volumes-from volume--stack-work--pandoc-citeproc --volumes-from volume--stack-work--hakyll -p 0.0.0.0:2224:22 -p 0.0.0.0:8084:8084 -d -t -i rlpowell/hblog bash -x /tmp/docker_run_init.sh
sleep 5
sudo docker logs hblog
sudo docker ps -a -f name=hblog
echo
echo
echo Now connect with SSH
