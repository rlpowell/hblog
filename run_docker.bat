call stop_docker.bat
docker rm hblog
docker build -t rlpowell/hblog .
docker run --name hblog -v /var/run/docker.sock:/var/run/docker.sock -v C:\Users\rlpowell\Dropbox:/dropbox:z --volumes-from volume--stack--global --volumes-from volume--local--hblog --volumes-from volume--stack-work--hblog --volumes-from volume--stack-work--pandoc --volumes-from volume--stack-work--pandoc-citeproc --volumes-from volume--stack-work--hakyll -p 0.0.0.0:2224:22 -p 0.0.0.0:8084:8084 -t -i -d rlpowell/hblog bash -x /tmp/docker_run_init.sh
timeout /nobreak /t 5
docker logs hblog
docker ps -a -f name=hblog
@echo ""
@echo ""
echo Now connect with SSH
