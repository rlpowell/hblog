docker kill hblog
docker rm hblog
docker build -t rlpowell/hblog .
docker run --name hblog -v /var/run/docker.sock:/var/run/docker.sock -v C:\Users\rlpowell\Dropbox:/dropbox:z --volumes-from stackstore --volumes-from stackstore-hblog-1 -p 0.0.0.0:2224:22 -p 0.0.0.0:8084:8084 -d -t -i rlpowell/hblog bash -x /tmp/docker_run_init.sh
timeout /nobreak /t 5
docker logs hblog
docker ps -a -f name=hblog
@echo ""
@echo ""
echo Now connect with SSH
