#!/bin/bash

cd ~/src/hblog/

if ! sudo docker ps | grep -q rlpowell/hblog
then
  ./run_docker.sh
fi

sudo docker exec -i hblog zsh -c "export PATH=/home/rlpowell/.local/bin:$PATH ; ./run_build.sh >/tmp/build.out 2>&1 || cat /tmp/build.out"
