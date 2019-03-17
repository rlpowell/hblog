#!/bin/bash

exec 2>&1
set -e
set -x

CONTAINER_BIN=${CONTAINER_BIN:-$(which podman)}
CONTAINER_BIN=${CONTAINER_BIN:-$(which docker)}

"$(dirname $0)"/stop_container.sh || true
sudo $CONTAINER_BIN rm hblog || true
cp ../Docker_Shell/container_run_init_start.sh .
cp ../Docker_Shell/container_run_init_end.sh .
sudo $CONTAINER_BIN build -t rlpowell/hblog .
rm container_run_init_start.sh || true
rm container_run_init_end.sh || true
sudo $CONTAINER_BIN run --name hblog -v ~/public_html/hblog:/web:z -v volume--home--rlpowell:/home/rlpowell -p 0.0.0.0:2224:22 -p 0.0.0.0:8084:8084 -d -t -i rlpowell/hblog bash -x /tmp/container_run_init.sh
sleep 5
sudo $CONTAINER_BIN logs hblog
sudo $CONTAINER_BIN ps -a -f name=hblog
echo
echo
echo Now connect with SSH
