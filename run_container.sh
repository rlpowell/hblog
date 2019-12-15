#!/bin/bash

exec 2>&1
set -e
set -x

# First, build
"$(dirname $0)"/build_container.sh

CONTAINER_BIN=${CONTAINER_BIN:-$(which podman)}
CONTAINER_BIN=${CONTAINER_BIN:-$(which docker)}

"$(dirname $0)"/stop_container.sh || true

sudo -u rlpowell $CONTAINER_BIN run --userns=keep-id --name hblog -v ~/public_html/hblog:/web:rw -v /home/rlpowell:/home/rlpowell:rw -p 2224:22 -p 8084:8084 -d -t -i rlpowell/hblog bash -x container_run_init.sh
sleep 5
sudo -u rlpowell $CONTAINER_BIN logs hblog
sudo -u rlpowell $CONTAINER_BIN ps -a -f name=hblog
echo
echo
echo Now connect with SSH
