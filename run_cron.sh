#!/bin/bash

exec 2>&1
set -e

CONTAINER_BIN=${CONTAINER_BIN:-$(which podman)}
CONTAINER_BIN=${CONTAINER_BIN:-$(which docker)}

cd ~/src/hblog/

if ! sudo $CONTAINER_BIN ps | grep -q rlpowell/hblog
then
  ./run_container.sh
fi

sudo $CONTAINER_BIN exec -i hblog zsh -c "export PATH=/home/rlpowell/.local/bin:$PATH ; ./run_build.sh >/tmp/build.out 2>&1 || cat /tmp/build.out"
