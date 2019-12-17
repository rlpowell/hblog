#!/bin/bash

exec 2>&1
set -e

CONTAINER_BIN=${CONTAINER_BIN:-$(which podman)}
CONTAINER_BIN=${CONTAINER_BIN:-$(which docker)}

cd ~/src/hblog/

if ! $CONTAINER_BIN ps | grep -q rlpowell/hblog
then
  ./run_container.sh
fi

set +e
$CONTAINER_BIN exec -i hblog zsh -c "export PATH=/home/rlpowell/.local/bin:$PATH ; bash -x ./run_build.sh 2>&1" >/tmp/hblog_cron.$$ 2>&1
exitcode=$?
set -e

if [ "$exitcode" -ne 0 ]
then
  echo "Errors found; showing full output."
  cat /tmp/hblog_cron.$$
elif grep -q -i mail /tmp/hblog_cron.$$
then
  echo "Changes found; showing full output."
  cat /tmp/hblog_cron.$$
fi
rm -f /tmp/hblog_cron.$$
