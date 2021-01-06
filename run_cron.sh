#!/bin/bash

export PATH="$HOME/bin:$HOME/.local/bin:/usr/local/bin/:/usr/local/sbin:/sbin:/bin:/usr/sbin:/usr/bin"

exec 2>&1

cd ~/src/hblog/

LOCK_FILE=/home/rlpowell/scratch/hblog_run_cron_lock
exec 99>"$LOCK_FILE"
flock -n 99
if [[ $? -ne 0 ]]
then
  echo "Could not acquire lock; exiting."
  exit 1
fi

./run_build.sh >/tmp/hblog_cron.$$ 2>&1
exitcode=$?

if [ "$exitcode" -ne 0 ]
then
  echo "Errors found; showing full output."
  cat /tmp/hblog_cron.$$
# If we see mail happening that isn't part of a Docker RUN line,
# changes must have occurred
elif grep -v '^STEP [0-9]*: RUN' /tmp/hblog_cron.$$ | grep -q -i mail
then
  echo "Changes found; showing full output."
  cat /tmp/hblog_cron.$$
fi
rm -f /tmp/hblog_cron.$$
