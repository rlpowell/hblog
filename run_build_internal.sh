#!/bin/bash

export PATH="$HOME/bin:$HOME/.local/bin:/usr/local/bin/:/usr/local/sbin:/sbin:/bin:/usr/sbin:/usr/bin"

dir="$(dirname $0)"
cd "$dir"

set -x
set -e

if [ ! -f run_build.sh ]
then
  echo "You seem to be in the wrong place."
  exit 1
fi

munge_files posts/

hblog build

if [ -d /web/ ]
then
  rsync -a --delete _site/ /web/
else
  echo "No /web/ mounted; not copying there."
fi
