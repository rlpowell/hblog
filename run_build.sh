#!/bin/bash

dir="$(dirname $0)"
cd "$dir"
./setup_links.sh

set -x
set -e

if [ ! -f run_docker.sh ]
then
  echo "You seem to be in the wrong place."
  exit 1
fi

rm -rf _site
rm -rf _cache

stack build hblog
stack install
hblog build

rm -rf _cache

./teardown_links.sh
