#!/bin/bash

dir="$(dirname $0)"
cd "$dir"
./setup_links.sh

set -x
set -e

if [ ! -f run_container.sh ]
then
  echo "You seem to be in the wrong place."
  exit 1
fi

rm -rf _site
rm -rf _cache

# munge_files runs a stack build and stack install
munge_files posts/

hblog build

rm -rf _cache

if [ -d /web/ ]
then
  chcon -R -t container_file_t /web/
  rsync -a --delete _site/ /web/
  chcon -R -t httpd_user_content_t /web/
else
  echo "No /web/ mounted; not copying there."
fi
