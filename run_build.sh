#!/bin/bash

set -x
set -e

if [ ! -f run_docker.sh ]
then
  echo "You seem to be in the wrong place."
  exit 1
fi

if [ ! -d posts/. ]
then
  rm -f posts
  if [ -e posts ]
  then
    echo "Couldn't remove the posts link"
    exit 1
  fi

  ln -s /dropbox/Docs/Public/hblog_posts posts
fi

rm -rf _site
rm -rf _cache

stack build hblog
stack install
hblog build

rm -rf _cache
