#!/bin/bash

set -x
set -e

if [ ! -f run_docker.sh ]
then
  echo "You seem to be in the wrong place."
  exit 1
fi

rm -f posts
if [ -e posts ]
then
  echo "Couldn't remove the posts link"
  exit 1
fi

ln -s /dropbox/Docs/Public/hblog_posts posts

rm -rf _site
rm -rf _cache

stack install
hblog build

rm -rf _cache
