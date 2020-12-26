#!/bin/bash

export PATH="$HOME/bin:$HOME/.local/bin:/usr/local/bin/:/usr/local/sbin:/sbin:/bin:/usr/sbin:/usr/bin"

dir="$(dirname $0)"
cd "$dir"

set -x
set -e

rm -rf _cache _site

podman build -f hblog/Dockerfile -t hblog /home/rlpowell/src

chcon -R -t container_file_t ~/src/hblog/ ~/public_html/hblog/

podman run --rm -it -v /home/rlpowell/src/hblog:/opt/hblog \
  -v ~/Docs/Public/hblog_posts:/opt/hblog/posts \
  -v ~/.hblog-cache:/root/.hblog-cache \
  -v ~/public_html/hblog:/web \
  hblog ./run_build_internal.sh

chcon -R -t httpd_user_content_t ~/public_html/hblog/
