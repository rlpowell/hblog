#!/bin/bash

export PATH="$HOME/bin:$HOME/.local/bin:/usr/local/bin/:/usr/local/sbin:/sbin:/bin:/usr/sbin:/usr/bin"

dir="$(dirname $0)"
cd "$dir"

set -x
set -e

rm -rf _cache _site

podman build -f hblog/Dockerfile -t hblog /home/rlpowell/src

# All the dancing around with hblog_new and stuff is because the
# container needs it to be container_file_t but Apache needs it to
# be httpd_user_content_t, and we can't do both and I didn't want to
# muck with my SELinux config more.

rm -rf ~/public_html/hblog_new ~/public_html/hblog_old

cp -pr ~/public_html/hblog ~/public_html/hblog_new

chcon -R -t container_file_t ~/src/hblog/ ~/public_html/hblog_new/

podman run --rm -it -v /home/rlpowell/src/hblog:/opt/hblog \
  -v ~/Docs/Public/hblog_posts:/opt/hblog/posts \
  -v ~/.hblog-cache:/root/.hblog-cache \
  -v ~/public_html/hblog_new:/web \
  hblog ./run_build_internal.sh

chcon -R -t httpd_user_content_t ~/public_html/hblog_new/

mv ~/public_html/hblog ~/public_html/hblog_old
mv ~/public_html/hblog_new ~/public_html/hblog
