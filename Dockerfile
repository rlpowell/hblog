FROM rlpowell/stack

USER rlpowell
WORKDIR /home/rlpowell/src/hblog

COPY docker_run_init.sh /tmp/
