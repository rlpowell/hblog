FROM rlpowell/stack

USER rlpowell
WORKDIR /home/rlpowell/src/hblog

COPY docker_run_init.sh /tmp/
# These two come out of Docker_Shell; see run_docker for how they get here
COPY docker_run_init_start.sh /tmp/
COPY docker_run_init_end.sh /tmp/
