FROM rlpowell/stack

RUN sudo yum install -y mailx exim

USER rlpowell
RUN mkdir -p /home/rlpowell/src/hblog
WORKDIR /home/rlpowell/src/hblog

COPY --chown=1000 docker_run_init.sh /tmp/
# These two come out of Docker_Shell; see run_docker for how they get here
COPY --chown=1000 docker_run_init_start.sh /tmp/
COPY --chown=1000 docker_run_init_end.sh /tmp/
RUN chmod 755 /tmp/docker_run_*.sh
