FROM rlpowell/stack

RUN sudo yum install -y mailx exim

USER rlpowell
RUN mkdir -p /home/rlpowell/src/hblog
WORKDIR /home/rlpowell/src/hblog
ENV PATH="/home/rlpowell/.local/bin/:${PATH}"

COPY container_run_init.sh /tmp/
RUN sudo chown 1000 /tmp/container_run_init.sh
# These two come out of Docker_Shell; see run_docker for how they get here
COPY container_run_init_start.sh /tmp/
RUN sudo chown 1000 /tmp/container_run_init_start.sh
COPY container_run_init_end.sh /tmp/
RUN sudo chown 1000 /tmp/container_run_init_end.sh
RUN chmod 755 /tmp/container_run_*.sh
