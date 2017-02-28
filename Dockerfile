FROM rlpowell/stack

RUN mkdir /home/rlpowell/src/hblog/.stack-work || true
RUN mkdir /home/rlpowell/.stack || true
RUN mkdir /home/rlpowell/.local || true
RUN chown -R rlpowell /home/rlpowell/src/hblog/.stack-work || true
RUN chown -R rlpowell /home/rlpowell/.stack || true
RUN chown -R rlpowell /home/rlpowell/.local || true

USER rlpowell
WORKDIR /home/rlpowell/src/hblog

COPY docker_run_init.sh /tmp/
