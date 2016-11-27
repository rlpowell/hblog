FROM rlpowell/stack

USER rlpowell
WORKDIR /home/rlpowell/src/hblog

# This is where .stack-work points to
RUN mkdir -p /tmp/hbsw
