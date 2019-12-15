FROM rlpowell/stack

# If you make changes here, run ./stop_container.sh to have them
# actually take effect

RUN sudo yum install -y mailx exim wdiff colordiff

USER rlpowell
RUN sudo mkdir -p /web
WORKDIR /home/rlpowell/src/hblog
ENV PATH="/home/rlpowell/.local/bin/:${PATH}"

COPY hbgwdiff.sh /tmp/
