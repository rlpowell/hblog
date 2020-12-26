FROM haskell:8.0

# The overall structure of this Dockerfile is "build up progressive
# layers with the goal that changes down near the bottom should be
# much more frequent than changes near the top"; the goal is to
# minimize rebuild times over the long run.

# Basic setup
RUN apt-get -y update

RUN apt-get -y install pkg-config libpcre3 libpcre3-dev rsync unzip wdiff heirloom-mailx

RUN git config --global user.email "robinleepowell@gmail.com"
RUN git config --global user.name "Robin Lee Powell"

WORKDIR /opt/hblog

# Start with a minimum stack config to do some bootstrapping
COPY hblog/stack-minimal.yaml stack.yaml

COPY hblog/package-minimal.yaml package.yaml

RUN stack upgrade

RUN ln -sf /root/.local/bin/stack /usr/local/bin/stack

RUN mkdir -p /tmp/.stack-work
RUN rm -rf .stack-work ; ln -sf /tmp/.stack-work

# Install most of our dependencies; do it before the package.yaml
# and stack.yaml files are fully in place so we don't have to re-do
# this basically ever
RUN mkdir -p /opt/upstreams/hakyll-4.9.8.0 /opt/upstreams/pandoc-2.0 /opt/upstreams/pandoc-citeproc-0.10.5.1
RUN sed -i '/upstreams/s/^/#/' stack.yaml
RUN stack install --only-dependencies

COPY pandoc /opt/upstreams/pandoc-2.0
RUN tar -zcf /opt/upstreams/pandoc-2.0.tar.gz /opt/upstreams/pandoc-2.0

COPY pandoc-citeproc /opt/upstreams/pandoc-citeproc-0.10.5.1
RUN tar -zcf /opt/upstreams/pandoc-citeproc-0.10.5.1.tar.gz /opt/upstreams/pandoc-citeproc-0.10.5.1

COPY hakyll /opt/upstreams/hakyll-4.9.8.0
RUN tar -zcf /opt/upstreams/hakyll-4.9.8.0.tar.gz /opt/upstreams/hakyll-4.9.8.0

RUN sed -i '/upstreams/s/^#//' stack.yaml
RUN stack install --only-dependencies

COPY hblog/hbgwdiff.sh /tmp/

# Install the rest of the dependencies
COPY hblog/package.yaml .

COPY hblog/stack.yaml .

RUN stack install --only-dependencies

RUN stack install pandoc

# Do the final install and tests
COPY hblog/ /opt/hblog/

RUN ln -sf /opt/hblog/munge_files.sh /usr/local/bin/munge_files

RUN stack install

RUN ./run_tests.sh
