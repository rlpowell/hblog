FROM haskell:7.10.2

# Initial Setup, Basic Package Installs
RUN DEBIAN_FRONTEND=noninteractive; apt-get update; apt-get upgrade -y
RUN DEBIAN_FRONTEND=noninteractive; apt-get install --no-install-recommends -y vim zsh build-essential git openssh-client wget curl make ca-certificates libbz2-dev libcurl4-gnutls-dev libpcre3-dev zlib1g-dev locales rsync
RUN DEBIAN_FRONTEND=noninteractive; apt-get clean
RUN /bin/echo -e 'LANG="en_US.UTF-8"\nLANGUAGE="en_US:en"\n' >/etc/default/locale
RUN echo 'en_US.UTF-8 UTF-8' >/etc/locale.gen
RUN locale-gen
RUN dpkg-reconfigure locales -fnoninteractive -plow
RUN rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# Match the rlpowell account, to maybe simplify permissions
RUN useradd -s /bin/bash -m hakyll -u 1000

# User setup
USER hakyll
ENV HOME /home/hakyll
ENV TZ America/Los_Angeles
ENV LANG en_US.UTF-8
RUN id -a
COPY zshrc /home/hakyll/.zshrc
COPY bothrc /home/hakyll/.bothrc
RUN sed -i '/PATH/d' /home/hakyll/.zshrc /home/hakyll/.bothrc
RUN sed -i '/cvim/d' /home/hakyll/.zshrc /home/hakyll/.bothrc
ENV PATH /home/hakyll/.cabal/bin:$PATH
RUN mkdir -p /home/hakyll/hblog
WORKDIR /home/hakyll/hblog

# Setup Cabal; seems to need a few tries to get it all done.
RUN cabal update
RUN cabal install cabal-install
RUN cabal update
RUN cabal install cabal-install
RUN cabal update

# Get to the point where we can run apt-get again, in case we need more stuff
USER root
RUN DEBIAN_FRONTEND=noninteractive; apt-get update
USER hakyll

# Get the package we actually want
RUN cabal install hakyll

# And some friends
USER root
RUN DEBIAN_FRONTEND=noninteractive; apt-get install --no-install-recommends -y pkg-config
USER hakyll
RUN cabal install pcre-heavy

# Mount stuff from the host
VOLUME /home/hakyll/hblog

USER hakyll

CMD ["/bin/zsh"]
