# Docker Hub maintains an official Clojure library. Using it as our base saves
# us from having to install Leiningen ourselves.

FROM clojure:lein-2.8.1

# Install curl so we can use it to download the Clojure command line tools,
# install time so we can measure how long it takes to run the examples, install
# rlwrap for use with clj, and install pip so we can install jupyter, and
# install cmake and xxd so we can build Planck.
RUN apt-get update -qq \
      && apt-get upgrade -qq \
      && apt-get install -qq -y \
        cmake \
        curl \
        nodejs \
        time \
        rlwrap \
        python3-pip \
        xxd

# Install Node so we can run our tests in JVM-hosted Clojurescript mode.

RUN ln -s /usr/bin/nodejs /usr/bin/node

# Install the Clojure command line tools. These instructions are taken directly
# from the Clojure "Getting Started" guide:
# https://clojure.org/guides/getting_started

ENV CLOJURE_VERSION 1.9.0.394
RUN curl -O https://download.clojure.org/install/linux-install-${CLOJURE_VERSION}.sh \
      && chmod +x linux-install-${CLOJURE_VERSION}.sh \
      && ./linux-install-${CLOJURE_VERSION}.sh

# Install Planck so we can run our tests in self-hosted mode.

RUN apt-get update \
        && apt-get install -y --no-install-recommends apt-utils \
        && apt-get install -qq -y \
          libjavascriptcoregtk-4.0 \
          libglib2.0-dev \
          libzip-dev \
          libcurl4-gnutls-dev \
          libicu-dev

RUN git clone https://github.com/planck-repl/planck.git \
        && cd planck \
        && git fetch --all --tags \
        && git checkout tags/2.21.0 \
        && script/build --fast \
        && script/install \
        && planck -h \
        && cd ..

# Install jupyter.

RUN pip3 install jupyter

# Create a new user to run commands as per the best practice.
# https://docs.docker.com/develop/develop-images/dockerfile_best-practices/#user
# Use --no-log-init to work around the bug detailed there.

RUN groupadd metaprob && \
    useradd --no-log-init -m -g metaprob metaprob

# Switch users early so files created by subsequent operations will be owned by the
# runtime user. This also makes it so that commands will not be run as root.

USER metaprob

ENV METAPROB_DIR /home/metaprob/projects/metaprob-clojure
RUN mkdir -p $METAPROB_DIR
WORKDIR $METAPROB_DIR

# Retrieve our dependencies now in order to reduce the time it takes for the
# notebook to start when the image is run.

COPY --chown=metaprob:metaprob ./deps.edn $METAPROB_DIR
COPY --chown=metaprob:metaprob ./project.clj $METAPROB_DIR
RUN clojure -e "(clojure-version)"

# downgrade tornado.
# see https://stackoverflow.com/questions/54963043/jupyter-notebook-no-connection-to-server-because-websocket-connection-fails

USER root
RUN pip3 uninstall -y tornado
RUN pip3 install tornado==5.1.1

USER metaprob

RUN lein jupyter install-kernel

# Copy in the rest of our source.

COPY --chown=metaprob:metaprob . $METAPROB_DIR
