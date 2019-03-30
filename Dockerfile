# Docker Hub maintains an official Clojure library. Using it as our base saves
# us from having to install Leiningen ourselves.

FROM clojure:lein-2.8.1

# Install curl so we can use it to download the Clojure command line tools,
# install time so we can measure how long it takes to run the examples, install
# rlwrap for use with clj, and install pip so we can install jupyter.

RUN apt-get update -qq \
      && apt-get upgrade -qq \
      && apt-get install -qq -y \
        curl \
        nodejs \
        time \
        rlwrap \
        python3-pip

# Install the Clojure command line tools. These instructions are taken directly
# from the Clojure "Getting Started" guide:
# https://clojure.org/guides/getting_started

ENV CLOJURE_VERSION 1.9.0.394
RUN curl -O https://download.clojure.org/install/linux-install-${CLOJURE_VERSION}.sh \
      && chmod +x linux-install-${CLOJURE_VERSION}.sh \
      && ./linux-install-${CLOJURE_VERSION}.sh

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
