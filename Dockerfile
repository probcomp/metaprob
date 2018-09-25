FROM jupyter/minimal-notebook
MAINTAINER MIT Probabilistic Computing Project <https://github.com/probcomp>

# Switch to root so we can install things
USER root

RUN sudo apt-get update -qq \
      && apt-get upgrade -qq \
      && apt-get install -qq -y \
        curl \
        leiningen

RUN curl -O https://download.clojure.org/install/linux-install-1.9.0.394.sh
RUN chmod +x linux-install-1.9.0.394.sh
RUN ./linux-install-1.9.0.394.sh

# Switch off root now that we've installed everything
USER $NB_USER

# Create the metaprob-clojure directory within the container
RUN mkdir -p /home/$NB_USER/metaprob-clojure

COPY ./deps.edn /home/$NB_USER/metaprob-clojure
COPY ./project.clj /home/$NB_USER/metaprob-clojure
WORKDIR /home/$NB_USER/metaprob-clojure

# Download and cache deps in ~/.m2
RUN clojure -Stree

# Install the Jupyter kernel
RUN lein jupyter install-kernel

CMD lein deps && lein jupyter lab