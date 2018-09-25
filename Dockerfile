FROM clojure:lein-2.8.1
MAINTAINER MIT Probabilistic Computing Project <https://github.com/probcomp>

RUN apt-get update -qq \
      && apt-get upgrade -qq \
      && apt-get install -qq -y \
        curl \
        python3-pip

RUN curl -O https://download.clojure.org/install/linux-install-1.9.0.394.sh \
      && chmod +x linux-install-1.9.0.394.sh \
      && ./linux-install-1.9.0.394.sh

RUN pip3 install jupyter

RUN groupadd metaprob && useradd -m -g metaprob metaprob
RUN chown -R metaprob /home/metaprob
USER metaprob

WORKDIR /home/metaprob
COPY --chown=metaprob:metaprob ./deps.edn /home/metaprob
COPY --chown=metaprob:metaprob ./project.clj /home/metaprob
RUN lein deps

RUN lein jupyter install-kernel

COPY --chown=metaprob:metaprob . /home/metaprob
