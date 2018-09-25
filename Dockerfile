FROM clojure:lein-2.8.1
MAINTAINER MIT Probabilistic Computing Project <https://github.com/probcomp>

RUN apt-get update -qq \
      && apt-get upgrade -qq \
      && apt-get install -qq -y \
        curl \
        python-pip

RUN curl -O https://download.clojure.org/install/linux-install-1.9.0.394.sh \
      && chmod +x linux-install-1.9.0.394.sh \
      && ./linux-install-1.9.0.394.sh

RUN pip install jupyter

RUN mkdir -p /usr/src/project
WORKDIR /usr/src/project

COPY ./deps.edn /usr/src/project
COPY ./project.clj /usr/src/project

RUN lein deps
RUN lein jupyter install-kernel

COPY . /usr/src/project

CMD lein jupyter lab