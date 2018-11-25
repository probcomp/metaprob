FROM probcomp/lein-notebook

ENV METAPROB_DIR /home/$NB_USER/metaprob
RUN mkdir -p $METAPROB_DIR
WORKDIR $METAPROB_DIR

# Retrieve our dependencies now in order to reduce the time it takes for the
# notebook to start when the image is run.
COPY ./deps.edn $METAPROB_DIR
COPY ./project.clj $METAPROB_DIR
RUN clojure -e "(clojure-version)"

# Copy in the rest of our source.
COPY . $METAPROB_DIR
