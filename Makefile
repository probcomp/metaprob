all: bin/lein .lein_classpath
	@echo "Good to go!"

# This target is referenced in README.md
bin/lein:
	wget "https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein"
	mv lein bin/lein
	chmod +x bin/lein
	@echo "Please add the 'lein' command to you PATH, e.g."
	@echo ln -sf $$PWD/bin/lein ~/bin/lein

# By setting java's classpath explicitly, instead of relying on 'lein'
# to do it for us, we cut the number of Java VM startups in half.
# This is a significant speedup.
# I got this hack from stackoverflow.
.lein_classpath: bin/lein
	bin/lein classpath > $@

# Incudes long-running tests
test:
	bin/lein test
	time bin/lein run -m metaprob.examples.main test
# 'test' is a directory name
.PHONY: test

# Create directory of .trace files from .vnts files.
# Requires python-metaprob.
# NOTE: This must run with the metaprob virtualenv active!
parse: ../metaprob/pythenv.sh python python/transcribe.py
	bin/parse-all

# Create directory of .clj files from .trace files
convert: src/metaprob/main.clj src/metaprob/to_clojure.clj .lein_classpath
	bin/lein compile :all
	bin/convert-all

# General rule for converting a .vnts (metaprob) file to a .trace file.
# The rule is for documentation purposes; I don't think it's used.
# Requires python-metaprob.
# NOTE: This must run with the metaprob virtualenv active!
%.trace: %.vnts
	../metaprob/pythenv.sh python python/transcribe.py -f $< $@.new
	mv -f $@.new $@

# General rule for converting a .trace file to a .clj file
# The rule is for documentation purposes; I don't think it's used.
%.clj: %.trace .lein_classpath
	java -cp `cat .lein_classpath` metaprob.main $< $@.new
	mv -f $@.new $@

# If you get errors with 'lein compile :all' try just 'lein
# compile'. I don't understand the difference.
# To change number of samples, pass the number on the command line.
# 2000 is a good number, but it takes hours to run.
# 5 is good for smoke tests.  To get more, you can say e.g.
#  make view COUNT=100
COUNT=10
exa: results/samples_from_the_gaussian_demo_prior.samples

SAMPLES=results/samples_from_the_gaussian_demo_prior.samples

$(SAMPLES):
	mkdir -p results
	time bin/lein run -m metaprob.examples.main $(COUNT)

$(SAMPLES).png: $(SAMPLES)
	for f in results/*.samples; do bin/gnuplot-hist $$f; done

view: $(SAMPLES).png
	open results/*.png

# suppress '.#foo.clj' somehow
tags:
	etags --language=lisp `find src -name "[a-z]*.clj"` `find test -name "[a-z]*.clj"`

# Targets for manipulating Docker below. NB_UID must be set to the current user
# for each of these because the base Docker image that we rely on requires it.
# For more information refer to the Jupyter Docker Stacks documentation:
# https://jupyter-docker-stacks.readthedocs.io/en/latest/using/common.html#docker-options

NB_UID := $(shell id -u)

.PHONY: docker-build
docker-build:
	@NB_UID=${NB_UID} docker build -t probcomp/metaprob-clojure .

.PHONY: docker-test
docker-test:
	@NB_UID=${NB_UID} docker-compose run notebook bash -c "lein test && time lein run -m metaprob.examples.main test"

.PHONY: notebook
notebook:
	@NB_UID=${NB_UID} docker-compose up
