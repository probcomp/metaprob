
all:
	bash bin/parse-all

# this target is referenced in README.md
lein:
	wget "https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein"
	chmod +x lein
	mv lein ~/bin
	lein

# Incudes long-running tests
test:
	lein test
	time lein run -m metaprob.examples.main test
# 'test' is a directory name
.PHONY: test

# Create directory of .trace files from .vnts files
# NOTE: This must run with the metaprob virtualenv active!
parse: ../metaprob/pythenv.sh python/transcribe.py
	bin/parse-all

# Create directory of .clj files from .trace files
convert: src/metaprob/main.clj src/metaprob/to_clojure.clj .lein_classpath
	lein compile :all
	bin/convert-all

# General rule for converting a .vnts (metaprob) file to a .trace file
# NOTE: This must run with the metaprob virtualenv active!
%.trace: %.vnts
	../metaprob/pythenv.sh python python/transcribe.py -f $< $@.new
	mv -f $@.new $@

# General rule for converting a .trace file to a .clj file
%.clj: %.trace .lein_classpath
	java -cp `cat .lein_classpath` metaprob.main $< $@.new
	mv -f $@.new $@

# By setting java's classpath explicitly, instead of relying on 'lein'
# to do it for us, we cut the number of Java VM startups in half.
# This is a significant speedup.
# I got this hack from stackoverflow.
.lein_classpath:
	lein classpath > $@

# If you get errors with 'lein compile :all' try just 'lein
# compile'. I don't understand the difference.
# To change number of samples, pass the number on the command line.
# 2000 is a good number, but it takes hours to run.
# 5 is good for smoke tests.  To get more, you can say e.g.
#  make view COUNT=100
COUNT=10
exa: results/samples_from_the_prior.samples

SAMPLES=results/samples_from_the_gaussian_demo_prior.samples

$(SAMPLES):
	mkdir -p results
	lein compile 
	time lein run -m metaprob.examples.main $(COUNT)

$(SAMPLES).png: $(SAMPLES)
	for f in results/*.samples; do bin/gnuplot-hist $$f; done

view: $(SAMPLES).png
	open results/*.png

# suppress '.#foo.clj' somehow
tags:
	etags --language=lisp `find src -name "[a-z]*.clj"` `find test -name "[a-z]*.clj"`
