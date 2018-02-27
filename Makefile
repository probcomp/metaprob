
all:
	bash bin/parse-all

# this target is referenced in README.md
lein:
	wget "https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein"
	chmod +x lein
	mv lein ~/bin
	lein

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
mapl:
	lein compile :all
	time lein run -m metaprob.mapl2018.main

histograms:
	bin/gnuplot-hist results/samples_from_the_prior.samples
	bin/gnuplot-hist results/samples_from_the_target.samples 
	bin/gnuplot-hist results/samples_from_importance_sampling_with_20_particles.samples 

tags:
	etags --language=lisp `find src -name "*.clj"` `find test -name "*.clj"`
