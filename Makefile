
all:
	bash parse-all.sh

# this target is referenced in README.md
lein:
	wget "https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein"
	chmod +x lein
	mv lein ~/bin
	lein

# Create directory of .trace files from .vnts files
# NOTE: This must run with the metaprob virtualenv active!
parse: ../metaprob/pythenv.sh python/transcribe.py
	./parse-all.sh

# Create directory of .clj files from .trace files
convert: src/dontknow/main.clj src/dontknow/to_clojure.clj .lein_classpath
	lein compile :all
	./convert-all.sh

# General rule for converting a .vnts (metaprob) file to a .trace file
# NOTE: This must run with the metaprob virtualenv active!
%.trace: %.vnts
	../metaprob/pythenv.sh python python/transcribe.py -f $< $@.new
	mv -f $@.new $@

# General rule for converting a .trace file to a .clj file
%.clj: %.trace .lein_classpath
	java -cp `cat .lein_classpath` dontknow.main $< $@.new
	mv -f $@.new $@

# By setting java's classpath explicitly, instead of relying on 'lein'
# to do it for us, we cut the number of Java VM startups in half.
# This is a significant speedup.
# I got this hack from stackoverflow.
.lein_classpath:
	lein classpath > $@

