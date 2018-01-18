#!/bin/bash

set -e

for f in `cd parsings; find . -name "*.trace"`; do
    g=parsings/$f
    d=converted/`dirname $f`
    mkdir -p $d
    base=`basename $f .trace`
    # Clojure prefers underscores in file names for some reason
    t=$d/${base/-/_}.clj
    echo Converting $g '->' $t
    java -cp `cat .lein_classpath` dontknow.main $g $t.new
    mv -f $t.new $t
done
