#!/bin/bash

set -e

dashtounderscore=yes

for f in `cd parsings; find . -name "*.trace"`; do
    g=parsings/$f
    d=converted/`dirname $f`
    mkdir -p $d
    base=`basename $f .trace`
    # Clojure prefers underscores in file names for some reason.
    # See bash manual under '${parameter/pattern/string}'
    if [ $dashtounderscore = yes ]; then
        t=$d/${base//-/_}.clj
    else
        t=$d/$base
    fi
    echo Converting $g '->' $t
    java -cp `cat .lein_classpath` dontknow.main $g $t.new
    mv -f $t.new $t
done
