#!/bin/bash

set -e

for f in `cd parsings; find . -name "*.trace"`; do
    g=parsings/$f
    d=converted/`dirname $f`
    mkdir -p $d
    t=$d/`basename $f .trace`.clj
    echo Converting $g '->' $t
    java -cp `cat .lein_classpath` dontknow.main $g $t
done
