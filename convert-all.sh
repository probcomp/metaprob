#!/bin/bash

for f in `cd parsings; find . -name "*.trace"`; do
    echo Converting $f
    d=converted/`dirname $f`
    mkdir -p $d
    java -cp $LEIN_CLASSPATH dontknow.main parsings/$f $d/`basename $f .trace`.clj
done
