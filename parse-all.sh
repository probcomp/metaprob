#!/bin/bash

# NOTE: This must run with the metaprob virtualenv active!


set -e

mkdir -p parsings

function doit {

    root=$1

    paths=`cd ../metaprob; find $root -name "*.vnts"`

    for path in $paths; do
        f=`basename $path .vnts`
        if [ $f != propose-subtrace ]; then
            d=`dirname $path`
            mkdir -p "parsings/$d"
            dest="parsings/$d/$f.trace"
            echo "writing $dest"
            ../metaprob/pythenv.sh python python/transcribe.py -f "../metaprob/$path" > "$dest".new
            mv "$dest".new "$dest"
        fi
    done

}

doit src
doit metacirc
doit examples

 
-from metaprob.parse
