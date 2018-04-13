# Installing metaprob-in-clojure

The system is not yet packaged up for easy installation.

## Install Java

You will need Java to run Leiningen and Clojure.  I (JAR) use OpenJDK
version 8.  You need the full Java development environment, not just the JVM.

## Install Leiningen and Clojure

It is not necessary to separately install Clojure if Leiningen is
installed.  Just install Leiningen, and let it take care of installing
the right version of Clojure.

Instructions for installing Leiningen are [here](https://leiningen.org/#install), 
but for a quick install try the short script in the Makefile: `make lein`.
This script assumes that `~/bin` is on your `PATH`, and it will put the `lein` command there.

Leiningen keeps some state in the `~/.lein` directory.

I interact with Clojure either using a REPL under emacs, or from the
shell using the `lein` command.  Tests (meaning just about any kind
computation that one situates inside a test file) can be run either
from the REPL or from the shell.  To run particular Clojure code
directly from the shell, you can put that code in the `-main` function
in `main.clj`.  These options are described below.

## Clone the metaprob-clojure repository

There is no particular installation procedure for metaprob-clojure.
Just set the working directory to the root of a clone of the
metaprob-clojure repository.

## Emacs setup

It is possible to use metaprob-clojure exclusively from the shell, but
running a REPL under emacs can be more pleasant.  (or less.)  So this
setup is optional.  What I suggest here is not necessarily right or
best; it's just stuff I got off the Internet.  I am not an expert.

Put the following in your `.emacs` file:

    ;; From http://clojure-doc.org/articles/tutorials/emacs.html.
    (require 'package)
    (add-to-list 'package-archives
                 '("melpa-stable" . "http://stable.melpa.org/packages/")
                 t)
    ;; "Run M-x package-refresh-contents to pull in the package listing."
    (package-initialize)
    (defvar clojure-packages '(projectile
                               clojure-mode
                               cider))
    (dolist (p clojure-packages)
      (unless (package-installed-p p)
        (package-install p)))

Here is my `~/.lein/profiles.clj`; I'm not sure why it is as it is,
but it seems to be harmless:

    ; Sample profile: https://gist.github.com/jamesmacaulay/5603176
    {:repl {:dependencies [[org.clojure/tools.namespace "0.2.11"]]
            :injections [(require '(clojure.tools.namespace repl find))]
            :plugins [[cider/cider-nrepl "0.15.1"]]}}

## Gnuplot

Gnuplot must be installed in order for plotting to work.  You're on
your own for that.
