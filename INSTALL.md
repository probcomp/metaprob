# Installing metaprob-in-clojure

The system is not yet packaged up for easy installation.

## Install Java

You will need Java in order to run Leiningen, Clojure, and Metaprob.
Peronsally I (JAR) use JRE version 1.8.0_05 (OpenJDK I think?).  You need the
full Java development environment, not just the JVM.

## Clone the metaprob-clojure repository

Clone this repository from github, and set the working directory to
the root of the clone:

    git clone git@github.com:probcomp/metaprob-clojure.git
    cd metaprob-clojure

## Install Leiningen and Clojure

For a quick Leiningen installation, do `make`, followed by some
command that will put the `lein` command on your `PATH`, e.g.

    make
    ln -sf $PWD/bin/lein ~/bin/

Leiningen keeps some state in the `~/.lein` directory.

Full instructions for installing Leiningen are
[here](https://leiningen.org/#install).

If Leiningen is installed, it is not necessary to separately install
Clojure, because Leiningen will install it.  If you are not using
Leiningen, be sure you are running Clojure 1.9 or later.

## Emacs setup

It is possible to use metaprob-in-clojure exclusively from the shell,
but running a REPL is better in a better supervised environment where
you have a transcript, can search, can get to source code easily, and
so on.  There may be all sorts of Clojure support for `vim` and
`eclipse` support; I don't know.  Clojure support under emacs is
pretty good, in case you know emacs or are willing to learn.

Following is some information on setting up emacs for Clojure.  What I
suggest here is not necessarily right or best; it's just stuff I got
off the Internet.  I am not an expert.

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

Gnuplot must be installed in order for plotting to work.  If you want
to make plots and do not already have gnuplot, you are on your own
because I don't remember how I installed it.

-----

NEXT: [Using metaprob-in-clojure](doc/interaction.md)
