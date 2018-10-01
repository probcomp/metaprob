# Installing metaprob-in-clojure

The system is not yet packaged up for easy installation.

## Install Java

You will need Java 1.8 or later in order to run Leiningen, Clojure,
and Metaprob.  Personally I (JAR) use JRE version 1.8.0_05.  You need
the full Java development environment (JDK), not just the JVM.

Check you local Java version with

    java -version

## Clone the metaprob-clojure repository

Clone this repository from github, and set the working directory to
the root of the clone:

    git clone git@github.com:probcomp/metaprob-clojure.git
    cd metaprob-clojure

Use `https` if the above method (ssh) doesn't work:

    git clone https://github.com/probcomp/metaprob-clojure.git
    cd metaprob-clojure

## Install the Clojure command line tools and Leiningen

To use Metaprob and run the tutorials you will need to install both the Clojure
command line tools and Leiningen. It is not necessary to separately install
Clojure, because it will be retrieved as-needed by these tools. Metaprob
requires Clojure 1.9 or later.

### MacOS

On MacOS we recommend you first [install Homewbrew](https://brew.sh/) and then
use it to install both the Clojure command line tools and Leiningen like so:

1. `brew update`
2. `brew upgrade`
3. `brew install clojure`
4. `brew install leiningen`

### Linux

To install the Clojure command line tools follow the instructions for your
system in the [Clojure getting started
guide](https://clojure.org/guides/getting_started#_clojure_installer_and_cli_tools).

For a quick Leiningen installation, just do `make`:

    make

This creates `bin/lein` which can be used as a shell command.  If you
want to be able to say just `lein`, you'll need to put it in your
PATH.  E.g. if you have a `~/bin` directory in your PATH, try this:

    ln -sf $PWD/bin/lein ~/bin/

Leiningen keeps some state in the `~/.lein` directory.

Full instructions for installing Leiningen are
[here](https://leiningen.org/#install).

The `make` that you run for this purpose also creates a file
`.lein_classpath`, which is used to speed up Java invocation in some
cases.


## Emacs setup

It is possible to use metaprob-in-clojure exclusively from the shell,
but running a REPL is better in a supervised environment where you
have a transcript, can search, can get to source code easily, and so
on.  This observation is independent of your choice of text editor.
There may be Clojure support for `vim` and `eclipse` support; I
don't know.  Clojure support under emacs is pretty good, in case you
know emacs or are willing to learn.

Following is some information on setting up emacs for Clojure.  What I
suggest here is not necessarily right or best; it's just stuff I got
off the Internet.

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

Here is my `~/.lein/profiles.clj`.  I'm not sure why it is as it is,
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
