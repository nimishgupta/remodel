remodel
==============================

make using MD5 instead of timestamps

_remodel_ is a dependency management program. Dependencies are
specified in a file with the following syntax.

target1 [ , target2 ... ] <- source1 [ , source2 ... [ : "command" ] ]

By default _remodel_ looks for (r|R)emodelfile. _remodel_ can be made
to use arbit filenames using _-f_ switch.  _remodle_, by default
tries to execute as many dependencies in parallel as it can. The amount
of parallelism can be controlled using _-j_ command line switch. _remodel_
should be executed in the directory in which all dependencies are assumed
to reside in rules file.

Usage: remodel [ options ] [ target ] ...
Options:
  -j  To specify the number of jobs to execute in parallel.
  -f  To specify a file other than (r|R)emodelfile
  -B  To force rebuild of dependencies


=============================

Building and Installing _remodel_

remodel is written in ocaml. The dependencies required to build ocaml are

  * ocaml v4.00.1
  * ocamlgraph

[opam](http://opam.ocaml.org/doc/Quick_Install.html) can be used to manage compiler versions and library dependencies.


Binary name : _remodel.native_
Build Instructions :-

  * ocaml setup.ml -configure
  * ocaml setup.ml -build
  
  Optional
  * ocaml setup.ml -install
