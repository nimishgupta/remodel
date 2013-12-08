remodel
==============================

make using MD5 instead of timestamps

_remodel_ is a dependency management program. Dependencies are
specified in a file with the following syntax:

target1 [ , target2 ... ] <- source1 [ , source2 ... [ : "command" ] ]

_remodel_, by default looks for (r|R)emodelfile. _remodel_ can be made
to use arbit filenames using _-f_ switch.  _remodel_, execute in parallel
as many dependencies as it can. The amount of parallelism can be controlled
using _-j_ command line switch. _remodel_ should be executed in the
directory in which all dependencies are assumed to reside in rules file.

Usage: remodel [ options ] [ target ] ...  
Options:  
  -j  To specify the number of jobs to execute in parallel.  
  -f  To specify a file other than (r|R)emodelfile.  
  -B  To force rebuild of dependencies.  
  -help To display help menu.  
  --help to display help menu.  


=============================

#### Building and Installing _remodel_

remodel is written in ocaml. The dependencies required to build _remodel_ are

  * ocaml v4.00.1
  * ocamlgraph

[opam](http://opam.ocaml.org/doc/Quick_Install.html) can be used to manage compiler versions and library dependencies.


Binary name : _remodel.native_  
Build Instructions :-  

  Execute the following commands in the project root directory  

  * ocaml setup.ml -configure
  * ocaml setup.ml -build
  
  Optional
  * ocaml setup.ml -install

===========================

#### Tests

Tests are located under _test_ directory. They can be run using 
_remodel.native_ built in the previous step. The details are as follows :  

Use command "remodel.native -f <remodelfile name> <arg> " to run a test

| remodelfile name  | arg | test description                    |
| ----------------  | --- | -----------------                   |
| nodefaultremodel  | -   | DEFAULT target missing              |
| nodefaultremodel  | c   | Rule missing to make a dependency   |
| compile/          | -   | builds example baz program          |
| multiple_targets/ | -   | builds multiple target              |
| cycle/*           | -/a | Cyclic dependency detection         |
| vanilla/          | -   | alternate actions, quick playground |
