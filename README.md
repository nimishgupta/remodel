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

### Building and Installing _remodel_

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

### Tests

Tests are located under _test_ directory. They can be run using 
_remodel.native_ built in the previous step. The details are as follows :  

Use command "remodel.native [ -f  <remodelfile name> ] arg" to run a test

| remodelfile name  | arg | test description                    |
| ----------------  | --- | -----------------                   |
| nodefaultremodel  | -   | DEFAULT target missing              |
| nodefaultremodel  | c   | Rule missing to make a dependency   |
| compile/          | -   | builds example baz program          |
| multiple_targets/ | -   | builds multiple target              |
| cycle/*           | -/a | Cyclic dependency detection         |
| vanilla/          | -   | alternate actions, quick playground |


=========================================

### Working

#### Dependency Management

_remodel_ parses the input file to form a dependency graph. Unless any particular target is
specified on command line, _remodel_ looks for a _DEFAULT_ target in rules file to build.


#### Determining which target needs to be rebuild

_remodel_ maintains a record of md5 digest of files in file _index.rmd_ located in a hidden
directory _.remodel_. Using entries in this record, _remodel_ is able to determine if a file
has changed from its previous build. Before triggering any command, current digest of file
is computed and is matched with digest entry of the file. Target is rebuilt if the digests
 do not match.

Upon changing any command to build the target, _remodel_ does not rebuilds the target on the
next run. The developer, if required, can force _remodel_ to rebuild target using  

  _remodel.native_ -B _target_


#### Maintaining index.rmd record

_index.rmd_ is a plain text file that contains dependency file path and its md5 digest. Only
dependency files that exist in rules file are present in _index.rmd_.  _remodel_ checks for 
existence of _.remodel/index.rmd_ on every run. If it is not found then _remodel_ creates
one, builds the target specified and updates index.rmd with new digest. As dependency files
are changed, their md5 is updated in this file.


Over time as project evolves, some files may be renamed. This renaming may lead to garbage
entries in _index.rmd_. _remodel_ takes care of this and prunes non-existent entries after
a successfull build.


#### Parallelism

_remodel_ determines which rules can be executed independent of each other and executes them
in parallel. The degree of parallelism can optionally be controlled using "-j" command line
switch. _remodel_ assigns a logical time to every dependency and executes dependency with
same logical timestamp in parallel.


#### Exclusiveness

In a directory, _remodel_ maintains exclusiveness by taking a lock on _index.rmd_ file so that
another _remodel_ process cannot clobber the project directory.
