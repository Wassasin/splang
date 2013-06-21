splang
======

Simple Programming Language compiler in Haskell

![Example of output](http://i.imgur.com/s4WgGJH.png)
![Friendly error messages](http://i.imgur.com/ChGOKrN.png)

Dependencies
============

`splang` can be compiled using `GHC 7.4.2` or higher.

Compilation has been tested with the following setups:
* Ubuntu 12.10, GHC 7.4.2
* Mac OSX 10.7.5, GHC 7.4.2

To install the software required to compile `splang`, use the following commands:

```
$ cabal update
$ cabal install ansi-terminal
$ cabal install edit-distance
$ cabal install derive
```

How to use it
=============

For instructions, run:

```
$ ./run -h
```

Testing
======================

`splang` comes with a set of unittests, which can be run using:

```
./run_tests
```

Note that thee `codegen`-phase of these tests require ssm-nogui and LLVM to be installed.

Testing (SSM)
======================

Some tests use the Utrecht Simple Stack Machine without GUI, by Markus Klinik, available from
[mklinik/ssm-nogui](https://github.com/mklinik/ssm-nogui). Please deploy this package
in the parent directory of `splang`, such that the jar-file can be accessed from:

```
../ssm-nogui/build/ssm-nogui.jar
```

Testing (LLVM)
======================

Some tests use the LLVM interpreter, compiler or linker. To ensure that you are
capable to run all LLVM-related tests, please use the following commands (on Ubuntu 12.10):

```
$ sudo apt-get install llvm clang 
$ sudo ln -s <path_to_splang>/build/splang /usr/bin/local/splang
```
