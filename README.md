# Hydra: functional computer hardware description language

Hydra is a functional computer hardware description language for
specifying the structure and behavior of digital circuits. It supports
several levels of abstraction, including logic gates, register
transfer level, datapath and control, and processors. There are tools
for simulating circuits, generating netlists, and emulating
instruction set architectures. It is an embedded domain specific
language implemented using Haskell.

## Installation

- Note (2020-10-06): it is planned to release Hydra on github within a
  few days (see Releases section on the github Hydra page).  It's
  better to wait for a release (the HEAD version may be unstable).

- Later, it is planned to release Hydra on the Hackage, the Haskell
  package server.  That will simplify the installation.  For now,
  follow the following instructions.

1. Install Haskell Platform (www.haskell.org).  This gives you the ghc
   compiler and the cabal package system.  Check that these are
   installed.  This version of Hydra was tested using these versions:
   
~~~~
$ ghc --version
The Glorious Glasgow Haskell Compilation System, version 8.10.2
$ cabal --version
cabal-install version 3.2.0.0
compiled using version 3.2.0.0 of the Cabal library 
~~~~

2. Download Hydra from https://github.com/jtod/Hydra
   Click Releases and download the most recent release.  (It isn't
   recommended to use the Code link; that will give you the HEAD
   branch which is not a stable release.)
   
3. Unpack the file and enter the directory.

4. Issue the following commands.  These update the Haskell package
   database from the Internet, compile the dependencies, and build a
   documentation web page.

~~~~
cabal update
cabal install --lib
cabal haddock
~~~~

5. You can run circuits using Hydra directly from the source files.
   To make this easier, add the following to your .bashrc
   configuration file (but replace /path/to/Hydra to the path where
   you actually put it!)
   
~~~~
export HYDRA=/path/to/Hydra
alias hydra="ghc -i${HYDRA}/src -e main"
~~~~

6. Test that it's working.  In your shell, go to Hydra/examples/adder
   and enter:

~~~~
hydra Add4Run
~~~~

This simulates the Add4.hs circuit using test data defined in
Add4Run.hs, and it should produce the following output:

~~~~
$ hydra Add4Run
  x =  5  y =  8  cin = 0    ==>    cout = 0  s = 13
  x =  7  y =  3  cin = 0    ==>    cout = 0  s = 10
  x =  8  y = 12  cin = 0    ==>    cout = 1  s =  4
  x =  8  y =  1  cin = 0    ==>    cout = 0  s =  9
  x = 12  y =  1  cin = 1    ==>    cout = 0  s = 14
  x =  2  y =  3  cin = 1    ==>    cout = 0  s =  6
  x = 15  y = 15  cin = 1    ==>    cout = 1  s = 15
$ 
~~~~

## Documentation

* The [User Guide](./doc/userguide/html/index.html) describes how to
  install and use the system. It contains an introductory tutorial, a
  number of examples, and discusses techniques for designing
  circuits. It explains how the system is implemented and how it
  works.  If the link is broken, here is a [plain text
  version](./src/docsrc/indexsrc.m4).

* The [Library interface reference](./dist/doc/html/Hydra/index.html)
  is specifies the API, including circuit and function types.  If the
  link is broken, see the Installation section in the User Guide.

* [Hydra.cabal](./Hydra.cabal) defines metadata for the software.

## About Hydra

Copyright (c) 2020 John T. O'Donnell.  This software is free and open
source; see LICENSE.txt.

--------  -----------------------------------------------------
author:   John T. O'Donnell
email:    john.t.odonnell9@gmail.com
web:      www.dcs.gla.ac.uk/~jtod/
          School of Computing Science, University of Glasgow,
          Glasgow G12 8QQ, United Kingdom
--------  -----------------------------------------------------
