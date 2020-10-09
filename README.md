# Hydra: functional computer hardware description language

Hydra is a functional computer hardware description language for
specifying the structure and behavior of digital circuits. It supports
several levels of abstraction, including logic gates, register
transfer level, datapath and control, and processors. There are tools
for simulating circuits, generating netlists, and emulating
instruction set architectures. It is an embedded domain specific
language implemented using Haskell.

## Installation

You need two pieces of software: ghc, a Haskell compiler, and the
Hydra source.  Both are free software, and they run on Windows,
Macintosh, and Gnu/Linux.

*Note (2020-10-09): it is planned to release Hydra on github within a
few days.  See **Releases** section on the github Hydra page.  It's
better to wait for a release, as the HEAD version is unstable.
Eventually, it is planned to release Hydra on the Hackage, the Haskell
package server.  That will simplify the installation.  For now, use
the following instructions.*

1. Install Haskell Platform (www.haskell.org).  This gives you the ghc
   compiler and the cabal package system.  Check that these are
   installed with these commands:
   
~~~~
$ ghc --version
The Glorious Glasgow Haskell Compilation System, version 8.10.2
$ cabal --version
cabal-install version 3.2.0.0
compiled using version 3.2.0.0 of the Cabal library 
~~~~

2. Download Hydra from https://github.com/jtod/Hydra -- click Releases
   and download the most recent version.  It isn't recommended to use
   the Code link; that will give you the development branch which is
   not a stable release.  The installation file is Hydra-i.j.k.zip (or
   .tgz).
   
3. Put the file somewhere in your user workspace and uppack it: on
   Linux, tar -xzf Hydra-i.j.k.tgz and on Windows use 7zip or tar.

4. Enter *make userinstall*.  Alternatively, enter the following
   commands, which update the Haskell package database from the
   Internet, compile the dependencies, and build a documentation web
   page.

~~~~
cabal update
cabal install --lib
cabal haddock
~~~~

5. Add the following to your .bashrc configuration file (but replace
   /path/to/Hydra to the path where you actually put it).  *(Why do
   you have to do this?  Unfortunately, the latest version of cabal
   just compiles the code but doesn't make it visible to ghc, so it's
   difficult to use the compiled code.  But you can run circuits
   directly using the Hydra source files, as shown below.)*
   
~~~~
export HYDRA=/path/to/Hydra
alias hydra="ghc -i${HYDRA}/src -e main"
alias hydrai="ghci -i${Hydra}/src"
~~~~

6. Test that it's working.  In your shell, go to Hydra/examples/adder
   and enter:

~~~~
hydra Add4Run
~~~~

This simulates the **Add4.hs** circuit using test data defined in
**Add4Run.hs**, and it should produce the following output:

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

* The [User Guide](./docs/userguide/HydraUserGuide.html) describes how
  to install and use the system. It contains an introductory tutorial,
  a number of examples, and discusses techniques for designing
  circuits. If the link is broken, there is a [plain text
  version](docs/userguide/HydraUserGuide.org).

* The *Library interface reference* specifies the API, including
  circuit and function types.  This can be found in the
  Hydra/dist-newstyle directory, with a URL similar to the following.
  The URL should be just one long line, but here it's broken into
  pieces:
~~~~  
file://path/to/Hydra/dist-newstyle/
build/x86_64-windows/ghc-8.10.2/Hydra-2.4.1/
doc/html/Hydra/index.html
~~~~

* [Hydra.cabal](./Hydra.cabal) defines metadata for the software.

## About Hydra

* Author: John T. O'Donnell, School of Computing Science, University
  of Glasgow
* Copyright (c) 2020 John T. O'Donnell
* License: This software is free and open source, using the GPL-3
  license.  See LICENSE.txt.
* Hydra web page: https://github.com/jtod/Hydra
* Author's web page: https://jtod.github.io/index.html
* Version: see Hydra.cabal
