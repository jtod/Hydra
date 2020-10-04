# Hydra: functional computer hardware description language

Hydra is a functional computer hardware description language for
specifying the structure and behavior of digital circuits. It supports
several levels of abstraction, including logic gates, register
transfer level, datapath and control, and processors. There are tools
for simulating circuits, generating netlists, and emulating
instruction set architectures. It is an embedded domain specific
language implemented using Haskell.</p>

## Installation

The documentation for cabal is incorrect, and has been incorrect since
at least September 2019. It does not describe the .cabal file format
correctly; it describes the commands incorrectly; it describes the
locations of installed files incorrectly. The cabal user guide does
not say what version it is talking about.  Its instructions do not
work for cabal-2* or for cabal-3.2.0.0, and appear to be wrong for all
versions of the software.

As of October 2020, using cabal-3.2.0.0, the following commands
partially work:

~~~~
cabal configure
cabal build
cabal install --lib
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
