# Hydra: functional computer hardware description language

Hydra is a functional computer hardware description language for
specifying the structure and behavior of digital circuits. It supports
several levels of abstraction, including logic gates, register
transfer level, datapath and control, and processors. There are tools
for simulating circuits, generating netlists, and emulating
instruction set architectures. It is an embedded domain specific
language implemented using Haskell.  This is free and open source
software released under the GPL-3 license.

This is version 2.4.2.  See https://github.com/jtod/Hydra for the
latest release.

## Installation

Before using Hydra, you need some standard software tools: a *text
editor*, a *shell*, and the *ghc Haskell compiler suite*.  All of the
software described here is free and open source, and all of it runs on
Windows, Macintosh, and Gnu/Linux.

### Text editor

You'll need to edit text files, both to configure the installation,
and also to develop your circuit specification source code.  A text
editor allows you to write and modify plain text characters.  It is
quite different from a word processor, which inserts invisible
formatting commands.

You can use any text editor you like; the choice is personal
preference.  There are some text editors aimed at beginners, such as
Notepad.  There are two standard text editors popular among serious
software developers: emacs and vim.  It's also possible to get word
processors, such as Word, to edit text, but you have to be careful
about saving your document as plain text.  A final alternative is to
use an integrated development environment.  The choice is yours.

### Shell

A shell is a window where you interact with software using text
commands.  There are many shells; they are largely similar but use
different syntax.  Bash is available on every platform, and has become
a defacto standard.  Windows has several shells of its own, including
Command Prompt and PowerShell, but you can also use bash on Windows.
You can use any shell you like, but the following instructions assume
bash.

Instead of a shell, it's also possible to use a GUI (graphical user
interface), but setting that up is a little more complicated, and the
GUI is less powerful than a shell.  This section assumes you're using
a shell.

Whatever shell you use, you need to learn some basic commands: how to
change the directory, list the files in the directory, etc.  Also, you
need to set the environment variables so the operating system can find
your software.

Bash comes pre-installed on Macintosh and Gnu/Linux.  It is available
for Windows but isn't there by default.  Here is a tutorial on [how to
install bash on
Windows:](https://itsfoss.com/install-bash-on-windows/).  Another
approach is to [install Cygwin](http://www.cygwin.com/), which
provides an entire Linux suite of software on Windows.

There are some system environment variables that tell the operating
system how to find your installed software.  If you're using bash,
this is done by editing a file named *.bashrc* which is normally in
your home directory.

### ghc compiler

Install Haskell Platform (www.haskell.org).  This gives you the ghc
compiler, the cabal package system, some standard libraries, and some
additional tools.   Check that these are installed with these commands:
   
~~~~
$ ghc --version
The Glorious Glasgow Haskell Compilation System, version 8.10.2
$ cabal --version
cabal-install version 3.2.0.0
compiled using version 3.2.0.0 of the Cabal library 
~~~~

### Hydra

1. Download Hydra from https://github.com/jtod/Hydra -- click Releases
   and download the most recent version.  It isn't recommended to use
   the Code link; that will give you the development branch which is
   not a stable release.  The installation file is Hydra-i.j.k.zip (or
   .tgz), where i.j.k is the version number.
   
2. Put the file somewhere in your user workspace and uppack it: on
   Linux, tar -xzf Hydra-i.j.k.tgz and on Windows use 7zip or tar.

3. Enter *make userinstall*.  Alternatively, enter the following
   commands, which update the Haskell package database from the
   Internet, compile the dependencies, and build a documentation web
   page.

~~~~
cabal update
cabal install --lib
cabal haddock
~~~~

4. Add the following to your .bashrc configuration file, but replace
   /path/to/Hydra to the path where you actually put it.  These
   definitions enable ghc to find the Hydra source.
   
~~~~
export HYDRA=/path/to/Hydra
alias hydra="ghc -i${HYDRA}/src -e main"
alias hydrai="ghci -i${HYDRA}/src"
~~~~

5. Test that it's working.  In your shell, go to Hydra/examples/adder
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
