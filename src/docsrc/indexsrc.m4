% Hydra User Guide
% John T. O'Donnell

m4_define(`BeginExercise',
  `<a name="exercise:$1">**Exercise**</a>
   <a href="#solution:$1">*(Go to solution)*</a>')
m4_define(`BeginSolution',
  `m4_divert(`1')<p>
  <a name="solution:$1">**Solution**</a>
  <a href="#exercise:$1">*(Go to exercise)*</a>')
m4_define(`EndSolution', `</p>m4_divert(`0')')
m4_define(`svgfig', `<object type="image/svg+xml"
  data="figures/svg/$1.svg"> </object>')

# Introduction

Hydra is a computer hardware description language (CHDL or HDL) that
enables you to design, analyse, and simulate digital circuits.  It
focuses on logic design, not on the physics of electronic components.
It is concerned with how to connect logic gates into a system, but not
with the electronic characteristics of an individual transistor.
Hydra supports *synchronous circuits*, where a clock keeps all the
flip flops synchronised with each other.

This is free and open source software; see LICENSE.txt for details.
To use the system you need the ghc compiler suite for Haskell (this is
needed to use Hydra, not just to install it).  See the sections
Getting started and Installation.

The language provides facilities that help to design complex circuits
as well as small ones. The notation is concise and readable.  Large
circuits can be described using *circuit generators*, which remove
most of the repetition in a circuit description without losing any of
the detail.  New circuit generators can be defined: you aren't limited
to the ones that are built in.

Hydra has a semantic foundation for circuits that supports the use of
*equational reasoning*.  This is a formal method that can be used to
transform a circuit to make it more efficient according to a cost
model, as well as to prove that an implementation satisfies a
specification.  In some cases, it is possible to start with an
abstract specification of the desired behavior, and to derive
mathematically a circuit that implements it.

Hydra is not suitable for all design problems.  It assumes that all
the values on wires are digital, rather than continuously varying
analogue voltages.  There are other hardware description languages
that can handle analogue circuits.

## Hardware description languages

To design a circuit, and to do anything useful with it, we need a way
to describe it.  The description can take two forms.  A
*specification* is a clear statement of what the system is intended to
do, while an *implementation* shows how the primitive operations that
are available can achieve that result.  Both the specification and
implementation need to be clear, complete, and precise.

An implementation may take the form of a diagram or a piece of text.
For digital circuits, a pictorial specification is called a *schematic
diagram* while a textual specification uses a *hardware description
language*.  Both forms can also be used for software: a textual
specification of an algorithm uses a programming language, but there
are also visual programming languages that used diagrams to describe
algorithms.

A schematic diagram is an abstract picture of a circuit.  It shows the
components and how they are connected with wires, but it does not
directly describe the circuit's function.  A schematic diagram implies
some geometric information, such as the relative placement of
components, and this geometry may be used in fabricating the circuit.

An alternative approach is to use a computer hardware description
language (CHDL or HDL).  A circuit specification using an HDL is a
text document, so it looks superficially like a computer program, but
it describes hardware rather than software.  Most hardware description
languages are based on existing programming languages intended for
software, and they model circuits using the paradigms of programming.

Schematic diagrams and hardware description languages each have their
merits.  A diagram may be easier for a beginner to understand, and
it's more obvious that a schematic describes hardware and isn't just a
computer program.  However, hardware description languages scale up
better to large and complex circuits, and fit better with software
tools that can analyse a circuit and simulate it.

## Modeling circuits as functions

Some HDLs are based on imperative programming languages, using the
assignment statement to model a state change in a circuit.  In
contrast, Hydra is based on pure functional programming, and it models
a circuit as a function that takes inputs and produces outputs. This
is natural, because a circuit and a function are both a black box that
takes some inputs and produces some outputs.  The underlying model --
circuits as functions rather than state changes as assignments -- is
the fundamental difference between Hydra and imperative HDLs.

Hydra is an example of an *embedded domain-specific language* (DSL or
EDSL).  A domain-specific language is a language intended for one
specific application domain, not for general purpose programming.
Hydra is suitable specifically for expressing algorithms as digital
circuits, but it isn't a general purpose programming language.  It is
implemented by *embedding* in Haskell, a general purpose functional
language.  This means that Hydra doesn't have a separate compiler;
instead, its implementation consists of a library of Haskell
modules. A circuit specification written in Hydra is compiled by the
Haskell compiler and linked with the Hydra library.  However, it is
best to think of Hydra as a distinct language: designing a circuit is
not the same as writing a program in Haskell, and some Haskell
programs don't correspond to circuits.

Hydra is implemented using Haskell, a general purpose functional
programming language.  Hydra is a subset of Haskell, augmented with
libraries.  A circuit specification is compiled using the Haskell
compiler, and there is no separate Hydra compiler.  However, it's
better to think of Hydra as a distinct language, and some of the
software tools will give an error message if you try to step outside
of Hydra and write general Haskell code in a circuit specification.
You don't need to know how Hydra is implemented in order to use it,
but Chapter ?? explains how the implementation works.

# Installation

You need two pieces of software: ghc, a Haskell compiler, and the
Hydra source.  Both are free software, and they run on Windows,
Macintosh, and Gnu/Linux.

  * The ghc compiler for Haskell.  To find out whether you have it,
    enter ghc --version.  If it isn't installed, go to
    www.haskell.org.  It's easiest to get the Haskell Platform, not
    just the ghc compiler.  This directory contains the installer, as
    well as the documentation and a collection of circuit examples.

  * The Hydra installation file is Hydra-i.j.k.zip (or .tgz), where
    i.j.k is the version number.  Download it somewhere in your user
    workspace and uppack it: on Linux, tar -xzf Hydra-i.j.k.tgz and on
    Windows use 7zip or tar.

The installation directory contains documentation in index.html.  Open
it in a browser; the page points to the User Guide (this document) and
the API reference.
   
You can either compile and install Hydra (the recommended approach),
or run it from the source files without compiling it.  In either case,
you need ghc.

## Compiling and installing

The easiest way to use the system is to build the library and install
it in the standard way, using the ghc compiler.

1. Open a shell and change into the installation directory.

2. Compile and install the software with this command: make install.
   Alternatively you can enter these commands separately: cabal
   configure, cabal install, cabal haddock.

To test the installation, go to Loading files.

## Running without compiling

It's possible to use the software without compiling it, using ghci to
interpret the source.  This avoids the need to have binary files
installed in the standard location, but on the other hand it's more
awkward to use the system.  Open a shell and navigate to the
installation directory, and enter these commands:

~~~~~
  ghci Hydra
  :cd your_workspace
  :load YourCircuit
~~~~~

## Building the documentation

The top level documentation page, index.html, contains links to the
User Guide and the API reference.  If either link is broken, you can
rebuild the documentation:

~~~~
  cabal haddock   Build the API reference
  make userguide  Build the User Guide (requires mp4, pandoc)
~~~~

# Hello world! Running a simulation

Let's start by running a circuit simulation.  For now, just run the
examples, and refer back to them as the subsequent sections explain
the language.  The examples directory contains a variety of circuits,
and the simplest is in examples/helloworld.

To run a circuit, two definitions are needed: the *circuit
specification*, and a *simulation driver*.  The circuit specification
states precisely the interface to the circuit, what components it
contains, and how they are connected.  The simulation driver says how
to parse the inputs using a readable input format, and how to format
the outputs to make them readable.

It's good practice to place the circuit definition and its simulation
driver in separate files.  By convention, the filename of the driver
ends in "Run".  The files in examples/helloworld are:

  * HelloWorld.hs defines a circuit named hello, which takes two input
    bits and outputs their logical and.
  
  * HelloWorldRun.hs defines a simulation driver for the circuit.
    Usually, a simulation driver file provides a main program named
    *main* which runs the simulation on test data which is also
    defined in the file.  In HelloWorldRun.hs, the test data runs the
    circuit on all possible inputs 00, 01, 10, 11.  Since the hello
    circuit is really just an and2 gate, the results should be 0, 0,
    0, 1.

There are several ways to run the simulation.  There is a graphical
user interface for Windows called WinGHCi.  Alternatively, you can use
commands in a shell.

## Graphical user interface (Windows only)

There is a graphical interface to Haskell for Windows computers called
WinGHCi. Launch this; it should be available on the Start menu under
Haskell Platform.  Then you can interact with ghci with a number of
graphical aids:

  * Open the file browser (the folder icon, or File: Open) and
    navigate to Hydra-i.j.k/examples/helloworld.

  * Click HelloWorldRun.hs.

  * Run the program by clicking the red triangle, or using Actions:
    Run "main".

## One-line command using the Haskell compiler

You can tell ghc, the Haskell compiler, to compile the circuit and
simulation driver and to execute the main in the driver, all with one
command.

~~~~
cd examples/helloworld
ghc -e main HelloWorldRun
~~~~

You are running the driver.  It isn't necessary to mention the circuit
definition file, because HelloWorldRun.hs imports the circuit.  You
need to execute this in the directory that contains the circuit and
the driver.  After running the simulation, the compiled code is
discarded.

## Separate compilation and execution

You can also compile the circuit and driver, and retain the
executable, so it can be run again without recompilation.

~~~~
ghc --make HelloWorldRun
./HelloWorldRun
~~~~

Since this saves the object code, it will only work in a directory
where you have permissions to save files.  That's fine in your user
workspace, but you might not have file write permissions in the Hydra
installation directory.

## Interacting with the Haskell interpreter

Go to the directory that contains the circuit and driver, and launch
the ghci interpreter.  Then you can give it commands, which begin with
colon (:).

~~~~
ghci
:load HelloWorldRun
:main
~~~~

This is sometimes quicker than using the ghc compiler, but the primary
advantage is that it allows interactive testing.  You aren't limited
to executing *main*; you can enter expressions interactively.

# A quick overview with examples

This section shows several examples of circuits of increasing
complexity.  You may be able to design and simulate some circuits on
your own by following and modifying these examples.  The various
design techniques are described in more detail in later sections.

## A minimal circuit: helloWorld

Files: *examples/helloworld/HelloWorld.hs* and
*examples/helloworld/HelloWorldRun.hs*

The HelloWorld circuit just contains one logic gate.

~~~~~
m4_include(`examples/helloworld/HelloWorld.hs')
~~~~~

The circuit itself is defined in a module HelloWorld, which is in the
file named HelloWorld.hs.  A module may contain any number of
definitions.  The module imports the standard library with *import
HDL.Hydra.Core.Lib*.  All circuit modules need this import; some
modules will need additional import statements.

The circuit specification has two parts: a *type declaration* which
contains the symbol :: and a *defining equation* which contains the
symbol =.

To test the circuit, we can simulate it with some inputs.  This
requires a *simulation driver* which is defined in a separate module
in the file HelloWorldRun.hs.

~~~~~
m4_include(`examples/helloworld/HelloWorldRun.hs')
~~~~~

Simulation drivers can be standalone modules containing the test data,
as in HelloWorldRun.  In this case, the name of the module should be
*Main*.  As well as importing the standard library, the module also
needs to import the circuit.

A Main module should define a function *main* which will run the
simulation.  This defines one or more actions to perform; in this case
there is only one action, which is to execute the simulation driver
(helloRun) on the circuit's inputs (testdata).

The test data itself is a list of lists.  The outer list gives the
inputs for each clock cycle:

~~~~
testdata =
  [ inputs for clock cycle 0
  , inputs for clock cycle 1
  , inputs for clock cycle 2
  , inputs for clock cycle 3
  ]
~~~~

For any clock cycle, there is a list of inputs.  Since the circuit has
two inputs, each clock cycle gets a list of two values.

~~~~
testdata =
  [ [0,  0]    -- in cycle 0, x=0 y=0
  , [0,  1]    -- in cycle 1, x=0 y=1
  , [1,  0]    -- in cycle 2, x=1 y=0
  , [1,  1]    -- in cycle 3, x=1 y=1
  ]
~~~~

The simulation driver itself begins with *helloRun input = runAllInput
input output*.  See the section on simulation drivers for more
details.  In brief, the purpose of the driver is to convert inputs and
outputs between a readable textual form and the internal signal
representations needed by the circuit.  It contains three sections.
The first section defines the input signals for the circuit (x, y).
The second section defines the output from the circuit (z).  The third
section formats what will be printed, which may include any of the
signals along with label strings.

## Connecting several logic gates: mux1

Files: *examples/mux/Mux1.hs* and *examples/mux/Mux1Run.hs*

The *multiplexer* is an example of a circuit that can be defined by
conecting several logic gates together.  It's not just an arbitrary
example: the multiplexer is one of the most important building blocks
for larger systems.  There are many varieties of multiplexer; here we
look at the 1-bit multiplexer, called *mux1*.

A multiplexer is a hardware version of the if-then-else expression,
and is used to perform conditional actions in a circuit.  It takes
three inputs: a control input *c*, and two data inputs *x* and *y*.

The idea is that the multiplexer will choose one of the data inputs
(*x* or *y*) and output it.  The data input that is not chosen is
simply ignored.  The choice is determined by the value of *c*.
Informally, the behavior of the multiplexer is:

~~~~~
mux1 c x y = if c is zero then x else y
~~~~~

(Aside: Sometimes it's helpful to describe some signals as *control
signals* because they affect what the circuit does, and to describe
others as *data signals* because they carry variable values.  Since
the behavior of the mux1 is controlled by *c*, this is a control
input.  Since *x* and *y* are just arbitrary values, these are called
data signals.  But it's important to realise that this terminology --
control and data signal -- is just a way of talking about the signals.
As far as the actual components and wires are concerned, there is no
difference between data and control.)

This if-then-else expression is a programming construct, and it
doesn't directly correspond to digital hardware.  The multiplexer can
be implemented with logic gates.

To connect a circuit to an input, write the circuit followed by the
input.  Thus *inv c* says there is an inverter, and its input is
connected to *c*, and the entire expression *inv c* denotes the output
signal produced by the inverter.  Similarly, *and2 (inv c) a* denotes
the output of an and2 gate; its first input is the output of the
inverter and its second input is *a*.

~~~~~
mux1 c x y = or2 (and2 (inv c) x) (and2 c y)
~~~~~

Here is the complete module defining the multiplexer:

~~~~~
m4_include(`examples/mux/Mux1.hs')
~~~~~

We can run it with a simulation driver that runs the circuit on all
possible inputs, so the outputs form a truth table.  It's good
practice to write the test data with clean indentation, so the inputs
line up in columns, and to include the expected outputs in comments.

~~~~~
m4_include(`examples/mux/Mux1Run.hs')
~~~~~

## Producing several outputs: halfAdd

Files: *examples/adder/HalfAdd.hs* and *examples/adder/HalfAddRun.hs*

A half adder circuit takes two inputs *x* and *y*, and produces a pair
of outputs, the carry output and the sum output.  The carry is the
logical *and* of *x* and *y*, while the sum is their exclusive *or*.
Here is the circuit specification (file *HalfAdd.hs*):

~~~~~
m4_include(`examples/adder/HalfAdd.hs')
~~~~~

The module statement gives a name to this module, and the import
statement brings in the essential Hydra library definitions.  The
circuit definition is a one-line equation which says *halfAdd* is a
circuit, gives names *x* and *y* to its inputs, and calculates the
outputs using *and2* and *xor2* logic gates.

To see the circuit working, we can simulate it.  This requires
three things, all provided in *HalfAddRun.hs*:

  * Suitable test data, expressed as a list of *[x,y]* inputs

  * A [Simulation driver](#simulation-drivers), which converts between
    human readable input and output and the internal signal
    representations.  The simulation driver is not part of the
    circuit; it's simply formatting inputs and outputs.

  * A main program that runs the simulation driver on the test data.

~~~~~
m4_include(`examples/adder/HalfAddRun.hs')
~~~~~

Run the simulation using any of the methods given above, e.g. enter
*ghc -e main HalfAddRun*.  Here is the result:

~~~~~
$ ghc -e main HalfAddRun
Input: x = 0 y = 0  Output: c = 0 s = 0
Input: x = 0 y = 1  Output: c = 0 s = 1
Input: x = 1 y = 0  Output: c = 0 s = 1
Input: x = 1 y = 1  Output: c = 1 s = 0
~~~~~

## Black box with internal signals: add4

Files: *adder/Add4.hs* and *examples/Add4Run.hs*

~~~~~
m4_include(`examples/adder/Add4.hs')
~~~~~

~~~~~
m4_include(`examples/adder/Add4Run.hs')
~~~~~



~~~~
*Main> :main
  x =  5  y =  8  cin = 0    ==>    cout = 0  s = 13
  x =  7  y =  3  cin = 0    ==>    cout = 0  s = 10
  x =  8  y = 12  cin = 0    ==>    cout = 1  s =  4
  x =  8  y =  1  cin = 0    ==>    cout = 0  s =  9
  x = 12  y =  1  cin = 1    ==>    cout = 0  s = 14
  x =  2  y =  3  cin = 1    ==>    cout = 0  s =  6
  x = 15  y = 15  cin = 1    ==>    cout = 1  s = 15
(0.00 secs, 252,808 bytes)
*Main>
~~~~

## Feedback and changing state: BSR4

Files: *BSR4.hs* and *BSR4Run.hs*

A bidirectional shift register

Define a shift register that takes an operation code op and data
inputs x, li, ri, and performs an a state change depending on op:

  * op=0 -- no state change
  * op=1 -- load input word x
  * op=2 -- shift right
  * op=3 -- shift left

The circuit uses a building block srb ("shift register block") which
has an internal state to hold the bit in that position in the word.
The inputs to an srb are an input from the left (for shifting to the
right), an input from the right (for shifting to the left), and a bit
input from the word x (for loading a word).  The circuit outputs a
triple: the left and right outputs, and the word giving the current
state of the register.  (Minor point: the left and right outputs
aren't essential, as they also appear as the most and least
significant bits of the word output, but this approach makes it easier
to connect several sr4 circuits together, and it also fits well with
the definition of the more general sr circuit below.)

The structure of the 4-bit version comes directly from the data
dependencies.

The shift register block uses a dff to hold the state, and it uses a
mux2 to determine the new value of the state.  This is either the old
value, the data bit x from a load, or the input from the left or right
in case of a shift.

~~~~~
m4_include(`examples/shift/BSR4.hs')
~~~~~

The test data and simulation driver are defined in *BSR4Run.hs*.

~~~~~
m4_include(`examples/shift/BSR4Run.hs')
~~~~~

Running the circuit produces this:

~~~~~
$ ghc -e main BSR4Run
op=01 l=0 r=0 x=9   Output lo=0 ro=0 y=0
op=00 l=0 r=0 x=0   Output lo=1 ro=1 y=9
op=11 l=0 r=0 x=0   Output lo=1 ro=1 y=9
op=11 l=0 r=1 x=0   Output lo=0 ro=0 y=2
op=00 l=0 r=0 x=0   Output lo=0 ro=1 y=5
op=01 l=0 r=0 x=4   Output lo=0 ro=1 y=5
op=10 l=1 r=0 x=0   Output lo=0 ro=0 y=4
op=10 l=0 r=0 x=0   Output lo=1 ro=0 y=a
op=10 l=0 r=0 x=0   Output lo=0 ro=1 y=5
op=10 l=1 r=0 x=0   Output lo=0 ro=0 y=2
op=00 l=0 r=0 x=0   Output lo=1 ro=1 y=9
~~~~~

# Modules and files



# Connecting components with signals

A data value in a circuit is called a *signal*.  A signal is carried
by a wire, and it transmits information from one component to another.
In logic design we don't usually care about the physical
characteristics of a wire, although these can be important at the
lower levels of chip design.  Therefore we will usually refer to
signals rather than wires.

The information carried by a signal may be represented as an
individual bit or a cluster comprising several bits.  We can also
describe circuits at a higher level, where signals represent integers
or other data types.

A bit (binary digit) can have one of two distinct values.  Several
names are commonly used for these values, including 0/1, Low/High,
False/True, and F/T.  In real hardware a bit signal is represented by
a voltage, but the precise voltage value is unimportant at the level
of logic design.  The particular names chosen for the two bit values
are also unimportant, although they can affect the readability of a
table showing the behavior of a circuit.  When Hydra prints out the
values of bit signals, it will normally use 0 and 1, but you can tell
it to use False and True, or any other names you prefer.  One
advantage of 0/1 is that they are consistent with treating a bit as a
binary digit (False/True suggest treating a bit as a Boolean).
Another advantage of 0 and 1 is that they take up only one character
and they look different.  (Try reading a table showing thousands of F
and T characters -- they can be hard to tell apart!).


## Logic gates

To design a new circuit, you need to take a set of existing circuits
and connect them with signals.  There are several libraries of
existing circuits that you can start with, and you can also define
libraries of your own circuits for further use.  The Hydra libraries
provide as primitives the standard logic gates, summarised in the
following table.

The buffer simply produces an output that is the same as the input; it
is the identify function.  The inverter outputs 0 if its input is 1,
and outputs 1 if its input is 0.

Many of the logical operations can be performed on any number of
inputs.  For example, there is the logical conjunction (*and*) of two,
three, or four inputs.  These correspond to distinct logic gates: the
*and2* gate has two input ports and there is no way to connect three
inputs to it.  Therefore Hydra doesn't have an *and* gate; it has
distinct *and2*, *and3*, *and4* gates.  This doesn't go on
indefinitely; Hydra does not define the *and5* gate or the *and73*
gate!  (A convenient way to *and* together large number of inputs is
to use *andw*.)

Component          Description
----------------   -----------------------
buf a              buffer
inv a              inverter
and2 a b           2-input and gate
and3 a b c         3-input and gate
and4 a b c d       4-input and gate
or2 a b            2-input or gate
or3 a b c          3-input or gate
or4 a b c d        4-input or gate
xor2 a b           2-input xor gate
xor3 a b c         3-input xor gate
xor4 a b c d       4-input xor gate
nand2 a b          2-input nand gate
nand3 a b c        3-input nand gate
nand4 a b c d      4-input nand gate
nor2 a b           2-input nor gate
nor3 a b c         3-input nor gate
nor4 a b c d       4-input nor gate
xnor2 a b          2-input xnor gate
xnor3 a b c        3-input xnor gate
xnor4 a b c d      4-input xnor gate

Most of these logic gates are provided for convenience, but only a few
of them are necessary.  For example, you can replace *and3 a b c* by
*and2 a (and2 b c)*.  However, logic gates with several inputs can be
fabricated on chips, they are slightly more efficient, and most
importantly, it's more readable to use *and3* rather than two *and2*
gates.

## Connecting a circuit to inputs

Suppose we have two signals named *x* and *y*, and want to connect
them to the inputs of an *or2* gate.  This is done by writing the name
of the component, followed by the names of the input signals:

~~~~~
or2 x y
~~~~~

The value of this expression is the output of the *or2* gate.  Such an
expression is called an *application* because the component is applied
to its input signals.

Each circuit takes a specific number of inputs, and an application
using that circuit must supply the corresponding number of input
signals.  Here are several applications of logic gates, each with the
right number of inputs.

~~~~~
inv x
and2 a one
xor3 p q r
nor4 a zero c d
~~~~~

## Anonymous signals

A signal may be given a name, such as *x* or *y*, although this is
optional.  You can also refer to a signal using an application of a
component to its inputs, such as *inv x*; the output of the inverter
is an anonymous signal as it has no name.

An anonymous signal is described by an expression with several tokens.
When you use it as an input to a circuit, this expression must be
enclosed by parentheses, to turn it into a single object.  For
example, suppose we want to connect the first input to an *and2* gate
to the output of an inverter whose input is *x*.  The second input to
the *and2* gate should be *y*.  Here is the correct way to write it:

~~~~~
and2 (inv x) y
~~~~~

There are two expressions following *and2*, denoting its two inputs.
The following notation would be wrong:

~~~~~
and2 inv x y   -- Wrong!
~~~~~

Here, it looks like the *and2* gate is being given three inputs, and
the first one isn't even a signal.

Parentheses are used in Hydra for grouping, just as in mathematics.
You don't need to use parentheses just to specify the arguments to a
function (that is, the inputs to a circuit).  Some programming
languages requires lots of punctuation to indicate function
application:

~~~~~
nand3 (x, and2 (p,q), z);   -- Wrong!
~~~~~

In Hydra (as in Haskell) you don't need the extra parentheses and
commas, and they will lead to error messages.  Use parentheses only
when they are necessary to get the right grouping:

~~~~~
nand3 x (and2 p q) z
~~~~~

It can be helpful to give both a schematic diagram and a textual
specification for a circuit.  Each form of description provides
insight, and having both together is often worthwhile.  It's important
to check that the two descriptions of the circuit are consistent with
each other.  To do this, check that every box in the diagram
corresponds to a circuit (function) in the text, and check that the
wires in the diagram correspond to the signals in the text.

BeginExercise(or2-and2) Write the Hydra notation for this schematic
diagram: ![](figures/xfig/andor.svg "schematic diagram")

BeginSolution(or2-and2) *x = or2 (and2 a b) c*

EndSolution

BeginExercise(inv-and2) Draw a schematic diagram for *inv (and2 a b)*.

BeginSolution(inv-and2) The diagram for inv and2 a b goes here.

EndSolution

BeginExercise(xor2-nand3) Draw a schematic diagram for *xor2 (nand3 p
q r) (or2 x y)*.

BeginSolution(xor2-nand3) The diagram for xor2-nand3 goes here.

EndSolution

## Named signals and equations

Sometimes it's useful to give a name to a signal, rather than using it
anonymously.  A named signal can be used as an input to several
different components, but an anonymous signal cannot.  Names can also
make it easier to explain the circuit, and well chosen names help
document the purpose of a signal.

A signal can be named using an equation.  The left hand side of the
equation is the name, and the right hand side is an expression that
defines the signal.  The following equation says that the output of
the *and3* gate has the name *x*.

~~~~~
x = and3 a (inv b) c
~~~~~

Sometimes the choice between anonymous and named signals is just a
matter of style.  Here is a signal defined using three anonymous
signals:

~~~~~
x = nand2 (xor2 a b) (inv (nor2 c d))
~~~~~

This can be rewritten so as to give every signal an explicit name, by
introducing additional equations:

~~~~~
x = nand2 p q
p = xor2 a b
q = inv r
r = nor2 c d
~~~~~

An equation like this is called a *defining equation*, because the
left hand side has to be a signal name whose value is defined to be
the right hand side.  It would be wrong, for example, to write

~~~~~
nand2 p q = x   -- Wrong!
~~~~~

When using *equational reasoning* you will encounter equations with a
more general form, but in defining signals, the left hand side is
always a signal name.

## Constant signals

A constant signal always carries the same value: either it is always
0, or always 1.  The names of these two constants are written as
*zero* and *one*.  Names in Hydra always begin with a lower case
letter, never with a digit.  Don't use 0/1, or T/F, or True/False in a
circuit specification; those notations have other meanings and will
lead to bizarre error messages.

# Defining new circuits

A new circuit can be designed by connecting together a number of
existing ones.  The examples given so far consist of logic gates,
which are primitive components.  To design larger scale systems, we
need the ability to define a circuit as a new *black box* component
and reuse it.  This is similar to using abstraction in a programming
language by defining a function or procedure for a commonly used
computation.  A circuit definition contains up to three parts:

  1. Circuit type (optional)

  2. Interface (mandatory)

  3. Internal signals (optional)

## Circuit type

The circuit type is covered in a later section.  It's optional,
although it is generally best to include it.  If present, the type can
be recognized by the :: symbol and a number of right arrow symbols; a
typical example is

~~~~
halfAdd :: Bit a => a -> a -> (a,a)
~~~~

## Interface

The interface gives the name of the circuit and names its inputs and
outputs.  A circuit is created with a *circuit defining equation*.
The left hand side of the equation is the name of the circuit followed
by the names of the input signals.  There may be any number of inputs.
The right hand side is an expression giving the value of the output
signal:

~~~~~
circ_name input1 input2 = expression
~~~~~

This defines a circuit whose name is *circ_name*, which takes two
inputs named *input1* and *input2*, and produces an output with the
specified signal value.  Here is an example:

~~~~~
mycirc a b c = and3 a (inv b) c
~~~~~

The input names *a*, *b*, and *c*, are local to the definition of
*mycirc*, and they can be used to calculate the value of the output.
Another circuit can connect signals with arbitrary names, or no names
at all, to the inputs of *mycirc*.

## Internal signals

This part of a definition is optional; if present it follows the
*where* keyword.

The expression that defines the circuit's output can become fairly
complicated, and it's often simpler to define it using several other
named signals.  Each of these needs a defining equation which is
inside the circuit.  To do this, write the keyword *where* after the
equation, and after the *where* you can write any number of signal
defining equations.  The general form is:

~~~~~
circuit_name input1 input2 = output
  where
    output = ...
    x = ... (internal signals...)
    y = ...
~~~~~

Here is an example of a circuit named c22 that takes three inputs and
produces one output.

~~~~~
c22 a b c = x
  where
    x = xor2 p q
    p = and2 a b
    q = or2 b c
~~~~~

The equations should be indented consistently, and there is no extra
punctuation (no curly braces, no semicolons).  The compiler determines
the structure of a definition from the indentation, not from
punctuation.  Therefore the indentation is essential, and if it's
wrong then the specification will be parsed incorrectly.



## Multiple outputs


## Feedback

A register is a circuit with an internal state, and with the ability
to load an external value into the state and to read out the state.

~~~~~
reg1 :: CBit a => a -> a -> a
reg1 ld x = r
  where r = dff (mux1 ld r x)
~~~~~

The reg1 circuit has a feedback loop: the output of the flip flop is
connected to one of the inputs to the mux1, whose output in turn is
input to the flip flop.  Hydra does not allow feedback loops in pure
combinational logic, but feedback that goes through a flip flop is
fine.  When a circuit contains a feedback loop, there will be a
circular path in the schematic diagram, and there will be circular
equations in its specification.  For the reg1 circuit. the feedback
loop can be seen in the equation which has r on both the left and
right hand side.  Thus r is being defined in terms of itself.  The way
this works, and the reason that r is well-defined, is explained in the
section on circuit semantics.

# Signal and circuit types

The *type* of a value determines what operations you can perform on
it.  This holds for hardware description just as for programming.  The
type of a signal determines what kind of information it carries, and
the type of a circuit specifies the types and organisation of its
input and output signals.

A circuit has an interface to the outside world, and an internal
organization.  To use the circuit, all we need to know about is the
interface: what inputs need to be provided and what the outputs mean.
The type expresses a useful portion of this information: it describes
the number and organization of the inputs and outputs.  The meanings
of the circuit outputs are not specified by the type; they should be
described in documentation for the circuit.  Since Hydra models a
circuit as a function, a circuit type looks just like a function type.

The type declaration for a circuit is optional, as the compiler can
work out the type for itself.  If you omit the type, your circuit will
still run.  However, there are several benefits in writing out the
type explicitly:

* The type gives useful information about the interface to the
  circuit.  Later on, if you want to use this circuit in a larger one,
  you will be more interested in the interface than the internal
  components inside the circuit.

* There is some redundancy between the type and the defining
  equation.  If there is any inconsistency between the two, the
  compiler will give a type error message.  That may be annoying, but
  at least you know that the error lies somewhere in the (small)
  specification of this one circuit.  If you omit the type
  declaration, but there is an error in the defining equation, you may
  get an error message that says, in effect, ``there is an error
  somewhere in the (large) file'', but it's up to you to figure out
  *where* the error is.

* If you do get a type error message, the compiler will do its
  best to give a helpful and informative message.  In practice,
  though, the error messages will be far more understandable if you
  include type declarations for your circuits.

If present, the type of a circuit should come immediately before the
defining equation.  Type declarations are easily recognizable: they
always contain the symbol *::*, and usually contain some arrows *=>*
and *->*.  A typical example is

~~~~
reg1 :: CBit a => a -> a
~~~~

A type declaration contains several parts:

  * The circuit name (e.g. reg1)
  * The :: symbol, read as "has type"
  * The signal class ending with => (e.g. CBit a =>)
  * The input and output signal types (e.g. a -> a)

## Signal types and classes

*Short version.* If you're writing a routine circuit and just want to
simulate it, you can just write *CBit a =>* for the signal class
constraint and then use *a* as the type for every bit signal.  In more
complicated situations, or if you want to know what this means, read
on.

When a circuit specification is executed, each signal has a specific
type.  Many types can be used, for example *Bool* or *Stream Lattice*.
The choice of type determines what happens during execution.  Some
types lead to combinational simulation, others lead to synchronous
simulation, others perform a path depth analysis, or generate a
netlist.

It's possible to define a circuit with a specific type, and if you do
this the class constraint (the part before =>) is omitted.  For
example, we could define a Bool version of the mux1 circuit (call it
halfAddB) to operate in signals of type Bool:

~~~~
halfAddB :: Bool -> Bool -> (Bool,Bool)
halfAddB x y = (and2 x y, xor2 x y)
~~~~

This is a little simpler than the standard definition halfAdd, which
(1) uses the type class constraint Bit a =>, and (2) uses *a* rather
than *Bool* as the bit signal type.

## Combinational signals: Bit a

~~~~
halfAdd :: Bit a => a -> a -> (a,a)
halfAdd x y = (and2 x y, xor2 x y)
~~~~

The main disadvantage of using Bool as the signal type is that
combinational simulation is the *only* thing you can do with the
circuit.  However, Hydra provides many other options.  For example,
you can perform synchronous simulations over many clock cycles, but to
do that, the signals must have a different type.  You can do these
other things with *halfAdd*, but not with *halfAddB*.

There is several sets of different type that can be used to represent
a signal.  Each set offers a number of operations that can be
performed on the signal.  For example, *Bit* is one of these sets; the
notation *Bit a =>* means that *a* can be any type in the set *Bit*,
and therefore all of the Bit operations can be performed on a signal
of type *a*.

The commonest signal class constraints are:

  * Bit a => is used when *a* is a bit signal in a circuit, so logic
    gates can be used.  The circuit may be either combinational or
    sequential.  The Bit class allows combinational simulation tools
    to be used, but flip flops are not allowed in the circuit.

  * CBit a => is used when *a* is a bit signal in a circuit, so logic
    gates may be used.  The circuit must be synchronous with a clock,
    and flip flops may be used.

## Clocked signals: Bit a

The signal class constraint 
Classes

Base signal types

  * Bool    (defined in Haskell standard libraries)

  * Word16  (defined in Haskell standard libraries)

  * Word32  (defined in Haskell standard libraries)

  * Lattice (defined in Hydra Core library)

## Inputs and outputs

After the signal class (i.e. after the *=>* symbol) come the types of
the inputs and output of the circuit.  In the simplest case, each
input or output signal is just a bit of type *a*.  There may be any
number of input arguments, and there must be one output result.  A
single arrow *->* must follow each input; thus the number of single
arrows in the type is the same as the number of inputs.

The inverter has one input of type *a*, which is followed by *->*, and
the type *a* of the output appears last.  The type declaration can be
read as "inv uses signals in the Bit class; it takes one input and
produces one output":
Thus the entire type declaration ``*inv :: Bit a => a -> a*'' says
``*inv* is a circuit that takes an input bit signal, and produces an
output bit signal.''

~~~~~
inv :: Bit a => a -> a
~~~~~

The notation *a -> a* means "the circuit takes an input signal and
produces an output signal".  This is similar to conventional
mathematical notation; for example in mathematics there is a function
*im* that is given a complex number (type $C$) and returns its
imaginary part (type $R$), and a mathematician might write its type as
im : C -> R.  (The reason :: is used in Haskell (and Hydra) is that :
is used for something else.)

Circuits that take several inputs have a slightly more complicated
type.  For example, here are the types for the family of and-gates:

~~~~~
and2 :: Bit a => a -> a -> a
and3 :: Bit a => a -> a -> a -> a
and4 :: Bit a => a -> a -> a -> a -> a
~~~~~

There is always one output, but any number of inputs, and every input
is followed by *->*.  To find out how many inputs a circuit takes,
just count the number of times *->* appears in its type.

If a circuit has several outputs, they must be enclosed in a
container, and this is reflected in the type.  See the section on
Containers.

# Containers

In a physical circuit, every wire carries one bit, and doesn't have
any relationship to any other wire (unless it is actually connected to
that other wire).  When we design a circuit, however, it takes several
wires to carry any data value that isn't just a Boolean.  For example,
it takes 16 wires to transmit a 16-bit word, and to the designer there
is definitely a clear relationship among these wires.

Circuits may contain large numbers of signals, and it would be
tiresome to name them all.  You can simplify the description of a
circuit by defining *containers* that hold a collection of signals.
Then you can use the container as a single object, without referring
explicitly to its components.

A design is clearer if related signals together are grouped together,
with a name for the entire collection.  For example, we could give the
name *x* to a 16-bit word, and just use *x* to refer to all the wires
collectively.

Hydra provides two kinds of container: *tuples* and *words*.
Tuples are useful for circuits that have multiple inputs and outputs;
an example of a tuple is *(x, (a,b))*.  Words are appropriate when
several signals are used to represent a number, for example
*[x0,x1,x2,x3]*.

Both kinds of container are written with several elements separated by
commas.  A quick way to tell them apart is that tuples use round
parentheses *( ...  )* but words use square brackets *[ ...  ]*.

Containers are just notations that help to simplify the description of
large circuits.  If you look at the layout of a chip under a
microscope, you won't see any tuples or words---just thousands of
individual wires and components.  A circuit specification that names
each one explicitly would be long and unreadable; containers enable us
to write compact and readable descriptions of such large circuits.


## Tuples

Tuples provide the simplest way to give a single name to a bundle of
signals.

Suppose we have a couple of signals named *a* and *b*.  They can be
collected together into a tuple by writing *(a,b)*.  The elements are
written inside round parentheses ( ... ) and separated by commas.

The elements of the tuple are expressions that describe signals.  Any
expression can be used; it doesn't have to be a signal name.  For
example, the tuple *(and2 x y, or2 x y)* is a tuple consisting of two
signals, the outputs of two logic gates.  In this example, the actual
signals in the tuple don't have names.

A tuple can have any number of elements.  Thus *(inv x, y, z)* is a
3-tuple and *(a,b,c,d)* is a 4-tuple.

If the basic signal type is *a*, as usual, then a 2-tuple has type
*(a,a)*, a 3-tuple has type *(a,a,a)*, and so on.  The type shows
explicitly the number of elements.

One of the commonest ways to use a tuple is to describe a circuit that
has several outputs.  Indeed, there is no way to do this without using
a cluster (a tuple or a word).  Recall that the type of a circuit
contains a number of arrows (*->*) and the type of the output comes
after the last arrow.  If there are actually several outputs, we need
to combine them into a cluster and give the cluster's type as the type
of the output.

Here is an example.  Suppose we want to define a circuit that has two
input bit signals, called *x* and *y*.  The circuit produces two
outputs, *and2 x y* as well as *or2 x y*.  Let's name the circuit
*aor2*.  Here is a full definition:

~~~~~
aor :: Bit a => a -> a -> (a,a)
aor x y = (and2 x y, or2 x y)
~~~~~

The definition of *aor* consists of two parts: a type declaration (the
line containing *::*), and a defining equation (thie line containing
the *=*).  In general, every circuit specification should contain
these two parts.

Notice that there are two arrows (*->*) in the type.  This means that
there are two inputs, and each has type *a* --- that is, each input is
a bit signal.  The type of the output comes after the last arrow, and
it is *(a,a)*, so the output of the circuit is a tuple containing two
bit signals.

The signal defining equations we have considered up to now have had a
signal name on the left hand side: *x = ...*.  In general, however,
the left hand side of an equation is a *pattern*.

It is also possible to have an input cluster.  The *aor* circuit above
has two inputs, and these were treated separately: there are two
arrows in the type, one after each input type.  An alternative
notation is to say that the circuit has just one input, which is a
cluster containing two elements:

~~~~~
aorTup :: Bit a => (a,a) -> (a,a)
aorTup (x,y) = (and2 x y, or2 x y)
~~~~~

Compare the definitions of *aorTup* and *aor*.  Both of them have two
input bits named *x* and *y*, but they are organized differently.  In
*aor*, the inputs are treated as separate arguments, each of type *a*,
and each followed by an arrow *->*.  In *aorTup*, the input bits are
collected together into the tuple *(x,y)* which has type *(a,a)*, and
this tuple is the sole argument.

These two circuits, *aor* and *aorTup*, are essentially the same.
They would look identical on a VLSI chip under the microscope.  The
only difference between them is the notation used to describe them.

There is an asymmetry in the notation.  If a circuit has several
inputs, there is a choice of notation: they can be treated as separate
arguments, or they can be collected together into a tuple.  However,
if a circuit has several outputs, there is no choice: they *must*
be collected together into a tuple.

This notation for types, with the arrows and the (apparently)
different treatment of circuit inputs and outputs, may look strange
and counterintuitive.  There is actually a very good reason the type
notation is designed this way, but it involves some techniques we are
not ready to discuss yet (see the chapter on design patterns).

There are other uses for tuples besides just handling circuits with
multiple outputs.  Sometimes tuples are useful just for cutting some
of the boilerplate in a specification, making it shorter and easier to
read.  Suppose we have a circuit where two signals, say *x* and *y*,
are needed as inputs to several other building block circuits *f1*,
*f2*, and *f3*.  We could write the specification with all the signals
written out explicitly:

~~~~~
circ :: Bit a => a -> a -> a
circ x y = z
  where
     p = f1 x y
     q = f2 x y
     r = f3 x y
     z = xor3 p q r
~~~~~

But we might be able to simplify this by changing the types of *circ*,
*f1*, *f2*, and *f3* to collect *x* and *y* into a tuple.

~~~~~
circ :: Bit a => (a,a) -> a
circ xy = z
  where
     p = f1 xy
     q = f2 xy
     r = f3 xy
     z = xor3 p q r
~~~~~

In a large and complicated system, this technique can make a big
difference.  For example, in a processor circuit there may be a number
of signals needed to control the arithmetic-logic unit, and those
signals travel together.  It can cut down on the notation
significantly just to combine them into a tuple, give the tuple a
name, and pass around the whole cluster without mentioning the
individual components.

Sometimes you may have a cluster, but you need to extract its elements
and give them individual names.  This can be done in a circuit black
box definition using a signal defining equation.  For example, the
following equation defines *alpha* and *beta* to be the names of the
elements of a tuple named *pair*:

~~~~~
(alpha,beta) = pair
~~~~~



Tuples can be nested.  For example, *(p, (x,y,z))* is a 2-tuple
(*not* a 4-tuple!).  The first element is *p*, and the second
element is a 3-tuple *(x,y,z)*.  The type is

~~~~~
(p, (x,y,z)) :: (a, (a,a,a))
~~~~~


This example shows a crucial property of tuples: their elements may
have different types; in this case the type of the first element is
*a* and the type of the second element is *(a,a,a)* and those types
are different, just as a physical wire is not the same thing as a
bundle of three physical wires.

Why use a tuple type like *(a,(a,a,a))* when a simple 4-tuple would
seem simpler?  The reason is that sometimes, in larger systems, a
sub-circuit produces many outputs, and groups of them will then be
connected to different destinations.  The notation to describe this is
simpler if the tuple structure matches the logical organization of the
circuit.  We will see several examples of this, especially in the
design of processors.

It is also possible to have two different signal representations in a
specification.  Each one needs its own distinct type variable name.
For example, suppose we are designing a circuit that has a basic bit
signal type *a*, but the circuit also has some values where we aren't
concerned about the bit representation (floating point numbers,
perhaps).  To abstract away from the bit representation, we could give
another type *b* to these abstract values.  Then a black box circuit
that outputs both a bit and a floating point number would have the
output type *(a,b)*.

## Words

There are two kinds of cluster that allow several signals to be
collected together into one entity.  The previous section discussed
tuples, and now we introduce words.  Tuples allow arbitrary groupings,
while words have a regular structure and their elements can be
accessed by indexing.  Words are frequently used for collections of
bits that represent binary numbers.


In a word, bit indices are 0, 1, ..., n-1 where bit 0 is most
significant.  The expression *[x0,x1,x2,x3]* denotes a word containing
the individual signals *x0*, ..., *x3*.  The syntax is similar to a
tuple; the difference is that an expression for a word uses square
brackets *[ \ ]* while a tuple uses round parentheses *( \ )*.

The basic usage of a word is similar to a tuple.  For example, a
circuit could collect several signals into a word and output that.
Here is an alternative definition of the half adder:

~~~~~
halfAddw :: Bit a -> a -> a -> [a]
halfAddw x y = [c,s]
  where
    c = and2 x y
    s = xor2 x y
~~~~~

There two differences between this definition and the one given
earlier.  First the output expression here is *[c,s]*, so it's a word,
while the output expression given for the original *halfAdd* is
*(c,s)*, which is a tuple.  The other difference is quite important:
the output type is *[a]*, rather than *(a,a)* for the original
*halfAdd*.

All the elements of a word must have the same type.  If this type is
*a*, then the word has type *[a]*.  The type of a word doesn't specify
how many elements the word contains.  This is different from a tuple,
where *(a,a)* contains exactly two elements, and *(a,a,a,a)* contains
exactly four elements.

Each element of a word has an index, a natural number that gives its
position within the word.  You can think of a word as an array or
vector.  The index of the leftmost position is 0, and the index of the
rightmost position is *k-1*, where *k* is the length of the word.
If we have defined some bit signals *x0*, *x1*, *x2*, and *x3*, then
we could define a word *x* of these bits with the equation

~~~~~
w = [x0,x1,x2,x3]
~~~~~

There are actually two conventions commonly used in computer systems.
One convention starts with position 0 at the left end, and counts up
going to the right.  This is called *big Endian* notation.  The
other convention, naturally called *little Endian*, starts with 0
as the index of the rightmost element, and the indices count up going
to the left.

~~~~~
[x0,x1,x2,x3]   -- Big Endian convention
[x3,x2,x1,x0]   -- Little Endian convention
~~~~~

As you might imagine, neither convention is fundamentally better than
the other, but there are all sorts of minor issues that might cause
one to be preferred over the other.  Hydra allows both conventions,
but in this book we will stick to Big Endian consistently.

There seems to be a phenomenon in computer systems, where the less
significant an issue is, the more heated debate there is about it.
This phenomenon was actually the inspiration for the odd names
Big/Little Endian.  The names come from Gullivers Travels, by Jonathan
Swift, where the citizens of the kingdom of Blefuscu open their eggs
at the big end, while the citizens of Lilliput open their eggs at the
little end.  The application of this story to computer systems comes
from an article by Danny Cohen, ``On Holy Wars and a Plea for Peach''
(IEEE Computer, October 1981).

The point here (aside from an entertaining digression) is that having
a standard is a good idea, and arguments for one particular choice are
less compelling than having a consistent standard.  Nevertheless,
there is one situation in hardware description where Little Endian is
slightly more convenient than Big Endian (see ref????) and some
authors actually combine both conventions.  The confusion isn't worth
it!

The size or length of a word is the number of elements it contains.
If a word contains $k$ elements, then their indices range from 0 to
$k-1$.  Hydra provides a meta-function *length* that takes a word and
returns an integer giving its size.

~~~~~
length :: [a] -> Int
~~~~~


For example, *length [x0,x1,x2] = 3*.  With just the parts of Hydra
covered so far, there is no way to use the length of a word, but later
we will encounter some more powerful features where an algorithm will
generate a circuit of a given size, and then the *length* function
will be useful.  It's important to remember that *length* is not a
circuit; it is part of the notation used to describe circuits.

There are several notations and operators that can be used to build
words from signals, and for extracting the signals within a word.  The
following sections introduce these notations, and then a couple of
example circuits will be presented.

## Example 2: A circuit with words and internal signals

Files: *Add4.hs* and *Add4Run.hs*

The *add4* circuit takes two 4-bit binary numbers *x* and *y*, and a
carry input *c*.  It adds them and outputs a carry output bit and a 4
bit sum.  The circuit is defined in *Add4.hs*.

~~~~~
m4_include(`examples/adder/Add4.hs')
~~~~~

A main program containing test data and a simulation driver is in
*Add4Run.hs*.

~~~~~
m4_include(`examples/adder/Add4Run.hs')
~~~~~

To run the simulation, enter *ghc -e main Add4Run*.  Here is the output:

~~~~~
$ ghc -e main Add4Run
  x =  5  y =  8  cin = 0    ==>    cout = 0  s = 13
  x =  7  y =  3  cin = 0    ==>    cout = 0  s = 10
  x =  8  y = 12  cin = 0    ==>    cout = 1  s =  4
  x =  8  y =  1  cin = 0    ==>    cout = 0  s =  9
  x = 12  y =  1  cin = 1    ==>    cout = 0  s = 14
  x =  2  y =  3  cin = 1    ==>    cout = 0  s =  6
  x = 15  y = 15  cin = 1    ==>    cout = 1  s = 15
~~~~~

### Building words

If you have expressions that define some signals, a word comprising
the signals can be constructed by writing the expressions in square
brackets, separated by commas.

~~~~~
[p,q,r,s]
~~~~~


The length of a word can be any natural number.  Thus *[]* is the
empty word, *[x]* is a word containing just one element, and so on.

~~~~~
[]                          -- length = 0
[x]                         -- length = 1
[x,y]                       -- length = 2
[x0,x1,x2,x3,x4,x5,x6,x7]   -- length = 8
~~~~~

Suppose you have a word *w*, of any length, and a bit signal *x*.
Thus *w :: [a]* and *x :: a*, where *a* is the basic signal type.
Then we can construct a new word which is just like *w* except that
the singleton *x* is attached to the front.  The notation for this is
*x:w*, which is pronounced ``*x cons w*''.  For example, suppose *w =
[p,q,r,s]*.  Then *(x:w) = [x,p,q,r,s]*.  The properties of the *(:)*
operator are summarized as follows:

~~~~~
x :: a
w :: [a]
(x:w) :: [a]
length (x:w) = 1 + length w
~~~~~

It's often useful to take two words that have already been defined,
and to define a bigger one that contains the elements of both.  This
is called *append* or *concatenation*, and is done using the
*(++)* operator.  The word *w1 ++ w2* is a word containing first the
elements of *w1*, and then the elements of *w2*.  Here are some
examples and properties of append:

~~~~~
[x0,x1,x2,x3] ++ [y0,y1] = [x0,x1,x2,x3,y0,y1]
length (w1 ++ w2) = length w1 + length w2
~~~~~



~~~~
(++)
~~~~

### Accessing parts of a word

Often we can perform operations on entire words, using word-oriented
digital circuits, without ever accessing individual elements of a
word.  Later we will see a family of building block circuits that
operate on words.  Normally this is the best way to organize a circuit
that works with words.

Sometimes, however, it's necessary to extract one or more elements of
a word.  One way to do this is by *indexing*.  Each element of a
word *w* has an index, ranging from 0 to $k-1$, where *k = length w*.
The *(!!)* operator uses an index to extract the element; thus *w!!i*
gives the $i$th element of the word *w*.  This is well defined if the
index *i* is in range: $i \leq length\ w$.  If $i<0$, or $i \geq
length\  W$, then *w!!i* is an error.


~~~~
w!!i                   i'th bit of word w
field w i j            bits i..i+j-1 of word w
~~~~


There are two special cases for indexing that are supported by
specific operators: you can get the least significant (or most
significant) bit of a word *w* using *lsb w* (or *msb w*).  The least
significant bit *lsb w* is equivalent to *w !! (length w -1)*, and the
most significant bit *msb w* is equivalent to *w !! 0*.

~~~~~
w !! i                 (!!) :: [a] -> Int -> a
lsb w                  lsb :: [a] -> a
msb w                  msb :: [a] -> a
~~~~~

There are three functions that give a field from a word; that is, the
result is itself a (smaller) word, not just an individual bit.  The
*take* and *drop* functions give a sub-word that is at the beginning
or end of a word.  Thus *take i w* gives a word consisting of the
leftmost $i$ elements of *w*, while *drop i w* gives a word consisting
of all the elements of *w* *except for* the leftmost $i$
elements.

More generally, it is sometimes necessary to extract an arbitrary
field from a word.  A *field* is a word consisting of any
consecutive set of elements.  A field has type *Field*, and it
consists of a pair of integers *(i,s)* where *i* is the index of the
starting position of the field, and *s* is its size.  Thus *field
(i,s) w = [w!!i, w!!(i+1), ..., w!!(i+s-1)]*.

~~~~~
type Field = (Int,Int)
take i w                 take :: Int -> [a] -> [a]
drop i w                 drop :: Int -> [a] -> [a]
field f w                field :: Field -> [a] -> [a]
~~~~~

An example of a circuit that operates on words is the 4-bit word
inverter *inv4*.  Its input and output are both 4-bit words, and each
output bit is the inversion of the corresponding input bit.  The type
notation for the word is concise, since the types of the individual
bits don't have to be repeated, but on the other hand the type doesn't
express the fact that this circuit works only on 4-bit words.


![](figures/xfig/inv4-wsig.svg)

![](figures/xfig/map4inv.svg)

![](figures/xfig/map4invbox.png)

![](figures/xfig/inv4-wsig.png)

~~~~~
inv4 :: Bit a => [a] -> [a]
inv4 [x0,x1,x2,x3] = [inv x0, inv x1, inv x2, inv x3]
~~~~~

The circuit specification for *inv4* is simple enough, but it would be
painful to extend this to much larger sizes, say 64-bit words.  The
chapter on design patterns shows a more elegant approach, but for
small words the style used here is adequate.  The Hydra libraries
provide a collection of straightforward circuit specifications written
in the same style as *inv4*, and they also provide circuits that are
defined using design patterns and that work for arbitrary word sizes,
no matter how large.

## Nested clusters

The cluster types can be nested.  A tuple may contain words (or deeper
tuples), and a word may contain tuples (or deeper words, although that
is unusual).

There is a style of circuit design called *bit slice
  organization*.  The idea is that a building block circuit is defined
for an arbitrary position within a word, and these building blocks can
then be combined.  Bit slice style often results in complex groupings,
with words of tuples, and notwithstanding the relatively complex types
it can result in simple specifications of efficient circuits.  The
essence of bit slice organization is to keep the corresponding bits of
several words together.  Thus two words $x$ and $y$ could be
represented as a word of pairs, rather than two separate words:

 $[(x0,y0), (x1,y1), (x2,y2), (x3,y3)] :: [(a,a)]$

Collecting a group of signals into a cluster is just a notational
convenience; it doesn't affect the actual circuit.  However, grouping
can simplify the way you *describe* the circuit, and this is essential
for large and complex circuits.

When you are designing a circuit with several input signals, you can
decide whether to treat them as separate arguments (each followed by
an arrow *->*) or as a single argument which is a tuple or word.
However, if you are using a circuit that has already been specified,
you need to follow the type used in its specification.

When a circuit has several outputs, there is no choice---the output
signals must be collected into a tuple or a word.  The reason for this
is that the underlying functional language requires that each function
has one result.  This does not limit our ability to express complex
circuits; it simply means that we need to use tuples or words.

Grouping is often helpful just to simplify the notation and to make
specifications more readable.

A tuple (x, (a,b)) is used to collect several values which may be
unrelated to each other.  Tuples are used for groups where the
components are unrelated, and indexing doesn't make sense.  The
components may have different types: $(a, (a,a), a)$ A word is used to
collect values that belong to specific bit positions, typically to
form a binary number.  Tuples and words can be combined to form
complex clusters.

Example: a 4-Bit ripple carry adder

For the multiplexer (the hardware equivalent of an if-then-else)
there is little to gain by grouping the inputs, so we use separate
parameters without grouping: *mux1 c x y = ... *

For the full-adder, which adds three bits, it's convenient to
group the bits $x$ and $y$ from the $i$th position in a word together,
and to keep them separate from the carry input bit $c$.
  *fullAdd (x,y) c = ... *

Don't worry---the reasons for these decisions will become clear later,
when we start making advanced uses of these circuits.  It's common to
make some changes to the grouping notation for a circuit after you
start using it extensively!

![](figures/xfig/rippleAdd4.svg)

~~~~~
rippleAdd4 c [(x0,y0), (x1,y1), (x2,y2), (x3,y3)] =
    (c0, [s0,s1,s2,s3])
  where
    (c0,s0) = fullAdd c1 (x0,y0)
    (c1,s1) = fullAdd c2 (x1,y1)
    (c2,s2) = fullAdd c3 (x2,y2)
    (c3,s3) = fullAdd c  (x3,y3)
~~~~~

**Exercise.**
A circuit has the type declaration *circ :: Bit a => a
    -> (a,a) -> [a] -> (a,[a])*.  How many groups of input bits are
  there?  How are they structured?  How is the output structured?

**Exercise.** Modify the definition of *rippleAdd4* to handle 6-bit
words.

**Exercise.** Define an 8-bit adder, named *rippleAdd8*.  Don't follow
the pattern of *rippleAdd4*, with eight equations.  Instead, use
*rippleAdd4* as a building block circuit.  In your definition of
*rippleAdd8*, use two separate internal *rippleAdd4* circuits, and
connect them up appropriately.

**Exercise.** Suppose *x = [x0,x1,x2]*, *y = [y0,y1,y2,y3]*, and *z =
  x++y*.  What are the values of *z*, *length z*, and *z!!4*?

# Combinational simulation

One way to simulate a combinational circuit is to apply it directly to
its inputs.  This works best if the circuit is defined with Bool as
the signal type.   Here is an example:

~~~~
module HalfAddB where
import HDL.Hydra.Core.Lib

-- Demonstrate a circuit with a concrete type Bool, instead of a type
-- class constraint Bit a =>.

halfAddB :: Bool -> Bool -> (Bool,Bool)
halfAddB x y = (and2 x y, xor2 x y)
~~~~

To simulate the circuit, the HalfAddB module is loaded, and then the
circuit is applied to several values of the input signals.  For a bit
signal of 0, use False as the input, and use True for a 1 bit.

~~~~
$ ghci
GHCi, version 8.0.1: http://www.haskell.org/ghc/  :? for help
Prelude> :load HalfAddB
[1 of 1] Compiling HalfAddB     
*HalfAddB> halfAddB False False
(False,False)
*HalfAddB> halfAddB False True
(False,True)
*HalfAddB> halfAddB True False
(False,True)
*HalfAddB> halfAddB True True
(True,False)
~~~~

This approach works only for combinational circuits because the Bool
type does not support clocked signals.  For sequential circuits, see
the following section.

# Synchronous sequential simulation

A sequential circuit may have feedback and state.  A sequential
circuit is *synchronous* if it uses a clock to ensure that all flip
flops change state simultaneously.

The execution of a synchronous circuit can be 

## General form


~~~~
simCirc input = runAllInput input output
  where
-- Extract input signals from the input data
     (equation for each input signal)
     ...

-- The circuit to be simulated
    output_signals = circ input_signals

-- Format the outputs
    output =
      [ (formatted signal values)... ]
~~~~

~~~~
simCirc input = runAllInput input output
  where
-- Extract input signals from the input data
    cin = getbit input 0
    x   = getbit input 1
    y   = getbit input 2

-- The circuit to be simulated
    (cout,sum) = fullAdd a b c

-- Format the outputs
    output =
      [string "Inputs: cin=", bit cin,
       string " x=", bit x,
       string " y=", bit y,
       string "  Outputs: cout=", bit cout,
       string " sum=", bit sum]
~~~~


## Parsing the inputs

The input data is written as a list of lists.  For the outer list,
the ith element is a list giving all the input values for clock cycle
i.  The simulation will run for cycle 0, cycle 1, ..., until it runs
out of input data, and then it will terminate.  Each element of the
outer list is a list of integers from which the input signals are
derived.

Each input signal (or container) should be defined with an equation.
The left hand side of the equation is the name of the signal, which
can be used as an input to the circuit.  The right hand side of the
equation says how an element of the input list is used to define the
signal; this is analogous to an input format specifier.

  * b = getbit input i
  * xy = getbit2 input i
  * n = getbin k input i
  * m = gettc k input i

## Formatting the outputs

## Defining a main program

## Modules and libraries

## Compilation and interpretation

# Standard library for bits


## Constant signals

~~~~
zero                   signal with constant 0 value
one                    signal with constant 1 value
~~~~

## Logic gates

~~~~
inv                    inverter
and2, and3, and4       and gate with 2, 3, 4 inputs
nand2, nand3, nand4    and gate with 2, 3, 4 inputs
or2, or3, or4          or gate with 2, 3, 4 inputs
nor2, nor3, nor4       nor gate with 2, 3, 4 inputs
xor2, xor3, xor4       xor gate with 2, 3, 4 inputs
~~~~



## Replicating a signal

Fanout takes a signal and splits it to several outputs.

~~~~~
fanout2 :: a -> (a,a)
fanout2 x = (x,x)

fanout3 :: a -> (a,a,a)
fanout3 x = (x,x,x)

fanout4 :: a -> (a,a,a,a)
fanout4 x = (x,x,x,x)
~~~~~

## Multiplexers and demultiplexers

~~~~~
mux1 :: Bit a => a -> a -> a -> a
mux1 p a b = x
  where x = or2 (and2 (inv p) a) (and2 p b)

mux2 :: Bit a => (a,a) -> a -> a -> a -> a -> a
mux2 (c,d) p q r s =
  mux1 c  (mux1 d p q)
          (mux1 d r s)

mux3 :: Bit a => (a,a,a) -> a -> a -> a -> a -> a-> a -> a -> a -> a
mux3 (c0,c1,c2) a0 a1 a2 a3 a4 a5 a6 a7 =
  mux1 c0
    (mux1 c1
      (mux1 c2 a0 a1)
      (mux1 c2 a2 a3))
    (mux1 c1
      (mux1 c2 a4 a5)
      (mux1 c2 a6 a7))

mux22 :: Bit a => (a,a) -> (a,a) -> (a,a) -> (a,a) -> (a,a) -> (a,a)
mux22 (p0,p1) (a0,a1) (b0,b1) (c0,c1) (d0,d1) = (x,y)
  where x = mux2 (p0,p1) a0 b0 c0 d0
        y = mux2 (p0,p1) a1 b1 c1 d1
~~~~~

~~~~~
mux1 :: Bit a => a -> a -> a -> a
mux1 p a b = x
  where x = or2 (and2 (inv p) a) (and2 p b)
~~~~~

~~~~~
mux2 :: Bit a => (a,a) -> a -> a -> a -> a -> a
mux2 (c,d) p q r s =
  mux1 c  (mux1 d p q)
          (mux1 d r s)
~~~~~

~~~~~
mux3 :: Bit a => (a,a,a) -> a -> a -> a -> a -> a-> a -> a -> a -> a
mux3 (c0,c1,c2) a0 a1 a2 a3 a4 a5 a6 a7 =
  mux1 c0
    (mux1 c1
      (mux1 c2 a0 a1)
      (mux1 c2 a2 a3))
    (mux1 c1
      (mux1 c2 a4 a5)
      (mux1 c2 a6 a7))
~~~~~

~~~~~
mux22 :: Bit a => (a,a) -> (a,a) -> (a,a) -> (a,a) -> (a,a) -> (a,a)
mux22 (p0,p1) (a0,a1) (b0,b1) (c0,c1) (d0,d1) = (x,y)
  where x = mux2 (p0,p1) a0 b0 c0 d0
        y = mux2 (p0,p1) a1 b1 c1 d1
~~~~~

A demultiplexer is an important building block circuit which is
related to the multiplexer.  It plays a central role in digital
circuit design, and we will see many applications that require them.
A common application a demultiplexer is to decode binary numbers.  For
example, we will use them later to implement memories (since the
address needs to be decoded), and they are also crucial in a
computer's control unit (where they are used to decode instruction
opcodes).

A 1-bit demultiplexer, called *demux1*, takes a control input *c* and
a data input *x*.  It produces two outputs *y0* and *y1* --- so it
provides a good practical example of the use of tuples.

~~~~~
(y0,y1) = demux1 c x
~~~~~

The idea of *demux1* is that we want to send the data input *x* to one
of the two outputs, and the choice depends on the control input *c*
--- thus if *c=0* then *y0=x*, but if *c=1* then *y1=x*.
But what happens to the output that is *not* selected by *c*?
That output has to have a well-defined value too, and we will set it
to the constant 0.  To summarize, the behavior of the *demux1* is

~~~~~
y0 = if c==0 then x else 0
y1 = if c==1 then x else 0
~~~~~


      c   x   y0   y1
     --- --- ---- ----
      0   0    0    0
      0   1    1    0
      1   0    0    0
      1   1    0    1

Here is the same thing but without any leading spaces

 c   x   y0   y1
--- --- ---- ----
 0   0    0    0
 0   1    1    0
 1   0    0    0
 1   1    0    1

Table: Truth table for *(y0,y1) = demux1 c x*

The implementation is straightforward.  From the truth table, you can
see that the *y1* has the same truth table as the *and2* gate, and
*y0=1* if *c=0* and *x=1*.

~~~~~
demux1 :: Bit a => a -> a -> (a,a)
demux1 c x = (y0,y1)
  where  y0 = and2 (inv c) x
         y1 = and2 c x
~~~~~

It isn't actually necessary to define the names of the outputs; here
is an alternative definition that outputs a tuple of anonymous
signals.  The two circuits are identical; the only difference is in
the way they are described.  One advantage of the first definition is
that it offers the names *y0* and *y1* that may be helpful in
discussing how the circuit works, but the definitions yield the same
circuit and the choice between them is a matter of style.

~~~~~
demux1 :: Bit a => a -> a -> (a,a)
demux1 c x = (and2 (inv c) x, and2 c x)
~~~~~

There are several ways that a larger circuit could incorporate a
*demux1*.  If the pair *(y0,y1)* is being connected to the input of
some other circuit *circ* that takes a pair, then we could simply
write *circ (demux1 c x)*.  However, if the larger circuit needs
explicit access to *y0* or *y1*, then they should be given names using
an equation.

A demux2 circuit takes a two-bit control and produces $2^{2} = 4$
outputs.

~~~~~
demux2 :: Bit a => (a,a) -> a -> (a,a,a,a)
demux2 (c0,c1) x = (y0,y1,y2,y3)
  where  (p,q) = demux1 c0 x
         (y0,y1) = demux1 c1 p
         (y2,y3) = demux1 c1 q
~~~~~

## Bit addition

When two bits are added together, the result could be 0, 1, or 2.  Two
bits are needed to represent the result, so a bit adder is an example
of a circuit that needs to output several signals.  The circuit that
does this is called a ``half adder'', and its name is *halfAdd*.
(Later we will discuss the ``full adder'', which adds three bits.)
The half adder can be specified with a truth table:

    | x  y  |  x+y  |  c  s |
    |-------+-------+-------|
    | 0  0  |   0   |  0  0 |
    | 0  1  |   1   |  0  1 |
    | 1  0  |   1   |  0  1 |
    | 1  1  |   2   |  1  0 |

Table: Truth table for halfAdd

|  x  |  y  |  x+y  |  c  |  s  |
|----:+:---:+:-----:+:---:+:----|
|  0  |  0  |   0   |  0  |  0  |
|  0  |  1  |   1   |  0  |  1  |
|  1  |  0  |   1   |  0  |  1  |
|  1  |  1  |   2   |  1  |  0  |

From the table, it is clear that the carry function is just *and2*,
and the sum function is *xor2*.

~~~~~
halfAdd :: Bit a => a -> a -> (a,a)
halfAdd x y = (c,s)
  where
    c = and2 x y
    s = xor2 x y
~~~~~

If you don't want to give names to the outputs *c* and *s*, the
definition can be shortened by putting the expressions for the signals
directly in the output tuple:

~~~~~
halfAdd :: Bit a => a -> a -> (a,a)
halfAdd x y = (and2 x y, xor2 x y)
~~~~~

The choice between these alternative definitions is a matter of style:
both are correct and both describe the same circuit.  The definition
with anonymous signals is shorter, while the definition with named
outputs uses simpler expressions and gives standard names for talking
about the outputs.

There is another bit adder circuit that illustrates how inputs can be
handled using either separate arguments or tuples.  This is the *full
adder*, which adds three bits.  Full adders are needed to add binary
numbers, because we have to add the carry as well as the two data bits
at each position.

    | x  y  z |  x+y+z  | c  s |
    |---------+---------+------|
    | 0  0  0 |    0    | 0  0 |
    | 0  0  1 |    1    | 0  1 |
    | 0  1  0 |    1    | 0  1 |
    | 0  1  1 |    2    | 1  0 |
    | 1  0  0 |    1    | 0  1 |
    | 1  0  1 |    2    | 1  0 |
    | 1  1  0 |    2    | 1  0 |
    | 1  1  1 |    3    | 1  1 |

Table: Truth table for fullAdd.  The three input bits x, y, z are
added to produce a two-bit result consisting of a carry c and a sum s.
(Note that the input bits do *not* represent a 3-bit binary number;
they are simply three separate variables to be added.)

Since there are two output signals, it is necessary to combine them in
a tuple, so the type will have the form *... -> (a,a)*.  We have a
choice for handling the three input signals.  They could be treated as
separate arguments:

~~~~~
(1) fullAdd :: Bit a => a -> a -> a -> (a,a)
~~~~~


Alternatively, the three inputs could be collected into a tuple:

~~~~~
(2) fullAdd :: Bit a => (a,a,a) -> (a,a)
~~~~~


But those are not the only possibilities.  Another approach is to
collect just two of the signals into a tuple, so there would be two
arguments, a tuple and a bit.  This gives two more ways to organize
the inputs:

~~~~~
(3) fullAdd :: Bit a => (a,a) -> a -> (a,a)
(4) fullAdd :: Bit a => a -> (a,a) -> (a,a)
~~~~~


At this point, there is little reason to prefer one of these types
over another.  Later, however, when design patterns are introduced, it
will turn out that the design of larger circuits can be simplified if
we choose version (3), so that is the type actually used for the half
adder in the Hydra circuit library.

Don't worry about making the ``best'' choice for such decisions.  No
one always can make the best choice among the possible alternatives
while designing a large system.  What happens in the real world is
that systems are designed according to experience, judgment, and
taste.  If it turns out later that the design could be made clearer or
more elegant by changing one of these arbitrary choices, then that can
be done when the system is cleaned up.  The Hydra libraries have going
through this process several times.

Now we can define the full adder circuit.  For convenience, the
calculation of the carry and sum results will be performed by
auxiliary circuits, *bcarry* and *bsum*.

~~~~~
fullAdd :: Bit a => (a,a) -> a -> (a,a)
fullAdd (x,y) c = (bcarry (x,y) c, bsum (x,y) c)
~~~~~

It isn't necessary to name the *x* and *y* signals individually.
Notice that the pair *(x,y)* comes into the circuit, and is then
passed to *bcarry* and *bsum*.  The *fullAdd* circuit itself doesn't
use either *x* or *y* directly.  Therefore we could just give a name,
such as *xy*, to the cluster *(x,y)*.  This shortens the notation:

~~~~~
fullAdd :: Bit a => (a,a) -> a -> (a,a)
fullAdd xy c = (bcarry xy c, bsum xy c)
~~~~~


Note that the signals *x* and *y* in the previous definition have the
bit signal type *a*.  This can be stated as *x :: a* and *y :: a*.  In
the simplified definition, the argument *xy* is a pair of bits, so *xy
:: (a,a)*.

To complete the circuit, we need to implement *bcarry* and *bsum*.
There are many ways to do this; the following specifications are
reasonable.  Since *bsum* and *bcarry* have the same type, we can
declare those types in one statement.  Read this as ``*bsum* and
*bcarry* both have type ...''.

~~~~~
bsum, bcarry :: Bit a => (a,a) -> a -> a
bsum (x,y) c = xor3 x y c
bcarry (x,y) c = or3 (and2 x y) (and2 x c) (and2 y c)
~~~~~

## Flip flops and registers

~~~~
dff                    delay flip flop
~~~~

~~~~
reg1 :: CBit a => a -> a -> a
~~~~

# Standard library for words


~~~~

winv w                 invert the bits in a word
mux1w                  use 1-bit control to select between two words
bitslice2 x y          convert pair of words to word of pairs
mux2                   use two bit control to select one of four inputs
~~~~


## Replication and constant words

## Replicating a word

fanout :: Bit a => Int -> a -> [a]
fanout k x = take k (repeat x)
~~~~~

Buffered fanout takes a signal and splits it to several outputs, and
inserts a buffer to ensure the outputs are strong enough.

~~~~~
fanoutbuf2 :: Bit a => a -> (a,a)
fanoutbuf2 x = (y,y)
  where y = buf x

fanoutbuf3 :: Bit a => a -> (a,a,a)
fanoutbuf3 x = (y,y,y)
  where y = buf x

fanoutbuf4 :: Bit a => a -> (a,a,a,a)
fanoutbuf4 x = (y,y,y,y)
  where y = buf x
~~~~~

~~~~~
fanout2 :: a -> (a,a)
fanout2 x = (x,x)

fanout3 :: a -> (a,a,a)
fanout3 x = (x,x,x)

fanout4 :: a -> (a,a,a,a)
fanout4 x = (x,x,x,x)
~~~~~


~~~~
fanout :: Bit a => Int -> a -> [a]
fanout n b             connect bit b to n outputs, forming a word
~~~~

A wiring pattern that replicates a singleton signal to form a
word. The input x is a signal, which is replicated n times to form a
word w of size n.

~~~~
w = fanout n x
~~~~


Representing a boolean bit as a word: boolword takes a bit x, and
pads it to the left with 0s to form a word.  If the input x is
False (0), the result is the integer 0 (i.e. n 0-bits), and if x is
True (1) the result is the integer 1 (rightmost bit is 1, all
others are 0).


~~~~~
boolword :: Bit a => Int -> a -> [a]
boolword n x = fanout (n-1) zero ++ [x]
~~~~~

~~~~
boolword n b           form an n-bit word, lsb = b, other bits = 0
~~~~


## Rearranging bits in a word

### Combinational shifting

Shift a word to the right (shr) or to the left (shl).  In both cases,
this is just a wiring pattern.  A 0 is brought in on one side, and the
bit on the other side is just thrown away.


~~~~
shl :: Bit a => [a] -> [a]
~~~~

shl is a wiring pattern that shifts a word to the left.  A zero is
brought in on the right side, and the value on the left is discarded.
This is a circuit generator that works for words of any size.  It is a
wiring pattern; no logic gates are generated.  Similar to shr.

Example:

~~~~
shl [a,b,c,d] = [b,c,d,zero]
~~~~

~~~~
shr :: Bit a => [a] -> [a]
~~~~

shr is a wiring pattern that shifts a word to the right.  A zero is
brought in on the left side, and the value on the right is discarded.
This is a circuit generator that works for words of any size.  It is a
wiring pattern; no logic gates are generated.  Similar to shl.

Example:

~~~~
shl [a,b,c,d] = [zero,a,b,c]
~~~~


~~~~~
shr x = zero : [x!!i | i <- [0..k-2]]
  where k = length x
shl x = [x!!i | i <- [1..k-1]] ++ [zero]
  where k = length x
~~~~~


### Bit slice representation

~~~~
bitslice2 :: [a] -> [a] -> [(a,a)]
~~~~

~~~~
unbitslice2 :: [(a,b)] -> ([a], [b])
~~~~


## Logic on words


Calculating a bit from a word

~~~~
any1                   or the bits in a word: result is 1 if any 1 bit
~~~~

~~~~
orw :: Bit a -> [a] -> a
~~~~

~~~~
andw :: Bit a -> [a] -> a
~~~~


And/Or over a word: Determine whether there exists a 1 in a word,
or whether all the bits are 0.  A tree fold can do this in log
time, but for simplicity this is just a linear time fold.

~~~~~
orw, andw :: Bit a => [a] -> a
orw = foldl or2 zero
andw = foldl and2 one
~~~~~

Logic on each bit in a word

Word inverter: winv takes a word and inverts each of its bits

~~~~~
winv :: Bit a => [a] -> [a]
winv x = map inv x
~~~~~


## Conditionals and addresses

### Multiplexers

~~~~
mux1w :: Bit a => a -> [a] -> [a] -> [a]
~~~~

~~~~
z = mux1w c x y
If c=zero, then z=x, but otherwise z=y
~~~~

A singleton control signal is used to choose between two data words.
If the control is zero the first data word is sent to the output,
otherwise the second data word is sent to the output.  The two input
data words should have the same size, and the output word
automatically has that size as well.  This is a circuit generator that
works for any word size.


~~~~
mux1w c x y = map2 (mux1 c) x y
~~~~

~~~~
mux2w cc = map4 (mux2 cc)
~~~~



BeginExercise(use-mux1w)
We have two word signals *x* and *y*, which have the same word size
(but we don't know or care exactly what the size is).  There is a
single control bit *c*.  Define a signal *z* which is a word; on each
clock cycle, *z* is the same as *x* if *c=0* during that cycle, but
*z* is the same as *y* if *c=1*.

BeginSolution(use-mux1w)
*z = mux1w c x y*

EndSolution

### Demultiplexers


~~~~~
demux1w :: Bit a => [a] -> a -> [a]
demux1w [c0] x =
  let (a0,a1) = demux1 c0 x
  in [a0,a1]
~~~~~

~~~~~
demux2w :: Bit a => [a] -> a -> [a]
demux2w [c0,c1] x =
  let (a0,a1) = demux1 c0 x
      w0 = demux1w [c1] a0
      w1 = demux1w [c1] a1
  in w0++w1
~~~~~

~~~~~
demux3w :: Bit a => [a] -> a -> [a]
demux3w [c0,c1,c2] x =
  let (a0,a1) = demux1 c0 x
      w0 = demux2w [c1,c2] a0
      w1 = demux2w [c1,c2] a1
  in w0++w1
~~~~~

~~~~~
demux4w :: Bit a => [a] -> a -> [a]
demux4w [c0,c1,c2,c3] x =
  let (a0,a1) = demux1 c0 x
      w0 = demux3w [c1,c2,c3] a0
      w1 = demux3w [c1,c2,c3] a1
  in w0++w1
~~~~~

## Arithmetic



### Binary addition

~~~~~
bsum, bcarry :: Bit a => (a,a) -> a -> a
bsum (x,y) c = xor3 x y c
bcarry (x,y) c = or3 (and2 x y) (and2 x c) (and2 y c)
~~~~~

~~~~
rippleAdd :: Bit a => a -> [(a,a)] -> (a,[a])
~~~~

The ripple carry adder takes a carry input, and two words organised in
bit slice form.  It produces a carry output and a sum word.  This is a
circuit generator, which allows input words of any size.


## Registers

~~~~
wlatch :: CBit a => Int -> [a] -> [a]
~~~~

Defines a register with output r, containing n bits, and with input x.
At every clock cycle, the register discards its old state and replaces
it with the current value of the input.

~~~~
r = wlatch n x
~~~~



~~~~
reg :: CBit a => Int -> a -> [a] -> [a]
~~~~

~~~~
reg n ld x                 n-bit register with load control ld, data input x
~~~~




## Registers

~~~~
reg
  :: CBit a =>
  Int             -- ^ k = the word size
  -> a          -- ^ ld = the load control signal
  -> [a]        -- ^ input word of size k
  -> [a]        -- ^ output is the register state

reg k ld x = mapn (reg1 ld) k x
~~~~


~~~~
regfile n k ld d sa sb x   register file with 2^k registers, each n-bits wide,
                           load control ld, destination address d,
                           reads out registers sa and sb, data input x
~~~~


# Circuit generators

## Operating on words

Duplicating a bit to form a word: fanout takes a wordsize k and a
signal x, and produces a word of size k each of whose bits takes
the value of x.

~~~~~
fanout :: Bit a => Int -> a -> [a]
fanout k x = take k (repeat x)
~~~~~

Buffered Fanout

~~~~~
fanoutbuf2 :: Bit a => a -> (a,a)
fanoutbuf2 x = (y,y)
  where y = buf x

fanoutbuf3 :: Bit a => a -> (a,a,a)
fanoutbuf3 x = (y,y,y)
  where y = buf x

fanoutbuf4 :: Bit a => a -> (a,a,a,a)
fanoutbuf4 x = (y,y,y,y)
  where y = buf x
~~~~~


Building a constant integer word

Representing a boolean bit as a word: boolword takes a bit x, and
pads it to the left with 0s to form a word.  If the input x is
False (0), the result is the integer 0 (i.e. n 0-bits), and if x is
True (1) the result is the integer 1 (rightmost bit is 1, all
others are 0).

~~~~~
boolword :: Bit a => Int -> a -> [a]
boolword n x = fanout (n-1) zero ++ [x]
~~~~~

Combinational shifting

Shift a word to the right (shr) or to the left (shl).  In both
cases, this is just a wiring pattern.  A 0 is brought in on one
side, and the bit on the other side is just thrown away.

~~~~~
shr x = zero : [x!!i * i <- [0..k-2]]
  where k = length x
shl x = [x!!i * i <- [1..k-1]] ++ [zero]
  where k = length x
~~~~~

## Recursive circuit definitions

~~~~~
demux1w :: Bit a => [a] -> a -> [a]
demux1w [c0] x =
  let (a0,a1) = demux1 c0 x
  in [a0,a1]

demux2w :: Bit a => [a] -> a -> [a]
demux2w [c0,c1] x =
  let (a0,a1) = demux1 c0 x
      w0 = demux1w [c1] a0
      w1 = demux1w [c1] a1
  in w0++w1

demux3w :: Bit a => [a] -> a -> [a]
demux3w [c0,c1,c2] x =
  let (a0,a1) = demux1 c0 x
      w0 = demux2w [c1,c2] a0
      w1 = demux2w [c1,c2] a1
  in w0++w1

demux4w :: Bit a => [a] -> a -> [a]
demux4w [c0,c1,c2,c3] x =
  let (a0,a1) = demux1 c0 x
      w0 = demux3w [c1,c2,c3] a0
      w1 = demux3w [c1,c2,c3] a1
  in w0++w1
~~~~~

## Tree structured circuits

~~~~~
regfile1 :: CBit a => Int -> a -> [a] -> [a] -> [a] -> a -> (a,a)

regfile1 0 ld d sa sb x = (r,r)
  where r = reg1 ld x

regfile1 (k+1) ld (d:ds) (sa:sas) (sb:sbs) x = (a,b)
  where
    (a0,b0) = regfile1 k ld0 ds sas sbs x
    (a1,b1) = regfile1 k ld1 ds sas sbs x
    (ld0,ld1) = demux1 d ld
    a = mux1 sa a0 a1
    b = mux1 sb b0 b1
~~~~~


~~~~~
regfile :: CBit a => Int -> Int
  -> a -> [a] -> [a] -> [a] -> [a] -> ([a],[a])

regfile n k ld d sa sb x =
   unbitslice2 [regfile1 k ld d sa sb (x!!i)  * i <- [0..n-1]]
~~~~~


## Memory

~~~~~
mem1 :: CBit a => Int
  -> a -> [a] -> [a] -> a -> a

~~~~~

~~~~~
mem1 0 ld d sa x = reg1 ld x
mem1 (k+1) ld (d:ds) (sa:sas) x = a
  where
    (ld0,ld1) = demux1 d ld
    a0 = mem1 k ld0 ds sas x
    a1 = mem1 k ld1 ds sas x
    a = mux1 sa a0 a1
~~~~~

~~~~
mem1a :: CBit a => Int -> a -> [a] -> a -> a
mem1a 0 sto p x = reg1 sto x
mem1a (k+1) sto (p:ps) x =
  let (sto0,sto1) = demux1 p sto
      m0 = mem1a k sto0 ps x
      m1 = mem1a k sto1 ps x
  in mux1 p m0 m1
~~~~


~~~~
memw
  :: CBit a
  => Int
  -> Int
  -> a
  -> [a]
  -> [a]
  -> [a]

memw n k sto p x =
  [mem1a k sto p (x!!i) | i <- [0..n-1]]
~~~~


# Combinators

We will generally specify large circuits using a circuit generator,
not by drawing every component individually.  There are two kinds of
circuit generator.  Design patterns (higher order functions) are the
focus of this chapter.  Special languages for special kinds of circuit
(e.g. control algorithms) are covered later.

Design patterns use circuits as building blocks

Design patterns are *higher order* functions: they take one or
more *circuit specifications* as parameters.  The pattern defines
how to connect up these given circuits in a regular pattern.  A
pattern definition looks just like an ordinary circuit specification,
except It uses recursion to decompose groups of signals.  It uses
abstract circuits, supplied as parameters, instead of specific
circuits.  Its type may include building block circuits (these
parameters contain an *->* in their type) and/or size
parameters (with a type like *Int*).

## Map

Word inverter: winv takes a word and inverts each of its bits

~~~~~
winv :: Bit a => [a] -> [a]
winv x = map inv x
~~~~~

Operating on each element of a word of known size: mapn

~~~~
wlatch :: CBit a => Int -> [a] -> [a]
wlatch k x = mapn dff k x
~~~~

The word register

~~~~~
reg
  :: CBit a =>
  Int             -- ** k = the word size
  -> a          -- ** ld = the load control signal
  -> [a]        -- ** input word of size k
  -> [a]        -- ** output is the register state
reg k ld x = mapn (reg1 ld) k x
~~~~~

Mapping a circuit with multiple inputs

~~~~~
mux1w :: Bit a => a -> [a] -> [a] -> [a]
mux1w c x y = map2 (mux1 c) x y
~~~~~

~~~~~
mux2w cc = map4 (mux2 cc)
~~~~~







 Sometimes you have a circuit (it's arbitrary, so call it $f$)
  that takes an input (say it has type $a$) and produces an output
  (call its type $b$).
 You need to take a word of signals, and process each one with
  the circuit $f$.  For example, *inv4* processes each
  signal with an *inv*.
 The *map* pattern describes this in general.


~~~~
map :: (a->b) -> [a] -> [b]
~~~~

 The first argument to the pattern is a circuit with type
  *a->b*
 The pattern then generates a circuits, which takes an input
  word of type *[a]* and produces an output word of type
  *[b]*.

Example of map

We can define a word inverter using the pattern that places an
inverter on each input signal, to produce the corresponding output
signals.

~~~~
winv :: Bit a => [a] -> [a]
winv x = map inv x
~~~~

Technical note: in a defining equation of the form *f a b c
  = g c*, you can ``factor out'' the rightmost parameter from both
sides, giving a slightly shorter form.

~~~~
winv :: Bit a => [a] -> [a]
winv = map inv
~~~~

This is attractive because it describes just the pattern.



Word inverter: ys = map inv xs

  \includegraphics[angle=-90,scale=0.5]{figures/xfig/map4inv.eps}

  \includegraphics[angle=-90,scale=0.5]{figures/xfig/map4invbox.eps}

Definition of map

~~~~
map :: (a->b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs
~~~~


A recursion, based on the word structure of the input.

The base case is an empty input word *[]*.  In this
case, the output is also empty.

The recursion (or induction) case has an input word
  *x:xs* consisting of an initial bit *x* followed by
  the rest of the word, *xs*.  The circuit introduces a copy
  of the *f* circuit to process *x*, and handles the
  rest recursively.


The following figure illustrates the structure of map recursion

  \includegraphics[angle=0,scale=0.4]{figures/xfig/map-recursion.eps}

After the recursion has completed:

  \includegraphics[angle=-90,scale=0.5]{figures/xfig/map4.eps}

  \includegraphics[angle=-90,scale=0.5]{figures/xfig/map4box.eps}


Extending map to multiple inputs

The *map2* pattern is similar to *map*, but it uses a
circuit that takes two inputs (thus its type is *a->b->c*).
Note that *map2* is *not* a bit-slice pattern; it uses
separate words.

~~~~
map2 :: (a->b->c) -> [a] -> [b] -> [c]
~~~~

We can extend the basic multiplexor to handle words:

~~~~
mux1w :: Bit a => a -> [a] -> [a] -> [a]
mux1w c x y = map2 (mux1 c) x y
~~~~


### Sized map

The *mapn* pattern is similar to *map*, except it
takes a size parameter, and guarantees to produce an output of that
size.

~~~~
mapn :: (a->b) -> Int -> [a] -> [b]
~~~~


 Registers are defined using *mapn*, to ensure that the number
  of flip flops is defined
 Combinational circuits may be defined using *map*, so
  they inherit the word size of their input


## Fold

The folding patterns define a linear circuit structure.


 There is an input word of type *[b]*.

 The elements of the word are combined using a building block
  *f*.

 There is a ``horizontal'' signal of some type (call it
  *a*), which is goes across the word from left to right.

 An initial horizontal input, of type *a* is provided.

 The output is the final horizontal output (produced by the
  rightmost *f* circuit).

Folding corresponds to a linear computation from one end of the
word to the other, starting with an initial value a (sometimes
called an accumulator, but this is not to be confused with
accumulator registers!).



### Fold from the left

In general, a fold can proceed either direction across the word.
The *foldl* pattern describes a *fold from the left*;
i.e. the information flows from left to right across the word.

~~~~
foldl :: (a->b->a) -> a -> [b] -> a
~~~~

The pattern is defined recursively:

~~~~
foldl f a [] = a
foldl f a (x:xs) = foldl f (f a x) xs
~~~~


  \includegraphics[angle=0,scale=0.5]{figures/xfig/foldl4.eps}

  \includegraphics[angle=0,scale=0.5]{figures/xfig/foldl4box.eps}

\subsection{Examples: orw, andw}

The *orw* circuit determines whether there is any 1 bit in a
word.

~~~~
orw :: Bit a => [a] -> a
orw = foldl or2 zero
~~~~

The *andw* circuit determines whether all the bits in a word
are 1.

And/Or over a word

~~~~~
orw, andw :: Bit a => [a] -> a
orw = foldl or2 zero
andw = foldl and2 one
~~~~~

~~~~
andw :: Bit a => [a] -> a
andw = foldl and2 one
~~~~

The time required (the path depth) is linear in the word size.
There are also tree-structured patterns that can do these
computations in logarithmic time.


Efficiency

 The definitions of *orw* and *andw* are not
  very efficient

   If a large number of signals are being combined, a tree
    structure of logic gates reduces the path depth.  If this
    circuit is on the critical path, that will help.
   If the technology supplies 3 or 4 input gates, it would
    likely be faster to use some of those, rather than just the 2
    input gates.
   The *foldl* pattern uses one extra gate to include
    the ``default'' value of zero or one.  This is overhead.

 This inefficiency is not a concern, because

   There are alternative patterns that generate more efficient
    circuits
   A circuit optimiser can generate optimal results
   If the circuit isn't on the critical path, it makes no
    difference anyway.


### Binary comparison using foldl

 The problem: input two binary numbers, in bit slice form:
  *[(x0,y0), (x1,y1), ..., (xk,yk)]*
 Output the result of a comparision: *(lt,eq,gt)*,
  giving the values of $(x<y, x=y, x>y)$.  Exactly one of the three
  output bits must be 1.
 Idea: start from left, assuming the numbers are equal so far:
  *(0,1,0)*.
 Move over the columns from left to right, updating the
  results of the comparision using a building block circuit
  *cmp1*.
 Going from left to right, once we have established either $<$
  or $>$, that result will never change.
 If the current result is $=$ and $x=y$, it's still $=$.
 If the current result is $=$ but $x$ and $y$ are different,
  the new result becomes $<$ or $>$.

A bit comparison building block circuit:

~~~~
cmp1 :: Bit a => (a,a,a) -> (a,a) -> (a,a,a)
cmp1 (lt,eq,gt) (x,y) =
  (or2 lt (and3 eq (inv x) y),
   and2 eq (inv (xor2 x y)),
   or2 gt (and3 eq x (inv y))
  )
~~~~

The ripple comparison circuit is defined simply using the pattern:

~~~~
rippleCmp :: Bit a => [(a,a)] -> (a,a,a)
rippleCmp = foldl cmp1 (zero,one,zero)
~~~~

### Fold from the right: foldr

You can also run a fold across a word from the right to the left.

~~~~
foldr :: (b->a->a) -> a -> [b] -> a
foldr f a [] = a
foldr f a (x:xs) = f x (foldr f a xs)
~~~~

This is symmetric with *foldl*.

The foldr pattern

  \includegraphics[angle=0,scale=0.5]{figures/xfig/foldr4.eps}

  \includegraphics[angle=0,scale=0.5]{figures/xfig/foldr4box.eps}

## Scan

A fold calculates a sequence of intermediate values, one for every bit
position.  A more general kind of pattern---a *scan*---outputs this
word of intermediate values.  For every kind of fold, there is a
corresponding scan, and there are also some more general patterns that
are based on scan.  Scans are important because many important
computations can be expressed via scans, and a variety of patterns
exist that implement scans efficiently.

### Scan from the left: scanl

  \includegraphics[angle=0,scale=0.5]{figures/xfig/scanl4.eps}

~~~~
\[ [y_0, y_1, y_2, y_3, y_4] \ =
  \ *scanl*\ f\ a\ [x_0, x_1, x_2, x_3]
\]

\[ *ys* \ = \ *scanl*\ f\ a\ *xs* \]
~~~~

### Scan from the right: scanr

The *ascanr* pattern yields the word of intermediate results
that would occur during a *foldr*.  Specifically, it gives
the horizontal *input* to each box in the *foldr*.

~~~~
ascanr :: (b->a->a) -> a -> [b] -> [a]
~~~~

The value of the output word can be defined directly in terms of
*foldr* of portions of the input word.  This is useful for
intuition and for formal mathematical reasoning.

~~~~
ascanr :: (b->a->a) -> a -> [b] -> (a,[a])
ascanr f a [] = (a,[])
ascanr f a (x:xs) = (f x a', a':xs')
  where (a',xs') = ascanr f a xs
~~~~



  \includegraphics[angle=0,scale=0.5]{figures/xfig/scanr4.eps}

~~~~
\[ [y_0, y_1, y_2, y_3, y_4] \ =
  \ *scanr*\ f\ a\ [x_0, x_1, x_2, x_3]
\]

\[ *ys* \ = \ *scanr*\ f\ a\ *xs* \]
~~~~


### Combining a map with a scan

Many circuits combine a map with a scan: they output a value in each
bit position that depends on both horizontal input and the value of
the word in that bit position.

The *mscanr* pattern is useful for such cases (and there is
a corresponding *mscanl*).

~~~~
mscanr :: (a->b->(b,c)) -> b -> [a] -> (b,[c])
mscanr f a [] = (a,[])
mscanr f a (x:xs) = (a'',y:ys)
  where
    (a',ys) = mscanr f a xs
    (a'',y) = f x a'
~~~~


Unidirectional mapping scan: mscanl, mscanr


  \includegraphics[angle=0,scale=0.5]{figures/xfig/mscanl4.eps}


(z, [y_0, y_1, y_2, y_3]  = mscanr f a [x_0, x_1, x_2, x_3]

(z,ys)  =  mscanr f a xs

\includegraphics[angle=0,scale=0.5]{figures/xfig/mscanr4.eps}


(z, [y_0, y_1, y_2, y_3]  = mscanr f a [x_0, x_1, x_2, x_3]

(z,ys)  =  mscanr f a *xs*

The ripple carry adder is an excellent example of scanr.

~~~~
fullAdd :: Bit a => (a,a) -> a -> (a,a)
fullAdd (x,y) c = (bcarry (x,y) c, bsum (x,y) c)
~~~~

### Ripple carry addition

rippleAdd4

  \includegraphics[angle=0,scale=0.4]{figures/xfig/rippleAdd4.eps}

~~~~~
rippleAdd4 :: Bit a => a -> [(a,a)] -> (a,[a])
rippleAdd4 cin [(x0,y0),(x1,y1),(x2,y2),(x3,y3)]
  = (c0, [s0,s1,s2,s3])
  where
    (c0,s0) = fullAdd (x0,y0) c1
    (c1,s1) = fullAdd (x1,y1) c2
    (c2,s2) = fullAdd (x2,y2) c3
    (c3,s3) = fullAdd (x3,y3) cin
~~~~~

~~~~
rippleAdd :: Bit a => a -> [(a,a)] -> (a,[a])
rippleAdd = mscanr fullAdd
~~~~

Two's complement addition and subtraction

~~~~
addSub :: Bit a => a -> [(a,a)] -> (a,[a])
addSub sub xy = rippleAdd sub (map f xy)
  where f (x,y) = (x, xor2 sub y)
~~~~

Circuit specifications using patterns can often be simplified if you
pay attention to the order of parameters.  For example, the ripple
carry adder specification would be clunkier if we had defined the full
adder as *fullAddInelegant :: Bit a => a -> (a,a) -> (a,a)*.

How can you make the definitions work out cleanly?  Often there are
choices that seem arbitrary at first, but later on you realise things
would be simpler if the choice had been different.  This is a good
time to go back and clean up your specifications.

Two's complement addition and subtraction

~~~~~
addSub :: Bit a => a -> [(a,a)] -> (a,[a])
addSub sub xy = rippleAdd sub (map f xy)
  where f (x,y) = (x, xor2 sub y)
~~~~~

### Bidirectional scan

A very general pattern is the *mscan* pattern, which takes a
word, and *two* horizontal values, one moving from left to right
and the other from right to left.  The pattern combines a
*foldl*, a *foldr*, and a *map*.

ALU circuits often use the *mscan* pattern.

~~~~
mscan :: (a->b->c->(b,a,d)) -> a -> b -> [c] -> (b,a,[d])
mscan f a b [] = (b,a,[])
mscan f a b (x:xs) = (b'', a'', y:ys)
  where
    (b'',a',y) = f a b' x
    (b',a'',ys) = mscan f a' b xs
~~~~

  \includegraphics[angle=0,scale=0.5]{figures/xfig/mscan.eps}

(b', a', [y_0, y_1, y_2, y_3])  =
   mscanr f a b [x_0, x_1, x_2, x_3]

(b', a', ys)  =  mscanr f a b *xs*


# Circuit semantics

A digital circuit takes some input signals, performs a computation,
and produces some output signals.  Since the outputs depend on the
inputs, the circuit acts like a mathematical function.  This
connection between circuits and functions is discussed in more depth
in the chapter on synchronous circuits.

## Boolean signals

## Streams

## Synchronous circuits

## Multiple semantics

## Path depth

## Netlists

# Equational reasoning

## Substituting equals for equals

An equation in Hydra is a true mathematical equation, not an
assignment statement.  This has several consequences.  One is that the
order of the equations does not matter, and it is not necessary to
define a signal before it is used.  The set of equations above
illustrates this: *x* is defined using *p* and *q*, whose definitions
appear later.  These equations could equally well be written in
reverse order:

~~~~~
r = nor2 c d
q = inv r
p = xor2 a b
x = nand2 p q
~~~~~

The order of the equations makes no difference at all to the circuit;
it's best to put them in whatever order seems to be easiest to
understand.  If a top-down view of the circuit seems natural, then the
first set of equations is suitable, but if a bottom-up view is
clearer, then the second set is better.

There is another important consequence of the fact that we are working
with mathematical equations.  We can perform *equational
  reasoning* to transform a circuit specification into a different
form, with a guarantee that the circuit itself has not been affected.
Suppose that we have an equation *x = e*.  Equational reasoning means
that if *x* appears in *e'*, we can replace it with *e*; conversely,
if *e* appears in *e'* we can replace it with *x*.  This process is
sometimes called ``substituting equals with equals'', and it is
central in algebraic reasoning.

Equational reasoning is one of the central advantages to a functional
hardware description language.  It has many applications: it can be
used to rewrite a circuit to make it more readable; it can be used to
transform a circuit to calculate the same result but do it faster; it
can be used to prove that a circuit is correct according to an
abstract specification.  Equational reasoning can even be used to
derive an implementation from a specification.

Here is a simple example of equational reasoning.  Start with the set
of equations discussed above:

~~~~~
x = nand2 p q
p = xor2 a b
q = inv r
r = nor2 c d
~~~~~


We can calculate the value of *x* using equational reasoning, in a
sequence of steps:

~~~~~
x = {definition of x}
nand2 p q
  = {substitute value of p}
nand2 (xor2 a b) q
  = {substitute value of q}
nand2 (xor2 a b) (inv r)
  = {substitute value of r}
nand2 (xor2 a b) (inv (nor2 c d))
~~~~~

This calculation may not look very impressive, but later we will see
how equational reasoning can solve some quite challenging problems.

## Equivalent ways to describe a circuit


# Summary of syntax

This section summarises the language syntax.  Hydra is actually
Haskell with some additional libraries, and it adopts all the syntax
rules of Haskell.


Comments.  There are two ways to indicate a comment
  (1) enclose the comment in brackets {- so this is a comment -}
  (2) A double dash -- indicates that everything else
                    -- on the line is a comment


## Indentation

Haskell normally uses indentation, rather than punctuation, to
determine the structure of a definition.  There are good reasons
behind this approach to syntax.

It is also possible to use braces and semicolons to determine the
structure, instead of indentation.  This is particularly useful for
generators, where the specification is not written by hand and also
not intended to be human readable.  There are also some situations
where a large number of very short equations can be more readable with
many placed on each line, separated by punctuation.  However, these
situations are relatively uncommon.  Normally it's best to use
indentation and to make the layout of the code as readable as
possible.


Indentation
  The equations in a definition need to be lined up vertically

circ x y = a   -- a good definition
  where
    p = bla bla...
    q = bla...
    r = bla...

circ x y = a  -- a bad definition
  where
    p = bla bla...
   q = bla...
    r = bla...
-}


## Names

Names (also called identifiers) are used for circuits (e.g. logic
gates) and signals.  A name must begin with a lower case letter, and
may contain letters, digits, underscores, and primes (single quote).
The following are valid names:

~~~~~
x
select
adder
y'
bypass_ctl
~~~~~

The following identifiers cannot be used to name a circuit or signal:

~~~~~
Product    -- begins with upper case letter
x?3        -- contains invalid character ?
0          -- use zero to get the constant 0 signal
~~~~~

## Signal expressions

A signal is specified in Hydra by an expression.  The simplest form of
expression is simply the name of a signal.  For example, suppose we
have signals named *x* and *y*.  Then the following expressions denote
the corresponding signals.  Note that *zero* and *one* are simply the
names of the constant signals.

~~~~~
zero
one
x
y
~~~~~

A signal is denoted by an expression, which can have any of the following forms:

  * The constants *zero* and *one* are names that are pre-defined.  These
    should not be redefined: don't write zero or one on the left hand side
    of an equation.

  * The name of a signal which is in scope: *x*, *carry*, *ctl_ld*.
    Later we will see how to define these, using equations or circuit
    inputs.

  * An application of a circuit to inputs denotes a signal: *or2 x y*
    specifies a signal which is the output of an *or2* gate connected
    to inputs *x* and *y*.

Any of these notations can be used as input to a component.

The component and the inputs are separated by a space; don't use
punctuation.

~~~~~
c = and2 a b          -- correct
d = and2 (a,b);       -- wrong! don't use ( , ) ;
~~~~~

## Defining equations for signals

BeginExercise(text-for-andor) Write a textual specification of the
circuit in the following diagram:
  
![](figures/xfig/andor.svg "schematic diagram")

BeginSolution(text-for-andor) *y = or2 (and2 a b) c*

EndSolution

## Defining equations for circuits

# Error messages and runtime errors

This section shows some typical error messages and runtime errors, and
gives hints about how to handle them.

# Implementation of Hydra

## Files

Metadata

  index.html     Top level of documentation
  README.txt     General information
  LICENSE.txt    GPL license
  Hydra.cabal    Information needed to compile the software
  Setup.hs       Haskell build configuration
  makefile       commands for maintainance and development

## Source directories

  src contains the Hydra source code and the user guide source text

  circuits contains a collection of examples.  There is a naming
  convention: a file whose name matches *Run.hs is a main module
  containing a simulation driver and test data; you can run the
  simulation by entering ghc -e main *Run.  Other files contain
  circuit specifications.

Automatically generated directories (can be deleted)

  doc contains documentation files in html format.  These are
  generated from sources (in src/docsrc).  The entire doc directory
  can be deleted and regenerated by make doc.

  dist contains the object code and interface files, as well as
  the library documentation generated by haddock

 Hydra
    archve
       ... lots of tarballs, this is where snapshots will be saved ...
    current-version
       Hydra-i.j.k
    released
       ... some tarballs or zips ...


# Further information

For detailed documentation of the circuits and Hydra language tools,
 see the <a href="../../../dist/doc/html/Hydra/index.html">library
 interface reference</a>.

# Solutions to the exercises

m4_undivert(`1')

# Index

# Colophon

This document is written in markdown augmented with m4 macros.  The
source is indexsrc.m4; this is processed by m4 producing indextemp.txt
which is then processed by pandoc to produce index.html.

The links between exercises and solutions are generated by macros.
xBeginExercise, xBeginSolution, xEndsolution (without leading x)
should all start at the beginning of a line, after a blank line.  If
xBeginSolution doesn't follow a blank line the content can become
scrambled.

Here are some experiments with diagrams.

svgfig(xor2-a-b)

This is a separator paragraph.

svgfig(xor2-a-b-modified)

This is a separator paragraph.

![](figures/pgf/test-and2-svg.svg)
