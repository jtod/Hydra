#----------------------------------------------------------------------
# makefile for Hydra, (c) 2020 John T. O'Donnell
# See README.md, LICENSE.txt, doc/HydraUserGuide.org
# In browser, visit docs/userguide/HydraUserGuide.html
#----------------------------------------------------------------------

# Requires ghc, pandoc

# Usage:

#  make show-parameters for debugging, show makefile variables
#  make install         compile and install
#  make doc             haddock and user guide
#  make userguide       create doc/html files from markdown source
#  make clean           delete temp files but keep documentation

#----------------------------------------------------------------------
# Calculate locations and times, don't need to edit these

# Provide version number in generated html documentation
HydraVersion != grep < Hydra.cabal ^version
VersionDate = $(HydraVersion)

# make show-parameters -- print calculated parameters
.PHONY : show-parameters
show-parameters :
	echo PackagePath = $(PackagePath)
	echo PackageName = $(PackageName)
	echo DateTimeStamp = $(DateTimeStamp)
	echo BuildDate = $(BuildDate)
	echo TarballName = $(TarballName)
	echo FullCopyName = $(FullCopyName)
	echo ArchiveLocation = $(ArchiveLocation)
	echo HydraVersion = $(HydraVersion)
	echo VersionDate = $(VersionDate)

#----------------------------------------------------------------------
# Compilation

# Compile and install but don't try to build the user guide, which
# requires software that some users may not have installed (m4,
# pandoc).

.PHONY : install
install :
	cabal configure
	cabal install
	cabal haddock

# Compile, install, and build the user guide
.PHONY : all
all :
	make install
	make userguide

#----------------------------------------------------------------------
# User guide

# The source for the user guide (in src/docsrc) is written in org.
# The m4 and pandoc programs are needed to build the html.

.PHONY : olduserguide
olduserguide :
	mkdir -p doc/userguide/html
	cp -r -u src/docsrc/figures doc/userguide
	cp src/docsrc/style.css doc/userguide/html
	m4 -P src/docsrc/indexsrc.m4 \
	  >doc/userguide/html/indextemp.txt
	pandoc --standalone \
          --read markdown --write html \
          --table-of-contents --toc-depth=4 \
          --variable=date:'$(VersionDate)' \
          --variable=css:style.css \
          -o doc/userguide/html/index.html \
	    doc/userguide/html/indextemp.txt

docs/userguide/HydraUserGuide.html : docs/userguide/HydraUserGuide.org
	m4 -P docs/userguide/HydraUserGuide.org \
	  >docs/userguide/ugTEMP.org
	pandoc --standalone \
          --from=org \
          --to=html \
          --template=docs/userguide/userguide-template.html \
          --table-of-contents --toc-depth=4 \
          --metadata title="Hydra User Guide" \
          --variable=date:'$(VersionDate)' \
          --variable=css:HydraUserGuide.css \
          -o docs/userguide/HydraUserGuide.html \
	  docs/userguide/ugTEMP.org
