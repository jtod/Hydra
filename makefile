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

docs/userguide/HydraUserGuide.html : docs/userguide/HydraUserGuide.org
	pandoc --standalone \
          --from=org \
          --to=html \
          --template=docs/userguide/userguide-template.html \
          --table-of-contents --toc-depth=4 \
          --metadata title="Hydra User Guide" \
          --variable=date:'$(VersionDate)' \
          --variable=css:style.css \
          --output=docs/html/userguide/userguide.html \
          -o docs/userguide/HydraUserGuide.html \
	  docs/userguide/HydraUserGuide.org
