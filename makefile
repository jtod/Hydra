#----------------------------------------------------------------------
# makefile for Hydra
#----------------------------------------------------------------------

#  make userinstall     user requires ghc and cabal
#  make devinstall      developer requires ghc, cabal, pandoc
#  make clean           delete temp files but keep documentation

#----------------------------------------------------------------------
# Calculate parameters: don't need to edit these

# Obtain version number from cabal file to use in generated html documentation
VERSION != grep < Hydra.cabal ^version

# Define dates in several formats, for inclusion in the app and user guide
YEAR=$(shell date +"%Y")
MONTHYEAR=$(shell date +"%B %Y")
MONTHYEARDAY=$(shell date +"%F")

# Define metadata line to put at top of html
MDVERSION="$(VERSION), $(MONTHYEAR)"
MDCOPYRIGHT="Copyright $(YEAR) John T. O&apos;Donnell"
MDLATEST="For latest version, see <a href='https://github.com/jtod/Hydra' target='_blank'>https://github.com/jtod/Hydra</a>"
MDHEADER=$(MDVERSION).\ $(MDCOPYRIGHT).\ $(MDLATEST).

#----------------------------------------------------------------------
# Installation

# User: install Haskell libraries needed to run circuits
.PHONY : userinstall
userinstall :
	cabal update
	cabal install --lib
	cabal haddock
	# Warning: Cabal install does not actually install the code
	# See README.md, which explains how to run Hydra

# Developer: build files to be disseminated
.PHONY : build
build :
	make docs/userguide/HydraUserGuide.html

#----------------------------------------------------------------------
# User guide: generate html from the source .org file

docs/userguide/HydraUserGuide.html : docs/userguide/HydraUserGuide.org \
	  docs/userguide/userguide-template.html
	pandoc --standalone \
          --from=org \
          --to=html \
          --metadata title="Hydra User Guide" \
          --template=docs/userguide/userguide-template.html \
          --table-of-contents --toc-depth=4 \
	  --variable=mdheader:${MDHEADER} \
          --variable=css:./HydraUserGuide.css \
          --output=docs/userguide/HydraUserGuide.html \
	  docs/userguide/HydraUserGuide.org
