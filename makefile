#----------------------------------------------------------------------
# makefile for Hydra
#----------------------------------------------------------------------

#  make userinstall     user requires ghc and cabal
#  make devinstall      developer requires ghc, cabal, pandoc
#  make clean           delete temp files but keep documentation

#----------------------------------------------------------------------
# Calculate parameters: don't need to edit these

# Obtain version number from cabal file to use in generated html
# documentation
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
# User installation

.PHONY : userinstall
userinstall :
	cabal v2-update
	cabal v2-install --lib
	cabal v2-haddock

# Developer: build files to be disseminated

# make setVersion

.PHONY: setVersion
setVersion:
	echo "$(VERSION)" > VERSION.txt
	echo "Copyright (c) $(YEAR) John T. O'Donnell" > COPYRIGHT.txt

.PHONY : build
build :
	make setVersion
	make docs

#----------------------------------------------------------------------
# User guide and README: generate html and tex from .org using emacs

.PHONY : docs
docs :
	make README.pdf
	make docs/UserGuide/HydraUserGuide.pdf

README.pdf: README.tex
	pdflatex README
	pdflatex README

docs/UserGuide/HydraUserGuide.pdf : docs/UserGuide/HydraUserGuide.tex
	pdflatex docs/UserGuide/HydraUserGuide

#----------------------------------------------------------------------
.PHONY : clean
clean :
	find . \( -name '*~' \
	  -o -name '*.hi' \
	  -o -name '*.log' \
	  -o -name '*.aux' \
	  -o -name '*.out' \
	  -o -name '*.toc' \
	  -o -name '*.bak' \) -delete
	rm -rf docs/auto
	rm -f m1output.txt
	rm -f m1write.txt

# deprecated: make build
#	echo ${MDHEADER}
#	make docs/userguide/HydraUserGuide.html
#	make index.html

# deprecated
index.html : README.md \
	  docs/userguide/userguide-template.html
	pandoc --standalone \
          --from=markdown \
          --to=html \
	  --metadata pagetitle="Hydra hardware description langauge" \
          --template=docs/userguide/userguide-template.html \
	  --variable=mdheader:${MDHEADER} \
          --variable=css:index.css \
          --output=index.html \
	  README.md

# deprecated	rm -f docs/M1_System_Circuit.tex

# deprecated; using org to generate html
docs/userguide/HydraUserGuide.html : docs/userguide/HydraUserGuide.org \
	  docs/userguide/userguide-template.html \
	  Hydra.cabal
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
