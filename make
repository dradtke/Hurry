#!/bin/bash
#
# Temporary build script for Hurry.
# This should ultimately get replaced with Setup.hs and a cabal project file.
#

if [[ $1 = "clean" ]]; then
	rm -f *.{o,hi} Allegro/*.{o,hi} c/*.o Main Allegro/{Event,Display,Keyboard,Mouse,Graphics}.hs
	echo "  Clean as a jiffy, suh."
	exit 0
fi

# the compilation command
hsc2hs Allegro/*.hsc && ghc -debug -XRecordWildCards --make *.hs c/*.c -lallegro
