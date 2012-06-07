#!/bin/bash
if [[ $1 = "clean" ]]; then
	rm -f *.{o,hi} Allegro/*.{o,hi} c/*.o Main
	echo "  Clean as a jiffy, suh."
	exit 0
fi

# the compilation command
ghc --make *.hs c/al-wrapper.c -lallegro
