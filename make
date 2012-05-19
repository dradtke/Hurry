#!/bin/bash
if [[ $1 = "clean" ]]; then
	rm -f *.o Allegro/*.o c/*.o *.hi Main
	echo "  Clean as a jiffy, suh."
	exit 0
fi

# the compilation command
ghc --make *.hs c/al-wrapper.c -lallegro
