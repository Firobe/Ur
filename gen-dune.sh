#/bin/sh
SKEL=dune.skel
DIRS="shaders themes"

files=$(find $DIRS -type f -exec echo \(\"{}\" as \"{}\"\) \;)
escaped=$(echo $files | sed 's/\./\./g' | sed 's/&/\\&/g')
sed "s|#FILES#|$escaped|" $SKEL > dune
