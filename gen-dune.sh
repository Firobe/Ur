#/bin/sh
SKEL=dune.skel
SHARE_DIRS=data

files=$(find $SHARE_DIRS -type f -exec echo \(\"{}\" as \"{}\"\) \;)
escaped=$(echo $files | sed 's/\./\./g' | sed 's/&/\\&/g')
sed "s|#FILES#|$escaped|" $SKEL > dune
