#!/bin/bash
rm all.type.txt
for f in *.F
do
 echo "Processing $f"
 fortran-src $f -S -F 90 --show-make-graph > $f.dot
 fortran-src $f -t -F 90 >> all.type.txt
 dot -Tpng $f.dot -o $f.png
done
