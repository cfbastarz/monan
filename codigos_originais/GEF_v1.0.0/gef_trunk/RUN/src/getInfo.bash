#!/bin/bash
rm all.type.txt
for f in *.f90
do
 echo "Processing $f"
 ~/../home2/workspace/script_fortran/fortran-src $f -S --show-make-graph > $f.dot
 ~/../home2/workspace/script_fortran/fortran-src $f -t  >> all.type.txt
 dot -Tpng $f.dot -o $f.png
done