#!/bin/bash
DINCORE_PATH=$1
DIR_OUT=$2
MODEL=$3

TOOLS="/home/qas_files/tools"
export LD_LIBRARY_PATH="${TOOLS}/graphviz/local/lib:${LD_LIBRARY_PATH}"

for f in $(find ${DINCORE_PATH} -type f \( -iname \*.f90 -o -iname \*.F90 -o -iname \*.F \))
do
 echo "Processing $f"
 $TOOLS/fortran-src $f -S -F 90 --show-make-graph > $f.dot
 $TOOLS/fortran-src $f -t -F 90 >> $DIR_OUT/all.type.txt
 $TOOLS/graphviz/bin/dot -Tsvg $f.dot -o $f.svg
done

python3 ${HOME}/monan/tools/qas_eval/check.py $DINCORE_PATH $DIR_OUT $MODEL
