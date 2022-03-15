#!/bin/bash
DIROUT=qas_out/$(date +'%Y%m%d-%H%m%S')
mkdir -p ${DIROUT}

DINCORE_PATH=$1
TOOLS="/home/qas_files/tools"
export LD_LIBRARY_PATH="${TOOLS}/graphviz/local/lib:${LD_LIBRARY_PATH}"
for f in $(find $DINCORE_PATH -type f \( -iname \*.f90 -o -iname \*.F90 \))
do
 echo "Processing $f"
 $TOOLS/fortran-src $f -S -F 90 --show-make-graph > $f.dot
 $TOOLS/fortran-src $f -t -F 90 >> $DIROUT/all.type.txt
 $TOOLS/graphviz/bin/dot -Tsvg $f.dot -o $f.svg
done

python3 ${HOME}/monan/tools/qas_eval/check.py $DINCORE_PATH
