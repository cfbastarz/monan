#!/bin/bash
#PBS -N GEF_cube
#PBS -o __ETA_RUN__/../err/err_gef
#PBS -q pesq
#PBS -l mppwidth=__NPR__
#PBS -l walltime=6:00:00
#PBS -A CPTEC
#PBS -j oe

 
set -aeux

#module load intel
#module load mpt
#module load netcdf
#module load adaptive

#####cd $PBS_O_WORKDIR

RUN_DIR=__RUN_DIR__
PRP_DIR=__PRP_DIR__
ETA_RUN=__ETA_RUN__

cd ${ETA_RUN}

#mpiexec_mpt -prefix "[%g]" -np $PBS_NP ${RUN_DIR}/src/GEF.exe
#aprun -prefix "[%g]" -np $PBS_NP ${RUN_DIR}/src/GEF.exe
#aprun -n 600 ${RUN_DIR}/GEF.exe
ulimit -c unlimited
ulimit -s unlimited
ulimit -a

#rm core
#echo "TEST"

T1=`date +%s%N`
 
aprun -n __NPR__ ${RUN_DIR}/src/GEF.exe 1> ${ETA_RUN}/../err/err.gef 2>&1 
 
T2=`date +%s%N`

TEMPO=`echo ${T2} - ${T1} | bc -l`
TEMPO=`echo ${TEMPO} / 1000000000 | bc -l > ${ETA_RUN}/runtime.gef`

##qsub ${TMP_DIR}/run_etapost.sh


