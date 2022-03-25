#!/bin/bash -l

#PBS -o err.gef
#PBS -e err.gef
#PBS -N run_gef
#PBS -A rm
#PBS -q debug
#PBS -l procs=600
#PBS -l walltime=00:30:00
#PBS -d .

set -aeux

module load intel
module load mpt
module load netcdf
module load adaptive

cd $PBS_O_WORKDIR

RUN_DIR=__RUN_DIR__

ln -sf data_in/static2/co2.38_25mb co2.dat
ln -sf data_in/static2/eta_micro_lookup.dat eta_micro_lookup.dat

mpiexec_mpt -prefix "[%g]" -np $PBS_NP ${RUN_DIR}/src/GEF.exe
