#!/bin/bash
#PBS -N GEF_cube
#PBS -o err_calculo
#PBS -q debug
#PBS -l mppwidth=2400
#PBS -l walltime=2:00:00
#PBS -A CPTEC
#PBS -j oe


set -aeux

#module load intel
#module load mpt
#module load netcdf
#module load adaptive

#####cd $PBS_O_WORKDIR

RUN_DIR=/scratchin/grupos/grpeta/projetos/tempo/oper/GEF_v1.0.0-KF/gef_trunk/RUN/src/Converte

cd ${RUN_DIR}

ulimit -c unlimited
ulimit -s unlimited
ulimit -a


aprun -n 2400 ${RUN_DIR}/calculo.x 1> ${RUN_DIR}/err.cal 2>&1








