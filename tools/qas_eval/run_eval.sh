#!/bin/bash
sif_dir=$1
export SINGULARITY_TMPDIR=$sif_dir/singularity_tmpdir
singularity exec --bind ${sif_dir}:$HOME ${sif_dir}/qas_eval.sif ${HOME}/monan/tools/qas_eval/getInfo.bash "${HOME}//DinCore/GFDL_atmos_cubed_sphere/model/" 

singularity shell --bind ${sif_dir}:$HOME ${sif_dir}/qas_eval.sif
