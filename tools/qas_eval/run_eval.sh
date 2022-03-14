#!/bin/bash
sif_dir=$1
singularity exec --bind ${sif_dir}:$HOME ${sif_dir}/qas_eval.sif ${HOME}/monan/tools/qas_eval/getInfo.bash "${HOME}//DinCore/GFDL_atmos_cubed_sphere/model/" 
