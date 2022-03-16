#!/bin/bash
sif_dir=$1

MODEL="fv3"

export SINGULARITY_TMPDIR=$sif_dir/singularity_tmpdir
singularity exec --bind ${sif_dir}:$HOME ${sif_dir}/qas_eval.sif ${HOME}/monan/tools/qas_eval/getInfo.bash "${HOME}//DinCore/GFDL_atmos_cubed_sphere/model/" 
singularity exec --bind ${sif_dir}:$HOME ${sif_dir}/qas_eval.sif /home/qas_files/tools/jdk-17.0.2/bin/java -cp $HOME/monan/tools/qas_eval/FortranAnalyser-v2.0/target/FortranAnalyser.jar es.uvigo.esei.ephyslab.fortrananalyser.Principal "pt" "${HOME}/DinCore/GFDL_atmos_cubed_sphere/model/" "${HOME}/qas_out/QualityReport_${MODEL}.pdf" "NOGUI"
