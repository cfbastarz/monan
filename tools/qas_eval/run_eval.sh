#!/bin/bash

MODEL=$1
PATH_MODEL=$2
# EX
# MODEL="fv3"
# PATH_MODEL="DinCore/GFDL_atmos_cubed_sphere/model/"

# $HOME é o home do singularity
PATH_MODEL="${HOME}/${PATH_MODEL}"

sif_dir=$PWD

export SINGULARITY_TMPDIR=$sif_dir/singularity_tmpdir
mkdir -p $SINGULARITY_TMPDIR

SUB_DIR_OUT="qas_out/$(date +'%Y%m%d-%H%m%S')"
mkdir -p ${PWD}/${SUB_DIR_OUT}
DIR_OUT=${HOME}/${SUB_DIR_OUT}



function exec_aval() {

singularity exec --bind ${sif_dir}:$HOME ${sif_dir}/qas_eval.sif ${HOME}/monan/tools/qas_eval/getInfo.bash $PATH_MODEL $DIR_OUT $MODEL
singularity exec --bind ${sif_dir}:$HOME ${sif_dir}/qas_eval.sif /home/qas_files/tools/jdk-17.0.2/bin/java -cp  /home/qas_files/tools/FortranAnalyser.jar es.uvigo.esei.ephyslab.fortrananalyser.Principal "pt" $PATH_MODEL "${DIR_OUT}/QualityReport_${MODEL}.pdf" "NOGUI"

}

exec_aval

echo "---"
echo " All done ! "
echo "Check Quality Reports ${SUB_DIR_OUT}/QualityReport_${MODEL}.pdf and ${SUB_DIR_OUT}/Check_Report_${MODEL}.txt"


