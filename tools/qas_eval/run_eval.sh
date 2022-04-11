#!/bin/bash

sif_dir=$PWD
export SINGULARITY_TMPDIR=$sif_dir/singularity_tmpdir
mkdir -p $SINGULARITY_TMPDIR

date_now=$(date +'%Y%m%d-%H%m%S')
DIR_OUT_NOW="qas_out/${date_now}"

function exec_aval() {

  # $HOME é o home do singularity
  PATH_MODEL="${HOME}/${PATH_MODEL}"

  SUB_DIR_OUT="${DIR_OUT_NOW}/${MODEL}"
  mkdir -p ${PWD}/${SUB_DIR_OUT}
  DIR_OUT=${HOME}/${SUB_DIR_OUT}

  echo ""
  echo "Check Quality Reports ${DIR_OUT_NOW}/QualityReport_${MODEL}.pdf (FortranAnalyser) and ${DIR_OUT_NOW}/Check_Report_${MODEL}.txt (Check.py)"

  echo "Reports are also printed below. In pdf case, it was converted to txt and printed only tail. Check full report on pdf"
  echo ""
  echo "========================================================== "
  echo " Report from Check.py "

  singularity exec --bind ${sif_dir}:$HOME ${sif_dir}/qas_eval.sif ${HOME}/monan/tools/qas_eval/getInfo.bash $PATH_MODEL $DIR_OUT $MODEL

  singularity exec --bind ${sif_dir}:$HOME ${sif_dir}/qas_eval.sif /home/qas_files/tools/jdk-17.0.2/bin/java -cp  /home/qas_files/tools/FortranAnalyser.jar es.uvigo.esei.ephyslab.fortrananalyser.Principal "pt" $PATH_MODEL "${DIR_OUT}/QualityReport_${MODEL}.pdf" "NOGUI"

  echo ""
  echo "========================================================== "
  echo "Report from FortranAnalyser (Check full report at ${SUB_DIR_OUT}/QualityReport_${MODEL}.pdf - below may not show all final table)"
  pdftotext ${SUB_DIR_OUT}/QualityReport_${MODEL}.pdf
  tail -63 ${SUB_DIR_OUT}/QualityReport_${MODEL}.txt

  echo "===================== END OF MODEL ${MODEL} REPORT ====================================== "

}

MODEL="FV3"
PATH_MODEL="monan/codigos_originais/ufs-weather-model-develop-2022-03-29-10h20/FV3/atmos_cubed_sphere/model"
exec_aval

MODEL="MPAS"
PATH_MODEL="monan/codigos_originais/MPAS-Model/src/core_atmosphere"
exec_aval

MODEL="GEF"
PATH_MODEL="monan/codigos_originais/GEF_v1.0.0/gef_trunk/RUN/src"
exec_aval

echo ""
echo "---"
echo "All done ! "
echo "======================== ALL REPORTS END ========================================================= "

