#!/bin/ksh 
#set -aeux

IM0=401             # Dimension of each face, for cube the total number of grid points in one layer is IM0xIM0x6
LM=50               # Number of vertical layers, should always be 38 for this version in conrad.f90 open file co2.dat for LM 38 or co2.60_10mb for LM 60
NSUB=10             # Subdivision number, e.g. if NSUB 2, each face are divided, into 2x2
NMIN=6              # Is the number of faces, should always be 6 for cube, 14 for octagon
idatin=12,16,2019    # starting time of integration in UTC (mes,dia,ano)
ihrstin=0
nday=30             # total length of integration in days
outnum=120          # number of outputs
dt=40
ndassimm=0
sstc=1              # sst data source, 1--NCEP, 2--TMI
sigmain=false
FInitBC=gfs0.25

########RESTART##################

restrt=false           #restart

nhrs=1                #defines the initial output and increases during the integration of the model, after writing each output file in OUT2.f90
                      #it depends on output frequency, currently set to 6 h, so nhrs=1 after 6 h of integration, nhrs=2, after 12 h of integration, etc 
                      #when restrt=true, it defines the time of the restart of the model (for example, if nhrs=120, the model will continue integration 30 days
		      #after the initial integration)

#restart frequency is defined in RUN/src/OUT2.f90, line 328, it is currently set to create restart file on day 1 of each month, at 00 UTC
#please, change it there, by defining yr, mon, day, utc, according to your needs

#################################

typeset -Z2 dia mes hora
typeset -Z4 ano
dia=`echo ${idatin} | cut -d, -f2`
mes=`echo ${idatin} | cut -d, -f1`
ano=`echo ${idatin} | cut -d, -f1,3`
hora=${ihrstin}
Run_Date=${ano}${mes}${dia}${hora}


outstr=${ndassimm}
let ntstm=3600*24*${nday}/${dt}      # total time steps

let outend=${ntstm}+${ndassimm} 
let iout1=${outend}-${outstr}
let iout=${iout1}/${outnum}   # steps between two output

let NPR=${NMIN}*${NSUB}*${NSUB}


# define locations of source code and temporary run directories
MYDIR=/scratchin/grupos/grpeta/projetos/tempo/oper/GEF_v1.0.0-KF/gef_trunk
TMP_DIR=/scratchin/grupos/grpeta/projetos/tempo/oper/GEF_v1.0.0-KF/gef_trunk/PRP/gef_tmp
DIR_OUT=/scratchout/grupos/grpeta/projetos/tempo/oper/GEF_v1.0.0-KF

PRP_DIR=${MYDIR}/PRP
RUN_DIR=${MYDIR}/RUN
ETA_RUN=${DIR_OUT}/${Run_Date}

mkdir -p ${DIR_OUT}/${Run_Date}
mkdir -p ${DIR_OUT}/err
##mkdir -p ${DIR_OUT}/GLOBRUN/${Run_Date}
mkdir -p ${DIR_OUT}/INIT_DATA
mkdir -p ${DIR_OUT}/out
#####################################################


####### Configuration file ##########################
##GSM rm -f CONF
##GSM echo "IM0= "${IM0} >> CONF
##GSM echo "LM= "${LM} >> CONF
##GSM echo "NSUB= "${NSUB} >> CONF
##GSM echo "NMIN= "${NMIN} >> CONF
##GSM echo "DT= "${dt} >> CONF
##GSM echo "SIGMA= "${sigmain} >> CONF
##GSM echo "OUTNUM= "${outnum} >> CONF

INIM0=`cat CONF | grep "IM0=" | cut -d= -f2`
INLM0=`cat CONF | grep "LM=" | cut -d= -f2`
INSUB=`cat CONF | grep "NSUB=" | cut -d= -f2`
INNMI=`cat CONF | grep "NMIN=" | cut -d= -f2`
DTIN=`cat CONF | grep "DT=" | cut -d= -f2`
SIGMAIN=`cat CONF | grep "SIGMA=" | cut -d= -f2`
OUTNUMIN=`cat CONF | grep "OUTNUM=" | cut -d= -f2`
#####################################################


if [ ${IM0} != ${INIM0} ] || [ ${LM} != ${INLM0} ] || [ ${NSUB} != ${INSUB} ] || [ ${NMIN} != ${INNMI} ] || [ ${dt} != ${DTIN} ] || [ ${sigmain} != ${SIGMAIN} ] || [ ${outnum} != ${OUTNUMIN} ] ; then

rm -f CONF
echo "IM0= "${IM0} >> CONF
echo "LM= "${LM} >> CONF
echo "NSUB= "${NSUB} >> CONF
echo "NMIN= "${NMIN} >> CONF
echo "DT= "${dt} >> CONF
echo "SIGMA= "${sigmain} >> CONF
echo "OUTNUM= "${outnum} >> CONF

if [ $NSUB -eq 1 ]
then
   IM=$IM0
else
   IM=$IM0/$NSUB+1
fi

cd $MYDIR/RUN/src
###cat PARMETA.in | sed s:IM0IN:$IM0:g | sed s:IMIN:$IM:g | sed s:LMIN:$LM:g| sed s:NSUBIN:$NSUB:g | sed s:NMIN:$NMIN:g  > MODULE_PARMETA.f90
#RESTART
cat PARMETA.in | sed s:IM0IN:$IM0:g | sed s:IMIN:$IM:g | sed s:LMIN:$LM:g| sed s:NSUBIN:$NSUB:g | sed s:NMIN:$NMIN:g | sed s:outnumin:$outnum:g | sed s:ndayin:$nday:g > MODULE_PARMETA.f90



cd $MYDIR/PRP/src/include
cat param_o.in | sed s:im0in:$IM0:| \
sed s:lmin:$LM:| sed s:nsubin:$NSUB:| \
sed s:nmin:$NMIN:> param_o.h

cat const.in | sed s:dtin:$dt:| sed s:idatin:$idatin:| sed s:ihrstin:$ihrstin:|\
sed s:outnumin:$outnum:| sed s:ndayin:$nday:|sed s:sigmain:$sigmain:| \
sed s:sstcin:$sstc:>const.h 

cd ../../../

fi

#####################
###### RESTART ######
#####################
if [ $restrt == false ]
then

mkdir -p ${DIR_OUT}/GLOBRUN/${Run_Date}
datactl=`${MYDIR}/util/caldate.3.0 ${Run_Date} + 0d 'hhZddmmmyyyy' `
cat ${PRP_DIR}/bin/globrun.ctl_NEW | sed s:__datactl__:${datactl}:g  > ${DIR_OUT}/GLOBRUN/${Run_Date}/globrun.ctl

rm -rf ${ETA_RUN}/restrt_dates.txt

cd $MYDIR/PRP/src/include
cat param_o.in | sed s:im0in:$IM0:| \
sed s:lmin:$LM:| sed s:nsubin:$NSUB:| \
sed s:nmin:$NMIN:  | sed s:restdatein:$Run_Date:> param_o.h
fi

if [ $restrt == true ]
then
#let hoursofintegr=${nhrs}*6  (nday*24)/outnum
let outputfreq=${nday}*24/${outnum}
let hoursofintegr=${nhrs}*${outputfreq}
restartdate=`${MYDIR}/util/caldate.3.0 ${Run_Date} + ${hoursofintegr}h 'ddmmyyyyhh' ` 
mkdir -p ${DIR_OUT}/GLOBRUN/RESTART_$restartdate 

let ctlrestt=${hoursofintegr}+${outputfreq}
#echo "ctlrtest= "${ctlrestt}
datactlr=`${MYDIR}/util/caldate.3.0 ${Run_Date} + ${ctlrestt}h 'hhZddmmmyyyy' `
#echo "datactlr= "${datactlr}
cat ${PRP_DIR}/bin/globrun.ctl_NEW | sed s:__datactl__:${datactlr}:g  > ${DIR_OUT}/GLOBRUN/RESTART_$restartdate/globrun.ctl


cd $MYDIR/PRP/src/include
cat param_o.in | sed s:im0in:$IM0:| \
sed s:lmin:$LM:| sed s:nsubin:$NSUB:| \
sed s:nmin:$NMIN:  | sed s:restdatein:$restartdate:> param_o.h

cd $MYDIR/PRP/src/post_pgi_single
make clean
make

fi
######################

# remove and create temporary run directory
rm -rf ${TMP_DIR}
mkdir ${TMP_DIR}

cat << EOF > ${MYDIR}/cnstdata
&CNSTDATA
 INDAY=${nday},IHRST=${ihrstin},IDAT=${idatin},IOUTNUM=${outnum},IIOUT=${iout}
&END
EOF

# copy preprocessing and model run scripts
cat ${PRP_DIR}/bin/run_grid.sh               | sed s:__TMP_DIR__:${TMP_DIR}:g  | sed s:__PRP_DIR__:${PRP_DIR}:g | sed s:__ETA_RUN__:${ETA_RUN}:g | sed "s/IM1/${IM0}/g" | sed "s/LM1/${LM}/g" | sed "s/NSUB1/${NSUB}/g" | sed "s/NMIN1/${NMIN}/g" | sed s:__FInitBC__:${FInitBC}:g > ${TMP_DIR}/run_grid.sh
cat ${PRP_DIR}/bin/run_init.sh               | sed s:__TMP_DIR__:${TMP_DIR}:g  | sed s:__PRP_DIR__:${PRP_DIR}:g | sed s:__ETA_RUN__:${ETA_RUN}:g | sed s:__FInitBC__:${FInitBC}:g             | sed s:__DATE__:${Run_Date}:g > ${TMP_DIR}/run_init.sh
cat ${RUN_DIR}/job/run_gef.sh                | sed s:__RUN_DIR__:${RUN_DIR}:g  | sed s:__PRP_DIR__:${PRP_DIR}:g | sed s:__ETA_RUN__:${ETA_RUN}:g | sed s:__NPR__:${NPR}:g > ${TMP_DIR}/run_gef.sh
cat ${PRP_DIR}/bin/run_topo.sh               | sed s:__PRP_DIR__:${PRP_DIR}:g  | sed s:__ETA_RUN__:${ETA_RUN}:g > ${TMP_DIR}/run_topo.sh
cat ${PRP_DIR}/bin/run_etapost_aux.sh        | sed s:__TMP_DIR__:${TMP_DIR}:g  | sed s:__ETA_RUN__:${ETA_RUN}:g > ${TMP_DIR}/run_etapost_aux.sh
cat ${PRP_DIR}/bin/submit_post_aux.ksh_Templ | sed s:__TMP_DIR__:${TMP_DIR}:g  | sed s:__ETA_RUN__:${ETA_RUN}:g > ${TMP_DIR}/submit_post_aux.ksh

chmod 755 ${TMP_DIR}/run_grid.sh
chmod 755 ${TMP_DIR}/run_topo.sh
chmod 755 ${TMP_DIR}/run_init.sh
chmod 755 ${TMP_DIR}/run_etapost_aux.sh
chmod 755 ${TMP_DIR}/submit_post_aux.ksh
chmod 755 ${TMP_DIR}/run_gef.sh

#RESTART
cat << EOF > ${ETA_RUN}/restrt_nmlst.txt
${restrt}
${nhrs}
EOF

#GSMdatactl=`${MYDIR}/util/caldate.3.0 ${Run_Date} + 0d 'hhZddmmmyyyy' `
#GSMcat ${PRP_DIR}/bin/globrun.ctl_NEW | sed s:__datactl__:${datactl}:g  > ${DIR_OUT}/GLOBRUN/${Run_Date}/globrun.ctl
                                     
rm -f ${ETA_RUN}/GEF_clouds0001.0006

cat $PRP_DIR/bin/get_${FInitBC}_data.ksh_Templ | sed s:__DIR_OUT__:${DIR_OUT}:g  > ${TMP_DIR}/get_${FInitBC}_data.ksh
cat $PRP_DIR/bin/${FInitBC}_deco.sh_Templ | sed s:__DIR_OUT__:${DIR_OUT}:g  > ${TMP_DIR}/${FInitBC}_deco.sh

chmod 755 ${TMP_DIR}/get_${FInitBC}_data.ksh 
chmod 755 ${TMP_DIR}/${FInitBC}_deco.sh


cp ${PRP_DIR}/INIT_DATA/feb1998/ICEC_WEASD_${FInitBC}.bin ${PRP_DIR}/INIT_DATA/feb1998/ICEC_WEASD.bin
#cp /scratchin/oper/tempo/oensMB09/pre/datain/gdas1.T${hora}Z.sstgrd.${Run_Date} ${ETA_RUN}/sstgrb_gfs # Modelo Funcional Padrão
cp /scratchin/grupos/grpeta/projetos/tempo/oper/GEF_v1.0.0-KF/gef_trunk/SST/SST_Eta.bin ${ETA_RUN}/sstgrb_gfs
cd ${TMP_DIR}
chmod u+x run_*.sh

ln -sf ${PRP_DIR}/data_in .
ln -sf ${PRP_DIR}/INIT_DATA .

# run the first script which will then run/submit subsequent scripts/jobs
${TMP_DIR}/run_grid.sh
#${TMP_DIR}/run_topo.sh
#${TMP_DIR}/run_init.sh
#qsub ${TMP_DIR}/run_gef.sh
