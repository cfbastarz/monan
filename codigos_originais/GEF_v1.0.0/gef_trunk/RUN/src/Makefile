include ../../PRP/src/include/make.inc

EXE=GEF.exe

LIB=${W3_LIB}


OBJ=MODULE_F77KINDS.o      MODULE_PARMETA.o       MODULE_CONTIN.o        MODULE_CTLBLK.o        MODULE_DOM.o\
    MODULE_DYNAM.o         MODULE_MASKS.o         MODULE_METRCS.o        MODULE_MPPSTAFF.o      MODULE_NEBMAP.o\
    MODULE_REALPAR.o       MODULE_SET_ASSOC.o     MODULE_SET_ZERO.o      MODULE_VRBLS.o         MODULE_HS.o\
    MODULE_DGNSOUT.o       MODULE_PVRBLS.o        MODULE_PARM_TBL.o      MODULE_PHYS.o          MODULE_LOOPS.o\
    MODULE_PARMSOIL.o      MODULE_ACMRDL.o        MODULE_ACMRDS.o        MODULE_ACMSFC.o        MODULE_ACMPRE.o\
    MODULE_SOIL.o          MODULE_CNVCLD.o        MODULE_ACMCLD.o        MODULE_CLDWTR.o        MODULE_SEASO3.o\
    MODULE_HCON.o          MODULE_RDPARM.o        MODULE_ACMCLH.o        MODULE_RNDDTA.o        MODULE_CO2DTA.o\
    MODULE_TABCOM.o        MODULE_PPTASM.o        MODULE_C_FRACN.o       MODULE_MAPOT.o         MODULE_KFFDBK.o\
    MODULE_KFLUT.o         MODULE_CUPARM.o        MODULE_NHYDRO.o        MODULE_TPRFIL.o        MODULE_TBLTMP.o\
    MODULE_SSALB.o         MODULE_SWRSAV.o        MODULE_CMICRO_CONS.o   MODULE_CMICRO_STATS.o  MODULE_CMY600.o\
    MODULE_IACCR_TABLES.o  MODULE_IMASS_TABLES.o  MODULE_IRATE_TABLES.o  MODULE_IRIME_TABLES.o  MODULE_IVENT_TABLES.o\
    MODULE_SDENS_TABLES.o  MODULE_RACCR_TABLES.o  MODULE_RMASS_TABLES.o  MODULE_RVELR_TABLES.o  MODULE_RRATE_TABLES.o\
    MODULE_RVENT_TABLES.o  MODULE_COMPVS0.o       MODULE_COMPVS.o        MODULE_ASTSAV.o        MODULE_ABCI.o\
    MODULE_O3DATA.o        MODULE_OPTIONS.o       MODULE_RDFSAV.o        MODULE_RITE.o          MODULE_TEND.o\
    MODULE_SAVMEM.o        MODULE_CMICRO_START.o  MODULE_RD1TIM.o        MODULE_CUINIT.o        MODULE_SCRTCH.o\
    BOCOHMPI.o             BOCOVMPI.o             BOCOV_HMPI.o           ALLOC.o		ZERO.o\
    AVRV3.o                ADJUST.o               FINISHMPI.o            HZADV.o                INIT.o\
    INITDOM.o              INITMPI.o              INITTOP2.o             KINEN.o                OUT.o\
    OUTSD.o                PDETE.o                PDNEW.o                VTADV.o                UTIL.o\
    NEWTON.o               INITTOP1.o             ARR_OP.o               FOUR1.o\
    FLUX_CORRECTION.o      FLUX_CORRECTION1D.o    ISCORNER2.o            HDIFF.o                INITCKMM.o\
    CHECKMXMN.o            REALFT.o               TURBL.o                MIXLEN.o               PRODQ2.o\
    DIFCOF.o               VDIFQ.o                SFCDIF.o               SURFCE.o               VDIFH.o\
    VDIFV.o                SFLX.o                 RADTN.o                ZENITH.o               OZON2D.o\
    RADFS.o                CLO89.o                LWR88.o                SWR93.o                FST88.o\
    E1E290.o               SPA88.o                E290.o                 E2SPEC.o               E3V88.o\
    RDTEMP.o               GRADFS.o               HCONST.o               O3INT.o                CONRAD.o\
    TABLE.o                SOLARD.o               O3CLIM.o               GFDLRD.o               INIT_RNDDTA.o\
    GSMDRIVE.o             GSMCONST.o             GSMCOLUMN.o            KFDRIVE.o              KFPARA.o\
    TPMIX2.o               DTFRZ2.o               CONDLOAD.o             ENVIRTHT.o             PROF5.o\
    TPMIX2DD.o             KFTEND.o               CLTEND.o               LUTAB.o                HZADVQ.o\
    DDAMP.o                TIMEF_BOG.o            CHSST.o                DIGFLT.o               OUT2.o\
    CORNERHM4.o            BOCOVMPI_SC.o\
    CORNER_MFL.o           CORNERHM2.o            SPONGE.o               OUT_HEAT.o		SHAP_FILTER.o\
    GEF.o                  SSTCH.o                VEGUPDT.o              GETDATE.o\
    READ_SST12M.o          TTBLEX.o               CUCNVC.o               READ_RESTRT.o                   

# SUFFIXES: .f90 .o .a

#test 03/09/2019 -default64
$(EXE): $(OBJ)
	${FCMPI} $(FFLAGS) ${OBJ} ${LIB} -o ${EXE} 

#original commented 03/09/2019
#$(EXE): $(OBJ)
#	${FCMPI} $(FFLAGS) ${OBJ} ${LIB} -o ${EXE} 



#ADJUST.o: ADJUST.f90
#	$(FCMPI) -g -Kieee -Mchkstk -Mchkfpstk -Mchkptr -Mpgicoff -c $<

#ZENITH.o: ZENITH.f90
#	$(FCMPI) -g -Kieee -Mchkstk -Mchkfpstk -Mchkptr -Mpgicoff -c $<

#FST88.o: FST88.f90
#	$(FCMPI) -g -Kieee -Mchkstk -Mchkfpstk -Mchkptr -Mpgicoff -c $< 

#LWR88.o: LWR88.f90
#	$(FCMPI) -g -Kieee -Mchkstk -Mchkfpstk -Mchkptr -Mpgicoff -c $<

#RADFS.o: RADFS.f90
#	$(FCMPI) -g -Kieee -Mchkstk -Mchkfpstk -Mchkptr -Mpgicoff -c $<

#RADTN.o: RADTN.f90
#	$(FCMPI) -g -Kieee -Mchkstk -Mchkfpstk -Mchkptr -Mpgicoff -c $<


#test 03/09/2019 -default64
%.mod:%.f90
	$(FCMPI) $(FFLAGS)  -c $<

%.o:%.f90
	$(FCMPI) $(FFLAGS)  -c $<


#original commented 03/09/2019
#%.mod:%.f90
#	$(FCMPI) $(FFLAGS) -c $<

#%.o:%.f90
#	$(FCMPI) $(FFLAGS) -c $<




clean:
	rm -f *.o *.mod ${EXE}
