!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief OUTPUT OF FIELDS
!> @details PRINT IN A SEPARATE FILE FOR EACH PE AND EACH OUTPUT PERIOD.
!> @author ORIGINATOR - ?????
!> @date ??-??-?? \n
!> @author LUCCI 
!> @date 18-03-20 \n
!> @version V1.1.0
!> @details MODERNIZATION OF THE CODE, INCLUDING:
!!                      * F77 TO F90/F95
!!                      * INDENTATION & UNIFORMIZATION CODE
!!                      * REPLACEMENT OF COMMONS BLOCK FOR MODULES
!!                      * DOCUMENTATION WITH DOXYGEN
!!                      * OPENMP FUNCTIONALITY 
!<
!> @details <b>Use Module:</b>
!! @arg @c ACMCLD
!! @arg @c ACMCLH
!! @arg @c ACMPRE
!! @arg @c ACMRDL
!! @arg @c ACMRDS
!! @arg @c ACMSFC
!! @arg @c CLDWTR
!! @arg @c CNVCLD
!! @arg @c CONTIN
!! @arg @c CTLBLK
!! @arg @c DGNSOUT
!! @arg @c LOOPS
!! @arg @c MASKS
!! @arg @c MPPSTAFF
!! @arg @c NHYDRO
!! @arg @c PARMETA
!! @arg @c PHYS
!! @arg @c PPTASM
!! @arg @c PVRBLS
!! @arg @c SOIL
!! @arg @c TEND
!! @arg @c VRBLS
!> @details <b>Driver:</b> 
!! @arg @c GEF
!> @details <b>Calls:</b>
!--------------------------------------------------------------------------------------------------     
    SUBROUTINE OUT2
!-------------------------------------------------------------------------------------------------- 
! SUBROUTINE OUT2
!
! SUBROUTINE: OUT2 - OUTPUT OF FIELDS
! PROGRAMMER: ?????  
! ORG: ?????
! DATE: ??-??-??
!
! ABSTRACT: 
! PRINT IN A SEPARATE FILE FOR EACH PE AND EACH OUTPUT PERIOD 
!
! PROGRAM HISTORY LOG:
! ??-??-??  ?????      - ORIGINATOR
! 18-01-15  LUCCI      - MODERNIZATION OF THE CODE, INCLUDING:
!                        * F77 TO F90/F95
!                        * INDENTATION & UNIFORMIZATION CODE
!                        * REPLACEMENT OF COMMONS BLOCK FOR MODULES
!                        * DOCUMENTATION WITH DOXYGEN
!                        * OPENMP FUNCTIONALITY
!
! INPUT ARGUMENT LIST:
! NONE
!
! OUTPUT ARGUMENT LIST:
! NONE
!
! INPUT/OUTPUT ARGUMENT LIST:
! NONE
!
! OUTPUT FILES:
! OUTPUT - PRINT FILE.
!
! USE MODULES: ACMCLD
!              ACMCLH
!              ACMPRE
!              ACMRDL
!              ACMRDS
!              ACMSFC
!              CLDWTR
!              CNVCLD
!              CONTIN
!              CTLBLK
!              DGNSOUT
!              LOOPS
!              MASKS
!              MPPSTAFF
!              NHYDRO
!              PARMETA
!              PHYS
!              PPTASM
!              PVRBLS
!              SOIL
!              TEND
!              VRBLS
!
! DRIVER     : GEF
!
! CALLS      : ----- 
!--------------------------------------------------------------------------------------------------
    USE ACMCLD
    USE ACMCLH
    USE ACMPRE
    USE ACMRDL
    USE ACMRDS
    USE ACMSFC
    USE CLDWTR
    USE CNVCLD
    USE CO2DTA   !RESTART
    USE C_FRACN  !RESTART
    USE CONTIN
    USE CTLBLK
    USE DGNSOUT
    USE DYNAM    !RESTART
    USE KFFDBK   !RESTART
    USE KFLUT    !RESTART
    USE LOOPS
    USE MASKS
    USE MPPSTAFF
    USE NHYDRO
    USE PARMETA
    USE PHYS
    USE PPTASM
    USE PVRBLS
    USE SEASO3   !RESTART
    USE SOIL
    USE TEND     !RESTART
    USE VRBLS

!RESTART
!    USE SEASO3 
!    USE KFLUT
!    USE KFFDBK
!    USE CO2DTA
!    USE C_FRACN
!
!
    IMPLICIT NONE
!
    CHARACTER(LEN=4)                                                                            ::&
    & C_NHRS
!   
    CHARACTER(LEN=150)                                                                          ::&
    & OUTFILE , OUTFILE2, OUTFILE3, OUTFILE4, OUTFILE5, OUTFILE6, OUTFILE7, OUTFILE8
    

!
!RESTART
    INTEGER(KIND=I4)                                                                            ::& 
    & IYR     , IMO     , IDY      ,                                                              &
    & UTC     , DAY     , MON      ,                                                              &
    & YR      , IUTC
!!!!!!!  

    NHRS = NHRS + 1
!
    WRITE(C_NHRS,'(I4.4)') NHRS 
!            
    OUTFILE = "GEFfcst_"//C_MYPE//"."//C_NHRS
!
    OPEN (UNIT=LISTOUT, FILE= TRIM(OUTFILE), FORM = 'UNFORMATTED')
!
    WRITE(LISTOUT) PD
    WRITE(LISTOUT) T
    WRITE(LISTOUT) U
    WRITE(LISTOUT) V
    WRITE(LISTOUT) Q
    WRITE(LISTOUT) CWM
    WRITE(LISTOUT) THS
    WRITE(LISTOUT) U00
    WRITE(LISTOUT) UL
!    WRITE(LISTOUT) W      
!    WRITE(LISTOUT) DWDT    !NON-HYDROSTATIC
!    WRITE(LISTOUT) Z
!
    CLOSE (LISTOUT)
!
    OUTFILE2 = "GEFfcstVEG_"//C_MYPE//"."//C_NHRS
!
    OPEN (UNIT=LISTOUT, FILE= TRIM(OUTFILE2), FORM = 'UNFORMATTED')
!       
    WRITE(LISTOUT) TSHLTR
    WRITE(LISTOUT) QSHLTR
    WRITE(LISTOUT) PLM
    WRITE(LISTOUT) SFCLHX
    WRITE(LISTOUT) SFCSHX
    WRITE(LISTOUT) QWBS !!!
    WRITE(LISTOUT) TWBS  !!!
    WRITE(LISTOUT) GRNFLX !!!
    WRITE(LISTOUT) VEGFRC 
    WRITE(LISTOUT) IVGTYP !!!
    WRITE(LISTOUT) ISLTYP !!!
    WRITE(LISTOUT) GLAT !!!
    WRITE(LISTOUT) GLON !!!
    WRITE(LISTOUT) SM   
    WRITE(LISTOUT) HTM
    WRITE(LISTOUT) VTM
    WRITE(LISTOUT) SICE
    WRITE(LISTOUT) Z0
    WRITE(LISTOUT) OMGALF
    WRITE(LISTOUT) ALBEDO
    WRITE(LISTOUT) ALBASE
    WRITE(LISTOUT) MXSNAL !!!
    WRITE(LISTOUT) CZEN !!!
    WRITE(LISTOUT) CZMEAN !!!
    WRITE(LISTOUT) FIS !!!
    WRITE(LISTOUT) Q10 !!!    
    WRITE(LISTOUT) TH10 !!!    
    WRITE(LISTOUT) QZ0 !!!    
    WRITE(LISTOUT) THZ0 !!!   
    WRITE(LISTOUT) U10
    WRITE(LISTOUT) V10
    WRITE(LISTOUT) UZ0 !!!
    WRITE(LISTOUT) VZ0 !!!
    WRITE(LISTOUT) USTAR !!!
    WRITE(LISTOUT) THS !!!
    WRITE(LISTOUT) Q2 !!!   
    WRITE(LISTOUT) TCUCN  !!!  
    WRITE(LISTOUT) TRAIN  !!!  
    WRITE(LISTOUT) DIV !!! 
    WRITE(LISTOUT) RTOP !!!
! 
    CLOSE (LISTOUT)
!
    OUTFILE3 = "GEF_sw_rdtn"//C_MYPE//"."//C_NHRS
!
    OPEN (UNIT=LISTOUT, FILE= TRIM(OUTFILE3), FORM = 'UNFORMATTED')
!
    WRITE(LISTOUT) RSWIN !!!
    WRITE(LISTOUT) RSWOUT !!!
    WRITE(LISTOUT) RSWTOA !!!
    WRITE(LISTOUT) ASWIN
    WRITE(LISTOUT) ASWOUT
    WRITE(LISTOUT) ASWTOA !!!
    WRITE(LISTOUT) RSWNET !!!
    WRITE(LISTOUT) RSWTT !!!
!
    CLOSE (LISTOUT)
!
    OUTFILE4 = "GEF_lw_rdtn"//C_MYPE//"."//C_NHRS
!
    OPEN (UNIT=LISTOUT, FILE= TRIM(OUTFILE4), FORM = 'UNFORMATTED')
!
    WRITE(LISTOUT) RLWIN !!!
    WRITE(LISTOUT) RLWOUT !!!
    WRITE(LISTOUT) RLWTOA !!!
    WRITE(LISTOUT) ALWIN
    WRITE(LISTOUT) ALWOUT
    WRITE(LISTOUT) ALWTOA 
    WRITE(LISTOUT) RLWNET !!!
    WRITE(LISTOUT) RLWTT !!!
    WRITE(LISTOUT) EPSR !!!
    WRITE(LISTOUT) RADOT  !!!  
    WRITE(LISTOUT) SIGT4  !!!
!    
    CLOSE (LISTOUT)
!
    OUTFILE5 = "GEF_clouds"//C_MYPE//"."//C_NHRS
!
    OPEN (UNIT=LISTOUT, FILE= TRIM(OUTFILE5), FORM = 'UNFORMATTED')
!
    WRITE(LISTOUT) CUPPT !!!
    WRITE(LISTOUT) CFRACL !!!
    WRITE(LISTOUT) CFRACM !!!
    WRITE(LISTOUT) CFRACH !!!
    WRITE(LISTOUT) ACFRCV !!!
    WRITE(LISTOUT) ACFRST !!!
    
    WRITE(LISTOUT) F_ICE !!!
    WRITE(LISTOUT) F_RAIN !!!
    WRITE(LISTOUT) F_RIMEF !!!

    WRITE(LISTOUT) CNVTOP !!!
    WRITE(LISTOUT) CNVBOT !!!
    WRITE(LISTOUT) HTOP
    WRITE(LISTOUT) HBOT
!
    CLOSE (LISTOUT)	 
!
    OUTFILE6 = "GEF_precandsoil"//C_MYPE//"."//C_NHRS
!
    OPEN (UNIT=LISTOUT, FILE= TRIM(OUTFILE6), FORM = 'UNFORMATTED')
!
    WRITE(LISTOUT) ACPREC
    WRITE(LISTOUT) CUPREC
    WRITE(LISTOUT) APREC
    WRITE(LISTOUT) ACSNOW
    WRITE(LISTOUT) ACSNOM !!!
    WRITE(LISTOUT) PREC
    WRITE(LISTOUT) STC
    WRITE(LISTOUT) SMC
    WRITE(LISTOUT) SMSTAV
    WRITE(LISTOUT) BGROFF
    WRITE(LISTOUT) SSROFF
    WRITE(LISTOUT) CMC !!!
    WRITE(LISTOUT) POTEVP
    WRITE(LISTOUT) POTFLX
    WRITE(LISTOUT) SFCEVP
    WRITE(LISTOUT) SUBSHX
    WRITE(LISTOUT) SNOPCX
    WRITE(LISTOUT) SI
    WRITE(LISTOUT) SNO !!!
    WRITE(LISTOUT) SMSTOT !!!
    WRITE(LISTOUT) SOILTB !!! 
    WRITE(LISTOUT) TG !!!
    WRITE(LISTOUT) SR !!!
    WRITE(LISTOUT) AKHS !!!
    WRITE(LISTOUT) AKMS !!!
!    
    CLOSE (LISTOUT)	 
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!RESTART

        IYR=IDAT(3)
        IMO=IDAT(1)
        IDY=IDAT(2)
        IUTC=NTSD*DT/3600

        CALL GETDATE(IYR,IMO,IDY,IUTC,YR,MON,DAY,UTC) 
	  
        OPEN(333, FILE= "restrt_dates.txt", POSITION='APPEND', FORM = 'FORMATTED')

	  
           IF(NHRS.EQ.1.AND.MYPE.EQ.0)THEN
	     WRITE(333,*) 'THE AVAILABLE RESTART DATES (DD/MM/YYYY) ARE:'
           ENDIF


!********************************
!RESTART OUTPUT FREQUENCY
!********************************

!GSM        IF(DAY.EQ.10.AND.UTC.EQ.0)THEN
        IF(DAY.EQ.11.AND.UTC.EQ.0)THEN


	   IF(MYPE.EQ.0)THEN
             WRITE(333,'(A,X,I2,X,A,X,I2,X,A,X,I4,2X,I2,X,A,4X,A,X,I6)') 'DATE:',DAY,'/',MON,'/',YR,UTC,'UTC,','NHRS =',NHRS
!	     CLOSE(333)
 	   ENDIF


         OUTFILE7 = "GEF_from_modules"//C_MYPE//"."//C_NHRS
         OPEN(UNIT=LISTOUT, FILE= TRIM(OUTFILE7), FORM = 'UNFORMATTED')

!soil_comm
         WRITE(LISTOUT)SFCEXC
         WRITE(LISTOUT)PCTSNO
	 WRITE(LISTOUT)ISLOPE
	 WRITE(LISTOUT)SH2O
	 WRITE(LISTOUT)SLDPTH
	 WRITE(LISTOUT)RTDPTH
!acmpre_comm
	 WRITE(LISTOUT)TPREC
!seaso3_comm ?
	 WRITE(LISTOUT)XDUO3N
	 WRITE(LISTOUT)XDO3N2
	 WRITE(LISTOUT)XDO3N3
	 WRITE(LISTOUT)XDO3N4
	 WRITE(LISTOUT)PRGFDL
	 WRITE(LISTOUT)O3O3
	 WRITE(LISTOUT)XRAD1
	 WRITE(LISTOUT)XRAD2
	 WRITE(LISTOUT)XRAD3
	 WRITE(LISTOUT)XRAD4
!phys_comm
	 WRITE(LISTOUT)RADIN
	 WRITE(LISTOUT)SIGT4
	 WRITE(LISTOUT)SST    !!!!!!!!!!!
!kflut_comm
	 WRITE(LISTOUT)TTAB
	 WRITE(LISTOUT)QSTAB
	 WRITE(LISTOUT)THE0K
	 WRITE(LISTOUT)ALU
!kffdbk_comm	 
	 WRITE(LISTOUT)DTDT
	 WRITE(LISTOUT)DQDT
	 WRITE(LISTOUT)DQCDT
	 WRITE(LISTOUT)W0AVG
	 WRITE(LISTOUT)PPTKF
	 WRITE(LISTOUT)TNCA
	 WRITE(LISTOUT)SUMFB
	 WRITE(LISTOUT)SPSRC
	 WRITE(LISTOUT)CIN
!contin_comm	 
	 WRITE(LISTOUT)PDSL
	 WRITE(LISTOUT)PSDT
	 WRITE(LISTOUT)ETADT
!co2dta_comm	 
	 WRITE(LISTOUT)CO251
	 WRITE(LISTOUT)CO258
	 WRITE(LISTOUT)CDT51
	 WRITE(LISTOUT)CDT58
	 WRITE(LISTOUT)C2D51
	 WRITE(LISTOUT)C2D58
	 WRITE(LISTOUT)CO2M51
	 WRITE(LISTOUT)CO2M58
	 WRITE(LISTOUT)CDTM51
	 WRITE(LISTOUT)CDTM58
	 WRITE(LISTOUT)C2DM51
	 WRITE(LISTOUT)C2DM58                !!!!!!!   CO2 (IT WILL BE NECESSARY FOR CLIMATE INTEGRATIONS)
	 WRITE(LISTOUT)STEMP
	 WRITE(LISTOUT)GTEMP
	 WRITE(LISTOUT)CO231
	 WRITE(LISTOUT)CO238
	 WRITE(LISTOUT)CDT31
	 WRITE(LISTOUT)CDT38
	 WRITE(LISTOUT)C2D31
	 WRITE(LISTOUT)C2D38
	 WRITE(LISTOUT)CO271
	 WRITE(LISTOUT)CO278
	 WRITE(LISTOUT)CDT71
	 WRITE(LISTOUT)CDT78
	 WRITE(LISTOUT)C2D71
	 WRITE(LISTOUT)C2D78
	 WRITE(LISTOUT)CO211
	 WRITE(LISTOUT)CO218
!acmcld_comm
	 WRITE(LISTOUT)NCFRCV
	 WRITE(LISTOUT)NCFRST
!!	 WRITE(LISTOUT)TCLOD        !INCLUDE IN CUT-CELL VERSION?
!!	 WRITE(LISTOUT)NCLOD        !

!vrbls_comm
	 WRITE(LISTOUT)RES    !?
	 WRITE(LISTOUT)PDOLD   !?
!	 WRITE(LISTOUT)PBH    !?
	 WRITE(LISTOUT)PB     !?
	 WRITE(LISTOUT)ACDTDT  !?
	 WRITE(LISTOUT)RDDTDT
!pvrbls_comm
 	 WRITE(LISTOUT)QS
	 WRITE(LISTOUT)ACCLIQ
	 WRITE(LISTOUT)CLDEFI   !BMJ, change init.f90 for restart
!pptasm_comm	 
	 WRITE(LISTOUT)TLATCU   !BMJ
	 WRITE(LISTOUT)TLATGS   !BMJ
!cldwtr_comm
	 WRITE(LISTOUT)LC
!
!GSM	 CLOSE(LISTOUT)

!
!
!GSM         OUTFILE8 = "GEF_from_modules_new"//C_MYPE//"."//C_NHRS
!GSM         OPEN(UNIT=LISTOUT, FILE= TRIM(OUTFILE8), FORM = 'UNFORMATTED')

!acmrdl_comm
	 WRITE(LISTOUT)TRDLW
	 WRITE(LISTOUT)ARDLW
	 WRITE(LISTOUT)NRDLW
!acmrds.comm
	 WRITE(LISTOUT)TRDSW
	 WRITE(LISTOUT)ARDSW
	 WRITE(LISTOUT)NRDSW
!acmsfc.comm
	 WRITE(LISTOUT)TSRFC
	 WRITE(LISTOUT)ASRFC
	 WRITE(LISTOUT)APHTIM
	 WRITE(LISTOUT)NSRFC
!loops.comm
	 WRITE(LISTOUT)LMH
	 WRITE(LISTOUT)LMV
!phys_comm
	 WRITE(LISTOUT)DFRLG
	 WRITE(LISTOUT)DTQ2
	 WRITE(LISTOUT)TDTQ2
	 WRITE(LISTOUT)DTD
	 WRITE(LISTOUT)TDTD
	 WRITE(LISTOUT)ROS	 
	 WRITE(LISTOUT)DS
	 WRITE(LISTOUT)ROI
	 WRITE(LISTOUT)CI	 
	 WRITE(LISTOUT)DI
	 WRITE(LISTOUT)PL
	 WRITE(LISTOUT)THL	 
	 WRITE(LISTOUT)RDQ
	 WRITE(LISTOUT)RDTH
	 WRITE(LISTOUT)RDP	 
	 WRITE(LISTOUT)RDTHE
	 WRITE(LISTOUT)PLQ
	 WRITE(LISTOUT)RDPQ	 
	 WRITE(LISTOUT)RDTHEQ
	 WRITE(LISTOUT)PLQ
	 WRITE(LISTOUT)RDPQ	 
	 WRITE(LISTOUT)QS0	 
	 WRITE(LISTOUT)SQS
	 WRITE(LISTOUT)THE0
	 WRITE(LISTOUT)STHE	 
	 WRITE(LISTOUT)THE0Q
	 WRITE(LISTOUT)STHEQ
	 WRITE(LISTOUT)PTBL	 
	 WRITE(LISTOUT)TTBL
	 WRITE(LISTOUT)TTBLQ
	 WRITE(LISTOUT)SSTM	 
	 WRITE(LISTOUT)KTM
	 WRITE(LISTOUT)GFFC
	 WRITE(LISTOUT)HDAC	 
	 WRITE(LISTOUT)HDACV
	 WRITE(LISTOUT)SST1
!kflut_comm
         WRITE(LISTOUT)RDPR
         WRITE(LISTOUT)RDTHK
         WRITE(LISTOUT)PTOP
!kffdbk_comm	 
	 WRITE(LISTOUT)RAINCV
	 WRITE(LISTOUT)PPTCA
	 WRITE(LISTOUT)PSRC
	 WRITE(LISTOUT)PCLB
	 WRITE(LISTOUT)UMFB
	 WRITE(LISTOUT)SPCLB
	 WRITE(LISTOUT)TST
	 WRITE(LISTOUT)NCAD
	 WRITE(LISTOUT)NCA
	 WRITE(LISTOUT)NCLDCK	 
!co2dta_comm	 
	 WRITE(LISTOUT)B0
	 WRITE(LISTOUT)B1
	 WRITE(LISTOUT)B2
	 WRITE(LISTOUT)B3
!pvrbls_comm
 	 WRITE(LISTOUT)HGTSUB
	 WRITE(LISTOUT)ZEFFIJ
!tend_comm
 	 WRITE(LISTOUT)T_ADJ
	 WRITE(LISTOUT)T_OLD	 
!ctlblk_comm
 	 WRITE(LISTOUT)IDAT
 	 WRITE(LISTOUT)RUN	 
 	 WRITE(LISTOUT)FIRST	 
 	 WRITE(LISTOUT)RESTRT	 
 	 WRITE(LISTOUT)SIGMA	 
 	 WRITE(LISTOUT)SUBPOST	 
 	 WRITE(LISTOUT)LCORNERM
 	 WRITE(LISTOUT)IOUT
 	 WRITE(LISTOUT)IHRST	 
 	 WRITE(LISTOUT)NFCST	 
 	 WRITE(LISTOUT)NBC	 
 	 WRITE(LISTOUT)LIST	 
 	 WRITE(LISTOUT)NTSD	 
 	 WRITE(LISTOUT)NTSTM	 
 	 WRITE(LISTOUT)NDDAMP
 	 WRITE(LISTOUT)NPREC	 
 	 WRITE(LISTOUT)IDTAD	 
 	 WRITE(LISTOUT)NBOCO	 
 	 WRITE(LISTOUT)NSHDE	 
 	 WRITE(LISTOUT)NCP	 
 	 WRITE(LISTOUT)NPHS
 	 WRITE(LISTOUT)NCNVC
 	 WRITE(LISTOUT)NRADS	 
 	 WRITE(LISTOUT)NRADL	 
 	 WRITE(LISTOUT)NTDDMP	 
 	 WRITE(LISTOUT)NSTART	 
 	 WRITE(LISTOUT)OUTSTR	 
 	 WRITE(LISTOUT)NDASSIM
 	 WRITE(LISTOUT)NDASSIMM
 	 WRITE(LISTOUT)INITSS	 
 	 WRITE(LISTOUT)IDTAD2	 
 	 WRITE(LISTOUT)NTSD_INIT	 
 	 WRITE(LISTOUT)DT
!dynam_comm
 	 WRITE(LISTOUT)FADV
 	 WRITE(LISTOUT)FADV2	 
 	 WRITE(LISTOUT)FADT
 	 WRITE(LISTOUT)R	 
 	 WRITE(LISTOUT)PT
 	 WRITE(LISTOUT)F4D	 
 	 WRITE(LISTOUT)F4Q
 	 WRITE(LISTOUT)EF4T	 
 	 WRITE(LISTOUT)FKIN
 	 WRITE(LISTOUT)FCP	 
 	 WRITE(LISTOUT)DELTY
 	 WRITE(LISTOUT)DELTHZ	 
 	 WRITE(LISTOUT)KAPPA
 	 WRITE(LISTOUT)P0	 
 	 WRITE(LISTOUT)TSPH
 	 WRITE(LISTOUT)RTDF	 
 	 WRITE(LISTOUT)RTDFDT
 	 WRITE(LISTOUT)FADTQ	 
 	 WRITE(LISTOUT)F4D25
 	 WRITE(LISTOUT)DETA	 
 	 WRITE(LISTOUT)RDETA
 	 WRITE(LISTOUT)AETA	 
 	 WRITE(LISTOUT)DAETA
 	 WRITE(LISTOUT)F4Q2
 	 WRITE(LISTOUT)ETA
 	 WRITE(LISTOUT)DFL	 
 	 WRITE(LISTOUT)WPDAR
 	 WRITE(LISTOUT)F11	 
 	 WRITE(LISTOUT)F12
 	 WRITE(LISTOUT)F21	 
 	 WRITE(LISTOUT)F22
 	 WRITE(LISTOUT)P11	 
 	 WRITE(LISTOUT)P12
 	 WRITE(LISTOUT)P21	 
 	 WRITE(LISTOUT)P22
 	 WRITE(LISTOUT)FDIV
 	 WRITE(LISTOUT)FDDMP	 
 	 WRITE(LISTOUT)HSINP
 	 WRITE(LISTOUT)HCOSP	 
 	 WRITE(LISTOUT)FVDIFF
 	 WRITE(LISTOUT)HBMSK	 
 	 WRITE(LISTOUT)DX
 	 WRITE(LISTOUT)DDMP	 
!nhydro_comm
 	 WRITE(LISTOUT)DWDT
 	 WRITE(LISTOUT)PINT	 
 	 WRITE(LISTOUT)W
 	 WRITE(LISTOUT)Z
 	 WRITE(LISTOUT)PDWDT

!
	 CLOSE(LISTOUT)
!	 
!
!
!****************************************
       ENDIF
!****************************************


       CLOSE(333)
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


    ACPREC = 0.
    CUPREC = 0.
    APREC  = 0.
    ACSNOW = 0.
    ACSNOM = 0.
    PREC   = 0.
    BGROFF = 0.
    SSROFF = 0.
    POTEVP = 0.
    POTFLX = 0.
    SFCEVP = 0.
    SUBSHX = 0.
    SNOPCX = 0.
    Q2     = 0.
!
    SFCLHX = 0.
    SFCSHX = 0.
    SFCUVX = 0.  !apr 2019 RESTART BMJ
!
   
    END SUBROUTINE OUT2