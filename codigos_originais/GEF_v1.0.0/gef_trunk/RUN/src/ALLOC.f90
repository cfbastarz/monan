!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief ALLOCATE ALL FIELDS
!> @details ALLOCATE ALL FIELDS DEFINED AS ALLOCATBLE IN ALL MODULES.
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
!! @arg @c C_FRACN
!! @arg @c DYNAM
!! @arg @c F77KINDS
!! @arg @c HS
!! @arg @c KFFDBK
!! @arg @c LOOPS
!! @arg @c MASKS
!! @arg @c METRCS
!! @arg @c NHYDRO
!! @arg @c PARMETA
!! @arg @c PARMSOIL
!! @arg @c PHYS
!! @arg @c PPTASM
!! @arg @c PVRBLS
!! @arg @c VRBLS
!! @arg @c SOIL
!> @details <b>Driver:</b> 
!! @arg @c GEF
!> @details <b>Calls:</b> 
!<
!--------------------------------------------------------------------------------------------------
    SUBROUTINE ALLOC
!--------------------------------------------------------------------------------------------------
! SUBROUTINE ALLOC
! 
! SUBPROGRAM: ALLOC - ALLOCATE ALL FIELDS
! PROGRAMMER: ????? 
! ORG: W/NP22
! DATE: ??-??-??
!
! ABSTRACT:  
! ALLOCATE ALL FIELDS DEFINED AS ALLOCATBLE IN ALL MODULES
!
! PROGRAM HISTORY LOG:
! ??-??-??  ?????   - ORIGINATOR
! 18-03-20  LUCCI   - MODERNIZATION OF THE CODE, INCLUDING:
!                     * F77 TO F90/F95
!                     * INDENTATION & UNIFORMIZATION CODE
!                     * REPLACEMENT OF COMMONS BLOCK FOR MODULES
!                     * DOCUMENTATION WITH DOXYGEN
!                     * OPENMP FUNCTIONALITY 
!
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
! INPUT FILES:
! NONE
!
! OUTPUT FILES:
! NONE
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
!              C_FRACN
!              DYNAM
!              F77KINDS
!              HS
!              KFFDBK
!              LOOPS
!              MASKS
!              METRCS
!              NHYDRO
!              PARMETA
!              PARMSOIL
!              PHYS
!              PPTASM
!              PVRBLS
!              VRBLS
!              SOIL
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
    USE CONTIN 
    USE C_FRACN
    USE DYNAM
    USE F77KINDS
    USE HS 
    USE KFFDBK
    USE LOOPS 
    USE MASKS
    USE METRCS
    USE NHYDRO
    USE PARMETA
    USE PARMSOIL , ONLY : NSOIL
    USE PHYS 
    USE PPTASM
    USE PVRBLS
    USE VRBLS 
    USE SOIL 
!
    IMPLICIT NONE

!-----------------------------------------------------------------------
!
!-------------------
! FROM MODULE KFFDBK
!-------------------
    ALLOCATE (DTDT   (0:IM+1, 0:JM+1, LM))
    ALLOCATE (DQDT   (0:IM+1, 0:JM+1, LM))
    ALLOCATE (DQCDT  (0:IM+1, 0:JM+1, LM))
    ALLOCATE (W0AVG  (0:IM+1, 0:JM+1, LM))
!
    ALLOCATE (RAINCV (0:IM+1, 0:JM+1))
    ALLOCATE (PPTKF  (0:IM+1, 0:JM+1))
    ALLOCATE (PPTCA  (0:IM+1, 0:JM+1))
    ALLOCATE (TNCA   (0:IM+1, 0:JM+1))
    ALLOCATE (PSRC   (0:IM+1, 0:JM+1))
    ALLOCATE (PCLB   (0:IM+1, 0:JM+1))
    ALLOCATE (UMFB   (0:IM+1, 0:JM+1))
    ALLOCATE (SUMFB  (0:IM+1, 0:JM+1))
    ALLOCATE (SPSRC  (0:IM+1, 0:JM+1))
    ALLOCATE (SPCLB  (0:IM+1, 0:JM+1))
    ALLOCATE (CIN    (0:IM+1, 0:JM+1))
    ALLOCATE (NCA    (0:IM+1, 0:JM+1))
    ALLOCATE (NCAD   (0:IM+1, 0:JM+1))
!--------------------
! FROM MODULE C_FRACN
!--------------------
    ALLOCATE (F_ICE  (LM, 0:IM+1, 0:JM+1))
    ALLOCATE (F_RAIN (LM, 0:IM+1, 0:JM+1))
    ALLOCATE (F_RIMEF(LM, 0:IM+1, 0:JM+1))
!-------------------
! FROM MODULE PPTASM
!-------------------
    ALLOCATE (PHOUR  (0:IM+1, 0:JM+1))
    ALLOCATE (APREC  (0:IM+1, 0:JM+1))
!
    ALLOCATE (TLATCU (0:IM+1, 0:JM+1, LM))
    ALLOCATE (TLATGS (0:IM+1, 0:JM+1, LM))
!
    ALLOCATE (PPTDAT (0:IM+1, 0:JM+1,  3))
!------------------
! FROM MODULE VRBLS
!------------------
    ALLOCATE (PD     (0:IM+1, 0:JM+1))
    ALLOCATE (FIS    (0:IM+1, 0:JM+1))
    ALLOCATE (RES    (0:IM+1, 0:JM+1))
    ALLOCATE (PDOLD  (0:IM+1, 0:JM+1))
!
    ALLOCATE (T      (0:IM+1, 0:JM+1, LM))
    ALLOCATE (Q      (0:IM+1, 0:JM+1, LM))
    ALLOCATE (U      (0:IM+1, 0:JM+1, LM))
    ALLOCATE (V      (0:IM+1, 0:JM+1, LM))
    ALLOCATE (PBH    (0:IM+1, 0:JM+1, LM))
    ALLOCATE (PB     (0:IM+1, 0:JM+1, LM))
    ALLOCATE (ACDTDT (0:IM+1, 0:JM+1, LM))
    ALLOCATE (RDDTDT (0:IM+1, 0:JM+1, LM))
!-------------------
! FROM MODULE PVRBLS
!-------------------
     ALLOCATE (Q2     (0:IM+1, 0:JM+1, LM))
!
     ALLOCATE (UZ0    (0:IM+1, 0:JM+1))
     ALLOCATE (VZ0    (0:IM+1, 0:JM+1))
     ALLOCATE (USTAR  (0:IM+1, 0:JM+1))
     ALLOCATE (THS    (0:IM+1, 0:JM+1))
     ALLOCATE (QS     (0:IM+1, 0:JM+1))
     ALLOCATE (THZ0   (0:IM+1, 0:JM+1))
     ALLOCATE (QZ0    (0:IM+1, 0:JM+1))
     ALLOCATE (Z0     (0:IM+1, 0:JM+1))
     ALLOCATE (AKMS   (0:IM+1, 0:JM+1))
     ALLOCATE (AKHS   (0:IM+1, 0:JM+1))
     ALLOCATE (U10    (0:IM+1, 0:JM+1))
     ALLOCATE (V10    (0:IM+1, 0:JM+1))
     ALLOCATE (TSHLTR (0:IM+1, 0:JM+1))
     ALLOCATE (QSHLTR (0:IM+1, 0:JM+1))
     ALLOCATE (TH10   (0:IM+1, 0:JM+1))
     ALLOCATE (Q10    (0:IM+1, 0:JM+1))
     ALLOCATE (PREC   (0:IM+1, 0:JM+1))
     ALLOCATE (ACCLIQ (0:IM+1, 0:JM+1))
     ALLOCATE (QWBS   (0:IM+1, 0:JM+1))
     ALLOCATE (TWBS   (0:IM+1, 0:JM+1))
     ALLOCATE (SNO    (0:IM+1, 0:JM+1))
     ALLOCATE (ACPREC (0:IM+1, 0:JM+1))
     ALLOCATE (CUPREC (0:IM+1, 0:JM+1))
     ALLOCATE (SI     (0:IM+1, 0:JM+1))
     ALLOCATE (CLDEFI (0:IM+1, 0:JM+1))
     ALLOCATE (HGTSUB (0:IM+1, 0:JM+1))
     ALLOCATE (PLM    (0:IM+1, 0:JM+1))
!
     ALLOCATE (ZEFFIJ (0:IM+1, 0:JM+1,  4))
!-------------------
! FROM MODULE METRCS
!-------------------
    ALLOCATE (SQV     (0:IM+1, 0:JM+1))
    ALLOCATE (SQH     (0:IM+1, 0:JM+1))
    ALLOCATE (Q11     (0:IM+1, 0:JM+1))
    ALLOCATE (Q12     (0:IM+1, 0:JM+1))
    ALLOCATE (Q22     (0:IM+1, 0:JM+1))
    ALLOCATE (QBV11   (0:IM+1, 0:JM+1))
    ALLOCATE (QBV12   (0:IM+1, 0:JM+1))
    ALLOCATE (QBV22   (0:IM+1, 0:JM+1))
!    ALLOCATE (SQBV11  (0:IM+1, 0:JM+1))
!    ALLOCATE (SQBV12  (0:IM+1, 0:JM+1))
!    ALLOCATE (SQBV22  (0:IM+1, 0:JM+1))
!    ALLOCATE (SQBV21  (0:IM+1, 0:JM+1))
    ALLOCATE (RSQH    (0:IM+1, 0:JM+1))
    ALLOCATE (QH11    (0:IM+1, 0:JM+1))
    ALLOCATE (QH12    (0:IM+1, 0:JM+1))
    ALLOCATE (QH22    (0:IM+1, 0:JM+1))
    ALLOCATE (RSQV    (0:IM+1, 0:JM+1))
    ALLOCATE (SQV_NORM(0:IM+1, 0:JM+1))
!
    ALLOCATE (QD11    (0:IM+1, 0:JM+1,  4))
    ALLOCATE (QD12    (0:IM+1, 0:JM+1,  4))
    ALLOCATE (QD21    (0:IM+1, 0:JM+1,  4))
    ALLOCATE (QD22    (0:IM+1, 0:JM+1,  4))
!
    ALLOCATE (QVH     (0:IM+1, 0:JM+1,  4, 2, 2))
    ALLOCATE (QHV     (0:IM+1, 0:JM+1,  4, 2, 2))
    ALLOCATE (QVH2    (0:IM+1, 0:JM+1,  4, 2, 2))
    ALLOCATE (QHV2    (0:IM+1, 0:JM+1,  4, 2, 2))
!
    ALLOCATE (QA      (0:IM+1, 0:JM+1, 13, 2, 2))
    ALLOCATE (QB      (0:IM+1, 0:JM+1,  2, 2))
!----------------- 
! FROM MODULE SOIL
!-----------------
    ALLOCATE (SOILTB (0:IM+1, 0:JM+1))
    ALLOCATE (SFCEXC (0:IM+1, 0:JM+1))
    ALLOCATE (SMSTAV (0:IM+1, 0:JM+1))
    ALLOCATE (SMSTOT (0:IM+1, 0:JM+1))
    ALLOCATE (GRNFLX (0:IM+1, 0:JM+1))
    ALLOCATE (PCTSNO (0:IM+1, 0:JM+1))
    ALLOCATE (VEGFRC (0:IM+1, 0:JM+1))
    ALLOCATE (CMC    (0:IM+1, 0:JM+1))
!
    ALLOCATE (IVGTYP (0:IM+1, 0:JM+1))
    ALLOCATE (ISLTYP (0:IM+1, 0:JM+1))
    ALLOCATE (ISLOPE (0:IM+1, 0:JM+1))
!
    ALLOCATE (SMC    (0:IM+1, 0:JM+1, NSOIL))
    ALLOCATE (STC    (0:IM+1, 0:JM+1, NSOIL))
    ALLOCATE (SH2O   (0:IM+1, 0:JM+1, NSOIL))
!
    ALLOCATE (SLDPTH (NSOIL))
    ALLOCATE (RTDPTH (NSOIL))
!------------------
! FROM MODULE  PHYS
!------------------
    ALLOCATE (DFRLG  (LM+1))
!
    ALLOCATE (SSTM   (0:IM+1, 0:JM+1, 356))
!
    ALLOCATE (RADOT  (0:IM+1, 0:JM+1))
    ALLOCATE (RADIN  (0:IM+1, 0:JM+1))
    ALLOCATE (CZMEAN (0:IM+1, 0:JM+1))
    ALLOCATE (CZEN   (0:IM+1, 0:JM+1))
    ALLOCATE (SIGT4  (0:IM+1, 0:JM+1))
    ALLOCATE (TG     (0:IM+1, 0:JM+1))
    ALLOCATE (EPSR   (0:IM+1, 0:JM+1))
    ALLOCATE (ALBEDO (0:IM+1, 0:JM+1))
    ALLOCATE (MXSNAL (0:IM+1, 0:JM+1))
    ALLOCATE (HTOP   (0:IM+1, 0:JM+1))
    ALLOCATE (HBOT   (0:IM+1, 0:JM+1))
    ALLOCATE (CNVTOP (0:IM+1, 0:JM+1))
    ALLOCATE (CNVBOT (0:IM+1, 0:JM+1))
    ALLOCATE (GFFC   (0:IM+1, 0:JM+1))
    ALLOCATE (ALBASE (0:IM+1, 0:JM+1))
    ALLOCATE (HDAC   (0:IM+1, 0:JM+1))
    ALLOCATE (SST    (0:IM+1, 0:JM+1))
    ALLOCATE (HDACV  (0:IM+1, 0:JM+1))
    ALLOCATE (GLAT   (0:IM+1, 0:JM+1))
    ALLOCATE (GLON   (0:IM+1, 0:JM+1))
    ALLOCATE (SST1   (0:IM+1, 0:JM+1))
!------------------
! FROM MODULE MASKS
!------------------
    ALLOCATE (SM     (0:IM+1, 0:JM+1))
    ALLOCATE (SICE   (0:IM+1, 0:JM+1))
!
    ALLOCATE (HTM    (0:IM+1, 0:JM+1, LM))
    ALLOCATE (VTM    (0:IM+1, 0:JM+1, LM))
!-------------------
! FROM MODULE CONTIN 
!-------------------
    ALLOCATE (PDSL   (0:IM+1, 0:JM+1))
    ALLOCATE (PSDT   (0:IM+1, 0:JM+1))
!
    ALLOCATE (RTOP   (0:IM+1, 0:JM+1, LM))
    ALLOCATE (OMGALF (0:IM+1, 0:JM+1, LM))
    ALLOCATE (DIV    (0:IM+1, 0:JM+1, LM))
!
    ALLOCATE (ETADT  (0:IM+1, 0:JM+1, 0:LM))
!------------------
! FROM MODULE DYNAM
!------------------
    ALLOCATE (DETA   (LM))
    ALLOCATE (RDETA  (LM))
    ALLOCATE (AETA   (LM))
    ALLOCATE (DAETA  (LM))
    ALLOCATE (F4Q2   (LM))
!
    ALLOCATE (ETA    (LM+1))
    ALLOCATE (DFL    (LM+1))
!
    ALLOCATE (WPDAR  (0:IM+1, 0:JM+1))
    ALLOCATE (F11    (0:IM+1, 0:JM+1))
    ALLOCATE (F12    (0:IM+1, 0:JM+1))
    ALLOCATE (F21    (0:IM+1, 0:JM+1))
    ALLOCATE (F22    (0:IM+1, 0:JM+1))
    ALLOCATE (P11    (0:IM+1, 0:JM+1))
    ALLOCATE (P12    (0:IM+1, 0:JM+1))
    ALLOCATE (P21    (0:IM+1, 0:JM+1))
    ALLOCATE (P22    (0:IM+1, 0:JM+1))
    ALLOCATE (FDIV   (0:IM+1, 0:JM+1))
    ALLOCATE (FDDMP  (0:IM+1, 0:JM+1))
    ALLOCATE (HSINP  (0:IM+1, 0:JM+1))
    ALLOCATE (HCOSP  (0:IM+1, 0:JM+1))
    ALLOCATE (FVDIFF (0:IM+1, 0:JM+1))
    ALLOCATE (HBMSK  (0:IM+1, 0:JM+1))
    ALLOCATE (DX     (0:IM+1, 0:JM+1))
    ALLOCATE (DDMP   (0:IM+1, 0:JM+1))
!------------------- 
! FROM MODULE ACMSFC
!------------------- 
    ALLOCATE (SFCSHX (0:IM+1, 0:JM+1))
    ALLOCATE (SFCLHX (0:IM+1, 0:JM+1))
    ALLOCATE (SUBSHX (0:IM+1, 0:JM+1))
    ALLOCATE (SNOPCX (0:IM+1, 0:JM+1))
    ALLOCATE (SFCUVX (0:IM+1, 0:JM+1))
    ALLOCATE (SFCEVP (0:IM+1, 0:JM+1))
    ALLOCATE (POTEVP (0:IM+1, 0:JM+1))
    ALLOCATE (POTFLX (0:IM+1, 0:JM+1))
!-------------------
! FROM MODULE ACMRDS
!-------------------
    ALLOCATE (RSWIN  (0:IM+1, 0:JM+1))
    ALLOCATE (RSWOUT (0:IM+1, 0:JM+1))
    ALLOCATE (RSWTOA (0:IM+1, 0:JM+1))
    ALLOCATE (ASWIN  (0:IM+1, 0:JM+1))
    ALLOCATE (ASWOUT (0:IM+1, 0:JM+1))
    ALLOCATE (ASWTOA (0:IM+1, 0:JM+1))
    ALLOCATE (RSWNET (0:IM+1, 0:JM+1))
!
    ALLOCATE (RSWTT  (0:IM+1, 0:JM+1, LM))
!-------------------
! FROM MODULE ACMRDL
!-------------------
    ALLOCATE (RLWIN  (0:IM+1, 0:JM+1))
    ALLOCATE (RLWOUT (0:IM+1, 0:JM+1))
    ALLOCATE (RLWTOA (0:IM+1, 0:JM+1))
    ALLOCATE (ALWIN  (0:IM+1, 0:JM+1))
    ALLOCATE (ALWOUT (0:IM+1, 0:JM+1))
    ALLOCATE (ALWTOA (0:IM+1, 0:JM+1))
    ALLOCATE (RLWNET (0:IM+1, 0:JM+1))
!
    ALLOCATE (RLWTT  (0:IM+1, 0:JM+1, LM))
!---------------
! FROM MODULE HS
!---------------
    ALLOCATE (KTDT   (0:IM+1, 0:JM+1, LM))
!
    ALLOCATE (UTOP0  (0:IM+1, 0:JM+1))
    ALLOCATE (VTOP0  (0:IM+1, 0:JM+1))
!
    ALLOCATE (KVDT   (LM))
!
    ALLOCATE (TTOP0  (0:IM+1, 0:JM+1, 2))
!------------
! FROM CLDWTR
!------------ 
    ALLOCATE (CWM    (0:IM+1, 0:JM+1, LM))
!
    ALLOCATE (U00    (0:IM+1, 0:JM+1))
    ALLOCATE (LC     (0:IM+1, 0:JM+1))
    ALLOCATE (SR     (0:IM+1, 0:JM+1))
!
    ALLOCATE (UL     (2*LM))
!------------
! FROM CNVCLD
!------------
    ALLOCATE (CUPPT  (0:IM+1, 0:JM+1))
    ALLOCATE (CFRACL (0:IM+1, 0:JM+1))
    ALLOCATE (CFRACM (0:IM+1, 0:JM+1))
    ALLOCATE (CFRACH (0:IM+1, 0:JM+1))
!------------
! FROM ACMCLD
!------------
    ALLOCATE (ACFRCV (0:IM+1, 0:JM+1))
    ALLOCATE (ACFRST (0:IM+1, 0:JM+1))
    ALLOCATE (NCFRCV (0:IM+1, 0:JM+1))
    ALLOCATE (NCFRST (0:IM+1, 0:JM+1))
!------------
! FROM ACMCLH
!------------
    ALLOCATE (TRAIN  (0:IM+1, 0:JM+1, LM))
    ALLOCATE (TCUCN  (0:IM+1, 0:JM+1, LM))
!------------
! FROM ACMPRE
!------------
    ALLOCATE (ACSNOW (0:IM+1, 0:JM+1))
    ALLOCATE (ACSNOM (0:IM+1, 0:JM+1))
    ALLOCATE (SSROFF (0:IM+1, 0:JM+1))
    ALLOCATE (BGROFF (0:IM+1, 0:JM+1))
!-----------
! FROM LOOPS
!-----------
    ALLOCATE (LMH    (0:IM+1, 0:JM+1))
    ALLOCATE (LMV    (0:IM+1, 0:JM+1))
!---------------------------
! FROM NHYDRO (DRAGAN, 2015)
!---------------------------
    ALLOCATE (DWDT   (0:IM+1, 0:JM+1, LM))
    ALLOCATE (PDWDT  (0:IM+1, 0:JM+1, LM))
!
    ALLOCATE (PINT   (0:IM+1, 0:JM+1, LM+1))
    ALLOCATE (W      (0:IM+1, 0:JM+1, LM+1))
    ALLOCATE (Z      (0:IM+1, 0:JM+1, LM+1))
!
    END SUBROUTINE ALLOC
