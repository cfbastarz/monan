    SUBROUTINE ZERO
!--------------------------------------------------------------------------------------------------
! SUBROUTINE ZERO
! 
! SUBPROGRAM: ZERO - ZEROS ALL THE ALLOCATED VARIABLES
! PROGRAMMER: LUTHI 
! ORG: W/NP22
! DATE: ??-??-??
!
! ABSTRACT:  
! ZEROS ALL THE ALLOCATED VARIABLES
!
! PROGRAM HISTORY LOG:
! ??-??-??  LUCCI   - ORIGINATOR
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
!-------------------
! FROM MODULE KFFDBK
!-------------------
        DTDT(:,:,:)     = 0.0
        DQDT(:,:,:)     = 0.0
       DQCDT(:,:,:)     = 0.0
       W0AVG(:,:,:)     = 0.0
!
      RAINCV(:,:)       = 0.0
       PPTKF(:,:)       = 0.0
       PPTCA(:,:)       = 0.0
        TNCA(:,:)       = 0.0
        PSRC(:,:)       = 0.0
        PCLB(:,:)       = 0.0
        UMFB(:,:)       = 0.0
       SUMFB(:,:)       = 0.0
       SPSRC(:,:)       = 0.0
       SPCLB(:,:)       = 0.0
         CIN(:,:)       = 0.0
         NCA(:,:)       = 0.0
        NCAD(:,:)       = 0.0
!--------------------
! FROM MODULE C_FRACN
!--------------------
       F_ICE(:,:,:)     = 0.0
      F_RAIN(:,:,:)     = 0.0
     F_RIMEF(:,:,:)     = 0.0
!-------------------
! FROM MODULE PPTASM
!-------------------
       PHOUR(:,:)       = 0.0
       APREC(:,:)       = 0.0
!
      TLATCU(:,:,:)     = 0.0
      TLATGS(:,:,:)     = 0.0
!
      PPTDAT(:,:,:)     = 0.0
!------------------
! FROM MODULE VRBLS
!------------------
          PD(:,:)       = 0.0
         FIS(:,:)       = 0.0
         RES(:,:)       = 0.0
       PDOLD(:,:)       = 0.0
!
           T(:,:,:)     = 0.0
           Q(:,:,:)     = 0.0
           U(:,:,:)     = 0.0
           V(:,:,:)     = 0.0
         PBH(:,:,:)     = 0.0
          PB(:,:,:)     = 0.0
      ACDTDT(:,:,:)     = 0.0
      RDDTDT(:,:,:)     = 0.0
!-------------------
! FROM MODULE PVRBLS
!-------------------
          Q2(:,:,:)     = 0.0
!
         UZ0(:,:)       = 0.0
         VZ0(:,:)       = 0.0
       USTAR(:,:)       = 0.0
         THS(:,:)       = 0.0
          QS(:,:)       = 0.0
        THZ0(:,:)       = 0.0
         QZ0(:,:)       = 0.0
          Z0(:,:)       = 0.0
        AKMS(:,:)       = 0.0
        AKHS(:,:)       = 0.0
         U10(:,:)       = 0.0
         V10(:,:)       = 0.0
      TSHLTR(:,:)       = 0.0
      QSHLTR(:,:)       = 0.0
        TH10(:,:)       = 0.0
         Q10(:,:)       = 0.0
        PREC(:,:)       = 0.0
      ACCLIQ(:,:)       = 0.0
        QWBS(:,:)       = 0.0
        TWBS(:,:)       = 0.0
         SNO(:,:)       = 0.0
      ACPREC(:,:)       = 0.0
      CUPREC(:,:)       = 0.0
          SI(:,:)       = 0.0
      CLDEFI(:,:)       = 0.0
      HGTSUB(:,:)       = 0.0
         PLM(:,:)       = 0.0
!
      ZEFFIJ(:,:,:)     = 0.0
!-------------------
! FROM MODULE METRCS
!-------------------
         SQV(:,:)       = 0.0
         SQH(:,:)       = 0.0
         Q11(:,:)       = 0.0
         Q12(:,:)       = 0.0
         Q22(:,:)       = 0.0
       QBV11(:,:)       = 0.0
       QBV12(:,:)       = 0.0
       QBV22(:,:)       = 0.0
!      SQBV11(:,:)       = 0.0
!      SQBV12(:,:)       = 0.0
!      SQBV22(:,:)       = 0.0
!      SQBV21(:,:)       = 0.0
        RSQH(:,:)       = 0.0
        QH11(:,:)       = 0.0
        QH12(:,:)       = 0.0
        QH22(:,:)       = 0.0
        RSQV(:,:)       = 0.0
    SQV_NORM(:,:)       = 0.0
!
        QD11(:,:,:)     = 0.0
        QD12(:,:,:)     = 0.0
        QD21(:,:,:)     = 0.0
        QD22(:,:,:)     = 0.0
!
         QVH(:,:,:,:,:) = 0.0
         QHV(:,:,:,:,:) = 0.0
        QVH2(:,:,:,:,:) = 0.0
        QHV2(:,:,:,:,:) = 0.0
!
          QA(:,:,:,:,:) = 0.0
          QB(:,:,:,:)   = 0.0
!----------------- 
! FROM MODULE SOIL
!-----------------
     SOILTB(:,:)        = 0.0
     SFCEXC(:,:)        = 0.0
     SMSTAV(:,:)        = 0.0
     SMSTOT(:,:)        = 0.0
     GRNFLX(:,:)        = 0.0
     PCTSNO(:,:)        = 0.0
     VEGFRC(:,:)        = 0.0
        CMC(:,:)        = 0.0
!
     IVGTYP(:,:)        = 0.0
     ISLTYP(:,:)        = 0.0
     ISLOPE(:,:)        = 0.0
!
        SMC(:,:,:)      = 0.0
        STC(:,:,:)      = 0.0
       SH2O(:,:,:)      = 0.0
!
     SLDPTH(:)          = 0.0
     RTDPTH(:)          = 0.0
!------------------
! FROM MODULE  PHYS
!------------------
      DFRLG(:)          = 0.0
!
       SSTM(:,:,:)      = 0.0
!
      RADOT(:,:)        = 0.0
      RADIN(:,:)        = 0.0
     CZMEAN(:,:)        = 0.0
       CZEN(:,:)        = 0.0
      SIGT4(:,:)        = 0.0
         TG(:,:)        = 0.0
       EPSR(:,:)        = 0.0
     ALBEDO(:,:)        = 0.0
     MXSNAL(:,:)        = 0.0
       HTOP(:,:)        = 0.0
       HBOT(:,:)        = 0.0
     CNVTOP(:,:)        = 0.0
     CNVBOT(:,:)        = 0.0
       GFFC(:,:)        = 0.0
     ALBASE(:,:)        = 0.0
       HDAC(:,:)        = 0.0
        SST(:,:)        = 0.0
      HDACV(:,:)        = 0.0
       GLAT(:,:)        = 0.0
       GLON(:,:)        = 0.0
       SST1(:,:)        = 0.0
!------------------
! FROM MODULE MASKS
!------------------
         SM(:,:)        = 0.0
       SICE(:,:)        = 0.0
!
        HTM(:,:,:)      = 0.0
        VTM(:,:,:)      = 0.0
!-------------------
! FROM MODULE CONTIN 
!-------------------
       PDSL(:,:)        = 0.0
       PSDT(:,:)        = 0.0
!
       RTOP(:,:,:)      = 0.0
     OMGALF(:,:,:)      = 0.0
        DIV(:,:,:)      = 0.0
!
      ETADT(:,:,:)      = 0.0
!------------------
! FROM MODULE DYNAM
!------------------
      DETA(:)           = 0.0 
     RDETA(:)           = 0.0
      AETA(:)           = 0.0
     DAETA(:)           = 0.0
      F4Q2(:)           = 0.0
!
       ETA(:)           = 0.0
       DFL(:)           = 0.0
!
     WPDAR(:,:)         = 0.0
       F11(:,:)         = 0.0
       F12(:,:)         = 0.0
       F21(:,:)         = 0.0
       F22(:,:)         = 0.0
       P11(:,:)         = 0.0
       P12(:,:)         = 0.0
       P21(:,:)         = 0.0
       P22(:,:)         = 0.0
      FDIV(:,:)         = 0.0
     FDDMP(:,:)         = 0.0
     HSINP(:,:)         = 0.0
     HCOSP(:,:)         = 0.0
    FVDIFF(:,:)         = 0.0
     HBMSK(:,:)         = 0.0
        DX(:,:)         = 0.0
      DDMP(:,:)         = 0.0
!------------------- 
! FROM MODULE ACMSFC
!------------------- 
     SFCSHX(:,:)        = 0.0
     SFCLHX(:,:)        = 0.0
     SUBSHX(:,:)        = 0.0
     SNOPCX(:,:)        = 0.0
     SFCUVX(:,:)        = 0.0
     SFCEVP(:,:)        = 0.0
     POTEVP(:,:)        = 0.0
     POTFLX(:,:)        = 0.0
!-------------------
! FROM MODULE ACMRDS
!-------------------
      RSWIN(:,:)        = 0.0
     RSWOUT(:,:)        = 0.0
     RSWTOA(:,:)        = 0.0
      ASWIN(:,:)        = 0.0
     ASWOUT(:,:)        = 0.0
     ASWTOA(:,:)        = 0.0
     RSWNET(:,:)        = 0.0
!
      RSWTT(:,:,:)      = 0.0
!-------------------
! FROM MODULE ACMRDL
!-------------------
      RLWIN(:,:)        = 0.0
     RLWOUT(:,:)        = 0.0
     RLWTOA(:,:)        = 0.0
      ALWIN(:,:)        = 0.0
     ALWOUT(:,:)        = 0.0
     ALWTOA(:,:)        = 0.0
     RLWNET(:,:)        = 0.0
!
      RLWTT(:,:,:)      = 0.0
!---------------
! FROM MODULE HS
!---------------
       KTDT(:,:,:)      = 0.0
!
      UTOP0(:,:)        = 0.0
      VTOP0(:,:)        = 0.0
!
       KVDT(:)          = 0.0
!
      TTOP0(:,:,:)      = 0.0
!------------
! FROM CLDWTR
!------------ 
        CWM(:,:,:)      = 0.0
!
        U00(:,:)        = 0.0
         LC(:,:)        = 0.0
         SR(:,:)        = 0.0
!
         UL(:)          = 0.0
!------------
! FROM CNVCLD
!------------
      CUPPT(:,:)        = 0.0
     CFRACL(:,:)        = 0.0
     CFRACM(:,:)        = 0.0
     CFRACH(:,:)        = 0.0
!------------
! FROM ACMCLD
!------------
     ACFRCV(:,:)        = 0.0
     ACFRST(:,:)        = 0.0
     NCFRCV(:,:)        = 0.0
     NCFRST(:,:)        = 0.0
!------------
! FROM ACMCLH
!------------
      TRAIN(:,:,:)      = 0.0
      TCUCN(:,:,:)      = 0.0
!------------
! FROM ACMPRE
!------------
     ACSNOW(:,:)        = 0.0
     ACSNOM(:,:)        = 0.0
     SSROFF(:,:)        = 0.0
     BGROFF(:,:)        = 0.0
!-----------
! FROM LOOPS
!-----------
        LMH(:,:)        = 0.0
        LMV(:,:)        = 0.0
!---------------------------
! FROM NHYDRO (DRAGAN, 2015)
!---------------------------
       DWDT(:,:,:)      = 0.0
      PDWDT(:,:,:)      = 0.0
!
       PINT(:,:,:)      = 0.0
          W(:,:,:)      = 0.0
          Z(:,:,:)      = 0.0
!
    END SUBROUTINE ZERO













