!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Inserir Brief de SSTSTP
!> @details Inserir Details de SSTSTP
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
!> @param[in]   SSTUPSTEP  - Significado de SSTUPSTEP
!> @details <b>Use Module:</b>
!! @arg @c CTLBLK
!! @arg @c F77KINDS
!! @arg @c LOOPS
!! @arg @c MASKS 
!! @arg @c MPPSTAFF
!! @arg @c PARMETA   
!! @arg @c PARM_TBL   
!! @arg @c PHYS
!> @details <b>Driver:</b> 
!! @arg @c GEF
!> @details <b>Calls:</b>
!! @arg @c GETDATE
!--------------------------------------------------------------------------------------------------    
    SUBROUTINE SSTSTP(SSTUPSTEP)
!--------------------------------------------------------------------------------------------------
! SUBROUTINE SSTSTP
!
! SUBPROGRAM: SSTSTP - ?????
! PROGRAMMER: ?????
! ORG: ?????
! DATE: ??-??-??
!
! ABSTRACT:
! ?????
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
! USE MODULES: CTLBLK
!              F77KINDS
!              LOOPS
!              MASKS
!              MPPSTAFF
!              PARMETA
!              PARM_TBL
!              PHYS
!
! DRIVER     : GEF
!
! CALLS      : GETDATE
!-------------------------------------------------------------------------------------------------- 
    USE CTLBLK
    USE F77KINDS
    USE LOOPS
    USE MASKS 
    USE MPPSTAFF
    USE PARMETA   
    USE PARM_TBL   
    USE PHYS  
!
    IMPLICIT NONE 
!    
    INTEGER(KIND=I4)    , PARAMETER :: LA     = 13  
    INTEGER(KIND=I4)    , PARAMETER :: IMJM   = IM * JM -  JM / 2
    INTEGER(KIND=I4)    , PARAMETER :: JAM    =  6 +  2 * (JM - 10)
    INTEGER(KIND=I4)    , PARAMETER :: LTOP   =  1
    INTEGER(KIND=I4)    , PARAMETER :: LBOT   = LM 
    INTEGER(KIND=I4)    , PARAMETER :: LSCRCH =  4 * LM + 1  + LA + 1
!
    INTEGER(KIND=I4)                                                      , INTENT(IN)          ::&
    & SSTUPSTEP
!   
    INTEGER(KIND=I4)                                                                            ::&
    & DAY     , MON     , YR      , UTC     , MA      , MB      , IMES    , IMES2   , NMESSST
!   
    REAL   (KIND=R4)                                                                            ::&
    & YDAY    , ADD     , FA      , FB
!   
    INTEGER(KIND=I4)    , DIMENSION(12)                                                         ::&
    & MONL
!
    CHARACTER(LEN=10)                                                                           ::&
    & CHARDATE
!     
    CHARACTER(LEN=256)                                                                          ::&
    & M_BEFORE, M_AFTER
!
    CHARACTER(LEN=4)                                                                            ::&
    & CN
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1)                                             ::&
    & MONBEFORE         , MONAFTER
!
    INTEGER(KIND=I4)                                                                            ::&
    & X       , NTSDDAY , CALC_MON
!------------------------
! IMPLICIT NONE VARIABLES
!------------------------
    INTEGER(KIND=I4)                                                                            ::&
    & IYR     , IMO     , IDY     , IUTC    , INITSST
!         
    IYR = IDAT(3)  ! 
    IMO = IDAT(1)  ! INITIAL DATE, DAY, MONTH, YEAR
    IDY = IDAT(2)  !
!
    PRINT*, "IDY=", IDY, "IMO=", IMO
! 
    IUTC = NTSD * DT / 3600     
!
    CALL GETDATE(IYR, IMO, IDY, IUTC, YR, MON, DAY, UTC)
!
!    DATA MONL /30,30,30,30,30,30,30,30,30,30,30,30/!!!!!!!!!!!!!!!!
!
    DATA MONL /31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/   
!      
    IF (MOD(YR,4) == 0) THEN
        MONL(2) = 29 
    END IF   
!
    YDAY = DAY    
    MA   = MON - 1
!   
    IF (YDAY >= (1.0 + FLOAT(MONL(MON)) / 2.0)) THEN 
!------------------------------------------------------------
! CHANGING THE MONTH OF SST FROM THE ARCHIVE,  
! EACH 16TH OF THE MONTH, FOR THE MONTHS WITH 30 DAYS,
! EACH 17TH OF THE MONTH, FOR THE MONTHS WITH 31 DAYS,
! 15TH OF THE MONTH EACH FEBRUARY, 
! BESIDE WHEN IT HAS 29 DAYS, THEN IT CHANGES AT THE DAY 16TH 
!------------------------------------------------------------         
        MA = MON
    END IF
!
    MB = MA + 1 
!
    IF (MA < 1) THEN
        MA = 12
    END IF   
!       
    IF (MB > 12) THEN
        MB =  1
    END IF
!
    PRINT*, "MA", MA, "DAY", YDAY, "YR", YR
!
    PRINT*, 'NTSD=', NTSD
!----------------------------------------------
! NEW PART OF THE CODE - DRAGAN, SEPTEMBER 2011  
!----------------------------------------------
!
!--------------------------------------------------------------------------------------
! THE ARCHIVE CONSISTS OF THE MONTHLY SST DATA FOR THE PERIOD DECEMBER 1981 - JULY 2011
!--------------------------------------------------------------------------------------
    INITSST = (IYR - 1982) * 12 + 1 + IMO        ! THIS LINE CALCULATES THE INITIAL SST
!-------------------------------------------------------------------------------------------
! IF THE DAY IS IN THE FIRST HALF OF EACH MONTH, IT WILL BE RECALCULATED BELOW, IN CALC_MON,
! IT WILL USE MA=MON-1, AND CALC_MON WILL START FROM -1
!-------------------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------------
! DEFINES THE PROBLEM WHEN THE YEAR IS CHANGING, FOR THE FIRST 15 DAYS OF EACH JANUARY
!-------------------------------------------------------------------------------------
    IF (MA == 12 .AND. YDAY < (FLOAT(MONL(MON)) / 2.0 + 1.0)) THEN 
!--------------------------------------------------------------------------------------------------
! CALCULATE THE CURRENT MONTH OF SST TAKEN FROM ARCHIVE, FOR THE PERIOD 1-15TH OF JANUARY EACH YEAR
!--------------------------------------------------------------------------------------------------
        CALC_MON = (YR - IYR) * 12 + (MA - IMO) - 12                        
    ELSE
!-------------------------------------------------------------------------------
! CALCULATES THE CURRENT MONTH OF SST TAKEN FROM ARCHIVE FOR ALL THE OTHER DATES
 !-------------------------------------------------------------------------------
        CALC_MON = (YR - IYR) * 12 + (MA - IMO)                           
    END IF
!---------------------------------------------------------------------------------------------
! "PREVIOUS" MONTH FROM THE ARCHIVE, FOR MAKING INTERPOLATION OF MONTHLY SST FOR SPECIFIC DATE
!---------------------------------------------------------------------------------------------
    IMES = INITSST + CALC_MON 
!-------------------------------------------------------------------------------------------
! "FUTURE" MONTH FROM THE ARCHIVE, FOR MAKING INTERPOLATION OF MONTHLY SST FOR SPECIFIC DATE
!-------------------------------------------------------------------------------------------                             
    IMES2 = IMES + 1                               
!     
    IF (IMES < 1) THEN
        IMES = 12
    END IF   
!---------------------------------------------------------------------------------------------
! IN THE CASE THAT THE INITIAL CONDITIONS ARE FROM THE PERIOD 1-15TH OF JANUARY, AND IF YR=IYR
!---------------------------------------------------------------------------------------------
    IF (IMES2 > 12) THEN
        MB = 1
    END IF
!
    ADD = FLOAT(MONL(MA)) / 2.0 - 1.0
!       
    IF (MA == MON) THEN
        ADD = -ADD - 2.
    END IF 
!
    FB = 2.0 * (YDAY + ADD) / FLOAT(MONL(MA) + MONL(MB))   
    FA = 1.0 - FB
!
    PRINT*, "IMES=", IMES, "IMES2=", IMES2
!
    WRITE(CN,'(I4.4)') IMES
!
    M_BEFORE='/scratchout/grupos/grpeta/projetos/tempo/oper/gef_v1.0.0/initdata/monthly_sst' // CN // '.' // C_MYPE
!
    OPEN(11, FILE=M_BEFORE, FORM='UNFORMATTED', STATUS='UNKNOWN', ACCESS='SEQUENTIAL')
!
    READ(11) MONBEFORE
!
    CLOSE (11)
!      
    WRITE(CN,'(I4.4)') IMES2
!  
    M_AFTER = '/scratchout/grupos/grpeta/projetos/tempo/oper/gef_v1.0.0/initdata/monthly_sst' // CN // '.' // C_MYPE
!
    OPEN(22, FILE=M_AFTER, FORM='UNFORMATTED', STATUS='UNKNOWN', ACCESS='SEQUENTIAL')
!
    READ(22) MONAFTER
!
    CLOSE (22)
!
    SST = FA * MONBEFORE(0:IM+1, 0:IM+1) + FB * MONAFTER(0:IM+1, 0:JM+1)
!
    RETURN
!   
    END SUBROUTINE SSTSTP
