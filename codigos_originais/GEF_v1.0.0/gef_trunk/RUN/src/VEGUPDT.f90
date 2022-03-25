!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Inserir Brief de VEGUPDT
!> @details Inserir Details de VEGUPDT 
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
!! @arg @c CTLBLK	
!! @arg @c F77KINDS
!! @arg @c LOOPS
!! @arg @c MASKS
!! @arg @c MPPSTAFF							   
!! @arg @c PARMETA	
!! @arg @c PARMSOIL							  
!! @arg @c PARM_TBL								  
!! @arg @c PHYS							  
!! @arg @c SOIL
!> @details <b>Driver:</b> 
!<
!> @details <b>Calls:</b>
!! @arg @c GETDATE
!--------------------------------------------------------------------------------------------------     
    SUBROUTINE VEGUPDT
!--------------------------------------------------------------------------------------------------
! SUBROUTINE VEGUPDT
!
! SUBPROGRAM: VEGUPDT - 
! PROGRAMMER: ?????   
! ORG: ?????
! DATE: ??-??-??
! 
! ABSTRACT: 
! ?????
!
! PROGRAM HISTORY LOG:
! ??-??-??  ?????       - ORIGINATOR
! 18-01-15  LUCCI       - MODERNIZATION OF THE CODE, INCLUDING:
!                         * F77 TO F90/F95
!                         * INDENTATION & UNIFORMIZATION CODE
!                         * REPLACEMENT OF COMMONS BLOCK FOR MODULES
!                         * DOCUMENTATION WITH DOXYGEN
!                         * OPENMP FUNCTIONALITY
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
! NONE
!
! USE MODULES: CTLBLK
!              F77KINDS
!              LOOPS
!              MASKS
!              MPPSTAFF
!              PARMETA
!              PARMSOIL
!              PARM_TBL
!              PHYS
!              SOIL
!
! DRIVER     : -----
!
! CALLS      : GETDATE
!--------------------------------------------------------------------------------------------------	
    USE CTLBLK	
    USE F77KINDS
    USE LOOPS
    USE MASKS
    USE MPPSTAFF							   
    USE PARMETA	
    USE PARMSOIL							  
    USE PARM_TBL								  
    USE PHYS							  
    USE SOIL
!
    IMPLICIT NONE
!							  
    INTEGER(KIND=I4)    , PARAMETER    :: LA     = 13						  
    INTEGER(KIND=I4)    , PARAMETER    :: IMJM   = IM * JM -  JM / 2
    INTEGER(KIND=I4)    , PARAMETER    :: JAM    =  6 +  2 * (JM - 10)
    INTEGER(KIND=I4)    , PARAMETER    :: LTOP   =  1
    INTEGER(KIND=I4)    , PARAMETER    :: LBOT   = LM 
    INTEGER(KIND=I4)    , PARAMETER    :: LSCRCH =  4 * LM +  1 + LA + 1
    INTEGER(KIND=I4)    , PARAMETER    :: LL1     = LA + LM +  1
    INTEGER(KIND=I4)    , PARAMETER    :: LL2     = LA +  2 * LM +  1
    INTEGER(KIND=I4)    , PARAMETER    :: LL3     = LA +  3 * LM +  1
    INTEGER(KIND=I4)    , PARAMETER    :: LL4     = LA +  4 * LM +  1			  
!   
    INTEGER(KIND=I4)                                                                            ::&
    & DAY     , MON     , YR      , UTC     , MA      , MB      , IMES2   , IMES    , NMESST  ,   &
    & IYR     , IMO     , IDY     , IUTC
!
    REAL   (KIND=R4)                                                                            ::&
    & YDAY    , ADD     , FA      , FB
!
    INTEGER(KIND=I4)    , DIMENSION(12)	                                                        ::&
    & MONL
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1)                                             ::&
    & VEGFRM
!     
    CHARACTER(LEN=256)                                                                          ::&
    & M_BEFORE2         , M_AFTER2
!
    CHARACTER(LEN=2)                                                                            ::&
    & CN2
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1)                                             ::&
    & MONBEFORE         , MONAFTER
!      
    IYR  = IDAT(3)
    IMO  = IDAT(1)
    IDY  = IDAT(2)
    IUTC = NTSD * DT / 3600 ! CHOU DT EH 90SEG
!  
    CALL GETDATE(IYR, IMO, IDY, IUTC, YR, MON, DAY, UTC)
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
    IF (YDAY >= (1.0+FLOAT(MONL(MON)) / 2.0)) THEN     
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
        MB = 1
    END IF
!       
    PRINT*, "MAVEG", MA, "DAYVEG", YDAY, "YRVEG", YR
!     
    ADD = FLOAT(MONL(MA)) / 2.0 - 1.0
!      
    IF (MA == MON) THEN
        ADD = -ADD - 2.0
    END IF
!    						   
    FB = 2.0 * (YDAY + ADD) / FLOAT(MONL(MA) + MONL(MB))					   
    FA = 1.0 - FB
!
    PRINT*, "IMESVEG=", MA, "IMES2VEG=", MB
!	
    WRITE(CN2,'(I2.2)') MA
!
    M_BEFORE2 = 'VGREEN_12mon' // CN2 // '.' // C_MYPE
!      
    OPEN (33, FILE= M_BEFORE2, FORM='UNFORMATTED', STATUS='UNKNOWN', ACCESS='SEQUENTIAL')
!
    READ(33) MONBEFORE
!
    CLOSE(33)
!      
    WRITE(CN2,'(I2.2)') MB
!  
    M_AFTER2 = 'VGREEN_12mon' // CN2 // '.' // C_MYPE
!      
   OPEN (44, FILE=M_AFTER2, FORM='UNFORMATTED', STATUS='UNKNOWN', ACCESS='SEQUENTIAL')
!
   READ(44) MONAFTER
!
    CLOSE(44)
!      
    VEGFRC = FA * MONBEFORE(:,:) + FB * MONAFTER(:,:)						   
!      
    RETURN
!										   
    END SUBROUTINE VEGUPDT
	
