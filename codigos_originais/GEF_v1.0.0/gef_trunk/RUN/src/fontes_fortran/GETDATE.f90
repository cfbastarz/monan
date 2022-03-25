!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Inserir Brief de GETDATE
!> @details Inserir Details de GETDATE
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
!> @param[in] IYR    - Significado de IYR
!> @param[in] IMO  - Significado de IMO
!> @param[in] IDY - Significado de IDY
!> @param[in] IUTC - Significado de IUTC
!> @param[out] CYR - Significado de CYR
!> @param[out] CMON - Significado de CMON
!> @param[out] CDAY - Significado de CDAY
!> @param[out] CUTC - Significado de CUTC
!> @details <b>Use Module:</b>
!! @arg @c F77KINDS
!> @details <b>Driver:</b> 
!! @arg @c SSTCH
!! @arg @c VEGUPDT
!> @details <b>Calls:</b>
!-------------------------------------------------------------------------------------------------- 
 SUBROUTINE GETDATE(IYR, IMO, IDY, IUTC, CYR, CMON, CDAY, CUTC)
!--------------------------------------------------------------------------------------------------
! SUBROUTINE GETDATE
!
! SUBPROGRAM: GETDATE - ?????
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
! IYR  - 
! IMO  - 
! IDY  - 
! IUTC -                                      
!  
! OUTPUT ARGUMENT LIST:
! CYR  -
! CMON - 
! CDAY - 
! CUTC -
!
! INPUT/OUTPUT ARGUMENT LIST:
! NONE                                                 
!        
! USE MODULES: F77KINDS
!
! DRIVER     : SSTCH
!              VEGUPDT
!
! CALLS      : -----
!-------------------------------------------------------------------------------------------------- 
    USE F77KINDS
!
    IMPLICIT NONE
!
    INTEGER(KIND=I4)                                                      , INTENT(IN)          ::&
    & IYR     , IMO     , IDY     , IUTC
!
    INTEGER(KIND=I4)                                                      , INTENT(OUT)         ::&
    & CYR     , CMON    , CDAY    , CUTC
!
    INTEGER(KIND=I4)                                                                            ::&
    & YR      , MON     , DAY     , UTC
!
    INTEGER(KIND=I4)    , DIMENSION(12)                                                         ::&
    & MONL
!
    INTEGER(KIND=I4)                                                                            ::&
    & DYSTEP  , DAYSTEP
!
    YR  = IYR
    MON = IMO
    DAY = IDY
    UTC = IUTC
!      
!    DATA MONL /30,30,30,30,30,30,30,30,30,30,30,30/
    DATA MONL /31,28,31,30,31,30,31,31,30,31,30,31/
!
!    DAYSTEP = 0	
    DYSTEP = 0
!
    IF (MOD(YR,4) == 0) THEN  
        MONL(2) = 29   
    END IF 
!
    IF (UTC >= 24) THEN
        DYSTEP = DYSTEP + (UTC / 24)
!        DAYSTEP = DAYSTEP + (UTC / 24)
!   
        UTC    = (MOD(UTC, 24))
    END IF
!
    DAY = DAY + DYSTEP     
!    DAY = DAY + DAYSTEP
!
    DO
        IF (DAY > MONL(MON)) THEN
            DAY = DAY - MONL(MON)
            MON = MON + 1
!
            IF (MON == 13) THEN
                MON = 1
                YR  = YR + 1
!                IF (MOD(YR,4) == 0) MONL(2) = 29
            END IF
!
        ELSE
!
            EXIT
!
        END IF
!
    END DO
!
    CYR  = YR
    CMON = MON
    CDAY = DAY
    CUTC = UTC
!  
!    IF (MYPE == 0) THEN      
!        PRINT*, 'DAY_GETDATE=', DAY
!    ENDIF
!
    RETURN
!
    END SUBROUTINE GETDATE
