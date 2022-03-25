!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief DEFINITION OF CONSTANTS THAT CONTROLS THE DOMAIN
!> @details DEFINITION OF CONSTANTS THAT CONTROLS THE DOMAIN.
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
!! @arg @c DOM
!! @arg @c F77KINDS
!! @arg @c MPPSTAFF
!! @arg @c PARMETA
!> @details <b>Driver:</b> 
!! @arg @c GEF
!> @details <b>Calls:</b>
!--------------------------------------------------------------------------------------------------
    SUBROUTINE INITDOM 
!--------------------------------------------------------------------------------------------------
! SUBROUTINE INITDOM
! 
! SUBPROGRAM: INITDOM -  DEFINITION OF CONSTANTS THAT CONTROLS THE DOMAIN
! PROGRAMMER: ?????          
! ORG: W/NP22
! DATE: ??-??-??
!
! ABSTRACT:
! DEFINITION OF CONSTANTS THAT CONTROLS THE DOMAIN                                          
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
! USE MODULES: DOM
!              F77KINDS
!              MPPSTAFF
!              PARMETA
! 
! DRIVER     : GEF
!
! CALLS      : 
!--------------------------------------------------------------------------------------------------
    USE DOM
    USE F77KINDS
    USE MPPSTAFF
    USE PARMETA
!
    IMPLICIT NONE
!--------------------------------------------
! LOCAL VARIABLES
! N    : 
! NSTRD: STARTING PROCESSOR NUMBER ON MY FACE
!--------------------------------------------
    INTEGER(KIND=I4)                                                                            ::&
    & N       , NSTRD
!
    MY_FACE = MYPE    / NXY
    NSTRD   = MY_FACE * NXY
!
    JY = (MYPE - NSTRD) / JYM + 1
    IX = MOD(MYPE - NSTRD, JYM) + 1
!
    MY_FACE = MY_FACE + 1
!
    LWEST  = IX == 1
    LEAST  = IX == IXM
    LSOUTH = JY == 1
    LNORTH = JY == JYM
!
    DO N=1,NM
        SW(N) = (N-1) * NXY
        SE(N) = SW(N) + IXM - 1
        NW(N) = SW(N) + IXM * (JYM-1)
        NE(N) = NW(N) + IXM-1
    END DO
!
    END SUBROUTINE INITDOM 
