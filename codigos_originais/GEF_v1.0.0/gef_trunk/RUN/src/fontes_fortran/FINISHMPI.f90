!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief FINALIZE MPI TASKS
!> @details FINALIZE MPI TASKS.
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
!! @arg @c F77KINDS
!! @arg @c MPPSTAFF
!! @arg @c DGNSOUT
!> @details <b>Driver:</b> 
!! @arg @c GEF
!> @details <b>Calls:</b>
!! @arg @c MPI_FINALIZE
!--------------------------------------------------------------------------------------------------
    SUBROUTINE FINISHMPI
!--------------------------------------------------------------------------------------------------
! SUBROUTINE FINISHMPI
!
! SUBPROGRAM: FINISHMPI - FINALIZE MPI TASKS.
! PROGRAMMER: ?????
! ORG: ?????
! DATE: ??-??-??
!
! ABSTRACT:
! FINALIZE MPI TASKS.
! 
! PROGRAM HISTORY LOG:
! ??-??-??  ?????      - ORIGINATOR
! 18-03-20  LUCCI      - MODERNIZATION OF THE CODE, INCLUDING:
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
! USE MODULES: F77KINDS
!              MPPSTAFF
!              DGNSOUT
!  
! DRIVER     : GEF
!
! CALLS      : MPI_FINALIZE
!--------------------------------------------------------------------------------------------------
    USE DGNSOUT
    USE F77KINDS   
    USE MPPSTAFF
!
    IMPLICIT NONE
!
    INCLUDE "mpif.h"
!
    CHARACTER(LEN=80)                                                                           ::&
    & OUTFILE

    INTEGER(KIND=I4)                                                                            ::&
    & IT      , JT      , ECODE   , IERR
!
    CALL MPI_FINALIZE(IERR)              
!
    END SUBROUTINE FINISHMPI
      
