!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief INITIALIZE MPI
!> @details INITIALIZE MPI.
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
!> @details <b>Driver:</b> 
!! @arg @c GEF
!> @details <b>Calls:</b>
!! @arg @c MPI_BARRIER
!! @arg @c MPI_COMM_RANK
!! @arg @c MPI_COMM_SIZE
!! @arg @c MPI_INIT
!--------------------------------------------------------------------------------------------------
    SUBROUTINE INITMPI
!--------------------------------------------------------------------------------------------------
! SUBROUTINE INITMPI
! 
! SUBPROGRAM: INITMPI -  INITIALIZE MPI
! PROGRAMMER: ?????          
! ORG: W/NP22
! DATE: ??-??-??
!
! ABSTRACT:
! INITIALIZE MPI                                     
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
! USE MODULES: F77KINDS
!              MPPSTAFF
! 
! DRIVER     : GEF
!
! CALLS      : MPI_BARRIER
!              MPI_COMM_RANK
!              MPI_COMM_SIZE
!              MPI_INIT
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
    USE MPPSTAFF
!
    IMPLICIT NONE
!
    INCLUDE "mpif.h"
!
    INTEGER(KIND=I4)                                                                            ::&
    & IERR
!------------------
! INITIAL MPI CALLS
!------------------
    CALL MPI_INIT(IERR)
    CALL MPI_COMM_RANK(MPI_COMM_WORLD, MYPE, IERR)
    CALL MPI_COMM_SIZE(MPI_COMM_WORLD, NPES, IERR)
!
    ITYPE   = MPI_REAL
    DGNSNUM = NPES - 1
    DGNSTAG = 101
!--------
! NUMBERS
!--------  
    LISTINVRBLS = 10
    LISTINCONST = 11
    LISTOUT     = 98
!------
! CHARS
!------  
    WRITE(C_MYPE,1000) MYPE
    1000 FORMAT(I4.4)    
!
    CALL MPI_BARRIER(MPI_COMM_WORLD, IERR)
!
    END SUBROUTINE INITMPI
      
