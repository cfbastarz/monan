!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief CHECK MIN MAX OF THE FIELDS
!> @details CHECK MIN MAX OF THE FIELDS.
!> @author ORIGINATOR - ????? 
!> @date 87-06-?? \n
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
!! @arg @c CLDWTR
!! @arg @c CONTIN
!! @arg @c CTLBLK
!! @arg @c DGNSOUT
!! @arg @c F77KINDS
!! @arg @c LOOPS
!! @arg @c MPPSTAFF
!! @arg @c PARMETA
!! @arg @c PVRBLS
!! @arg @c VRBLS  
!> @details <b>Driver:</b> 
!! @arg @c DIGFLT
!! @arg @c GEF
!> @details <b>Calls:</b>
!! @arg @c MPI_REDUCE
!<
!--------------------------------------------------------------------------------------------------
    SUBROUTINE CHECKMXMN
!--------------------------------------------------------------------------------------------------
! SUBROUTINE CHECKMXMN
! 
! SUBPROGRAM: CHECKMXMN - CHECK MIN MAX OF THE FIELDS
! PROGRAMMER: ????? 
! ORG: W/NP22
! DATE: ??-??-??
!
! ABSTRACT:  
! CHECK MIN MAX OF THE FIELDS
!
! PROGRAM HISTORY LOG:
! 87-06-??  ?????   - ORIGINATOR
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
! USE MODULES: CLDWTR
!              CONTIN
!              CTLBLK
!              DGNSOUT
!              F77KINDS
!              LOOPS
!              MPPSTAFF
!              PARMETA
!              PVRBLS
!              VRBLS
!
! DRIVER     : DIGFLT
!              INIT 
!
! CALLS      : MPI_REDUCE
!--------------------------------------------------------------------------------------------------
    USE CLDWTR
    USE CONTIN
    USE CTLBLK
    USE DGNSOUT
    USE F77KINDS
    USE LOOPS
    USE MPPSTAFF
    USE PARMETA
    USE PVRBLS
    USE VRBLS
!
    IMPLICIT NONE
!
    INCLUDE "mpif.h"
!
    INTEGER(KIND=I4)                                                                            ::&
    & I       , J       , L       , IT      , JT      , ECODE   , IERR    , IRTN
!
    REAL   (KIND=R4)    , DIMENSION(8)                                                          ::&
    & HVMIN   , HVMAX   , HVMAX_0 , HVMIN_0
!
    HVMAX = -1.E10
    HVMIN =  1.E10
!
    DO I=1,IM
        DO J=1,JM
!
            HVMAX(2) = MAX(HVMAX(2), PD(I,J))
            HVMIN(2) = MIN(HVMIN(2), PD(I,J))
!
            DO L=1,LMH(I,J)
                HVMAX(1) = MAX(HVMAX(1),   T(I,J,L))
                HVMIN(1) = MIN(HVMIN(1),   T(I,J,L))
!
                HVMAX(6) = MAX(HVMAX(6), DIV(I,J,L))
                HVMIN(6) = MIN(HVMIN(6), DIV(I,J,L))
!
                HVMAX(7) = MAX(HVMAX(7),   Q(I,J,L))
                HVMIN(7) = MIN(HVMIN(7),   Q(I,J,L))
!
                HVMAX(8) = MAX(HVMAX(8), CWM(I,J,L))
                HVMIN(8) = MIN(HVMIN(8), CWM(I,J,L))
            END DO
!
            DO L=1,LMV(I,J)
                HVMAX(3) = MAX(HVMAX(3),   U(I,J,L))
                HVMIN(3) = MIN(HVMIN(3),   U(I,J,L))
!
                HVMAX(4) = MAX(HVMAX(4),   V(I,J,L))
                HVMIN(4) = MIN(HVMIN(4),   V(I,J,L))
            END DO
!
            DO L=1,LMH(I,J)-1
                HVMAX(5) = MAX(HVMAX(5),  Q2(I,J,L))
                HVMIN(5) = MIN(HVMIN(5),  Q2(I,J,L))
            END DO
!
        END DO
    END DO
!
    CALL MPI_REDUCE(HVMAX, HVMAX_0, 8, MPI_REAL, MPI_MAX, 0, MPI_COMM_WORLD, IRTN)
    CALL MPI_REDUCE(HVMIN, HVMIN_0, 8, MPI_REAL, MPI_MIN, 0, MPI_COMM_WORLD, IRTN)
!
    END SUBROUTINE CHECKMXMN
