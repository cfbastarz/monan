!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief INITIALIZE ARRAYS
!> @details INITIALIZE ARRAYS FOR CHECKING MIN MAX OF THE FIELDS.
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
!! @arg @c DGNSOUT
!! @arg @c F77KINDS
!> @details <b>Driver:</b> 
!! @arg @c INIT
!> @details <b>Calls:</b>
!--------------------------------------------------------------------------------------------------
    SUBROUTINE INITCKMM
!--------------------------------------------------------------------------------------------------
! SUBROUTINE INITCKMM
! 
! SUBPROGRAM: INITCKMM -  INITIALIZE ARRAYS
! PROGRAMMER: ????? 
! ORG: W/NP22
! DATE: ??-??-??
!
! ABSTRACT:  
! INITIALIZE ARRAYS FOR CHECKING MIN MAX OF THE FIELDS.
!
! PROGRAM HISTORY LOG:
! ??-??-??  ?????  - ORIGINATOR
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
! USE MODULES: CTLBLK
!              DGNSOUT
!              F77KINDS
!
! DRIVER     : INIT
!
! CALLS      : -----             
!--------------------------------------------------------------------------------------------------
    USE CTLBLK
    USE DGNSOUT
    USE F77KINDS
!
    IMPLICIT NONE
!
    INTEGER(KIND=I4)                                                                            ::&
    & N
!
    N     = NTSTM / ICKMM + 1
!
    CKNUM = 0
!
    ALLOCATE ( TMXMN(N,2), PDMXMN(N,2), UMXMN(N,2), VMXMN(N,2) )
!    ALLOCATE ( TMXMN(N,2))
!    ALLOCATE (PDMXMN(N,2))
!    ALLOCATE ( UMXMN(N,2))
!    ALLOCATE ( VMXMN(N,2))
!
    ALLOCATE ( LOCTMM(N,3,2), LOCPDMM(N,3,2), LOCUMM(N,3,2), LOCVMM(N,3,2) )
!    ALLOCATE ( LOCTMM(N,3,2))
!    ALLOCATE (LOCPDMM(N,3,2))
!    ALLOCATE ( LOCUMM(N,3,2))
!    ALLOCATE ( LOCVMM(N,3,2))
!
    END SUBROUTINE INITCKMM
