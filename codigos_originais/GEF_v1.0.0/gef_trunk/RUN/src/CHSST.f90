!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Adicionar um Brief
!> @details Adicionar um Details.
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
!> @param[in] N - Significado de N
!> @details <b>Use Module:</b> 
!! @arg @c F77KINDS
!! @arg @c MPPSTAFF
!! @arg @c PHYS
!> @details <b>Driver:</b> 
!<
!> @details <b>Calls:</b> 
!<
!--------------------------------------------------------------------------------------------------
    SUBROUTINE CHSST(N)
!--------------------------------------------------------------------------------------------------
! SUBROUTINE CHSST
! 
! SUBPROGRAM: CHSST - ?????
! PROGRAMMER: ????? 
! ORG: W/NP22
! DATE: ??-??-??
!
! ABSTRACT:  
! 
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
! N - 
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
! USE MODULES: F77KINDs
!              MPPSTAFF
!              PHYS
!
! DRIVER     : -----         
!
! CALLS      : -----
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
    USE MPPSTAFF
    USE PHYS
!
    IMPLICIT NONE
!
    INTEGER(KIND=I4)                                                      , INTENT(IN)          ::&
    & N
!
    CHARACTER(LEN=80)                                                                           ::&
    & FNAME
!
    CHARACTER(LEN= 3)                                                                           ::&
    & CN
!
    WRITE(CN,'(I3.3)') N
!
    FNAME='sst'//CN//'.'//C_MYPE
!
    IF (MYPE == 0) THEN
        PRINT *, FNAME
    END IF
!
    OPEN (1,FILE=FNAME,STATUS='OLD',FORM='UNFORMATTED')
!
    READ(1) SST
!
    CLOSE (1)
!
    END SUBROUTINE CHSST
