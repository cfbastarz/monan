!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief MODULE F77KINDS ARE USED TO IMPLICIT THE VARIABLE TYPE 
!! @details MODULE F77KINDS ARE USED TO IMPLICIT THE VARIABLE TYPE IN ALL SUBROUTINES, MODULES AND 
!! FUNCTIONS. 
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @details <b>Driver:</b> 
!! @arg @c ALL SUBROUTINES, MODULES AND FUNCTIONS
!<
!--------------------------------------------------------------------------------------------------
    MODULE F77KINDS
!--------------------------------------------------------------------------------------------------
! MODULE F77KINDS
!
! USE MODULES: ----- 
!        
! DRIVER: ALL SUBROUTINES, MODULES AND FUNCTIONS.
!--------------------------------------------------------------------------------------------------
    IMPLICIT NONE
!
    INTEGER, PARAMETER :: I1   =  1  ! INTEGER*1	  
    INTEGER, PARAMETER :: I2   =  2  ! INTEGER*2
    INTEGER, PARAMETER :: I4   =  4  ! INTEGER*4
    INTEGER, PARAMETER :: I8   =  8  ! INTEGER*8
!
    INTEGER, PARAMETER :: L1   =  1  ! LOGICAl*1
    INTEGER, PARAMETER :: L4   =  4  ! LOGICAL*4
    INTEGER, PARAMETER :: L8   =  8  ! LOGICAL*8
!
    INTEGER, PARAMETER :: R4   =  4  ! REAL*4  
    INTEGER, PARAMETER :: R8   =  8  ! REAL*8
!
    INTEGER, PARAMETER :: CX8  =  8  ! COMPLEX*8
    INTEGER, PARAMETER :: CX16 =  16 ! COMPLEX*16
!
  END MODULE F77KINDS
