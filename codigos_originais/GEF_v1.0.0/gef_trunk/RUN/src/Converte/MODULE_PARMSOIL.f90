!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module PARMSOIL...
!! @details Details of Module PARMSOIL...
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS
!! @details <b>Driver:</b> 
!! @arg @c ALLOC
!! @arg @c INIT
!! @arg @c RADTN
!! @arg @c READ_SST12M
!! @arg @c SURFCE
!! @arg @c VEGUPDT
!<
!--------------------------------------------------------------------------------------------------
    MODULE PARMSOIL
!--------------------------------------------------------------------------------------------------
! MODULE PARMSOIL
!
! USE MODULES: F77KINDS
!
! DRIVER     : ALLOC
!              INIT
!              RADTN
!              READ_SST12M
!              SURFCE
!              VEGUPDT 
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    INTEGER(KIND=I4), PARAMETER :: NSOIL = 4
    INTEGER(KIND=I4), PARAMETER :: NROOT = 3
!
    END MODULE PARMSOIL
