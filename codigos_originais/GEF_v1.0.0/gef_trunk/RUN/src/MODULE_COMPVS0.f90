!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module COMPVS0... 
!! @details Details of Module COMPVS0... 
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS 
!! @details <b>Driver:</b> 
!! @arg @c GSMCONST 
!! @arg @c MODULE_COMPVS
!<
!--------------------------------------------------------------------------------------------------
    MODULE COMPVS0
!--------------------------------------------------------------------------------------------------
! MODULE COMPVS0
!
! USE MODULES: F77KINDS
! 
! DRIVER     : GSMCONST
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    INTEGER(KIND=I4)    , PARAMETER :: NX = 7501
!
    REAL   (KIND=R4)                                                                            ::&
    & C1XPVS0 , C2XPVS0
!
    REAL   (KIND=R4)    , DIMENSION(NX)                                                         ::&
    & TBPVS0
!
    END MODULE COMPVS0
