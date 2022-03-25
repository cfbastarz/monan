!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module COMPVS... 
!! @details Details of Module COMPVS... 
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c COMPVS
!! @arg @c F77KINDS 
!! @details <b>Driver:</b> 
!! @arg @c GSMCONST 
!<
!--------------------------------------------------------------------------------------------------
    MODULE COMPVS
!--------------------------------------------------------------------------------------------------
! MODULE COMPVS
!
! USE MODULES: COMPVS0
!              F77KINDS
! 
! DRIVER     : GSMCONST
!--------------------------------------------------------------------------------------------------
    USE COMPVS0 , ONLY : NX
    USE F77KINDS
!
    IMPLICIT NONE
!
    REAL   (KIND=R4)                                                                            ::&
    & C1XPVS  , C2XPVS
!
    REAL   (KIND=R4)    , DIMENSION(NX)                                                         ::&
    & TBPVS
!
    END MODULE COMPVS
