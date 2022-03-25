!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module ACMPRE... 
!! @details Details of Module ACMPRE... 
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS 
!! @details <b>Driver:</b> 
!! @arg @c ALLOC 
!! @arg @c INIT
!! @arg @c KFTEND
!! @arg @c OUT
!! @arg @c OUT2
!! @arg @c OUT_HEAT
!! @arg @c READ_SST12M
!! @arg @c SURFCE 
!<
!--------------------------------------------------------------------------------------------------
    MODULE ACMPRE
!--------------------------------------------------------------------------------------------------
! MODULE ACMPRE
!
! USE MODULES: F77KINDS
!              
! DRIVER     : ALLOC
!              INIT
!              KFTEND
!              OUT
!              OUT2
!              OUT_HEAT
!              READ_SST12M
!              SURFCE
!-------------------------------------------------------------------------------------------------- 
    USE F77KINDS
!
    IMPLICIT NONE
!
    SAVE
!
    REAL   (KIND=R4)                                                                            ::&
    & TPREC
!
    REAL   (KIND=R4)    , DIMENSION(:,:)              , ALLOCATABLE                             ::&
    & ACSNOW  , ACSNOM  , SSROFF  , BGROFF
!
    END MODULE ACMPRE
