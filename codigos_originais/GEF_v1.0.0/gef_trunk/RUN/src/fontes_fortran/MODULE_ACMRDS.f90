!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module ACMRDS... 
!! @details Details of Module ACMRDS... 
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS 
!! @details <b>Driver:</b> 
!! @arg @c ALLOC 
!! @arg @c INIT
!! @arg @c OUT
!! @arg @c OUT2
!! @arg @c OUT_HEAT
!! @arg @c RADTN
!! @arg @c RDTEMP
!! @arg @c READ_SST12M
!! @arg @c SURFCE 
!<
!--------------------------------------------------------------------------------------------------
    MODULE ACMRDS
!--------------------------------------------------------------------------------------------------
! MODULE ACMRDS
!
! USE MODULES: F77KINDS
!              
! DRIVER     : ALLOC
!              INIT
!              OUT
!              OUT2
!              OUT_HEAT
!              RADTN
!              RDTEMP
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
    & TRDSW   , ARDSW
!
    INTEGER(KIND=I4)                                                                            ::&
    & NRDSW
!
    REAL   (KIND=R4)    , DIMENSION(:,:)              , ALLOCATABLE                             ::&
    & RSWIN   , RSWOUT  , RSWTOA  ,                                                               &
    & ASWIN   , ASWOUT  , ASWTOA  ,                                                               &
    & RSWNET
!
    REAL   (KIND=R4)    , DIMENSION(:,:,:)            , ALLOCATABLE                             ::&
    & RSWTT
!
    END MODULE ACMRDS
