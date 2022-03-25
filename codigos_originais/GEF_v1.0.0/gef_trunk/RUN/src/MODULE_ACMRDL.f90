!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module ACMRDL... 
!! @details Details of Module ACMRDL... 
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
    MODULE ACMRDL
!--------------------------------------------------------------------------------------------------
! MODULE ACMRDL
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
    & TRDLW   , ARDLW
!
    INTEGER(KIND=I4)                                                                            ::&
    & NRDLW
!
    REAL   (KIND=R4)    , DIMENSION(:,:)              , ALLOCATABLE                             ::&
    & RLWIN   , RLWOUT  , RLWTOA  ,                                                               &
    & ALWIN   , ALWOUT  , ALWTOA  ,                                                               &
    & RLWNET
!
    REAL   (KIND=R4)    , DIMENSION(:,:,:)            , ALLOCATABLE                             ::&
    & RLWTT
!
    END MODULE ACMRDL
