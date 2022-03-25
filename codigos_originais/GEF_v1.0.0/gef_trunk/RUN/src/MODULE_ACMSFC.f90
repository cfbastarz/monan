!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module ACMSFC... 
!! @details Details of Module ACMSFC... 
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS 
!! @details <b>Driver:</b> 
!! @arg @c ALLOC 
!! @arg @c INIT
!! @arg @c OUT2
!! @arg @c READ_SST12M
!! @arg @c SURFCE 
!<
!--------------------------------------------------------------------------------------------------
    MODULE ACMSFC
!--------------------------------------------------------------------------------------------------
! MODULE ACMSFC
!
! USE MODULES: F77KINDS
!              
! DRIVER     : ALLOC
!              INIT
!              OUT2
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
    & TSRFC   , ASRFC   , APHTIM
!
    INTEGER(KIND=I4)                                                                            ::&
    & NSRFC
!
    REAL   (KIND=R4)    , DIMENSION(:,:)              , ALLOCATABLE                             ::&
    & SFCSHX  , SFCLHX  ,                                                                         &
    & SUBSHX  , SNOPCX  ,                                                                         &
    & SFCUVX  , SFCEVP  ,                                                                         &
    & POTEVP  , POTFLX
!
    END MODULE ACMSFC
