!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module SOIL...
!! @details Details of Module SOIL...
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS
!! @details <b>Driver:</b> 
!! @arg @c ALLOC
!! @arg @c GEF
!! @arg @c INIT
!! @arg @c OUT
!! @arg @c OUT2
!! @arg @c OUT_HEAT
!! @arg @c RADTN
!! @arg @c READ_SST12M
!! @arg @c SURFCE
!! @arg @c VEGUPDT
!<
!--------------------------------------------------------------------------------------------------
    MODULE SOIL
!--------------------------------------------------------------------------------------------------
! MODULE SOIL
!
! USE MODULES: F77KINDS
!
! DRIVER     : ALLOC
!              GEF
!              INIT
!              OUT
!              OUT2
!              OUT_HEAT
!              RADTN
!              READ_SST12M
!              SURFCE
!              VEGUPDT
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    SAVE
!
    REAL   (KIND=R4)    , DIMENSION(:,:)              , ALLOCATABLE                             ::&
    & SOILTB  , SFCEXC  , SMSTAV  , SMSTOT  , GRNFLX  , PCTSNO  , VEGFRC  , CMC
!
    INTEGER(KIND=I4)    , DIMENSION(:,:)              , ALLOCATABLE                             ::&
    & IVGTYP  , ISLTYP  , ISLOPE
!
    REAL   (KIND=R4)    , DIMENSION(:,:,:)            , ALLOCATABLE                             ::&
    & SMC     , STC     , SH2O
!
    REAL   (KIND=R4)    , DIMENSION(:)                , ALLOCATABLE                             ::&
    & SLDPTH  , RTDPTH
!
    END MODULE SOIL
