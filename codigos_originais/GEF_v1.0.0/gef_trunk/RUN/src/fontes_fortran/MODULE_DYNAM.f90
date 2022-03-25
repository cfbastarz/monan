!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module DYNAM... 
!! @details Details of Module DYNAM... 
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS
!! @details <b>Driver:</b> 
!! @arg @c ADJUST
!! @arg @c ALLOC
!! @arg @c BOCOHMPI
!! @arg @c BOCOVMPI_SC
!! @arg @c CUCNVC
!! @arg @c DDAMP
!! @arg @c DIGFLT
!! @arg @c GSMCONST
!! @arg @c GSMDRIVE
!! @arg @c HDIFF
!! @arg @c HZADV
!! @arg @c HZADVQ
!! @arg @c INIT
!! @arg @c KFDRIVE
!! @arg @c KFPARA
!! @arg @c KFTEND
!! @arg @c KINEN
!! @arg @c MODULE_CUPARM
!! @arg @c NEWTON
!! @arg @c OUTSD
!! @arg @c PDETE
!! @arg @c RADTN
!! @arg @c READ_SST12M
!! @arg @c SURFCE
!! @arg @c TURBL
!! @arg @c VTADV
!<
!--------------------------------------------------------------------------------------------------
    MODULE DYNAM
!--------------------------------------------------------------------------------------------------
! MODULE DYNAM
!
! USE MODULES: F77KINDS
!
! DRIVER     : ADJUST
!              ALLOC
!              BOCOHMPI
!              BOCOVMPI_SC
!              CUCNVC
!              DDAMP
!              DIGFLT
!              GSMCONST
!              GSMDRIVE
!              HDIFF
!              HZADV
!              HZADVQ
!              INIT
!              KFDRIVE
!              KFPARA
!              KFTEND
!              KINEN
!              MODULE_CUPARM
!              NEWTON
!              OUTSD
!              PDETE
!              RADTN
!              READ_SST12M
!              SURFCE
!              TURBL
!              VTADV 
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    SAVE
!
    REAL   (KIND=R4)    , PARAMETER :: CP = 1004.6
!
    REAL   (KIND=R4)                                                                            ::&
    & FADV    , FADV2   , FADT    , R       , PT      , F4D     , F4Q     , EF4T    , FKIN    ,   &
    & FCP     , DELTY   , DELTHZ  , KAPPA   , P0      , TSPH    , RTDF    , RTDFDT  , FADTQ   ,   &
    & F4D25
!
    REAL   (KIND=R4)    , DIMENSION(:)                , ALLOCATABLE                             ::&
    & DETA    , RDETA   , AETA    , DAETA   , F4Q2
!
    REAL   (KIND=R4)    , DIMENSION(:)                , ALLOCATABLE                             ::&
    & ETA     , DFL
!
    REAL   (KIND=R4)    , DIMENSION(:,:)              , ALLOCATABLE                             ::&
    & WPDAR   , F11     , F12     , F21     , F22     , P11     , P12     , P21     , P22     ,   &
    & FDIV    , FDDMP   , HSINP   , HCOSP   , FVDIFF  , HBMSK   , DX      , DDMP
!
    END MODULE DYNAM
