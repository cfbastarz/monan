 !--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module METRCS...
!! @details Details of Module METRCS...
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS
!! @details <b>Driver:</b> 
!! @arg @c ADJUST
!! @arg @c ALLOC
!! @arg @c EPS
!! @arg @c HDIFF
!! @arg @c HZADV
!! @arg @c HZADVQ
!! @arg @c INIT
!! @arg @c KFPARA
!! @arg @c KINEN
!! @arg @c SPONGE
!! @arg @c TURBL
!! @arg @c VTADV
!<
!--------------------------------------------------------------------------------------------------
    MODULE METRCS
!--------------------------------------------------------------------------------------------------
! MODULE METRCS
!
! USE MODULES: F77KINDS
!
! DRIVER     : ADJUST
!              ALLOC
!              EPS
!              HDIFF
!              HZADV
!              HZADVQ
!              INIT
!              KFPARA
!              KINEN
!              SPONGE
!              TURBL
!              VTADV
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    SAVE
!
    REAL   (KIND=R4)    , DIMENSION (:,:)             , ALLOCATABLE                             ::&
    &  SQV    ,  SQH    ,   Q11   ,   Q12   ,   Q22   ,                                           &
    &                     QBV11   , QBV12   , QBV22   ,                                           &
    &           RSQH    ,  QH11   ,  QH12   ,  QH22   ,                                           &
    & RSQV    , SQV_NORM
!
    REAL   (KIND=R4)    , DIMENSION(:,:,:)            , ALLOCATABLE                             ::&
    & QD11    , QD12    , QD21    , QD22
!
    REAL   (KIND=R4)    , DIMENSION(:,:,:,:,:)        , ALLOCATABLE                             ::&
    & QVH     , QHV     , QVH2    , QHV2
!
    REAL   (KIND=R4)    , DIMENSION(:,:,:,:,:)        , ALLOCATABLE                             ::&
    & QA
!  
    REAL   (KIND=R4)    , DIMENSION(:,:,:,:)          , ALLOCATABLE                             ::&
    & QB
!
    END MODULE METRCS
