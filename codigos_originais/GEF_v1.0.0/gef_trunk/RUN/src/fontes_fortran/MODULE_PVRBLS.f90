!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module PVRBLS...
!! @details Details of Module PVRBLS...
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS
!! @details <b>Driver:</b> 
!! @arg @c ALLOC
!! @arg @c CHECKMXMN
!! @arg @c CUCNVC
!! @arg @c DIGFLT
!! @arg @c GEF
!! @arg @c GSMDRIVE
!! @arg @c HDIFF
!! @arg @c HZADV
!! @arg @c HZADVQ
!! @arg @c INIT
!! @arg @c KFDRIVE
!! @arg @c KFPARA
!! @arg @c KFTEND
!! @arg @c OUT
!! @arg @c OUTSD
!! @arg @c OUT2
!! @arg @c OUT_HEAT
!! @arg @c RADTN
!! @arg @c RDTEMP
!! @arg @c READ_SST12M
!! @arg @c SURFCE
!! @arg @c TURBL
!! @arg @c VTADV
!<
!-------------------------------------------------------------------------------------------------- 
    MODULE PVRBLS
!--------------------------------------------------------------------------------------------------
! MODULE PVRBLS
!
! USE MODULES: F77KINDS
!
! DRIVER     : ALLOC
!              CHECKMXMN
!              CUCNVC
!              DIGFLT
!              GEF
!              GSMDRIVE
!              HDIFF
!              HZADV
!              HZADVQ
!              INIT
!              KFDRIVE
!              KFPARA
!              KFTEND
!              OUT
!              OUTSD
!              OUT2
!              OUT_HEAT
!              RADTN
!              RDTEMP
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
    REAL   (KIND=R4)    , PARAMETER :: EPSQ2 = 0.2
!
    REAL   (KIND=R4)    , DIMENSION(:,:,:)            , ALLOCATABLE                             ::&
    & Q2
!
    REAL   (KIND=R4)    , DIMENSION(:,:)              , ALLOCATABLE                             ::&
    & UZ0     , VZ0     , USTAR   , THS     , QS      , THZ0    , QZ0     , Z0      , AKMS    ,   &
    & AKHS    , U10     , V10     , TSHLTR  , QSHLTR  , TH10    , Q10     , PREC    , ACCLIQ  ,   &
    & QWBS    , TWBS    , SNO     , ACPREC  , CUPREC  , SI      , CLDEFI  , HGTSUB
!
    REAL   (KIND=R4)    , DIMENSION(:,:)              , ALLOCATABLE                             ::&
    & PLM
!
    REAL   (KIND=R4)    , DIMENSION(:,:,:)            , ALLOCATABLE                             ::&
    & ZEFFIJ
!
    END MODULE PVRBLS
