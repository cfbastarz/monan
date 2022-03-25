!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module VRBLS...
!! @details Details of Module VRBLS...
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS
!! @details <b>Driver:</b> 
!! @arg @c ADJUST
!! @arg @c ALLOC 
!! @arg @c CHECKMXMN
!! @arg @c CLTEND
!! @arg @c CUCNVC
!! @arg @c DDAMP
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
!! @arg @c KINEN
!! @arg @c NEWTON
!! @arg @c OUT
!! @arg @c OUT2
!! @arg @c OUTSD
!! @arg @c OUT_HEAT
!! @arg @c PDETE
!! @arg @c PDNEW
!! @arg @c RADTN
!! @arg @c RDTEMP
!! @arg @c READ_SST12M
!! @arg @c SHAP_FILTER
!! @arg @c SPONGE
!! @arg @c SURFACE
!! @arg @c TURBL
!! @arg @c VTADV
!<
!--------------------------------------------------------------------------------------------------
    MODULE VRBLS
!-------------------------------------------------------------------------------------------------- 
! MODULE VRBLS
!
! USE MODULES: F77KINDS
!
! DRIVER     : 
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    SAVE
!
    REAL   (KIND=R4)    , DIMENSION (:,:)             , ALLOCATABLE                             ::&
    & PD     , FIS     , RES     , PDOLD
!
    REAL   (KIND=R4)    , DIMENSION (:,:,:)           , ALLOCATABLE                             ::&
    & T       , Q       , U       , V
!-------------------------------
! FOR ENTHALPY CONSERVING SCHEME
!-------------------------------
    REAL   (KIND=R4)    , DIMENSION(:,:,:)            , ALLOCATABLE                             ::&
    & PBH     , PB
!
    REAL   (KIND=R4)    , DIMENSION(:,:,:)            , ALLOCATABLE                             ::&
    & ACDTDT  , RDDTDT
!
    END MODULE VRBLS

