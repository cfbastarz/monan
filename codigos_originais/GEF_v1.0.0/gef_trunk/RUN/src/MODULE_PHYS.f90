!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module PARMSOIL...
!! @details Details of Module PARMSOIL...
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS
!! @arg @c PARM_TBL
!! @details <b>Driver:</b> 
!! @arg @c ALLOC
!! @arg @c CCHSST
!! @arg @c CUCNVC
!! @arg @c DIGFLT
!! @arg @c GEF
!! @arg @c GSMDRIVE
!! @arg @c INIT
!! @arg @c KFDRIVE
!! @arg @c KFPARA
!! @arg @c OUT
!! @arg @c OUT2
!! @arg @c OUT_HEAT
!! @arg @c RADTN
!! @arg @c RDTEMP
!! @arg @c READ_SST12M
!! @arg @c SSTCH
!! @arg @c SURFCE
!! @arg @c TURBL
!! @arg @c VEGUPDT
!! @arg @c ZENITH
!<
!--------------------------------------------------------------------------------------------------
    MODULE PHYS
!--------------------------------------------------------------------------------------------------
! MODULE PHYS
!
! USE MODULES: F77KINDS
!              PARM_TBL
!
! DRIVER     : ALLOC
!              CCHSST
!              CUCNVC
!              DIGFLT
!              GEF
!              GSMDRIVE
!              INIT
!              KFDRIVE
!              KFPARA
!              OUT
!              OUT2
!              OUT_HEAT
!              RADTN
!              RDTEMP
!              READ_SST12M
!              SSTCH
!              SURFCE
!              TURBL
!              VEGUPDT
!              ZENITH
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
    USE PARM_TBL
! 
    IMPLICIT NONE
!
    SAVE
!
    REAL   (KIND=R4)    , DIMENSION(:)                , ALLOCATABLE                             ::&
    & DFRLG
!
    REAL   (KIND=R4)                                                                            ::&
    & DTQ2    , TDTQ2   , DTD     , TDTD    , ROS     , CS      , DS      , ROI     , CI      ,   &
    & DI      , PL      , THL     , RDQ     , RDTH    , RDP     , RDTHE   , PLQ     , RDPQ    ,   &
    & RDTHEQ  
!
    REAL   (KIND=R4)    , DIMENSION(JTB)                                                        ::&
    & QS0     , SQS
!
    REAL   (KIND=R4)    , DIMENSION(ITB)                                                        ::&
    & THE0    , STHE
!
    REAL   (KIND=R4)    , DIMENSION(ITBQ)                                                       ::&
    & THE0Q   , STHEQ
!
    REAL   (KIND=R4)    , DIMENSION(ITB, JTB)                                                   ::&
    & PTBL
!
    REAL   (KIND=R4)    , DIMENSION(JTB, ITB)                                                   ::&
    & TTBL
!
    REAL   (KIND=R4)    , DIMENSION(JTBQ, ITBQ)                                                 ::&
    & TTBLQ
 !
    REAL   (KIND=R4)    , DIMENSION(:,:,:)            , ALLOCATABLE                             ::&
    & SSTM
!
    INTEGER(KIND=I4)                                                                            ::&
    & KTM
!
    REAL   (KIND=R4)    , DIMENSION(:,:)              , ALLOCATABLE                             ::&
    & RADOT   , RADIN   , CZMEAN  , CZEN    , SIGT4   , TG      , EPSR    , ALBEDO  , MXSNAL  ,   &
    & HTOP    , HBOT    , CNVTOP  , CNVBOT  , GFFC    , ALBASE  , HDAC    , SST     , HDACV   ,   &
    & GLAT    , GLON    , SST1
!
    END MODULE PHYS

