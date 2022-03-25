!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module MASKS...
!! @details Details of Module MASKS...
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS  
!! @details <b>Driver:</b> 
!! @arg @c ADJUST
!! @arg @c ALLOC
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
!! @arg @c OUTSD
!! @arg @c OUT2
!! @arg @c PDETE
!! @arg @c RADTN
!! @arg @c RDTEMP
!! @arg @c READ_SST12M
!! @arg @c SPONGE
!! @arg @c SSTCH
!! @arg @c SURFCE
!! @arg @c TURBL
!! @arg @c VADZ_GEF
!! @arg @c VEGUPDT
!! @arg @c VTADV
!<
!--------------------------------------------------------------------------------------------------
    MODULE MASKS
!--------------------------------------------------------------------------------------------------
! MODULE MASKS
!
! USE MODULES: F77KINDS
!
! DRIVER     : ADJUST
!              ALLOC
!              CLTEND
!              CUCNVC
!              DDAMP
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
!              KINEN
!              NEWTON
!              OUTSD
!              OUT2
!              PDETE
!              RADTN
!              RDTEMP
!              READ_SST12M
!              SPONGE
!              SSTCH
!              SURFCE
!              TURBL
!              VADZ_GEF
!              VEGUPDT
!              VTADV
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    SAVE
!
    REAL   (KIND=R4)    , DIMENSION(:,:)              , ALLOCATABLE                             ::&
    & SM      , SICE
! 
    REAL   (KIND=R4)    , DIMENSION(:,:,:)            , ALLOCATABLE                             ::&
    & HTM     , VTM
!
    END MODULE MASKS
