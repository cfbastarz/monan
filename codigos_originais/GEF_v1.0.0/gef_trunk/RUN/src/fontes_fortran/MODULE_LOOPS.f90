!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module LOOPS... 
!! @details Details of Module LOOPS... 
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS
!! @details <b>Driver:</b> 
!! @arg @c ALLOC
!! @arg @c CHECKMXMN
!! @arg @c CUCNVC
!! @arg @c GEF
!! @arg @c GSMDRIVE
!! @arg @c HZADV
!! @arg @c INIT
!! @arg @c KFDRIVE
!! @arg @c KFPARA
!! @arg @c KFTEND
!! @arg @c OUT
!! @arg @c OUTSD
!! @arg @c OUT2
!! @arg @c OUT_HEAT
!! @arg @c PDTEDT
!! @arg @c RADTN
!! @arg @c READ_SST12M
!! @arg @c SSTCH
!! @arg @c SURFCE
!! @arg @c TURBL
!! @arg @c VEGUPDT
!! @arg @c VTADV
!<
!--------------------------------------------------------------------------------------------------
    MODULE LOOPS
!--------------------------------------------------------------------------------------------------
! MODULE LOOPS
!
! USE MODULES: F77KINDS
!
! DRIVER     : ALLOC
!              CHECKMXMN
!              CUCNVC
!              GEF
!              GSMDRIVE
!              HZADV
!              INIT
!              KFDRIVE
!              KFPARA
!              KFTEND
!              OUT
!              OUTSD
!              OUT2
!              OUT_HEAT
!              PDTEDT
!              RADTN
!              READ_SST12M
!              SSTCH
!              SURFCE
!              TURBL
!              VEGUPDT
!              VTADV
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    SAVE
!
    INTEGER(KIND=I4)    , DIMENSION(:,:)              , ALLOCATABLE                             ::&
    & LMH     , LMV
!
    END MODULE LOOPS
