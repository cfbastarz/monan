!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module CONTIN... 
!! @details Details of Module CONTIN... 
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS 
!! @details <b>Driver:</b> 
!! @arg @c ADJUST
!! @arg @c ALLOC 
!! @arg @c CHECKMXMN
!! @arg @c DDAMP
!! @arg @c HDIFF
!! @arg @c HZADV
!! @arg @c HZADVQ
!! @arg @c INIT
!! @arg @c KFDRIVE
!! @arg @c KINEN
!! @arg @c OUT
!! @arg @c OUTSD
!! @arg @c OUT2
!! @arg @c OUT_HEAT
!! @arg @c PDETE
!! @arg @c PDNEW
!! @arg @c READ_SST12M
!! @arg @c VTADV
!<
!--------------------------------------------------------------------------------------------------
    MODULE CONTIN
!--------------------------------------------------------------------------------------------------
! MODULE CONTIN
!
! USE MODULES: F77KINDS
!
! DRIVER     : ADJUST
!              ALLOC 
!              CHECKMXMN
!              DDAMP
!              HDIFF
!              HZADV
!              HZADVQ
!              INIT
!              KFDRIVE
!              KINEN
!              OUT
!              OUTSD
!              OUT2
!              OUT_HEAT
!              PDETE
!              PDNEW
!              READ_SST12M
!              VTADV
!>--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    SAVE
!
    REAL   (KIND=R4)    , DIMENSION(:,:)              , ALLOCATABLE                             ::&
    & PDSL    , PSDT     
!
    REAL   (KIND=R4)    , DIMENSION(:,:,:)            , ALLOCATABLE                             ::&
    & RTOP    , OMGALF  , DIV
!
    REAL   (KIND=R4)    , DIMENSION(:,:,:)            , ALLOCATABLE                             ::&
    & ETADT
!
    END MODULE CONTIN
