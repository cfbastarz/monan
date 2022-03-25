!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module CLDWTR... 
!! @details Details of Module CLDWTR... 
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS 
!! @details <b>Driver:</b> 
!! @arg @c ALLOC
!! @arg @c CHECKMXMN
!! @arg @c GSMDRIVE
!! @arg @c HZADVQ
!! @arg @c INIT
!! @arg @c KFTEND
!! @arg @c OUTSD
!! @arg @c OUT2
!! @arg @c OUT_HEAT
!! @arg @c RADTN
!! @arg @c READ_SST12M
!! @arg @c VTADV
!<
!--------------------------------------------------------------------------------------------------
    MODULE CLDWTR
!--------------------------------------------------------------------------------------------------
! MODULE CLDWTR
!
! USE MODULES: F77KINDS
!
! DRIVER     : ALLOC
!              CHECKMXMN
!              GSMDRIVE
!              HZADVQ
!              INIT
!              KFTEND
!              OUTSD
!              OUT2
!              OUT_HEAT
!              RADTN
!              READ_SST12M
!              VTADV
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    SAVE
!
    REAL   (KIND=R4)    , DIMENSION(:,:,:)            , ALLOCATABLE                             ::&
    & CWM
!
    REAL   (KIND=R4)    , DIMENSION(:,:)              , ALLOCATABLE                             ::&
    & U00     , LC      , SR
!
    REAL   (KIND=R4)    , DIMENSION(:)                , ALLOCATABLE                             ::&
    & UL
!
    END MODULE CLDWTR
