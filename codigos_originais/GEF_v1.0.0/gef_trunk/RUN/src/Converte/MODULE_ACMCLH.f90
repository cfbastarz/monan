!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module ACMCLH... 
!! @details Details of Module ACMCLH... 
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS 
!! @details <b>Driver:</b> 
!! @arg @c ALLOC 
!! @arg @c CUCNVC
!! @arg @c GSMDRIVE
!! @arg @c INIT
!! @arg @c KFPARA
!! @arg @c KFTEND
!! @arg @c OUT
!! @arg @c OUT2
!! @arg @c OUT_HEAT
!! @arg @c READ_SST12M
!<
!--------------------------------------------------------------------------------------------------
    MODULE ACMCLH
!--------------------------------------------------------------------------------------------------
! MODULE ACMCLH
!
! USE MODULES: F77KINDS       
!
! DRIVER     : ALLOC
!              CUCNVC
!              GSMDRIVE
!              INIT
!              KFPARA
!              KFTEND
!              OUT
!              OUT2
!              OUT_HEAT
!              READ_SST12M
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    SAVE
!
    REAL   (KIND=R4)                                                                            ::&
    & THEAT   , NHEAT   , AVRAIN  , AVCNVC  , ARATIM  , ACUTIM
!
    REAL   (KIND=R4)    , DIMENSION(:,:,:)            , ALLOCATABLE                             ::&
    & TRAIN   , TCUCN
!
    END MODULE ACMCLH
