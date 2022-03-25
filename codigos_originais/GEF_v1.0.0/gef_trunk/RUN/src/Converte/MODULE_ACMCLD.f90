!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module ACMCLD... 
!! @details Details of Module ACMCLD... 
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS 
!! @details <b>Driver:</b> 
!! @arg @c ALLOC 
!! @arg @c INIT
!! @arg @c OUT2
!! @arg @c RADTN
!! @arg @c READ_SST12M
!<
!--------------------------------------------------------------------------------------------------
    MODULE ACMCLD
!--------------------------------------------------------------------------------------------------
! MODULE ACMCLD
!
! USE MODULES: F77KINDS     
! 
! DRIVER     : ALLOC
!              INIT
!              OUT2
!              RADTN
!              READ_SST12M
!>-------------------------------------------------------------------------------------------------- 
    USE F77KINDS
!
    IMPLICIT NONE
!
    SAVE
!
    REAL   (KIND=R4)                                                                            ::&
    & TCLOD
!
    INTEGER(KIND=I4)                                                                            ::&
    & NCLOD
!
    REAL   (KIND=R4)    , DIMENSION(:,:)              , ALLOCATABLE                             ::&
    & ACFRCV  , ACFRST
!
    INTEGER(KIND=I4)    , DIMENSION(:,:)              , ALLOCATABLE                             ::&
    & NCFRCV  , NCFRST
!
    END MODULE ACMCLD
