!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module CNVCLD... 
!! @details Details of Module CNVCLD... 
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS 
!! @details <b>Driver:</b> 
!! @arg @c ALLOC  
!! @arg @c CUCNVC
!! @arg @c KFPARA
!! @arg @c KFTEND
!! @arg @c OUT2
!! @arg @c RADTN
!! @arg @c READ_SST12M 
!<
!--------------------------------------------------------------------------------------------------
    MODULE CNVCLD
!--------------------------------------------------------------------------------------------------
! MODULE CNVCLD
!
! USE MODULES: F77KINDS
!
! DRIVER     : ALLOC 
!              CUCNVC
!              KFPARA
!              KFTEND
!              OUT2
!              RADTN
!              READ_SST12M
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    SAVE
!
    REAL   (KIND=R4)    , DIMENSION(:,:)              , ALLOCATABLE                             ::&
    & CUPPT   , CFRACL  , CFRACM  , CFRACH
!
    END MODULE CNVCLD
