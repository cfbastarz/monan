!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module C_FRACN... 
!! @details Details of Module C_FRACN... 
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS 
!! @details <b>Driver:</b> 
!! @arg @c ALLOC
!! @arg @c GSMDRIVE
!<
!--------------------------------------------------------------------------------------------------
    MODULE C_FRACN
!--------------------------------------------------------------------------------------------------
! MODULE C_FRACN
!
! USE MODULES: F77KINDS
!
! DRIVER     : ALLOC
!              GSMDRIVE
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    SAVE
!
    REAL   (KIND=R4)    , DIMENSION(:,:,:)            , ALLOCATABLE                             ::&
    & F_ICE   ,                                                                                   &
    & F_RAIN  ,                                                                                   &
    & F_RIMEF
!
    END MODULE C_FRACN
