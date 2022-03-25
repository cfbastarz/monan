!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module CMICRO_START... 
!! @details Details of Module CMICRO_START... 
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS 
!! @details <b>Driver:</b> 
!! @arg @c GSMDRIVE  
!! @arg @c INIT 
!<
!--------------------------------------------------------------------------------------------------
    MODULE CMICRO_START
!--------------------------------------------------------------------------------------------------
! MODULE CMICRO_START
!
! USE MODULES: F77KINDS
! 
! DRIVER     : GSMDRIVE
!              INIT
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!--------------------------------------------------------- 
! THE FOLLOWING VARIABLES ARE FOR MICROPHYSICAL STATISTICS
!--------------------------------------------------------- 
    LOGICAL(KIND=L4)                                                                            ::&
    & MICRO_START
!
    END MODULE CMICRO_START
