!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module SDENS_TABLES...
!! @details Details of Module SDENS_TABLES...
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS
!! @arg @c IACCR_TABLES
!! @details <b>Driver:</b> 
!! @arg @c GSMCONST
!<
!--------------------------------------------------------------------------------------------------
    MODULE SDENS_TABLES
!--------------------------------------------------------------------------------------------------
! MODULE SDENS_TABLES
!
! USE MODULES: F77KINDS
!              IACCR_TABLES
!
! DRIVER     : GSMCONST  
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
    USE IACCR_TABLES, ONLY : MDIMIN, MDIMAX
!
    IMPLICIT NONE
!
    REAL   (KIND=R4)    , DIMENSION(MDIMIN:MDIMAX)                                              ::&
    & SDENS
!
    END MODULE SDENS_TABLES
