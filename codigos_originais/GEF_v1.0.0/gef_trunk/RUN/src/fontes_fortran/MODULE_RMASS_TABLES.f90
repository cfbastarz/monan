!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module RMASS_TABLES...
!! @details Details of Module RMASS_TABLES...
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS
!! @arg @c RACCR_TABLES
!! @details <b>Driver:</b> 
!! @arg @c GSMCOLUMN
!! @arg @c GSMCONST
!<
!-------------------------------------------------------------------------------------------------- 
    MODULE RMASS_TABLES
!--------------------------------------------------------------------------------------------------
! MODULE RMASS_TABLES
!
! USE MODULES: F77KINDS
!              RACCR_TABLES
!
! DRIVER     : GSMCOLUMN
!              GSMCONST
!--------------------------------------------------------------------------------------------------
!
!------------------------------------------------------
! RMASS_TABLES - LOOKUP TABLES FOR MASS CONTENT OF RAIN
!------------------------------------------------------
    USE F77KINDS
    USE RACCR_TABLES, ONLY : MDRMIN, MDRMAX
!
    IMPLICIT NONE
!
    REAL   (KIND=R4)    , DIMENSION(MDRMIN:MDRMAX)                                              ::&
    & MASSR  
!
    END MODULE RMASS_TABLES
