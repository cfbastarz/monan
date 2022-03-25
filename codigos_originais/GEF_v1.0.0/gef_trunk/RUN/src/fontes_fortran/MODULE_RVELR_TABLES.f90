!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief LOOKUP TABLES FOR FALL SPEEDS OF RAIN
!! @details LOOKUP TABLES FOR FALL SPEEDS OF RAIN
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
    MODULE RVELR_TABLES
!--------------------------------------------------------------------------------------------------
! MODULE RVELR_TABLES
!
! USE MODULES: F77KINDS
!              RACCR_TABLES
!
! DRIVER     : GSMCOLUMN
!              GSMCONST
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
    USE RACCR_TABLES, ONLY : MDRMIN, MDRMAX
!
    IMPLICIT NONE
!----------------------------------------------------- 
! RVELR_TABLES - LOOKUP TABLES FOR FALL SPEEDS OF RAIN
!----------------------------------------------------- 
    REAL   (KIND=R4)    , DIMENSION(MDRMIN:MDRMAX)                                              ::&
    & VRAIN  
!
    END MODULE RVELR_TABLES
