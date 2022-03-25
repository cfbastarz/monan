!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module RVENT_TABLES...
!! @details Details of Module RVENT_TABLES...
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
    MODULE RVENT_TABLES
!--------------------------------------------------------------------------------------------------
! MODULE RVENT_TABLES
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
    & VENTR1  , VENTR2
!
    END MODULE RVENT_TABLES
