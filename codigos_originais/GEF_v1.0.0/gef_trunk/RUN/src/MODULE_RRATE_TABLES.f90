!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief LOOKUP TABLES FOR PRECIPITATION RATES OF RAIN
!! @details LOOKUP TABLES FOR PRECIPITATION RATES OF RAIN
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS
!! @arg @c RACCR_TABLES
!! @details <b>Driver:</b> 
!! @arg @c GSMCONST
!<
!--------------------------------------------------------------------------------------------------
    MODULE RRATE_TABLES
!--------------------------------------------------------------------------------------------------
! MODULE RRATE_TABLES
!
! USE MODULES: F77KINDS
!              RACCR_TABLES
!
! DRIVER     : GSMCONST
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
    USE RACCR_TABLES, ONLY : MDRMIN, MDRMAX
!
    IMPLICIT NONE
!-------------------------------------------------------------
! RRATE_TABLES - LOOKUP TABLES FOR PRECIPITATION RATES OF RAIN
!-------------------------------------------------------------
    REAL   (KIND=R4)    , DIMENSION(MDRMIN:MDRMAX)                                              ::&
    & RRATE  
!
    END MODULE RRATE_TABLES
