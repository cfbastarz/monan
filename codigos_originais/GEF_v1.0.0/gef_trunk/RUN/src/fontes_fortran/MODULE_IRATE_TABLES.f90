!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief LOOKUP TABLES FOR PRECIPITATION RATES OF ICE
!! @details LOOKUP TABLES FOR PRECIPITATION RATES OF ICE
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS
!! @arg @c IACCR_TABLES  
!! @details <b>Driver:</b> 
!! @arg @c GSMCOLUMN
!<
!--------------------------------------------------------------------------------------------------
    MODULE IRATE_TABLES
!--------------------------------------------------------------------------------------------------
! MODULE IRATE_TABLES
!
! USE MODULES: F77KINDS
!              IACCR_TABLES
!
! DRIVER     : GSMCOLUMN
!--------------------------------------------------------------------------------------------------
!
!------------------------------------------------------------
! IRATE_TABLES - LOOKUP TABLES FOR PRECIPITATION RATES OF ICE
!------------------------------------------------------------
    USE F77KINDS
    USE IACCR_TABLES, ONLY : MDIMIN, MDIMAX
!
    IMPLICIT NONE
!
    REAL   (KIND=R4)    , DIMENSION(MDIMIN:MDIMAX)                                              ::&
    & VSNOWI
!
    END MODULE IRATE_TABLES
