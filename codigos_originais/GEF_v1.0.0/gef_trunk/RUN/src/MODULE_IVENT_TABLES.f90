!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief LOOKUP TABLES FOR VENTILATION EFFECTS OF ICE
!! @details LOOKUP TABLES FOR VENTILATION EFFECTS OF ICE
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS
!! @arg @c IACCR_TABLES  
!! @details <b>Driver:</b> 
!! @arg @c GSMCOLUMN
!! @arg @c GSMCONST
!<
!--------------------------------------------------------------------------------------------------
    MODULE IVENT_TABLES
!--------------------------------------------------------------------------------------------------
! MODULE IVENT_TABLES
!
! USE MODULES: F77KINDS
!              IACCR_TABLES
!
! DRIVER     : GSMCOLUMN
!              GSMCONST
!--------------------------------------------------------------------------------------------------
!
!------------------------------------------------------------
! IVENT_TABLES - LOOKUP TABLES FOR VENTILATION EFFECTS OF ICE
!------------------------------------------------------------
!
!------------------------------------------------------------------------------------ 
! MASS-WEIGHTED FALL SPEED OF SNOW (LARGE ICE), USED TO CALCULATE PRECIPITATION RATES
!------------------------------------------------------------------------------------ 
    USE F77KINDS
    USE IACCR_TABLES, ONLY : MDIMIN, MDIMAX
!
    IMPLICIT NONE
!
    REAL   (KIND=R4)    , DIMENSION(MDIMIN:MDIMAX)                                              ::&
    & VENTI1  , VENTI2
!
    END MODULE IVENT_TABLES
