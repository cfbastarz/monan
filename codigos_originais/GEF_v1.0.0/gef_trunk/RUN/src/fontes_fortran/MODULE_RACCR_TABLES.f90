!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module RACCR_TABLES...
!! @details Details of Module RACCR_TABLES...
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS
!! @details <b>Driver:</b> 
!! @arg @c GSMCOLUMN
!! @arg @c GSMCONST
!! @arg @c MODULE_RMASS_TABLES
!! @arg @c MODULE_RRATE_TABLES
!! @arg @c MODULE_RVELR_TABLES
!! @arg @c MODULE_RVENT_TABLES
!<
!--------------------------------------------------------------------------------------------------
    MODULE RACCR_TABLES
!--------------------------------------------------------------------------------------------------
! MODULE RACCR_TABLES
!
! USE MODULES: F77KINDS
!
! DRIVER     : GSMCOLUMN
!              GSMCONST
!              MODULE_RMASS_TABLES
!              MODULE_RRATE_TABLES
!              MODULE_RVELR_TABLES
!              MODULE_RVENT_TABLES
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!--------------------------------------------------------
! IACCR_TABLES - LOOKUP TABLES FOR ACCRETION RATES OF ICE
!--------------------------------------------------------
!
!------------------------------------------------------------------------
! ACCRI - INTEGRATED QUANTITY ASSOCIATED W/ CLOUD WATER COLLECTION BY ICE
!------------------------------------------------------------------------
!
!-------------------------------------------------------------
! MEAN RAIN DROP DIAMETERS VARY FROM 50 MICRONS TO 450 MICRONS
!-------------------------------------------------------------
    REAL   (KIND=R4)    , PARAMETER :: DMRMIN =  .05E-3
    REAL   (KIND=R4)    , PARAMETER :: DMRMAX =  .45E-3
    REAL   (KIND=R4)    , PARAMETER :: DELDMR = 1.00E-6
    REAL   (KIND=R4)    , PARAMETER :: XMRMIN = 1.00E6 * DMRMIN 
    REAL   (KIND=R4)    , PARAMETER :: XMRMAX = 1.00E6 * DMRMAX
!
    INTEGER(KIND=I4)    , PARAMETER :: MDRMIN = XMRMIN
    INTEGER(KIND=I4)    , PARAMETER :: MDRMAX = XMRMAX
!
    REAL   (KIND=R4)    , DIMENSION(MDRMIN:MDRMAX)                                              ::&
    & ACCRR  
!
    END MODULE RACCR_TABLES
