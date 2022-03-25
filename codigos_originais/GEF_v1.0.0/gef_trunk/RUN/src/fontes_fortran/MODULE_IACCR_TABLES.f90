!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief LOOKUP TABLES FOR ACCRETION RATES OF ICE 
!! @details LOOKUP TABLES FOR ACCRETION RATES OF ICE 
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS
!! @details <b>Driver:</b> 
!! @arg @c MODULE_IMASS_TABLES
!! @arg @c MODULE_IRATE_TABLES
!! @arg @c MODULE_IVENT_TABLES
!! @arg @c MODULE_SDENS_TABLES
!<
!--------------------------------------------------------------------------------------------------
    MODULE IACCR_TABLES
!--------------------------------------------------------------------------------------------------
! MODULE IACCR_TABLES
!
! USE MODULES: F77KINDS
!
! DRIVER     : GSMCOLUMN
!              GSMCONST
!              MODULE_IMASS_TABLES
!              MODULE_IRATE_TABLES
!              MODULE_IVENT_TABLES
!              MODULE_SDENS_TABLES
!--------------------------------------------------------------------------------------------------
!
!--------------------------------------------------------
! IACCR_TABLES - LOOKUP TABLES FOR ACCRETION RATES OF ICE
!--------------------------------------------------------
!
!------------------------------------------------------------------------
! ACCRI - INTEGRATED QUANTITY ASSOCIATED W/ CLOUD WATER COLLECTION BY ICE
!------------------------------------------------------------------------
!
!-----------------------------------------------------------------
! MEAN ICE PARTICLE DIAMETERS VARY FROM 50 MICRONS TO 1000 MICRONS
!-----------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    REAL   (KIND=R4)    , PARAMETER :: DMIMIN =  .05E-3
    REAL   (KIND=R4)    , PARAMETER :: DMIMAX = 1.00E-3
    REAL   (KIND=R4)    , PARAMETER :: DELDMR = 1.00E-6
    REAL   (KIND=R4)    , PARAMETER :: XMIMIN = 1.00E6  * DMIMIN
    REAL   (KIND=R4)    , PARAMETER :: XMIMAX = 1.00E6  * DMIMAX
!
    INTEGER(KIND=I4)    , PARAMETER :: MDIMIN = XMIMIN
    INTEGER(KIND=I4)    , PARAMETER :: MDIMAX = XMIMAX
!
!
    REAL   (KIND=R4)    , DIMENSION(MDIMIN:MDIMAX)                                              ::&
    & ACCRI  
!
    END MODULE IACCR_TABLES
