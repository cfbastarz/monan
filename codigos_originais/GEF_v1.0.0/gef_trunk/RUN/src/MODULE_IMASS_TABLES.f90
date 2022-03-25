!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief LOOKUP TABLES FOR MASS CONTENT OF ICE
!! @details LOOKUP TABLES FOR MASS CONTENT OF ICE 
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
    MODULE IMASS_TABLES
!--------------------------------------------------------------------------------------------------
! MODULE IMASS_TABLES
!
! USE MODULES: F77KINDS
!              IACCR_TABLES
!
! DRIVER     : GSMCOLUMN
!              GSMCONST
!--------------------------------------------------------------------------------------------------
!
!-----------------------------------------------------
! IMASS_TABLES - LOOKUP TABLES FOR MASS CONTENT OF ICE
!-----------------------------------------------------
!
!---------------------------------------------------- 
! MASSI  - INTEGRATED QUANTITY ASSOCIATED W/ ICE MASS
!---------------------------------------------------- 
    USE F77KINDS
    USE IACCR_TABLES, ONLY : MDIMIN, MDIMAX
!
    IMPLICIT NONE
!
    REAL   (KIND=R4)    , DIMENSION(MDIMIN:MDIMAX)                                              ::&
    & MASSI
!
    END MODULE IMASS_TABLES
