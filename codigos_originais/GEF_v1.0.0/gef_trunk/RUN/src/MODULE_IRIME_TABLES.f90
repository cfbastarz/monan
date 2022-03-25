!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module IRIME_TABLES...
!! @details Details of Module IRIME_TABLES...
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
    MODULE IRIME_TABLES
!--------------------------------------------------------------------------------------------------
! MODULE IRIME_TABLES
!
! USE MODULES: F77KINDS
!
! DRIVER     : GSMCOLUMN
!              GSMCONST
!--------------------------------------------------------------------------------------------------
!
!--------------------------------------------------------------------------------------------------
! VEL_RF - VELOCITY INCREASE OF RIMED PARTICLES AS FUNCTIONS OF CRUDE PARTICLE SIZE CATEGORIES 
! (AT 0.1 MM INTERVALS OF MEAN ICE PARTICLE SIZES) AND RIME FACTOR (DIFFERENT VALUES OF RIME FACTOR
! OF 1.1 ** N, WHERE N = 0 TO NRIME).
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    INTEGER(KIND=I4)    , PARAMETER :: NRIME = 40
!
    REAL   (KIND=R4)    , DIMENSION(2:9, 0:NRIME)                                               ::&
    & VEL_RF
!
    END MODULE IRIME_TABLES
