!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module TBLTMP...
!! @details Details of Module TBLTMP...
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS
!! @arg @c RDPARM
!! @details <b>Driver:</b> 
!! @arg @c GFDLRD
!! @arg @c TABLE
!<
!--------------------------------------------------------------------------------------------------
    MODULE TBLTMP
!--------------------------------------------------------------------------------------------------
! MODULE TBLTMP
!
! USE MODULES: F77KINDS
!              RDPARM
!
! DRIVER     : GFDLRD
!              TABLE
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
    USE RDPARM  , ONLY : NBLY
!
    IMPLICIT NONE
!
    REAL   (KIND=R4)    , DIMENSION(NBLY)                                                       ::&
    & DELCM
!-------------------------------------------------------------------------------------------------- 
! BLOCK DATA INTIALIZES QUANTITIES NEEDED BY THE GFDL CODES.
! BD2, BD3, BD4, BD5, BLCKFS ALL COMBINED INTO 1 BLOCKDATA FOR FRONTEND.
!
! BLOCK DATA BD1 GIVES INPUT DATA (TEMPS,PRESSURES,MIXING RATIOS, CLOUD AMTS AND HEIGHTS) FOR 
! TESTING THE RADIATION CODE AS A STAND ALONE MODEL.
!--------------------------------------------------------------------------------------------------
    DATA DELCM  /                                                                                 &
    &  0.300000E+02,  0.110000E+03,  0.600000E+02,  0.400000E+02,  0.200000E+02,  0.500000E+02,   &
    &  0.400000E+02,  0.500000E+02,  0.110000E+03,  0.130000E+03,  0.100000E+03,  0.900000E+02,   &
    &  0.800000E+02,  0.130000E+03,  0.110000E+03/
!
    END MODULE TBLTMP
