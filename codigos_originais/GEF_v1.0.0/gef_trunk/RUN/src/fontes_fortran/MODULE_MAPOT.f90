!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module MAPOT...
!! @details Details of Module MAPOT...
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS
!! @arg @c PARMETA  
!! @details <b>Driver:</b> 
!! @arg @c GSMCONST
!! @arg @c READ_SST12M
!<
!--------------------------------------------------------------------------------------------------
    MODULE MAPOT
!--------------------------------------------------------------------------------------------------
! MODULE MAPOT
!
! USE MODULES: F77KINDS
!              PARMETA
!
! DRIVER     : GSMCONST
!              READ_SST12M
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
    USE PARMETA
!
    IMPLICIT NONE
!
    SAVE
!
    INTEGER(KIND=I4)                                                                            ::&
    & LSL
!
    INTEGER(KIND=I4)    , DIMENSION(99)                                                         ::&
    & ISHDE
!
    REAL   (KIND=R4)                                                                            ::&
    & WBD     , SBD     , TLM0D   , TPH0D   , DLMD    , DPHD    , CMLD    , DP30    , X1P     ,   &
    & Y1P     , DISLP   , Z0SLP   , ERLAM0  , CPHI0   , SPHI0    
!
    REAL   (KIND=R4)    , DIMENSION(LSM)                                                        ::&
    & SPL     , ALSL
!
    REAL   (KIND=R4)    , DIMENSION(99)                                                         ::&
    & TSHDE
!
    END MODULE MAPOT
