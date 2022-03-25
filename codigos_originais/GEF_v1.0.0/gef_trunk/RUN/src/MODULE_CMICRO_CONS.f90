!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module CMICRO_CONS... 
!! @details Details of Module CMICRO_CONS... 
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS 
!! @details <b>Driver:</b> 
!! @arg @c GSMCOLUMN   
!! @arg @c GSMCONST
!<
!--------------------------------------------------------------------------------------------------
    MODULE CMICRO_CONS
!--------------------------------------------------------------------------------------------------
! MODULE CMICRO_CONS
!
! USE MODULES: F77KINDS
! 
! DRIVER     : GSMCOLUMN
!              GSMCONST
!--------------------------------------------------------------------------------------------------
!
!---------------------------------------------------- 
! CONSTANTS INITIALIZED IN GSMCONST 
!----------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    REAL   (KIND=R4)                                                                            ::&
    & ABFR              , CBFR              ,                                                     &
    & CIACW             , CIACR             ,                                                     &
    & C_N0R0            , CN0R0             ,                                                     &
    & CN0R_DMRMIN       , CN0R_DMRMAX       ,                                                     &
    & CRACW             , CRAUT             ,                                                     &
    & ESW0              , QAUT0             , RFMAX   , RHGRD             ,                       &
    & RQR_DR1           , RQR_DR2           , RQR_DR3 , RQR_DRMIN         , RQR_DRMAX         ,   &
    & RR_DR1            , RR_DR2            , RR_DR3  , RR_DRMIN          , RR_DRMAX
!
    END MODULE CMICRO_CONS
