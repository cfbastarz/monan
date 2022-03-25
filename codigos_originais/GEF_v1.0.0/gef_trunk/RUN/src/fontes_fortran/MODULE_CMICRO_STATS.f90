!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module CMICRO_STATS... 
!! @details Details of Module CMICRO_STATS... 
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS 
!! @details <b>Driver:</b> 
!! @arg @c GSMCOLUMN  
!! @arg @c GSMDRIVE 
!<
!--------------------------------------------------------------------------------------------------
    MODULE CMICRO_STATS
!--------------------------------------------------------------------------------------------------
! MODULE CMICRO_STATS
!
! USE MODULES: F77KINDS
!
! DRIVER     : GSMCOLUMN
!              GSMDRIVE
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!--------------------------------------------------------- 
! THE FOLLOWING VARIABLES ARE FOR MICROPHYSICAL STATISTICS
!--------------------------------------------------------- 
    INTEGER(KIND=I4)    , PARAMETER :: ITLO = -60
    INTEGER(KIND=I4)    , PARAMETER :: ITHI =  40
!
    INTEGER(KIND=I4)    , DIMENSION(ITLO:ITHI,  4)                                              ::&
    & NSTATS
!
    REAL   (KIND=R4)    , DIMENSION(ITLO:ITHI,  5)                                              ::&
    & QMAX
!
    REAL   (KIND=R4)    , DIMENSION(ITLO:ITHI, 22)                                              ::&
    & QTOT
!
    END MODULE CMICRO_STATS
