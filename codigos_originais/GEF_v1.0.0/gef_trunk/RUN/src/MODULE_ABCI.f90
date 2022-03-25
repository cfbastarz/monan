!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module ABCI... 
!! @details Details of Module ABCI... 
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS 
!! @details <b>Driver:</b> 
!! @arg @c SFLX 
!<
!--------------------------------------------------------------------------------------------------
     MODULE ABCI
!--------------------------------------------------------------------------------------------------
! MODULE ABCI
!
! USE MODULES: F77KINDS
! 
! DRIVER     : SFLX
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    INTEGER(KIND=I4)    , PARAMETER :: NSOLD = 20 
!
    REAL   (KIND=R4)    , DIMENSION(NSOLD)                                                      ::&
    & AI      , BI      , CI
!
    END MODULE ABCI
