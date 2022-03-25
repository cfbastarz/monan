!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module RITE...
!! @details Details of Module RITE...
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS
!! @details <b>Driver:</b> 
!! @arg @c SFLX
!<
!--------------------------------------------------------------------------------------------------
    MODULE RITE
!--------------------------------------------------------------------------------------------------
! MODULE RITE
!
! USE MODULES: F77KINDS
! 
! DRIVER     : SFLX
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    REAL   (KIND=R4)                                                                            ::&
    & BETA    , DRIP    , EC      , EDIR    , ETT     , FLX1    , FLX2    , FLX3    , RUNOF   ,   &
    & DEW     , RIB     , RUNOFF3
!
    END MODULE RITE
