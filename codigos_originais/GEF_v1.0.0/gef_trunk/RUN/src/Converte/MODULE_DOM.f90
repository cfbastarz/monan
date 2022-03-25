!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module DOM... 
!! @details Details of Module DOM... 
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS
!! @arg @c PARMETA 
!! @details <b>Driver:</b> 
!! @arg @c BOCOHMPI
!! @arg @c BOCOVMPI
!! @arg @c BOCOVMPI_SC
!! @arg @c BOCOV_HMPI
!! @arg @c INITDOM
!! @arg @c INITTOP1
!! @arg @c INITTOP2
!! @arg @c ISCORNER2
!! @arg @c UTIL
!<
!--------------------------------------------------------------------------------------------------
    MODULE DOM
!--------------------------------------------------------------------------------------------------
! MODULE DOM
!
! USE MODULES: F77KINDS
!              PARMETA
!
! DRIVER     : BOCOHMPI
!              BOCOVMPI
!              BOCOVMPI_SC
!              BOCOV_HMPI
!              INITDOM
!              INITTOP1
!              INITTOP2
!              ISCORNER2
!              UTIL 
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
    USE PARMETA
!
    IMPLICIT NONE
!
    SAVE
!
    LOGICAL(KIND=L4)                                                                            ::&
    & LWEST   , LEAST   , LNORTH  , LSOUTH
!
    LOGICAL(KIND=L4)                                                                            ::&
    & LSWC    , LSEC    , LNWC    , LNEC
!
    INTEGER(KIND=I4)                                                                            ::&
    & IX      , JY      , MY_FACE
!  
    INTEGER(KIND=I4)    , DIMENSION(NM)                                                         ::&
    & SW      , SE      , NW      , NE
!
    END MODULE DOM
