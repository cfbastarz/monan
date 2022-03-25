!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module RDFSAV...
!! @details Details of Module RDFSAV...
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS
!! @details <b>Driver:</b> 
!! @arg @c GRADFS
!! @arg @c RADFS
!<
!--------------------------------------------------------------------------------------------------
    MODULE RDFSAV
!--------------------------------------------------------------------------------------------------
! MODULE RDFSAV
!
! USE MODULES: F77KINDS
!
! DRIVER     : GRADFS
!              RADFS
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    REAL   (KIND=R4)                                                                            ::&
    & EMISP   , EMIST   , XLATT   , XLATP   , Q19001  , HP98    , H3M6    , HP75    , H6M2    ,   &
    & HP537   , H74E1   , H15E1   , Q14330  , HP2     , TWENTY  , HNINE   , DEGRAD  , HSIGMA  ,   &
    & DAYSEC  , RCO2 
!
    REAL   (KIND=R4)    , DIMENSION(5)                                                          ::&
    & CAO3SW  , CAH2SW  , CBSW
!
    END MODULE RDFSAV
