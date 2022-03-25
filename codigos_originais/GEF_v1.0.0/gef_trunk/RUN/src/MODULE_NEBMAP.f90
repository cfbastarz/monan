!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module NEBMAP...
!! @details Details of Module NEBMAP...
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
! @arg @c F77KINDS
!! @details <b>Driver:</b> 
!! @arg @c BOCOHMPI
!! @arg @c BOCOVMPI
!! @arg @c BOCOVMPI_SC
!! @arg @c BCOV_HMPI
!! @arg @c INITTOP1
!! @arg @c INITTOP2
!! @arg @c MODULE_SET_ASSOC
!<
!--------------------------------------------------------------------------------------------------
    MODULE NEBMAP
!--------------------------------------------------------------------------------------------------
! MODULE NEBMAP
!
! USE MODULES: F77KINDS
!
! DRIVER     : BOCOHMPI
!              BOCOVMPI
!              BOCOVMPI_SC
!              BCOV_HMPI
!              INITTOP1
!              INITTOP2
!              MODULE_SET_ASSOC
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    SAVE
!
    INTEGER(KIND=I4)    , DIMENSION(8)                                                          ::&
    &    MY_NEB         ,                                                                         &
    & ISGNU_NEB         ,                                                                         &
    & ISGNV_NEB         ,                                                                         &
    &  CROT_NEB         ,                                                                         &
    &  LINV_NEB2
!
    LOGICAL(KIND=L4)    , DIMENSION(4)                                                          ::&
    & LINV_NEB
!
    LOGICAL(KIND=L4)    , DIMENSION(8)                                                          ::&
    & LEXC_NEB          ,                                                                         &
    & LSWP_NEB
! 
    INTEGER(KIND=I4)                                                                            ::&
    & ITARG_N , ITARG_E , ITARG_S , ITARG_W ,                                                     &
    & ITARG_NE, ITARG_SE, ITARG_SW, ITARG_NW,                                                     &
    & ISGNU_N , ISGNU_E , ISGNU_S , ISGNU_W ,                                                     &
    & ISGNU_NE, ISGNU_SE, ISGNU_SW, ISGNU_NW,                                                     &
    & ISGNV_N , ISGNV_E , ISGNV_S , ISGNV_W ,                                                     &
    & ISGNV_NE, ISGNV_SE, ISGNV_SW, ISGNV_NW,                                                     &
    &  CROT_E ,  CROT_W ,  CROT_N ,  CROT_S ,                                                     &
    &  CROT_NE,  CROT_NW,  CROT_SE,  CROT_SW,                                                     &
    &  LINV_N2,  LINV_E2,  LINV_S2,  LINV_W2               

    LOGICAL(KIND=L4)                                                                            ::&
    & LINV_N  , LINV_E  , LINV_S  , LINV_W  ,                                                     &
    & LEXC_N  , LEXC_E  , LEXC_S  , LEXC_W  ,                                                     &
    & LEXC_NE , LEXC_SE , LEXC_SW , LEXC_NW ,                                                     &
    & LSWP_E  , LSWP_W  , LSWP_N  , LSWP_S  ,                                                     &
    & LSWP_NE , LSWP_NW , LSWP_SE , LSWP_SW
!
    END MODULE NEBMAP
