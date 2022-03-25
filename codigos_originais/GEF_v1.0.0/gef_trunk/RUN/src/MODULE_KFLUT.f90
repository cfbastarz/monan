!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module KFLUT... 
!! @details Details of Module KFLUT... 
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS 
!! @details <b>Driver:</b> 
!! @arg @c ENVIRTHT
!! @arg @c KFPARA
!! @arg @c LUTAB
!! @arg @c TPMIX2
!! @arg @c TPMIX2DD
!<
!--------------------------------------------------------------------------------------------------
    MODULE KFLUT
!--------------------------------------------------------------------------------------------------
! MODULE KFLUT
!
! USE MODULES: F77KINDS
!
! DRIVER     : ENVIRTHT
!              KFPARA
!              LUTAB
!              TPMIX2
!              TPMIX2DD
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    SAVE
!      
    INTEGER(KIND=I4)    , PARAMETER :: KFNT = 250
    INTEGER(KIND=I4)    , PARAMETER :: KFNP = 220
!
    REAL   (KIND=R4)    , DIMENSION(KFNT,KFNP)                                                  ::&
    & TTAB    , QSTAB
!
    REAL   (KIND=R4)    , DIMENSION(KFNP)                                                       ::&
    & THE0K
!
    REAL   (KIND=R4)    , DIMENSION(200)                                                        ::&
    & ALU
!
    REAL   (KIND=R4)                                                                            ::&
    & RDPR    , RDTHK   , PTOP
!
    END MODULE KFLUT
