!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module REALPAR...
!! @details Details of Module REALPAR...
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS
!! @details <b>Driver:</b> 
!! @arg @c ADJUST
!! @arg @c HDIFF
!! @arg @c HZADV
!! @arg @c HZADVQ
!! @arg @c INIT
!! @arg @c KINEN
!! @arg @c MODULE_SET_ZERO
!! @arg @c OUTSD
!! @arg @c PDETE
!! @arg @c VTADV
!<
!--------------------------------------------------------------------------------------------------
    MODULE REALPAR
!--------------------------------------------------------------------------------------------------
! MODULE REALPAR
! 
! USE MODULES: F77KINDS
!
! DRIVER     : ADJUST
!              HDIFF
!              HZADV
!              HZADVQ
!              INIT
!              KINEN
!              MODULE_SET_ZERO
!              OUTSD
!              PDETE
!              VTADV
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    REAL   (KIND=R4)    , PARAMETER :: D00  = 0.000E0
    REAL   (KIND=R4)    , PARAMETER :: D25  = 0.250E0
    REAL   (KIND=R4)    , PARAMETER :: D50  = 0.500E0
    REAL   (KIND=R4)    , PARAMETER :: D10  = 0.100E0
    REAL   (KIND=R4)    , PARAMETER :: D608 = 0.608E0
    REAL   (KIND=R4)    , PARAMETER :: H1   = 1.000E0
    REAL   (KIND=R4)    , PARAMETER :: H2   = 2.000E0
    REAL   (KIND=R4)    , PARAMETER :: H4   = 4.000E0
    REAL   (KIND=R4)    , PARAMETER :: RDH  = 0.200E-4
!
    REAL   (KIND=R4)    , PARAMETER :: TLC  = 2. * 0.703972477

    END MODULE REALPAR
