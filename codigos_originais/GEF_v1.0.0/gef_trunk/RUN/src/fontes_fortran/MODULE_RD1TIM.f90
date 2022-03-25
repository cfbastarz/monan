!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module RD1TIM...
!! @details Details of Module RD1TIM...
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS
!! @details <b>Driver:</b> 
!! @arg @c INIT
!! @arg @c RADTN
!<
!--------------------------------------------------------------------------------------------------
    MODULE RD1TIM
!--------------------------------------------------------------------------------------------------
! MODULE RD1TIM
!
! USE MODULES: F77KINDS
!
! DRIVER     : INIT
!              RADTN
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    INTEGER(KIND=I4)                                                                            ::&
    & K400
!
    REAL   (KIND=R4)                                                                            ::&
    & R1
!
    REAL   (KIND=R4)    , DIMENSION(:)                , ALLOCATABLE                             ::&
    & CTHK    , TAUCV
!
    REAL   (KIND=R4)    , DIMENSION(:)                , ALLOCATABLE                             ::&
    & PTOPC
!
    INTEGER(KIND=R4)    , DIMENSION(:)                , ALLOCATABLE                             ::&
    & LTOP
!
    INTEGER(KIND=R4)    , DIMENSION(:,:)              , ALLOCATABLE                             ::&
    & LVL
!
    END MODULE RD1TIM
