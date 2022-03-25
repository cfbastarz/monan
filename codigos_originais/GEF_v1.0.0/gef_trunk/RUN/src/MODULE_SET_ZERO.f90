!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief  MODULE FOR SETING UP TO 10 2D ARRAYS TO ZERO
!! @details  MODULE FOR SETING UP TO 10 2D ARRAYS TO ZERO
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS
!! @arg @c PARMETA
!! @arg @c REALPAR
!! @details <b>Driver:</b> 
!! @arg @c ADJUST
!! @arg @c KINEN
!! @arg @c VTADV
!<
!--------------------------------------------------------------------------------------------------
    MODULE SET_ZERO
!--------------------------------------------------------------------------------------------------
! MODULE SET_ZERO
!
! USE MODULES: F77KINDS
!              PARMETA
!              REALPAR
!              
! DRIVER     : ADJUST
!              KINEN
!              VTADV
!--------------------------------------------------------------------------------------------------
    IMPLICIT NONE
!
    CONTAINS
!
    SUBROUTINE ZERO(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9)
!
    USE F77KINDS
    USE PARMETA
    USE REALPAR
!
    IMPLICIT NONE
!
    REAL   (KIND=R4)    , DIMENSION (:,:)   , OPTIONAL                    , INTENT (INOUT)      ::&
    & A0      , A1      , A2      , A3      , A4      , A5      , A6      , A7      , A8      ,   &
    & A9 
!
    IF (PRESENT(A0))  A0(:,:) = D00
    IF (PRESENT(A1))  A1(:,:) = D00
    IF (PRESENT(A2))  A2(:,:) = D00
    IF (PRESENT(A3))  A3(:,:) = D00
    IF (PRESENT(A4))  A4(:,:) = D00
    IF (PRESENT(A5))  A5(:,:) = D00
    IF (PRESENT(A6))  A6(:,:) = D00
    IF (PRESENT(A7))  A7(:,:) = D00
    IF (PRESENT(A8))  A8(:,:) = D00
    IF (PRESENT(A9))  A9(:,:) = D00
!
    END SUBROUTINE ZERO
!
    END MODULE SET_ZERO
