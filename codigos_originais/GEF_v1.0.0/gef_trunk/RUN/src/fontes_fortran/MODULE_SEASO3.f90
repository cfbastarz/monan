!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module SEASO3...
!! @details Details of Module SEASO3...
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS
!! @arg @c PARMETA
!! @details <b>Driver:</b> 
!! @arg @c O3CLIM
!! @arg @c OZON2D
!<
!--------------------------------------------------------------------------------------------------
    MODULE SEASO3
!--------------------------------------------------------------------------------------------------
! MODULE SEASO3
!
! USE MODULES: F77KINDS
!              PARMETA
!       
! DRIVER     : O3CLIM
!              OZON2D
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
    USE PARMETA
!
    IMPLICIT NONE
!
    SAVE
!
    INTEGER(KIND=I4)    , PARAMETER :: NL    = 81
    INTEGER(KIND=I4)    , PARAMETER :: NLP1  = NL + 1
    INTEGER(KIND=I4)    , PARAMETER :: LNGTH = 37 * NL
!----------------------- 
! XDUO3N(37,NL) - WINTER
! XDO3N2(37,NL) - SPRING
! XDO3N3(37,NL) - SUMMER
! XDO3N4(37,NL) - FALL
!----------------------- 
    REAL   (KIND=R4)    , DIMENSION(37, NL)                                                     ::&
    & XDUO3N  , XDO3N2  , XDO3N3  , XDO3N4
!
    REAL   (KIND=R4)    , DIMENSION(NL)                                                         ::&
    & PRGFDL
!
    REAL   (KIND=R4)    , DIMENSION(37, NL, 4)                                                  ::&
    & O3O3
!
    REAL   (KIND=R4)    , DIMENSION(LNGTH)                                                      ::&
    & XRAD1   , XRAD2   , XRAD3   , XRAD4          
!   
    EQUIVALENCE (XRAD1(1), XDUO3N(1,1), O3O3(1,1,1)),                                             &
    &           (XRAD2(1), XDO3N2(1,1), O3O3(1,1,2)),                                             &
    &           (XRAD3(1), XDO3N3(1,1), O3O3(1,1,3)),                                             &
    &           (XRAD4(1), XDO3N4(1,1), O3O3(1,1,4))
!
    END MODULE SEASO3
