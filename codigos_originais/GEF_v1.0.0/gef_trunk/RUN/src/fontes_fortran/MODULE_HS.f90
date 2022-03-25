!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module HS... 
!! @details Details of Module HS... 
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS
!! @details <b>Driver:</b> 
!! @arg @c ALLOC
!! @arg @c INIT
!! @arg @c NEWTON
!! @arg @c SPONGE
!<
!--------------------------------------------------------------------------------------------------
    MODULE HS
!--------------------------------------------------------------------------------------------------
! MODULE HS
!
! USE MODULES: F77KINDS
!
! DRIVER     : ALLOC
!              INIT
!              NEWTON
!              SPONGE
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    SAVE
!
    REAL   (KIND=R4)    , DIMENSION(:,:,:)            , ALLOCATABLE                             ::&
    & KTDT
!
    REAL   (KIND=R4)    , DIMENSION(:)                , ALLOCATABLE                             ::&
    & KVDT
!
    REAL   (KIND=R4)    , DIMENSION(:,:)              , ALLOCATABLE                             ::&
    & UTOP0   , VTOP0
!
    REAL   (KIND=R4)    , DIMENSION(:,:,:)            , ALLOCATABLE                             ::&
    & TTOP0
!
    END MODULE HS
