!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module PPTASM...
!! @details Details of Module PPTASM...
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS
!! @details <b>Driver:</b> 
!! @arg @c ALLOC
!! @arg @c CUCNVC
!! @arg @c GSMDRIVE
!! @arg @c INIT
!! @arg @c OUT2
!<
!--------------------------------------------------------------------------------------------------
    MODULE PPTASM
!--------------------------------------------------------------------------------------------------
! MODULE PPTASM
!
! USE MODULES: F77KINDS
!
! DRIVER     : ALLOC
!              CUCNVC
!              GSMDRIVE
!              INIT
!              OUT2
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    SAVE
!
    REAL   (KIND=R4)    , DIMENSION(:,:)              , ALLOCATABLE                             ::&
    & PHOUR   , APREC
!
    REAL   (KIND=R4)    , DIMENSION(:,:,:)            , ALLOCATABLE                             ::&
    & TLATCU  , TLATGS
!
    REAL   (KIND=R4)    , DIMENSION(:,:,:)            , ALLOCATABLE                             ::&
    & PPTDAT
!
    REAL   (KIND=R4)                                                                            ::&
    & MTSTPE  , ITSTLOC , JTSTLOC
!--------------------------------------------------------------------------------------------------
! APREC - GRID-SCALE PRECIP, CALCULATED IN PRECPD. DIDN'T SEEM TO SERVE ANY PARTICULAR PURPOSE. 
! I'M JUST GOING TO INCLUDE IT IN THE COMMON BLOCK AND USE IT TO FIGURE OUT HOW MUCH OF THE PRECIP
! AT EACH TIMESTEP IS CONVECTIVE.
!
! TLATCU: CUCNVC LATENT HEAT
! TLATGS: GRID-SCALE LATENT HEAT, WHICH IS KEPT TRACK OF BY BRAD IN TMOD.
!
! ITSTLOC, JTSTLOC, MTSTPE: LOCAL (ITEST,JTEST) POINT, AND THE NODE IT BELONGS TO
!--------------------------------------------------------------------------------------------------
    END MODULE PPTASM
