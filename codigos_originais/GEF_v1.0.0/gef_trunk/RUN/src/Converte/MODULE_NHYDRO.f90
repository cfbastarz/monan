!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module NHYDRO...
!! @details Details of Module NHYDRO...
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS
!! @details <b>Driver:</b> 
!! @arg @c ALLOC
!! @arg @c GEF
!! @arg @c INIT
!! @arg @c OUT2
!<
!--------------------------------------------------------------------------------------------------
    MODULE NHYDRO
!--------------------------------------------------------------------------------------------------
! MODULE NHYDRO
!
! USE MODULES: F77KINDS
!
! DRIVER     : ALLOC
!              GEF
!              INIT
!              OUT2
!--------------------------------------------------------------------------------------------------    
    USE F77KINDS
! 
    IMPLICIT NONE
!
    LOGICAL(KIND=L4)                                                                            ::&
    & HYDRO   , SPLINE
!
    REAL   (KIND=R4)    , DIMENSION(:,:,:)            , ALLOCATABLE                             ::&
    & DWDT    , PINT    , W       , Z       , PDWDT
!--------------------------------------
! COMMON BLOCK NHHYDRO WAS:
! HYDRO , SPLINE
!  DWDT(IDIM1:IDIM2, JDIM1:JDIM2, LM  )
!  PINT(IDIM1:IDIM2, JDIM1:JDIM2, LM+1)
!     W(IDIM1:IDIM2, JDIM1:JDIM2, LM+1)
!     Z(IDIM1:IDIM2, JDIM1:JDIM2, LM+1)
! PDWDT(IDIM1:IDIM2, JDIM1:JDIM2, LM  )
!--------------------------------------
    END MODULE NHYDRO

