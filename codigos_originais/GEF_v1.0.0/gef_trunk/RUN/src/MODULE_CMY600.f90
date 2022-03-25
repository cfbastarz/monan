!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module CMY600... 
!! @details Details of Module CMY600... 
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS 
!! @details <b>Driver:</b> 
!! @arg @c GSMCOLUMN  
!! @arg @c GSMDRIVE 
!<
!--------------------------------------------------------------------------------------------------
    MODULE CMY600
!--------------------------------------------------------------------------------------------------
! MODULE CMY600
!
! USE MODULES: F77KINDS
! 
! DRIVER     : GSMCOLUMN
!              GSMCONST
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    INTEGER(KIND=I4)    , PARAMETER :: MY_T1 =  1
    INTEGER(KIND=I4)    , PARAMETER :: MY_T2 = 35
!
    REAL   (KIND=R4)    , DIMENSION(MY_T1:MY_T2)                                                ::&
    & MY_GROWTH
!
    END MODULE CMY600
