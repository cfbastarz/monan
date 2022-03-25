!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module PARM_TBL...
!! @details Details of Module PARM_TBL...
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS
!! @details <b>Driver:</b> 
!! @arg @c CUCNVC
!! @arg @c GSMDRIVE
!! @arg @c MODULE_PHYS
!! @arg @c READ_SST12M
!! @arg @c SSTCH
!! @arg @c VEGUPDT
!<
!--------------------------------------------------------------------------------------------------
    MODULE PARM_TBL
!--------------------------------------------------------------------------------------------------
! MODULE PARM_TBL
!
! USE MODULES: F77KINDS
!
! DRIVER     : CUCNVC
!              GSMDRIVE
!              MODULE_PHYS
!              READ_SST12M
!              SSTCH
!              VEGUPDT
!--------------------------------------------------------------------------------------------------  
    USE F77KINDS
! 
    IMPLICIT NONE
!
    INTEGER(KIND=I4)    , PARAMETER :: ITB  =  76
    INTEGER(KIND=I4)    , PARAMETER :: JTB  = 134
    INTEGER(KIND=I4)    , PARAMETER :: ITBQ = 152
    INTEGER(KIND=I4)    , PARAMETER :: JTBQ = 440
!
    END MODULE PARM_TBL
