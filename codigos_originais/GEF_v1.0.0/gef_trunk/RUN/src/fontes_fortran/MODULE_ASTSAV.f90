!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module ASTSAV... 
!! @details Details of Module ASTSAV... 
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS 
!! @details <b>Driver:</b> 
!! @arg @c RADFS 
!<
!--------------------------------------------------------------------------------------------------
    MODULE ASTSAV
!--------------------------------------------------------------------------------------------------
! MODULE ASTSAV
!
! USE MODULES: F77KINDS
!
! DRIVER     : RADFS
!--------------------------------------------------------------------------------------------------
!
!---------------------------------------------------------------
! SOLC, THE SOLAR CONSTANT IS SCALED TO A MORE CURRENT VALUE.
! I.E. IF SOLC=2.0 LY/MIN THEN SSOLAR=1.96 LY/MIN.
! RE-COMPUTED CAUSE SSOLAR OVERWRITTEN AS PART OF SCRATCH COMMON
!---------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    SAVE
!
    REAL(KIND=R4)                                                                               ::&
    & SOLC    , RSIN1   , RCOS1   , RCOS2
!
    END MODULE ASTSAV
