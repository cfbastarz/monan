!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief BLOCK DATA INTIALIZES QUANTITIES NEEDED BY THE GFDL CODES.
!> @details BLOCK DATA BD1 GIVES INPUT DATA (TEMPS, PRESSURES, MIXING RATIOS, CLOUD AMTS AND HEIGHTS) 
!! FOR TESTING THE RADIATION CODE AS A STANDALONE MODEL.
!> @author ORIGINATOR - ????? 
!> @date ??-??-?? \n
!> @author LUCCI 
!> @date 18-03-20 \n
!> @version V1.1.0
!> @details MODERNIZATION OF THE CODE, INCLUDING:
!!                      * F77 TO F90/F95
!!                      * INDENTATION & UNIFORMIZATION CODE
!!                      * REPLACEMENT OF COMMONS BLOCK FOR MODULES
!!                      * DOCUMENTATION WITH DOXYGEN
!!                      * OPENMP FUNCTIONALITY 
!<
!> @details <b>Use Module:</b>
!<
!> @details <b>Driver:</b> 
!! @arg @c F77KINDS
!! @arg @c PARMETA
!! @arg @c RDPARM
!! @arg @c SSALB
!! @arg @c TBLTMP
!> @details <b>Calls:</b>
!--------------------------------------------------------------------------------------------------
    BLOCK DATA GFDLRD
!--------------------------------------------------------------------------------------------------
! BLOCK DATA GFDLRD
! 
! BLOCK DATA: GFDLRD - BLOCK DATA INTIALIZES QUANTITIES NEEDED BY THE GFDL CODES.
! PROGRAMMER: ?????
! ORG: ?????
! DATE: ??-??-??
!
! ABSTRACT:
! BLOCK DATA BD1 GIVES INPUT DATA (TEMPS, PRESSURES, MIXING RATIOS, CLOUD AMTS AND HEIGHTS) FOR 
! TESTING THE RADIATION CODE AS A STANDALONE MODEL.
!
! PROGRAM HISTORY LOG:
! ??-??-??  ?????      - ORIGINATOR
! 18-01-15  LUCCI      - MODERNIZATION OF THE CODE, INCLUDING:
!                       * F77 TO F90/F95
!                       * INDENTATION & UNIFORMIZATION CODE
!                       * REPLACEMENT OF COMMONS BLOCK FOR MODULES
!                       * DOCUMENTATION WITH DOXYGEN
!                       * OPENMP FUNCTIONALITY
!
! INPUT ARGUMENT LIST:
! NONE
!
! OUTPUT ARGUMENT LIST:
! NONE
!
! INPUT/OUTPUT ARGUMENT LIST:
! NONE
!
! USE MODULES: F77KINDS
!              PARMETA
!              RDPARM
!              SSALB
!              TBLTMP
!
! DRIVER     : -----
!
! CALLS      : -----  
!-------------------------------------------------------------------------------------------------
    USE F77KINDS
    USE PARMETA
    USE RDPARM
    USE SSALB
    USE TBLTMP
!
    IMPLICIT NONE
!    
    END BLOCK DATA GFDLRD
