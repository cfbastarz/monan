!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief DESIGNED TO DUPLICATE TIMEF
!> @details DESIGNED TO DUPLICATE TIMEF
!> @author ORIGINATOR -  M. PYLE
!> @date 99-12-?? \n
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
!! @arg @c F77KINDS
!> @details <b>Driver:</b> 
!<
!> @details <b>Calls:</b>
!--------------------------------------------------------------------------------------------------     
    FUNCTION TIMEF()
!--------------------------------------------------------------------------------------------------
! FUNCTION TIMEF
!
! FUNCTION: TIMEF - DESIGNED TO DUPLICATE TIMEF
! PROGRAMMER: M. PYLE   
! ORG: ?????
! DATE: 99-12-??
! 
! ABSTRACT:
! DESIGNED TO DUPLICATE TIMEF
!
! PROGRAM HISTORY LOG:
! 99-12-??  M. PYLE     - ORIGINATOR
! 18-01-15  LUCCI       - MODERNIZATION OF THE CODE, INCLUDING:
!                         * F77 TO F90/F95
!                         * INDENTATION & UNIFORMIZATION CODE
!                         * REPLACEMENT OF COMMONS BLOCK FOR MODULES
!                         * DOCUMENTATION WITH DOXYGEN
!                         * OPENMP FUNCTIONALITY
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
!
! DRIVER     : -----
!
! CALLS      : -----
!-------------------------------------------------------------------------------------------------- 
    USE F77KINDS
!
    REAL   (KIND=R4)    , DIMENSION(2)                                                          ::&
    & ET
!
    REAL   (KIND=R8)                                                                            ::&
    & TIMEF
!
    TIMEF = ETIME(ET)
!    TIMEF = TIMEF * 1.E3
!
   END FUNCTION TIMEF
