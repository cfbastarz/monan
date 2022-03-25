!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief UPDATE SURFACE PRESSURE
!> @details UPDATES THE SURFACE PRESSURE FROM THE TENDENCY COMPUTED IN PDTE.
!> @author ORIGINATOR - JANJIC
!> @date 87-06-?? \n
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
!> @param[in] IM   - Significado de IM
!> @param[in] JM   - Significado de JM
!> @param[in] LM   - Significado de LM
!> @details <b>Use Module:</b>
!! @arg @c CONTIN
!! @arg @c CTLBLK
!! @arg @c F77KINDS
!! @arg @c MPPSTAFF
!! @arg @c VRBLS
!> @details <b>Driver:</b> 
!! @arg @c DIGFLT
!! @arg @c GEF
!> @details <b>Calls:</b>
!! @arg @c BOCOHMPI
!--------------------------------------------------------------------------------------------------  
    SUBROUTINE PDNEW
!--------------------------------------------------------------------------------------------------
! SUBROUTINE PDNEW
!
! SUBROUTINE: PDNEW - UPDATE SURFACE PRESSURE
! PROGRAMMER: JANJIC
! ORG: W/NP22
! DATE: 87-06-??
!
! ABSTRACT:
! PDNEW UPDATES THE SURFACE PRESSURE FROM THE TENDENCY COMPUTED IN PDTE.
!
! PROGRAM HISTORY LOG:
! 87-06-??  JANJIC     - ORIGINATOR
! 95-03-25  BLACK      - CONVERSION FROM 1-D TO 2-D IN HORIZONTAL
! 98-10-30  BLACK      - MODIFIED FOR DISTRIBUTED MEMORY
! 18-01-15  LUCCI      - MODERNIZATION OF THE CODE, INCLUDING:
!                        * F77 TO F90/F95
!                        * INDENTATION & UNIFORMIZATION CODE
!                        * REPLACEMENT OF COMMONS BLOCK FOR MODULES
!                        * DOCUMENTATION WITH DOXYGEN
!                        * OPENMP FUNCTIONALITY
!
! INPUT ARGUMENT LIST:
! IM - 
! JM - 
! LM - 
!
! OUTPUT ARGUMENT LIST:
! NONE
!
! INPUT/OUTPUT ARGUMENT LIST:
! NONE
!
! OUTPUT FILES:
! NONE
!
! USE MODULES: CONTIN
!              CTLBLK
!              F77KINDS
!              MPPSTAFF     
!              VRBLS     
!
! DRIVER     : DIGFLT
!              GEF      
!
! CALLS      : BOCHMPI
!--------------------------------------------------------------------------------------------------
    USE CONTIN
    USE CTLBLK
    USE F77KINDS
    USE MPPSTAFF
    USE PARMETA , ONLY: IM, JM, LM
    USE VRBLS
!
    IMPLICIT NONE
!
    REAL   (KIND=R4)    , PARAMETER :: R3 = 1. / 3.
!
    INTEGER(KIND=I4)                                                                            ::&
    & I       , J
!-----------------------------
! UPDATING PRESSURE DIFFERENCE
!-----------------------------
!    PDOLD = PD
!
    DO J=1,JM
        DO I=1,IM
            PD(I,J) = PSDT(I,J) + PD(I,J)
        END DO
    END DO
!
    CALL BOCOHMPI(PD,1) 
!
    END SUBROUTINE PDNEW
