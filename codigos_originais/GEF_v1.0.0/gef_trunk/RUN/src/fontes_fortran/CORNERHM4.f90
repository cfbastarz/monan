!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Inserir Brief de CORNERHM4
!> @details Inserir Details de CORNERHM4
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
!> @param[in] GDPD2 - Significado de GDPD2
!> @param[inout] HXY - Significado de HXY
!> @details <b>Use Module:</b> 
!! @arg @c F77KINDS
!! @arg @c PARMETA
!> @details <b>Driver:</b> 
!<
!> @details <b>Calls:</b>
!<
!--------------------------------------------------------------------------------------------------
       SUBROUTINE CORNERHM4(HXY, GDPD2)
!--------------------------------------------------------------------------------------------------
! SUBROUTINE CORNERHM4
!
! SUBPROGRAM: CORNERHM4 - ?????
! PROGRAMMER: ????? 
! ORG: W/NP22
! DATE: ??-??-??
!     
! ABSTRACT: 
! ?????    
!
! PROGRAM HISTORY LOG:
! ??-??-??  ?????    - ORIGINATOR            
! 18-03-20  LUCCI    - MODERNIZATION OF THE CODE, INCLUDING:
!                      * F77 TO F90/F95
!                      * INDENTATION & UNIFORMIZATION CODE
!                      * REPLACEMENT OF COMMONS BLOCK FOR MODULES
!                      * DOCUMENTATION WITH DOXYGEN
!                      * OPENMP FUNCTIONALITY
!
! INPUT  ARGUMENT LIST:
! GDPD2 - 
!
! OUTPUT ARGUMENT LIST:
! NONE
!
! INPUT/OUTPUT ARGUMENT LIST:
! HXY   - 
!     
! USE MODULES: F77KINDS
!              PARMETA
!
! DRIVER     : -----
!
! CALLS      : -----
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
    USE PARMETA
!
    IMPLICIT NONE
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1)                       , INTENT(INOUT)       ::&
    & HXY
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1)                       , INTENT(IN)          ::&
    & GDPD2
!
    INTEGER(KIND=I4)                                                                            ::&
    & ISCORNER2
!
    IF (ISCORNER2(1,1) == 1) THEN
        HXY(2,1) = HXY(2,1) - GDPD2(1,1) / 48.
        HXY(1,2) = HXY(1,2) - GDPD2(1,1) / 48.
!
        HXY(1,1) = HXY(1,1) + 0.125 * GDPD2(1,1)                                                  &
    &            - (GDPD2(2,1) + GDPD2(1,2) - 2. * GDPD2(0,1)) / 72.
    END IF
!
    IF (ISCORNER2(IM,1) == 2) THEN
        HXY(IM-2,1) = HXY(IM-2,1) - GDPD2(IM,1) / 48.
        HXY(IM-1,2) = HXY(IM-1,2) - GDPD2(IM,1) / 48.
!
        HXY(IM-1,1) = HXY(IM-1,1) + 0.125 * GDPD2(IM,1)                                           &
    &               - (GDPD2(IM-1,1) + GDPD2(IM,2) - 2. * GDPD2(IM+1,1)) / 72.
    END IF
!
    IF (ISCORNER2(1,JM) == 3) THEN
        HXY(2,JM-1) = HXY(2,JM-1) - GDPD2(1,JM) / 48.
        HXY(1,JM-2) = HXY(1,JM-2) - GDPD2(1,JM) / 48.
!
        HXY(1,JM-1) = HXY(1,JM-1) + 0.125 * GDPD2(1,JM)                                           &
    &               - (GDPD2(1,JM-1) + GDPD2(2,JM) - 2. * GDPD2(1,JM+1)) / 72.
    END IF
!
    IF (ISCORNER2(IM,JM) == 4) THEN
        HXY(IM-2,JM-1) = HXY(IM-2,JM-1) - GDPD2(IM,JM) / 48.
        HXY(IM-1,JM-2) = HXY(IM-1,JM-2) - GDPD2(IM,JM) / 48.
!
        HXY(IM-1,JM-1) = HXY(IM-1,JM-1) + 0.125 * GDPD2(IM,JM)                                    &
    &                  - (GDPD2(IM,JM-1) + GDPD2(IM-1,JM) - 2. * GDPD2(IM+1,JM)) / 72.
    END IF
!
    END SUBROUTINE CORNERHM4      

