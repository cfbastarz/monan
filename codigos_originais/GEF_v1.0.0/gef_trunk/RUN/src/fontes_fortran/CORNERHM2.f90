!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Inserir Brief de CORNERHM2
!> @details Inserir Details de CORNERHM2
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
!> @param[in] GDPD - Significado de GDPD
!> @param[inout] HXY - Significado de HXY
!> @details <b>Use Module:</b> 
!! @arg @c F77KINDS
!! @arg @c PARMETA
!> @details <b>Driver:</b> 
!! @arg @c HZADV
!! @arg @c HZADVQ
!> @details <b>Calls:</b>
!<
!--------------------------------------------------------------------------------------------------
     SUBROUTINE CORNERHM2(HXY, GDPD)
!--------------------------------------------------------------------------------------------------
! SUBROUTINE CORNERHM2
!
! SUBPROGRAM: CORNERHM2 - ?????
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
! GDPD -
!
! OUTPUT ARGUMENT LIST:
! NONE
!
! INPUT/OUTPUT ARGUMENT LIST:
! HXY -
!     
! USE MODULES: F77KINDS
!              PARMETA
!
! DRIVER     : HZADV
!              HZADVQ
!
! CALLS      : -----
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
    USE PARMETA
!
    IMPLICIT NONE
!
    REAL   (KIND=R4)    , PARAMETER :: D1R3 = 1.0 / 3.0
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1)                       , INTENT(INOUT)       ::&
    & HXY
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1)                       , INTENT(IN)          ::&
    & GDPD
!
    INTEGER(KIND=I4)                                                                            ::&
    & ISCORNER2
!
    IF (ISCORNER2(1,1) == 1) THEN
        HXY(1,1) = HXY(1,1) + GDPD(1,1) * D1R3
    END IF
!
    IF (ISCORNER2(IM,1) == 2) THEN
        HXY(IM-1,1) = HXY(IM-1,1) + GDPD(IM,1) * D1R3
    END IF
!
    IF (ISCORNER2(1,JM) == 3) THEN
        HXY(1,JM-1) = HXY(1,JM-1) + GDPD(1,JM) * D1R3
    END IF
!
    IF (ISCORNER2(IM,JM) == 4) THEN
        HXY(IM-1,JM-1) = HXY(IM-1,JM-1) + GDPD(IM,JM) * D1R3
    END IF
!
    END SUBROUTINE CORNERHM2
