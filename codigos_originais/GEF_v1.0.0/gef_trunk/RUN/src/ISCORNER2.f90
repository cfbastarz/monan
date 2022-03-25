!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Inserir Brief de ISCORNER2
!> @details Inserir Details de ISCORNER2
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
!> @param[in] I - Significado de I
!> @param[in] J - Significado de J
!> @details <b>Use Module:</b>
!! @arg @c DOM
!! @arg @c F77KINDS
!! @arg @c PARMETA
!> @details <b>Driver:</b> 
!<
!> @details <b>Calls:</b>
!--------------------------------------------------------------------------------------------------
    INTEGER FUNCTION ISCORNER2(I,J)
!--------------------------------------------------------------------------------------------------
! SUBPROGRAM ISCORNER2
! 
! SUBPROGRAM: ISCORNER2 - ?????
! PROGRAMMER: ?????          
! ORG: W/NP22
! DATE: ??-??-??
!
! ABSTRACT:
! ?????
!
! PROGRAM HISTORY LOG:
! ??-??-??  ?????      - ORIGINATOR
! 18-01-15  LUCCI      - MODERNIZATION OF THE CODE, INCLUDING:
!                        * F77 TO F90/F95
!                        * INDENTATION & UNIFORMIZATION CODE
!                        * REPLACEMENT OF COMMONS BLOCK FOR MODULES
!                        * DOCUMENTATION WITH DOXYGEN
!                        * OPENMP FUNCTIONALITY
!
! INPUT ARGUMENT LIST:
! I - 
! J - 
!
! OUTPUT ARGUMENT LIST:
! NONE
!
! INPUT/OUTPUT ARGUMENT LIST:
! NONE
!
! USE MODULES: DOM
!              F77KINDS
!              PARMETA
! 
! DRIVER     : -----
!
! CALLS      : -----
!--------------------------------------------------------------------------------------------------
    USE DOM
    USE F77KINDS
    USE PARMETA
!
    IMPLICIT NONE
!
    INTEGER(KIND=I4)                                                      , INTENT(IN)          ::&
    & I       , J
!
    ISCORNER2 = 0
!-----------
! CONDITIONS
!-----------
    IF      (I ==  1 .AND. J ==  1 .AND. LWEST .AND. LSOUTH) THEN
        ISCORNER2 = 1
    ELSE IF (I == IM .AND. J ==  1 .AND. LEAST .AND. LSOUTH) THEN
        ISCORNER2 = 2
    ELSE IF (I ==  1 .AND. J == JM .AND. LWEST .AND. LNORTH) THEN
        ISCORNER2 = 3
    ELSE IF (I == IM .AND. J == JM .AND. LEAST .AND. LNORTH) THEN
        ISCORNER2 = 4
    END IF
!
    END FUNCTION ISCORNER2
