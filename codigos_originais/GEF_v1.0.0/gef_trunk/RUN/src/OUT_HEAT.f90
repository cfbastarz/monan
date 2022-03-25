!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Inserir Brief de OUT_HEAT
!> @details Inserir Details de OUT_HEAT
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
!! @arg @c ACMCLH
!! @arg @c ACMPRE
!! @arg @c ACMRDL
!! @arg @c ACMRDS
!! @arg @c CLDWTR
!! @arg @c CONTIN
!! @arg @c CTLBLK
!! @arg @c DGNSOUT
!! @arg @c LOOPS
!! @arg @c MPPSTAFF
!! @arg @c PARMETA
!! @arg @c PHYS
!! @arg @c PVRBLS
!! @arg @c SOIL
!! @arg @c VRBLS
!> @details <b>Driver:</b> 
!! @arg @c GEF
!> @details <b>Calls:</b>
!--------------------------------------------------------------------------------------------------     
	SUBROUTINE OUT_HEAT
!-------------------------------------------------------------------------------------------------- 
! SUBROUTINE OUT_HEAT
!
! SUBROUTINE: OUT_HEAT - ?????
! PROGRAMMER: ?????  
! ORG: ?????
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
! NONE
!
! OUTPUT ARGUMENT LIST:
! NONE
!
! INPUT/OUTPUT ARGUMENT LIST:
! NONE
!
! OUTPUT FILES:
! OUTPUT - PRINT FILE.
!
! USE MODULES: ACMCLH
!              ACMPRE
!              ACMRDL
!              ACMRDS
!              CLDWTR
!              CONTIN
!              CTLBLK
!              DGNSOUT
!              LOOPS
!              MPPSTAFF
!              PARMETA
!              PHYS
!              PVRBLS
!              SOIL
!              VRBLS
!
! DRIVER     : GEF
!
! CALLS      : ----- 
!--------------------------------------------------------------------------------------------------
    USE ACMCLH
    USE ACMPRE
    USE ACMRDL
    USE ACMRDS
    USE CLDWTR
    USE CONTIN
    USE CTLBLK
    USE DGNSOUT
    USE LOOPS
    USE MPPSTAFF
    USE PARMETA
    USE PHYS
    USE PVRBLS
    USE SOIL
    USE VRBLS
! 
    IMPLICIT NONE
!
    CHARACTER(LEN=80)                                                                           ::&
    & OUTFILE
!
!!!!    OUTFILE = 'heat_budget_'//C_MYPE//'.dat'    !commented DRAGAN 08/07/2019
!
!!!!    OPEN (UNIT=LISTOUT, FILE= TRIM(OUTFILE), FORM = 'UNFORMATTED')
!
!!!!    WRITE(LISTOUT) RDDTDT
!!!!    WRITE(LISTOUT) TCUCN
!!!!    WRITE(LISTOUT) ACDTDT
!
    CLOSE (LISTOUT)
!
    END SUBROUTINE OUT_HEAT
