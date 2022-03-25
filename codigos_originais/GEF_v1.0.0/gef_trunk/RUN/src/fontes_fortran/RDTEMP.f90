!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief RADIATIVE TEMPERATURE CHANGE
!> @details APPLIES THE TEMPERATURE TENDENCIES DUE TO RADIATION AT ALL LAYERS AT EACH ADJUSTMENT TIME
! STEP.
!> @author ORIGINATOR - BLACK
!> @date 93-12-29 \n
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
!! @arg @c ACMRDL
!! @arg @c ACMRDS
!! @arg @c CTLBLK
!! @arg @c F77KINDS
!! @arg @c MASKS
!! @arg @c MPPSTAFF
!! @arg @c PARMETA
!! @arg @c PHYS
!! @arg @c PVRBLS
!! @arg @c VRBLSS
!> @details <b>Driver:</b> 
!! @arg @c GEF
!> @details <b>Calls:</b>
!! @arg @c BOCOHMPI
!! @arg @c ZENITH
!--------------------------------------------------------------------------------------------------      
    SUBROUTINE RDTEMP
!--------------------------------------------------------------------------------------------------
! SUBROUTINE RDTEMP
! 
! SUBPROGRAM: RDTEMP - RADIATIVE TEMPERATURE CHANGE
! PROGRAMMER: BLACK 
! ORG: W/NP22
! DATE: 93-12-29
!
! ABSTRACT:  
! RDTEMP APPLIES THE TEMPERATURE TENDENCIES DUE TO RADIATION AT ALL LAYERS AT EACH ADJUSTMENT TIME
! STEP.
!
! PROGRAM HISTORY LOG:
! 93-12-29  BLACK   - ORIGINATOR
! 95-03-25  BLACK   - CONVERSION FROM 1-D TO 2-D IN HORIZONTAL
! 95-11-20  ABELES  - PARALLEL OPTIMIZATION
! 98-10-30  BLACK   - MODIFIED FOR DISTRIBUTED MEMORY
! 18-03-20  LUCCI   - MODERNIZATION OF THE CODE, INCLUDING:
!                     * F77 TO F90/F95
!                     * INDENTATION & UNIFORMIZATION CODE
!                     * REPLACEMENT OF COMMONS BLOCK FOR MODULES
!                     * DOCUMENTATION WITH DOXYGEN
!                     * OPENMP FUNCTIONALITY 
!
!
! INPUT ARGUMENT LIST:
! NONE
!
! OUTPUT ARGUMENT LIST:
! 
!
! INPUT/OUTPUT ARGUMENT LIST:
! NONE
!
! INPUT FILES:
! NONE
!
! OUTPUT FILES:
! NONE
!
! USE MODULES: ACMRDL
!              ACMRDS
!              CTLBLK
!              F77KINDS
!              MASKS
!              MPPSTAFF
!              PARMETA
!              PHYS
!              PVRBLS
!              VRBLS
!
! DRIVER     : GEF
!
! CALLS      : BOCOHMPI
!              ZENITH            
!--------------------------------------------------------------------------------------------------
    USE ACMRDL
    USE ACMRDS
    USE CTLBLK
    USE F77KINDS
    USE MASKS
    USE MPPSTAFF
    USE PARMETA
    USE PHYS
    USE PVRBLS
    USE VRBLS
!
    IMPLICIT NONE
!
    INTEGER(KIND=I4)                                                                            ::&
    & I       , J       , L
!
    REAL   (KIND=R4)                                                                            ::&
    & TIMES   , TTNDKL  , DAYI    , HOUR
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1)                                             ::&
    & FACTR
!--------------------------------------- 
! GET CURRENT VALUE OF COS(ZENITH ANGLE)
!---------------------------------------
!GSM    TIMES = (NTSD - 1) * DT
    TIMES = NTSD * DT
!
    CALL ZENITH(TIMES, DAYI, HOUR)
!
!$omp parallel do
! 
    DO J=1,JM 
        DO I=1,IM
!
            IF (CZMEAN(I,J) > 0.) THEN 
                FACTR(I,J) = CZEN(I,J) / CZMEAN(I,J)
            ELSE
                FACTR(I,J) = 0.
            END IF
!
        END DO 
    END DO 
!
!$omp parallel do private(TTNDKL)
!
    DO L=1,LM
        DO J=1,JM  
            DO I=1,IM  
                     TTNDKL   =  RSWTT(I,J,L) * FACTR(I,J)  + RLWTT(I,J,L)
                     T(I,J,L) =      T(I,J,L) + TTNDKL * DT *   HTM(I,J,L)
                RDDTDT(I,J,L) = RDDTDT(I,J,L) + TTNDKL * DT *   HTM(I,J,L)
            END DO
        END DO
    END DO
!
    CALL BOCOHMPI(T     , LM)
    CALL BOCOHMPI(RDDTDT, LM)
!
    RETURN
!
    END SUBROUTINE RDTEMP
