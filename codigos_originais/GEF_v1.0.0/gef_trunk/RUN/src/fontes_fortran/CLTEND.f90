!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief TEMPERATURE CHANGE BY CLOUD PROCESSES
!> @details GRADUALLY UPDATES TEMPERATURE TENDENCIES FROM CONVECTION GRID-SCALE MICROPHYSICS AND 
!! PRECIPITATION ASSIMILATION.
!> @author ORIGINATOR - FERRIER 
!> @date 01-09-26 \n
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
!> @param[in] ICLTEND - FLAG SET TO -1 PRIOR TO PHYSICS CALLS, 0 AFTER PHYSICS CALLS, AND 1 FOR 
!! UPDATING TEMPERATURES EVERY TIME STEP
!> @details <b>Use Module:</b> 
!! @arg @c CTLBLK
!! @arg @c F77KINDS
!! @arg @c MASKS
!! @arg @c MPPSTAFF
!! @arg @c PARMETA
!! @arg @c TEND
!! @arg @c VRBLS
!> @details <b>Driver:</b> 
!! @arg @c RADFS
!> @details <b>Calls:</b>
!<
!--------------------------------------------------------------------------------------------------
    SUBROUTINE CLTEND(ICLTEND)
!--------------------------------------------------------------------------------------------------
! SUBROUTINE CLTEND
!
! SUBPROGRAM: CLTEND - TEMPERATURE CHANGE BY CLOUD PROCESSES
! PROGRAMMER: FERRIER 
! ORG: W/NP22
! DATE: 01-09-26
!     
! ABSTRACT: 
! CLTEND GRADUALLY UPDATES TEMPERATURE TENDENCIES FROM CONVECTION GRID-SCALE MICROPHYSICS AND 
! PRECIPITATION ASSIMILATION.    
!
! PROGRAM HISTORY LOG:
! 01-09-26  FERRIER  - ORIGINATOR            
! 18-03-20  LUCCI    - MODERNIZATION OF THE CODE, INCLUDING:
!                      * F77 TO F90/F95
!                      * INDENTATION & UNIFORMIZATION CODE
!                      * REPLACEMENT OF COMMONS BLOCK FOR MODULES
!                      * DOCUMENTATION WITH DOXYGEN
!                      * OPENMP FUNCTIONALITY
!
! INPUT  ARGUMENT LIST:
! ICLTEND - FLAG SET TO -1 PRIOR TO PHYSICS CALLS, 0 AFTER PHYSICS CALLS, AND 1 FOR UPDATING 
!           TEMPERATURES EVERY TIME STEP
!
! OUTPUT ARGUMENT LIST:
! NONE
!
! INPUT/OUTPUT ARGUMENT LIST:
! NONE
!     
! USE MODULES: CTLBLK
!              F77KINDS
!              MASKS
!              MPPSTAFF
!              PARMETA
!              TEND
!              VRBLS
!
! DRIVER     : GEF
!
! CALLS      : -----
!-------------------------------------------------------------------------------------------------- 
    USE CTLBLK
    USE F77KINDS 
    USE MASKS 
    USE MPPSTAFF
    USE PARMETA
    USE TEND
    USE VRBLS
!
    IMPLICIT NONE
!
    INTEGER(KIND=I4)                                                      , INTENT(IN)          ::&
    & ICLTEND
!
!GSM    REAL   (KIND=R4)    , DIMENSION(0:IM+1,0:JM+1,LM) , SAVE                                    ::&
!GSM    & T_OLD 
!
!GSM    REAL   (KIND=R4)    , DIMENSION(0:IM+1,0:JM+1,LM) , SAVE                                    ::&
!GSM    & T_ADJ
!
    INTEGER(KIND=I4)                                                                            ::&
    & I       , J       , L
!
    REAL   (KIND=R4)                                                                            ::&
    & DELTPH
!
    IF (ICLTEND < 0) THEN
!-------------------------------------------------------------
! SAVE OLD TEMPERATURE ARRAY BEFORE CUCNVC, GSMDRIVE, & ADJPPT
!-------------------------------------------------------------
        DO L=1,LM
            DO J=0,JM+1
                DO I=0,IM+1
                    T_OLD(I,J,L) = T(I,J,L)
                END DO
            END DO
        END DO
!
    ELSE IF (ICLTEND == 0) THEN
!-----------------------------------------------------------------
! CALCULATE TEMPERATURE TENDENCIES FROM CUCNVC, GSMDRIVE, & ADJPPT 
!-----------------------------------------------------------------
        DELTPH = 1. / FLOAT(NPHS)
!
        DO L=1,LM
            DO J=0,JM+1
                DO I=0,IM+1
                    T_ADJ(I,J,L) =   HTM(I,J,L) * DELTPH * (T(I,J,L) - T_OLD(I,J,L))
                        T(I,J,L) = T_OLD(I,J,L)
                END DO
            END DO
        END DO
!
    ELSE
!--------------------------------------------------------------------------------------------------
! GRADUALLY UPDATE TEMPERATURE FROM CUCNVC, GSMDRIVE, & ADJPPT IN SMALL INCREMENTS EVERY DYNAMICS 
! TIME STEP
!--------------------------------------------------------------------------------------------------
        DO L=1,LM
            DO J=0,JM+1
                DO I=0,IM+1
                    T(I,J,L) = T(I,J,L) + T_ADJ(I,J,L)
                END DO
            END DO
        END DO
!
        ACDTDT = ACDTDT + T_ADJ
!
    END IF
!
    RETURN
!
    END SUBROUTINE CLTEND
