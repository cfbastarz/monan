!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief CALCULATES THE CONTRIBUTION OF THE VERTICAL ADVECTION TO THE TENDENCIES OF TEMPERATURE 
!! AND WIND COMPONENTS.
!> @details CALCULATES THE CONTRIBUTION OF THE VERTICAL ADVECTION TO THE TENDENCIES OF TEMPERATURE 
!! AND WIND COMPONENTS.
!! A SIMPLE CENTERED DIFFERENCE SCHEME IN SPACE IS USED IN CONJUNCTION WITH THE EULER-BACKWARD 
!! TIME SCHEME.
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
!! @arg @c CLDWTR
!! @arg @c CTLBLK
!! @arg @c CONTIN
!! @arg @c DYNAM
!! @arg @c F77KINDS
!! @arg @c LOOPS
!! @arg @c MASKS
!! @arg @c METRCS
!! @arg @c MPPSTAFF
!! @arg @c PARMETA
!! @arg @c PVRBLS
!! @arg @c REALPAR
!! @arg @c SET_ZERO
!! @arg @c VRBLS
!> @details <b>Driver:</b> 
!! @arg @c DIGFLT
!! @arg @c GEF
!> @details <b>Calls:</b>
!! @arg @c AVRH
!! @arg @c BOCOHMPI
!! @arg @c BOCOVMPI
!! @arg @c FLUX_CORRECTION1D
!! @arg @c ZERO
!--------------------------------------------------------------------------------------------------     
    SUBROUTINE VTADV
!--------------------------------------------------------------------------------------------------
! SUBROUTINE VTADV
!
! SUBPROGRAM: VTADV - 
! PROGRAMMER: ?????   
! ORG: ?????
! DATE: ??-??-??
! 
! ABSTRACT: 
! CALCULATES THE CONTRIBUTION OF THE VERTICAL ADVECTION TO THE TENDENCIES OF TEMPERATURE AND WIND
! COMPONENTS.
! A SIMPLE CENTERED DIFFERENCE SCHEME IN SPACE IS USED IN CONJUNCTION WITH THE EULER-BACKWARD 
! TIME SCHEME.
!
! PROGRAM HISTORY LOG:
! ??-??-??  ?????       - ORIGINATOR
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
! OUTPUT FILES:
! NONE
!
! USE MODULES: CLDWTR
!              CTLBLK
!              CONTIN
!              DYNAM
!              F77KINDS
!              LOOPS
!              MASKS
!              METRCS
!              MPPSTAFF
!              PARMETA
!              PVRBLS
!              REALPAR
!              SET_ZERO
!              VRBLS
!
! DRIVER     : DIGFLT
!              GEF
!
! CALLS      : AVRH
!              BOCOHMPI
!              BOCOVMPI
!              FLUX_CORRECTION1D
!              ZERO
!--------------------------------------------------------------------------------------------------
    USE CLDWTR
    USE CTLBLK
    USE CONTIN
    USE DYNAM
    USE F77KINDS
    USE LOOPS
    USE MASKS
    USE METRCS
    USE MPPSTAFF
    USE PARMETA
    USE PVRBLS
    USE REALPAR
    USE SET_ZERO
    USE VRBLS
!
    IMPLICIT NONE
!
    INCLUDE "mpif.h"
!
    INTEGER(KIND=I4)    , PARAMETER :: LM2 = LM - 2
!
    REAL   (KIND=R4)    , PARAMETER :: CFL_MAX = 0.97
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1)                                             ::&
    & TTB     , TUB     , TVB     , RPDXY   , PDSQ    , PDXY    , ETADTL  , ETADTXY , TQ2B
!
    INTEGER(KIND=I4)    , DIMENSION(0:IM+1, 0:JM+1)                                             ::&
    & LTOP_CFL_T        , LBOT_CFL_T        , LTOP_CFL_M        , LBOT_CFL_M
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1)                                             ::&
    & DQTI    , DQBI    , QDEDB   , QDEUB   , DQDEB   , EDBD
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1, LM)                                         ::&
    & TSTL    , Q2ST    , USTL    , VSTL    , QBI     , SAM     , ARRAY1  , ARRAY2
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1, LM)                                         ::&
    & VAD_TEND1         , VAD_TEND2
!
    INTEGER(KIND=I4)                                                                            ::&
    & I       , J       , L       , IERR    , LSTART  , LSTOP   , NWTR    , MSA     , NMSAP   ,   &
    & LLMH
!
    REAL   (KIND=R4)                                                                            ::&
    & TTAK    , VMF4D   , TUAK    , TVAK    , TQ2AK   , CFL     , DTAD    , EXTREM  , DQTIK   ,   &
    & ASTIK   , ASBIK   , QDEDTK  , QDEUTK  , SEDBK   , DQDEK   , EDBFK   , EDTDK
!
    REAL   (KIND=R4)    , DIMENSION(LM)                                                         ::&
    & VAD_TNDX1         , VAD_TNDX2
!
    REAL   (KIND=R4)    , DIMENSION(:,:,:)  , ALLOCATABLE                                       ::&
    & WATER
!
    LOGICAL(KIND=L4)                                                                            ::&
    & NOSLA
!
    REAL   (KIND=R4)    , PARAMETER :: EPSQ = 1.E-12
!
    REAL   (KIND=R4)    , DIMENSION(0:LM+1)                                                     ::&
    & WFA     , WFB
!
    REAL   (KIND=R4)    , DIMENSION(LM)                                                         ::&
    & ETADT2  , FXQ2L   , FXQ2H   , FVADV2  , ANTID   , Q2PRE   , MTMDT   , FVADV3
!------------------------------------------
! DEFINE ADDED UPSTREAM ADVECTION CONSTANTS
!------------------------------------------
    DO L=1,LM1
           WFA(L) = DETA(L  )  / (DETA(L) + DETA(L+1))
           WFB(L) = DETA(L+1)  / (DETA(L) + DETA(L+1))
	FVADV2(L) = -1. / (0.5 * (DETA(L) + DETA(L+1))) * IDTAD
    END DO
!
    NOSLA = .FALSE.
!
    NMSAP = 3
!
    VAD_TEND1  = 0.
!
    LTOP_CFL_T = 0
    LBOT_CFL_T = 0
    LTOP_CFL_M = 0
    LBOT_CFL_M = 0
!
    DTAD = IDTAD
!
    DO L=1,LM1
        DO I=1,IM
            DO J=1,JM 
!------------
! MASS POINTS
!------------
                CFL = ETADT(I,J,L) * DTAD / (0.5 * (DETA(L) + DETA(L+1)))
!
                IF (ABS(CFL) > CFL_MAX) THEN
                    IF (LTOP_CFL_T(I,J) == 0) LTOP_CFL_T(I,J) = MAX(L,2)
                    IF (LBOT_CFL_T(I,J) <  L) LBOT_CFL_T(I,J) = MAX(L,2)
                END IF
!---------
! MOMENTUM
!---------
                CFL = (ETADT(I,J,L) + ETADT(I,J+1,L) + ETADT(I+1,J,L) + ETADT(I+1,J+1,L))         &
    &               / (2. * (DETA(L) + DETA(L+1)))
!
                IF (ABS(CFL) > CFL_MAX) THEN
                    IF (LTOP_CFL_M(I,J) == 0) LTOP_CFL_M(I,J) = MAX(L,2)
                    IF (LBOT_CFL_M(I,J) <  L) LBOT_CFL_M(I,J) = MAX(L,2)
                END IF
!
            END DO
        END DO
    END DO
!----------------------------------
! VERTICAL (MATSUNO) ADVECTION OF T
!----------------------------------
    CALL ZERO(TTB)
!
    DO L=1,LM1
        DO J=1,JM
            DO I=1,IM
                TTAK             = (T(I,J,L+1) - T(I,J,L)) * ETADT(I,J,L) * F4D
                VAD_TEND1(I,J,L) = (TTAK + TTB(I,J)) * RDETA(L)
                TTB(I,J)         =  TTAK
            END DO
        END DO
    END DO
!      
    DO J=1,JM
        DO I=1,IM
            VAD_TEND1(I,J,LM) = TTB(I,J) * RDETA(LM)
        END DO
    END DO
!
    DO L=1,LM
        DO J=1,JM
            DO I=1,IM
                TSTL(I,J,L) = T(I,J,L) + VAD_TEND1(I,J,L)
            END DO
        END DO
    END DO
!-------------------------------
! SECOND (BACKWARD) MATSUNO STEP
!-------------------------------
    CALL ZERO(TTB)
!
    VAD_TEND1 = 0.
!
    DO L=1,LM1
        DO J=1,JM
            DO I=1,IM
                TTAK             = (TSTL(I,J,L+1) - TSTL(I,J,L)) * ETADT(I,J,L) * F4D
                VAD_TEND1(I,J,L) = (TTAK + TTB(I,J)) * RDETA(L)
                TTB(I,J)         =  TTAK
            END DO
        END DO
    END DO
!
    DO J=1,JM
        DO I=1,IM
            VAD_TEND1(I,J,LM) = TTB(I,J) * RDETA(LM)
        END DO
    END DO

!---------------------------------------------------------------------
! IF THE CFL CRITERION IS VIOLATED THEN VERTICALLY SMOOTH THE TENDENCY
!---------------------------------------------------------------------
    DO J=1,JM
        DO I=1,IM
            IF (LTOP_CFL_T(I,J) > 0) THEN
                LSTART = LTOP_CFL_T(I,J)
                LSTOP  = MIN(LBOT_CFL_T(I,J), LM-1)
!
                DO L=LSTART,LSTOP
                    VAD_TNDX1(L) = (VAD_TEND1(I,J,L-1)  + VAD_TEND1(I,J,L+1) + 2.                 &
    &                            *  VAD_TEND1(I,J,L  )) * 0.25
                END DO
!
                DO L=LSTART,LSTOP
                    VAD_TEND1(I,J,L) = VAD_TNDX1(L)
                END DO
!
            END IF
        END DO
    END DO
!
    DO L=1,LM
        DO J=1,JM
            DO I=1,IM
                T(I,J,L) = T(I,J,L) + VAD_TEND1(I,J,L)
            END DO
        END DO
    END DO
!-------------------------
! VERTICAL ADVECTION OF Q2
!-------------------------
    DO I=1,IM
        DO J=1,JM
            LLMH = LMH(I,J)
!
            ETADT2 = 0.
            FXQ2H  = 0.
            FXQ2L  = 0.
            ANTID  = 0.
!
            ETADT2(1)    = ETADT(I,J,     1) * 0.5
            ETADT2(LLMH) = ETADT(I,J,LLMH-1) * 0.5
!
            DO L=2,LLMH-1
                ETADT2(L) = 0.5 * (ETADT(I,J,L) + ETADT(I,J,L-1))
            END DO
!
            DO L=1,LLMH-1
                 MTMDT(L) = 1. / (1. + FVADV2(L) * (ETADT2(L+1) - ETADT2(L)))
                FVADV3(L) = MTMDT(L) * FVADV2(L)
            END DO
!
            FXQ2H(1) = ETADT2(1) * Q2(I,J,1) * 0.5
!
            IF (ETADT2(1) <= 0) THEN
	        FXQ2L(1) = ETADT2(1) * Q2(I,J,1)
            END IF
!
            DO L=2,LLMH
	        FXQ2H(L) = ETADT2(L) * (Q2(I,J,L) + Q2(I,J,L-1)) * 0.5
!
                IF (ETADT2(L) <= 0) THEN
	            FXQ2L(L) = ETADT2(L) * Q2(I,J,L)
                ELSE
	            FXQ2L(L) = ETADT2(L) * Q2(I,J,L-1)
                END IF
!
            END DO
!
            ANTID = FXQ2H - FXQ2L
!
            DO L=1,LLMH-1
	        Q2PRE(L) = Q2(I,J,L) * MTMDT(L) + FVADV3(L) * (FXQ2L(L+1) - FXQ2L(L))
            END DO
!
            CALL FLUX_CORRECTION1D(Q2PRE,Q2(I,J,:), FVADV3, ANTID, LLMH)
!
            DO L=1,LLMH-1
                Q2(I,J,L) = Q2PRE(L) + FVADV3(L) * (ANTID(L+1) - ANTID(L))
                Q2(I,J,L) = AMAX1(Q2(I,J,L), EPSQ2)
            END DO
!
        END DO 
    END DO
!------------------------------------------------------------
! PIECEWISE LINEAR UPSTREAM VERTICAL ADVECTION OF Q AND CLOUD 
!------------------------------------------------------------
    ALLOCATE (WATER(0:IM+1, 0:JM+1, LM), STAT=I)
!-------------------------------------------------------------------------------------------
! INTIALIZE Q AT THE BOTTOM INTERFACE AND THE SLOPE ADJUSTMENT MASK (SAM=1 FOR SA PERMITTED,
! 0 FOR NOT PERMITTED)
!-------------------------------------------------------------------------------------------
!
!--------------------------
! LOOP OVER WATER VARIABLES
!--------------------------
    DO NWTR=1,2
!
        IF (NWTR == 1) THEN
!
!$omp parallel do
!
            DO L=1,LM
                DO J=0,JM+1
                    DO I=0,IM+1
                        WATER(I,J,L) = Q(I,J,L)
                    END DO
                END DO
            END DO
!
        ELSE
!
!$omp parallel do
!
            DO L=1,LM
                DO J=0,JM+1
                    DO I=0,IM+1
                        WATER(I,J,L) = CWM(I,J,L)
                    END DO
                END DO
            END DO
!
        END IF
!
!$omp parallel do
!
        DO L=1,LM
            DO J=0,JM+1
                DO I=0,IM+1
                    QBI(I,J,L) = WATER(I,J,L)
                    SAM(I,J,L) = 1.
                END DO
            END DO
        END DO
!
        IF (NOSLA) GOTO 290
!-------------------------------------------------------------------------------- 
! THE SLOPE ADJUSTMENT CODE NO SLOPE PERMITTED AT THE TOP AND AT THE BOTTOM LAYER
!--------------------------------------------------------------------------------
        SAM(:, :,  1) = 0.
        SAM(:, :, LM) = 0.
!
!$omp parallel do
!
        DO L=1,LM1
            DO J=0,JM+1
                DO I=0,IM+1
                    SAM(I,J,L) = SAM(I,J,L) * HTM(I,J,L+1)
                END DO
            END DO
        END DO
!---------------------------------------------------------------------------------------------
! NOW, SEARCH FOR THE MAXIMA AND MINIMA OF Q (AT THE FIRST PASS) AND FOR LAYERS WHICH HAD OVER
! ADJUSTED (AT SUBSEQUENT PASSES) DUE TO ROUND-OFF ERRORS
!---------------------------------------------------------------------------------------------
!
!$omp parallel do private(DQBI, DQTI, EXTREM)
!
        DO L=2,LM1
            DO J=1,JM
                DO I=1,IM
                    DQTI(I,J) = WATER(I,J,L  ) - WATER(I,J,L-1)
                    DQBI(I,J) = WATER(I,J,L+1) - WATER(I,J,L  )
                    EXTREM    =  DQTI(I,J)     *  DQBI(I,J)
!
                    IF (EXTREM <= 0.) SAM(I,J,L) = 0.
                END DO
            END DO
        END DO
!
!$omp parallel do
!
        DO L=2,LM1
            DO J=1,JM
                DO I=1,IM
                    ARRAY1(I,J,L) = WFA(L-1) * (1. - SAM(I,J,L-1)) + WFB(L-1)
                    ARRAY2(I,J,L) = WFA(L  ) + WFB(L) * (1. - SAM(I,J,L+1))
                END DO
            END DO
        END DO
!
        DO MSA=1,NMSAP
!--------------------------------------------------------------------------------------------------
! CALCULATE DQ AT INTERFACES AND ADJUST THE SLOPES WHERE AND TO THE EXTENT PERMITTED OBSERVING THE 
! MONOTONICITY CONDITION (E.G. VAN LEER, J. COMP. PHYS. 1977, 276-299)
!--------------------------------------------------------------------------------------------------
!
!$omp parallel do
!
            DO J=1,JM
                DO I=1,IM
                    DQBI(I,J) = 2. * WATER(I,J,2) - QBI(I,J,2) - QBI(I,J,1)
                END DO
            END DO
!
            DO L=2,LM1
!
!$omp parallel do private(ASBIK, ASTIK, DQTIK)
!
                DO J=1,JM
                    DO I=1,IM
                        DQTIK      =       DQBI(I,J)
                        ASTIK      =     ARRAY1(I,J,L)   * DQTIK
                        DQBI(I,J)  = 2. * WATER(I,J,L+1) -  QBI(I,J,L+1) - QBI(I,J,L)
                        ASBIK      =     ARRAY2(I,J,L)   * DQBI(I,J)
                        QBI(I,J,L) =        QBI(I,J,L)   + (ASTIK - SIGN(1., ASTIK)               &
    &                              *    DIM(ABS(ASTIK), ABS(ASBIK))) * SAM(I,J,L) 
                    END DO
                END DO
!
            END DO
!
        END DO
!---------------------------------------------------------------------- 
! SLOPE ADJUSTMENT OF THE LAYERS ABOVE THAT NEXT TO THE SURFACE IS DONE
! NOW ADJUST THE LOWERMOST LAYER
!----------------------------------------------------------------------
        DO L=9,LM1
!
!$omp parallel do
!
            DO J=1,JM
                DO I=1,IM
                    IF (HTM(I,J,L+1) == 0.) QBI(I,J,L) = 2. * WATER(I,J,L) - QBI(I,J,L-1)
                END DO
            END DO
!
        END DO
!
!$omp parallel do
!
        DO J=1,JM
            DO I=1,IM
                QBI(I,J,LM) = 2. * WATER(I,J,LM) - QBI(I,J,LM1)
            END DO
        END DO
!---------------------------------
! END OF THE SLOPE ADJUSTMENT CODE
!---------------------------------
    290 CONTINUE
!
!$omp parallel do
!
        DO J=1,JM
            DO I=1,IM
                QDEDB(I,J) = 0.
                QDEUB(I,J) = 0.
                DQDEB(I,J) = 2. * (QBI(I,J,1) - WATER(I,J,1)) * RDETA(1)
                EDBD (I,J) = 0.
            END DO
        END DO
!
        DO L=1,LM1
!
!$omp parallel do private (DQDEK, EDBFK, EDTDK, QDEDTK, QDEUTK, SEDBK)
!
            DO J=1,JM
                DO I=1,IM
                    QDEDTK       = QDEDB(I,J)
                    QDEUTK       = QDEUB(I,J)
!
                    SEDBK        = SIGN(1., ETADT(I,J,L))
!
                    DQDEK        = DQDEB(I,J)
!
                    DQDEB(I,J)   = 2. * (QBI(I,J,L+1) - WATER(I,J,L+1)) * RDETA(L+1)
!
                    EDBFK        = ETADT(I,J,L) * F4D
!
                    QDEDB(I,J)   = (1. + SEDBK) * (QBI(I,J,L) + DQDEK * EDBFK) * (-EDBFK)
!
                    QDEUB(I,J)   = (1. - SEDBK) * (2.*WATER(I,J,L+1) - QBI(I,J,L+1)               &
    &                            + DQDEB(I,J) * EDBFK) * EDBFK
!
                    EDTDK        =  EDBD(I,J)
!
                    EDBD (I,J)   = ETADT(I,J,L) * (-F4Q)
!
                    WATER(I,J,L) = WATER(I,J,L) + (QDEDTK - QDEUTK - QDEDB(I,J) + QDEUB(I,J)      &
    &                            + WATER(I,J,L) * (EDBD(I,J) - EDTDK)) * RDETA(L)
                END DO
            END DO
        END DO
!
!$omp parallel do
!
        DO J=1,JM
            DO I=1,IM
                WATER(I,J,LM) = WATER(I,J,LM) + ( QDEDB(I,J)  - QDEUB(I,J)                        &
    &                         + WATER(I,J,LM) * (-EDBD(I,J))) * RDETA(LM)
            END DO
        END DO
!--------------------------------------------------------
! NEGATIVE MOISTURE MAY OCCUR DUE TO VIOLATION OF THE CFL
!--------------------------------------------------------
        DO L=1,LM1
!
!$omp parallel do
!
            DO J=1,JM
                DO I=1,IM
                    IF (WATER(I,J,L) < EPSQ) THEN
                         DQBI(I,J)     = WATER(I,J,L  )
                        WATER(I,J,L)   = EPSQ
                        WATER(I,J,L+1) = WATER(I,J,L+1) + DETA(L) * RDETA(L+1) * DQBI(I,J)
                    END IF
                END DO
            END DO
!
        END DO
!
!$omp parallel do
!
        DO J=1,JM
            DO I=1,IM
                IF (WATER(I,J,LM) < EPSQ) WATER(I,J,LM) = EPSQ
            END DO
        END DO
!
        IF (NWTR == 1) THEN
!
!$omp parallel do
!
            DO L=1,LM
                DO J=1,JM
                    DO I=1,IM
                        Q(I,J,L) = WATER(I,J,L)
                    END DO
                END DO
            END DO
!
        ELSE
!
!$omp parallel do
!
            DO L=1,LM
                DO J=1,JM
                    DO I=1,IM
                        CWM(I,J,L) = WATER(I,J,L)
                    END DO
                END DO
            END DO
!
        END IF
!
    END DO
!
    DEALLOCATE(WATER)
!-------------------------------------------
! DEFINITION OF VARIABLES NEEDED AT V POINTS
!-------------------------------------------
    DO J=1,JM
        DO I=1,IM
            PDSQ(I,J) = PDSL(I,J) * SQH(I,J)
        END DO
    END DO
!          
    CALL AVRH(PDSQ, PDXY, IM, JM)
!
    DO J=1,JM1
        DO I=1,IM1
            RPDXY(I,J) = 0.25
        END DO
    END DO

!--------------------------------------
! VERTICAL (MATSUNO) ADVECTION OF U & V
!--------------------------------------
    CALL ZERO(TUB,TVB)
!
    DO L=1,LM1
!
        ETADTL = ETADT(0:IM+1, 0:JM+1, L)
!
        CALL AVRH(ETADTL, ETADTXY, IM, JM)
!
        DO J=1,JM1
            DO I=1,IM1
                VMF4D       = VTM(I,J,L+1) * F4D
                TUAK        = ETADTXY(I,J) * (U(I,J,L+1) - U(I,J,L)) * RPDXY(I,J) * VMF4D
                USTL(I,J,L) = (TUAK + TUB(I,J)) * RDETA(L) + U(I,J,L)
                 TUB(I,J)   = TUAK
                TVAK        = ETADTXY(I,J) * (V(I,J,L+1) - V(I,J,L)) * RPDXY(I,J) * VMF4D
                VSTL(I,J,L) = (TVAK + TVB(I,J)) * RDETA(L) + V(I,J,L)
                 TVB(I,J)   = TVAK
            END DO
        END DO
    END DO
!
    DO J=1,JM1
        DO I=1,IM1
            USTL(I,J,LM) = U(I,J,LM) + TUB(I,J) * RDETA(LM)
            VSTL(I,J,LM) = V(I,J,LM) + TVB(I,J) * RDETA(LM)
        END DO
    END DO
!-------------------------------
! SECOND (BACKWARD) MATSUNO STEP
!-------------------------------
    CALL ZERO(TUB, TVB)
!
    DO L=1,LM1
!
        ETADTL = ETADT(0:IM+1, 0:JM+1, L)
!
        CALL AVRH(ETADTL, ETADTXY, IM, JM)
!
        DO J=1,JM1
            DO I=1,IM1
                VMF4D            = VTM(I,J,L+1) * F4D
                TUAK             = ETADTXY(I,J) * (USTL(I,J,L+1) - USTL(I,J,L)) * RPDXY(I,J)*VMF4D
                VAD_TEND1(I,J,L) = (TUAK + TUB(I,J)) * RDETA(L)
                      TUB(I,J)   =  TUAK
                TVAK             = ETADTXY(I,J) * (VSTL(I,J,L+1) - VSTL(I,J,L)) * RPDXY(I,J)*VMF4D
                VAD_TEND2(I,J,L) = (TVAK + TVB(I,J)) * RDETA(L)
                      TVB(I,J)   =  TVAK
            END DO
        END DO
    END DO
!
    DO J=1,JM1
        DO I=1,IM1
            VAD_TEND1(I,J,LM) = TUB(I,J) * RDETA(LM)
            VAD_TEND2(I,J,LM) = TVB(I,J) * RDETA(LM)
        END DO
    END DO
!-----------------------------------------------------------------------
! IF THE CFL CRITERION IS VIOLATED THEN VERTICALLY SMOOTH THE TENDENCIES
!-----------------------------------------------------------------------
    DO J=1,JM1
        DO I=1,IM1
!
            IF (LTOP_CFL_M(I,J) > 0) THEN
!
                LSTART =     LTOP_CFL_M(I,J)
                LSTOP  = MIN(LBOT_CFL_M(I,J), LM-1)
!
                DO L=LSTART,LSTOP
                    VAD_TNDX1(L) =     (VAD_TEND1(I,J,L-1)  + VAD_TEND1(I,J,L+1)                  &
    &                            +2. *  VAD_TEND1(I,J,L  )) * 0.25
!
                    VAD_TNDX2(L) =     (VAD_TEND2(I,J,L-1)  + VAD_TEND2(I,J,L+1)                  &
    &                            + 2. * VAD_TEND2(I,J,L  )) * 0.25
                END DO
!
                DO L=LSTART,LSTOP
                    VAD_TEND1(I,J,L) = VAD_TNDX1(L)
                    VAD_TEND2(I,J,L) = VAD_TNDX2(L)
                END DO
!
            END IF
!
        END DO
    END DO
!
    DO L=1,LM
        DO I=1,IM1
            DO J=1,JM1
                U(I,J,L) = U(I,J,L) + VAD_TEND1(I,J,L)
                V(I,J,L) = V(I,J,L) + VAD_TEND2(I,J,L)
            END DO
        END DO
    END DO
!
    CALL BOCOHMPI(T  , LM)
    CALL BOCOHMPI(Q2 , LM)
    CALL BOCOVMPI(U  , V , LM)
    CALL BOCOHMPI(Q  , LM)
    CALL BOCOHMPI(CWM, LM)
!
    END SUBROUTINE VTADV
