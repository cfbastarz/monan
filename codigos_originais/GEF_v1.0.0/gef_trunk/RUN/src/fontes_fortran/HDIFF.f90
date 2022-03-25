!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief HORIZONTAL DIFFUSION
!> @details CALCULATES THE CONTRIBUTION OF THE HORIZONTAL DIFFUSION TO THE TENDENCIES OF TEMPERATURE,
!! SPECIFIC HUMIDITY, WIND COMPONENTS AND TURBULENT KINETIC ENERGY AND THEN UPDATES THOSE VARIABLES.
!! A SECOND-ORDER NONLINEAR SCHEME SIMILAR TO SMAGORINSKYS IS USED WHERE THE DIFFUSION COEFFICIENT 
!! IS A FUNCTION OF THE DEFORMATION FIELD AND OF THE TURBULENT KINETIC ENERGY.
!> @author ORIGINATOR - JANJIC 
!> @date 93-11-17 \n
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
!> @param[in] IM - Significado de IM
!> @param[in] JM - Significado de JM
!> @param[in] LM - Significado de LM
!> @details <b>Use Module:</b>
!! @arg @c CONTIN 
!! @arg @c CTLBLK
!! @arg @c DYNAM
!! @arg @c F77KINDS
!! @arg @c MASKS
!! @arg @c METRCS
!! @arg @c MPPSTAFF
!! @arg @c PVRBLS
!! @arg @c REALPAR    , ONLY: D00,D25,RDH,H1
!! @arg @c TPRFIL
!! @arg @c VRBLS
!> @details <b>Driver:</b> 
!! @arg @c GEF
!> @details <b>Calls:</b>
!! @arg @c BOCOHMPI
!! @arg @c BOCOVMPI
!--------------------------------------------------------------------------------------------------
    SUBROUTINE HDIFF
!--------------------------------------------------------------------------------------------------
! SUBROUTINE HDIFF
!
! SUBPROGRAM: HDIFF - HORIZONTAL DIFFUSION
! PROGRAMMER: JANJIC
! ORG: W/NP22
! DATE: 93-11-17
!
! ABSTRACT:
! HDIFF CALCULATES THE CONTRIBUTION OF THE HORIZONTAL DIFFUSION TO THE TENDENCIES OF TEMPERATURE,
! SPECIFIC HUMIDITY, WIND COMPONENTS AND TURBULENT KINETIC ENERGY AND THEN UPDATES THOSE VARIABLES.
! A SECOND-ORDER NONLINEAR SCHEME SIMILAR TO SMAGORINSKYS IS USED WHERE THE DIFFUSION COEFFICIENT 
! IS A FUNCTION OF THE DEFORMATION FIELD AND OF THE TURBULENT KINETIC ENERGY.
!
! PROGRAM HISTORY LOG:
! 87-06-??  JANJIC     - ORIGINATOR
! 95-03-25  BLACK      - CONVERSION FROM 1-D TO 2-D IN HORIZONTAL
! 96-03-28  BLACK      - ADDED EXTERNAL EDGE
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
! USE MODULES: CONTIN
!              CTLBLK
!              DYNAM
!              F77KINDS
!              MASKS
!              METRCS
!              MPPSTAFF
!              PVRBLS
!              REALPAR
!              TPRFIL
!              VRBLS
!
! DRIVER     : GEF
!
! CALLS      : BOCOHMPI
!              BOCOVMPI
!--------------------------------------------------------------------------------------------------
    USE CONTIN 
    USE CTLBLK
    USE DYNAM
    USE F77KINDS
    USE MASKS
    USE METRCS
    USE MPPSTAFF
    USE PARMETA , ONLY: IM, JM, LM
    USE PVRBLS
    USE REALPAR , ONLY: D00,D25,RDH,H1
    USE TPRFIL
    USE VRBLS
    USE CLDWTR
!
    IMPLICIT NONE
!
    INCLUDE "mpif.h"
!
    REAL   (KIND=R4)    , DIMENSION(4)                                                          ::&
    & UNEBT   , VNEBT
!
    INTEGER(KIND=I4)    , DIMENSION(4)                                                          ::&
    & IT      , JT
!
    INTEGER(KIND=I4)                                                                            ::&
    & L       , I       , J       , ISCORNER2         , K       , N       , IERR
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1)                                             ::&
    & UG      , VG      , GCHUT   , GCHVT , GCHI      , HMASK   , GMTXP   , GMTYP   , SX      ,   &
    & SY
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1, LM)                                         ::&
    & TT      , TT1     , UT      , VT      , UT1     , VT1
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1, LM)                                         ::&
    & QT      , QT1     , Q2T     , Q2T1
!--------------------------
! CHOU INCLUDED CLOUD WATER
!--------------------------
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1, LM)                                         ::&
    & CWMT      , CWMT1   
!
    REAL   (KIND=R4)                                                                            ::&
    & UNEB    , VNEB    , TMPU    , TMPV    , SCOR    ,                                           &
    & DIFN    ,                                                                                   &
    & DH1     , DH2     , DH3     , DH4     , DHM     , HM      , VM
! 
    REAL   (KIND=R8)                                                                            ::&
    & TIMEF   , TTMPF
!
    INTEGER(KIND=I4)                                                                            ::&
    & ITER    , ITER_MAX
!
    ITER_MAX = 4
!
    HMASK = H1
!
    IF (SIGMA) THEN
        DO J=1,JM
            DO I=1,IM
                DH1 = ABS(FIS(I  ,J-1) - FIS(I,J))
                DH2 = ABS(FIS(I+1,J  ) - FIS(I,J))
                DH3 = ABS(FIS(I  ,J+1) - FIS(I,J))
                DH4 = ABS(FIS(I-1,J  ) - FIS(I,J))
!
                DHM = AMAX1(DH1,DH2,DH3,DH4) * RDH
!
                IF (DHM > 0.100) THEN
                    HMASK(I,J) = D00
                END IF
!
            END DO
        END DO   
    END IF
!------------
! TEMPERATURE
!------------
    TT(0:IM+1, 0:JM+1, 1:LM)   = T(0:IM+1, 0:JM+1, 1:LM)
!----------------------------------------------------
! ADDED SPECIFIC HUMIDITY + CLOUD WATER CHOU 20190701 
!----------------------------------------------------
    QT(0:IM+1, 0:JM+1, 1:LM)   = Q(0:IM+1, 0:JM+1, 1:LM)
    CWMT(0:IM+1, 0:JM+1, 1:LM) = CWM(0:IM+1, 0:JM+1, 1:LM)
!-------------------------------------------------
! TEST DRAGAN-ODKOMENTARISANO KAO U STAROJ VERZIJI
!-------------------------------------------------
    Q2T(0:IM+1, 0:JM+1, 1:LM) = Q2(0:IM+1, 0:JM+1, 1:LM)
!
    DO ITER=1,ITER_MAX
        TT1  = TT
!--------------------------------------------
! ADDED SPECIFIC HUMIDITY + CWM CHOU 20190613
!--------------------------------------------
        QT1  = QT
        CWMT1= CWMT
        Q2T1 = Q2T
!
        DO L=1,LM
!
            DO J=1,JM
                DO I=1,IM
!
                    IF (ISCORNER2(I,J) == 0) THEN
                        HM = D25 * HTM(I,J,L) * HMASK(I,J)
!
                        TT(I,J,L) = -((TT1(I+1, J  , L) - TT1(I, J, L)) * HTM(I+1, J  , L)        &
    &                             +   (TT1(I-1, J  , L) - TT1(I, J, L)) * HTM(I-1, J  , L)        &
    &                             +   (TT1(I  , J+1, L) - TT1(I, J, L)) * HTM(I  , J+1, L)        &
    &                             +   (TT1(I  , J-1, L) - TT1(I, J, L)) * HTM(I  , J-1, L)) * HM
!--------------------------------------------
! ADDED SPECIFIC HUMIDITY + CWM CHOU 20190613
!--------------------------------------------
                        QT(I,J,L) = -((QT1(I+1, J  , L) - QT1(I, J, L)) * HTM(I+1, J  , L)        &
    &                             +   (QT1(I-1, J  , L) - QT1(I, J, L)) * HTM(I-1, J  , L)        &
    &                             +   (QT1(I  , J+1, L) - QT1(I, J, L)) * HTM(I  , J+1, L)        &
    &                             +   (QT1(I  , J-1, L) - QT1(I, J, L)) * HTM(I  , J-1, L)) * HM
!
                        CWMT(I,J,L)=-((CWMT1(I+1, J, L)   - CWMT1(I, J, L)) * HTM(I+1, J  , L)        &
    &                             +   (CWMT1(I-1, J , L)  - CWMT1(I, J, L)) * HTM(I-1, J  , L)        &
    &                             +   (CWMT1(I  , J+1, L) - CWMT1(I, J, L)) * HTM(I  , J+1, L)        &
    &                             +   (CWMT1(I  , J-1, L) - CWMT1(I, J, L)) * HTM(I  , J-1, L)) * HM
!
                        Q2T(I,J,L) = -((Q2T1(I+1, J  , L) - Q2T1(I, J, L)) * HTM(I+1, J  , L)         &
    &                              +   (Q2T1(I-1, J  , L) - Q2T1(I, J, L)) * HTM(I-1, J  , L)         &
    &                              +   (Q2T1(I  , J+1, L) - Q2T1(I, J, L)) * HTM(I  , J+1, L)         &
    &                              +   (Q2T1(I  , J-1, L) - Q2T1(I, J, L)) * HTM(I  , J-1, L)) * HM
                    ELSE
                         TT(I,J,L) = D00
!--------------------------------------------
! ADDED SPECIFIC HUMIDITY + CWM CHOU 20190613
!--------------------------------------------
                         QT(I,J,L) = D00
                         CWMT(I,J,L) = D00
                        Q2T(I,J,L) = D00
                    END IF
!
                END DO
            END DO
!---------------------------
! SLOPE CORRECTION FOR SIGMA
!--------------------------- 
            IF (SIGMA .AND. N == 1) THEN
                GMTXP = 0.
                GMTYP = 0.
!
                DO J=1,JM
                    DO I=1,IM
                        IF (PD(I+1,J) /= PD(I-1,J)) THEN
                            GMTXP(I,J) = (TT1(I+1,J,L) - TT1(I-1,J,L)) / (PD(I+1,J) - PD(I-1,J))
                        END IF
!
                        IF (PD(I,J+1) /= PD(I,J-1)) THEN
                            GMTYP(I,J) = (TT1(I,J+1,L) - TT1(I,J-1,L)) / (PD(I,J+1) - PD(I,J-1))
                        END IF
                    END DO
                END DO
! 
            END IF
!
        END DO
!     
!CHOU        IF (ITER < ITER_MAX) THEN  ITER_MAX used to be 2, then no iteration was done.
        IF (ITER <= ITER_MAX) THEN
            CALL BOCOHMPI(TT , LM)
!--------------------------------------------
! ADDED SPECIFIC HUMIDITY + CWM CHOU 20190613
!--------------------------------------------
            CALL BOCOHMPI(QT , LM)
            CALL BOCOHMPI(CWMT , LM)
            CALL BOCOHMPI(Q2T, LM)
        END IF
!
    END DO
!
    DO L=1,LM
        DO J=1,JM 
            DO I=1,IM  
                 HM       = RTDFDT    * HTM(I,J,L)
!
                 T(I,J,L) =  T(I,J,L) -  TT(I,J,L) * HM
!--------------------------------------------
! ADDED SPECIFIC HUMIDITY + CWM CHOU 20190613
!--------------------------------------------
                 Q(I,J,L) =  Q(I,J,L) -  QT(I,J,L) * HM
                 CWM(I,J,L)= CWM(I,J,L) -  CWMT(I,J,L) * HM
                Q2(I,J,L) = Q2(I,J,L) - Q2T(I,J,L) * HM
            END DO
        END DO
    END DO
!
    CALL BOCOHMPI(T , LM)
!--------------------------------------------
! ADDED SPECIFIC HUMIDITY + CWM CHOU 20190613
!--------------------------------------------
    CALL BOCOHMPI(Q , LM)
    CALL BOCOHMPI(CWM , LM)
    CALL BOCOHMPI(Q2, LM)
!---------
! VELOCITY
!---------
    UT = U
    VT = V
!
    DO ITER=1,ITER_MAX
        UT1 = UT
        VT1 = VT
!
        DO L=1,LM
            DO J=1,JM-1
                DO I=1,IM-1
                    IT = (/I-1, I+1, I  , I  /)
                    JT = (/J  , J  , J-1, J+1/)
!
                    TMPU = D00
                    TMPV = D00
                    DIFN = D00
!
                    DO K=1,4
                        UNEB = UT1(IT(K), JT(K), L)
                        VNEB = VT1(IT(K), JT(K), L)
!
                        UNEBT(K) = UNEB * QD11(I,J,K) + VNEB * QD12(I,J,K)
                        VNEBT(K) = UNEB * QD21(I,J,K) + VNEB * QD22(I,J,K)
!
                        TMPU = TMPU + UNEBT(K) * VTM(IT(K), JT(K), L)
                        TMPV = TMPV + VNEBT(K) * VTM(IT(K), JT(K), L)
!
                        DIFN = DIFN + VTM(IT(K), JT(K), L)
                    END DO
!
                    IF (DIFN /= D00) THEN 
                        UT(I,J,L) = -(TMPU - DIFN * UT1(I,J,L)) * D25
                        VT(I,J,L) = -(TMPV - DIFN * VT1(I,J,L)) * D25
                    END IF
!
                END DO
            END DO
        END DO
!
        IF (ITER<ITER_MAX) THEN
            CALL BOCOVMPI(UT,VT,LM)
        END IF
!
    END DO
!
    DO L=1,LM
        DO J=1,JM-1
            DO I=1,IM-1
                VM = RTDFDT * VTM(I,J,L)
!
                U(I,J,L) = U(I,J,L) - UT(I,J,L) * VM
                V(I,J,L) = V(I,J,L) - VT(I,J,L) * VM
            END DO
        END DO
    END DO
!
    CALL BOCOVMPI(U,V,LM)
!
    END SUBROUTINE HDIFF
