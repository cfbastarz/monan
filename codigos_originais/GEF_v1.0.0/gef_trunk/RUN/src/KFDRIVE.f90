!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief CONVECTIVE PRECIPITATION PARAMETERIZATION
!> @details MAIN DRIVER FOR THE KAIN-FRITSCH CONVECTION.
!! GRID POINTS ARE ISOLATED TO BE CHECKED FOR POSSIBLE INITAITION OF THE K-F SCHEME.
!> @author ORIGINATOR - KAIN 
!> @date 00-03-30 \n
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
!! @arg @c CONTIN
!! @arg @c CTLBLK
!! @arg @c DYNAM
!! @arg @c F77KINDS
!! @arg @c KFFDBK
!! @arg @c LOOPS
!! @arg @c MASKS
!! @arg @c MPPSTAFF
!! @arg @c PARMETA
!! @arg @c PHYS
!! @arg @c PVRBLS
!! @arg @c VRBLS
!> @details <b>Driver:</b> 
!<
!> @details <b>Calls:</b>
!! @arg @c KFPARA
!--------------------------------------------------------------------------------------------------
    SUBROUTINE KFDRIVE
!--------------------------------------------------------------------------------------------------
! SUBPROGRAM KFDRIVE
! 
! SUBPROGRAM: KFDRIVE - CONVECTIVE PRECIPITATION PARAMETERIZATION
! PROGRAMMER: KAIN          
! ORG: W/NP22
! DATE: 00-03-30
!
! ABSTRACT:
! KFDRIVE IS THE MAIN DRIVER FOR THE KAIN-FRITSCH CONVECTION.
! GRID POINTS ARE ISOLATED TO BE CHECKED FOR POSSIBLE INITAITION OF THE K-F SCHEME.
!
! PROGRAM HISTORY LOG:
! 00-03-30  KAIN       - ORIGINATOR
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
! USE MODULES: CONTIN
!              CTLBLK
!              DYNAM
!              F77KINDS
!              KFFDBK
!              LOOPS
!              MASKS
!              MPPSTAFF
!              PARMETA
!              PHYS
!              PVRBLS
!              VRBLS 
!
! DRIVER     : GEF
!
! CALLS      : KFPARA
!--------------------------------------------------------------------------------------------------
    USE CONTIN
    USE CTLBLK
    USE DYNAM
    USE F77KINDS
    USE KFFDBK
    USE LOOPS
    USE MASKS
    USE MPPSTAFF
    USE PARMETA
    USE PHYS
    USE PVRBLS
    USE VRBLS
!
    INCLUDE "mpif.h"
!
    REAL   (KIND=R4)    , PARAMETER :: XLV  =    2.5E6
    REAL   (KIND=R4)    , PARAMETER :: G    =    9.80616
!
    REAL   (KIND=R4)    , PARAMETER :: ALIQ =  613.3
    REAL   (KIND=R4)    , PARAMETER :: BLIQ =   17.502
    REAL   (KIND=R4)    , PARAMETER :: CLIQ = 4780.8
    REAL   (KIND=R4)    , PARAMETER :: DLIQ =   32.19 

    REAL   (KIND=R4)    , DIMENSION(LM)                                                         ::&
    & SEM     , SEMS    , PRS     , TV0     , Z00     , SED     , DP
!
    INTEGER(KIND=I4)    , DIMENSION(IM)                                                         ::&
    & ICUYES  , LSB     , NSHALL 
!
    ROVG = R / G
!----------------------------------- 
! NOTE: SEARCHING ONLY LOWEST 200 MB 
!----------------------------------- 
    DO L=1,LM
        DO J=1,JM 
            DO I=1,IM 	 
	        W0           = -OMGALF(I,J,L) * CP / (G * DT)
                W0AVG(I,J,L) =  (W0AVG(I,J,L) * (TST - 1.) + W0) / TST
            END DO
        END DO
    END DO
!
!    IF (MOD(NTSD - NCLDCK / 2, NCLDCK) /= 0) RETURN
!
    DO J=1,JM
        NSHALL(J) = 0
    END DO
!-----------------------------------------------------------------------------------------------
! MAKE A QUICK FOR CHECK FOR THESE THINGS THAT CAN ELIMINATE GRID POINTS FROM THE POSSIBILITY OF 
! CONVECTIVE INITIATION:
!
! 1.) CONVECTION ALREADY ACTIVE AT THIS POINT
! 2.) POINT CLOSE TO GRID-DOMAIN BOUNDARY
! 3.) DOWNWARD MOTION AT ALL LEVELS IN LOWEST 300 MB
! 4.) NO CAPE
!-----------------------------------------------------------------------------------------------
!
!$omp parallel do private                                                                         &
!$omp         (DP       , ES      , I       , ICUYES  , ISINK   , L       , L200    , L400    ,   &
!$omp          LG       , LSB     , NCUYES  , P200    , P400    , PRS     , PSFCK   , QSS     ,   &
!$omp          QTMP     , SED     , SEM     , SEMS    , TTMP    , TV0     , Z00)
!
    DO 100 J=1,JM
        NCUYES = 0
!
        DO I=0,IM+1
            ICUYES(I) = 0
               LSB(I) = 0
        END DO
!
        DO 60 I=1,IM  
            IF (NCA(I,J) > NCLDCK) GOTO 60
            LG    = LMH(I,J)
            PSFCK =  PD(I,J) * RES(I,J) * AETA(LG) + PT
            P200  = PSFCK - 2.E4
!            P300  = PSFCK - 3.E4
            P400  = PSFCK - 4.E4
!
            DO L=LG,1,-1
                PRS(L) = PD(I,J) * RES(I,J) * AETA(L) + PT  
            END DO
!
            DO L=LG,1,-1
                IF (PRS(L) > P200) L200 = L
                IF (PRS(L) > P400) L400 = L
            END DO 
!--------------------------------------------------------------------
! VERTICAL VELOCITY MUST BE UPWARD AT SOME LEVEL IN THE LOWEST 300 MB
! LOWEST 400 MB 09/25/97 JSK
!--------------------------------------------------------------------
            ISINK = 0
!
            DO L=LG,L400,-1
!            DO L=LG,L200,-1
!
!                IF (OMGALF(I,J,L) < 0.) GOTO 25
                IF (OMGALF(I,J,L) < 0. .OR. Q2(I,J,L) > 1.) GOTO 25
            END DO
!
            ISINK = 1
!
 25         CONTINUE
!-----------------------------------------------------------------------------------------
! CALCULATE MOIST STATIC ENERGY AND SATURATION MOIST STATIC ENERGY AT EACH VERTICAL LEVEL.
!-----------------------------------------------------------------------------------------
            DO L=LG,1,-1
                TTMP = T(I,J,L)
                QTMP = Q(I,J,L)
! 
                 DP(L) = (ETA(L+1) - ETA(L)) * PD(I,J) * RES(I,J)
                TV0(L) = TTMP * (1. + 0.608 * QTMP)
!
                IF (L == LG) THEN
                    Z00(L) = 0.
                ELSE
                    Z00(L) = Z00(L+1) - ROVG * 0.5 * (TV0(L) + TV0(L+1)) * ALOG(PRS(L) / PRS(L+1))
                END IF
!
                ES  = ALIQ  * EXP((BLIQ * TTMP - CLIQ) / (TTMP - DLIQ))
!
!CHOU                QSS = 0.622 * ES / (PRS(L) - ES)
!
                IF (PRS(L) <= 2500.) THEN
                    QSS= 1.0E-10
                ELSE
                    QSS = 0.622 * ES / (PRS(L) - ES)
                ENDIF
!
                 SED(L) = CP * TTMP + G * Z00(L)
                SEMS(L) = SED(L) + XLV * QSS
                 SEM(L) = SED(L) + XLV * QTMP
            END DO
!------------------------------------------------------------------------------------------- 
! IF AIR IS SINKING EVERYWHERE IN LOWEST 400 MB, 
! REQUIRE SUPERADIABATIC LAYER IN LOWEST 200 MB BEFORE CHECKING FOR CONVECTION IN KF SCHEME.
!-------------------------------------------------------------------------------------------
            IF (ISINK == 1) THEN
                DO L=LG,L400-1,-1
                    DO NL=L-1,L400,-1 
                        IF (SED(L) > SED(NL)) THEN
                            NCUYES = NCUYES + 1
                            ICUYES(NCUYES) = I
                            LSB(I) = LG - L + 1
                            GOTO 60   
                        END IF
                    END DO
                END DO
            ELSE
!--------------------------------------------------------------------------------------------------
! IF THERE IS UPWARD MOTION, REQUIRE CONDITIONAL INSTABILITY FOR A PARCEL ORIGINATING IN THE LOWEST
! 400 MB.
!--------------------------------------------------------------------------------------------------
                DO L=LG,L400,-1
                    DO NL=L-1,1,-1
                        IF (SEM(L) > SEMS(NL)) THEN
                            NCUYES = NCUYES + 1
                            ICUYES(NCUYES) = I
                            LSB(I) = LG - L + 1
                            GOTO 60
                        END IF
                    END DO
                END DO
!
            END IF
!--------------
! END OF I LOOP
!--------------
 60     CONTINUE
!
        IF (NCUYES > 0) THEN
            CALL KFPARA(NCUYES, ICUYES, J, LSB, NSHALL(J))
        END IF
!--------------
! END OF J LOOP
!--------------
100 CONTINUE
!
    RETURN
!
    END SUBROUTINE KFDRIVE     
