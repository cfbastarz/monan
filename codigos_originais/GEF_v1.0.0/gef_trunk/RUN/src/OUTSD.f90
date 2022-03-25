!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief VERTICALY INTERPOLATE FROM SIGMA TO (FOR OUTPUT)
!> @details VERTICALY INTERPOLATE FROM SIGMA TO (FOR OUTPUT) 'STANDARD'PRESSURE VALUES. 
!! USE CUBIC SPLINE FOR INTERPOLATION AND LAGRANGIAN FORMULA FOR EXTRAPOLATION (IF NEEDED). 
!! M. RANCIC (MAY 2002) 
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
!> @param[out] POUT - Significado de POUT
!> @param[out] TOUT - Significado de TOUT
!> @param[out] FOUT - Significado de FOUT
!> @param[out] UOUT - Significado de UOUT
!> @param[out] VOUT - Significado de VOUT
!> @param[out] QOUT - Significado de QOUT
!> @param[out] WOUT - Significado de WOUT
!> @details <b>Use Module:</b>
!! @arg @c CLDWTR
!! @arg @c CONTIN
!! @arg @c CTLBLK
!! @arg @c DGNSOUT
!! @arg @c DYNAM
!! @arg @c F77KINDS
!! @arg @c LOOPS
!! @arg @c MASKS
!! @arg @c MPPSTAFF
!! @arg @c PARMETA
!! @arg @c PVRBLS
!! @arg @c REALPAR
!! @arg @c VRBLS
!> @details <b>Driver:</b> 
!! @arg @c OUT
!> @details <b>Calls:</b>
!! @arg @c AVRH
!! @arg @c SPLINE
!! @arg @c SPLINT
!--------------------------------------------------------------------------------------------------      
	SUBROUTINE OUTSD(POUT, TOUT, FOUT, UOUT, VOUT, QOUT, WOUT)
!-------------------------------------------------------------------------------------------------- 
! SUBROUTINE OUTSD
!
! SUBROUTINE: OUTSD - VERTICALY INTERPOLATE FROM SIGMA TO (FOR OUTPUT)
! PROGRAMMER: ?????  
! ORG: ?????
! DATE: ??-??-??
!
! ABSTRACT: 
! VERTICALY INTERPOLATE FROM SIGMA TO (FOR OUTPUT) 'STANDARD'PRESSURE VALUES. 
! USE CUBIC SPLINE FOR INTERPOLATION AND LAGRANGIAN FORMULA FOR EXTRAPOLATION (IF NEEDED). 
! M. RANCIC (MAY 2002) 
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
! POUT - 
! TOUT - 
! FOUT - 
! UOUT -
! VOUT -
! QOUT - 
! WOUT -
!
! INPUT/OUTPUT ARGUMENT LIST:
! NONE
!
! OUTPUT FILES:
! OUTPUT - PRINT FILE.
!
! USE MODULES: CLDWTR
!              CONTIN
!              CTLBLK
!              DGNSOUT
!              DYNAM
!              F77KINDS
!              LOOPS
!              MASKS
!              MPPSTAFF
!              PARMETA
!              PVRBLS
!              REALPAR
!              VRBLS
!
! DRIVER     : GEF
!
! CALLS      : AVRH
!              SPLINE
!              SPLINT 
!--------------------------------------------------------------------------------------------------
    USE CLDWTR
    USE CONTIN
    USE CTLBLK
    USE DGNSOUT
    USE DYNAM
    USE F77KINDS
    USE LOOPS
    USE MASKS
    USE MPPSTAFF
    USE PARMETA
    USE PVRBLS
    USE REALPAR
    USE VRBLS
!
    IMPLICIT NONE
!
    REAL   (KIND=R4)    , PARAMETER :: GI    =    1. / 9.80616
    REAL   (KIND=R4)    , PARAMETER :: DP    = 6000.
    REAL   (KIND=R4)    , PARAMETER :: H1M12 =    1.E-12
    REAL   (KIND=R4)    , PARAMETER :: P1000 = 1000.E2
    REAL   (KIND=R4)    , PARAMETER :: CAPA  =    0.28589641
    REAL   (KIND=R4)    , PARAMETER :: RD    =  287.04
    REAL   (KIND=R4)    , PARAMETER :: GAMMA =    6.5E-3
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1)                       , INTENT(OUT)         ::&
    & POUT
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1, LP1)                                        ::&
    & ALPINT
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1, LSD)                  , INTENT(OUT)         ::&
    & TOUT    , FOUT    , UOUT    , VOUT    , QOUT    , WOUT
!
    REAL   (KIND=R4)    , DIMENSION(LM)                                                         ::&
    & PSG     , TL      , AFI     , TL2     ,  ULL    , VLL     , UL2     , VL2     , QL      ,   &
    & QL2     , WL      , WL2
!
    REAL   (KIND=R4)    , DIMENSION(LP1)                                                        ::&
    & PETA    , APETA   , FI      , FL2
!
    REAL   (KIND=R4)    , DIMENSION(LSD)                                                        ::&
    & TSD     , FSD     , USD     , VSD     , QSD     , WSD
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1)                                             ::&
    & APD     , APDSL   , QSFC    , TSFC    , TBAR    , QBAR    , ALPBAR
!
    REAL   (KIND=R4)                                                                            ::&
    & ZL      , TVRTL   , TVRT    , PSFC    , THSFC   , TSUM    , QSUM    , RNLEV   , PBI     ,   &
    & ZBAR    , TAVG    , QAVG    , TVRBAR  , ZSFC    , ALPSFC  , PTOP    , ALPS    , ALPM    ,   &
    & PM
!
    INTEGER(KIND=I4)                                                                            ::&
    & I       , J       , L       , LT      , LLMH    , NLEV
!
    REAL   (KIND=R4)                                                                            ::&
    & PSURF   , YP1     , YP2
!
    IF (SIGMA) THEN
        PDSL = PD
    ELSE
        PDSL = RES * PD
    END IF
!
    DO 30 J=1,JM
        DO 30 I=1,IM
!
            LLMH = LMH(I,J)
!
            DO L=1,LLMH+1
                PBI           = PT + PDSL(I,J) * ETA(L)
                ALPINT(I,J,L) = ALOG(PBI)
            END DO
!-------------------------------------------------------    
! LOCATE TOP OF LAYER OVER WHICH TO COMPUTE MEAN FIELDS.
!-------------------------------------------------------
            PSFC      = PD(I,J) + PT
            PTOP      = PSFC    - DP
            QSFC(I,J) = (1. - SM(I,J)) *  QS(I,J) + SM(I,J) *  QZ0(I,J)
            QSFC(I,J) = AMAX1(H1M12,QSFC(I,J))
            THSFC     = (1. - SM(I,J)) * THS(I,J) + SM(I,J) * THZ0(I,J)
            TSFC(I,J) = THSFC * (P1000 / PSFC) ** (-CAPA)
            ALPS      = D50   * (ALPINT(I,J,LLMH) + ALPINT(I,J,LLMH+1))
!---------------------
! COMPUTE MEAN FIELDS.
!---------------------
            NLEV = 1
            TSUM = T(I,J,LLMH)
            QSUM = Q(I,J,LLMH)
!
            ALPBAR(I,J) = ALPS
              TBAR(I,J) = TSUM
              QBAR(I,J) = QSUM
!
            IF (LLMH == LM) GOTO 30
!
            DO 10 L = LLMH-1,1,-1
                ALPM = D50 * (ALPINT(I,J,L) + ALPINT(I,J,L+1))
	        PM   = EXP(ALPM)
!
                IF (PM < PTOP) GOTO 20
!
                NLEV = NLEV + 1
                ALPS = ALPS + ALPM
                TSUM = TSUM + T(I,J,L)
                QSUM = QSUM + Q(I,J,L)
 10         CONTINUE
!
 20     CONTINUE
!
        RNLEV       = 1.   /  NLEV
        ALPBAR(I,J) = ALPS * RNLEV
          TBAR(I,J) = TSUM * RNLEV
          QBAR(I,J) = QSUM * RNLEV
!
 30 CONTINUE
!
    FI = 0.
!-----------------------------------
! VARIABLES DEFINED AT SCALAR POINTS
!-----------------------------------
    DO I = 1,IM
        DO J = 1,JM
!
            DO L = 1, LM
                PSG(L) = PT + AETA(L) * PDSL(I,J)
!
	        IF (HTM(I,J,L) > 0) THEN
                    TL(L) =   T(I,J,L)
                    QL(L) =   Q(I,J,L)
                    WL(L) = CWM(I,J,L)
	        ELSE
	            TL(L) = TL(L-1)
	            QL(L) = QL(L-1)
	            WL(L) = WL(L-1)
                END IF
!
            END DO
!
            PSFC   =   PD(I,J) + PT
            ALPSFC = ALOG(PSFC)
            ZSFC   =  FIS(I,J) * GI
            LLMH   =  LMH(I,J)
            TVRT   = TBAR(I,J) * (H1 + D608 * QBAR(I,J))
            QAVG   = D50       * (QSFC(I,J) + QBAR(I,J))
            TAVG   = D50       * (TSFC(I,J) + TBAR(I,J))
            TVRBAR = TAVG * (H1 + D608 * QAVG)
            ZBAR   = RD * GI * TVRBAR * (ALPSFC - ALPBAR(I,J)) + ZSFC

            DO 40 L = LLMH+1,LM
                ZL     = GI   * DFL(L)
                TVRTL  = TVRT + (ZBAR - ZL) * GAMMA
                TL(L) = TVRTL / (H1 + D608 * QBAR(I,J))
 40         CONTINUE
!	    
            DO L=LLMH,LM
                ALPINT(I,J,L+1) = (DFL(L) - DFL(L+1)) / (R * TL(L)) + ALPINT(I,J,L)
!
            END DO
!
            APETA = ALPINT(I,J,:)
            PETA  = EXP(APETA)
!       
            IF (SIGMA) THEN
                FI(LP1) = FIS(I,J)
            ELSE
                FI(LP1) = D00
            END IF
!
            DO L = LM, 1, -1
                IF (HTM(I,J,L) == 0) THEN
	            FI(L) = DFL(L)
                ELSE
                    FI(L) = FI(L+1) + R * TL(L) * (H1+D608 * QL(L)) * (APETA(L+1) - APETA(L))
                END IF
!
            END DO
!
            DO L = 1, LM
                AFI(L) = 0.5 * (FI(L) + FI(L+1))
            END DO
!
            DO L=2,LM
	        IF (TL(L) == 0) THEN
	            TL(L) = TL(L-1)
                END IF
            END DO
!
            IF (LM == 1) THEN
	        POUT(I,J)   = PSURF
	        TOUT(I,J,:) =  TL(1)
	        QOUT(I,J,:) =  QL(1)
	        WOUT(I,J,:) =  WL(1)
	        FOUT(I,J,:) = AFI(1)
            ELSE
!---------------------
! PREPARE CUBIC SPLINE
!---------------------
                YP1 = 0.
                YP2 = 0.
!
                CALL SPLINE(PSG , TL, LM , YP1, YP2, TL2)
                CALL SPLINE(PSG , QL, LM , YP1, YP2, QL2)
                CALL SPLINE(PSG , WL, LM , YP1, YP2, WL2)
                CALL SPLINE(PETA, FI, LP1, YP1, YP2, FL2)
!
                DO L = 1, LSD
                    IF (PSD(L) * 100 > PSG(LM)) THEN
                        TSD(L) = TL(LM)
                        QSD(L) = QL(LM)
                        WSD(L) = WL(LM)
                    ELSE
                        CALL SPLINT(PSG, TL, TL2, LM, PSD(L)*100., TSD(L))
                        CALL SPLINT(PSG, QL, QL2, LM, PSD(L)*100., QSD(L))
                        CALL SPLINT(PSG, WL, WL2, LM, PSD(L)*100., WSD(L))
                    END IF
!
                    CALL SPLINT(PETA, FI, FL2, LP1, PSD(L)*100., FSD(L))
!
                    TOUT(I,J,L) = TSD(L)
                    QOUT(I,J,L) = QSD(L)
                    WOUT(I,J,L) = WSD(L)
                    FOUT(I,J,L) = FSD(L) * GI        ! CHANGE TO HEIGHT
                END DO
!
                POUT(I,J) = PSFC
!
            END IF
!
        END DO
    END DO
!---------------------------------
! VARIABLES DEFINED AT WIND POINTS
!---------------------------------
    CALL AVRH(PD  , APD  , IM, JM)
    CALL AVRH(PDSL, APDSL, IM, JM)
!
      APD(:,:) =   APD(:,:) * D25
    APDSL(:,:) = APDSL(:,:) * D25
!      
    DO I = 1, IM1
        DO J = 1, JM1
            PSURF = APD(I,J) + PT
            DO L = 1, LM
                PSG(L) = PT + AETA(L) * APDSL(I,J)
!
	        IF (VTM(I,J,L) > 0) THEN
                    ULL(L) = U(I,J,L)
                    VLL(L) = V(I,J,L)
                ELSE
	            ULL(L) = ULL(L-1)
	            VLL(L) = VLL(L-1)
                END IF
!
            END DO
!
            DO L = 1,LP1
                PETA(L) = PT + ETA(L) * APDSL(I,J)
!
                IF (PETA(L) /= 0) THEN
                    APETA(L) = ALOG(PETA(L))
                ELSE
	            APETA(L) = 0.
                END IF
!
            END DO
!
            IF (LM == 1) THEN
	        UOUT(I,J,:) = ULL(1)
	        VOUT(I,J,:) = VLL(1)
            ELSE
!---------------------
! PREPARE CUBIC SPLINE
!---------------------
                YP1 = 0.
                YP2 = 0.
!
                CALL SPLINE(PSG, ULL, LM, YP1, YP2, UL2)
                CALL SPLINE(PSG, VLL, LM, YP1, YP2, VL2)
!
                DO L = 1, LSD
!
                    IF (PSD(L) * 100 > PSG(LM)) THEN
                        USD(L) = ULL(LM)
	                VSD(L) = VLL(LM)
                    ELSE
                        CALL SPLINT(PSG, ULL, UL2, LM, PSD(L)*100., USD(L))
                        CALL SPLINT(PSG, VLL, VL2, LM, PSD(L)*100., VSD(L))
                    END IF
!
                    UOUT(I,J,L) = USD(L)
                    VOUT(I,J,L) = VSD(L)
!
                END DO
!
            END IF
!
        END DO
!
    END DO
!
    END SUBROUTINE OUTSD
