!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief CONVECTIVE PRECIPITATION PARAMETERIZATION
!> @details THE CENTRAL SUBROUTINE IN THE K-F PARAMETERIZATION.
!! SHALLOW CONVECTION IS INCLUDED WITHOUT CAPE DEPENDENCE.
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
!> @param[in] NCUYES  - NUMBER OF GRID POINTS TO CHECK ON THIS J-SLICE
!> @param[in] ICUYES  - ARRAY CONTAINING THE I-VALUES FOR GRID POINTS TO CHECK
!> @param[in] J       - THE ROW NUMBER
!> @param[in] LSB     - ARRAY CONTAINING L VALUES FOR EACH NCUYES POINT AT WHICH CHECKS FOR CONVECTIVE 
!!                      INITAITION SHOULD BEGIN
!> @param[out] NSHALL - COUNTER FOR NUMBER OF SHALLOW CONVECTION POINTS ACTIVATED
!> @details <b>Use Module:</b>
!! @arg @c ACMCLH
!! @arg @c CNVCLD
!! @arg @c CTLBLK
!! @arg @c DYNAM
!! @arg @c F77KINDS
!! @arg @c KFFDBK
!! @arg @c KFLUT
!! @arg @c LOOPS
!! @arg @c MASKS
!! @arg @c METRCS
!! @arg @c MPPSTAFF
!! @arg @c PARMETA
!! @arg @c PHYS    , ONLY : HTOP, HBOT, CNVTOP, CNVBOT
!! @arg @c PVRBLS
!! @arg @c VRBLS
!> @details <b>Driver:</b> 
!! @arg @c KFDRIVE
!> @details <b>Calls:</b>
!! @arg @c CONDLOAD
!! @arg @c DTFRZNEW
!! @arg @c ENVIRTHT
!! @arg @c PROF5
!! @arg @c TPMIX2
!! @arg @c TPMIX2DD
!! @arg @c TP_CAPE
!--------------------------------------------------------------------------------------------------
    SUBROUTINE KFPARA(NCUYES, ICUYES, J, LSB, NSHALL)
!--------------------------------------------------------------------------------------------------
! SUBPROGRAM KFPARA
! 
! SUBPROGRAM: KFPARA - KFPARA CONVECTIVE PRECIPITATION PARAMETERIZATION
! PROGRAMMER: KAIN          
! ORG: W/NP22
! DATE: 00-04-13
!
! ABSTRACT:
! KFPARA IS THE CENTRAL SUBROUTINE IN THE K-F PARAMETERIZATION.
! SHALLOW CONVECTION IS INCLUDED WITHOUT CAPE DEPENDENCE.
!
! PROGRAM HISTORY LOG:
! 00-04-13  KAIN       - ORIGINATOR
! 18-01-15  LUCCI      - MODERNIZATION OF THE CODE, INCLUDING:
!                        * F77 TO F90/F95
!                        * INDENTATION & UNIFORMIZATION CODE
!                        * REPLACEMENT OF COMMONS BLOCK FOR MODULES
!                        * DOCUMENTATION WITH DOXYGEN
!                        * OPENMP FUNCTIONALITY
!
! INPUT ARGUMENT LIST:
! NCUYES - NUMBER OF GRID POINTS TO CHECK ON THIS J-SLICE
! ICUYES - ARRAY CONTAINING THE I-VALUES FOR GRID POINTS TO CHECK
! J      - THE ROW NUMBER
! LSB    - ARRAY CONTAINING L VALUES FOR EACH NCUYES POINT AT WHICH CHECKS FOR CONVECTIVE 
!          INITAITION SHOULD BEGIN
!
! OUTPUT ARGUMENT LIST:
! NSHALL - COUNTER FOR NUMBER OF SHALLOW CONVECTION POINTS ACTIVATED
!
! INPUT/OUTPUT ARGUMENT LIST:
! NONE
!
! USE MODULES: ACMCLH
!              CNVCLD
!              CTLBLK
!              DYNAM
!              F77KINDS
!              KFFDBK
!              KFLUT
!              LOOPS
!              MASKS
!              METRCS
!              MPPSTAFF
!              PARMETA
!              PHYS    , ONLY : HTOP, HBOT, CNVTOP, CNVBOT
!              PVRBLS
!              VRBLS
! 
! DRIVER     : KFDRIVE
!
! CALLS      : CONDLOAD
!              DTFRZNEW
!              ENVIRTHT
!              PROF5
!              TPMIX2
!              TPMIX2DD
!              TP_CAPE
!--------------------------------------------------------------------------------------------------
!
!---------------------------------------------------------------
! ETA INPUT: TEMPERATURE (T, K); SPECIFIC HUMIDITY (Q, KG/KG)
! PD, RES, AND PT USED TO CALCULATE PRESSURE (PASCAL) 
! HORIZONTAL WIND SPEED (U AND V, M/S) 
! HORIZONTAL GRID SPACING (DX, M) 
! MODEL TIME STEP (DT, SECONDS) 
! NUMBER OF TIME STEPS INTEGRATED (NTSD, NO UNITS) 
! INSTANTANEOUS SURFACE SENSIBLE HEAT FLUX (TWBS, W/M^2) 
! INSTANTANEOUS SURFACE LATENT HEAT FLUX (QWBS, W/M^2) 
! ETA MODEL LEVELS (ETA, NO UNITS) 
! HALF ETA MODEL LEVELS (AETA, NO UNITS) 
! MODEL LEVEL OF TOP OF PBL (PBLLEV) 
! DIMENSIONS OF MODEL GRID (IMX, JMX, KMX FOR IM, JM, LM) 
! ARRAYS SPECIFYING WHERE H AND V ARRAYS ARE BELOW 
! GROUND (LMH AND VMH, NO UNITS) 
! 
! OUTPUT: CONVECTIVE TENDENCIES OF TEMPERATURE (DTDT), WATER 
! VAPOR (DQDT), CLOUD WATER (DQCDT), RAIN WATER (DQRDT) 
! !!!!! NOTE: CLOUD WATER AND RAINWATER ARRAYS (DQCDT AND DQRDT) 
! ARRAYS REMOVED FOR IMPLEMENTATION IN ETA MODEL 
! AND PRECIPITATION RATE (RAINCV) 
! HORIZ. LOCATION OF ACTIVE CONVECTION (NCA, NO UNITS) 
!---------------------------------------------------------------
! 
!--------------------------------------------------------------------------------------------------
! REFERENCES: C
! KAIN AND FRITSCH (1993): "CONVECTIVE PARAMETERIZATION IN MESOSCALE MODELS:
!                           THE KAIN-FRITSCH SCHEME"
!                           IN REPRESENTATION OF CUMULUS CONVECTION IN NUMERICAL MODELS, A.M.S.
! 
! MONOGRAPH, K.A. EMANUEL AND D.J. RAYMOND, EDS., 165-170. 
! 
! FRITSCH AND KAIN (1993): "CONVECTIVE PARAMETERIZATION IN MESOSCALE MODELS:
!                           THE FRITSCH-CHAPPELL SCHEME"
!                           IN REPRESENTATION OF CUMULUS CONVECTION IN NUMERICAL MODELS, A.M.S.C
!
! MONOGRAPH, K.A. EMANUEL AND D.J. RAYMOND, EDS., 165-170.
! 
! STENSRUD AND FRITSCH (1994), MON. WEA. REV., 2084-2104. 
! 
! FRITSCH AND CHAPPELL (1980), J. ATMOS. SCI., 1722-1733. 
!--------------------------------------------------------------------------------------------------
    USE ACMCLH
    USE CNVCLD
    USE CTLBLK
    USE DYNAM
    USE F77KINDS
    USE KFFDBK
    USE KFLUT
    USE LOOPS
    USE MASKS
    USE METRCS
    USE MPPSTAFF
    USE PARMETA
    USE PHYS    , ONLY : HTOP, HBOT, CNVTOP, CNVBOT
    USE PVRBLS
    USE VRBLS
!
    IMPLICIT NONE
!
    INTEGER(KIND=I4)    , PARAMETER :: KX   = LM
    INTEGER(KIND=I4)    , PARAMETER :: KXP1 = KX + 1
    INTEGER(KIND=I4)    , PARAMETER :: ILX  = IM - 1
    INTEGER(KIND=I4)    , PARAMETER :: JLX  = JM - 1

    REAL   (KIND=R4)    , DIMENSION(KX)                                                         ::&
    & QUER    , TKE     , CLDHGT  , QSD     , DILFRC  , DDILFRC , THTEEG  , TGU     , QGU
!
    INTEGER(KIND=I4)    , DIMENSION(IM)                                                         ::&
    & ICUYES  , LSB
!-----------------------
! DEFINE LOCAL VARIABLES
!-----------------------
    REAL   (KIND=R4)    , DIMENSION(KXP1)                                                       ::&
    & P0P     , Q0      , THTA0   , OMG
!
    REAL   (KIND=R4)    , DIMENSION(KX)                                                         ::&
    & Z00     , T0      , TV0     , U0      , V0      , TU      , TVU     , QU      , TZ      ,   &
    & TVD     , QD      , QES     , THTES   , TG      , TVG     , QG      , WU      , WD      ,   &
    & EMS     , EMSD    , W0      , UMF     , UER     , UDR     , DMF     , DER     , DDR     ,   &
    & DZQ     , UMF2    , UER2    , UDR2    , DMF2    , DER2    , DDR2    , DZA     , THETEE  ,   &
    & THTAU   , THETEU  , THTAD   , THETED  , QLIQ    , QICE    , QLQOUT  , QICOUT  , PPTLIQ  ,   &
    & PPTICE  , DETLQ   , DETIC   , DETLQ2  , DETIC2  , RATIO   , RATIO2
!
    REAL   (KIND=R4)    , DIMENSION(KX)                                                         ::&
    & DOMGDP  , EXN     , RHOE    , TVQU    , DP      , RH      , EQFRC   , WSPD    , QDT     ,   &
    & FXM     , THTAG   , THTESG  , THPA    , THFXIN  , THFXOUT , QPA     , QFXIN   , QFXOUT  ,   &
    & QLPA    , QLFXIN  , QLFXOUT , QIPA    , QIFXIN  , QIFXOUT , QRPA    , QRFXIN  , QRFXOUT ,   &
    & QSPA    , QSFXIN  , QSFXOUT , QL0     , QLG     , QI0     , QIG     , QR0     , QRG     ,   &
    & QS0     , QSG
!
    REAL   (KIND=R4)    , DIMENSION(KX)                                                         ::&
    & RAINFB  , SNOWFB  
!        
    INTEGER(KIND=I4)    , DIMENSION(4)                                                          ::&
    & II1      , J1    
!
    INTEGER(KIND=I4)                                                                            ::&
    & K1      , NK      , NLAYRS
!
    REAL   (KIND=R4)                                                                            ::&
    & P00     , T00     , G       , RLF     , RHIC    , RHBC    , PIE     , TTFRZ   , TBFRZ   ,   &
    & C5      , RATE    , RV      , ALIQ    , BLIQ    , CLIQ    , DLIQ    , AICE    , BICE    ,   &
    & CICE    , DICE    , XLV0    , XLV1    , XLS0    , XLS1    , FBFRC   , SUMV    , ES      ,   &
    & ROVG    , GDRY    , DT2     , CHMAX   , DPTHMX  , QEF     , PM15    , TMIX    , QMIX    ,   &
    & ZMIX    , PMIX    , EMIX    , ASTRT   , AINC    , A1      , TP      , VALUE   , AINTRP  ,   &
    & TLOG    , TDPT    , TLCL    , TVLCL   , ZLCL    , DLP     , TENV    , QENV    , TVEN    ,   &
    & WKLCL   , WKL     , DTLCL   , U00     , QSLCL   , RHLCL   , DQSDT   , DTRH    , THMX    ,   &
    & TVAVG   , PLCL0   , RHOLCL  , WTW     , PLCL    , WLCL    , GDT     , DTTOT   , AU0     ,   &
    & VMFLCL  , UPOLD   , UPNEW   , ABE     , TRPPT   , DXR     , FDX     , FBFQV
!
    INTEGER(KIND=I4)                                                                            ::&
    & I       , J       , LC      , INDLU   , KLCL    , L44     , L5      , LLFC    , K       ,   &
    & KMIX    , LOW     , KPBL    , LCL     , LET     , IFLAG   , KFRZ
!
    INTEGER(KIND=I4)                                                                            ::&
    & NK1     , NC      , NCUYES  , NCHM    , ISHALL  , ML      , KL      , NLEV    , KLM     ,   &
    & NLOOP   , LFC     , NCCNT   , LTOP    , KSTART  , NJ      , LTOP1   , LTOPM1  , LVF     ,   &
    & NIC     , LFS     , KLFS    , ND      , ND1     , LDT     , LDB     , NDK     , LMAX    ,   &
    & NCOUNT  , NOITR   , ISTOP   , NSTEP   , NTC     , NSHALL  , LBOT
!
    REAL   (KIND=R4)                                                                            ::&
    & FRC1    , PPTMLT  , RNC     , TMB     , TMM     , BCOEFF  , ACOEFF  , QVDIFF  , TOPOMG  ,   &
    & CPM     , DQ      , BINC    , QESE    , ABEG    , THEU_ADJ, DABE    , AINCOLD , DFDA    ,   &
    & FABEOLD , FRC2    , QINIT   , QFNL    , DPT     , ERR2    , RELERR  , EMST    , TMA     ,   &
    & DTIME   , EVAC    , DTT     , ABSOMG  , ABSOMGTC, FRDP    , DTT1    , TDER2   , PPTFL2  ,   &
    & FABE    , STAB    , TKEMAX  , PPTLMT  , DTMELT  , AINCM2  , DDINC   , AINCMX  , AINCM1  ,   &
    & RHH     , DSSDT   , DTMP    , T1RH    , QSRH    , PPTFLX  , CPR     , CNDTNF  , UPDINC  ,   &
    & DPDD    , DPTMLT  , RDD     , RHBAR   , DPPP    , QSS 
!
    REAL   (KIND=R4)                                                                            ::&
    & SHSIGN  , VWS     , PEF     , PEFF    , PEFF2   , TDER    , CBH     , RCBH    , PEFCBH  ,   &
    & USR     , VCONV   , TIMEC   , TADVEC  , P150    , THTTMP  , CIN1    , Z00LFC  , DTUDZ   ,   &
    & DTEDZ   , ZLFC    , TVLFC   , DUMFDP  , EE      , TSAT    , THTA    , CHMIN   , DPTT    ,   &
    & WSQ     , RL      , EE2     , UD2     , TTMP    , F1      , F2      , THTMP   , QTMP    ,   &
    & TMPLIQ  , TMPICE  , TU95    , TU10    , TVDIFF  , EE1     , UD1     , THMIX   , THTUDL  ,   &
    & TUDL    , TTEMP   , RAD     , DPMIN   , XTIME   , DXSQ    , P200    , P300    , P400    ,   &
    & QNEWLQ  , QNEWIC  , QFRZ    , BE      , BOTERM  , PMIX0   , REI     , DILBE   , UDLBE   ,   &
    & DZZ     , DMFFRC  , ENTERM 
!----------------------------------------------
! DEFINE CONSTANTS NEEDED FOR KFPARA SUBROUTINE
!----------------------------------------------
    DATA P00   /   1.E5        /
    DATA T00   / 273.16        /
    DATA G     /   9.80616     /
    DATA RLF   /   3.339E5     /
    DATA RHIC  /   1.          /
    DATA RHBC  /   0.90        /
    DATA PIE   /   3.141592654 /
    DATA TTFRZ / 268.16        /
    DATA TBFRZ / 248.16        / 
    DATA C5    /   1.0723E-3   /
    DATA RATE  /   0.03        /
    DATA RV    / 461.5         /
    DATA ALIQ  / 613.3         /
    DATA BLIQ  /  17.502       /
    DATA CLIQ  /4780.8         /
    DATA DLIQ  /  32.19        /
    DATA AICE  / 613.2         /
    DATA BICE  /  22.452       /
    DATA CICE  /6133.0         / 
    DATA DICE  /   0.61        / 
    DATA XLV0  /   3.147E6     / 
    DATA XLV1  /2369.          / 
    DATA XLS0  /   2.905E6     / 
    DATA XLS1  / 259.532       /
!--------------------------------------------------------------------------------------------------
! OPTION TO FEED CONVECTIVELY GENERATED RAINWATER INTO GRID-RESOLVED RAINWATER (OR SNOW/GRAUPEL)
! FIELD. 'FBFRC' IS THE FRACTION OF AVAILABLE PRECIPITATION TO BE FED BACK (0.0 - 1.0)
!--------------------------------------------------------------------------------------------------
    FBFRC = 0.0
!
    ROVG =  R  / G
    GDRY = -G  / CP
    DT2  =  2. * DT
!----------------------------------------------------------------------
! SPECIFY DOWNDRAFT MASS FLUX AS FRACTION OF UPDRAFT MASS FLUX (DMFFRC)
!----------------------------------------------------------------------
    DMFFRC = 0.9
!-----------------------------------------------
! LOOP OVER ALL POTENTIAL CONVECTIVE GRID POINTS
!-----------------------------------------------
    NCCNT = 0
!
    DO 900 NC=1,NCUYES
!--------------------------------------
! VARIABLES TO ALLOW SHALLOW CONVECTION
!--------------------------------------
        NCHM   =    0
        ISHALL =    0
        RAD    = 1500.
        FBFRC  =    0.
!-----------------------------
! CORRECTION FROM REGIONAL ETA 
!-----------------------------
!
!------------------------------------------------------------------------------
! ASSUME THE FOLLOWING FUNCTIONAL DEPENDENCE FOR 1 - 40 KM RESOLUTION:
! FBFRC=1. FOR DX = 50 KM, 
! FBFRC=1. FOR DX =  1 KM, WHERE
!
! DXR = 111. * (DPHD ** 2 + DLMD ** 2) ** .5 ! MODEL RESOLUTION AT EQUATOR (KM)
! DRAGAN JAN 2016
!------------------------------------------------------------------------------
        DXR = 40075 / (4 * IM0)
!
        DXR = MIN(50., MAX(1., DXR))
!
        IF (DXR <= 1.) THEN
            FDX = 0.
        ELSE
            IF ((DXR > 1.) .AND. (DXR < 50.)) THEN
                FDX = 0.6 * LOG(DXR) - 0.0443162
            ELSE
                FDX = 1.
            END IF
        END IF
!
        FBFRC = 1 - FDX
        FBFQV = 0.4      !FRACTION OF LIQUID WATER THAT EVAPORATES  
!
        DPMIN = 5.E3
!
        I     = ICUYES(NC)
        XTIME = NTSD * DT / 60.
!
        DXSQ = DX(I,J) * DX(I,J)
        P200 = PD(I,J) + PT - 2.E4
        P300 = PD(I,J) + PT - 3.E4
        P400 = PD(I,J) + PT - 4.E4
!
        ML = 0
!----------------------------------------------------------
! DEFINE NUMBER OF LAYERS ABOVE GROUND LEVEL - CALL THIS KL
!----------------------------------------------------------
        KL   = LMH(I,J)
        NLEV = KX - KL
        KLM  = KL - 1
!--------------------------------------------------------------------------------------------------
! INPUT A VERTICAL SOUNDING
!
! THE FOLLOWING LOOP IS CRUCIAL. THE KF SCHEME IS SET UP TO HAVE LEVELS STARTING WITH 1 AT THE 
! LOWEST MODEL LEVEL AND GOING UP, WHICH IS INVERTED FROM WHAT SIGMA AND ETA COORDINATE MODELS
! TEND TO USE. THUS, THE FIRST STEP IS TO SWITCH THE ORDER OF THE VARIABLES USED WITHIN THE KF 
! SCHEME. NOTE THAT THE SCHEME NEEDS THE FOLLOWING VARIABLES
!
! INPUT: TEMPERTURE (T0, K) ; SPECIFIC HUMIDITY (Q0, KG/KG) ;
! HORIZONTAL WIND SPEED (U0 AND V0, M/S) ;
! PRESSURE (P0, PASCAL) ; HEIGHT (Z0, M);
! VERTICAL MOTION (W0, M/S).
!--------------------------------------------------------------------------------------------------
!CHOU  P0P tem KX+1 elementos
        DO K=1,KX+1
            NK = KX - K + 1 - NLEV
            P0P(K) = PD(I,J) * RES(I,J) * ETA(NK) + PT
        END DO
!CHOU
        DO 15 K=1,KL
            NK = KX - K + 1 - NLEV
!CHOU            P0P(K) = PD(I,J) * RES(I,J) * AETA(NK) + PT
             T0(K) =  T(I,J,NK)
             Q0(K) =  Q(I,J,NK)
            TKE(K) = Q2(I,J,NK)
!--------------------------------------------------------------------------------------------------
! SATURATION VAPOR PRESSURE (ES) IS CALCULATED USING FORMULA GIVEN BY BUCK (1981). IF Q0 IS ABOVE
! SATURATION VALUE USING THIS METHOD, REDUCE IT TO SATURATION LEVEL.
!--------------------------------------------------------------------------------------------------
            ES     = ALIQ  * EXP((BLIQ * T0(K) - CLIQ) / (T0(K) - DLIQ))
!CHOU skip if PRESSURE <= 25 hPa
            IF (P0P(K) <= 2500.) THEN
               QES(K) = 1.0E-10
            ELSE
               QES(K) = 0.622 * ES / (P0P(K) - ES)
            ENDIF
!CHOU
             Q0(K) = AMIN1(QES(K), Q0(K))
!
            IF (Q0(K) <= 0.1E-8) THEN
                Q0(K) = 0.1E-8
            END IF
!
            RH(K) = Q0(K) / QES(K)
!--------------------------------------------------------------------------------------------------
! SET HYDROMETEOR CONCENTRATIONS TO ZERO INITIALLY. ALTHOUGH SOME HYDROMETEORS MAY ACTUALLY BE 
! PRESENT, THIS IS DONE TO DISALLOW ENTRAINMENT OF THESE INTO CONVECTIVE UPDRAFTS AND DOWNDRAFTS.
! NECESSARY BECAUSE CONVECTIVE SCHEME OPERATES OVER MULTIPLE TIME STEPS ASSUMING STEADY-STATE 
! ENVIRONMENTAL CONDITIONS AND THERE IS NO WAY OF GUARANTEEING THAT LIQUID WATER WILL CONTINUE TO
! REMAIN AVAILABLE FOR ENTRAINMENT EVEN IF IT IS THERE INITIALLY.
!--------------------------------------------------------------------------------------------------
               QL0(K) = 0.
               QI0(K) = 0.
               QR0(K) = 0.
               QS0(K) = 0.
            DILFRC(K) = 1.
!---------------------------------------------------------------------
! CALCULATE WIND AT H-POINT AS AVERAGE OF 4 SURROUNDING V-POINT VALUES
!---------------------------------------------------------------------
            SUMV = VTM(I,J,NK) + VTM(I,J-1,NK) + VTM(I-1,J,NK) + VTM(I-1,J-1,NK)
!------------------------------------------------
! MP - CORRECT FOR POSSIBLE HTM/VTM DISCREPANCIES
!------------------------------------------------
            IF (SUMV > 0) THEN
                II1 = (/I-1, I  , I-1, I/)
                J1  = (/J-1, J-1, J  , J/)
                U0(K) = 0.
                V0(K) = 0.
!
                DO K1=1,4
                    U0(K) = U0(K) + (U(II1(K1), J1(K1), NK) * QVH(I, J, K1, 1, 1)                 &
    &                     +          V(II1(K1), J1(K1), NK) * QVH(I, J, K1, 1, 2))                &
    &                     *        VTM(II1(K1), J1(K1), NK)
!
                    V0(K) = V0(K) + (U(II1(K1), J1(K1), NK) * QVH(I, J, K1, 2, 1)                 &
    &                     +          V(II1(K1), J1(K1), NK) * QVH(I, J, K1, 2, 2))                &
    &                     *        VTM(II1(K1), J1(K1) ,NK)
                END DO
!
                U0(K) = U0(K) / SUMV
                V0(K) = V0(K) / SUMV
!
            ELSE
!
                U0(K) = 0.
                V0(K) = 0.
!
            END IF
!
             TV0(K) =  T0(K) * (1. + 0.608 *  Q0(K))
            RHOE(K) = P0P(K) /         (R  * TV0(K))
!
            DZQ(K) = ROVG * TV0(K) * ALOG((PD(I,J) * RES(I,J) * ETA(NK+1) + PT)                   &
    &              /                      (PD(I,J) * RES(I,J) * ETA(NK  ) + PT))
!
             DP(K) = (ETA(NK+1) - ETA(NK)) * PD(I,J) * RES(I,J)
             W0(K) = W0AVG(I,J,NK)
!--------------------------------------------------------------------
! DZQ IS DZ BETWEEN ETA SURFACES. DZA IS DZ BETWEEN MODEL HALF LEVEL.
! DP IS THE PRESSURE INTERVAL BETWEEN FULL ETA LEVELS.
!--------------------------------------------------------------------
            IF (P0P(K) >= 500E2) L5   = K
            IF (P0P(K) >= 400E2) L44  = K
            IF (P0P(K) >=  P300) LLFC = K
            IF ( T0(K) >    T00) ML   = K
!
            CLDHGT(K) = 0.
!
 15         CONTINUE
!----------------------------------
! FILL REMAINING LAYERS WITH ZEROES
!----------------------------------
            DO K=KL+1,KX
                   P0P(K) = 0.
                    T0(K) = 0.
                    Q0(K) = 0.
                   QES(K) = 0.
                    U0(K) = 0.
                    V0(K) = 0.
                   QL0(K) = 0.
                   QI0(K) = 0.
                   QR0(K) = 0.
                   QS0(K) = 0.
                   TV0(K) = 0.
                  RHOE(K) = 0.
                   DZQ(K) = 0.
                    DP(K) = 0.
                   Z00(K) = 0.
                    W0(K) = 0.
                   DZA(K) = 0.
                DILFRC(K) = 0.
            END DO
!
            Z00(1) = .5 * DZQ(1)
!
            DO K=2,KL
                Z00(K  ) = Z00(K-1) + 0.5 * (DZQ(K) + DZQ(K-1))
                DZA(K-1) = Z00(K  ) - Z00(K-1)
            END DO
!
            DZA(KL) = 0.
            NLOOP   = 0.
!
            KMIX = LSB(I)
!
 25         LOW   = KMIX
            NLOOP = NLOOP + 1
!
            IF (NLOOP > 50) THEN
                PRINT*, 'I, J, LOW, ISHALL, NCHM =',I, J, LOW, ISHALL, NCHM
                IF (NLOOP > 100) THEN
                    GOTO 600
                END IF
            END IF
!---------------------------------------------------------------------------------
! IF PARCEL ORIGINATES FROM 300 MB ABOVE SURFACE OR HIGHER THEN SCHEME IS NOT USED 
! CHECK NEXT GRID POINT VARIABLES FOR SHALLOW CONVECTION
!---------------------------------------------------------------------------------
            IF (LOW > LLFC) THEN
                IF (ISHALL == 1) THEN
                    CHMAX = 0.
                     NCHM = 0
!
                    DO NK=1,LLFC
                        IF (CLDHGT(NK) > CHMAX) THEN
                            NCHM  = NK
                            CHMAX = CLDHGT(NK)
                        END IF
                    END DO
!
                    KMIX = NCHM
                    GOTO 25
!
                END IF
!
                GOTO 900
!
            END IF
!
            LC = LOW
!
            IF (ISHALL == 1 .AND. LC == NCHM) THEN
                FBFRC = 1.
!--------------------------------------------------------------------------------------------------
! COMMENT OUT STATEMENT BELOW SO THAT SHALLOW CLOUD DRAWS FROM SAME LAYER AS DEEP CLOUD TO PREVENT
! SHALLOW CLOUD FROM BECOMING TOO DEEP!
!
! DPMIN = 2.5E3
!--------------------------------------------------------------------------------------------------
            END IF
!--------------------------------------------------------------------------------------------------
! ASSUME THAT IN ORDER TO SUPPORT A DEEP UPDRAFT YOU NEED A LAYER OF UNSTABLE AIR 50 TO 100 MB DEEP
! TO APPROXIMATE THIS, ISOLATE A GROUP OF ADJACENT INDIVIDUAL MODEL LAYERS, WITH THE BASE AT LEVEL
! LC, SUCH THAT THE COMBINED DEPTH OF THESE LAYERS IS AT LEAST 60 MB.
!
! INTRODUCE A MOISTURE PERTURBATION IN UPDRAFT SOURCE LAYERS IF RH > 75%. NOTE THAT THIS APPROACH 
! SHOULD PERHAPS BE MODIFIED IF THE GRID-SCALE CONDENSATION THRESHOLD IS CHANGED.
!--------------------------------------------------------------------------------------------------
            NLAYRS = 0
            DPTHMX = 0.
!
            DO NK=LC,KX
                DPTHMX   = DPTHMX + DP(NK)
                NLAYRS   = NLAYRS + 1
                   QEF   = 0.
                QUER(NK) = Q0(NK) * (1. + QEF)
!
                IF (DPTHMX > DPMIN) GOTO 50
            END DO
!
            GOTO 900
!
 50         KPBL = LC + NLAYRS - 1
!--------------------------------------------------------------------------------------------------
! DETERMINE WHAT LEVEL TO START WITH FOR THE NEXT MIXTURE IN CASE THE CURRENT MIXTURE, WITH BASE AT
! LEVEL LC, IS NOT BUOYANT.
! INSTEAD OF CHECKING MIXTURES USING EVERY SINGLE LAYER, MOVE UP IN INCREMENTS OF AT LEAST 20 MB.
!--------------------------------------------------------------------------------------------------
            PM15 = P0P(LC) - 15.E2
!
            DO NK=LC+1,KL
                IF (P0P(NK) < PM15) THEN
                    KMIX = NK
                    GOTO 75
                END IF
            END DO
!
            GOTO 900
!
 75         CONTINUE
!--------------------------------------------------------------------------------------------------
! FOR COMPUTATIONAL SIMPLICITY WITHOUT MUCH LOSS IN ACCURACY, MIX TEMPERATURE INSTEAD OF THETA FOR
! EVALUATING CONVECTIVE INITIATION (TRIGGERING) POTENTIAL.
!--------------------------------------------------------------------------------------------------
            TMIX = 0.
            QMIX = 0.
            ZMIX = 0.
            PMIX = 0.
!--------------------------------------------------------------------------------------------------
! FIND THE THERMODYNAMIC CHARACTERISTICS OF THE LAYER BY MASS-WEIGHTING THE CHARACTERISTICS OF THE
! INDIVIDUAL MODEL LAYERS.
!--------------------------------------------------------------------------------------------------
            DO NK=LC,KPBL
                TMIX = TMIX + DP(NK) *   T0(NK)
                QMIX = QMIX + DP(NK) * QUER(NK)
                ZMIX = ZMIX + DP(NK) *  Z00(NK)
                PMIX = PMIX + DP(NK) *  P0P(NK)
            END DO
!
            TMIX = TMIX / DPTHMX
            QMIX = QMIX / DPTHMX
            ZMIX = ZMIX / DPTHMX
            PMIX = PMIX / DPTHMX
            EMIX = QMIX * PMIX / (0.622 + QMIX)
!-----------------------------------------------
! FIND THE TEMPERATURE OF THE MIXTURE AT ITS LCL
!-----------------------------------------------
            ASTRT = 1.E-3
            AINC  = 0.075
!
            A1 = EMIX / ALIQ
            TP = (A1 - ASTRT) / AINC
!
            INDLU  = INT(TP) + 1
            VALUE  =  (INDLU - 1) * AINC + ASTRT
            AINTRP = (A1 - VALUE) / AINC
            TLOG   = AINTRP * ALU(INDLU + 1) + (1 - AINTRP) * ALU(INDLU)
            TDPT   = (CLIQ - DLIQ * TLOG) / (BLIQ - TLOG)
             TLCL  = TDPT - (0.212+1.571E-3 * (TDPT-T00) - 4.36E-4 * (TMIX - T00)) * (TMIX - TDPT)
             TLCL  = AMIN1(TLCL, TMIX)
            TVLCL  = TLCL * (1. + 0.608 * QMIX)
             ZLCL  = ZMIX + (TLCL - TMIX) / GDRY
!
            DO NK=LC,KL
                KLCL = NK
                IF (ZLCL <= Z00(NK)) GOTO 100
            END DO
!
            GOTO 900
!
100         K = KLCL - 1
!----------------------------------------
! CALCULATE DLP USING Z INSTEAD OF LOG(P)
!----------------------------------------
            DLP = (ZLCL - Z00(K)) / (Z00(KLCL) - Z00(K))
!----------------------------------------------------------------
! ESTIMATE ENVIRONMENTAL TEMPERATURE AND MIXING RATIO AT THE LCL.
!----------------------------------------------------------------
            TENV = T0(K) + (T0(KLCL) - T0(K)) * DLP
            QENV = Q0(K) + (Q0(KLCL) - Q0(K)) * DLP
!
            TVEN = TENV * (1. + 0.608 * QENV)
!--------------------------------------------------------------------------------------------------
! CHECK TO SEE IF CLOUD IS BUOYANT USING FRITSCH-CHAPPELL TRIGGER FUNCTION DESCRIBED IN KAIN AND 
! FRITSCH (1992). W0 IS AN APROXIMATE VALUE FOR THE RUNNING-MEAN GRID-SCALE VERTICAL VELOCITY, 
! WHICH GIVES SMOOTHER FIELDS OF CONVECTIVE INITIATION THAN THE INSTANTANEOUS VALUE. FORMULA 
! RELATING TEMPERATURE PERTURBATION TO VERTICAL VELOCITY HAS BEEN USED WITH THE MOST SUCCESS AT 
! GRID LENGTHS NEAR 25 KM. FOR DIFFERENT GRID-LENGTHS, ADJUST VERTICAL VELOCITY TO EQUIVALENT VALUE
! FOR 25 KM GRID LENGTH, ASSUMING LINEAR DEPENDENCE OF W ON GRID LENGTH.
!--------------------------------------------------------------------------------------------------
            IF (ZLCL < 2.E3) THEN
                WKLCL = 0.02 * ZLCL / 2.E3
            ELSE
                WKLCL = 0.02
            END IF
!
            WKL = (W0(K) + (W0(KLCL) - W0(K)) * DLP) * DX(I,J) / 25.E3 - WKLCL
!
            IF (WKL < 0.0001) THEN
                DTLCL = 0.
            ELSE
                DTLCL = 4.64 * WKL ** 0.33
            END IF
!---------------------------------------------------------------------------------------
! GIVE PARCEL AN EXTRA TEMPERATURE PERTURBATION BASED THE THRESHOLD RH FOR CONDENSATION.
! FOR NOW, JUST ASSUME U00 = 0.75.
!---------------------------------------------------------------------------------------
            U00 = 0.75
!
            IF (U00 < 1.) THEN
                QSLCL = QES(K) + (QES(KLCL) - QES(K)) * DLP
                RHLCL = QENV / QSLCL
                DQSDT = QMIX * (CLIQ - BLIQ * DLIQ) / ((TLCL - DLIQ) * (TLCL - DLIQ))

                IF (RHLCL >= 0.75 .AND. RHLCL <= 0.95) THEN
                    DTRH = 0.25 * (RHLCL - 0.75) * QMIX / DQSDT
                ELSE IF (RHLCL > 0.95) THEN
                    DTRH = (1. / RHLCL - 1.) * QMIX / DQSDT
                ELSE
                    DTRH = 0.
                END IF
            END IF
!
            IF (TLCL + DTLCL + DTRH > TENV)   GOTO 150
            IF (ISHALL == 1 .AND. LC == NCHM) GOTO 900
            IF (KMIX <= LLFC)                 GOTO 25
!---------------------------
! SHALLOW CONVECTION CHANGES
!---------------------------
            IF (ISHALL == 1) THEN
                CHMAX = 0.
                 NCHM = 0
!
                DO NK=1,LLFC
                    IF (CLDHGT(NK) > CHMAX) THEN
                        NCHM  = NK
                        CHMAX = CLDHGT(NK)
                    END IF
                END DO
!
                KMIX = NCHM
                GOTO 25
            END IF
!
            GOTO 900
!
150     CONTINUE
!----------------------------------------------------------------
! CONVECTIVE TRIGGERING CRITERIA HAS BEEN SATISFIED.
! COMPUTE EQUIVALENT POTENTIAL TEMPERATURE.
! (THETEU) AND VERTICAL VELOCITY OF THE RISING PARCEL AT THE LCL.
!----------------------------------------------------------------
        THMIX     = TMIX  * (1.E5 / PMIX) ** (0.2854 * (1. - 0.28 * QMIX))
        THETEU(K) = THMIX * EXP((3374.6525 / TLCL - 2.5403) * QMIX * (1. + 0.81 * QMIX))
        ES        = ALIQ  * EXP((TENV  * BLIQ - CLIQ) / (TENV - DLIQ))
        TVAVG     = 0.5   * (TV0(KLCL) + TENV * (1.   + 0.608 * QENV))
!-------------------------------------------------------
! MODIFY CALCULATION OF INITIAL PARCEL VERTICAL VELOCITY
!-------------------------------------------------------
        DTTOT = DTLCL + DTRH
!
        IF (DTTOT  > 1.E-4) THEN
            GDT  = 2. * G   * DTTOT * 500. / TVEN
            WLCL = 1. + 0.5 * SQRT(GDT)
            WLCL = AMIN1(WLCL, 3.)
        ELSE
            WLCL = 1.
        END IF
!
        PLCL   = P0P(K) + (P0P(KLCL) - P0P(K)) * DLP
        WTW    = WLCL   * WLCL
        TVLCL  = TLCL   * (1. + 0.608 * QMIX)
        RHOLCL = PLCL   / (R  * TVLCL)
        PLCL0  = PLCL
!
        LCL = KLCL
        LET = LCL
!
        IF (WKL < 0.) THEN
            RAD = 1000.
        ELSE IF (WKL > 0.1) THEN
            RAD = 2000.
        ELSE
            RAD = 1000. + 1.E4 * WKL
        END IF
!---------------------------
! COMPUTE UPDRAFT PROPERTIES
!---------------------------
!
!--------------------------------------------
! ESTIMATE INITIAL UPDRAFT MASS FLUX (UMF(K))
!--------------------------------------------
        WU(K)  = WLCL
        AU0    = 0.01   * DXSQ
        UMF(K) = RHOLCL * AU0
        VMFLCL = UMF(K)
        UPOLD  = VMFLCL
        UPNEW  = UPOLD
!--------------------------------------------------------------------------------------------------
! RATIO2 IS THE DEGREE OF GLACIATION IN THE CLOUD (0 TO 1), UER IS THE ENVIR ENTRAINMENT RATE, ABE 
! IS AVAILABLE BUOYANT ENERGY, TRPPT IS THE TOTAL RATE OF PRECIPITATION PRODUCTION.
!--------------------------------------------------------------------------------------------------
        RATIO2(K) = 0.
           UER(K) = 0.
        ABE       = 0.
        TRPPT     = 0.
            TU(K) = TLCL
           TVU(K) = TVLCL
            QU(K) = QMIX
         EQFRC(K) = 1.
          QLIQ(K) = 0.
          QICE(K) = 0.
        QLQOUT(K) = 0.
        QICOUT(K) = 0.
         DETLQ(K) = 0.
         DETIC(K) = 0.
        PPTLIQ(K) = 0.
        PPTICE(K) = 0.
        IFLAG     = 0
        KFRZ      = LC
!--------------------------------------------------------------------------------------------------
! THE AMOUNT OF CONV AVAIL POT ENERGY (CAPE) IS CALCULATED WITH RESPECT TO UNDILUTE PARCEL ASCENT; 
! EQ POT TEMP OF UNDILUTE PARCEL IS THTUDL, UNDILUTE TEMPERATURE IS GIVEN BY TUDL.
!--------------------------------------------------------------------------------------------------
        THTUDL = THETEU(K)
        TUDL   = TLCL
!--------------------------------------------------------------------------------------------------
! TTEMP IS USED DURING CALCULATION OF THE LINEAR GLACIATION PROCESS; IT IS INITIALLY SET TO THE 
! TEMPERATURE AT WHICH FREEZING IS SPECIFIED TO BEGIN. WITHIN THE GLACIATION INTERVAL, IT IS SET 
! EQUAL TO THE UPDRAFT TEMP AT THE PREVIOUS MODEL LEVEL.
!--------------------------------------------------------------------------------------------------
        TTEMP = TTFRZ
!--------------------------------------------------------------------------------------------------
! ENTER THE LOOP FOR UPDRAFT CALCULATIONS. CALCULATE UPDRAFT TEMP, MIXING RATIO, VERTICAL MASS 
! FLUX, LATERAL DETRAINMENT OF MASS AND MOISTURE, PRECIPITATION RATES AT EACH MODEL LEVEL.
!--------------------------------------------------------------------------------------------------
!
!----------------
!DIAGNOSTIC STUFF
!----------------
        PLCL0 = PLCL
        LFC   = 0
        PMIX0 = PMIX
        REI   = 0.
        DILBE = 0.
        UDLBE = 0.
!
        DO 250 NK=K,KLM
            NK1         = NK + 1
            RATIO2(NK1) = RATIO2(NK)
            FRC1        = 0.
                TU(NK1) =     T0(NK1)
            THETEU(NK1) = THETEU(NK)
                QU(NK1) =     QU(NK)
              QLIQ(NK1) =   QLIQ(NK)
              QICE(NK1) =   QICE(NK)
 !
            CALL TPMIX2(P0P(NK1), THETEU(NK1), TU(NK1), QU(NK1), QLIQ(NK1), QICE(NK1), QNEWLQ,    &
    &                   QNEWIC  , RATIO2(NK1), XLV1   , XLV0)
!--------------------------------------------------------------------------------------------------
! CHECK TO SEE IF UPDRAFT TEMP IS ABOVE THE TEMPERATURE AT WHICH GLACIATION IS ASSUMED TO INITIATE;
! IF IT IS, CALCULATE THE FRACTION OF REMAINING LIQUID WATER TO FREEZE. FRZ IS THE TEMP AT WHICH 
! FREEZING BEGINS, TBFRZ THE TEMP BELOW WHICH ALL LIQUID WATER IS FROZEN AT EACH LEVEL.
!--------------------------------------------------------------------------------------------------
            IF (TU(NK1) <= TTFRZ) THEN
!
                IF (TU(NK1) > TBFRZ) THEN
!
                    IF (TTEMP > TTFRZ) TTEMP = TTFRZ
                        FRC1 = (TTEMP - TU(NK1)) / (TTEMP - TBFRZ)
                    ELSE
                        FRC1  = 1.
                        IFLAG = 1
                    END IF
!
                    TTEMP = TU(NK1)
!-------------------------------------------------------------------------------
! DETERMINE THE EFFECTS OF LIQUID WATER FREEZING WHEN TEMPERATURE IS BELOW TTFRZ
!-------------------------------------------------------------------------------
                    QFRZ = (QLIQ(NK1) + QNEWLQ) * FRC1
!
                    QNEWIC = QNEWIC + QNEWLQ * FRC1
                    QNEWLQ = QNEWLQ - QNEWLQ * FRC1
!
                    QICE(NK1) = QICE(NK1) + QLIQ(NK1) * FRC1
                    QLIQ(NK1) = QLIQ(NK1) - QLIQ(NK1) * FRC1
!
                    CALL DTFRZNEW(TU(NK1), P0P(NK1), THETEU(NK1), QU(NK1), QFRZ, QICE(NK1),       &
    &                             ALIQ   , BLIQ    , CLIQ       , DLIQ)
!
                END IF
!
                TVU(NK1) = TU(NK1) * (1. + 0.608 * QU(NK1))
!--------------------------------------------------------------
! CALCULATE UPDRAFT VERTICAL VELOCITY AND PRECIPITATION FALLOUT
!--------------------------------------------------------------
                IF (NK == K) THEN
                    BE     = (TVLCL + TVU(NK1)) / (TVEN + TV0(NK1)) - 1.
                    BOTERM = 2. * (Z00(NK1) - ZLCL) * G * BE / 1.5
                    DZZ    = Z00(NK1) - ZLCL
                ELSE
                    BE     = (TVU(NK) + TVU(NK1)) / (TV0(NK) + TV0(NK1)) - 1.
                    BOTERM = 2. * DZA(NK) * G * BE / 1.5
                    DZZ    = DZA(NK)
                END IF
 !
                ENTERM = 2. * REI * WTW / UPOLD
!------------
! DIAGNOSTICS
!------------
                IF (TVU(NK1) > TV0(NK1)) THEN
                    IF (TVU(NK) < TV0(NK) .OR. NK1 == KLCL) LFC = NK1
                END IF
!
                WSQ = WTW
!
                CALL CONDLOAD(QLIQ(NK1), QICE(NK1),  WTW     , DZZ         , BOTERM     , ENTERM, &
    &                          RATE    , QNEWLQ   , QNEWIC   ,  QLQOUT(NK1), QICOUT(NK1))
!--------------------------------------------------------------------------------------------------
! IF VERT VELOCITY IS LESS THAN ZERO, EXIT THE UPDRAFT LOOP AND, IF CLOUD IS TALL ENOUGH, FINALIZE
! UPDRAFT CALCULATIONS...
!--------------------------------------------------------------------------------------------------
                IF (WTW < 1.E-3) THEN
                    GOTO 275
                ELSE
                    WU(NK1) = SQRT(WTW)
                END IF
!--------------------------------------------------------------------------------------------------
! CALL SUBROUTINE TO CALCULATE ENVIRONMENTAL EQUIVALENT POTENTIAL TEMP...WITHIN GLACIATION INTERVAL
! THETAE MUST BE CALCULATED WITH RESPECT TO THE SAME DEGREE OF GLACIATION FOR ALL ENTRAINING AIR.
!
! FOR LOOKUP TABLE VERSION, CALCULATE THETAE WITH RESPECT TO LIQUID WATER AT ALL LEVELS.
!--------------------------------------------------------------------------------------------------
                IF (NK1 <= KPBL) THEN
                    CALL ENVIRTHT(P0P(NK1), T0(NK1), QUER(NK1), THETEE(NK1), 0.  ,                &
    &                             RL      , ALIQ   , BLIQ     , CLIQ       , DLIQ,                &
    &                                       AICE   , BICE     , CICE       , DICE)
!
                ELSE
!
                    CALL ENVIRTHT(P0P(NK1), T0(NK1), Q0(NK1)  , THETEE(NK1), 0.  ,                &
    &                             RL      , ALIQ   , BLIQ     , CLIQ       , DLIQ,                &
    &                                       AICE   , BICE     , CICE       , DICE)
                END IF
!----------------------------------------
! REI IS THE RATE OF ENVIRONMENTAL INFLOW
!----------------------------------------
                REI = VMFLCL * DP(NK1) * 0.03 / RAD
                TVQU(NK1) = TU(NK1) * (1. + 0.608 * QU(NK1) - QLIQ(NK1) - QICE(NK1))
!
                IF (NK == K) THEN
                    DILBE = ((TVLCL    + TVQU(NK1)) / (TVEN    + TV0(NK1)) - 1.) * DZZ
                ELSE
                    DILBE = ((TVQU(NK) + TVQU(NK1)) / (TV0(NK) + TV0(NK1)) - 1.) * DZZ
                END IF
!
                IF (DILBE > 0.) ABE = ABE + DILBE * G
!-----------------------------------------------------------
! IF CLOUD PARCELS ARE VIRTUALLY COLDER THAN THE ENVIRONMENT
! NO ENTRAINMENT IS ALLOWED AT THIS LEVEL.
!-----------------------------------------------------------
                IF (TVQU(NK1) <= TV0(NK1))THEN
!--------------------------------------------------
! USE A MINIMUM ENTRAINMENT RATE (UER) OF 0.5 * REI
!--------------------------------------------------
                    UER(NK1) = 0.5 * REI
                    UDR(NK1) = 1.5 * REI
!
                    EE2 = 0.5
                    UD2 = 1.
!
                    EQFRC(NK1) = 0.
!
                    GOTO 200
!
                END IF
!
                LET  = NK1
                TTMP = TVQU(NK1)
!-----------------------------------------------------------------------
! DETERMINE THE CRITICAL MIXED FRACTION OF UPDRAFT AND ENVIRONMENTAL AIR
!-----------------------------------------------------------------------
                F1 = 0.95
                F2 = 1.   - F1
!
                THTTMP = F1 * THETEE(NK1) + F2 * THETEU(NK1)
                QTMP   = F1 *     Q0(NK1) + F2 *     QU(NK1)
                TMPLIQ = F2 *   QLIQ(NK1)
                TMPICE = F2 *   QICE(NK1)
!
                CALL TPMIX2(P0P(NK1), THTTMP, TTMP       , QTMP, TMPLIQ, TMPICE,                  &
    &                       QNEWLQ  , QNEWIC, RATIO2(NK1), XLV1, XLV0  , .FALSE.)
! 
                TU95 = TTMP * (1. + 0.608 * QTMP - TMPLIQ - TMPICE)
! 
                IF (TU95 > TV0(NK1)) THEN
                    EE2        = 1.
                    UD2        = 0.
                    EQFRC(NK1) = 1.
                    GOTO 175
                END IF
!
                F1 = 0.10
                F2 = 1.   - F1
!
                THTTMP = F1 * THETEE(NK1) + F2 * THETEU(NK1)
                QTMP   = F1 *     Q0(NK1) + F2 *     QU(NK1)
                TMPLIQ = F2 *   QLIQ(NK1)
                TMPICE = F2 *   QICE(NK1)
!
                CALL TPMIX2(P0P(NK1), THTTMP, TTMP       , QTMP, TMPLIQ, TMPICE,                  &
    &                       QNEWLQ  , QNEWIC, RATIO2(NK1), XLV1, XLV0)
!
                TU10   = TTMP * (1. + 0.608 * QTMP - TMPLIQ - TMPICE)
                TVDIFF = ABS(TU10 - TVQU(NK1))
!
                IF (TVDIFF < 1.E-3) THEN
                    EE2        = 1.
                    UD2        = 0.
                    EQFRC(NK1) = 1.0
                    GOTO 175
                END IF
!
                EQFRC(NK1) = (TV0(NK1) - TVQU(NK1)) * F1 / (TU10 - TVQU(NK1))
                EQFRC(NK1) = AMAX1(0.0, EQFRC(NK1))
                EQFRC(NK1) = AMIN1(1.0, EQFRC(NK1))
!
                IF (EQFRC(NK1) == 1) THEN
                    EE2 = 1.
                    UD2 = 0.
                    GOTO 175
                ELSE IF (EQFRC(NK1) == 0.) THEN
                    EE2 = 0.
                    UD2 = 1.
                    GOTO 175
                ELSE
!--------------------------------------------------------------------------------------------------
! SUBROUTINE PROF5 INTEGRATES OVER THE GAUSSIAN DIST TO DETERMINE THE FRACTIONAL ENTRAINMENT AND 
! DETRAINMENT RATES
!--------------------------------------------------------------------------------------------------
                    CALL PROF5(EQFRC(NK1), EE2, UD2)
                END IF
!
175             CONTINUE
!
                IF (NK == K) THEN
                    EE1 = 1.
                    UD1 = 0.
                END IF
!----------------------------------------------------------------------------------------------
! NET ENTRAINMENT AND DETRAINMENT RATES ARE GIVEN BY THE AVERAGE FRACTIONAL VALUES IN THE LAYER
!
! USE A MINIMUM ENTRAINMENT RATE (UER) OF 0.5 * REI
!----------------------------------------------------------------------------------------------
                EE2 = AMAX1(EE2, 0.5)
                UD2 = 1.5 * UD2
!
                UER(NK1) = 0.5 * REI * (EE1 + EE2)
                UDR(NK1) = 0.5 * REI * (UD1 + UD2)
!-----------------------------------------------------------------------
! IF THE CALCULATED UPDRAFT DETRAINMENT RATE IS GREATER THAN THE TOTAL
! UPDRAFT MASS FLUX, ALL CLOUD MASS DETRAINS, EXIT UPDRAFT CALCULATIONS.
!-----------------------------------------------------------------------
200             IF (UMF(NK) - UDR(NK1) < 10.) THEN
!--------------------------------------------------------------------------------------------------
! IF THE CALCULATED DETRAINED MASS FLUX IS GREATER THAN THE TOTAL UPWARD MASS FLUX, IMPOSE TOTAL 
! DETRAINMENT OF UPDRAFT MASS AT THE PREVIOUS MODEL LVL
!--------------------------------------------------------------------------------------------------
                    IF (DILBE > 0.) ABE = ABE - DILBE * G
                    LET = NK
                    GOTO 275
                END IF
!
                EE1 = EE2
                UD1 = UD2
!
                UPOLD = UMF(NK) - UDR(NK1)
                UPNEW = UPOLD   + UER(NK1)
!
                   UMF(NK1) = UPNEW
                DILFRC(NK1) = UPNEW / UPOLD
!----------------------------------------------------------------------------------------------
! DETLQ AND DETIC ARE THE RATES OF DETRAINMENT OF LIQUID AND ICE IN THE DETRAINING UPDRAFT MASS
!----------------------------------------------------------------------------------------------
                DETLQ(NK1) = QLIQ(NK1) * UDR(NK1)
                DETIC(NK1) = QICE(NK1) * UDR(NK1)
                  QDT(NK1) =   QU(NK1)
!
                IF (NK1 <= KPBL) THEN
                    QU(NK1) = (UPOLD * QU(NK1) + UER(NK1) * QUER(NK1)) / UPNEW
                ELSE
                    QU(NK1) = (UPOLD * QU(NK1) + UER(NK1) *   Q0(NK1)) / UPNEW
                END IF
!
                THETEU(NK1) = (THETEU(NK1) * UPOLD + THETEE(NK1) * UER(NK1)) / UPNEW
                  QLIQ(NK1) =    QLIQ(NK1) * UPOLD / UPNEW
                  QICE(NK1) =    QICE(NK1) * UPOLD / UPNEW
!--------------------------------------------------------------------------------------------------
! KFRZ IS THE HIGHEST MODEL LEVEL AT WHICH LIQUID CONDENSATE IS GENERATED...
! PPTLIQ IS THE RATE OF GENERATION (FALLOUT) OF LIQUID PRECIP AT A GIVEN MODEL LVL, PPTICE THE SAME
! FOR ICE, TRPPT IS THE TOTAL RATE OF PRODUCTION OF PRECIP UP TO THE CURRENT MODEL LEVEL...
!--------------------------------------------------------------------------------------------------
                IF (ABS(RATIO2(NK1)-1.) > 1.E-6) KFRZ = NK1
!-----------------------------------------------------------------------------------
! REVERSE THE MOD THAT ALLOWS FEEDBACK OF RAIN/SNOW THAT ORIGINATES IN DETRAINED AIR
!-----------------------------------------------------------------------------------
                PPTLIQ(NK1) = QLQOUT(NK1) * UMF(NK)
                PPTICE(NK1) = QICOUT(NK1) * UMF(NK)
!
                TRPPT       = TRPPT + PPTLIQ(NK1) + PPTICE(NK1)
!
                IF (NK1 <= KPBL) UER(NK1) = UER(NK1) + VMFLCL * DP(NK1) / DPTHMX
!
250             CONTINUE
!--------------------------------------------------------------------------------------------------
! CHECK CLOUD DEPTH. IF CLOUD IS TALL ENOUGH, ESTIMATE THE EQUILIBRIUM TEMPERATURE LEVEL (LET) AND
! ADJUST MASS FLUX PROFILE AT CLOUD TOP SO THAT MASS FLUX DECREASES TO ZERO AS A LINEAR FUNCTION OF
! PRESSURE BETWEEN THE LET AND CLOUD TOP.
!
! LTOP IS THE MODEL LEVEL JUST BELOW THE LEVEL AT WHICH VERTICAL VELOCITY FIRST BECOMES NEGATIVE
!--------------------------------------------------------------------------------------------------
275             LTOP = NK
!
                CLDHGT(LC) = Z00(LTOP) - ZLCL
!------------------------------------------------------------------------------------------ 
! IF CLOUD TOP HEIGHT IS LESS THAN THE SPECIFIED MINIMUM FOR DEEP CONVECTION, SAVE VALUE TO 
! CONSIDER THIS LEVEL AS SOURCE FOR SHALLOW CONVECTION, GO BACK UP TO CHECK NEXT LEVEL.
!
! TRY SPECIFYING MINIMUM CLOUD DEPTH AS A FUNCTION OF TLCL
!------------------------------------------------------------------------------------------
                IF (TLCL > 293.) THEN
                   CHMIN = 4.E3
                ELSE IF (TLCL <= 293. .AND. TLCL >= 273.) THEN
                    CHMIN = 2.E3 + 100. * (TLCL - 273.)
                ELSE IF (TLCL < 273.) THEN
                    CHMIN = 2.E3
                END IF
!
                KSTART = MAX0(KPBL, KLCL)
!-------------------------------------------------------------------
! DO NOT ALLOW ANY CLOUD FROM THIS LAYER IF:
!
! 1.) IF THERE IS NO CAPE, OR
! 2.) CLOUD TOP IS AT MODEL LEVEL JUST ABOVE LCL, OR
! 3.) CLOUD TOP IS WITHIN UPDRAFT SOURCE LAYER, OR
! 4.) CLOUD-TOP DETRAINMENT LAYER BEGINS WITHIN UPDRAFT SOURCE LAYER
!-------------------------------------------------------------------
                IF (LTOP <= KLCL .OR. LTOP <= KPBL .OR. LET + 1 <= KPBL) THEN
                    CLDHGT(LC) = 0.
!--------------------------------------------------------------------------------------------------
! IF THIS IS SELECTED SHALLOW SOURCE AND STILL DOES NOT MAKE A SIGNIFICANT CLOUD, GO ON TO NEXT 
! GRID POINT.
!--------------------------------------------------------------------------------------------------
                IF (LC ==  NCHM) GOTO 900
!--------------------------------------------------------------------------------------
! IF ALL LAYERS IN THE SPECIFIED PORTION OF LOWER ATMOSPHERE HAVE BEEN CHECKED, THEN...
!--------------------------------------------------------------------------------------
                IF (KMIX > LLFC) THEN
!---------------------------------------------------------------------
!IF NO POSSIBLE SHALLOW CLOUD LAYERS WERE FOUND, GO TO NEXT GRID POINT
!---------------------------------------------------------------------
                    IF (ISHALL == 0) THEN
                        GOTO 900
                    ELSE
!------------------------------------------------------------------------------------------------
! IF SOME POTENTIAL SHALLOW CLOUD SOURCE LAYERS WERE FOUND FIND ONE THAT GIVES THE TALLEST CLOUD.
!------------------------------------------------------------------------------------------------
                        GOTO 305
                    END IF
!
                ELSE
!----------------------------------------------------------------------------------
! IF THERE ARE MORE LAYERS TO CHECK, RESET CLOUD CHARACTERISTICS, GO TO NEXT LEVEL.
!----------------------------------------------------------------------------------
                    GOTO 310
                END IF
!------------------------------------------------------------------------------------------
! IF THIS LAYER HAS BEEN SELECTED AS A SHALLOW CONVECTIVE SOURCE, ALLOW SHALLOW CONVECTION.
! (EVEN IF THE SHALLOW-CLOUD PARAMETERS ALLOW CLDHGT TO EXCEED CHMIN)
!------------------------------------------------------------------------------------------
            ELSE IF (LC == NCHM) THEN
                GOTO 315
!--------------------------------------------------------------------------------------------------
!IF CLOUD DEPTH IS GREATER THAN MINIMUM DEPTH CRITERION, AND THIS LAYER HAS NOT BEEN MARKED AS A 
! SHALLOW CONVECTIVE SOURCE LAYER, ALLOW DEEP CONVECTION.
!--------------------------------------------------------------------------------------------------
            ELSE IF (CLDHGT(LC) > CHMIN .AND. ABE > 1.) THEN
                ISHALL = 0
                GOTO 315
            END IF
!--------------------------------------------------------------------------------------------------
! AT THIS POINT, WE HAVE A PARCEL THAT IS ABLE TO MAINTAIN UPWARD MOMENTUM FOR AT LEAST A SHORT 
! DISTANCE, BUT THE CLOUD FROM THIS SOURCE LAYER (LC) IS NOT DEEP ENOUGH FOR "DEEP" (PRECIPITATING)
! CONVECTION. SET ISHALL=1 TO SAVE LC AS A POSSIBLE FOR SOURCE LAYER FOR SHALLOW CONVECTION.
!--------------------------------------------------------------------------------------------------
!
!------------------------------------------------------ 
! TO DISALLOW SHALLOW CONVECTION, COMMENT OUT NEXT LINE 
!------------------------------------------------------
            ISHALL = 1
!
            IF (KMIX > LLFC) THEN
                GOTO 305
            ELSE
                GOTO 310
            END IF
!
305         CONTINUE
!
            IF (ISHALL == 0) THEN
                GOTO 900
            ELSE
                CHMAX = 0.
                NCHM  = 0
!
                DO NK=1,LLFC
                    IF (CLDHGT(NK) > CHMAX) THEN
                        NCHM  = NK
                        CHMAX = CLDHGT(NK)
                    END IF
                END DO
!
                KMIX = NCHM
!
            END IF
!
310         CONTINUE
!
            DO NK=K,LTOP
                   UMF(NK) = 0.
                   UDR(NK) = 0.
                   UER(NK) = 0.
                 DETLQ(NK) = 0.
                 DETIC(NK) = 0.
                PPTLIQ(NK) = 0.
                PPTICE(NK) = 0.
            END DO
!
            GOTO 25
!
315         CONTINUE
!
            IF (ISHALL == 1) THEN
                KSTART = MAX0(KPBL, KLCL)
                LET    = KSTART
            END IF
!--------------------------------------------------------------------------------------
! IF THE LET AND LTOP ARE THE SAME, DETRAIN ALL OF THE UPDRAFT MASS FLUX AT THIS LEVEL.
!--------------------------------------------------------------------------------------
            IF (LET == LTOP) THEN
                  UDR(LTOP) =  UMF(LTOP) + UDR(LTOP) - UER(LTOP)
                DETLQ(LTOP) = QLIQ(LTOP) * UDR(LTOP) * UPNEW / UPOLD
                DETIC(LTOP) = QICE(LTOP) * UDR(LTOP) * UPNEW / UPOLD
!------------------------------------------------------------------------------------
! REVERSE THE MOD THAT ALLOWS FEEDBACK OF RAIN/SNOW THAT ORIGINATES IN DETRAINED AIR.
!------------------------------------------------------------------------------------
                UER(LTOP) = 0.
                UMF(LTOP) = 0.
!
                GOTO 350
!
            END IF
!---------------------------------------------------
! BEGIN TOTAL DETRAINMENT AT THE LEVEL ABOVE THE LET
!---------------------------------------------------
            DPTT = 0.
!
            DO NJ=LET+1,LTOP
                DPTT = DPTT + DP(NJ)
            END DO
!
            DUMFDP = UMF(LET) / DPTT
!--------------------------------------------------------------------------------------------------
! ADJUST MASS FLUX PROFILES, DETRAINMENT RATES, AND PRECIPITATION FALL RATES TO REFLECT THE LINEAR
! DECREASE IN MASS FLX BETWEEN THE LET AND
!--------------------------------------------------------------------------------------------------
            DO 325 NK=LET+1,LTOP
!--------------------------------------------------------------------------------------------------
! ENTRAINMENT IS ALLOWED AT EVERY LEVEL EXCEPT FOR LTOP, SO DISALLOW ENTRAINMENT AT LTOP AND ADJUST
! ENTRAINMENT RATES BETWEEN LET AND LTOP SO THE THE DILUTION FACTOR DUE TO ENTRAINMENT IS NOT 
! CHANGED BUT THE ACTUAL ENTRAINMENT RATE WILL CHANGE DUE DUE FORCED TOTAL DETRAINMENT IN THIS 
! LAYER.
!--------------------------------------------------------------------------------------------------
                IF (NK == LTOP) THEN
                      UDR(NK) = UMF(NK-1)
                      UER(NK) = 0.
                    DETLQ(NK) = UDR(NK) * QLIQ(NK) * DILFRC(NK)
                    DETIC(NK) = UDR(NK) * QICE(NK) * DILFRC(NK)
                ELSE
                      UMF(NK) = UMF(NK-1) -   DP(NK) * DUMFDP
                      UER(NK) = UMF(NK  ) * (1. - 1. / DILFRC(NK))
                      UDR(NK) = UMF(NK-1) -  UMF(NK) +    UER(NK)
                    DETLQ(NK) = UDR(NK  ) * QLIQ(NK) * DILFRC(NK)
                    DETIC(NK) = UDR(NK  ) * QICE(NK) * DILFRC(NK)
                END IF
!------------------------------------------------------------------------------------
! REVERSE THE MOD THAT ALLOWS FEEDBACK OF RAIN/SNOW THAT ORIGINATES IN DETRAINED AIR.
!------------------------------------------------------------------------------------
                IF (NK >= LET+2) THEN
                    TRPPT      = TRPPT     - PPTLIQ(NK) - PPTICE(NK)
                    PPTLIQ(NK) = UMF(NK-1) * QLQOUT(NK)
                    PPTICE(NK) = UMF(NK-1) * QICOUT(NK)
                    TRPPT      = TRPPT     + PPTLIQ(NK) + PPTICE(NK)
                END IF
!
325             CONTINUE
!
350             CONTINUE
!-------------------------------------------------------------------------------------------- 
! SEND UPDRAFT CHARACTERISTICS TO OUTPUT FILES
!
! WHEN MOIST PERTURBATION IS ADDED TO UPDRAFT AND KLCL <= KPBL, RESET THETEE SO THAT MOISTURE 
! PERTURBATION ONLY AFFECTS UPDRAFTS.
!--------------------------------------------------------------------------------------------
                IF (KLCL <= KPBL) THEN
                    DO NK1=KLCL,KPBL
                        CALL ENVIRTHT(P0P(NK1), T0(NK1), Q0(NK1), THETEE(NK1), 0.  ,              &
    &                                 RL      , ALIQ   , BLIQ   , CLIQ       , DLIQ,              &
    &                                           AICE   , BICE   , CICE       , DICE)
                    END DO
                END IF
!
                XTIME = NTSD * DT / 60.
!-----------------------------------------------------------------------------------
! EXTEND THE UPDRAFT MASS FLUX PROFILE DOWN TO THE SOURCE LAYER FOR THE UPDRAFT AIR.
! ALSO, DEFINE THETAE FOR LEVELS BELOW THE LCL.
!-----------------------------------------------------------------------------------
                DO 360 NK=1,K
!
                    IF (NK >= LC) THEN
!
                        IF (NK == LC) THEN
                            UMF(NK) = VMFLCL * DP(NK) / DPTHMX
                            UER(NK) = VMFLCL * DP(NK) / DPTHMX
                        ELSE IF (NK <= KPBL) THEN
                            UER(NK) = VMFLCL * DP(NK) / DPTHMX
                            UMF(NK) = UMF(NK-1) + UER(NK)
                        ELSE
                            UMF(NK) = VMFLCL
                            UER(NK) = 0.
                        END IF
!
                        TU(NK) = TMIX + (Z00(NK) - ZMIX) * GDRY
                        QU(NK) = QMIX
                        WU(NK) = WLCL
!
                    ELSE
!
                         TU(NK) = 0.
                         QU(NK) = 0.
                        UMF(NK) = 0.
                         WU(NK) = 0.
                        UER(NK) = 0.
!
                    END IF
!
                       UDR(NK) = 0.
                       QDT(NK) = 0.
                      QLIQ(NK) = 0.
                      QICE(NK) = 0.
                    QLQOUT(NK) = 0.
                    QICOUT(NK) = 0.
                    PPTLIQ(NK) = 0.
                    PPTICE(NK) = 0.
                     DETLQ(NK) = 0.
                     DETIC(NK) = 0.
                    RATIO2(NK) = 0.
!
                    EE   = Q0(NK) * P0P(NK) / (0.622 + Q0(NK))
                    TLOG = ALOG(EE / ALIQ)
                    TDPT = (CLIQ - DLIQ * TLOG) / (BLIQ - TLOG)
                    TSAT = TDPT - (.212 + 1.571E-3 * (TDPT - T00) - 4.36E-4 * (T0(NK) - T00))     &
    &                    *                                                    (T0(NK) - TDPT)
!
                    THTA       = T0(NK) * (1.E5 / P0P(NK)) ** (0.2854 * (1. - 0.28 * Q0(NK)))
!
                    THETEE(NK) = THTA   * EXP((3374.6525 / TSAT   - 2.5403) *  Q0(NK)             &
    &                          * (1. + 0.81 * Q0(NK)))
!
                    THTES(NK)  = THTA   * EXP((3374.6525 / T0(NK) - 2.5403) * QES(NK)             &
    &                          * (1. + 0.81 * QES(NK)))
!
                    EQFRC(NK)  = 1.0
!
360                 CONTINUE
!----------------------------------
! FIND HEIGHT OF LFC, CALCULATE CIN
!----------------------------------
                    CIN1 = 0. ! DULE
!
                    IF (LFC > 0) THEN
                        CIN1 = 0.
                        IF (LFC == KLCL) THEN
!
                            IF (TVLCL > TVEN) THEN
                                CIN1 = 0.
                            ELSE
                                Z00LFC = Z00(LFC)
!
                                IF (Z00LFC /= ZLCL) THEN
                                    DTUDZ = (TVU(LFC) - TVLCL) / (Z00LFC - ZLCL)
                                    DTEDZ = (TV0(LFC) - TVEN ) / (Z00LFC - ZLCL)
!
                                    ZLFC  = ZLCL + (TVEN-TVLCL) / (DTUDZ-DTEDZ)
                                    TVLFC = TVEN + (ZLFC-ZLCL)  * DTEDZ
                                ELSE
                                    ZLFC  = ZLCL
                                    TVLFC = TVEN
                                END IF
!
                                 CIN1 = G * (ZLFC - ZLCL) * (TVLCL - TVEN) / (TVEN + TVLFC)
!
                            END IF
!
                        ELSE
!
                            CIN1 = CIN1 + G * (Z00(KLCL) - ZLCL) * ((TVU(KLCL) + TVLCL)           &
    &                            /                                  (TV0(KLCL) + TVEN) - 1.)
!
                            DO NK=KLCL+1,LFC
!
                                IF (NK == LFC) THEN
                                    DTUDZ = (TVU(LFC) - TVU(LFC-1)) / (Z00(LFC) - Z00(LFC-1))
                                    DTEDZ = (TV0(LFC) - TV0(LFC-1)) / (Z00(LFC) - Z00(LFC-1))
!
                                    ZLFC = Z00(LFC-1) + (TV0(LFC-1) - TV0(LFC-1)) / (DTUDZ - DTEDZ)
                                    TVEN = TV0(LFC-1) + (ZLFC - Z00(LFC-1)) * DTEDZ
!
                                    CIN1 = CIN1 + G * (ZLFC - Z00(LFC-1)) * (TVU(LFC-1)           &
    &                                    -                    TV0(LFC-1)) / (TVEN + TV0(LFC-1))
!
                                ELSE
!
                                    CIN1 = CIN1 + G * (Z00(NK) - Z00(NK-1))                       &
    &                                    *           ((TVU(NK) + TVU(NK-1))                       &
    &                                    /            (TV0(NK) + TV0(NK-1)) - 1.)
!
                                END IF
!
                            END DO
!
                        END IF
!
                    END IF
!
                    LTOP1  = LTOP + 1
                    LTOPM1 = LTOP - 1
!---------------------------------
! DEFINE VARIABLES ABOVE CLOUD TOP
!---------------------------------
                    DO NK=LTOP1,KX
                           UMF(NK) = 0.
                           UDR(NK) = 0.
                           UER(NK) = 0.
                           QDT(NK) = 0.
                          QLIQ(NK) = 0.
                          QICE(NK) = 0.
                        QLQOUT(NK) = 0.
                        QICOUT(NK) = 0.
                         DETLQ(NK) = 0.
                         DETIC(NK) = 0.
                        PPTLIQ(NK) = 0.
                        PPTICE(NK) = 0.
!
                        IF (NK > LTOP1) THEN
                            TU(NK) = 0.
                            QU(NK) = 0.
                            WU(NK) = 0.
                        END IF
!
                        THTA0(NK) = 0.
                        THTAU(NK) = 0.
                          EMS(NK) = 0.
                         EMSD(NK) = 0.
                           TG(NK) = T0(NK)
                           QG(NK) = Q0(NK)
                          QLG(NK) = 0.
                          QIG(NK) = 0.
                          QRG(NK) = 0.
                          QSG(NK) = 0.
                          OMG(NK) = 0.
                    END DO
!
                    OMG(KXP1) = 0.
!
                    P150 = P0P(KLCL) - 1.50E4
!
                    DO 375 NK=1,LTOP
                         EMS(NK) = DP(NK) * DXSQ / G
                        EMSD(NK) = 1. / EMS(NK)
!----------------------------------------------------------------------------
! INITIALIZE SOME VARIABLES TO BE USED LATER IN THE VERTICAL ADVECTION SCHEME
!----------------------------------------------------------------------------
                           EXN(NK) =   (P00 / P0P(NK)) ** (0.2854 * (1. - 0.28 * QDT(NK)))
                         THTAU(NK) = TU(NK) * EXN(NK)
                           EXN(NK) =   (P00 / P0P(NK)) ** (0.2854 * (1. - 0.28 *  Q0(NK)))
                         THTA0(NK) = T0(NK) * EXN(NK)
!-----------------------------------------------------------------------------------------------
! LVF IS THE LEVEL AT WHICH MOISTURE FLUX IS ESTIMATED AS THE BASIS FOR PRECIPITATION EFFICIENCY
! CALCULATIONS.
!-----------------------------------------------------------------------------------------------
                        IF (P0P(NK) > P150) LVF = NK
                        DDILFRC(NK) = 1. / DILFRC(NK)
                            OMG(NK) = 0.
!
375                     CONTINUE
!
                        LVF = MIN0(LVF,LET)
                        USR = UMF(LVF+1) * (QU(LVF+1) + QLIQ(LVF+1) + QICE(LVF+1))
                        USR = AMIN1(USR,TRPPT)
!-------------------------------------------------------------------------------------------
! COMPUTE CONVECTIVE TIME SCALE(TIMEC). THE MEAN WIND AT THE LCL AND MIDTROPOSPHERE IS USED.
!-------------------------------------------------------------------------------------------
                        WSPD(KLCL) = SQRT(U0(KLCL) * U0(KLCL) + V0(KLCL) * V0(KLCL))
                        WSPD(L44)  = SQRT(U0(L44)  * U0(L44)  + V0(L44)  * V0(L44))
                        WSPD(LTOP) = SQRT(U0(LTOP) * U0(LTOP) + V0(LTOP) * V0(LTOP))
!
                        VCONV  = 0.5 * (WSPD(KLCL) + WSPD(L44))
                        TIMEC  = DX(I,J) / VCONV
                        TADVEC = TIMEC
!
                        TIMEC = AMAX1(1800., TIMEC)
                        TIMEC = AMIN1(3600., TIMEC)
!
                        IF (ISHALL == 1) TIMEC = 2400.
!
                        NIC   = NINT(TIMEC / (0.5 * DT2))
                        TIMEC = FLOAT(NIC) *  0.5 * DT2
!-------------------------------------------------
! COMPUTE WIND SHEAR AND PRECIPITATION EFFICIENCY.
!-------------------------------------------------
                        IF (WSPD(LTOP) > WSPD(KLCL)) THEN
                            SHSIGN =  1.
                        ELSE
                            SHSIGN = -1.
                        END IF
!
                        VWS = (U0(LTOP) - U0(KLCL))                                               &
    &                       * (U0(LTOP) - U0(KLCL)) + (V0(LTOP) - V0(KLCL))                       &
    &                       * (V0(LTOP) - V0(KLCL))
!
                        VWS = 1.E3  * SHSIGN * SQRT(VWS) / (Z00(LTOP) - Z00(LCL))
                        PEF = 1.591 + VWS    * (-.639 + VWS * (9.53E-2 - VWS * 4.96E-3))
                        PEF = AMAX1(PEF, .2)
                        PEF = AMIN1(PEF, .9)
!-------------------------------------------------------------------
! PRECIPITATION EFFICIENCY IS A FUNCTION OF THE HEIGHT OF CLOUD BASE
!-------------------------------------------------------------------
                        CBH = (ZLCL - Z00(1)) * 3.281E-3
!
                        IF (CBH < 3.) THEN
                            RCBH =  .02
                        ELSE
                            RCBH = 0.96729352 + CBH * (- .70034167   + CBH * ( .162179896 + CBH   &
    &                            *                    (-1.2569798E-2 + CBH                        &
    &                            *                    ( 4.2772E-4    - CBH * 5.44E-6))))
                        END IF
!
                        IF (CBH > 25) RCBH = 2.4
!
                        PEFCBH = 1. / (1. + RCBH)
                        PEFCBH = AMIN1(PEFCBH,.9)
!----------------------------------------------------
! MEAN PRECIP EFFICIENCY IS USED TO COMPUTE RAINFALL.
!----------------------------------------------------
                        PEFF  = .5 * (PEF + PEFCBH)
                        PEFF2 = PEFF
!-----------------------------
! COMPUTE DOWNDRAFT PROPERTIES 
!-----------------------------
                        TDER = 0.
!
                        IF (ISHALL == 1) THEN
                            LFS = 1
                            GOTO 450
                        END IF
!----------------------------------------------
! START DOWNDRAFT ABOUT 150 MB ABOVE CLOUD BASE
!----------------------------------------------
                        KSTART = KPBL + 1
!
                        DO NK=KSTART+1,KL
                            DPPP = P0P(KSTART) - P0P(NK)
                            IF (DPPP > 150.E2) THEN
                                KLFS = NK
                                GOTO 405
                            END IF
                        END DO
!
405                     CONTINUE
!
                        KLFS = MIN0(KLFS,LET-1)
                        LFS  = KLFS
!--------------------------------------------------------------------------------------------------
! IF LFS IS NOT AT LEAST 50 MB ABOVE CLOUD BASE (IMPLYING THAT THE LEVEL OF EQUIL TEMP, LET, IS 
! JUST ABOVE CLOUD BASE) DO NOT ALLOW A DOWNDRAFT.
!--------------------------------------------------------------------------------------------------
                        IF ((P0P(KSTART) - P0P(LFS)) < 50.E2) THEN
                            TDER = 0.
                            GOTO 450
                        END IF
!
                        THETED(LFS) = THETEE(LFS)
                            QD(LFS) =     Q0(LFS)
!----------------------------------------
! CALL TPMIX2DD TO FIND WET-BULB TEMP, QV
!----------------------------------------
                        CALL TPMIX2DD(P0P(LFS), THETED(LFS), TZ(LFS), QSS)
!
                        THTAD(LFS) = TZ(LFS) * (P00 / P0P(LFS)) ** (0.2854 * (1. - 0.28 * QSS))
!---------------------------------------------------------
! TAKE A FIRST GUESS AT THE INITIAL DOWNDRAFT MASS FLUX...
!---------------------------------------------------------
                        TVD(LFS) = TZ(LFS)  * (1. + 0.608 * QSS)
                        RDD      = P0P(LFS) / (R  * TVD(LFS))
                        A1       = (1. - PEFF) * AU0
                        DMF(LFS) = -A1 * RDD
                        DER(LFS) = DMF(LFS)
                        DDR(LFS) = 0.
                        RHBAR    = RH(LFS) * DP(LFS)
                        DPTT     = DP(LFS)
!
                        DO ND=LFS-1,KSTART,-1
                            ND1 = ND + 1
                               DER(ND) = DER(LFS) * EMS(ND) / EMS(LFS)
                               DDR(ND) = 0.
                               DMF(ND) =                DMF(ND1) +    DER(ND)
                            THETED(ND) = (THETED(ND1) * DMF(ND1) + THETEE(ND) * DER(ND)) / DMF(ND)
                                QD(ND) = (    QD(ND1) * DMF(ND1) +     Q0(ND) * DER(ND)) / DMF(ND)
!
                            DPTT  = DPTT  + DP(ND)
                            RHBAR = RHBAR + RH(ND) * DP(ND)
                        END DO
!
                        RHBAR  = RHBAR / DPTT
                        DMFFRC = 2. * (1. - RHBAR)
                        DPDD   = 0.
!----------------------------------------------------- 
! CALCULATE MELTING EFFECT.
! FIRST, COMPUTE TOTAL FROZEN PRECIPITATION GENERATED.
!-----------------------------------------------------
                        PPTMLT = 0.
!
                        DO NK=KLCL,LTOP
                            PPTMLT = PPTMLT + PPTICE(NK)
                        END DO
!
                        IF (LC < ML) THEN
!--------------------------------------------------------------------------------------------------
! FOR NOW, CALCULATE MELTING EFFECT AS IF DMF=-UMF AT KLCL, I.E., AS IF DMFFRC=1. OTHERWISE, FOR 
! SMALL DMFFRC, DTMELT GETS TOO LARGE.
!--------------------------------------------------------------------------------------------------
                            DTMELT = RLF * PPTMLT / (CP * UMF(KLCL))
                        ELSE
                            DTMELT = 0.
                        END IF
!
                        LDT = MIN0(LFS-1, KSTART-1)
!
                        CALL TPMIX2DD(P0P(KSTART), THETED(KSTART), TZ(KSTART), QSS)
!
                        TZ(KSTART) = TZ(KSTART) - DTMELT
                        ES         = ALIQ  * EXP((BLIQ * TZ(KSTART) - CLIQ) / (TZ(KSTART) - DLIQ))
                        QSS        = 0.622 * ES / (P0P(KSTART) - ES)
!
                        THETED(KSTART) = TZ(KSTART) * (1.E5 / P0P(KSTART)) ** (0.2854             &
    &                                  * (1. - 0.28 * QSS)) * EXP((3374.6525 / TZ(KSTART)         &
    &                                  -    2.5403) * QSS   * (1. + 0.81 * QSS))
!
                        LDT = MIN0(LFS-1, KSTART-1)
!
                        DO 425 ND=LDT,1,-1
                            DPDD       = DPDD + DP(ND)
                            THETED(ND) = THETED(KSTART)
                                QD(ND) =     QD(KSTART)
!-------------------------------------------------------------
! CALL TPMIX2DD TO FIND WET BULB TEMP, SATURATION MIXING RATIO
!-------------------------------------------------------------
                            CALL TPMIX2DD(P0P(ND), THETED(ND), TZ(ND), QSS)
!
                            QSD(ND) = QSS
!-------------------------------------------
! SPECIFY RH DECREASE OF 10%/KM IN DOWNDRAFT
!-------------------------------------------
                            RHH = 1. - 0.2 / 1000. * (Z00(KSTART) - Z00(ND))
!-----------------------------------------
! ADJUST DOWNDRAFT TEMP, Q TO SPECIFIED RH
!-----------------------------------------
                            IF (RHH < 1.) THEN
                                DSSDT = (CLIQ - BLIQ * DLIQ) / ((TZ(ND) - DLIQ) * (TZ(ND) - DLIQ))
                                RL   = XLV0 - XLV1 * TZ(ND)
                                DTMP = RL * QSS * (1. - RHH) / (CP + RL * RHH * QSS * DSSDT)
                                T1RH = TZ(ND) + DTMP
                                ES   = RHH * ALIQ * EXP((BLIQ * T1RH - CLIQ) / (T1RH - DLIQ))
                                QSRH = 0.622 * ES / (P0P(ND) - ES)
!-------------------------------------------------------------
! CHECK TO SEE IF MIXING RATIO AT SPECIFIED RH IS LESS THAN
! ACTUAL MIXING RATIO. IF SO, ADJUST TO GIVE ZERO EVAPORATION.
!-------------------------------------------------------------
                                IF (QSRH < QD(ND)) THEN
                                    QSRH = QD(ND)
                                    T1RH = TZ(ND) + (QSS - QSRH) * RL / CP
                                END IF
!
                                 TZ(ND) = T1RH
                                QSS     = QSRH
                                QSD(ND) = QSS
!
                            END IF
!
                            TVD(ND) = TZ(ND) * (1. + 0.608 * QSD(ND))
!
                            IF (TVD(ND) > TV0(ND) .OR. ND == 1) THEN
                                LDB = ND
                                GOTO 430
                            END IF
!
425                     CONTINUE
!
430                 CONTINUE
!
                    IF ((P0P(LDB) - P0P(LFS)) < 50.E2) THEN ! NO DOWNDRAFT ALLOWED!
                        TDER = 0.
                        GOTO 450
                    END IF
!
                    TDER = 0.
!--------------------------------------------------
! CALCULATE AN EVAPORATION RATE FOR GIVEN MASS FLUX
!--------------------------------------------------
            DO ND=LDT,LDB,-1
                ND1       = ND + 1
                  DDR(ND) = -DMF(KSTART) * DP(ND) / DPDD
                  DER(ND) = 0.
                  DMF(ND) = DMF(ND1) + DDR(ND)
                TDER      = TDER + (QSD(ND) - QD(ND)) * DDR(ND)
                   QD(ND) = QSD(ND)
                THTAD(ND) = TZ(ND) * (P00 / P0P(ND)) ** (0.2854 * (1. - 0.28 * QD(ND)))
            END DO
!
450     CONTINUE
!--------------------------------------------------------------------------
! IF DOWNDRAFT DOES NOT EVAPORATE ANY WATER FOR SPECIFIED RELATIVE HUMIDITY
! NO DOWNDRAFT IS ALLOWED.
!--------------------------------------------------------------------------
        IF (TDER < 1.) THEN
            PPTFLX = TRPPT
            CPR    = TRPPT
            TDER   = 0.
            CNDTNF = 0.
            UPDINC = 1.
            LDB    = LFS
!
            DO NDK=1,LTOP
                  DMF(NDK) = 0.
                  DER(NDK) = 0.
                  DDR(NDK) = 0.
                THTAD(NDK) = 0.
                   WD(NDK) = 0.
                   TZ(NDK) = 0.
                   QD(NDK) = 0.
            END DO
!
            AINCM2 = 100.
!
            GOTO 475
!
        END IF
!--------------------------------------------------------------------------------------------------
! ADJUST DOWNDRAFT MASS FLUX SO THAT EVAPORATION RATE IN DOWNDRAFT IS CONSISTENT WITH PRECIPITATION
! EFFICIENCY RELATIONSHIP.
!
! DDINC IS THE FACTOR BY WHICH TO INCREASE THE FIRST-GUESS DOWNDRAFT MOMENTUM FLUX TO SATISFY THE 
! PRECIP EFFICIENCY RELATIONSHIP.
!
! UPDINC IS THE FACTOR BY WHICH TO INCREASE THE UPDRAFT MASS FLUX BELOW THE LFS TO ACCOUNT FOR 
! TRANSFER OF MASS FROM UPDRAFT TO DOWNDRAFT.
!--------------------------------------------------------------------------------------------------
        DDINC  = -DMFFRC * UMF(KLCL) / DMF(KSTART)
        UPDINC = 1.
!
        IF (TDER * DDINC > TRPPT) THEN
            DDINC = TRPPT / TDER
        END IF
!
        TDER = TDER * DDINC
!
        DO NK=LDB,LFS
            DMF(NK) = DMF(NK) * DDINC
            DER(NK) = DER(NK) * DDINC
            DDR(NK) = DDR(NK) * DDINC
        END DO
!
        CPR    = TRPPT
        PPTFLX = TRPPT - TDER
!----------------------------------------------------------------------------------------------
! ADJUST UPDRAFT MASS FLUX, MASS DETRAINMENT RATE, AND LIQUID WATER AND DETRAINMENT RATES TO BE
! CONSISTENT WITH THE TRANSFER OF THE ESTIMATE FROM THE UPDRAFT TO THE DOWNDRAFT AT THE LFS.
!----------------------------------------------------------------------------------------------
        DO NK=LC,LFS
               UMF(NK) =    UMF(NK) * UPDINC
               UDR(NK) =    UDR(NK) * UPDINC
               UER(NK) =    UER(NK) * UPDINC
            PPTLIQ(NK) = PPTLIQ(NK) * UPDINC
            PPTICE(NK) = PPTICE(NK) * UPDINC
             DETLQ(NK) =  DETLQ(NK) * UPDINC
        END DO
!-------------------------------------------------------------------------------
! ZERO OUT THE ARRAYS FOR DOWNDRAFT DATA AT LEVELS ABOVE AND BELOW THE DOWNDRAFT
!-------------------------------------------------------------------------------
        IF (LDB > 1) THEN
            DO NK=1,LDB-1
                  DMF(NK) = 0.
                  DER(NK) = 0.
                  DDR(NK) = 0.
                   WD(NK) = 0.
                   TZ(NK) = 0.
                   QD(NK) = 0.
                THTAD(NK) = 0.
            END DO
        END IF
!
         DO NK=LFS+1,KX
              DMF(NK) = 0.
              DER(NK) = 0.
              DDR(NK) = 0.
               WD(NK) = 0.
               TZ(NK) = 0.
               QD(NK) = 0.
            THTAD(NK) = 0.
        END DO
!
        DO NK=LDT+1,LFS-1
               TZ(NK) = 0.
               QD(NK) = 0.
            THTAD(NK) = 0.
        END DO
!
475     CONTINUE
!----------------------------------------------------------------------------------------------
! SET LIMITS ON THE UPDRAFT AND DOWNDRAFT MASS FLUXES SO THAT THE INFLOW INTO CONVECTIVE DRAFTS 
! FROM A GIVEN LAYER IS NO MORE THAN IS AVAILABLE IN THAT LAYER INITIALLY.
!----------------------------------------------------------------------------------------------
        AINCMX = 1000.
        LMAX   = MAX0(KLCL,LFS)
!
        DO NK=LC,LMAX
            IF ((UER(NK) - DER(NK)) > 1.E-3) THEN
                AINCM1 = EMS(NK) / ((UER(NK) - DER(NK)) * TIMEC)
                AINCMX = AMIN1(AINCMX, AINCM1)
            END IF
        END DO
!
        AINC = 1.
!
        IF (AINCMX < AINC) AINC = AINCMX
!-------------------------------------------------------------------------------------------
! SAVE THE RELEVENT VARIABLES FOR A UNIT UPDRFT AND DOWNDRFT.
! THEY WILL BE ADJUSTED ITERATIVELY BY THE FACTOR AINC TO SATISFY THE STABILIZATION CLOSURE.
!-------------------------------------------------------------------------------------------
        NCOUNT = 0
        TDER2  = TDER
        PPTFL2 =  PPTFLX
!
        DO NK=1,LTOP
            DETLQ2(NK) = DETLQ(NK)
            DETIC2(NK) = DETIC(NK)
              UDR2(NK) =   UDR(NK)
              UER2(NK) =   UER(NK)
              DDR2(NK) =   DDR(NK)
              DER2(NK) =   DER(NK)
              UMF2(NK) =   UMF(NK)
              DMF2(NK) =   DMF(NK)
        END DO
!
        FABE  = 1.
        STAB  = 0.95
        NOITR = 0
        ISTOP = 0
!
        IF (AINC / AINCMX > 0.999) THEN
            NCOUNT = 0
            GOTO 575
        END IF
!--------------------------- 
! SHALLOW CONVECTION EFFECTS
!--------------------------- 
        IF (ISHALL == 1) THEN
!-----------------------------------------------
! FIND THE MAXIMUM TKE VALUE BETWEEN LC AND KLCL
!-----------------------------------------------
            TKEMAX = 0.
!
            DO K=LC,KLCL
                NK     = KX - K + 1 - NLEV
                TKEMAX = AMAX1(TKEMAX, Q2(I,J,NK))
            END DO
!
            TKEMAX = AMIN1(TKEMAX, 10.)
            TKEMAX = AMAX1(TKEMAX,  5.)
!--------------------------------------------------------------------------------------------------
! DPMIN WAS CHANGED FOR SHALLOW CONVECTION SO THAT IT IS THE THE SAME AS FOR DEEP CONVECTION (5.E3)
! SINCE THIS DOUBLES (ROUGHLY) THE VALUE OF DPTHMX, ADD A FACTOR OF 0.5 TO THE CALCULATION OF EVAC.
!--------------------------------------------------------------------------------------------------
            EVAC = 0.5  * TKEMAX * 0.1
            AINC = EVAC * DPTHMX * DX(I,J) * DX(I,J) / (VMFLCL * G * TIMEC)
            GOTO 575
        END IF
!-------------------------------------------------
! COMPUTE PROPERTIES FOR COMPENSATIONAL SUBSIDENCE
!-------------------------------------------------
500     NCOUNT = NCOUNT + 1
!--------------------------------------------------------------------------------------------
! DETERMINE OMEGA VALUE NECESSARY AT TOP AND BOTTOM OF EACH LAYER TO SATISFY MASS CONTINUITY.
!--------------------------------------------------------------------------------------------
        DTT = TIMEC
!
        DO NK=1,LTOP
!
            DOMGDP(NK) = -(UER(NK) - DER(NK) - UDR(NK) - DDR(NK)) * EMSD(NK)
!
            IF (NK > 1) THEN
!
                OMG(NK)  = OMG(NK-1) - DP(NK-1) * DOMGDP(NK-1)
                ABSOMG   = ABS(OMG(NK))
                ABSOMGTC = ABSOMG * TIMEC
                FRDP     = 0.75 * DP(NK-1)
!
                IF (ABSOMGTC > FRDP) THEN
                    DTT1 = FRDP / ABSOMG
                    DTT  = AMIN1(DTT, DTT1)
                END IF
!
            END IF
        END DO
!
        DO NK=1,LTOP
           THPA(NK) = THTA0(NK)
            QPA(NK) =    Q0(NK)
              NSTEP = NINT(TIMEC / DTT + 1)
              DTIME = TIMEC / FLOAT(NSTEP)
            FXM(NK) =   OMG(NK) * DXSQ / G
        END DO
!------------------------------------------------------
! DO AN UPSTREAM/FORWARD-IN-TIME ADVECTION OF THETA, QV
!------------------------------------------------------
        DO 525 NTC=1,NSTEP
!-----------------------------------------------------------------------------------
! ASSIGN THETA AND Q VALUES AT THE TOP AND BOTTOM OF EACH LAYER BASED SIGN OF OMEGA.
!-----------------------------------------------------------------------------------
            DO NK=1,LTOP
                 THFXIN(NK) = 0.
                THFXOUT(NK) = 0.
                  QFXIN(NK) = 0.
                 QFXOUT(NK) = 0.
            END DO
!
            DO NK=2,LTOP
!
                IF (OMG(NK) <= 0.) THEN
                     THFXIN(NK) = -FXM(NK) * THPA(NK-1)
                      QFXIN(NK) = -FXM(NK) *  QPA(NK-1)
!
                    THFXOUT(NK-1) = THFXOUT(NK-1) + THFXIN(NK)
                     QFXOUT(NK-1) =  QFXOUT(NK-1) +  QFXIN(NK)
                ELSE
                    THFXOUT(NK) = FXM(NK) * THPA(NK)
                     QFXOUT(NK) = FXM(NK) *  QPA(NK)
!
                    THFXIN(NK-1) = THFXIN(NK-1) + THFXOUT(NK)
                     QFXIN(NK-1) =  QFXIN(NK-1) +  QFXOUT(NK)
                END IF
!
            END DO
!---------------------------------------------
! UPDATE THE THETA AND QV VALUES AT EACH LEVEL
!---------------------------------------------
            DO NK=1,LTOP
                THPA(NK) =  THPA(NK) + ( THFXIN(NK) +  UDR(NK) * THTAU(NK)  +   DDR(NK)           &
    &                    * THTAD(NK) -  THFXOUT(NK) - (UER(NK) -   DER(NK)) * THTA0(NK))          &
    &                    * DTIME     *     EMSD(NK)
!
                IF (NK >= LC .AND. NK <= KPBL) THEN
!
                    QPA(NK) = QPA(NK) + ( QFXIN(NK) + UDR(NK) *  QDT(NK) + DDR(NK) * QD(NK)       &
    &                       -            QFXOUT(NK) - UER(NK) * QUER(NK) + DER(NK) * Q0(NK))      &
    &                       * DTIME   *    EMSD(NK)
!
                ELSE
!
                    QPA(NK) = QPA(NK) + ( QFXIN(NK) +  UDR(NK) * QDT(NK)  + DDR(NK)  * QD(NK)     &
    &                       -            QFXOUT(NK) - (UER(NK) - DER(NK)) *  Q0(NK))              &
    &                       * DTIME   *    EMSD(NK)
!
                END IF
            END DO
!
525     CONTINUE
!
        DO NK=1,LTOP
            THTAG(NK) = THPA(NK)
               QG(NK) =  QPA(NK)
        END DO
!--------------------------------------------------------------------------------------------------
! CHECK TO SEE IF MIXING RATIO DIPS BELOW ZERO ANYWHERE. IF SO BORROW MOISTURE FROM ADJACENT LAYERS
! TO BRING IT BACK UP ABOVE ZERO.
!--------------------------------------------------------------------------------------------------
        DO 530 NK=1,LTOP
! 
            IF (QG(NK) < 0.) THEN
!
                IF (NK == 1) THEN
                    PRINT *, 'PROBLEM WITH KF SCHEME:'
                    PRINT *, 'QG = 0 AT THE SURFACE !'
                    PRINT *, I, J, MYPE
                END IF
! 
                NK1 = NK+1
!
                IF (NK == LTOP) NK1 = KLCL
!
                TMA =  QG(NK1)       * EMS(NK1)
                TMB =  QG(NK-1)      * EMS(NK-1)
                TMM = (QG(NK)-1.E-9) * EMS(NK)
!
                BCOEFF = -TMM / ((TMA * TMA) / TMB + TMB)
                ACOEFF = BCOEFF * TMA / TMB
!
                TMB = TMB * (1. - BCOEFF)
                TMA = TMA * (1. - ACOEFF)
! 
                IF (NK == LTOP) THEN
                    QVDIFF = (QG(NK1) - TMA * EMSD(NK1)) * 100. / QG(NK1)
!
                    IF (ABS(QVDIFF) > 1.) THEN
                        PRINT*, 'WARNING ! CLOUD BASE WATER VAPOR CHANGES BY ', QVDIFF,           &
    &                           '% WHEN MOISTURE IS BORROWED TO PREVENT NEG VALUES IN KAIN-FRITSCH'
                    END IF
! 
                END IF
! 
                QG(NK)   = 1.E-9
                QG(NK1)  = TMA * EMSD(NK1)
                QG(NK-1) = TMB * EMSD(NK-1)
            END IF
!
530     CONTINUE
!
        TOPOMG = (UDR(LTOP) - UER(LTOP)) * DP(LTOP) * EMSD(LTOP)
! 
        IF (ABS(TOPOMG-OMG(LTOP)) > 1.E-3) THEN
            WRITE(99,*) 'ERR:  MASS NOT BALANCED IN KF SCHEME; TOPOMG, OMG =', TOPOMG, OMG(LTOP)
            WRITE(6,* ) 'I, J, NTSD =', I, J, NTSD
            WRITE(6,* ) 'ERR:  MASS NOT BALANCED IN KF SCHEME; TOPOMG, OMG =', TOPOMG, OMG(LTOP)
!
            ISTOP = 1
            GOTO 600
!
        END IF
!-------------------
! CONVERT THETA TO T
!-------------------
        DO NK=1,LTOP
            EXN(NK) = (P00 / P0P(NK)) ** (0.2854 * (1. - 0.28 * QG(NK)))
             TG(NK) = THTAG(NK) / EXN(NK)
            TVG(NK) = TG(NK) * (1. + 0.608 * QG(NK))
        END DO
!--------
! SHALLOW
!--------
        IF (ISHALL == 1) THEN
            GOTO 600
        END IF
!---------------------------------------------------------- 
! COMPUTE NEW CLOUD AND CHANGE IN AVAILABLE BUOYANT ENERGY.
!---------------------------------------------------------- 
!
!----------------------------------------------------------- 
! THE FOLLOWING COMPUTATIONS ARE SIMILAR TO THAT FOR UPDRAFT
!----------------------------------------------------------- 
        TMIX = 0.
        QMIX = 0.
!-------------------------------------------------------------------------------------------------
! FIND THE THERMODYNAMIC CHARACTERISTICS OF THE LAYER BY MASS-WEIGHTING THE CHARACTERISTICS OF THE
! INDIVIDUAL MODEL LAYERS.
!-------------------------------------------------------------------------------------------------
        DO NK=LC,KPBL
            TMIX = TMIX + DP(NK) * TG(NK)
            QMIX = QMIX + DP(NK) * QG(NK)
        END DO
!
        TMIX = TMIX  / DPTHMX
        QMIX = QMIX  / DPTHMX
        ES   = ALIQ  * EXP((TMIX * BLIQ - CLIQ) / (TMIX - DLIQ))
        QSS  = 0.622 * ES / (PMIX - ES)
!------------------------------------------------------------- 
! REMOVE SUPERSATURATION FOR DIAGNOSTIC PURPOSES, IF NECESSARY
!------------------------------------------------------------- 
        IF (QMIX > QSS) THEN
            RL    = XLV0 - XLV1 * TMIX
            CPM   = CP  * (1. + 0.887 * QMIX)
            DSSDT = QSS * (CLIQ - BLIQ * DLIQ) / ((TMIX - DLIQ) * (TMIX - DLIQ))
            DQ    = (QMIX - QSS) / (1. + RL * DSSDT / CPM)
            TMIX  = TMIX  + RL   / CP  * DQ
            QMIX  = QMIX  - DQ
            TLCL  = TMIX
        ELSE
            QMIX   = AMAX1(QMIX, 0.)
            EMIX   = QMIX * PMIX / (0.622 + QMIX)
            ASTRT  = 1.E-3
            BINC   = 0.075
            A1     = EMIX / ALIQ
            TP     = (A1 - ASTRT) / BINC
            INDLU  = INT(TP) + 1
            VALUE  = (INDLU - 1) * BINC + ASTRT
            AINTRP = (A1 - VALUE) / BINC
            TLOG   = AINTRP * ALU(INDLU + 1) + (1 - AINTRP) * ALU(INDLU)
            TDPT   = (CLIQ - DLIQ * TLOG) / (BLIQ - TLOG)
            TLCL   = TDPT - (.212 + 1.571E-3 * (TDPT-T00) - 4.36E-4 * (TMIX - T00)) * (TMIX - TDPT)
            TLCL   = AMIN1(TLCL, TMIX)
        END IF
!
        TVLCL = TLCL * (1. + 0.608 * QMIX)
        ZLCL  = ZMIX + (TLCL - TMIX) / GDRY
!
        DO NK=LC,KL
            KLCL = NK
            IF (ZLCL <= Z00(NK)) GOTO 550
        END DO
!
        PRINT*, 'PROBLEM... LCL CANNOT BE FOUND AFTER CONV ADJUSTMENT !'
        STOP    'KF BAD ADJUST'
!
550     CONTINUE
!
        K   =  KLCL - 1
        DLP = (ZLCL - Z00(K)) / (Z00(KLCL) - Z00(K))
!--------------------------------------------------------------- 
! ESTIMATE ENVIRONMENTAL TEMPERATURE AND MIXING RATIO AT THE LCL
!--------------------------------------------------------------- 
        TENV = TG(K) + (TG(KLCL) - TG(K)) * DLP
        QENV = QG(K) + (QG(KLCL) - QG(K)) * DLP
!
        TVEN = TENV   * (1. + 0.608 * QENV)
        PLCL = P0P(K) + (P0P(KLCL) - P0P(K)) * DLP
!
        THETEU(K) = TMIX * (1.E5 / PMIX) ** (0.2854 * (1.-0.28 * QMIX)) *                         &
    &               EXP((3374.6525 / TLCL - 2.5403) * QMIX * (1. + 0.81 * QMIX))
!
        ES        = ALIQ  * EXP((TENV  * BLIQ - CLIQ) / (TENV - DLIQ))
        QESE      = 0.622 * ES / (PLCL - ES)
!
        THTESG(K) = TENV * (1.E5 / PLCL) ** (0.2854 * (1. - 0.28 * QESE)) *                       &
    &               EXP((3374.6525 / TENV - 2.5403) * QESE * (1. + 0.81 * QESE))
!---------------------------
! COMPUTE ADJUSTED ABE(ABEG)
!--------------------------- 
        ABEG     = 0.
        THTUDL   = THETEU(K)
        THEU_ADJ = THTUDL
!
        DO 560 NK=K,LTOPM1
!
            NK1 = NK + 1
!
            THETEU(NK1) = THETEU(NK)
!
            CALL TP_CAPE(P0P(NK1), THETEU(NK1), TGU(NK1), QGU(NK1))
!
            TVQU(NK1) = TGU(NK1) * (1. + 0.608 * QGU(NK1) - QLIQ(NK1) - QICE(NK1))
!
            IF(NK == K) THEN
                DZZ   = Z00(KLCL) - ZLCL
                DILBE = ((TVLCL    + TVQU(NK1)) / (TVEN    + TVG(NK1)) - 1.) * DZZ
            ELSE
                DZZ   =    DZA(NK)
                DILBE = ((TVQU(NK) + TVQU(NK1)) / (TVG(NK) + TVG(NK1)) - 1.) * DZZ
            END IF
!
            IF (DILBE > 0.) ABEG = ABEG + DILBE * G
!------------------------------------------------------
! DILUTE BY ENTRAINMENT BY THE RATE AS ORIGINAL UPDRAFT
!------------------------------------------------------
            CALL ENVIRTHT(P0P(NK1), TG(NK1) , QG(NK1) , THTEEG(NK1)       , 0.      , RL      ,   &
    &                     ALIQ    , BLIQ    , CLIQ    , DLIQ              ,                       &
    &                     AICE    , BICE    , CICE    , DICE)
!
            THETEU(NK1) = THETEU(NK1) * DDILFRC(NK1) + THTEEG(NK1) * (1. - DDILFRC(NK1))
560     CONTINUE
!-----------------------------------------------------------------------------------
! ASSUME AT LEAST 90% OF CAPE (ABE) IS REMOVED BY CONVECTION DURING THE PERIOD TIMEC
!-----------------------------------------------------------------------------------
        IF (NOITR == 1) THEN
            GOTO 600
        END IF
!
        DABE = AMAX1(ABE - ABEG,0.1 * ABE)
        FABE = ABEG/(ABE + 1.E-8)
!
        IF (FABE > 1. .AND. ISHALL == 0) THEN
            GOTO 900
        END IF
!
        IF (NCOUNT /=  1) THEN
!
            IF (ABS(AINC-AINCOLD) < 0.0001) THEN
                NOITR = 1
                AINC  = AINCOLD
                GOTO 575
            END IF
!
            DFDA = (FABE - FABEOLD) / (AINC - AINCOLD)
!
            IF (DFDA > 0.) THEN
                NOITR = 1
                AINC  = AINCOLD
                GOTO 575
            END IF
!
        END IF
!
        AINCOLD = AINC
        FABEOLD = FABE
!
        IF (AINC/AINCMX > 0.999 .AND. FABE > 1.05-STAB) THEN
            GOTO 600
        END IF
!
        IF (FABE <= 1.05-STAB .AND. FABE >= 0.95-STAB) GOTO 600
!
        IF (NCOUNT > 10) THEN
            GOTO 600
        END IF
!-----------------------------------------------------
! IF MORE THAN 10% OF THE ORIGINAL CAPE REMAINS;
! INCREASE THE CONVECTIVE MASS FLUX BY THE FACTOR AINC
!-----------------------------------------------------
        IF (FABE == 0.) THEN
            AINC = AINC * 0.5
        ELSE
            IF (DABE < 1.E-4) THEN
                NOITR = 1
                AINC  = AINCOLD
            ELSE
                AINC  = AINC * STAB * ABE / DABE
            END IF
        END IF
!
575     AINC = AMIN1(AINCMX, AINC)
!-------------------------------------------------------------------------------------
! IF AINC BECOMES VERY SMALL, EFFECTS OF CONVECTION WILL BE MINIMAL SO JUST IGNORE IT.
!-------------------------------------------------------------------------------------
        IF (AINC < 0.05) THEN
            GOTO 900
        END IF
!
        TDER   = TDER2  * AINC
        PPTFLX = PPTFL2 * AINC
!
        DO NK=1,LTOP
              UMF(NK) =   UMF2(NK) * AINC
              DMF(NK) =   DMF2(NK) * AINC
            DETLQ(NK) = DETLQ2(NK) * AINC
            DETIC(NK) = DETIC2(NK) * AINC
              UDR(NK) =   UDR2(NK) * AINC
              UER(NK) =   UER2(NK) * AINC
              DER(NK) =   DER2(NK) * AINC
              DDR(NK) =   DDR2(NK) * AINC
        END DO
!---------------------------------
! GO BACK UP FOR ANOTHER ITERATION
!---------------------------------
        GOTO 500
!
600     CONTINUE
!---------------------------------------------------------------------------------
! COMPUTE HYDROMETEOR TENDENCIES AS IS DONE FOR T, QV
!
! FRC2 IS THE FRACTION OF TOTAL CONDENSATE GENERATED THAT GOES INTO PRECIPITIATION
!---------------------------------------------------------------------------------
        IF (CPR > 0.) THEN
            FRC2 = PPTFLX / (CPR * AINC)
        ELSE
            FRC2 = 0.
        END IF
!
        DO NK=1,LTOP
              QLPA(NK) = QL0(NK)
              QIPA(NK) = QI0(NK)
              QRPA(NK) = QR0(NK)
              QSPA(NK) = QS0(NK)
            RAINFB(NK) = PPTLIQ(NK) * AINC * FBFRC * FRC2
            SNOWFB(NK) = PPTICE(NK) * AINC * FBFRC * FRC2
        END DO
!
        DO 625 NTC=1,NSTEP
!--------------------------------------------------------------------------------------------------
! ASSIGN HYDROMETEORS CONCENTRATIONS AT THE TOP AND BOTTOM OF EACH LAYER BASED ON THE SIGN OF OMEGA
!--------------------------------------------------------------------------------------------------
            DO NK=1,LTOP
                 QLFXIN(NK) = 0.
                QLFXOUT(NK) = 0.
                 QIFXIN(NK) = 0.
                QIFXOUT(NK) = 0.
                 QRFXIN(NK) = 0.
                QRFXOUT(NK) = 0.
                 QSFXIN(NK) = 0.
                QSFXOUT(NK) = 0.
            END DO
!
            DO NK=2,LTOP
                IF (OMG(NK) <= 0.) THEN
                    QLFXIN(NK) = -FXM(NK) * QLPA(NK-1)
                    QIFXIN(NK) = -FXM(NK) * QIPA(NK-1)
                    QRFXIN(NK) = -FXM(NK) * QRPA(NK-1)
                    QSFXIN(NK) = -FXM(NK) * QSPA(NK-1)
!
                    QLFXOUT(NK-1) = QLFXOUT(NK-1) + QLFXIN(NK)
                    QIFXOUT(NK-1) = QIFXOUT(NK-1) + QIFXIN(NK)
                    QRFXOUT(NK-1) = QRFXOUT(NK-1) + QRFXIN(NK)
                    QSFXOUT(NK-1) = QSFXOUT(NK-1) + QSFXIN(NK)
!
                ELSE
!
                    QLFXOUT(NK)  = FXM(NK) * QLPA(NK)
                    QIFXOUT(NK)  = FXM(NK) * QIPA(NK)
                    QRFXOUT(NK)  = FXM(NK) * QRPA(NK)
                    QSFXOUT(NK)  = FXM(NK) * QSPA(NK)
!
                    QLFXIN(NK-1) = QLFXIN(NK-1) + QLFXOUT(NK)
                    QIFXIN(NK-1) = QIFXIN(NK-1) + QIFXOUT(NK)
                    QRFXIN(NK-1) = QRFXIN(NK-1) + QRFXOUT(NK)
                    QSFXIN(NK-1) = QSFXIN(NK-1) + QSFXOUT(NK)
                END IF
            END DO
!----------------------------------------------------------
! UPDATE THE HYDROMETEOR CONCENTRATION VALUES AT EACH LEVEL
!----------------------------------------------------------
            DO 620 NK=1,LTOP
                QLPA(NK) = QLPA(NK) + (QLFXIN(NK) + DETLQ(NK) - QLFXOUT(NK))  * DTIME * EMSD(NK)
                QIPA(NK) = QIPA(NK) + (QIFXIN(NK) + DETIC(NK) - QIFXOUT(NK))  * DTIME * EMSD(NK)
!------------------------------------------------------------------------------------
! REVERSE THE MOD THAT ALLOWS FEEDBACK OF RAIN/SNOW THAT ORIGINATES IN DETRAINED AIR.
!------------------------------------------------------------------------------------
                QRPA(NK) = QRPA(NK) + (QRFXIN(NK) - QRFXOUT(NK) + RAINFB(NK)) * DTIME * EMSD(NK)
!------------------------------------------------------------------------------------
! REVERSE THE MOD THAT ALLOWS FEEDBACK OF RAIN/SNOW THAT ORIGINATES IN DETRAINED AIR.
!------------------------------------------------------------------------------------
                QSPA(NK) = QSPA(NK) + (QSFXIN(NK) - QSFXOUT(NK) + SNOWFB(NK)) * DTIME * EMSD(NK)
!
620         CONTINUE
!
625     CONTINUE
!
        DO NK=1,LTOP
            QLG(NK) = QLPA(NK)
            QIG(NK) = QIPA(NK)
            QRG(NK) = QRPA(NK)
            QSG(NK) = QSPA(NK)
        END DO
!
        CNDTNF = (1. - EQFRC(LFS)) * (QLIQ(LFS) + QICE(LFS)) * DMF(LFS)
!-------------------------
! EVALUATE MOISTURE BUDGET
!-------------------------
        QINIT = 0.
        QFNL  = 0.
        DPT   = 0.
!
        DO NK=1,LTOP
            DPT   = DPT   +   DP(NK)
            QINIT = QINIT +   Q0(NK) * EMS(NK)
            QFNL  = QFNL  +   QG(NK) * EMS(NK)
            QFNL  = QFNL  + (QLG(NK) + QIG(NK) + QRG(NK) + QSG(NK)) * EMS(NK)
        END DO
!
        QFNL = QFNL + PPTFLX * TIMEC * (1. - FBFRC)
!
        ERR2 = (QFNL - QINIT) * 100. / QINIT
!
        IF (ABS(ERR2) > 0.05 .AND. ISTOP == 0) THEN
            WRITE(99,*)'BLUE SCREEN OF DEATH: MOISTURE BUDGET ERROR IN KFPARA'
!
            WRITE(99,666) QINIT, QFNL, ERR2
666         FORMAT(' QINIT=',E12.5,' QFNL=',E12.5,' ERR2=',E12.5)
        END IF
!
        IF (PPTFLX > 0.) THEN
            RELERR = ERR2 * QINIT / (PPTFLX * TIMEC)
        ELSE
            RELERR = 0.
        END IF
!----------------------------------------------------------------------
! FEEDBACK TO RESOLVABLE SCALE TENDENCIES.
!
! IF THE ADVECTIVE TIME PERIOD (TADVEC) IS LESS THAN SPECIFIED MINIMUM
! TIMEC, ALLOW FEEDBACK TO OCCUR ONLY DURING TADVEC.
!---------------------------------------------------------------------
        IF (TADVEC < TIMEC) NIC = NINT(TADVEC / (0.5 * DT2))
!
        NCA(I,J) = NIC     
!
        IF (ISHALL == 1) THEN
            TIMEC    = 2400.
            NCA(I,J) = NCLDCK
            NSHALL   = NSHALL + 1
        END IF
!
        DO 675 K=1,KX-NLEV
            NK = KX - K + 1 - NLEV
!--------------------------------------------------------------------------------------------------
! EVAPORATE OR SUBLIMATE ALL HYDROMETEORS SO THAT THE FEEDBACKS ARE TEMPERATURE AND WATER VAPOR 
! TENDENCIES. THIS MAY CREATE SUPERSATURATED VALUES OF TG, BUT THESE WILL BE REMOVED BY NORMAL 
! SUPERSATURATION-REMOVAL MECHANISMS.
!--------------------------------------------------------------------------------------------------
            QLG(K) = QLG(K) + QIG(K) + QRG(K) + QSG(K)
!
            IF (ISHALL == 1) THEN
                RL     = XLV0  - XLV1 *  TG(K)
                 QG(K) = QG(K) + QLG(K)
                 TG(K) = TG(K) - RL   * QLG(K) / CP
                QLG(K) = 0.
!------------------------------
! CORRECTION FROM REGIONAL ETA 
!------------------------------ 
            ELSE
!
                RL     =   XLV0  -  XLV1   * TG(K)
                 QG(K) =   QG(K) + (QLG(K) * FBFRC) * FBFQV
                 TG(K) =   TG(K) - RL * ((QLG(K) * FBFRC) * 0.4) / CP
                QLG(K) = (QLG(K) * (1 - FBFRC))  * (1 - FBFQV)
!
            END IF
!
             DTDT(I, J, NK) = ( TG(K) -  T0(K)) / TIMEC
             DQDT(I, J, NK) = ( QG(K) -  Q0(K)) / TIMEC
            DQCDT(I, J, NK) = (QLG(K) - QL0(K)) / TIMEC
!
            IF (ISHALL == 0) THEN
                NCAD(I,J) = NCA(I,J)
                PSRC(I,J) = PMIX0  / 100.
                PCLB(I,J) = PLCL0  / 100.
                EMST      = DPTHMX * DXSQ   / G
                UMFB(I,J) = 100.   * VMFLCL * TIMEC * AINC / EMST
                 CIN(I,J) = CIN1
            END IF
!
675     CONTINUE
!---------------------------------------- 
! SAVE CLOUD TOP AND BOTTOM FOR RADIATION 
!----------------------------------------
               LTOP = KL - LTOP + 1
               LBOT = KL - LCL  + 1
!
          HTOP(I,J) = MIN(FLOAT(LTOP),   HTOP(I,J))
          HBOT(I,J) = MAX(FLOAT(LBOT),   HBOT(I,J))
        CNVTOP(I,J) = MIN(FLOAT(LTOP), CNVTOP(I,J))
        CNVBOT(I,J) = MAX(FLOAT(LBOT), CNVBOT(I,J))
!
        RAINCV(I,J) = .1 * .5 * DT2 * PPTFLX * (1. - FBFRC) / DXSQ
            RNC     = RAINCV(I,J) * NIC
          NCCNT     = NCCNT + 1
900 CONTINUE
!--------------------------------
! END OF MARCH THROUGH THE POINTS
!--------------------------------
    RETURN
!
    END SUBROUTINE KFPARA
!
!
!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief THE CENTRAL SUBROUTINE IN THE K-F PARAMETERIZATION
!> @details THE CENTRAL SUBROUTINE IN THE K-F PARAMETERIZATION.
!! SHALLOW CONVECTION IS INCLUDED WITHOUT CAPE DEPENDENCE.
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
!> @param[in] P     - Significado de P
!> @param[in] THES  - Significado de THES
!> @param[out] TU   - Significado de TU
!> @param[out] QU   - Significado de QU
!> @details <b>Use Module:</b>
!! @arg @c F77KINDS
!! @arg @c KFLUT
!> @details <b>Driver:</b> 
!! @arg @c KFDRIVE
!> @details <b>Calls:</b>
!--------------------------------------------------------------------------------------------------
      SUBROUTINE TP_CAPE(P, THES, TU, QU)
!--------------------------------------------------------------------------------------------------
! SUBPROGRAM TP_CAPE
! 
! SUBPROGRAM: TP_CAPE - THE CENTRAL SUBROUTINE IN THE K-F PARAMETERIZATION
! PROGRAMMER: ?????          
! ORG: W/NP22
! DATE: 00-04-13
!
! ABSTRACT:
! KFPARA IS THE CENTRAL SUBROUTINE IN THE K-F PARAMETERIZATION.
! SHALLOW CONVECTION IS INCLUDED WITHOUT CAPE DEPENDENCE.
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
! P    - 
! THES - 
!
! OUTPUT ARGUMENT LIST:
! TU   - 
! QU   -
!
! INPUT/OUTPUT ARGUMENT LIST:
! NONE
!
! USE MODULES: F77KINDS
!              KFLUT 
! 
! DRIVER     : KFPARA
!
! CALLS      : -----
!--------------------------------------------------------------------------------------------------
      USE F77KINDS
      USE KFLUT
!---------------------------------- 
! SCALING PRESSURE & TT TABLE INDEX
!---------------------------------- 
      TP   = (P - PTOP) * RDPR
      QQ   = TP - AINT(TP)
      IPTB =       INT(TP) + 1
!-------------------------------- 
! BASE AND SCALING FACTOR FOR THE
!--------------------------------
!
!----------------------------- 
! SCALING THE & TT TABLE INDEX
!----------------------------- 
    BTH = (THE0K(IPTB+1) - THE0K(IPTB)) * QQ + THE0K(IPTB)
    TTH = (THES - BTH) * RDTHK
    PP  = TTH - AINT(TTH)
    ITHTB =      INT(TTH) + 1
!
    T00 = TTAB(ITHTB  , IPTB  )
    T10 = TTAB(ITHTB+1, IPTB  )
    T01 = TTAB(ITHTB  , IPTB+1)
    T11 = TTAB(ITHTB+1, IPTB+1)
!
    Q00 = QSTAB(ITHTB  , IPTB  )
    Q10 = QSTAB(ITHTB+1, IPTB  )
    Q01 = QSTAB(ITHTB  , IPTB+1)
    Q11 = QSTAB(ITHTB+1, IPTB+1)
!-------------------
! PARCEL TEMPERATURE
!-------------------
    TU = (T00 + (T10 - T00) * PP + (T01 - T00) * QQ + (T00 - T10 - T01 + T11) * PP * QQ)
    QU = (Q00 + (Q10 - Q00) * PP + (Q01 - Q00) * QQ + (Q00 - Q10 - Q01 + Q11) * PP * QQ)
!
    RETURN
!      
    END SUBROUTINE TP_CAPE


