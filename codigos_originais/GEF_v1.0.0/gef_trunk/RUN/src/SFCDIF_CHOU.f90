!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief SURFACE LAYER
!> @details AMMENDED TO USE THE "EFFECTIVE ROUGHNESS" OF MASON (1986, SEE GEORGELIN ET AL., MWR JULY 1994)
!> @author ORIGINATOR - BLACK
!> @date 96-03-28 \n
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
!> @param[in]    LMHK  - Significado de LMHK
!> @param[in]    SM    - Significado de SM
!> @param[in]    THS   - Significado de THS
!> @param[in]    QS    - Significado de QS
!> @param[in]    ZEFF  - Significado de ZEFF
!> @param[in]    HPBL  - Significado de HPBL
!> @param[in]    VLM   - Significado de VLM
!> @param[in]    T     - Significado de T
!> @param[in]    Q     - Significado de Q
!> @param[in]    Z     - Significado de Z
!> @param[in]    PD    - Significado de PD
!> @param[in]    PT    - Significado de PT
!> @param[in]    TLM   - Significado de TLM
!> @param[in]    APE   - Significado de APE
!> @param[out]   CT    - Significado de CT
!> @param[out]   U10   - Significado de U10
!> @param[out]   V10   - Significado de V10
!> @param[out]   TH02  - Significado de TH02
!> @param[out]   TH10  - Significado de TH10
!> @param[out]   Q02   - Significado de Q02
!> @param[out]   Q10   - Significado de Q10
!> @param[inout] UZ0   - Significado de UZ0
!> @param[inout] VZ0   - Significado de VZ0
!> @param[inout] THZ0  - Significado de THZ0
!> @param[inout] QZ0   - Significado de QZ0
!> @param[inout] USTAR - Significado de USTAR
!> @param[inout] Z0    - Significado de Z0
!> @param[inout] AKMS  - Significado de AKMS
!> @param[inout] AKHS  - Significado de AKHS
!> @param[inout] ULM   - Significado de ULM
!> @details <b>Use Module:</b>
!! @arg @c CTLBLK
!! @arg @c F77KINDS
!! @arg @c MPPSTAFF
!! @arg @c PARMETA
!> @details <b>Driver:</b> 
!! @arg @c TURBL
!> @details <b>Calls:</b>
!-------------------------------------------------------------------------------------------------- 
    SUBROUTINE SFCDIF(LMHK , SM   , THS  , QS   , UZ0  , VZ0  , THZ0 , QZ0  , USTAR, Z0   , ZEFF ,&
!    &                 AKMS , AKHS , HPBL , CT   , U10  , V10  , TH02 , TH10 , Q02  , Q10  , ULM  ,&
! retorna temperautura absoluta a 2 m ao invés da temperatura potencial a 2 m
    &                 AKMS , AKHS , HPBL , CT   , U10  , V10  , T02 , TH10 , Q02  , Q10  , ULM  ,&
    &                 VLM  , T    , Q    , APE  , Z    , PD   , PT   , TLM)
!--------------------------------------------------------------------------------------------------
! SUBROUTINE SFCDIF
!
! SUBROUTINE: SFCDIF - SURFACE LAYER
! PROGRAMMER: BLACK
! ORG: ?????
! DATE: 96-03-28
!
! ABSTRACT:
! AMMENDED TO USE THE "EFFECTIVE ROUGHNESS" OF MASON (1986, SEE GEORGELIN ET AL., MWR JULY 1994)
!
! PROGRAM HISTORY LOG:
! 96-03-28  BLACK      - ORIGINATOR
! 97-06-??  MEYS       - MODIFIED FOR DISTRIBUTED MEMORY
! 99-07-06  BLACK      - FULL ARRAY RATHER THAN JUST EDGES
! 18-01-15  LUCCI      - MODERNIZATION OF THE CODE, INCLUDING:
!                        * F77 TO F90/F95
!                        * INDENTATION & UNIFORMIZATION CODE
!                        * REPLACEMENT OF COMMONS BLOCK FOR MODULES
!                        * DOCUMENTATION WITH DOXYGEN
!                        * OPENMP FUNCTIONALITY
!
! INPUT ARGUMENT LIST:
! LMHK  - 
! SM    - 
! THS   - 
! QS    -
! ZEFF  -
! HPBL  - 
! VLM   -
! T     -
! Q     - 
! Z     -
! PD    - 
! PT    - 
! TLM   -
! APE   - 
!
! OUTPUT ARGUMENT LIST:
! CT    - 
! U10   - 
! V10   -
! TH02  -
! TH10  - 
! Q02   -
! Q10   - 
!
! INPUT/OUTPUT ARGUMENT LIST:
! UZ0   - 
! VZ0   - 
! THZ0  - 
! QZ0   -
! USTAR - 
! Z0    -
! AKMS  - 
! AKHS  - 
! ULM   - 
!
! OUTPUT FILES:
! NONE
!
! USE MODULES: CTLBLK
!              F77KINDS
!              MPPSTAFF
!              PARMETA
!     
! DRIVER     : TURBL
!
! CALLS      : -----
!---------------------------------------------------------------------------------------------------
!
!--------------------------------------------------------------------------------------------------
! SURFACE LAYER
!
! AMMENDED TO USE THE "EFFECTIVE ROUGHNESS" OF MASON (1986, SEE GEORGELIN ET AL., MWR JULY 1994), 
! BY FM, RW, JUNE 1995
!--------------------------------------------------------------------------------------------------
    USE CTLBLK
    USE F77KINDS
    USE MPPSTAFF
    USE PARMETA
!
    IMPLICIT NONE
!
    INTEGER(KIND=I4)    , PARAMETER :: ITRMX  = 05 
!
    REAL   (KIND=R4)    , PARAMETER :: WWST   = 1.2
    REAL   (KIND=R4)    , PARAMETER :: WWST2  = WWST * WWST
    REAL   (KIND=R4)    , PARAMETER :: G      = 9.80616
    REAL   (KIND=R4)    , PARAMETER :: USTFC  = 0.018 / G
    REAL   (KIND=R4)    , PARAMETER :: VKRM   = 0.40 
    REAL   (KIND=R4)    , PARAMETER :: RIC    = 0.183
    REAL   (KIND=R4)    , PARAMETER :: RFC    = 0.191
    REAL   (KIND=R4)    , PARAMETER :: FHNEU  = 0.8
    REAL   (KIND=R4)    , PARAMETER :: RRIC   = 1.0   / RIC
    REAL   (KIND=R4)    , PARAMETER :: RFAC   = RIC   / (FHNEU * RFC * RFC)
    REAL   (KIND=R4)    , PARAMETER :: EXCM   = 0.001 
    REAL   (KIND=R4)    , PARAMETER :: BETA   = 1.    / 270.
    REAL   (KIND=R4)    , PARAMETER :: BTG    = BETA  * G 
    REAL   (KIND=R4)    , PARAMETER :: ELFC   = VKRM  * BTG
    REAL   (KIND=R4)    , PARAMETER :: CNV    = 0.608 * G / BTG
    REAL   (KIND=R4)    , PARAMETER :: WOLD   =  .15
    REAL   (KIND=R4)    , PARAMETER :: WNEW   = 1.    - WOLD
    REAL   (KIND=R4)    , PARAMETER :: PIHF   =   3.14159265 / 2. 
    REAL   (KIND=R4)    , PARAMETER :: PIFR   =   3.14159265 / 4. 
    REAL   (KIND=R4)    , PARAMETER :: EPSU2  =   1.E-2
    REAL   (KIND=R4)    , PARAMETER :: EPSUST =   0.07 
    REAL   (KIND=R4)    , PARAMETER :: EPSIT  =   1.E-4
!    REAL   (KIND=R4)    , PARAMETER :: EPSA   =   1.E-6
! copiado do Eta 
    REAL   (KIND=R4)    , PARAMETER :: EPSA   =   1.E-8
    REAL   (KIND=R4)    , PARAMETER :: ZTMIN  =  -5.
    REAL   (KIND=R4)    , PARAMETER :: ZTMAX  =   1.
    REAL   (KIND=R4)    , PARAMETER :: SMALL  =   0.35
    REAL   (KIND=R4)    , PARAMETER :: GLKBS  =  30.0
    REAL   (KIND=R4)    , PARAMETER :: GLKBR  =  10.0
    REAL   (KIND=R4)    , PARAMETER :: GRRS   =  GLKBR / GLKBS
    REAL   (KIND=R4)    , PARAMETER :: CZIV   =  SMALL * GLKBS
    REAL   (KIND=R4)    , PARAMETER :: VISC   =   1.5E-5
    REAL   (KIND=R4)    , PARAMETER :: TVISC  =   2.1E-5
    REAL   (KIND=R4)    , PARAMETER :: QVISC  =   2.1E-5
    REAL   (KIND=R4)    , PARAMETER :: RVISC  =   1. / VISC 
    REAL   (KIND=R4)    , PARAMETER :: RTVISC =   1. / TVISC
    REAL   (KIND=R4)    , PARAMETER :: RQVISC =   1. / QVISC
    REAL   (KIND=R4)    , PARAMETER :: SQPR   =   0.84
    REAL   (KIND=R4)    , PARAMETER :: SQSC   =   0.84
    REAL   (KIND=R4)    , PARAMETER :: ZQRZT  =  SQSC / SQPR
    REAL   (KIND=R4)    , PARAMETER :: USTR   =   0.225  
    REAL   (KIND=R4)    , PARAMETER :: USTC   =   0.7
    REAL   (KIND=R4)    , PARAMETER :: FZU1   =  CZIV   * VISC 
    REAL   (KIND=R4)    , PARAMETER :: FZT1   =  RVISC  * TVISC * SQPR
    REAL   (KIND=R4)    , PARAMETER :: FZQ1   =  RTVISC * QVISC * ZQRZT
    REAL   (KIND=R4)    , PARAMETER :: FZT2   =  CZIV   * GRRS  * TVISC * SQPR
    REAL   (KIND=R4)    , PARAMETER :: FZQ2   =  RTVISC * QVISC * ZQRZT
    REAL   (KIND=R4)    , PARAMETER :: ZTFC   =   1.0
!    REAL   (KIND=R4)    , PARAMETER :: CZIL   =    .1000
! copiado do Eta 
    REAL   (KIND=R4)    , PARAMETER :: CZIL   =    .2000
    REAL   (KIND=R4)    , PARAMETER :: SQVISC = 258.2 
    REAL   (KIND=R4)    , PARAMETER :: ZILFC  = -CZIL * VKRM * SQVISC
    REAL   (KIND=R4)    , PARAMETER :: PQ0    = 379.90516
    REAL   (KIND=R4)    , PARAMETER :: A2     =  17.2693882
    REAL   (KIND=R4)    , PARAMETER :: A3     = 273.16
    REAL   (KIND=R4)    , PARAMETER :: A4     =  35.86
    REAL   (KIND=R4)    , PARAMETER :: CAPA   =   0.28589641E0
    REAL   (KIND=R4)    , PARAMETER :: H1M5   =   1.E-5
!
    REAL   (KIND=R4)    , DIMENSION(LM)                                   , INTENT(IN)          ::&
    & T       , Q       , APE
!
    REAL   (KIND=R4)    , DIMENSION(LP1)                                  , INTENT(IN)          ::&
    & Z
!
    REAL   (KIND=R4)    , DIMENSION(4)                                    , INTENT(INOUT)       ::&
    & ZEFF                            
!------------------------
! IMPLICIT NONE VARIABLES
!------------------------
    INTEGER(KIND=I4)                                                      , INTENT(IN)          ::&
    & LMHK
!
    REAL   (KIND=R4)                                                      , INTENT(IN)          ::&
    & SM      , THS     , QS      , HPBL    , VLM     , PD      , PT      , TLM 
!
    REAL   (KIND=R4)                                                      , INTENT(OUT)         ::&
!    & CT      , U10     , V10     , TH02    , TH10    , Q02     , Q10
! Copiado do Eta 
    & CT      , U10     , V10     , T02    , TH10    , Q02     , Q10
    REAL   (KIND=R4)    TH02
!
    REAL   (KIND=R4)                                                      , INTENT(INOUT)       ::&
    & UZ0     , VZ0     , THZ0    , QZ0     , USTAR   , Z0      , AKMS    , AKHS    , ULM      
!
    INTEGER(KIND=I4)                                                                            ::&
    & LMHP    , ML      , MH      , ITR
!
    REAL   (KIND=R4)                                                                            ::&
    & PSLMU   , ZZ      , PSLMS   , PSLHU   , PSPMU   , XX      , PSPMS   , YY      , PSPHU   ,   &
    & PSPHS   , THLM    , QLM     , RR      , FZUI1   , PSLHS   ,                                 &
    & ZU      , WGHT    , RWGH    , ZT      , ZQ      , ALPHA   , X       , WLOW    ,             &
    & ZSL     , RDZ     , CXCH    , DTHV    , DU2     , RIB     , BTBH    , WSTAR2  , ZSLU    ,   &
    & ZSLT    , RLOGU   , RLOGT   , BTGH    , RLMO    , ZETALT  , ZETALU  , ZETAU   , ZETAT   ,   &
    & PSMZ    , SIMM    , PSHZ    , SIMH    , USTARK  , RLMN    , RLMP    , RLMA    , XLU4    ,   &
    & XLT4    , XU4     , XT4     , XLU     , XLT     , XU      , XT      , HSFLX   , HLFLX   ,   &
    & AKHS02  , ZU10    , ZT02    , ZT10    , RLNU10  ,                                           &
    & RLNT02  , RLNT10  , ZTAU10  , ZTAT02  , SIMM10  ,                                           &
    & XLU104  , AKHS10  , ZTAT10  , SIMH02  , SIMH10  , XLT024  ,                                 &
    & XLT104  , XLU10   , XLT02   , XLT10   , PDS     , TERM1   ,                                 &
!    & PSHLTR  , T02     , QSAT2   , T10     , QSAT10  , P100    , U10E    ,                       &
! Copiado do Eta
    & PSHLTR  , QSAT2   , T10     , QSAT10  , P100    , U10E    ,                       &
    & V10E    , U100E   , V100E   , ZUUZ    , ZTAU    , XU104   , XU10    , EKMS10  ,             &
    & WSTAR   , HV      , FCT     , UMFLX   , VMFLX   , AKMS10
!
    LMHP = LMHK + 1
!
    THLM = T(LMHK) * APE(LMHK)
     QLM = Q(LMHK)
!
    Z0   = (1. - SM) * Z0 + SM * AMAX1(USTFC * USTAR * USTAR, 1.59E-5)
!-----------------
! VISCOUS SUBLAYER
!-----------------
    IF (SM > 0.5 .AND. USTAR < USTC) THEN
!
        IF (USTAR < USTR) THEN
            ZU   = FZU1 * SQRT(SQRT(Z0 * USTAR * RVISC)) / USTAR
!
            WGHT = AKMS * ZU    * RVISC
            RWGH = WGHT / (WGHT + 1.)
!
            UZ0  = (ULM * RWGH + UZ0) * 0.5
            VZ0  = (VLM * RWGH + VZ0) * 0.5
!
            ZT   = FZT1 * ZU
            WGHT = AKHS * ZT * RTVISC
!
            THZ0 = ((WGHT * THLM + THS) / (WGHT + 1.) + THZ0) * 0.5
!
            ZQ   = FZQ1 * ZT
            WGHT = AKHS * ZQ * RQVISC
!
            QZ0  = ((WGHT * QLM + QS)   / (WGHT + 1.) + QZ0)  * 0.5
        END IF
!
        IF (USTAR >= USTR .AND. USTAR < USTC) THEN
            ZU  = Z0
            UZ0 = 0.
            VZ0 = 0.
!
            ZT   = FZT2 * SQRT(SQRT(Z0 * USTAR * RVISC)) / USTAR
            WGHT = AKHS * ZT * RTVISC
!
            THZ0 = ((WGHT * THLM + THS) / (WGHT + 1.) + THZ0) * 0.5
!
            ZQ   = FZQ2 * ZT
            WGHT = AKHS * ZQ * RQVISC
!
            QZ0  = ((WGHT * QLM + QS) / (WGHT + 1.) + QZ0) * 0.5
        END IF
!
    ELSE
!
        ZU = Z0
!
	IF (SM <= 0.5) THEN
!
            IF (ULM < EPSU2) ULM = EPSU2
!
!            ALPHA = ABS(ATAN(VLM/ULM) + PIHF - EPSA)
!            X     = ABS(ATAN(VLM/ULM) + PIHF - EPSA) / PIFR
!            ML    = 1 + (ABS(ATAN(VLM/ULM) + PIHF - EPSA) / PIFR)
!            MH    = 1 + MOD(ML,4)
!
!            WLOW  = (ABS(ATAN(VLM/ULM) + PIHF - EPSA) / PIFR) - (1 + (ABS(ATAN(VLM/ULM) + PIHF - EPSA) / PIFR)) + 1
!            ZU    = WLOW * ZEFF(ML) + (1. - WLOW) * ZEFF(MH)
! copiado do Eta
            ALPHA = ABS(ATAN(VLM / ULM) + PIHF - EPSA)
            X     = ALPHA / PIFR
            ML    = 1 + X
            ML    = MIN(4,ML)
            MH    = 1 + MOD(ML,4)
            WLOW  = X - ML + 1
            ZU    = WLOW * ZEFF(ML) + (1. - WLOW) * ZEFF(MH)
        END IF
!
        UZ0  = 0.
        VZ0  = 0.
!
        ZT   = Z0
        THZ0 = THS
!
        ZQ   = Z0
!----------------------------
! Adicionado na versao Dragan
!----------------------------
        QZ0  = QS
!
    END IF
!
    ZSL = (Z(LMHK) - Z(LMHP)) * 0.5
!
    ZU  = AMIN1(ZU, 0.5 * ZSL)
!
    RDZ  =  1.  / ZSL
    CXCH = EXCM * RDZ
!
    IF (SM > 0.5) THEN
        DTHV = (0.608 * QLM + 1.) * THLM - (0.608 * QZ0 + 1.) * THZ0
    ELSE
        DTHV = (QLM - QZ0) * CNV + THLM - THZ0
        ZT   =   Z0 * ZTFC
    END IF
!
    DU2 = AMAX1((ULM - UZ0) ** 2 + (VLM - VZ0) ** 2, EPSU2)
!
    RIB = BTG * DTHV * ZSL / DU2
!----------------------------
! BELJARS CORRECTION OF USTAR
!----------------------------
    BTGH   = BTG   * HPBL
    WSTAR2 = WWST2 * ABS(BTGH * AKHS * DTHV) ** (2. / 3.)
    USTAR  = AMAX1(SQRT(AKMS * SQRT(DU2 + WSTAR2)), EPSUST)
!--------------------------
! ZILITINKEVITCH FIX FOR ZT 
!--------------------------
    IF (SM < 0.5) ZT = EXP(ZILFC * SQRT(USTAR * Z0)) * Z0
!
    IF (SM > 0.5 .AND. RIB >= RIC) THEN
        AKMS = AMAX1( VISC * RDZ, CXCH)
        AKHS = AMAX1(TVISC * RDZ, CXCH)
    ELSE
!
        ZSLU = ZSL + ZU
        ZSLT = ZSL + ZT
!
        RLOGU = ALOG(ZSLU / ZU)
        RLOGT = ALOG(ZSLT / ZT)
!
        RLMO  = ELFC * AKHS * DTHV / USTAR ** 3
!-----------------
! SEA POINTS FIRST
!-----------------
        IF (SM > 0.5) THEN
            DO ITR=1,ITRMX
!------------------------------- 
! 1./MONIN-OBUKKHOV LENGTH-SCALE
!------------------------------- 
                ZETALT = AMAX1(ZSLT * RLMO, ZTMIN)
                RLMO   = ZETALT / ZSLT
                ZETALU = ZSLU * RLMO
                 ZETAU = ZU   * RLMO
                 ZETAT = ZT   * RLMO
!---------------------- 
! LL FUNCTIONS OVER SEA
!----------------------
                IF (RLMO < 0.) THEN
                    PSMZ =          PSLMU(ZETAU)
                    SIMM =          PSLMU(ZETALU) - PSMZ + RLOGU
                    PSHZ =          PSLHU(ZETAT)
                    SIMH = FHNEU * (PSLHU(ZETALT) - PSHZ + RLOGT)
                ELSE
                    PSMZ =          PSLMS(ZETAU)
                    SIMM =          PSLMS(ZETALU) - PSMZ + RLOGU
                    PSHZ =          PSLHS(ZETAT)
                    SIMH = FHNEU * (PSLHS(ZETALT) - PSHZ + RLOGT)
                END IF
!------------------------------
! BELJAARS CORRECTION FOR USTAR
!------------------------------
                USTAR = AMAX1(SQRT(AKMS * SQRT(DU2 + WSTAR2)), EPSUST)
!
                USTARK = USTAR * VKRM
                AKMS   = AMAX1(USTARK / SIMM, CXCH)
                AKHS   = AMAX1(USTARK / SIMH, CXCH)
!
                WSTAR2 = WWST2 * ABS(BTGH * AKHS * DTHV) ** (2. / 3.)
                RLMN   = ELFC  * AKHS * DTHV / USTAR ** 3
!
                RLMP = RLMO
                RLMA = RLMO * WOLD + RLMN * WNEW
                RLMO = RLMA
!
            END DO
!----------------------------  
! END OF SEA POINT PROCESSING
!----------------------------
        ELSE
!----------------
! NOW LAND POINTS
!----------------
            DO ITR=1,ITRMX
!-------------------------------
! 1./MONIN-OBUKKHOV LENGTH-SCALE
!-------------------------------
                ZETALT = AMAX1(ZSLT * RLMO, ZTMIN)
                RLMO   = ZETALT / ZSLT
                ZETALU = ZSLU   * RLMO

                ZETAU = ZU * RLMO
                ZETAT = ZT * RLMO
!-----------------------------------------------
! PAULSON 1970 FUNCTIONS OVER LAND W RAD. SKIN T
!-----------------------------------------------
                IF (RLMO < 0.) THEN
                    XLU4 = 1.-16. * ZETALU
                    XLT4 = 1.-16. * ZETALT
                    XU4  = 1.-16. * ZETAU
                    XT4  = 1.-16. * ZETAT
!
                    XLU = SQRT(SQRT(XLU4))
                    XLT = SQRT(SQRT(XLT4))
                    XU  = SQRT(SQRT(XU4 ))
                    XT  = SQRT(SQRT(XT4 ))
!
                    PSMZ = PSPMU(XU)
                    SIMM = PSPMU(XLU) - PSMZ + RLOGU
!
                    PSHZ = PSPHU(XT)
                    SIMH = PSPHU(XLT) - PSHZ + RLOGT
                ELSE
                     ZETAU = AMIN1(ZETAU , ZTMAX)
                     ZETAT = AMIN1(ZETAT , ZTMAX)
                    ZETALU = AMIN1(ZETALU, ZTMAX)
                    ZETALT = AMIN1(ZETALT, ZTMAX)
!
                    PSMZ = PSPMS(ZETAU)
                    SIMM = PSPMS(ZETALU) - PSMZ + RLOGU
                    PSHZ = PSPHS(ZETAT)
                    SIMH = PSPHS(ZETALT) - PSHZ + RLOGT
                END IF
!------------------------------
! BELJAARS CORRECTION FOR USTAR 
!------------------------------
                USTAR = AMAX1(SQRT(AKMS * SQRT(DU2 + WSTAR2)), EPSUST)
!--------------------------
! ZILITINKEVITCH FIX FOR ZT
!--------------------------
                ZT    = EXP(ZILFC * SQRT(USTAR * Z0)) * Z0
                ZSLT  = ZSL + ZT
                RLOGT = ALOG(ZSLT / ZT)
!
                USTARK = USTAR * VKRM
                AKMS   = AMAX1(USTARK / SIMM, CXCH)
                AKHS   = AMAX1(USTARK / SIMH, CXCH)
!
                WSTAR2 = WWST2 * ABS(BTGH * AKHS * DTHV) ** (2. / 3.)
                RLMN   = ELFC  * AKHS * DTHV / USTAR ** 3
!
                RLMP = RLMO
                RLMA = RLMO * WOLD + RLMN * WNEW
                RLMO = RLMA
!
            END DO 
!---------------------------------------------------- 
! END OF LAND POINT PROCESSING AND SEA-LAND BRANCHING
!---------------------------------------------------- 
        END IF
!------------------------------------------
! END OF TURBULENCE-NO TURBULENCE BRANCHING 
!------------------------------------------
    END IF
!
    CT = 0. 
!-----------------
! DIAGNOSTIC BLOCK
!----------------- 
    WSTAR = SQRT(WSTAR2) / WWST
!
    UMFLX = AKMS * (ULM  - UZ0 )
    VMFLX = AKMS * (VLM  - VZ0 )
    HSFLX = AKHS * (THLM - THZ0)
    HLFLX = AKHS * (QLM  - QZ0 )
!
    IF (SM > 0.5 .AND. RIB >= RIC) THEN
!        AKMS10 = AMAX1( VISC / 10., CXCH)
!        AKHS02 = AMAX1(TVISC / 02., CXCH)
!        AKHS10 = AMAX1(TVISC / 10., CXCH)
! Copiado do Eta
        AKMS10 = AMAX1( VISC / 10., 4. * CXCH)
        AKHS02 = AMAX1(TVISC / 02., 4. * CXCH)
        AKHS10 = AMAX1(TVISC / 10., 4. * CXCH)
!
    ELSE
!
        ZU10 = ZU + 10.
        ZT02 = ZT + 02.
        ZT10 = ZT + 10.
!
        RLNU10 = ALOG(ZU10 / ZU)
        RLNT02 = ALOG(ZT02 / ZT)
        RLNT10 = ALOG(ZT10 / ZT)
!
        ZTAU10 = ZU10 * RLMP
        ZTAT02 = ZT02 * RLMP
        ZTAT10 = ZT10 * RLMP
!----------------------
! LL FUNCTIONS OVER SEA
!----------------------
        IF (SM > 0.5) THEN
!
            IF (RLMP < 0.) THEN
                SIMM10 =          PSLMU(ZTAU10) - PSMZ + RLNU10
                SIMH02 = FHNEU * (PSLHU(ZTAT02) - PSHZ + RLNT02)
                SIMH10 = FHNEU * (PSLHU(ZTAT10) - PSHZ + RLNT10)
            ELSE
                SIMM10 =          PSLMS(ZTAU10) - PSMZ + RLNU10
                SIMH02 = FHNEU * (PSLHS(ZTAT02) - PSHZ + RLNT02)
                SIMH10 = FHNEU * (PSLHS(ZTAT10) - PSHZ + RLNT10)
            END IF
!-----------------------------------------------
! PAULSON 1970 FUNCTIONS OVER LAND W RAD. SKIN T
!-----------------------------------------------
        ELSE
!
            IF (RLMP < 0.) THEN
                XLU104 = 1.-16. * ZTAU10
                XLT024 = 1.-16. * ZTAT02
                XLT104 = 1.-16. * ZTAT10
!
                XLU10  = SQRT(SQRT(XLU104))
                XLT02  = SQRT(SQRT(XLT024))
                XLT10  = SQRT(SQRT(XLT104))
! 
                SIMM10 = PSPMU(XLU10) - PSMZ + RLNU10
                SIMH02 = PSPHU(XLT02) - PSHZ + RLNT02
                SIMH10 = PSPHU(XLT10) - PSHZ + RLNT10
!
            ELSE
!
                ZTAU10 = AMIN1(ZTAU10, ZTMAX)
                ZTAT02 = AMIN1(ZTAT02, ZTMAX)
                ZTAT10 = AMIN1(ZTAT10, ZTMAX)
!
                SIMM10 = PSPMS(ZTAU10) - PSMZ + RLNU10
                SIMH02 = PSPHS(ZTAT02) - PSHZ + RLNT02
                SIMH10 = PSPHS(ZTAT10) - PSHZ + RLNT10
            END IF
!
        END IF
!
        AKMS10 = AMAX1(USTARK / SIMM10, CXCH)
        AKHS02 = AMAX1(USTARK / SIMH02, CXCH)
        AKHS10 = AMAX1(USTARK / SIMH10, CXCH)
!
    END IF
!
    U10  = UMFLX / AKMS10 + UZ0
    V10  = VMFLX / AKMS10 + VZ0
    TH02 = HSFLX / AKHS02 + THZ0
    TH10 = HSFLX / AKHS10 + THZ0
!--------------------------------------------------------------------------------------------------
! CHANGED THIS SECTION IN RESPONSE TO PROBLEM WITH 2-M DEW POINT OCCASIONALLY BEING GREATER THAN 
! 2-M TEMPERATURE AND SIMILAR PROBLEM AT 10-M. NOW, A SATURATION Q IS CALCULATED AT EACH LEVEL, AND
! THE Q IS CONSTRAINED TO BE NO HIGHER THAN THE SATURATION VALUE. 
!--------------------------------------------------------------------------------------------------
    PDS    = PD + PT
    TERM1  = -0.068283 / TLM
    PSHLTR = PDS   * EXP(TERM1)
    T02    = TH02  * (PSHLTR * H1M5) ** CAPA
    QSAT2  = PQ0   / PSHLTR * EXP(A2 * (T02 - A3) / (T02 - A4))
    Q02    = HLFLX / AKHS02 + QZ0
!
    IF (Q02 < 0.) THEN
!
        IF (QLM > 0.) THEN
            Q02 = QLM
        ELSE
            Q02 = 0.0001
        END IF
!
    END IF
!
    IF (Q02 > QSAT2) THEN
        Q02 = QSAT2
    END IF
!
    T10    = TH10  * (PSHLTR * H1M5) ** CAPA
    QSAT10 = PQ0   / PSHLTR  * EXP(A2 * (T10 - A3) / (T10 - A4))
    Q10    = HLFLX / AKHS10  + QZ0
!
    IF (Q10 < 0.) THEN
        IF (QLM > 0.) THEN
            Q10 = QLM
        ELSE
            Q10 = 0.0001
        END IF
    END IF
!
    IF (Q10 > QSAT10) THEN
        Q10 = QSAT10
    END IF
!------------------------------
! NEW CALCULATION OF 10-M WINDS
!-------- ----------------------
    U10E = U10
    V10E = V10
!
    IF (SM < 0.5) THEN
!-------------------------------
! CHOOSE THE EQUIVALENT Z0 HERE:
!-------------------------------
        ZU     = ZU *  0.1
        ZU10   = ZU + 10.
!
        RLNU10 = ALOG(ZU10 / ZU)
!
        ZTAU   = ZU   * RLMP
        ZTAU10 = ZU10 * RLMP
!
        IF (RLMP < 0) THEN
            XLU104 = 1.-16. * ZTAU10
            XU104  = 1.-16. * ZTAU
            XLU10  = SQRT(SQRT(XLU104))
            XU10   = SQRT(SQRT(XU104))
            SIMM10 = PSPMU(XLU10)  - PSPMU(XU10) + RLNU10
        ELSE
            ZTAU10 = AMIN1(ZTAU10, ZTMAX)
            SIMM10 = PSPMS(ZTAU10) - PSPMS(ZTAU) + RLNU10
        END IF
!
        EKMS10 = AMAX1(USTARK / SIMM10, CXCH)
        U10E   = UMFLX / EKMS10 + UZ0
        V10E   = VMFLX / EKMS10 + VZ0
!
    END IF
!
    U10 = U10E
    V10 = V10E

!
    RETURN
!
    END SUBROUTINE SFCDIF
!
!
!
    REAL FUNCTION PSLMU(ZZ)
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    REAL   (KIND=R4)                                                                            ::&
    & ZZ
!
    PSLMU = -0.96 * ALOG(1.0 - 4.5 * ZZ)
!
    END FUNCTION PSLMU
!
!
!
    REAL FUNCTION PSLMS(ZZ)
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    REAL   (KIND=R4)                                                                            ::&
    & ZZ
!
    REAL   (KIND=R4)    , PARAMETER :: RIC  = 0.183
    REAL   (KIND=R4)    , PARAMETER :: RRIC = 1.0   / RIC
!
    PSLMS = ZZ * RRIC - 2.076 * (1. - 1. / (ZZ + 1.))
!
    END FUNCTION PSLMS
!
!
!
    REAL FUNCTION PSLHU(ZZ)
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    REAL   (KIND=R4)                                                                            ::&
    & ZZ
!
    PSLHU = -0.96 * ALOG(1.0 - 4.5 * ZZ)
!
    END FUNCTION PSLHU
!
!
!
    REAL FUNCTION PSLHS(ZZ)
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    REAL   (KIND=R4)                                                                            ::&
    & ZZ
!
    REAL   (KIND=R4)    , PARAMETER :: RIC   = 0.183
    REAL   (KIND=R4)    , PARAMETER :: FHNEU = 0.8
    REAL   (KIND=R4)    , PARAMETER :: RFC   = 0.191
    REAL   (KIND=R4)    , PARAMETER :: RFAC  = RIC   / (FHNEU * RFC * RFC)
!
    PSLHS = ZZ * RFAC - 2.076 * (1. - 1. / (ZZ + 1.))
!
    END FUNCTION PSLHS
!
!
!
    REAL FUNCTION PSPMU(XX)
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    REAL   (KIND=R4)                                                                            ::&
    & XX
!
    REAL   (KIND=R4)    , PARAMETER :: PIHF = 3.14159265 / 2.
!
    PSPMU = -2. * ALOG((XX + 1.) * 0.5) - ALOG((XX * XX + 1.) * 0.5) + 2. * ATAN(XX) - PIHF
!
    END FUNCTION PSPMU
!
!
!
    REAL FUNCTION PSPMS(YY)
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    REAL   (KIND=R4)                                                                            ::&
    & YY
!
    PSPMS = 5. * YY
!
    END FUNCTION PSPMS
!
!
!
    REAL FUNCTION PSPHU(XX)
!--------------------------------------------------------------------------------------------------
    USE F77KINDs
!
    IMPLICIT NONE
!
    REAL   (KIND=R4)                                                                            ::&
    & XX
!
    PSPHU = -2. * ALOG((XX * XX + 1.) * 0.5)
!
    END FUNCTION PSPHU
!
!
!
    REAL FUNCTION PSPHS(YY)
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    REAL   (KIND=R4)                                                                            ::&
    & YY
!
    PSPHS = 5. * YY
!
    END FUNCTION PSPHS
