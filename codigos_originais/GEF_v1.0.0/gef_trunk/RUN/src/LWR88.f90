!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief COMPUTES TEMPERATURE-CORRECTED CO2 TRANSMISSION FUNCTIONS
!> @details COMPUTES TEMPERATURE-CORRECTED CO2 TRANSMISSION FUNCTIONS AND ALSO COMPUTES THE
!! PRESSURE GRID AND LAYER OPTICAL PATHS.
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
!> @param[in] PRESS - Significado de PRESS
!> @param[in] TEMP  - Significado de TEMP
!> @param[in] RH2O  - Significado de RH2O
!> @param[in] QO3   - Significado de QO3
!> @param[in] CAMT  - Significado de CAMT
!> @param[in] NCLDS - Significado de NCLDS 
!> @param[in] KTOP  - Significado de KTOP
!> @param[in] KBTM  - Significado de KBTM
!> @param[inout] CLDFAC - Significado de CLDFAC 
!> @param[inout] HEATRA - Significado de HEATRA
!> @param[inout] GRNFLX - Significado de GRNFLX
!> @param[inout] TOPFLX - Significado de TOPFLX
!> @details <b>Use Module:</b>
!! @arg @c CO2DTA
!! @arg @c F77KINDS
!! @arg @c HCON
!! @arg @c MPPSTAFF
!! @arg @c PARMETA
!! @arg @c RDPARM
!! @arg @c RNDDTA
!> @details <b>Driver:</b> 
!! @arg @c RADFS
!> @details <b>Calls:</b>
!! @arg @c FST88
!--------------------------------------------------------------------------------------------------    
    SUBROUTINE LWR88(HEATRA  , GRNFLX  , TOPFLX  , PRESS   , TEMP    , RH2O    , QO3     ,        &
    &                CLDFAC  , CAMT    , NCLDS   , KTOP    , KBTM)
!--------------------------------------------------------------------------------------------------
! SUBROUTINE LWR88
! 
! SUBPROGRAM: LWR88 - COMPUTES TEMPERATURE-CORRECTED CO2 TRANSMISSION FUNCTIONS
! PROGRAMMER: ????? 
! ORG: W/NP22
! DATE: ??-??-??
!
! ABSTRACT:  
! SUBROUTINE LWR88 COMPUTES TEMPERATURE-CORRECTED CO2 TRANSMISSION FUNCTIONS AND ALSO COMPUTES THE
! PRESSURE GRID AND LAYER OPTICAL PATHS.
!
! PROGRAM HISTORY LOG:
! ??-??-??  ?????  - ORIGINATOR
! 18-03-20  LUCCI  - MODERNIZATION OF THE CODE, INCLUDING:
!                    * F77 TO F90/F95
!                    * INDENTATION & UNIFORMIZATION CODE
!                    * REPLACEMENT OF COMMONS BLOCK FOR MODULES
!                    * DOCUMENTATION WITH DOXYGEN
!                    * OPENMP FUNCTIONALITY 
!
!
! INPUT ARGUMENT LIST:
! PRESS - 
! TEMP  -
! RH2O  -
! QO3   -
! CAMT  -
! NCLDS - 
! KTOP  -
! KBTM  -  
!
! OUTPUT ARGUMENT LIST:
! NONE
!
! INPUT/OUTPUT ARGUMENT LIST:
! CLDFAC - 
! HEATRA -
! GRNFLX -
! TOPFLX -
!
! INPUT FILES:
! NONE
!
! OUTPUT FILES:
! NONE
!
! USE MODULES: CO2DTA
!              F77KINDS
!              HCON
!              MPPSTAFF
!              PARMETA
!              RDPARM
!              RNDDTA
!
! DRIVER     : RADFS
!
! CALLS      : FST88           
!--------------------------------------------------------------------------------------------------
    USE CO2DTA
    USE F77KINDS
    USE HCON
    USE MPPSTAFF
    USE PARMETA
    USE RDPARM
    USE RNDDTA
!
      IMPLICIT NONE
!
    REAL   (KIND=R4)    , DIMENSION(IM, LP1)                              , INTENT(IN)          ::&
    & PRESS   , TEMP
!
    REAL   (KIND=R4)    , DIMENSION(IM, LM)                               , INTENT(IN)          ::&
    & RH2O    , QO3
!
    REAL   (KIND=R4)    , DIMENSION(IM, LP1)                              , INTENT(IN)          ::&
    & CAMT
!
    REAL   (KIND=R4)    , DIMENSION(IM, LP1, LP1)                         , INTENT(INOUT)       ::&
    & CLDFAC
!
    REAL   (KIND=R4)    , DIMENSION(IM, LP1, LP1)                                               ::&
    & CO21
!
    INTEGER(KIND=I4)    , DIMENSION(IM)                                   , INTENT(IN)          ::&
    & NCLDS
!
    INTEGER(KIND=I4)    , DIMENSION(IM, LP1)                              , INTENT(IN)          ::&
    & KTOP    , KBTM
!
    REAL   (KIND=R4)    , DIMENSION(IM, LM)                               , INTENT(INOUT)       ::&
    & HEATRA
!
    REAL   (KIND=R4)    , DIMENSION(IM, LM)                                                     ::&
    & DELP2   , QH2O    , DELP    , CO2NBL  , VAR1    , VAR2    , VAR3    , VAR4    , VSUM4   ,   &
    & CO2MR   , CO2MD   , CO2M2D  , VV      
!  
    REAL   (KIND=R4)    , DIMENSION(IM)                                   , INTENT(INOUT)       ::&
    & GRNFLX  , TOPFLX
!
    REAL   (KIND=R4)    , DIMENSION(IM)                                                         ::&
    & EMX1    , EMX2    , VSUM1   , VSUM2   , A1      , A2 
!
    REAL   (KIND=R4)    , DIMENSION(IM, LP1)                                                    ::&
    & T       , P       , CO2SP1  , CO2SP2  , CNTVAL  , TOTO3   , TPHIO3  , TOTPHI  , TOTVO2  ,   &
    & CO2R    , DIFT    , CO2R1   , DCO2D1  , D2CD21  , D2CD22  , CO2R2   , DCO2D2  , TDAV    ,   &
    & TSTDAV  , VSUM3   , DCO2DT  , D2CDT2  , TEXPSL  , TLSQU   
!
    REAL   (KIND=R4)    , DIMENSION(IM, LLP1)                                                   ::&
    & EMPL

!------------------------
! IMPLICIT NONE VARIABLES
!------------------------
    INTEGER(KIND=I4)                                                                            ::&
    & I       , K       , KP 
!
    EQUIVALENCE (VSUM3, TLSQU, TEXPSL)
    EQUIVALENCE (VV   , VSUM4)
!----------------------------------------------------------------------------------- 
! COMPUTE FLUX PRESSURES (P) AND DIFFERENCES (DELP2,DELP)
! COMPUTE FLUX LEVEL TEMPERATURES (T) AND CONTINUUM TEMPERATURE CORRECTIONS (TEXPSL)
!-----------------------------------------------------------------------------------
    DO 103 K=2,LM
        DO 103 I=1,IM
            P(I,K) = HAF * (PRESS(I,K-1) + PRESS(I,K))
            T(I,K) = HAF * ( TEMP(I,K-1) +  TEMP(I,K))
103 END DO
!
    DO 105 I=1,IM
        P(I,1  ) = ZERO
        P(I,LP1) = PRESS(I,LP1)
        T(I,1  ) =  TEMP(I,1  )
        T(I,LP1) =  TEMP(I,LP1)
105 END DO
!
    DO 107 K=1,LM
        DO 107 I=1,IM
            DELP2(I,K) = P(I,K+1) - P(I,K)
!-------------
! TO AVOID FPE
!-------------
        IF (DELP2(I,K) ==  0) THEN
            PRINT*,'DELP2 = 0 !',DELP2(I,K), I, K, DELP2(I-1, K-1), DELP2(I+1, K+1)
            DELP2(I,K) = 0.2
        END IF
!
             DELP(I,K) = ONE / DELP2(I,K)
!
107 END DO
!----------------------------------------------------------------------- 
! COMPUTE ARGUMENT FOR CONT.TEMP.COEFF. (THIS IS 1800.(1./TEMP-1./296.))
!-----------------------------------------------------------------------
    DO K=1,LP1
        DO I=1,IM
            IF (TEMP(I,K) /= 0) THEN
                TEXPSL(I,K) = H18E3 / TEMP(I,K) - H6P08108
            END IF
!
            TEXPSL(I,K) = EXP(TEXPSL(I,K))
!
        END DO
    END DO 
!--------------------------------------------------------------------------------------------------
! COMPUTE OPTICAL PATHS FOR H2O AND O3, USING THE DIFFUSIVITY APPROXIMATION FOR THE ANGULAR 
! INTEGRATION (1.66).
! OBTAIN THE UNWEIGHTED VALUES(VAR1,VAR3) AND THE WEIGHTED VALUES(VAR2,VAR4).
! THE QUANTITIES H3M4(.0003) AND H3M3(.003) APPEARING IN THE VAR2 AND VAR4 EXPRESSIONS ARE THE 
! APPROXIMATE VOIGT CORRECTIONS FOR H2O AND O3,RESPECTIVELY.
!--------------------------------------------------------------------------------------------------
    DO 131 K=1,LM
        DO 131 I=1,IM
            QH2O(I,K) = RH2O(I,K) * DIFFCTR
!------------------------------------------------------------------------------------------- 
! VV IS THE LAYER-MEAN PRESSURE (IN ATM),WHICH IS NOT THE SAME AS THE LEVEL PRESSURE (PRESS)
!------------------------------------------------------------------------------------------- 
              VV(I,K) = HAF * (P(I,K+1) + P(I,K)) * P0INV
!
            VAR1(I,K) = DELP2(I,K) * QH2O(I,K) * GINV
            VAR3(I,K) = DELP2(I,K) *  QO3(I,K) * DIFFCTR * GINV
            VAR2(I,K) =  VAR1(I,K) *  (VV(I,K) + H3M4)
            VAR4(I,K) =  VAR3(I,K) *  (VV(I,K) + H3M3)
!--------------------------------------------------------------------------------------------------
! COMPUTE OPTICAL PATH FOR THE H2O CONTINUUM, USING ROBERTS COEFFS.
! (BETINW), AND TEMP. CORRECTION (TEXPSL). THE DIFFUSIVITY FACTOR (WHICH CANCELS OUT IN THIS
! EXPRESSION) IS ASSUMED TO BE 1.66. THE USE OF THE DIFFUSIVITY FACTOR HAS BEEN SHOWN TO BE A 
! SIGNIFICANT SOURCE OF ERROR IN THE CONTINUUM CALCS.,BUT THE TIME PENALTY OF AN ANGULAR 
! INTEGRATION IS SEVERE.
!--------------------------------------------------------------------------------------------------         
            CNTVAL(I,K) = TEXPSL(I,K) * RH2O(I,K) *VAR2(I,K) * BETINW / (RH2O(I,K) + RATH2OMW)
131 CONTINUE
!-------------------------------------------------------  
! COMPUTE SUMMED OPTICAL PATHS FOR H2O, O3 AND CONTINUUM
!------------------------------------------------------- 
    DO 201 I=1,IM
        TOTPHI(I,1) = ZERO
         TOTO3(I,1) = ZERO
        TPHIO3(I,1) = ZERO
        TOTVO2(I,1) = ZERO
201 END DO
!
    DO 203 K=2,LP1
        DO 203 I=1,IM
            TOTPHI(I,K) = TOTPHI(I,K-1) +   VAR2(I,K-1)
             TOTO3(I,K) =  TOTO3(I,K-1) +   VAR3(I,K-1)
            TPHIO3(I,K) = TPHIO3(I,K-1) +   VAR4(I,K-1)
            TOTVO2(I,K) = TOTVO2(I,K-1) + CNTVAL(I,K-1)
203 END DO
!--------------------------------------------------------------------------------------------------
! EMX1 IS THE ADDITIONAL PRESSURE-SCALED MASS FROM PRESS(L) TO P(L). IT IS USED IN NEARBY LAYER AND
! EMISS CALCULATIONS.
! EMX2 IS THE ADDITIONAL PRESSURE-SCALED MASS FROM PRESS(L) TO P(LP1). IT IS USED IN CALCULATIONS 
! BETWEEN FLUX LEVELS L AND LP1.
!--------------------------------------------------------------------------------------------------
    DO 801 I=1,IM
        EMX1(I) = QH2O(I,LM) * PRESS(I,LM) * (PRESS(I,LM ) -     P(I,LM)) * GP0INV
        EMX2(I) = QH2O(I,LM) * PRESS(I,LM) * (    P(I,LP1) - PRESS(I,LM)) * GP0INV
801 END DO
!--------------------------------------------------------------------------------------------------
! EMPL IS THE PRESSURE SCALED MASS FROM P(K) TO PRESS(K) (INDEX 2-LP1) 
! OR TO PRESS(K+1) (INDEX LP2-LL)
!--------------------------------------------------------------------------------------------------
    DO 811 K=1,LM
        DO 811 I=1,IM
            EMPL(I,K+1 )    = QH2O(I,K  ) * P(I,K+1) * (P(I,K+1) - PRESS(I,K  )) * GP0INV
811 END DO
!
    DO 812 K=1,LM1
        DO 812 I=1,IM
            EMPL(I,LP2+K-1) = QH2O(I,K+1) * P(I,K+1) * (PRESS(I,K+1) - P(I,K+1)) * GP0INV
812 END DO
!
    DO 821 I=1,IM
        EMPL(I,1   ) = VAR2(I,LM)
        EMPL(I,LLP1) = EMPL(I,LL)
821 END DO
!----------------------------------------------------------------------------------------------- 
! COMPUTE WEIGHTED TEMPERATURE (TDAV) AND PRESSURE (TSTDAV) INTEGRALS FOR USE IN OBTAINING TEMP. 
! DIFFERENCE BET. SOUNDING AND STD. TEMP. SOUNDING (DIFT)
!-----------------------------------------------------------------------------------------------
    DO 161 I=1,IM
        TSTDAV(I,1) = ZERO
          TDAV(I,1) = ZERO
161 END DO
!
    DO 162 K=1,LP1
        DO 162 I=1,IM
            VSUM3(I,K) = TEMP(I,K) - STEMP(K)
162 END DO
!
    DO 163 K=1,LM
        DO 165 I=1,IM
            VSUM2(I) = GTEMP(K) * DELP2(I,K)
            VSUM1(I) = VSUM2(I) * VSUM3(I,K)
!
            TSTDAV(I,K+1) = TSTDAV(I,K) + VSUM2(I)
              TDAV(I,K+1) =   TDAV(I,K) + VSUM1(I)
    165 END DO
163 END DO
!------------------------------------------------------------- 
! EVALUATE COEFFICIENTS FOR CO2 PRESSURE INTERPOLATION (A1,A2)
!-------------------------------------------------------------
    DO 171 I=1,IM
        A1(I) = (PRESS(I,LP1) - P0XZP8      ) / P0XZP2
        A2(I) = (P0           - PRESS(I,LP1)) / P0XZP2
171 END DO
!--------------------------------------------------------------------------------------------------
! PERFORM CO2 PRESSURE INTERPOLATION ON ALL INPUTTED TRANSMISSION FUNCTIONS AND TEMP. 
! DERIVATIVES SUCCESSIVELY COMPUTING CO2R,DCO2DT AND D2CDT2 IS DONE TO SAVE STORAGE 
! (AT A SLIGHT LOSS IN COMPUTATION TIME)
!--------------------------------------------------------------------------------------------------
    DO 184 K=1,LP1
        DO 184 I=1,IM
             CO2R1(I,K) = A1(I) * CO231(K) + A2(I) * CO238(K)
            D2CD21(I,K) = H1M3  * (A1(I) * C2D31(K) + A2(I) * C2D38(K))
            DCO2D1(I,K) = H1M2  * (A1(I) * CDT31(K) + A2(I) * CDT38(K))
!
             CO2R2(I,K) = A1(I) * CO271(K) + A2(I) * CO278(K)
            D2CD22(I,K) = H1M3  * (A1(I) * C2D71(K) + A2(I) * C2D78(K))
            DCO2D2(I,K) = H1M2  * (A1(I) * CDT71(K) + A2(I) * CDT78(K))
184 END DO
!
    DO 190 K=1,LM
        DO 190 I=1,IM
             CO2MR(I,K) =         A1(I) * CO2M51(K) + A2(I) * CO2M58(K)
             CO2MD(I,K) = H1M2 * (A1(I) * CDTM51(K) + A2(I) * CDTM58(K))
            CO2M2D(I,K) = H1M3 * (A1(I) * C2DM51(K) + A2(I) * C2DM58(K))
190 END DO
!--------------------------------------------------------------------------------------------------
! COMPUTE CO2 TEMPERATURE INTERPOLATIONS FOR ALL BANDS, USING DIFT THE CASE WHERE K=1 IS HANDLED 
! FIRST.
! WE ARE NOW REPLACING 3-DIMENSIONAL ARRAYS BY 2-D ARRAYS, TO SAVE SPACE. 
! THUS THIS CALCULATION IS FOR (I,KP,1)
!--------------------------------------------------------------------------------------------------
    DO 211 KP=2,LP1
        DO 211 I=1,IM
            DIFT(I,KP) = TDAV(I,KP) / TSTDAV(I,KP)
211 END DO
!
    DO 212 I=1,IM
          CO21(I,1,1) = 1.0
        CO2SP1(I,1)   = 1.0
        CO2SP2(I,1)   = 1.0
212 END DO
!
    DO 215 KP=2,LP1
        DO 215 I=1,IM
!------------------------------ 
! CALCULATIONS FOR KP>1 FOR K=1
!------------------------------
              CO2R(I,KP) =         A1(I) * CO251(KP,1) + A2(I) * CO258(KP,1)
            DCO2DT(I,KP) = H1M2 * (A1(I) * CDT51(KP,1) + A2(I) * CDT58(KP,1))
            D2CDT2(I,KP) = H1M3 * (A1(I) * C2D51(KP,1) + A2(I) * C2D58(KP,1))
!
            CO21(I,KP,1) = CO2R(I,KP) + DIFT(I,KP) * (DCO2DT(I,KP)                                &
    &                    +        HAF * DIFT(I,KP) *  D2CDT2(I,KP))
!-------------------------------------------------------------------------------------------        
! CALCULATIONS FOR (EFFECTIVELY) KP=1,K>KP. THESE USE THE SAME VALUE OF DIFT DUE TO SYMMETRY
!-------------------------------------------------------------------------------------------
            CO2R(I,KP)   =         A1(I) * CO251(1,KP) + A2(I) * CO258(1,KP)
            DCO2DT(I,KP) = H1M2 * (A1(I) * CDT51(1,KP) + A2(I) * CDT58(1,KP))
            D2CDT2(I,KP) = H1M3 * (A1(I) * C2D51(1,KP) + A2(I) * C2D58(1,KP))
!
            CO21(I,1,KP) = CO2R(I,KP) + DIFT(I,KP) * (DCO2DT(I,KP)                                &
    &                    +        HAF * DIFT(I,KP) *  D2CDT2(I,KP))
215 END DO
!--------------------------------------------------------------------------------------------------
! THE TRANSMISSION FUNCTIONS USED IN SPA88 MAY BE COMPUTED NOW.
! (IN THE 250 LOOP,DIFT REALLY SHOULD BE (I,1,K), BUT DIFT IS INVARIANT WITH RESPECT TO K, KP, AND
! SO (I,1,K)=(I,K,1))
!--------------------------------------------------------------------------------------------------
    DO 250 K=2,LP1
        DO 250 I=1,IM
            CO2SP1(I,K) = CO2R1(I,K) + DIFT(I,K) * (DCO2D1(I,K) + HAF * DIFT(I,K) * D2CD21(I,K))
            CO2SP2(I,K) = CO2R2(I,K) + DIFT(I,K) * (DCO2D2(I,K) + HAF * DIFT(I,K) * D2CD22(I,K))
250 END DO
!--------------------------- 
! NEXT THE CASE WHEN K=2...L
!---------------------------
    DO 220 K=2,LM
        DO 222 KP=K+1,LP1
            DO 222 I=1,IM
                  DIFT(I,KP)    = (TDAV(I,KP) - TDAV(I,K)) / (TSTDAV(I,KP) - TSTDAV(I,K))
!
                  CO2R(I,KP)    =         A1(I) * CO251(KP,K) + A2(I) * CO258(KP,K)
                DCO2DT(I,KP)    = H1M2 * (A1(I) * CDT51(KP,K) + A2(I) * CDT58(KP,K))
                D2CDT2(I,KP)    = H1M3 * (A1(I) * C2D51(KP,K) + A2(I) * C2D58(KP,K))
!
                  CO21(I,KP ,K) = CO2R(I,KP) + DIFT(I,KP) * (DCO2DT(I,KP)                         &
    &                           +        HAF * DIFT(I,KP) * D2CDT2(I,KP))
!
                  CO2R(I,KP)    =         A1(I) * CO251(K,KP) + A2(I) * CO258(K,KP)
                DCO2DT(I,KP)    = H1M2 * (A1(I) * CDT51(K,KP) + A2(I) * CDT58(K,KP))
                D2CDT2(I,KP)    = H1M3 * (A1(I) * C2D51(K,KP) + A2(I) * C2D58(K,KP))
!
                  CO21(I,K ,KP) = CO2R(I,KP) + DIFT(I,KP) * (DCO2DT(I,KP)                         &
    &                           +        HAF  * DIFT(I,KP) * D2CDT2(I,KP))
    222 END DO
220 END DO
!------------------------------------
! FINALLY THE CASE WHEN K=KP,K=2..LP1
!------------------------------------
    DO 206 K=2,LP1
        DO 206 I=1,IM
              DIFT(I,K)   = HAF  * (VSUM3(I,K) + VSUM3(I,K-1))
!
              CO2R(I,K)   =         A1(I) * CO251(K,K) + A2(I) * CO258(K,K)
            DCO2DT(I,K)   = H1M2 * (A1(I) * CDT51(K,K) + A2(I) * CDT58(K,K))
            D2CDT2(I,K)   = H1M3 * (A1(I) * C2D51(K,K) + A2(I) * C2D58(K,K))
!
              CO21(I,K,K) = CO2R(I,K) + DIFT(I,K) * (DCO2DT(I,K) + HAF * DIFT(I,K) * D2CDT2(I,K))
206 END DO
!---------------------------------------------- 
! WE ARENT DOING NBL TFS ON THE 100 CM-1 BANDS.
!----------------------------------------------
    DO 260 K=1,LM
        DO 260 I=1,IM
            CO2NBL(I,K) = CO2MR(I,K) + VSUM3(I,K) * (CO2MD(I,K) + HAF * VSUM3(I,K) * CO2M2D(I,K))
260 END DO
!----------------------------------------------------
! COMPUTE TEMP. COEFFICIENT BASED ON T(K) (SEE REF.2)
!----------------------------------------------------
    DO 264 K=1,LP1
        DO 264 I=1,IM
            IF (T(I,K) <= H25E2) THEN
                TLSQU(I,K) =  B0 + (T(I,K) - H25E2)                                               &
    &                      * (B1 + (T(I,K) - H25E2) * (B2 + B3 * (T(I,K) - H25E2)))
            ELSE
                TLSQU(I,K) =  B0
            END IF
264 END DO
!--------------------- 
! APPLY TO ALL CO2 TFS
!---------------------
    DO 280 K=1,LP1
        DO 282 KP=1,LP1
            DO 282 I=1,IM
                CO21(I,KP,K) = CO21(I,KP,K) * (ONE - TLSQU(I,KP)) + TLSQU(I,KP)
    282 END DO
280 END DO
!
    DO 284 K=1,LP1
        DO 286 I=1,IM
            CO2SP1(I,K)      = CO2SP1(I,K)  * (ONE - TLSQU(I,1))  + TLSQU(I,1)
            CO2SP2(I,K)      = CO2SP2(I,K)  * (ONE - TLSQU(I,1))  + TLSQU(I,1)
    286 END DO
284 END DO
!
    DO 288 K=1,LM
        DO 290 I=1,IM
            CO2NBL(I,K)      = CO2NBL(I,K)  * (ONE - TLSQU(I,K))  + TLSQU(I,K)
    290 END DO
288 END DO
!
    CALL FST88(HEATRA, GRNFLX, TOPFLX, QH2O  , PRESS , P     , DELP  , DELP2 , TEMP  , T     ,    &
    &          CLDFAC, NCLDS , KTOP  , KBTM  , CAMT  , CO21  , CO2NBL, CO2SP1, CO2SP2, VAR1  ,    &
    &          VAR2  , VAR3  , VAR4  , CNTVAL, TOTO3 , TPHIO3, TOTPHI, TOTVO2, EMX1  , EMX2  ,    &
    &          EMPL)
!
    RETURN
!
    END SUBROUTINE LWR88
