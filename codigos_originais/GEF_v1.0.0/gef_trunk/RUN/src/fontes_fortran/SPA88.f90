!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief  COMPUTES EXACT CTS HEATING RATES AND FLUXES
!> @details COMPUTES EXACT CTS HEATING RATES AND FLUXES AND CORRESPONDING CTS EMISSIVITY 
!! QUANTITIES FOR H2O,CO2 AND O3.
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
!> @param[in]   PRESS  - Significado de PRESS                                     
!> @param[in]   VAR1   - Significado de VAR1
!> @param[in]   VAR2   - Significado de VAR2
!> @param[in]   P      - Significado de P
!> @param[in]   DELP   - Significado de DELP
!> @param[in]   DELP2  - Significado de DELP2        
!> @param[in]   TOTVO2 - Significado de TOTVO2
!> @param[in]   TO3SP  - Significado de TO3SP
!> @param[in]   TO3SPC - Significado de TO3SPC                             
!> @param[in]   CO2SP1 - Significado de CO2SP1
!> @param[in]   CO2SP2 - Significado de CO2SP2
!> @param[in]   CO2SP  - Significado de CO2SP                             
!> @param[in]   CLDFAC - Significado de CLDFAC
!> @param[in]   TEMP   - Significado de TEMP                                                                                                  
!> @param[in]   SORC   - Significado de SORC
!> @param[in]   CSOUR  - Significado de CSOUR                                      
!> @param[out]  EXCTS  - Significado de EXCTS
!> @param[out]  CTSO3  - Significado de CTSO3                                       
!> @param[out]  GXCTS  - Significado de GXCTS
!> @details <b>Use Module:</b>
!! @arg @c CTLBLK
!! @arg @c F77KINDS
!> @details <b>Driver:</b> 
!! @arg @c FST88
!> @details <b>Calls:</b>
!--------------------------------------------------------------------------------------------------  
    SUBROUTINE SPA88 (EXCTS   , CTSO3   , GXCTS   , SORC    , CSOUR   , CLDFAC  , TEMP    ,       &
    &                 PRESS   , VAR1    , VAR2    , P       , DELP    , DELP2   , TOTVO2  ,       &
    &                 TO3SP   , TO3SPC  , CO2SP1  , CO2SP2  , CO2SP)
!--------------------------------------------------------------------------------------------------
! SUBROUTINE SPA88
!
! SUBPROGRAM: SPA88 - COMPUTES EXACT CTS HEATING RATES AND FLUXES 
! PROGRAMMER: ?????
! ORG: ?????
! DATE: ??-??-??
!
! ABSTRACT:
!SUBROUTINE SPA88 COMPUTES EXACT CTS HEATING RATES AND FLUXES AND CORRESPONDING CTS EMISSIVITY 
!QUANTITIES FOR H2O,CO2 AND O3.
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
! TEMP   -
! PRESS  -                                        
! VAR1   -
! VAR2   -
! P      -
! DELP   -
! DELP2  -         
! TOTVO2 -
! TO3SP  -
! TO3SPC -                             
! CO2SP1 -
! CO2SP2 -
! CO2SP  -                              
! CLDFAC -                                                                                         
! SORC   -
! CSOUR  -                                        
!  
! OUTPUT ARGUMENT LIST:
! EXCTS -
! CTSO3 -                                       
! GXCTS -  
!
! INPUT/OUTPUT ARGUMENT LIST:
! NONE                                                 
!        
! USE MODULES: F77KINDS
!              HCON
!              MPPSTAFF
!              PARMETA
!              RDPARM
!              RNDDTA
!
! DRIVER     : FST88
!
! CALLS      : -----
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
    USE HCON
    USE MPPSTAFF
    USE PARMETA
    USE RDPARM
    USE RNDDTA
!    
    IMPLICIT NONE
!
    REAL   (KIND=R4)    , DIMENSION(IM, LP1, NBLY)                        , INTENT(IN)          ::&
    & SORC     
!
    REAL   (KIND=R4)    , DIMENSION(IM, LP1)                              , INTENT(IN)          ::&
    & CSOUR
!   
    REAL   (KIND=R4)    , DIMENSION(IM, LP1, LP1)                         , INTENT(IN)          ::&
    & CLDFAC               
!
    REAL   (KIND=R4)    , DIMENSION(IM, LP1)                              , INTENT(IN)          ::&
    & TEMP    , PRESS   , P       , TOTVO2  , TO3SP   , CO2SP1  , CO2SP2  , CO2SP
!
    REAL   (KIND=R4)    , DIMENSION(IM, LM)                               , INTENT(IN)          ::&
    & VAR1    , VAR2    , DELP    , DELP2   , TO3SPC
!
    REAL   (KIND=R4)    , DIMENSION(IM, LM)                               , INTENT(OUT)         ::&
    & EXCTS   , CTSO3
!
    REAL   (KIND=R4)    , DIMENSION(IM)                                   , INTENT(OUT)         ::&
    & GXCTS
!
    REAL   (KIND=R4)    , DIMENSION(IM, LM)                                                     ::&
    & PHITMP  , PSITMP  , TT      , FAC1    , FAC2    , X       , Y       , TOPM    , TOPPHI
!
    REAL   (KIND=R4)    , DIMENSION(IM, LP1)                                                    ::&
    & CTMP    , CTMP2   , CTMP3    
!
    REAL   (KIND=R4)    , DIMENSION(IM, LM)                                                     ::&
    & F       , FF      , AG      , AGG
!------------------------
! IMPLICIT NONE VARIABLES
!------------------------
    INTEGER(KIND=I4)                                                                            ::&
    & I       , K
 !
    EQUIVALENCE (F , AG , PHITMP)
    EQUIVALENCE (FF, AGG, PSITMP)
!-------------------------------------------------- 
! COMPUTE TEMPERATURE QUANTITIES FOR USE IN PROGRAM
!-------------------------------------------------- 
    DO 101 K=1,LM
        DO 101 I=1,IM
            X(I,K) = TEMP(I,K) - H25E2
            Y(I,K) =    X(I,K) * X(I,K)
101 END DO
!---------------------------------------------------------------------------------------------- 
! INITIALIZE CTMP(I,1),CTMP2(I,1),CTMP3(I,1) TO UNITY; THESE ARE TRANSMISSION FCTNS AT THE TOP.
!---------------------------------------------------------------------------------------------- 
    DO 345 I=1,IM
        CTMP (I,1) = ONE
        CTMP2(I,1) = 1.
        CTMP3(I,1) = 1.
345 END DO
!---------------------------------- 
! BEGIN LOOP ON FREQUENCY BANDS (1) 
!---------------------------------- 
!
!-----------------------------------------
! CALCULATION FOR BAND 1 (COMBINED BAND 1)
!----------------------------------------- 
!
!------------------------------------------------------------------------------------------- 
! OBTAIN TEMPERATURE CORRECTION (CAPPHI,CAPPSI),THEN MULTIPLY BY OPTICAL PATH (VAR1,VAR2) TO 
! COMPUTE TEMPERATURE-CORRECTED OPTICAL PATH AND MEAN PRESSURE FOR A LAYER (PHITMP,PSITMP)
!------------------------------------------------------------------------------------------- 
    DO 301 K=1,LM
        DO 301 I=1,IM
                  F(I,K) =  H44194M2  * (APCM   (1)    * X  (I,K)  + BPCM(1)  * Y(I,K))
                 FF(I,K) =  H44194M2  * (ATPCM  (1)    * X  (I,K)  + BTPCM(1) * Y(I,K))
                 AG(I,K) = (H1P41819  + F       (I,K)) * F  (I,K)  + ONE
                AGG(I,K) = (H1P41819  + FF      (I,K)) * FF (I,K)  + ONE
             PHITMP(I,K) =  VAR1(I,K) * (((( AG (I,K)  * AG (I,K))  ** 2) ** 2) ** 2)
             PSITMP(I,K) =  VAR2(I,K) * (((( AGG(I,K)  * AGG(I,K))  ** 2) ** 2) ** 2)
301 END DO
!--------------------------------------------------------------- 
! OBTAIN OPTICAL PATH,MEAN PRESSURE FROM THE TOP TO THE PRESSURE
! P(K) (TOPM,TOPPHI)
!--------------------------------------------------------------- 
    DO 315 I=1,IM
          TOPM(I,1) = PHITMP(I,1)
        TOPPHI(I,1) = PSITMP(I,1)
315 END DO
!
    DO 319 K=2,LM
        DO 317 I=1,IM
              TOPM(I,K) =   TOPM(I,K-1) + PHITMP(I,K)
            TOPPHI(I,K) = TOPPHI(I,K-1) + PSITMP(I,K)
    317 END DO
319 END DO
!----------------------------------------------- 
! TT IS THE CLOUD-FREE CTS TRANSMISSION FUNCTION
!----------------------------------------------- 
    DO 321 K=1,LM
        DO 321 I=1,IM
            FAC1(I,K)   = ACOMB(1)    *   TOPM(I,K)
            FAC2(I,K)   = FAC1 (I,K)  *   TOPM(I,K) / (BCOMB(1) * TOPPHI(I,K))
              TT(I,K)   = EXP  (HM1EZ *   FAC1(I,K) / SQRT  (1. + FAC2  (I,K)))
            CTMP(I,K+1) = TT   (I,K)  * CLDFAC(I,K+1,1)
321 END DO
!--------------------------------------------------------------- 
! EXCTS IS THE CTS COOLING RATE ACCUMULATED OVER FREQUENCY BANDS
!--------------------------------------------------------------- 
    DO 353 K=1,LM
        DO 353 I=1,IM
            EXCTS(I,K) = SORC(I,K,1) * (CTMP(I,K+1) - CTMP(I,K))
353 END DO
!----------------------------------------------------------------- 
! GXCTS IS THE EXACT CTS TOP FLUX ACCUMULATED OVER FREQUENCY BANDS
!----------------------------------------------------------------- 
    DO 361 I=1,IM
        GXCTS(I) =  CLDFAC(I,LP1,1) * ( TT(I,LM)      *  SORC(I,LM,1)                             &
    &            + (HAF             * DELP(I,LM)      * (  TT(I,LM1)    * (   P(I,LP1)            &
    &            - PRESS  (I,LM))   + TT  (I,LM)      * (   P(I,LP1)    + PRESS(I,LM)             &
    &            - TWO              * P   (I,LM))))   * (SORC(I,LP1,1)  -  SORC(I,LM,1)))
361 END DO
!----------------------------------------- 
! CALCULATION FOR BAND 2 (COMBINED BAND 2)
!----------------------------------------- 
!
!------------------------------------------------------------------------------------------- 
! OBTAIN TEMPERATURE CORRECTION (CAPPHI,CAPPSI),THEN MULTIPLY BY OPTICAL PATH (VAR1,VAR2) TO 
! COMPUTE TEMPERATURE-CORRECTED OPTICAL PATH AND MEAN PRESSURE FOR A LAYER (PHITMP,PSITMP)
!------------------------------------------------------------------------------------------- 
    DO 401 K=1,LM
        DO 401 I=1,IM
                 F(I,K) =  H44194M2  * (   APCM(2)    *   X(I,K)  + BPCM (2) * Y(I,K))
                FF(I,K) =  H44194M2  * (  ATPCM(2)    *   X(I,K)  + BTPCM(2) * Y(I,K))
                AG(I,K) = (H1P41819  +        F(I,K)) *   F(I,K)  + ONE
               AGG(I,K) = (H1P41819  +       FF(I,K)) *  FF(I,K)  + ONE
            PHITMP(I,K) =  VAR1(I,K) * ((((  AG(I,K)  *  AG(I,K)) ** 2) ** 2) ** 2)
            PSITMP(I,K) =  VAR2(I,K) * (((( AGG(I,K)  * AGG(I,k)) ** 2) ** 2) ** 2)
401 END DO
!--------------------------------------------------------------- 
! OBTAIN OPTICAL PATH,MEAN PRESSURE FROM THE TOP TO THE PRESSURE
! P(K) (TOPM,TOPPHI)
!--------------------------------------------------------------- 
    DO 415 I=1,IM
        TOPM  (I,1) = PHITMP(I,1)
        TOPPHI(I,1) = PSITMP(I,1)
415 END DO
!
    DO 419 K=2,LM
        DO 417 I=1,IM
              TOPM(I,K) =   TOPM(I,K-1) + PHITMP(I,K)
            TOPPHI(I,K) = TOPPHI(I,K-1) + PSITMP(I,K)
    417 END DO
419 END DO
!----------------------------------------------- 
! TT IS THE CLOUD-FREE CTS TRANSMISSION FUNCTION
!----------------------------------------------- 
    DO 421 K=1,LM
        DO 421 I=1,IM
            FAC1(I,K)   = ACOMB(2)    *   TOPM(I,K)
            FAC2(I,K)   =  FAC1(I,K)  *   TOPM(I,K) / (BCOMB(2) * TOPPHI(I,K))
              TT(I,K)   =   EXP(HM1EZ *   FAC1(I,K) / SQRT  (1. + FAC2  (I,K)))
            CTMP(I,K+1) =    TT(I,K)  * CLDFAC(I,K+1,1)
421 END DO
!--------------------------------------------------------------- 
! EXCTS IS THE CTS COOLING RATE ACCUMULATED OVER FREQUENCY BANDS
!---------------------------------------------------------------  
    DO 453 K=1,LM
        DO 453 I=1,IM
            EXCTS(I,K) = EXCTS(I,K)   + SORC(I,K,2) * (CTMP(I,K+1) - CTMP(I,K))
453 END DO
!----------------------------------------------------------------- 
! GXCTS IS THE EXACT CTS TOP FLUX ACCUMULATED OVER FREQUENCY BANDS
!-----------------------------------------------------------------   
    DO 461 I=1,IM
        GXCTS(I) = GXCTS(I)         + CLDFAC(I,LP1,1) * (  TT(I,LM)     *  SORC(I,LM,2)           &
    &            + (HAF             *   DELP(I,LM)    * (  TT(I,LM1)    * (   P(I,LP1)            &
    &            - PRESS(I,LM))     +     TT(I,LM)    * (   P(I,LP1)    + PRESS(I,LM)             &
    &            - TWO              *      P(I,LM)))) * (SORC(I,LP1, 2) -  SORC(I,LM,2)))
461 END DO
!----------------------------------------- 
! CALCULATION FOR BAND 3 (COMBINED BAND 3)
!----------------------------------------- 
!------------------------------------------------------------------------------------------- 
! OBTAIN TEMPERATURE CORRECTION (CAPPHI,CAPPSI),THEN MULTIPLY BY OPTICAL PATH (VAR1,VAR2) TO 
! COMPUTE TEMPERATURE-CORRECTED OPTICAL PATH AND MEAN PRESSURE FOR A LAYER (PHITMP,PSITMP)
!------------------------------------------------------------------------------------------- 
    DO 501 K=1,LM
        DO 501 I=1,IM
                 F(I,K) =  H44194M2  * (   APCM(3)    *   X(I,K)  + BPCM (3)   * Y(I,K))
                FF(I,K) =  H44194M2  * (  ATPCM(3)    *   X(I,K)  + BTPCM(3)   * Y(I,K))
                AG(I,K) = (H1P41819  +        F(I,K)) *   F(I,K)  + ONE
               AGG(I,K) = (H1P41819  +       FF(I,K)) *  FF(I,K)  + ONE
            PHITMP(I,K) =  VAR1(I,K) * ((((  AG(I,K)  *  AG(I,K)) ** 2) ** 2) ** 2)
            PSITMP(I,K) =  VAR2(I,K) * (((( AGG(I,K)  * AGG(I,K)) ** 2) ** 2) ** 2)
501 END DO
!--------------------------------------------------------------- 
! OBTAIN OPTICAL PATH,MEAN PRESSURE FROM THE TOP TO THE PRESSURE
! P(K) (TOPM,TOPPHI)
!--------------------------------------------------------------- 
    DO 515 I=1,IM
          TOPM(I,1) = PHITMP(I,1)
        TOPPHI(I,1) = PSITMP(I,1)
515 END DO
!
    DO 519 K=2,LM
        DO 517 I=1,IM
              TOPM(I,K) =   TOPM(I,K-1) + PHITMP(I,K)
            TOPPHI(I,K) = TOPPHI(I,K-1) + PSITMP(I,K)
    517 END DO
519 END DO
!----------------------------------------------- 
! TT IS THE CLOUD-FREE CTS TRANSMISSION FUNCTION
!----------------------------------------------- 
    DO 521 K=1,LM
        DO 521 I=1,IM
            FAC1(I,K)   = ACOMB(3)    *   TOPM(I,K)
            FAC2(I,K)   =  FAC1(I,K)  *   TOPM(I,K)      / (BCOMB(3) * TOPPHI(I,K))
              TT(I,K)   =   EXP(HM1EZ *   FAC1(I,K)      / SQRT  (1. +   FAC2(I,K)))
            CTMP(I,K+1) =    TT(I,K)  * CLDFAC(I,K+1,1)
521 END DO
!--------------------------------------------------------------- 
! EXCTS IS THE CTS COOLING RATE ACCUMULATED OVER FREQUENCY BANDS
!--------------------------------------------------------------- 
    DO 553 K=1,LM
        DO 553 I=1,IM
            EXCTS(I,K) = EXCTS(I,K) + SORC(I,K,3) * (CTMP(I,K+1) - CTMP(I,K))
553 END DO
!----------------------------------------------------------------- 
! GXCTS IS THE EXACT CTS TOP FLUX ACCUMULATED OVER FREQUENCY BANDS
!----------------------------------------------------------------- 
    DO 561 I=1,IM
        GXCTS(I) = GXCTS(I)       +  CLDFAC(I,LP1, 1) *  (TT(I,LM)  *  SORC(I,LM,3)               &
    &            + (HAF           *    DELP(I,LM)     * ( TT(I,LM1) * (   P(I,LP1)                &
    &            - PRESS(I,LM))   +      TT(I,LM)     * (  P(I,LP1) + PRESS(I,LM)   - TWO         &
    &            *     P(I,LM)))) * (  SORC(I,LP1,3)  - SORC(I,LM,3)))
561 END DO
!----------------------------------------- 
! CALCULATION FOR BAND 4 (COMBINED BAND 4)
!----------------------------------------- 
!------------------------------------------------------------------------------------------- 
! OBTAIN TEMPERATURE CORRECTION (CAPPHI,CAPPSI),THEN MULTIPLY BY OPTICAL PATH (VAR1,VAR2) TO 
! COMPUTE TEMPERATURE-CORRECTED OPTICAL PATH AND MEAN PRESSURE FOR A LAYER (PHITMP,PSITMP)
!------------------------------------------------------------------------------------------- 
    DO 601 K=1,LM
        DO 601 I=1,IM
                 F(I,K) =  H44194M2   * (  APCM(4)     *   X(I,K)  + BPCM (4)    * Y(I,K))
                FF(I,K) =  H44194M2   * ( ATPCM(4)     *   X(I,K)  + BTPCM(4)    * Y(I,K))
                AG(I,K) = (H1P41819   +       F(I,K))  *   F(I,K)  + ONE
               AGG(I,K) = (H1P41819   +      FF(I,K))  *  FF(I,K)  + ONE
            PHITMP(I,K) =  VAR1(I,K)  * ((((  AG(I,K)  *  AG(I,K)) ** 2) ** 2) ** 2)
            PSITMP(I,K) =  VAR2(I,K)  * (((( AGG(I,K)  * AGG(I,K)) ** 2) ** 2) ** 2)
601 END DO
!-----------------------------------------------------------------------------------
! OBTAIN OPTICAL PATH, MEAN PRESSURE FROM THE TOP TO THE PRESSURE P(K) (TOPM,TOPPHI)
!-----------------------------------------------------------------------------------
    DO 615 I=1,IM
          TOPM(I,1) = PHITMP(I,1)
        TOPPHI(I,1) = PSITMP(I,1)
615 CONTINUE
!
    DO 619 K=2,LM
        DO 617 I=1,IM
              TOPM(I,K) =   TOPM(I,K-1) + PHITMP(I,K)
            TOPPHI(I,K) = TOPPHI(I,K-1) + PSITMP(I,K)
    617 CONTINUE
619 CONTINUE
!----------------------------------------------- 
! TT IS THE CLOUD-FREE CTS TRANSMISSION FUNCTION
!----------------------------------------------- 
    DO 621 K=1,LM
        DO 621 I=1,IM
            FAC1(I,K)   = ACOMB(4)    *   TOPM(I,K)
            FAC2(I,K)   =  FAC1(I,K)  *   TOPM(I,K)      / (BCOMB(4) * TOPPHI(I,K))
              TT(I,K)   =   EXP(HM1EZ *   FAC1(I,K)      / SQRT  (1. + FAC2  (I,K)))
            CTMP(I,K+1) =    TT (I,K) * CLDFAC(I,K+1,1)
621 END DO
!--------------------------------------------------------------- 
! EXCTS IS THE CTS COOLING RATE ACCUMULATED OVER FREQUENCY BANDS
!--------------------------------------------------------------- 
    DO 653 K=1,LM
        DO 653 I=1,IM
            EXCTS(I,K) = EXCTS(I,K) + SORC(I,K,4) * (CTMP(I,K+1) - CTMP(I,K))
653 END DO
!----------------------------------------------------------------- 
! GXCTS IS THE EXACT CTS TOP FLUX ACCUMULATED OVER FREQUENCY BANDS
!----------------------------------------------------------------- 
    DO 661 I=1,IM
        GXCTS(I) = GXCTS(I)     + CLDFAC(I,LP1,1) * (  TT(I,LM)    *  SORC(I,LM,4)                &
    &            + (HAF         *   DELP(I,LM)    * (  TT(I,LM1)   * (   P(I,LP1)                 &
    &            - PRESS(I,LM)) +     TT(I,LM)    * (   P(I,LP1)   + PRESS(I,LM)                  &
    &            - TWO          *      P(I,LM)))) * (SORC(I,LP1,4) -  SORC(I,LM,4)))
661 END DO
!----------------------------------------- 
! CALCULATION FOR BAND 5 (COMBINED BAND 5)
!----------------------------------------- 
!------------------------------------------------------------------------------------------- 
! OBTAIN TEMPERATURE CORRECTION (CAPPHI,CAPPSI),THEN MULTIPLY BY OPTICAL PATH (VAR1,VAR2) TO 
! COMPUTE TEMPERATURE-CORRECTED OPTICAL PATH AND MEAN PRESSURE FOR A LAYER (PHITMP,PSITMP)
!------------------------------------------------------------------------------------------- 
    DO 701 K=1,LM
        DO 701 I=1,IM
                 F(I,K) =  H44194M2   * (   APCM(5)    *   X(I,K)  +  BPCM(5)    * Y(I,K))
                FF(I,K) =  H44194M2   * (  ATPCM(5)    *   X(I,K)  + BTPCM(5)    * Y(I,K))
                AG(I,K) = (H1P41819   +        F(I,K)) *   F(I,K)  + ONE
               AGG(I,K) = (H1P41819   +       FF(I,K)) *  FF(I,K)  + ONE
            PHITMP(I,K) =  VAR1(I,K)  * ((((  AG (I,K) *  AG(I,K)) ** 2) ** 2) ** 2)
            PSITMP(I,K) =  VAR2(I,K)  * (((( AGG(I,K)  * AGG(I,K)) ** 2) ** 2) ** 2)
701 END DO
!--------------------------------------------------------------- 
! OBTAIN OPTICAL PATH,MEAN PRESSURE FROM THE TOP TO THE PRESSURE
! P(K) (TOPM,TOPPHI)
!--------------------------------------------------------------- 
    DO 715 I=1,IM
          TOPM(I,1) = PHITMP(I,1)
        TOPPHI(I,1) = PSITMP(I,1)
715 END DO
!
    DO 719 K=2,LM
        DO 717 I=1,IM
              TOPM(I,K) =   TOPM(I,K-1) + PHITMP(I,K)
            TOPPHI(I,K) = TOPPHI(I,K-1) + PSITMP(I,K)
    717 END DO
719 END DO
!----------------------------------------------- 
! TT IS THE CLOUD-FREE CTS TRANSMISSION FUNCTION
!----------------------------------------------- 
    DO 721 K=1,LM
        DO 721 I=1,IM
            FAC1(I,K)   =  ACOMB(5)     *   TOPM(I,K)
            FAC2(I,K)   =   FAC1(I,K)   *   TOPM(I,K)   / (BCOMB(5)  * TOPPHI(I,K))
              TT(I,K)   =    EXP(HM1EZ  * ( FAC1(I,K)   /   SQRT(ONE +   FAC2(I,K))               &
    &                   + BETACM(5)     * TOTVO2(I,K+1) * SKO2D))
!            
            CTMP(I,K+1) =     TT(I,K)   * CLDFAC(I,K+1,1)
721 END DO
!--------------------------------------------------------------- 
! EXCTS IS THE CTS COOLING RATE ACCUMULATED OVER FREQUENCY BANDS
!--------------------------------------------------------------- 
    DO 753 K=1,LM
        DO 753 I=1,IM
            EXCTS(I,K) = EXCTS(I,K) + SORC(I,K,5) * (CTMP(I,K+1) - CTMP(I,K))
753 END DO
!----------------------------------------------------------------- 
! GXCTS IS THE EXACT CTS TOP FLUX ACCUMULATED OVER FREQUENCY BANDS
!----------------------------------------------------------------- 
    DO 761 I=1,IM
        GXCTS(I) = GXCTS(I)         + CLDFAC(I,LP1,1)  * (  TT(I,LM)     *  SORC(I,LM,5)          &
    &            + (HAF             *   DELP(I,LM)     * (  TT(I,LM1)    * (   P(I,LP1)           & 
    &            - PRESS(I,LM))     +     TT(I,LM)     * (   P(I,LP1)    + PRESS(I,LM)            &
    &            - TWO              *      P(I,LM))))  * (SORC(I,LP1,5)  -  SORC(I,LM,5)))
761 END DO
!----------------------------------------- 
! CALCULATION FOR BAND 6 (COMBINED BAND 6)
!----------------------------------------- 
!------------------------------------------------------------------------------------------- 
! OBTAIN TEMPERATURE CORRECTION (CAPPHI,CAPPSI),THEN MULTIPLY BY OPTICAL PATH (VAR1,VAR2) TO 
! COMPUTE TEMPERATURE-CORRECTED  OPTICAL PATH AND MEAN PRESSURE FOR A LAYER (PHITMP,PSITMP)
!------------------------------------------------------------------------------------------- 
    DO 801 K=1,LM
        DO 801 I=1,IM
                 F(I,K) =  H44194M2   * (   APCM(6)     *   X(I,K)  +  BPCM(6)    * Y(I,K))
                FF(I,K) =  H44194M2   * (  ATPCM(6)     *   X(I,K)  + BTPCM(6)    * Y(I,K))
                AG(I,K) = (H1P41819   +        F(I,K))  *   F(I,K)  + ONE
               AGG(I,K) = (H1P41819   +       FF(I,K))  *  FF(I,K)  + ONE
            PHITMP(I,K) =  VAR1(I,K)  * ((((  AG(I,K)   *  AG(I,K)) ** 2) ** 2) ** 2)
            PSITMP(I,K) =  VAR2(I,K)  * (((( AGG(I,K)   * AGG(I,K)) ** 2) ** 2) ** 2)
801 END DO
!--------------------------------------------------------------- 
! OBTAIN OPTICAL PATH,MEAN PRESSURE FROM THE TOP TO THE PRESSURE
! P(K) (TOPM,TOPPHI)
!--------------------------------------------------------------- 
    DO 815 I=1,IM
          TOPM(I,1) = PHITMP(I,1)
        TOPPHI(I,1) = PSITMP(I,1)
815 END DO
!
    DO 819 K=2,LM
        DO 817 I=1,IM
              TOPM(I,K) =   TOPM(I,K-1) + PHITMP(I,K)
            TOPPHI(I,K) = TOPPHI(I,K-1) + PSITMP(I,K)
    817 END DO
819 END DO
!----------------------------------------------- 
! TT IS THE CLOUD-FREE CTS TRANSMISSION FUNCTION
!----------------------------------------------- 
    DO 821 K=1,LM
        DO 821 I=1,IM
            FAC1(I,K)   =  ACOMB(6)    *   TOPM(I,K)
            FAC2(I,K)   =   FAC1(I,K)  *   TOPM(I,K)   / (BCOMB(6)  * TOPPHI(I,K))
              TT(I,K)   =    EXP(HM1EZ * ( FAC1(I,K)   /   SQRT(ONE +   FAC2(I,K))                &
    &                   + BETACM(6)    * TOTVO2(I,K+1) * SKO2D))
!
            CTMP(I,K+1) =     TT(I,K)  * CLDFAC(I,K+1,1)
821 END DO
!--------------------------------------------------------------- 
! EXCTS IS THE CTS COOLING RATE ACCUMULATED OVER FREQUENCY BANDS
!--------------------------------------------------------------- 
    DO 853 K=1,LM
        DO 853 I=1,IM
            EXCTS(I,K)= EXCTS(I,K) + SORC(I,K,6) * (CTMP(I,K+1) - CTMP(I,K))
853 END DO
!----------------------------------------------------------------- 
! GXCTS IS THE EXACT CTS TOP FLUX ACCUMULATED OVER FREQUENCY BANDS
!----------------------------------------------------------------- 
    DO 861 I=1,IM
        GXCTS(I) = GXCTS(I)        + CLDFAC(I,LP1,1) * ( TT(I,LM)  *  SORC(I,LM,6)                &
    &            + ( HAF           *   DELP(I,LM)    * ( TT(I,LM1) * (   P(I,LP1)                 &
    &            - PRESS(I,LM))    +     TT(I,LM)    * (  P(I,LP1) + PRESS(I,LM)  - TWO           &
    &            *     P(I,LM))))  * ( SORC(I,LP1,6) - SORC(I,LM,6)))
861 END DO
!----------------------------------------- 
! CALCULATION FOR BAND 7 (COMBINED BAND 7)
!----------------------------------------- 
!------------------------------------------------------------------------------------------- 
! OBTAIN TEMPERATURE CORRECTION (CAPPHI,CAPPSI),THEN MULTIPLY BY OPTICAL PATH (VAR1,VAR2) TO 
! COMPUTE TEMPERATURE-CORRECTED OPTICAL PATH AND MEAN PRESSURE FOR A LAYER (PHITMP,PSITMP)
!------------------------------------------------------------------------------------------- 
    DO 901 K=1,LM
        DO 901 I=1,IM
                 F(I,K) =  H44194M2   * (   APCM(7)    *   X(I,K)   + BPCM (7) * Y(I,K))
                FF(I,K) =  H44194M2   * (  ATPCM(7)    *   X(I,K)   + BTPCM(7) * Y(I,K))
                AG(I,K) = (H1P41819   +        F(I,K)) *   F(I,K)   + ONE
               AGG(I,K) = (H1P41819   +       FF(I,K)) *  FF(I,K)   + ONE
            PHITMP(I,K) =  VAR1(I,K)  * ((((  AG(I,K)  *  AG (I,K)) ** 2) ** 2) ** 2)
            PSITMP(I,K) =  VAR2(I,K)  * (((( AGG(I ,K) * AGG(I,K))  ** 2) ** 2) ** 2)
901 END DO
!---------------------------------------------------------------
! OBTAIN OPTICAL PATH,MEAN PRESSURE FROM THE TOP TO THE PRESSURE
! P(K) (TOPM,TOPPHI)
!--------------------------------------------------------------- 
    DO 915 I=1,IM
          TOPM(I,1) = PHITMP(I,1)
        TOPPHI(I,1) = PSITMP(I,1)
915 END DO
!
    DO 919 K=2,LM
        DO 917 I=1,IM
              TOPM(I,K) =   TOPM(I,K-1) + PHITMP(I,K)
            TOPPHI(I,K) = TOPPHI(I,K-1) + PSITMP(I,K)
    917 END DO
919 END DO
!----------------------------------------------- 
! TT IS THE CLOUD-FREE CTS TRANSMISSION FUNCTION
!----------------------------------------------- 
    DO 921 K=1,LM
        DO 921 I=1,IM
            FAC1(I,K)   =   ACOMB(7)    *   TOPM(I,K)
            FAC2(I,K)   =    FAC1(I,K)  *   TOPM(I,K)   / (BCOMB(7)  * TOPPHI(I,K))
              TT(I,K)   =     EXP(HM1EZ * ( FAC1(I,K)   /   SQRT(ONE +   FAC2(I,K))               &
    &                    + BETACM(7)    * TOTVO2(I,K+1) * SKO2D))
!
            CTMP(I,K+1) =      TT(I,K)  * CLDFAC(I,K+1,1)
921 END DO
!--------------------------------------------------------------- 
! EXCTS IS THE CTS COOLING RATE ACCUMULATED OVER FREQUENCY BANDS
!--------------------------------------------------------------- 
    DO 953 K=1,LM
        DO 953 I=1,IM
            EXCTS(I,K) = EXCTS(I,K) + SORC(I,K,7) * (CTMP(I,K+1) - CTMP(I,K))
953 END DO
!----------------------------------------------------------------- 
! GXCTS IS THE EXACT CTS TOP FLUX ACCUMULATED OVER FREQUENCY BANDS
!----------------------------------------------------------------- 
    DO 961 I=1,IM
        GXCTS(I) = GXCTS(I)     + CLDFAC(I,LP1,1) * (  TT(I,LM)    *  SORC(I,LM,7)                & 
    &            + (HAF         *   DELP(I,LM)    * (  TT(I,LM1)   * (   P(I,LP1)                 &
    &            - PRESS(I,LM)) +     TT(I,LM)    * (   P(I,LP1)   + PRESS(I,LM)                  &
    &            - TWO          *      P(I,LM)))) * (SORC(I,LP1,7) -  SORC(I,LM,7)))
961 END DO
!----------------------------------------- 
! CALCULATION FOR BAND 8 (COMBINED BAND 8)
!----------------------------------------- 
!------------------------------------------------------------------------------------------- 
! OBTAIN TEMPERATURE CORRECTION (CAPPHI,CAPPSI),THEN MULTIPLY BY OPTICAL PATH (VAR1,VAR2) TO 
! COMPUTE TEMPERATURE-CORRECTED OPTICAL PATH AND MEAN PRESSURE FOR A LAYER (PHITMP,PSITMP)
!-------------------------------------------------------------------------------------------
    DO 962 K=1,LM
        DO 962 I=1,IM
                 F(I,K) =  H44194M2   * (   APCM(8)    *   X(I,K)  + BPCM (8)    * Y(I,K))
                FF(I,K) =  H44194M2   * (  ATPCM(8)    *   X(I,K)  + BTPCM(8)    * Y(I,K))
                AG(I,K) = (H1P41819   +        F(I,K)) *   F(I,K)  + ONE
               AGG(I,K) = (H1P41819   +       FF(I,K)) *  FF(I,K)  + ONE
            PHITMP(I,K) =  VAR1(I,K)  * ((((  AG(I,K)  *  AG(I,K)) ** 2) ** 2) ** 2)
            PSITMP(I,K) =  VAR2(I,K)  * (((( AGG(I,K)  * AGG(I,K)) ** 2) ** 2) ** 2)
962 END DO
!--------------------------------------------------------------- 
! OBTAIN OPTICAL PATH,MEAN PRESSURE FROM THE TOP TO THE PRESSURE
! P(K) (TOPM,TOPPHI)
!--------------------------------------------------------------- 
    DO 963 I=1,IM
          TOPM(I,1) = PHITMP(I,1)
        TOPPHI(I,1) = PSITMP(I,1)
    963 END DO
!
    DO 965 K=2,LM
        DO 964 I=1,IM
            TOPM  (I,K) = TOPM  (I,K-1) + PHITMP(I,K)
            TOPPHI(I,K) = TOPPHI(I,K-1) + PSITMP(I,K)
    964 END DO
965 END DO
!----------------------------------------------- 
! TT IS THE CLOUD-FREE CTS TRANSMISSION FUNCTION
!----------------------------------------------- 
    DO 966 K=1,LM
        DO 966 I=1,IM
            FAC1(I,K)   =  ACOMB(8)    *   TOPM(I,K)
            FAC2(I,K)   =   FAC1(I,K)  *   TOPM(I,K)   / (BCOMB(8)  * TOPPHI(I,K))
              TT(I,K)   =    EXP(HM1EZ * ( FAC1(I,K)   /   SQRT(ONE +   FAC2(I,K))                &
    &                   + BETACM(8)    * TOTVO2(I,K+1) * SKO2D))
!
            CTMP(I,K+1) =     TT(I,K)  * CLDFAC(I,K+1,1)
966 END DO
!--------------------------------------------------------------- 
! EXCTS IS THE CTS COOLING RATE ACCUMULATED OVER FREQUENCY BANDS
!--------------------------------------------------------------- 
    DO 967 K=1,LM
        DO 967 I=1,IM
            EXCTS(I,K) = EXCTS(I,K) + SORC(I,K, 8) * (CTMP(I,K+1) - CTMP(I,K))
967 END DO
!----------------------------------------------------------------- 
! GXCTS IS THE EXACT CTS TOP FLUX ACCUMULATED OVER FREQUENCY BANDS
!----------------------------------------------------------------- 
    DO 968 I=1,IM
        GXCTS(I) = GXCTS(I)     + CLDFAC(I,LP1,1)  * (  TT(I,LM)     *  SORC(I,LM,8)              & 
    &            + (HAF         *   DELP(I,LM)     * (  TT(I,LM1)    * (   P(I,LP1)               & 
    &            - PRESS(I,LM)) +     TT(I,LM)     * (   P(I,LP1)    + PRESS(I,LM)                &
    &            - TWO          *      P(I,LM))))  * (SORC(I,LP1,8)  -  SORC(I,LM,8)))
968 END DO
!----------------------------------------------------- 
! CALCULATION FOR BAND 9 ( 560-670 CM-1; INCLUDES CO2)
!----------------------------------------------------- 
!------------------------------------------------------------------------------------------- 
! OBTAIN TEMPERATURE CORRECTION (CAPPHI,CAPPSI),THEN MULTIPLY BY OPTICAL PATH (VAR1,VAR2) TO
! COMPUTE TEMPERATURE-CORRECTED OPTICAL PATH AND MEAN PRESSURE FOR A LAYER (PHITMP,PSITMP)
!------------------------------------------------------------------------------------------- 
    DO 969 K=1,LM
        DO 969 I=1,IM
                 F(I,K) =  H44194M2   * (   APCM(9)    *   X(I,K)  + BPCM (9)    * Y(I,K))
                FF(I,K) =  H44194M2   * (  ATPCM(9)    *   X(I,K)  + BTPCM(9)    * Y(I,K))
                AG(I,K) = (H1P41819   +        F(I,K)) *   F(I,K)  + ONE
               AGG(I,K) = (H1P41819   +       FF(I,K)) *  FF(I,K)  + ONE
            PHITMP(I,K) =  VAR1(I,K)  * ((((  AG (I,K) *  AG(I,K)) ** 2) ** 2) ** 2)
            PSITMP(I,K) =  VAR2(I,K)  * (((( AGG(I,K)  * AGG(I,K)) ** 2) ** 2) ** 2)
969 END DO
!--------------------------------------------------------------- 
! OBTAIN OPTICAL PATH,MEAN PRESSURE FROM THE TOP TO THE PRESSURE
! P(K) (TOPM,TOPPHI)
!--------------------------------------------------------------- 
    DO 970 I=1,IM
          TOPM(I,1) = PHITMP(I,1)
        TOPPHI(I,1) = PSITMP(I,1)
970 END DO
!
    DO 972 K=2,LM
        DO 971 I=1,IM
              TOPM(I,K) =   TOPM(I,K-1) + PHITMP(I,K)
            TOPPHI(I,K) = TOPPHI(I,K-1) + PSITMP(I,K)
    971 END DO
972 END DO
!----------------------------------------------- 
! TT IS THE CLOUD-FREE CTS TRANSMISSION FUNCTION
!----------------------------------------------- 
    DO 973 K=1,LM
        DO 973 I=1,IM
            FAC1(I,K)   =  ACOMB(9)    *   TOPM(I,K)
            FAC2(I,K)   =   FAC1(I,K)  *   TOPM(I,K)   / (BCOMB(9)  * TOPPHI(I,K))
              TT(I,K)   =    EXP(HM1EZ * ( FAC1(I,K)   /   SQRT(ONE +   FAC2(I,K))                &
    &                   + BETACM(9)    * TOTVO2(I,K+1) * SKO2D))    * CO2SP1(I,K+1)
!            
            CTMP(I,K+1) =     TT(I,K)  * CLDFAC(I,K+1,1)
973 END DO
!--------------------------------------------------------------- 
! EXCTS IS THE CTS COOLING RATE ACCUMULATED OVER FREQUENCY BANDS
!--------------------------------------------------------------- 
    DO 974 K=1,LM
        DO 974 I=1,IM
            EXCTS(I,K) = EXCTS(I,K) + SORC(I,K, 9) * (CTMP(I,K+1) - CTMP(I,K))
974 END DO
!----------------------------------------------------------------- 
! GXCTS IS THE EXACT CTS TOP FLUX ACCUMULATED OVER FREQUENCY BANDS
!----------------------------------------------------------------- 
    DO 975 I=1,IM
        GXCTS(I) = GXCTS(I)     + CLDFAC(I,LP1, 1) * (  TT(I,LM)     *  SORC(I,LM,9)              &
    &            + (HAF         *   DELP(I,LM)     * (  TT(I,LM1)    * (   P(I,LP1)               &
    &            - PRESS(I,LM)) +     TT(I,LM)     * (   P(I,LP1)    + PRESS(I,LM)                & 
    &            - TWO          *      P(I,LM))))  * (SORC(I,LP1,9)  -  SORC(I,LM,9)))
975 END DO
!----------------------------------------------------- 
! CALCULATION FOR BAND 10 (670-800 CM-1; INCLUDES CO2)
!----------------------------------------------------- 
!------------------------------------------------------------------------------------------- 
! OBTAIN TEMPERATURE CORRECTION (CAPPHI,CAPPSI),THEN MULTIPLY BY OPTICAL PATH (VAR1,VAR2) TO 
! COMPUTE TEMPERATURE-CORRECTED OPTICAL PATH AND MEAN PRESSURE FOR A LAYER (PHITMP,PSITMP)
!------------------------------------------------------------------------------------------- 
    DO 976 K=1,LM
        DO 976 I=1,IM
                 F(I,K) =  H44194M2   * (   APCM(10)   *   X(I,K) +  BPCM(10)    * Y(I,K))
                FF(I,K) =  H44194M2   * (  ATPCM(10)   *   X(I,K) + BTPCM(10)    * Y(I,K))
                AG(I,K) = (H1P41819   +        F(I,K)) *   F(I,K) + ONE
               AGG(I,K) = (H1P41819   +       FF(I,K)) *  FF(I,K) + ONE
            PHITMP(I,K) =  VAR1(I,K)  * ((((  AG (I,K) *  AG(I,K)) ** 2) ** 2) ** 2)
            PSITMP(I,K) =  VAR2(I,K)  * (((( AGG(I,K)  * AGG(I,K)) ** 2) ** 2) ** 2)
976 END DO
!--------------------------------------------------------------- 
! OBTAIN OPTICAL PATH,MEAN PRESSURE FROM THE TOP TO THE PRESSURE
! P(K) (TOPM,TOPPHI)
!--------------------------------------------------------------- 
    DO 977 I=1,IM
          TOPM(I,1) = PHITMP(I,1)
        TOPPHI(I,1) = PSITMP(I,1)
977 END DO
!
    DO 979 K=2,LM
        DO 978 I=1,IM
              TOPM(I,K) =   TOPM(I,K-1) + PHITMP(I,K)
            TOPPHI(I,K) = TOPPHI(I,K-1) + PSITMP(I,K)
    978 END DO
979 END DO
!-----------------------------------------------
! TT IS THE CLOUD-FREE CTS TRANSMISSION FUNCTION
!-----------------------------------------------
    DO 980 K=1,LM
        DO 980 I=1,IM
             FAC1(I,K) = ACOMB(10)   *   TOPM(I,K)
             FAC2(I,K) =  FAC1(I,K)  *   TOPM(I,K)  / (BCOMB(10) * TOPPHI(I,K))
               TT(I,K) =   EXP(HM1EZ *  (FAC1(I,K)  /   SQRT(ONE +   FAC2(I,K))                   &
    &                  + BETACM(10)  * TOTVO2(I,K+1) * SKO2D))   * CO2SP2(I,K+1)
!              
            CTMP(I,K+1)= TT(I,K) * CLDFAC(I,K+1,1)
980 END DO
!--------------------------------------------------------------- 
! EXCTS IS THE CTS COOLING RATE ACCUMULATED OVER FREQUENCY BANDS
!--------------------------------------------------------------- 
    DO 981 K=1,LM
        DO 981 I=1,IM
            EXCTS(I,K) = EXCTS(I,K) + SORC(I,K,10) * (CTMP(I,K+1) - CTMP(I,K))
981 END DO
!----------------------------------------------------------------- 
! GXCTS IS THE EXACT CTS TOP FLUX ACCUMULATED OVER FREQUENCY BANDS
!----------------------------------------------------------------- 
    DO 982 I=1,IM
        GXCTS(I) = GXCTS(I)     + CLDFAC(I,LP1,1) * (  TT(I,LM)     *  SORC(I,LM,10)              &
    &             +(HAF         *   DELP(I,LM)    * (  TT(I,LM1)    * (   P(I,LP1)                &
    &             -PRESS(I,LM)) +     TT(I,LM)    * (   P(I,LP1)    + PRESS(I,LM)                 &
    &             -TWO          *      P(I,LM)))) * (SORC(I,LP1,10) -  SORC(I,LM,10)))
982 END DO
!--------------------------------------- 
! CALCULATION FOR BAND 11 (800-900 CM-1)
!--------------------------------------- 
!------------------------------------------------------------------------------------------- 
! OBTAIN TEMPERATURE CORRECTION (CAPPHI,CAPPSI),THEN MULTIPLY BY OPTICAL PATH (VAR1,VAR2) TO 
! COMPUTE TEMPERATURE-CORRECTED OPTICAL PATH AND MEAN PRESSURE FOR A LAYER (PHITMP,PSITMP)
!------------------------------------------------------------------------------------------- 
    DO 983 K=1,LM
        DO 983 I=1,IM
                 F(I,K) =  H44194M2  * ( APCM(11)   *   X(I,K)  +  BPCM(11)   * Y(I,K))
                FF(I,K) =  H44194M2  * (ATPCM(11)   *   X(I,K)  + BTPCM(11)   * Y(I,K))
                AG(I,K) = (H1P41819  +      F(I,K)) *   F(I,K)  + ONE
               AGG(I,K) = (H1P41819  +     FF(I,K)) *  FF(I,K)  + ONE
            PHITMP(I,K) =  VAR1(I,K) *(((( AG(I,K)  *  AG(I,K)) ** 2) ** 2) ** 2)
            PSITMP(I,K) =  VAR2(I,K) *(((( AGG(I,K) * AGG(I,K)) ** 2) ** 2) ** 2)
983 END DO
!--------------------------------------------------------------- 
! OBTAIN OPTICAL PATH,MEAN PRESSURE FROM THE TOP TO THE PRESSURE
! P(K) (TOPM,TOPPHI)
!--------------------------------------------------------------- 
    DO 984 I=1,IM
          TOPM(I,1) = PHITMP(I,1)
        TOPPHI(I,1) = PSITMP(I,1)
984 END DO
!
    DO 986 K=2,LM
        DO 985 I=1,IM
              TOPM(I,K) =  TOPM(I,K-1) + PHITMP(I,K)
            TOPPHI(I,K) =TOPPHI(I,K-1) + PSITMP(I,K)
    985 END DO
986 END DO
!----------------------------------------------- 
! TT IS THE CLOUD-FREE CTS TRANSMISSION FUNCTION
!----------------------------------------------- 
    DO 987 K=1,LM
        DO 987 I=1,IM
            FAC1(I,K) =  ACOMB(11)    *   TOPM(I,K)
            FAC2(I,K) =   FAC1(I,K)   *   TOPM(I,K)   / (BCOMB(11) * TOPPHI(I,K))
              TT(I,K) =    EXP(HM1EZ  * ( FAC1(I,K)   /   SQRT(ONE +   FAC2(I,K))                 &
    &                 + BETACM(11)    * TOTVO2(I,K+1) * SKO2D))
!              
            CTMP(I,K+1) =   TT(I,K)   * CLDFAC(I,K+1,1)
987 END DO
!--------------------------------------------------------------- 
! EXCTS IS THE CTS COOLING RATE ACCUMULATED OVER FREQUENCY BANDS
!--------------------------------------------------------------- 
    DO 988 K=1,LM
        DO 988 I=1,IM
            EXCTS(I,K) = EXCTS(I,K) + SORC(I,K,11) * (CTMP(I,K+1) - CTMP(I,K))
988 END DO
!----------------------------------------------------------------- 
! GXCTS IS THE EXACT CTS TOP FLUX ACCUMULATED OVER FREQUENCY BANDS
!----------------------------------------------------------------- 
    DO 989 I=1,IM
        GXCTS(I) = GXCTS(I)     + CLDFAC(I,LP1,1) * (  TT(I,LM)     *  SORC(I,LM,11)              &
    &            + (HAF         *   DELP(I,LM)    * (  TT(I,LM1)    * (   P(I,LP1)                &
    &            - PRESS(I,LM)) +     TT(I,LM)    * (   P(I,LP1)    + PRESS(I,LM)                 &
    &            - TWO          *      P(I,LM)))) * (SORC(I,LP1,11) -  SORC(I,LM,11)))
989 END DO
!--------------------------------------- 
! CALCULATION FOR BAND 12 (900-990 CM-1)
!--------------------------------------- 
!------------------------------------------------------------------------------------------- 
! OBTAIN TEMPERATURE CORRECTION (CAPPHI,CAPPSI),THEN MULTIPLY BY OPTICAL PATH (VAR1,VAR2) TO 
! COMPUTE TEMPERATURE-CORRECTED OPTICAL PATH AND MEAN PRESSURE FOR A LAYER (PHITMP,PSITMP)
!------------------------------------------------------------------------------------------- 
    DO 990 K=1,LM
        DO 990 I=1,IM
                 F(I,K) =  H44194M2  * (  APCM(12)   *   X(I,K) +  BPCM(12)    * Y(I,K))
                FF(I,K) =  H44194M2  * ( ATPCM(12)   *   X(I,K) + BTPCM(12)    * Y(I,K))
                AG(I,K) = (H1P41819  +       F(I,K)) *   F(I,K) + ONE
               AGG(I,K) = (H1P41819  +      FF(I,K)) *  FF(I,K) + ONE
            PHITMP(I,K) =  VAR1(I,K) *((((  AG(I,K)  *  AG(I,K)) ** 2) ** 2) ** 2)
            PSITMP(I,K) =  VAR2(I,K) *(((( AGG(I,K)  * AGG(I,K)) ** 2) ** 2) ** 2)
990 END DO
!--------------------------------------------------------------- 
! OBTAIN OPTICAL PATH,MEAN PRESSURE FROM THE TOP TO THE PRESSURE
! P(K) (TOPM,TOPPHI)
!--------------------------------------------------------------- 
    DO 991 I=1,IM
          TOPM(I,1) = PHITMP(I,1)
        TOPPHI(I,1) = PSITMP(I,1)
991 END DO
!
    DO 993 K=2,LM
        DO 992 I=1,IM
              TOPM(I,K) =   TOPM(I,K-1) + PHITMP(I,K)
            TOPPHI(I,K) = TOPPHI(I,K-1) + PSITMP(I,K)
    992 END DO
993 END DO
!----------------------------------------------- 
! TT IS THE CLOUD-FREE CTS TRANSMISSION FUNCTION
!----------------------------------------------- 
    DO 994 K=1,LM
        DO 994 I=1,IM
            FAC1(I,K)   =  ACOMB(12)   *   TOPM(I,K)
            FAC2(I,K)   =   FAC1(I,K)  *   TOPM(I,K)   / (BCOMB(12) * TOPPHI(I,K))
              TT(I,K)   =    EXP(HM1EZ * ( FAC1(I,K)   /   SQRT(ONE +   FAC2(I,K))                &
    &                   + BETACM(12)   * TOTVO2(I,K+1) * SKO2D))
!              
            CTMP(I,K+1) =     TT(I,K)  * CLDFAC(I,K+1,1)
994 END DO
!---------------------------------------------------------------  
! EXCTS IS THE CTS COOLING RATE ACCUMULATED OVER FREQUENCY BANDS
!---------------------------------------------------------------  
    DO 995 K=1,LM
        DO 995 I=1,IM
            EXCTS(I,K) = EXCTS(I,K) + SORC(I,K,12) * (CTMP(I,K+1) - CTMP(I,K))
995 END DO
!-----------------------------------------------------------------
! GXCTS IS THE EXACT CTS TOP FLUX ACCUMULATED OVER FREQUENCY BANDS
!-----------------------------------------------------------------
    DO 996 I=1,IM
        GXCTS(I) = GXCTS(I)     + CLDFAC(I,LP1,1) * (  TT(I,LM)     * SORC(I,LM,12)                &
    &            + (HAF         *   DELP(I,LM)    * (  TT(I,LM1)    * (  P(I,LP1)                  &
    &            - PRESS(I,LM)) +     TT(I,LM)    * (   P(I,LP1)    + PRESS(I,LM)                  &
    &            - TWO          *      P(I,LM)))) * (SORC(I,LP1,12) -  SORC(I,LM,12)))
996 END DO
!------------------------------------------------------ 
! CALCULATION FOR BAND 13 (990-1070 CM-1; INCLUDES O3))
!------------------------------------------------------ 
!--------------------------------------------------------------------------------------------- 
! OBTAIN TEMPERATURE CORRECTION (CAPPHI,CAPPSI),THEN MULTIPLY BY OPTICAL PATH (VAR1,VAR2) TO  
! COMPUTE TEMPERATURE-CORRECTED !   OPTICAL PATH AND MEAN PRESSURE FOR A LAYER (PHITMP,PSITMP)
!--------------------------------------------------------------------------------------------- 
    DO 997 K=1,LM
        DO 997 I=1,IM
                 F(I,K) =  H44194M2  *(   APCM(13)   *   X(I,K)  + BPCM(13)    * Y(I,K))
                FF(I,K) =  H44194M2  *(  ATPCM(13)   *   X(I,K)  + BTPCM(13)   * Y(I,K))
                AG(I,K) = (H1P41819  +       F(I,K)) *   F(I,K)  + ONE
               AGG(I,K) = (H1P41819  +      FF(I,K)) *  FF(I,K)  + ONE
            PHITMP(I,K) =  VAR1(I,K) * (((( AG(I,K)  *  AG(I,K)) ** 2) ** 2) ** 2)
            PSITMP(I,K) =  VAR2(I,K) * (((( AGG(I,K) * AGG(I,K)) ** 2) ** 2) ** 2)
997 END DO
!--------------------------------------------------------------- 
! OBTAIN OPTICAL PATH,MEAN PRESSURE FROM THE TOP TO THE PRESSURE
! P(K) (TOPM,TOPPHI)
!--------------------------------------------------------------- 
    DO 998 I=1,IM
          TOPM(I,1) = PHITMP(I,1)
        TOPPHI(I,1) = PSITMP(I,1)
998 END DO
!
    DO 831 K=2,LM
        DO 830 I=1,IM
              TOPM(I,K) =   TOPM(I,K-1) + PHITMP(I,K)
            TOPPHI(I,K) = TOPPHI(I,K-1) + PSITMP(I,K)
    830 END DO
831 END DO
!----------------------------------------------- 
! TT IS THE CLOUD-FREE CTS TRANSMISSION FUNCTION
!-----------------------------------------------  
    DO 832 K=1,LM
        DO 832 I=1,IM
            FAC1(I,K)   =  ACOMB(13)   *   TOPM(I,K)
            FAC2(I,K)   =   FAC1(I,K)  *   TOPM(I,K)   / (BCOMB(13) * TOPPHI(I,K))
              TT(I,K)   =    EXP(HM1EZ * ( FAC1(I,K)   /   SQRT(ONE + FAC2(I,K))                  &
    &                   + BETACM(13)   * TOTVO2(I,K+1) * SKO2D      + TO3SPC(I,K)))
!              
            CTMP(I,K+1) =     TT(I,K)  * CLDFAC(I,K+1,1)
832 END DO
!--------------------------------------------------------------- 
! EXCTS IS THE CTS COOLING RATE ACCUMULATED OVER FREQUENCY BANDS
!--------------------------------------------------------------- 
    DO 833 K=1,LM
        DO 833 I=1,IM
            EXCTS(I,K) = EXCTS(I,K) + SORC(I,K,13) * (CTMP(I,K+1) - CTMP(I,K))
833 END DO
!-----------------------------------------------------------------  
! GXCTS IS THE EXACT CTS TOP FLUX ACCUMULATED OVER FREQUENCY BANDS
!-----------------------------------------------------------------  
    DO 834 I=1,IM
        GXCTS(I) = GXCTS(I)     + CLDFAC(I,LP1,1) * (  TT(I,LM)     *  SORC(I,LM,13)               &
    &            + (HAF         *   DELP(I,LM)    * (  TT(I,LM1)    * (   P(I,LP1)                 &
    &            - PRESS(I,LM)) +     TT(I,LM)    * (   P(I,LP1)    + PRESS(I,LM)                  &
    &            - TWO          *      P(I,LM)))) * (SORC(I,LP1,13) -  SORC(I,LM,13)))
834 END DO
!----------------------------------------- 
! CALCULATION FOR BAND 14 (1070-1200 CM-1)
!----------------------------------------- 
!------------------------------------------------------------------------------------------- 
! OBTAIN TEMPERATURE CORRECTION (CAPPHI,CAPPSI),THEN MULTIPLY BY OPTICAL PATH (VAR1,VAR2) TO  
! COMPUTE TEMPERATURE-CORRECTED OPTICAL PATH AND MEAN PRESSURE FOR A LAYER (PHITMP,PSITMP)
!------------------------------------------------------------------------------------------- 
    DO 835 K=1,LM
        DO 835 I=1,IM
                 F(I,K) = H44194M2   * (   APCM(14)   *   X(I,K)  + BPCM(14)    * Y(I,K))
                FF(I,K) = H44194M2   * (  ATPCM(14)   *   X(I,K)  + BTPCM(14)   * Y(I,K))
                AG(I,K) = (H1P41819  +        F(I,K)) *   F(I,K)  + ONE
               AGG(I,K) = (H1P41819  +       FF(I,K)) *  FF(I,K)  + ONE
            PHITMP(I,K) =  VAR1(I,K) * ((((  AG(I,K)  *  AG(I,K)) ** 2) ** 2) ** 2)
            PSITMP(I,K) =  VAR2(I,K) * (((( AGG(I,K)  * AGG(I,K)) ** 2) ** 2) ** 2)
835 END DO
!--------------------------------------------------------------- 
! OBTAIN OPTICAL PATH,MEAN PRESSURE FROM THE TOP TO THE PRESSURE
! P(K) (TOPM,TOPPHI)
!--------------------------------------------------------------- 
    DO 836 I=1,IM
          TOPM(I,1) = PHITMP(I,1)
        TOPPHI(I,1) = PSITMP(I,1)
836 END DO
!
    DO 838 K=2,LM
        DO 837 I=1,IM
              TOPM(I,K) =  TOPM(I,K-1) + PHITMP(I,K)
            TOPPHI(I,K) =TOPPHI(I,K-1) + PSITMP(I,K)
    837 END DO
838 END DO
!----------------------------------------------- 
! TT IS THE CLOUD-FREE CTS TRANSMISSION FUNCTION
!----------------------------------------------- 
    DO 839 K=1,LM
        DO 839 I=1,IM
            FAC1(I,K)   =  ACOMB(14)   *   TOPM(I,K)
            FAC2(I,K)   =   FAC1(I,K)  *   TOPM(I,K)   / (BCOMB(14) * TOPPHI(I,K))
              TT(I,K)   =    EXP(HM1EZ * ( FAC1(I,K)   /   SQRT(ONE +   FAC2(I,K))                &
    &                   + BETACM(14)   * TOTVO2(I,K+1) * SKO2D))
!              
            CTMP(I,K+1) =     TT(I,K)  * CLDFAC(I,K+1,1)
839 END DO
!--------------------------------------------------------------- 
! EXCTS IS THE CTS COOLING RATE ACCUMULATED OVER FREQUENCY BANDS
!--------------------------------------------------------------- 
    DO 840 K=1,LM
        DO 840 I=1,IM
            EXCTS(I,K) = EXCTS(I,K) + SORC(I,K,14) * (CTMP(I,K+1) - CTMP(I,K))
840 END DO
!----------------------------------------------------------------- 
! GXCTS IS THE EXACT CTS TOP FLUX ACCUMULATED OVER FREQUENCY BANDS
!----------------------------------------------------------------- 
    DO 841 I=1,IM
        GXCTS(I) = GXCTS(I)     + CLDFAC(I,LP1,1) * (  TT(I,LM)     *  SORC(I,LM,14)               &
    &            + (HAF         *   DELP(I,LM)    * (  TT(I,LM1)    * (   P(I,LP1)                 &
    &            - PRESS(I,LM)) +     TT(I,LM)    * (   P(I,LP1)    + PRESS(I,LM)                  &
    &            - TWO          *      P(I,LM)))) * (SORC(I,LP1,14) -  SORC(I,LM,14)))
841 END DO
!--------------------------------------------------------------------------------------------------
! OBTAIN CTS FLUX AT THE TOP BY INTEGRATION OF HEATING RATES AND USING CTS FLUX AT THE BOTTOM  
! (CURRENT VALUE OF GXCTS). NOTE THAT THE PRESSURE QUANTITIES AND CONVERSION FACTORS HAVE NOT
! BEEN INCLUDED EITHER IN EXCTS OR IN GXCTS. THESE CANCEL OUT, THUS REDUCING COMPUTATIONS
!--------------------------------------------------------------------------------------------------
    DO 842 K=1,LM
        DO 842 I=1,IM
            GXCTS(I) = GXCTS(I) - EXCTS(I,K)
842 END DO
!--------------------------------------------------------------------------------------------------
! NOW SCALE THE COOLING RATE (EXCTS) BY INCLUDING THE PRESSURE FACTOR (DELP) AND THE CONVERSION 
! FACTOR (RADCON)
!--------------------------------------------------------------------------------------------------
    DO 843 K=1,LM
        DO 843 I=1,IM
            EXCTS(I,K) = EXCTS(I,K) * RADCON * DELP(I,K)
843 END DO
!---------------------------------------------------------------------------------------------- 
! THIS IS THE END OF THE EXACT CTS COMPUTATIONS; AT THIS POINT EXCTS HAS ITS APPROPRIATE VALUE.
!---------------------------------------------------------------------------------------------- 
!------------------------------------------------------------------------ 
! COMPUTE APPROXIMATE CTS HEATING RATES FOR 15UM AND 9.6 UM BANDS (CTSO3)
!------------------------------------------------------------------------ 
    DO 844 K=1,LM
        DO 844 I=1,IM
            CTMP2(I,K+1) = CO2SP(I,K+1) * CLDFAC(I,K+1,1)
            CTMP3(I,K+1) = TO3SP(I,K)   * CLDFAC(I,K+1,1)
844 END DO
!
    DO 845 K=1,LM
        DO 845 I=1,IM
           CTSO3(I,K) = RADCON       *   DELP(I,K)   * (CSOUR(I,K) * (CTMP2(I,K+1) - CTMP2(I,K))  &
    &                 + SORC(I,K,13) * (CTMP3(I,K+1) -  CTMP3(I,K)))
845 END DO
!
    RETURN
!
    END SUBROUTINE SPA88
