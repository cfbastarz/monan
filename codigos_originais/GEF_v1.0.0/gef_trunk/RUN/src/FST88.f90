!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief IS THE MAIN COMPUTATION MODULE OF THE LONG-WAVE RADIATION CODE
!> @details IN IT ALL "EMISSIVITY" CALCULATIONS, INCLUDING CALLS TO TABLE LOOKUP SUBROUTINES. 
!! ALSO, AFTER CALLING SUBROUTINE "SPA88", FINAL COMBINED HEATING RATES AND GROUND FLUX ARE OBTAINED
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
!> @param[in] QH2O   - H2O MASS MIXING RATIO,MULTIPLIED BY THE DIFFUSIVITY FACTOR (DIFFCTR)
!> @param[in] PRESS  - PRESSURE
!> @param[in] P      - PRESSURE AT FLUX LEVELS OF MODEL
!> @param[in] DELP   - INVERSE OF DELP2 
!> @param[in] DELP2  - PRESSURE DIFFERENCE BETWEEN FLUX LEVELS
!> @param[in] TEMP   - TEMPERATURE
!> @param[in] T      - TEMPERATURE AT FLUX LEVELS OF MODEL
!> @param[in] CLDFAC - CLOUD TRANSMISSION FUNCTIONS FOR THE LONGWAVE CODE
!> @param[in] NCLDS  - NUMBER OF CLOUD LAYERS
!> @param[in] KTOP   - HEIGHT OF CLOUD TOP OF EACH CLOUD LAYER (IN ETA LEVELS)
!> @param[in] KBTM   - BOTTOM OF EACH CLOUD LAYER
!> @param[in] CAMT   - CLOUD FRACTION OF EACH CLOUD LAYER
!> @param[in] CO21   - TRANSMISSION FCTN FOR THE 560-800 CM-1 BAND (AS 1 BAND). 
!!          INCLUDES CO2 (IN LWRAD) AND H2O(L+C) AFTER MULTIPLICATION WITH "OVER" IN FST88 
!> @param[in] CO2NBL - CO2 TRANS. FCTNS. (NOT PRESSURE-INTEGRATED) FOR ADJACENT LEVELS
!!          OVER THE 560-800 CM-1 RANGE.
!> @param[in] CO2SP1 - CO2 TRANS. FCTNS. (NOT PRESSURE-INTEGRATED) BET. A FLUX LEVEL AND SPACE
!!          FOR THE 560-670 CM-1 RANGE. USED FOR EXACT CTS CALCS. 
!> @param[in] CO2SP2 - SAME AS CO2SP1, BUT FOR THE 670-800 CM-1 RANGE. 
!> @param[in] VAR1   - H2O OPTICAL PATH IN MODEL LAYERS (BETWEEN FLUX LEVELS)
!> @param[in] VAR2   - PRESSURE-WEIGHTED H2O OPTICAL PATH IN MODEL LAYERS
!> @param[in] VAR3   - O3 OPTICAL PATH IN MODEL LAYERS 
!> @param[in] VAR4   - PRESSURE-WEIGHTED O3 OPTICAL PATH IN MODEL LAYERS
!> @param[in] CNTVAL - H2O CONTINUUM PATH IN MODEL LAYERS FOR THE 800-990 AND 1070-1200 CM-1 COMBINED BAND
!> @param[in] TOTO3  - SUMMED OPTICAL PATHS FOR O3
!> @param[in] TPHIO3 - 
!> @param[in] TOTPHI - SUMMED OPTICAL PATHS FOR H2O
!> @param[in] TOTVO2 - SUMMED OPTICAL PATHS FOR H2O CONTINUUM
!> @param[in] EMX1   - ADDITIONAL PRESSURE-SCALED MASS FROM PRESS(L) TO P(L). 
!!          IT IS USED IN NEARBY LAYER AND EMISS CALCULATIONS.
!> @param[in] EMX2   - ADDITIONAL PRESSURE-SCALED MASS FROM PRESS(L) TO P(LP1). 
!!          IT IS USED IN CALCULATIONS BETWEEN FLUX LEVELS L AND LP1.
!> @param[in] EMPL   - H2O AMOUNT,INPUT FOR E3 CALCULATION IN E3V88 (COMPUTED IN LWR88; STORED IN KDACOM.H)
!> @param[out] HEATRA - TOTAL HEATING RATES
!> @param[out] GRNFLX - SURFACE VALUE OF FLUX
!> @param[out] TOPFLX - FLUX AT THE TOP 
!> @details <b>Use Module:</b>
!! @arg @c F77KINDS
!! @arg @c HCON
!! @arg @c MPPSTAFF
!! @arg @c PARMETA
!! @arg @c RDPARM
!! @arg @c RNDDTA
!! @arg @c TABCOM
!> @details <b>Driver:</b> 
!! @arg @c LWR88
!> @details <b>Calls:</b>
!! @arg @c E1E290
!! @arg @c E290
!! @arg @c SPA88
!! @arg @c E3V88
!! @arg @c E2SPEC
!--------------------------------------------------------------------------------------------------
    SUBROUTINE FST88(HEATRA, GRNFLX, TOPFLX, QH2O  , PRESS  , P     , DELP  , DELP2 , TEMP  ,     &
    &                T     , CLDFAC, NCLDS , KTOP  , KBTM   , CAMT  , CO21  , CO2NBL, CO2SP1,     &
    &                CO2SP2, VAR1  , VAR2  , VAR3  , VAR4   , CNTVAL, TOTO3 , TPHIO3, TOTPHI,     &
    &                TOTVO2, EMX1  , EMX2  , EMPL)
!--------------------------------------------------------------------------------------------------  
! SUBROUTINE FST88
! 
! SUBPROGRAM: FST88 - IS THE MAIN COMPUTATION MODULE OF THE LONG-WAVE RADIATION CODE. 
!
! PROGRAMMER: ?????
! ORG: ?????
! DATE: ??-??-??
!
! ABSTRACT:
! IN IT ALL "EMISSIVITY" CALCULATIONS, INCLUDING CALLS TO TABLE LOOKUP SUBROUTINES. 
! ALSO, AFTER CALLING SUBROUTINE "SPA88", FINAL COMBINED HEATING RATES AND GROUND FLUX ARE OBTAINED
!
! PROGRAM HISTORY LOG:
! ??-??-??  ?????  - ORIGINATOR
! 18-01-15  LUCCI  - MODERNIZATION OF THE CODE, INCLUDING:
!                    * F77 TO F90/F95
!                    * INDENTATION & UNIFORMIZATION CODE
!                    * REPLACEMENT OF COMMONS BLOCK FOR MODULES
!                    * DOCUMENTATION WITH DOXYGEN
!                    * OPENMP FUNCTIONALITY
!
! INPUT ARGUMENT LIST:
! QH2O   - H2O MASS MIXING RATIO,MULTIPLIED BY THE DIFFUSIVITY FACTOR (DIFFCTR)
! PRESS  - PRESSURE
! P      - PRESSURE AT FLUX LEVELS OF MODEL
! DELP   - INVERSE OF DELP2 
! DELP2  - PRESSURE DIFFERENCE BETWEEN FLUX LEVELS
! TEMP   - TEMPERATURE
! T      - TEMPERATURE AT FLUX LEVELS OF MODEL
! CLDFAC - CLOUD TRANSMISSION FUNCTIONS FOR THE LONGWAVE CODE
! NCLDS  - NUMBER OF CLOUD LAYERS
! KTOP   - HEIGHT OF CLOUD TOP OF EACH CLOUD LAYER (IN ETA LEVELS)
! KBTM   - BOTTOM OF EACH CLOUD LAYER
! CAMT   - CLOUD FRACTION OF EACH CLOUD LAYER
! CO21   - TRANSMISSION FCTN FOR THE 560-800 CM-1 BAND (AS 1 BAND). 
!          INCLUDES CO2 (IN LWRAD) AND H2O(L+C) AFTER MULTIPLICATION WITH "OVER" IN FST88 
! CO2NBL - CO2 TRANS. FCTNS. (NOT PRESSURE-INTEGRATED) FOR ADJACENT LEVELS
!          OVER THE 560-800 CM-1 RANGE.
! CO2SP1 - CO2 TRANS. FCTNS. (NOT PRESSURE-INTEGRATED) BET. A FLUX LEVEL AND SPACE
!          FOR THE 560-670 CM-1 RANGE. USED FOR EXACT CTS CALCS. 
! CO2SP2 - SAME AS CO2SP1, BUT FOR THE 670-800 CM-1 RANGE. 
! VAR1   - H2O OPTICAL PATH IN MODEL LAYERS (BETWEEN FLUX LEVELS)
! VAR2   - PRESSURE-WEIGHTED H2O OPTICAL PATH IN MODEL LAYERS
! VAR3   - O3 OPTICAL PATH IN MODEL LAYERS 
! VAR4   - PRESSURE-WEIGHTED O3 OPTICAL PATH IN MODEL LAYERS
! CNTVAL - H2O CONTINUUM PATH IN MODEL LAYERS FOR THE 800-990 AND 1070-1200 CM-1 COMBINED BAND
! TOTO3  - SUMMED OPTICAL PATHS FOR O3
! TPHIO3 - 
! TOTPHI - SUMMED OPTICAL PATHS FOR H2O
! TOTVO2 - SUMMED OPTICAL PATHS FOR H2O CONTINUUM
! EMX1   - ADDITIONAL PRESSURE-SCALED MASS FROM PRESS(L) TO P(L). 
!          IT IS USED IN NEARBY LAYER AND EMISS CALCULATIONS.
! EMX2   - ADDITIONAL PRESSURE-SCALED MASS FROM PRESS(L) TO P(LP1). 
!          IT IS USED IN CALCULATIONS BETWEEN FLUX LEVELS L AND LP1.
! EMPL   - H2O AMOUNT,INPUT FOR E3 CALCULATION IN E3V88 (COMPUTED IN LWR88; STORED IN KDACOM.H)
!
! OUTPUT ARGUMENT LIST:
! HEATRA - TOTAL HEATING RATES
! GRNFLX - SURFACE VALUE OF FLUX
! TOPFLX - FLUX AT THE TOP 
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
!              TABCOM
!
! DRIVER     : LWR88
!
! CALLS      : E1E290
!              E290
!              SPA88
!              E3V88
!              E2SPEC
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
    USE HCON
    USE MPPSTAFF
    USE PARMETA
    USE RDPARM
    USE RNDDTA
    USE TABCOM
!
    IMPLICIT NONE
!
    INTEGER(KIND=I4)    , DIMENSION(IM)                                   , INTENT(IN)          ::&
    & NCLDS 
!
    INTEGER(KIND=I4)    , DIMENSION(IM)                                                         ::&
    & ITOP    , IBOT    , INDTC
!
    REAL   (KIND=R4)    , DIMENSION(IM)                                   , INTENT(IN)          ::&
    & EMX1    , EMX2 
!
    REAL   (KIND=R4)    , DIMENSION(IM)                                                         ::&
    & GXCTS   , FLX1E1  , DELPTC  , PTOP    , PBOT    , FTOP    , FBOT    
!
    REAL   (KIND=R4)    , DIMENSION(IM)                                   , INTENT(OUT)         ::&
    & GRNFLX  , TOPFLX 
!
    REAL   (KIND=R4)    , DIMENSION(IM, 2)                                                      ::&
    & EMSPEC
!
    REAL   (KIND=R4)    , DIMENSION(IM, LM)                               , INTENT(OUT)         ::&
    & HEATRA
!
    REAL   (KIND=R4)    , DIMENSION(IM, LM)                               , INTENT(IN)          ::&
    & CO2NBL  , VAR1    , VAR2    , VAR3    , VAR4     
!
    REAL   (KIND=R4)    , DIMENSION(IM, LM)                                                     ::&
    & TO3SPC  , CTS     , EXCTS   , CTSO3       
!
    REAL   (KIND=R4)    , DIMENSION(IM, LM)                               , INTENT(IN)          ::&
    & DELP    , DELP2   
!
    INTEGER(KIND=I4)    , DIMENSION(IM, LP1)                              , INTENT(IN)          ::&
    & KTOP    , KBTM     
!
    REAL   (KIND=R4)    , DIMENSION(IM, LP1)                              , INTENT(IN)          ::&
    & QH2O    , PRESS   , P       , TEMP    , T       ,  CAMT   , CO2SP1  , CO2SP2  ,             &
    & CNTVAL  , TOTO3   , TPHIO3  , TOTPHI  , TOTVO2      
!
    REAL   (KIND=R4)    , DIMENSION(IM, LP1)                                                    ::&
    & AVEPHI  , EMISS   , EMISSB  , AVPHO3  , TO31D   , CONT1D  , AVMO3   , OVER1D   ,            &
    & E1FLX   , CO2SP   , TO3SP   , OSS     , CSS     , SS1     , SS2     , TC       ,            &
    & DTC     , CSOUR   , AVVO2   , HEATEM
!
    REAL   (KIND=R4)    , DIMENSION(IM, LLP1)                             , INTENT(IN)          ::&
    & EMPL    
!
    REAL   (KIND=R4)    , DIMENSION(IM, LLP1)                                                   ::&
    & C       , C2
!
    REAL   (KIND=R4)    , DIMENSION(IM, LP1, LP1)                         , INTENT(IN)          ::&
    & CLDFAC   
!
    REAL   (KIND=R4)    , DIMENSION(IM, LP1, LP1)                         , INTENT(INOUT)       ::&
    & CO21
!
    REAL   (KIND=R4)    , DIMENSION(IM, LP1, NBLY)                                              ::&
    & SORC
!------------------------------------------------------ 
! DIMENSION OF VARIABLES EQUIVALENCED TO THOSE IN VTEMP
!------------------------------------------------------
    INTEGER(KIND=I4)    , DIMENSION(IM, LP1)                                                    ::&
    & IXO
!
    REAL   (KIND=R4)    , DIMENSION(IM, LP1)                                                    ::&
    & VTMP3   , DSORC   , FAC1    , DELPR1  , DELPR2  , EMISDG  , CONTDG  , TO3DG   ,             &
    & FLXNET  , VSUM1   , FLXTHK  , Z1             
!
    REAL   (KIND=R4)    , DIMENSION(IM, LLP1)                                                   ::&
    & ALP     , CSUB    , CSUB2
!------------------------------------------------------------------------------------ 
! DIMENSION OF VARIABLES PASSED TO OTHER SUBROUTINES (AND NOT FOUND IN COMMON BLOCKS)
!------------------------------------------------------------------------------------ 
    REAL   (KIND=R4)    , DIMENSION(IM, LM)                                                     ::&
    & E1CTS2  , E1CTW2
!
    REAL   (KIND=R4)    , DIMENSION(IM, LP1)                                                    ::&
    & E1CTS1  , E1CTW1
!
    REAL   (KIND=R4)    , DIMENSION(IM, LLP1)                                                   ::&
    & EMD     , TPL
!-------------------------------------------------------------------------------------------------- 
! IT IS POSSIBLE TO EQUIVALENCE EMD,TPL TO THE ABOVE VARIABLES, AS THEY GET CALLED AT DIFFERENT 
! TIMES
!-------------------------------------------------------------------------------------------------- 
    REAL   (KIND=R4)    , DIMENSION(IM,LP1)                                                     ::&
    & FXO     , DT      , FXOE2   , DTE2                                
!
    REAL   (KIND=R4)    , DIMENSION(IM,2)                                                       ::&
    & FXOSP   , DTSP
!----------------------------- 
! DIMENSION OF LOCAL VARIABLES
!----------------------------- 
    REAL   (KIND=R4)    , DIMENSION(IM,LM)                                                      ::&
    & RLOG
!
    REAL   (KIND=R4)    , DIMENSION(IM,LP1)                                                     ::&
    & FLX     , TOTEVV  , CNTTAU
!
    INTEGER(KIND=I4)                                                                            ::&
    & I       , K       , KP      , KLEN    , KK      , ICNT    , KCLDS   , KMIN    , KMAX    ,   &
    & J1      , J3 
!
    EQUIVALENCE (ALP,C,CSUB), (CSUB2,C2)
    EQUIVALENCE (FAC1, DSORC, OVER1D, DELPR2, FLXNET)
    EQUIVALENCE (DELPR1, HEATEM)
    EQUIVALENCE (IXO, AVVO2, FLXTHK, TO3DG)
    EQUIVALENCE (Z1, AVMO3, CONTDG)
    EQUIVALENCE (EMISDG, VSUM1, AVPHO3)
    EQUIVALENCE (EMD(1,1), E1CTS1(1,1)), (EMD(1,LP2), E1CTS2(1,1))
    EQUIVALENCE (TPL(1,1), E1CTW1(1,1)), (TPL(1,LP2), E1CTW2(1,1))
!------------------------------------------------------------------------------- 
! FIRST SECTION IS TABLE LOOKUP FOR SOURCE FUNCTION AND DERIVATIVE (B AND DB/DT)
! ALSO,THE NLTE CO2 SOURCE FUNCTION IS OBTAINED
!
! IN CALCS. BELOW, DECREMENTING THE INDEX BY 9 
! ACCOUNTS FOR THE TABLES BEGINNING AT T=100K.
! AT T=100K.
!------------------------------------------------------------------------------- 
    DO K=1,LP1
        DO I=1,IM
!-----------------------------  
! TEMP. INDICES FOR E1, SOURCE
!-----------------------------  
            VTMP3(I,K) = AINT(TEMP(I,K) * HP1)
              FXO(I,K) =     VTMP3(I,K) - 9.
               DT(I,K) =      TEMP(I,K) - TEN * VTMP3(I,K)
!--------------------------------------------  
! INTEGER INDEX FOR SOURCE (USED IMMEDIATELY)
!-------------------------------------------- 
            IXO(I,K) = FXO(I,K)
        END DO
    END DO
!
    DO K=1,LM
        DO I=1,IM
!----------------------------------------------------------------  
! TEMP. INDICES FOR E2 (KP=1 LAYER NOT USED IN FLUX CALCULATIONS)
!---------------------------------------------------------------- 
            VTMP3(I,K) = AINT(T(I,K+1) * HP1)
            FXOE2(I,K) =  VTMP3(I,K)   - 9.
             DTE2(I,K) =      T(I,K+1) - TEN * VTMP3(I,K)
        END DO
    END DO
!----------------------------------------------------------  
! SPECIAL CASE TO HANDLE KP=LP1 LAYER AND SPECIAL E2 CALCS.
!---------------------------------------------------------- 
    DO I=1,IM
        FXOE2(I,LP1) =   FXO(I,LM)
         DTE2(I,LP1) =    DT(I,LM)
        FXOSP(I,1)   = FXOE2(I,LM1)
        FXOSP(I,2)   =   FXO(I,LM1)
         DTSP(I,1)   =  DTE2(I,LM1)
         DTSP(I,2)   =    DT(I,LM1)
    END DO
!------------------------------------ 
! SOURCE FUNCTION FOR COMBINED BAND 1
!------------------------------------  
    DO I=1,IM
        DO K=1,LP1
            VTMP3(I,K) = SOURCE(IXO(I,K), 1)
            DSORC(I,K) =  DSRCE(IXO(I,K), 1)
        END DO
    END DO
!
    DO K=1,LP1
        DO I=1,IM
            SORC(I,K,1) = VTMP3(I,K) + DT(I,K) * DSORC(I,K)
        END DO
    END DO
!------------------------------------  
! SOURCE FUNCTION FOR COMBINED BAND 2
!------------------------------------
    DO I=1,IM
        DO K=1,LP1
            VTMP3(I,K) = SOURCE(IXO(I,K), 2)
            DSORC(I,K) =  DSRCE(IXO(I,K), 2)
        END DO
    END DO
!
    DO K=1,LP1
        DO I=1,IM
            SORC(I,K,2) = VTMP3(I,K) + DT(I,K) * DSORC(I,K)
        END DO
    END DO
!------------------------------------  
! SOURCE FUNCTION FOR COMBINED BAND 3
!------------------------------------  
    DO I=1,IM
        DO K=1,LP1
            VTMP3(I,K) = SOURCE(IXO(I,K), 3)
            DSORC(I,K) =  DSRCE(IXO(I,K), 3)
        END DO
    END DO
!
    DO K=1,LP1
        DO I=1,IM
            SORC(I,K,3) = VTMP3(I,K) + DT(I,K) * DSORC(I,K)
        END DO
    END DO
!------------------------------------ 
! SOURCE FUNCTION FOR COMBINED BAND 4
!------------------------------------
    DO I=1,IM
        DO K=1,LP1
            VTMP3(I,K) = SOURCE(IXO(I,K), 4)
            DSORC(I,K) =  DSRCE(IXO(I,K), 4)
        END DO
    END DO
!
    DO K=1,LP1
        DO I=1,IM
            SORC(I,K,4) = VTMP3(I,K) + DT(I,K) * DSORC(I,K)
        END DO
    END DO
!------------------------------------  
! SOURCE FUNCTION FOR COMBINED BAND 5
!------------------------------------ 
    DO I=1,IM
        DO K=1,LP1
            VTMP3(I,K) = SOURCE(IXO(I,K), 5)
            DSORC(I,K) =  DSRCE(IXO(I,K), 5)
        END DO
    END DO
!
    DO K=1,LP1
        DO I=1,IM
            SORC(I,K,5) = VTMP3(I,K) + DT(I,K) * DSORC(I,K)
        END DO
    END DO
!------------------------------------  
! SOURCE FUNCTION FOR COMBINED BAND 6
!------------------------------------
    DO I=1,IM
        DO K=1,LP1
            VTMP3(I,K) = SOURCE(IXO(I,K), 6)
            DSORC(I,K) =  DSRCE(IXO(I,K), 6)
        END DO
    END DO
!
    DO K=1,LP1
        DO I=1,IM
            SORC(I,K,6) = VTMP3(I,K) + DT(I,K) * DSORC(I,K)
        END DO
    END DO
!------------------------------------
! SOURCE FUNCTION FOR COMBINED BAND 7
!------------------------------------
    DO I=1,IM
        DO K=1,LP1
            VTMP3(I,K) = SOURCE(IXO(I,K), 7)
            DSORC(I,K) =  DSRCE(IXO(I,K), 7)
        END DO
    END DO
!
    DO K=1,LP1
        DO I=1,IM
            SORC(I,K,7) = VTMP3(I,K) + DT(I,K) * DSORC(I,K)
        END DO
    END DO
!------------------------------------
! SOURCE FUNCTION FOR COMBINED BAND 8
!------------------------------------
    DO I=1,IM
        DO K=1,LP1
            VTMP3(I,K) = SOURCE(IXO(I,K), 8)
            DSORC(I,K) =  DSRCE(IXO(I,K), 8)
        END DO
    END DO
!
    DO K=1,LP1
        DO I=1,IM
            SORC(I,K,8) = VTMP3(I,K) + DT(I,K) * DSORC(I,K)
        END DO
    END DO
!------------------------------------------
! SOURCE FUNCTION FOR BAND 9 (560-670 CM-1)
!------------------------------------------
    DO I=1,IM
        DO K=1,LP1
            VTMP3(I,K) = SOURCE(IXO(I,K), 9)
            DSORC(I,K) =  DSRCE(IXO(I,K), 9)
        END DO
    END DO
!
    DO K=1,LP1
        DO I=1,IM
            SORC(I,K,9) = VTMP3(I,K) + DT(I,K) * DSORC(I,K)
        END DO
    END DO
!-------------------------------------------
! SOURCE FUNCTION FOR BAND 10 (670-800 CM-1)
!-------------------------------------------
    DO I=1,IM
        DO K=1,LP1
            VTMP3(I,K) = SOURCE(IXO(I,K), 10)
            DSORC(I,K) =  DSRCE(IXO(I,K), 10)
        END DO
    END DO
!
    DO K=1,LP1
        DO I=1,IM
            SORC(I,K,10) = VTMP3(I,K) + DT(I,K) * DSORC(I,K)
        END DO
    END DO
!-------------------------------------------
! SOURCE FUNCTION FOR BAND 11 (800-900 CM-1)
!-------------------------------------------
    DO I=1,IM
        DO K=1,LP1
            VTMP3(I,K) = SOURCE(IXO(I,K), 11)
            DSORC(I,K) =  DSRCE(IXO(I,K), 11)
        END DO
    END DO
!
    DO K=1,LP1
        DO I=1,IM
            SORC(I,K,11) = VTMP3(I,K) + DT(I,K) * DSORC(I,K)
        END DO
    END DO
!-------------------------------------------
! SOURCE FUNCTION FOR BAND 12 (900-990 CM-1)
!-------------------------------------------
    DO I=1,IM
        DO K=1,LP1
            VTMP3(I,K) = SOURCE(IXO(I,K), 12)
            DSORC(I,K) =  DSRCE(IXO(I,K), 12)
        END DO
    END DO

    DO K=1,LP1
        DO I=1,IM
            SORC(I,K,12) = VTMP3(I,K) + DT(I,K) * DSORC(I,K)
        END DO
    END DO
!--------------------------------------------
! SOURCE FUNCTION FOR BAND 13 (990-1070 CM-1)
!--------------------------------------------
    DO I=1,IM
        DO  K=1,LP1
            VTMP3(I,K) = SOURCE(IXO(I,K), 13)
            DSORC(I,K) =  DSRCE(IXO(I,K), 13)
        END DO
    END DO
!
    DO K=1,LP1
        DO I=1,IM
            SORC(I,K,13) = VTMP3(I,K) + DT(I,K) * DSORC(I,K)
        END DO
    END DO
!---------------------------------------------
! SOURCE FUNCTION FOR BAND 14 (1070-1200 CM-1)
!---------------------------------------------
    DO I=1,IM
        DO K=1,LP1
            VTMP3(I,K) = SOURCE(IXO(I,K), 14)
            DSORC(I,K) =  DSRCE(IXO(I,K), 14)
        END DO
    END DO
!
    DO K=1,LP1
        DO I=1,IM
            SORC(I,K,14) = VTMP3(I,K) + DT(I,K) * DSORC(I,K)
        END DO
    END DO
!---------------------------------------------------------------------------------------
! THE FOLLOWING SUBROUTINE OBTAINS NLTE SOURCE FUNCTION FOR CO2
!
! CALL NLTE
!
! OBTAIN SPECIAL SOURCE FUNCTIONS FOR THE 15 UM BAND (CSOUR) AND THE WINDOW REGION (SS1)
!---------------------------------------------------------------------------------------
    DO K=1,LP1
        DO I=1,IM
            SS1(I,K) = SORC(I,K,11) + SORC(I,K,12) + SORC(I,K,14)
        END DO
    END DO
!
    DO K=1,LP1
        DO I=1,IM
            CSOUR(I,K) = SORC(I,K,9) + SORC(I,K,10)
        END DO
    END DO
!--------------------------------------------------------------------------- 
! COMPUTE TEMP 4 (TC) AND VERTICAL TEMPERATURE DIFFERENCES (OSS,CSS,SS2,DTC) 
! ALL THESE WILL BE USED LATER IN FLUX COMPUTATIONS.
!---------------------------------------------------------------------------
    DO K=1,LP1
        DO I=1,IM
            TC(I,K) = (TEMP(I,K) * TEMP(I,K)) ** 2
        END DO
    END DO
!
    DO K=1,LM
        DO I=1,IM
            OSS(I,K+1) =  SORC(I,K+1,13) -  SORC(I,K,13)
            CSS(I,K+1) = CSOUR(I,K+1)    - CSOUR(I,K)
            DTC(I,K+1) =    TC(I,K+1)    -    TC(I,K)
            SS2(I,K+1) =   SS1(I,K+1)    -   SS1(I,K)
        END DO
    END DO
!--------------------------------------------------------------------------------------------------
! THE FOLLOWIMG IS A DRASTIC REWRITE OF THE RADIATION CODE TO (LARGELY) ELIMINATE THREE-DIMENSIONAL
! ARRAYS.
! THE CODE WORKS ON THE FOLLOWING PRINCIPLES:
!
! LET K = FIXED FLUX LEVEL,
!    KP = VARYING FLUX LEVELTHEN FLUX(K)=SUM OVER KP: (DELTAB(KP)*TAU(KP,K)) OVER ALL KPS, FROM 1 
!         TO LP1.
!
! WE CAN BREAK DOWN THE CALCULATIONS FOR ALL KS AS FOLLOWS:
!
! FOR ALL KS K=1 TO LP1:
! FLUX(K)=SUM OVER KP: (DELTAB(KP)*TAU(KP,K))
! (1) OVER ALL KPS, FROM K+1 TO LP1 AND FOR KP FROM K+1 TO LP1: FLUX(KP) = DELTAB(K)*TAU(K,KP)              
! (2) NOW IF TAU(K,KP)=TAU(KP,K) (SYMMETRICAL ARRAYS)
!      WE CAN COMPUTE A 1-DIMENSIONAL ARRAY TAU1D(KP) FROM K+1 TO LP1, EACH TIME K IS INCREMENTED.
! EQUATIONS (1) AND (2) THEN BECOME:
!
! TAU1D(KP) = (VALUES FOR TAU(KP,K) AT THE PARTICULAR K)
! FLUX(K) = SUM OVER KP : (DELTAB(KP)*TAU1D(KP))   (3)
! FLUX(KP) = DELTAB(K)*TAU1D(KP)                   (4)
!
! THE TERMS FOR TAU (K,K) AND OTHER SPECIAL TERMS (FOR NEARBY LAYERS) MUST, OF COURSE, BE HANDLED
! SEPARATELY, AND WITH CARE.
!
! COMPUTE "UPPER TRIANGLE" TRANSMISSION FUNCTIONS FOR THE 9.6 UM BAND (TO3SP) AND THE 15 UM BAND 
! (OVER1D). ALSO, THE STAGE 1...COMPUTE O3 ,OVER TRANSMISSION FCTNS AND AVEPHI
!
! DO K=1 CALCULATION (FROM FLUX LAYER KK TO THE TOP) SEPARATELY AS VECTORIZATION IS IMPROVED,AND 
! OZONE CTS TRANSMISSIVITY MAY BE EXTRACTED HERE.
!--------------------------------------------------------------------------------------------------
    DO K=1,LM
        DO I=1,IM
            AVEPHI(I,K) = TOTPHI(I,K+1)
        END DO
    END DO
!--------------------------------------------------------------------------------------------------
! IN ORDER TO PROPERLY EVALUATE EMISS INTEGRATED OVER THE (LP1) LAYER, A SPECIAL EVALUATION OF 
! EMISS IS DONE THIS REQUIRES A SPECIAL COMPUTATION OF AVEPHI, AND IT IS STORED IN THE 
! (OTHERWISE VACANT) LP1TH POSITION
!--------------------------------------------------------------------------------------------------
    DO I=1,IM
        AVEPHI(I,LP1) = AVEPHI(I,LM1) + EMX1(I)
    END DO
!----------------------- 
! COMPUTE FLUXES FOR K=1
!----------------------- 
    CALL E1E290(E1CTS1, E1CTS2, E1FLX , E1CTW1, E1CTW2, EMISS, FXO, DT,                           &
    &           FXOE2 , DTE2  , AVEPHI, TEMP  , T)
!
    DO K=1,LM
        DO I=1,IM
              FAC1(I,K) = BO3RND(2)       * TPHIO3(I,K+1) / TOTO3(I,K+1)
            TO3SPC(I,K) = HAF*(FAC1(I,K)  * (SQRT(ONE + (FOUR * AO3RND(2) * TOTO3(I,K+1))         &
    &                   /      FAC1(I,K)) - ONE))
!--------------------------------------------------------------------------------------------------
! FOR K=1, TO3SP IS USED INSTEAD OF TO31D (THEY ARE EQUAL IN THIS CASE); TO3SP IS PASSED TO SPA90, 
! WHILE TO31D IS A WORK-ARRAY.
!--------------------------------------------------------------------------------------------------
             TO3SP(I,K) = EXP(HM1EZ * (            TO3SPC(I,K)    + SKO3R * TOTVO2(I,K+1)))
            OVER1D(I,K) = EXP(HM1EZ * (SQRT(AB15WD*TOTPHI(I,K+1)) + SKC1R * TOTVO2(I,K+1)))
!--------------------------------------------------------------------------------------------------
! BECAUSE ALL CONTINUUM TRANSMISSIVITIES ARE OBTAINED FROM THE 2-D QUANTITY CNTTAU 
! (AND ITS RECIPROCAL TOTEVV) WE STORE BOTH OF THESE HERE. FOR K=1, CONT1D EQUALS CNTTAU
!--------------------------------------------------------------------------------------------------
            CNTTAU(I,K) = EXP(HM1EZ * TOTVO2(I,K+1))
            TOTEVV(I,K) = 1. / CNTTAU(I,K)
        END DO
    END DO
!
    DO K=1,LM
        DO I=1,IM
            CO2SP(I,K+1) = OVER1D(I,K) * CO21(I,1,K+1)
        END DO
    END DO
!
    DO K=1,LM
        DO I=1,IM
            CO21(I,K+1,1) = CO21(I,K+1,1) * OVER1D(I,K)
        END DO
    END DO
!------------------------------------------------------ 
! RLOG IS THE NBL AMOUNT FOR THE 15 UM BAND CALCULATION
!------------------------------------------------------ 
    DO I=1,IM
        RLOG(I,1) = OVER1D(I,1) * CO2NBL(I,1)
    END DO
!-------------------------------------------------------------------------------------------------- 
! THE TERMS WHEN KP=1 FOR ALL K ARE THE PHOTON EXCHANGE WITH THE TOP OF THE ATMOSPHERE, AND ARE 
! OBTAINED DIFFERENTLY THAN THE  OTHER CALCULATIONS
!--------------------------------------------------------------------------------------------------
    DO K=2,LP1
        DO I=1,IM
            FLX(I,K) = (TC(I,1) * E1FLX(I,K)   +   SS1(I,1) * CNTTAU(I,K-1)  +   SORC(I,1,13)     &
    &                *            TO3SP(I,K-1) + CSOUR(I,1) *  CO2SP(I,K))   * CLDFAC(I,1,K)
        END DO
    END DO
!
    DO I=1,IM
        FLX(I,1) = TC(I,1) * E1FLX(I,1) + SS1(I,1) + SORC(I,1,13) + CSOUR(I,1)
    END DO
!------------------------ 
! THE KP TERMS FOR K=1...
!------------------------ 
    DO KP=2,LP1
        DO I=1,IM
            FLX(I,1) = FLX(I,1)  + (OSS(I,KP)   * TO3SP(I,KP-1) +   SS2(I,KP)    * CNTTAU(I,KP-1) &
    &                + CSS(I,KP) * CO21(I,KP,1) +   DTC(I,KP)   * EMISS(I,KP-1)) * CLDFAC(I,KP,1)
        END DO
    END DO
!-------------------------------------------------------------------------------------------------- 
! SUBROUTINE SPA88 IS CALLED TO OBTAIN EXACT CTS FOR WATER CO2 AND O3, AND APPROXIMATE CTS CO2 AND
! O3 CALCULATIONS.
!--------------------------------------------------------------------------------------------------
    CALL SPA88(EXCTS, CTSO3 , GXCTS, SORC  , CSOUR , CLDFAC, TEMP, PRESS, VAR1, VAR2, P, DELP,    &
    &          DELP2, TOTVO2, TO3SP, TO3SPC, CO2SP1, CO2SP2, CO2SP)
!-------------------------------------------------------------------------------------------------- 
! THIS SECTION COMPUTES THE EMISSIVITY CTS HEATING RATES FOR 2 EMISSIVITY BANDS: THE 0-160,
! 1200 -2200 CM-1 BAND AND THE 800-990, 1070-1200 CM-1 BAND. 
! THE REMAINING CTS COMTRIBUTIONS ARE CONTAINED IN CTSO3, COMPUTED IN SPA88.
!--------------------------------------------------------------------------------------------------
    DO I=1,IM
        VTMP3(I,1) = 1.
    END DO
!
    DO K=1,LM
        DO I=1,IM
            VTMP3(I,K+1) = CNTTAU(I,K) * CLDFAC(I,K+1,1)
        END DO
    END DO
!
    DO K=1,LM
        DO I=1,IM
              CTS(I,K)   = RADCON * DELP(I,K) * (TC(I,K) * (E1CTW2(I,K)   * CLDFAC(I,K+1,1)       &
    &                    -                                  E1CTW1(I,K)   * CLDFAC(I,K,1))        &
    &                    +                      SS1(I,K) *  (VTMP3(I,K+1) -  VTMP3(I,K)))
        END DO
    END DO
!
    DO K=1,LM
        DO I=1,IM
            VTMP3(I,K)   = TC(I,K)   * (CLDFAC(I,K,1)   * (E1CTS1(I,K) -   E1CTW1(I,K))           &
    &                    -              CLDFAC(I,K+1,1) * (E1CTS2(I,K) -   E1CTW2(I,K)))
        END DO
    END DO
!
    DO I=1,IM
        FLX1E1(I) = TC(I,LP1) * CLDFAC(I,LP1,1) * (E1CTS1(I,LP1) - E1CTW1(I,LP1))
    END DO
!
    DO K=1,LM
        DO I=1,IM
            FLX1E1(I) = FLX1E1(I) + VTMP3(I,K)
        END DO
    END DO
!--------------------------------------------------------------------------------------------------
! NOW REPEAT FLUX CALCULATIONS FOR THE K=2...LM1 CASES.
! CALCULATIONS FOR FLUX LEVEL L AND LP1 ARE DONE SEPARATELY, AS ALL EMISSIVITY AND CO2 CALCULATIONS
! ARE SPECIAL CASES OR NEARBY LAYERS.
!--------------------------------------------------------------------------------------------------
    DO K=2,LM1
        KLEN = K
!
        DO KK=1,LP1-K
            DO I=1,IM
                AVEPHI(I,KK+K-1) = TOTPHI(I,KK+K) - TOTPHI(I,K)
            END DO
        END DO
!
        DO I=1,IM
            AVEPHI(I,LP1) = AVEPHI(I,LM1) + EMX1(I)
        END DO
!--------------------------------------------------------------------------------------------------
! COMPUTE EMISSIVITY FLUXES (E2) FOR THIS CASE. NOTE THAT WE HAVE OMITTED THE NEARBY LATER CASE 
! (EMISS(I,K,K)) AS WELL AS ALL CASES WITH K=L OR LP1.
! BUT THESE CASES HAVE ALWAYS BEEN HANDLED AS SPECIAL CASES, SO WE MAY AS WELL COMPUTE THEIR FLUXES 
! SEPARASTELY.
!-------------------------------------------------------------------------------------------------- 
        CALL E290(EMISSB, EMISS, AVEPHI, KLEN, FXOE2, DTE2)

        DO KK=1,LP1-K
            DO I=1,IM
                 AVMO3(I,KK+K-1) =  TOTO3(I,KK+K  ) -  TOTO3(I,K  )
                AVPHO3(I,KK+K-1) = TPHIO3(I,KK+K  ) - TPHIO3(I,K  )
                 AVVO2(I,KK+K-1) = TOTVO2(I,KK+K  ) - TOTVO2(I,K  )
                CONT1D(I,KK+K-1) = CNTTAU(I,KK+K-1) * TOTEVV(I,K-1)
            END DO
        END DO
!
        DO KK=1,LP1-K
            DO I=1,IM
                  FAC1(I,K+KK-1) = BO3RND(2) * AVPHO3(I,K+KK-1)  / AVMO3(I,K+KK-1)
!
                 VTMP3(I,K+KK-1) = HAF * (FAC1(I,K+KK-1)                                          &
    &                            * (SQRT(ONE + (FOUR * AO3RND(2) * AVMO3(I,K+KK-1))               &
    &                            /  FAC1(I,K+KK-1)) - ONE))
!
                 TO31D(I,K+KK-1) = EXP(HM1EZ * (VTMP3(I,K+KK-1) + SKO3R *  AVVO2(I,K+KK-1)))
!
                OVER1D(I,K+KK-1) = EXP(HM1EZ * (            SQRT(AB15WD * AVEPHI(I,K+KK-1))       &
    &                            +                                SKC1R *  AVVO2(I,K+KK-1)))
!
                  CO21(I,K+KK,K) = OVER1D(I,K+KK-1) * CO21(I,K+KK,K)
            END DO
        END DO
!
        DO KP=K+1,LP1
            DO I=1,IM
                CO21(I,K,KP) = OVER1D(I,KP-1) * CO21(I,K,KP)
            END DO
        END DO
!------------------------------------------------------ 
! RLOG IS THE NBL AMOUNT FOR THE 15 UM BAND CALCULATION
!------------------------------------------------------ 
        DO I=1,IM
            RLOG(I,K) = OVER1D(I,K) * CO2NBL(I,K)
        END DO
!-------------------------------- 
! THE KP TERMS FOR ARBIRRARY K...
!-------------------------------- 
        DO KP=K+1,LP1
            DO I=1,IM
                FLX(I,K) =   FLX(I,K) + (OSS(I,KP) *  TO31D(I,KP-1)                               &
    &                    +               SS2(I,KP) * CONT1D(I,KP-1)                               &
    &                    +               CSS(I,KP) *   CO21(I,KP,K)                               &
    &                    +               DTC(I,KP) *  EMISS(I,KP-1))                              &
    &                    *                           CLDFAC(I,KP,K)
            END DO
        END DO
!
        DO KP=K+1,LP1
            DO I=1,IM
                FLX(I,KP) = FLX(I,KP) + (OSS(I,K)    *  TO31D(I,KP-1)                             &
    &                     +              SS2(I,K)    * CONT1D(I,KP-1)                             &
    &                     +              CSS(I,K)    *   CO21(I,K,KP)                             &
    &                     +              DTC(I,K)    * EMISSB(I,KP-1))                            &
    &                     *                            CLDFAC(I,K,KP)
            END DO
        END DO
!
    END DO
!--------------------------------------------------------------------------------------------------
! NOW DO K=L CASE. SINCE THE KP LOOP IS LENGTH 1, MANY SIMPLIFICATIONS OCCUR.
! ALSO, THE CO2 QUANTITIES (AS WELL AS THE EMISS QUANTITIES) ARE COMPUTED IN THE NBL SEDCTION; 
! THEREFORE, WE WANT ONLY OVER, TO3 AND CONT1D (OVER(I,L),TO31D(I,L) AND CONT1D(I,L) ACCORDING TO 
! THE NOTATION. THUS NO CALL IS MADE TO THE E290 SUBROUTINE.
! THE THIRD SECTION CALCULATES BOUNDARY LAYER AND NEARBY LAYER CORRECTIONS TO THE TRANSMISSION 
! FUNCTIONS OBTAINED ABOVE. METHODS ARE GIVEN IN REF. (4). 
! THE FOLLOWING RATIOS ARE USED IN VARIOUS NBL CALCULATIONS:
!
! THE REMAINING CALCULATIONS ARE FOR :
! 1) THE (K,K) TERMS, K=2,LM1;
! 2) THE (L,L) TERM
! 3) THE (L,LP1) TERM
! 4) THE (LP1,L) TERM
! 5) THE (LP1,LP1) TERM.
! EACH IS UNIQUELY HANDLED; DIFFERENT FLUX TERMS ARE COMPUTED DIFFERENTLY
!
! FOURTH SECTION OBTAINS WATER TRANSMISSION FUNCTIONS USED IN Q(APPROX) CALCULATIONS AND ALSO 
! MAKES NBL CORRECTIONS:
! 1) EMISS (I,J) IS THE TRANSMISSION FUNCTION MATRIX OBTAINED BY CALLING SUBROUTINE E1E288;
! 2) "NEARBY LAYER" CORRECTIONS (EMISS(I,I)) ARE OBTAINED USING SUBROUTINE E3V88;
! 3) SPECIAL VALUES AT THE SURFACE (EMISS(L,LP1),EMISS(LP1,L), EMISS(LP1,LP1)) ARE CALCULATED.
!
! OBTAIN ARGUMENTS FOR E1E288 AND E3V88:
!--------------------------------------------------------------------------------------------------
    DO I=1,IM
        TPL(I,1   ) = TEMP(I,LM)
        TPL(I,LP1 ) = HAF * (T(I,LP1) + TEMP(I,LM))
        TPL(I,LLP1) = HAF * (T(I,LM ) + TEMP(I,LM))
    END DO
!
    DO K=2,LM
        DO I=1,IM
            TPL(I,K   ) = T(I,K)
            TPL(I,K+LM) = T(I,K)
        END DO
    END DO
!--------------------------------------------------------------------------------------------------
! E2 FUNCTIONS ARE REQUIRED IN THE NBL CALCULATIONS FOR 2 CASES, DENOTED (IN OLD CODE) AS (L,LP1) 
! AND (LP1,LP1)
!--------------------------------------------------------------------------------------------------
    DO I=1,IM
        AVEPHI(I,1) = VAR2(I,LM)
        AVEPHI(I,2) = VAR2(I,LM) + EMPL(I,LM)
    END DO
!
    CALL E2SPEC(EMISS, AVEPHI, FXOSP, DTSP)
!---------------------------------------- 
! CALL E3V88 FOR NBL H2O TRANSMISSIVITIES
!---------------------------------------- 
    CALL E3V88(EMD, TPL, EMPL)
!-------------------------------------------------------------------------------------------------- 
! COMPUTE NEARBY LAYER AND SPECIAL-CASE TRANSMISSIVITIES FOR EMISS USING METHODS FOR H2O GIVEN IN
! REF. (4)
!--------------------------------------------------------------------------------------------------
    DO K=2,LM
        DO I=1,IM
            EMISDG(I,K) = EMD(I,K+LM) + EMD(I,K)
        END DO
    END DO
!-------------------------------------------------------------------  
! NOTE THAT EMX1/2 (PRESSURE SCALED PATHS) ARE NOW COMPUTED IN LWR88
!-------------------------------------------------------------------
    DO I=1,IM
        EMSPEC(I,1)   = (EMD(I,1)   * EMPL(I,1)                                                   &
    &                 -  EMD(I,LP1) * EMPL(I,LP1))                                                &
    &                 / EMX1(I)                                                                   &
    &                 + QUARTR      * (EMISS(I,1) + EMISS(I,2))
!
        EMISDG(I,LP1) = TWO *  EMD(I,LP1)
        EMSPEC(I,2)   = TWO * (EMD(I,1)    * EMPL(I,1)                                            &
    &                 -        EMD(I,LLP1) * EMPL(I,LLP1))                                        &
    &                 / EMX2(I)
    END DO
!
    DO I=1,IM
          FAC1(I,LM) = BO3RND(2) * VAR4(I,LM) / VAR3(I,LM)
         VTMP3(I,LM) = HAF * (FAC1(I,LM)                                                            &
    &               * (SQRT(ONE + (FOUR * AO3RND(2) * VAR3(I,LM)) / FAC1(I,LM)) - ONE))
!
         TO31D(I,LM) = EXP(HM1EZ   * (              VTMP3(I,LM)  + SKO3R * CNTVAL(I,LM)))
        OVER1D(I,LM) = EXP(HM1EZ   * (SQRT(AB15WD *  VAR2(I,LM)) + SKC1R * CNTVAL(I,LM)))
        CONT1D(I,LM) = CNTTAU(I,LM) * TOTEVV(I,LM1)
          RLOG(I,LM) = OVER1D(I,LM) * CO2NBL(I,LM)
    END DO
!
    DO K=1,LM
        DO I=1,IM
            RLOG(I,K) = LOG(RLOG(I,K))
        END DO
    END DO
!
    DO K=1,LM1
        DO I=1,IM
            DELPR1(I,K+1    ) =         DELP(I,K+1)  * (PRESS(I,K+1) - P(I,K+1))
               ALP(I,LP1+K-1) = -SQRT(DELPR1(I,K+1)) *   RLOG(I,K+1)
        END DO
    END DO
!
    DO K=1,LM
        DO I=1,IM
            DELPR2(I,K+1) =         DELP(I,K  )  *   (P(I,K+1) - PRESS(I,K))
               ALP(I,K  ) = -SQRT(DELPR2(I,K+1)) * RLOG(I,K  )
        END DO
    END DO
!
    DO I=1,IM
        ALP(I,LL  ) = -RLOG(I,LM)
        ALP(I,LLP1) = -RLOG(I,LM) * SQRT(DELP(I,LM) * (P(I,LP1) - PRESS(I,LM1)))
    END DO
!--------------------------------------------------------------------------------------------------
! THE FIRST COMPUTATION IS FOR THE 15 UM BAND,WITH THE FOR THE COMBINED H2O AND CO2 TRANSMISSION
! FUNCTION.
! 
! PERFORM NBL COMPUTATIONS FOR THE 15 UM BAND THE STATEMENT FUNCTION SF IN PREV. VERSIONS IS NOW 
! EXPLICITLY EVALUATED.
!--------------------------------------------------------------------------------------------------
    DO K=1,LLP1
        DO I=1,IM
            C(I,K) = ALP(I,K) * (HMP66667 + ALP(I,K) * (QUARTR + ALP(I,K) * HM6666M2))
        END DO
    END DO
!
    DO I=1,IM
        CO21(I,LP1,LP1) = ONE + C(I,LM)
        CO21(I,LP1,LM ) = ONE + (DELP2(I,LM)   *     C(I,LL)    - (PRESS(I,LM)                    &
    &                   -            P(I,LM))  *     C(I,LLM1))                                   &
    &                   /           (P(I,LP1)  - PRESS(I,LM))
!
        CO21(I,LM,LP1)  = ONE + ((P(I,LP1) - PRESS(I,LM1)) * C(I,LLP1)                            &
    &                   -        (P(I,LP1) - PRESS(I,LM )) * C(I,LM  ))                           &
    &                   /    (PRESS(I,LM ) - PRESS(I,LM1))
    END DO

    DO K=2,LM
        DO I=1,IM
            CO21(I,K,K) = ONE + HAF * (C(I,LM1+K) + C(I,K-1))
        END DO
    END DO
!-------------------------------------------------------------------------------------------------- 
! COMPUTE NEARBY-LAYER TRANSMISSIVITIES FOR THE O3 BAND AND FOR THE ONE-BAND CONTINUUM BAND 
! (TO3 AND EMISS2). 
! THE SF2 FUNCTION IS USED. THE METHOD IS THE SAME AS DESCRIBED FOR CO2 IN REF (4).
!--------------------------------------------------------------------------------------------------
    DO K=1,LM1
        DO I=1,IM
            CSUB(I,K+1    ) = CNTVAL(I,K+1) * DELPR1(I,K+1)
            CSUB(I,LP1+K-1) = CNTVAL(I,K  ) * DELPR2(I,K+1)
        END DO
    END DO
!---------------------------------------------------------------  
! THE SF2 FUNCTION IN PREV. VERSIONS IS NOW EXPLICITLY EVALUATED
!--------------------------------------------------------------- 
    DO K=1,LLM2
        DO I=1,IM
            CSUB2(I,K+1) = SKO3R * CSUB(I,K+1)
!
             C(I,K+1) =  CSUB(I,K+1) * (HMP5 +  CSUB(I,K+1) * (HP166666 -  CSUB(I,K+1) * H41666M2))
            C2(I,K+1) = CSUB2(I,K+1) * (HMP5 + CSUB2(I,K+1) * (HP166666 - CSUB2(I,K+1) * H41666M2))
        END DO
    END DO
!
    DO I=1,IM
        CONTDG(I,LP1) = 1. +  C(I,LLM1)
         TO3DG(I,LP1) = 1. + C2(I,LLM1)
    END DO

    DO K=2,LM
        DO I=1,IM
            CONTDG(I,K) = ONE + HAF * ( C(I,K) +  C(I,LM1+K))
             TO3DG(I,K) = ONE + HAF * (C2(I,K) + C2(I,LM1+K))
        END DO
    END DO
!-------------------------------------------- 
! NOW OBTAIN FLUXES FOR THE DIAGONAL TERMS...
!--------------------------------------------  
    DO K=2,LP1
        DO I=1,IM
            FLX(I,K) =    FLX(I,K) + (DTC(I,K) * EMISDG(I,K) + SS2(I,K)                           &
    &                * CONTDG(I,K) +  OSS(I,K) *  TO3DG(I,K) + CSS(I,K)                           &
    &                *   CO21(I,K,K))                                                             &
    &                * CLDFAC(I,K,K)
        END DO
    END DO
!----------------------------------  
! FOR THE TWO OFF-DIAGONAL TERMS...
!---------------------------------- 
    DO I=1,IM
        FLX(I,LM)   =    FLX(I,LM)  + (CSS(I,LP1) *  CO21(I,LP1,LM) + DTC(I,LP1)                  &
    &              * EMSPEC(I,2)    +  OSS(I,LP1) * TO31D(I,LM)     + SS2(I,LP1)                  &
    &              * CONT1D(I,LM))                                                                &
    &              * CLDFAC(I,LP1,LM)
!
        FLX(I,LP1) =    FLX(I,LP1) + (CSS(I,LM)  * CO21(I,LM,LP1)  + OSS(I,LM)                    &
    &              *  TO31D(I,LM)  +  SS2(I,LM)  * CONT1D(I,LM)    + DTC(I,LM)                    &
    &              * EMSPEC(I,1))                                                                 &
    &              * CLDFAC(I,LM,LP1)
    END DO
!----------------------------------------------------------------------------------------------- 
! FINAL SECTION OBTAINS EMISSIVITY HEATING RATES, TOTAL HEATING RATES AND THE FLUX AT THE GROUND
!
! CALCULATE THE EMISSIVITY HEATING RATES
!----------------------------------------------------------------------------------------------- 
    DO K=1,LM
        DO I=1,IM
            HEATEM(I,K) = RADCON * (FLX(I,K+1) - FLX(I,K)) * DELP(I,K)
        END DO 
    END DO
!----------------------------------  
! CALCULATE THE TOTAL HEATING RATES
!----------------------------------
    DO K=1,LM
        DO I=1,IM
            HEATRA(I,K) = HEATEM(I,K) - CTS(I,K) - CTSO3(I,K) + EXCTS(I,K)
        END DO 
    END DO
!--------------------------------------------------------------------------------------------------
! CALCULATE THE FLUX AT EACH FLUX LEVEL USING THE FLUX AT THE TOP (FLX1E1+GXCTS) AND THE INTEGRAL 
! OF THE HEATING RATES (VSUM1)
!--------------------------------------------------------------------------------------------------
    DO K=1,LM
        DO I=1,IM
            VSUM1(I,K) = HEATRA(I,K) * DELP2(I,K) * RADCON1
        END DO 
    END DO
!
    DO I=1,IM
        TOPFLX(I)   = FLX1E1(I) + GXCTS(I)
        FLXNET(I,1) = TOPFLX(I)
    END DO
!--------------------------------------------------------------------------------------------- 
! ONLY THE SURFACE VALUE OF FLUX (GRNFLX) IS NEEDED UNLESS THE THICK CLOUD SECTION IS INVOKED.
!--------------------------------------------------------------------------------------------- 
    DO K=2,LP1
        DO I=1,IM
            FLXNET(I,K) = FLXNET(I,K-1) + VSUM1(I,K-1)
        END DO 
    END DO
!
    DO I=1,IM
        GRNFLX(I) = FLXNET(I,LP1)
    END DO
!-------------------------------------------------------------------------------------------------- 
! THIS IS THE THICK CLOUD SECTION.OPTIONALLY, IF THICK CLOUD FLUXES ARE TO BE "CONVECTIVELY 
! ADJUSTED", IE, DF/DP IS CONSTANT, FOR CLOUDY PART OF GRID POINT, THE FOLLOWING CODE IS EXECUTED.
! FIRST,COUNT THE NUMBER OF CLOUDS ALONG THE LAT. ROW. SKIP THE ENTIRE THICK CLOUD COMPUTATION OF 
! THERE ARE NO CLOUDS.
!--------------------------------------------------------------------------------------------------
    ICNT = 0
!
    DO I=1,IM
        ICNT = ICNT + NCLDS(I)
    END DO
!
    IF (ICNT == 0) GOTO 668
!------------------------------------------------------ 
! FIND THE MAXIMUM NUMBER OF CLOUDS IN THE LATITUDE ROW
!------------------------------------------------------ 
    KCLDS = NCLDS(1)
!
    DO I=1,IM
        KCLDS = MAX(NCLDS(I), KCLDS)
    END DO
!-------------------------------------------------------------------------------------------------- 
! OBTAIN THE PRESSURES AND FLUXES OF THE TOP AND BOTTOM OF THE NC'TH CLOUD (IT IS ASSUMED THAT ALL 
! KTOP AND KBTM'S HAVE BEEN DEFINED!).
!--------------------------------------------------------------------------------------------------
    DO KK=1,KCLDS
        KMIN = LP1
        KMAX = 0
!
        DO I=1,IM
            J1 = KTOP(I,KK+1)
            J3 = KBTM(I,KK+1)
!
            IF (J3 > J1) THEN
                PTOP(I) =      P(I,J1  )
                PBOT(I) =      P(I,J3+1)
                FTOP(I) = FLXNET(I,J1  )
                FBOT(I) = FLXNET(I,J3+1)
!--------------------------------------------   
! OBTAIN THE "FLUX DERIVATIVE" DF/DP (DELPTC)
!-------------------------------------------- 
                DELPTC(I) = (FTOP(I) - FBOT(I)) / (PTOP(I) - PBOT(I))
                KMIN = MIN(KMIN, J1)
                KMAX = MAX(KMAX, J3)
            END IF
!
        END DO
!
        KMIN = KMIN + 1
!------------------------------------------------------------------------   
! CALCULATE THE TOT. FLUX CHG. FROM THE TOP OF THE CLOUD, FOR ALL LEVELS.
!------------------------------------------------------------------------ 
        DO K=KMIN,KMAX
            DO I=1,IM
                IF (KTOP(I,KK+1) < K .AND. K <= KBTM(I,KK+1)) THEN
                    Z1(I,K)     = (P(I,K) - PTOP(I)) * DELPTC(I) + FTOP(I)
                    FLXNET(I,K) = Z1(I,K)
                END IF
            END DO
        END DO
!
    END DO
!-------------------------------------------------------------------------------------------------- 
! USING THIS FLUX CHG. IN THE CLOUDY PART OF THE GRID BOX, OBTAIN THE NEW FLUXES, WEIGHTING THE 
! CLEAR AND CLOUDY FLUXES:AGAIN, ONLY THE FLUXES IN THICK-CLOUD LEVELS WILL EVENTUALLY BE USED.
!--------------------------------------------------------------------------------------------------
668 CONTINUE
!-----------------------------------------------------------------------------
! HE FINAL STEP IS TO RECOMPUTE THE HEATING RATES BASED ON THE REVISED FLUXES:
!-----------------------------------------------------------------------------
    DO K=1,LM
        DO I=1,IM
            HEATRA(I,K) = RADCON * (FLXNET(I,K+1) - FLXNET(I,K)) * DELP(I,K)
        END DO
    END DO
!----------------------------------- 
! THE THICK CLOUD SECTION ENDS HERE.
!----------------------------------- 
    RETURN
!
    END SUBROUTINE FST88
