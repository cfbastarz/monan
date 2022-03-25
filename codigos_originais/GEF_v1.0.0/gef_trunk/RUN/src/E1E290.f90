!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief COMPUTES THE EXCHANGE TERMS IN THE FLUX EQUATION FOR LONGWAVE RADIATION
!> @details COMPUTES THE EXCHANGE TERMS IN THE FLUX EQUATION FOR LONGWAVE RADIATION FOR ALL
!! TERMS EXCEPT THE EXCHANGE WITH THE TOP OF THE ATMOSPHERE. THE METHOD IS A TABLE LOOKUP ON A 
!! PRE-COMPUTED E2 FUNCTION (DEFINED IN REF. (4)). THE E1 FUNCTION  CALCULATIONS (FORMERLY DONE
!! IN SUBROUTINE E1V88 COMPUTE THE FLUX RESULTING FROM THE EXCHANGE OF PHOTONS BETWEEN A LAYER AND
!! THE TOP OF THE ATMOSPHERE.
!! THE METHOD IS A TABLE LOOKUP ON A PRE-COMPUTED E1 FUNCTION.
!! CALCULATIONS ARE DONE IN TWO FREQUENCY RANGES:
!! 1)   0-560, 1200-2200 CM-1   FOR Q(APPROX)
!! 2) 160-560            CM-1   FOR Q(APPROX,CTS).
!! MOTIVATION FOR THESE CALCULATIONS IS IN REFERENCES (1) AND (4).
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
!> @param[in] FXOE1  - TEMPERATURE INDEX USED FOR E1 FUNCTION
!> @param[in] FXOE2  - TEMPERATURE INDEX USED FOR E2 FUNCTION
!> @param[in] DTE1   - TEMP. DIFF. BETWEEN MODEL TEMP. AND TEMPS. AT TABULAR VALUES OF E1 FUNCTION.
!> @param[in] DTE2   - TEMP. DIFF. BETWEEN MODEL TEMP. AND TEMPS. AT TABULAR VALUES OF E2 FUNCTION.
!> @param[in] AVEPHI - H2O OPTICAL PATHS BET. FLUX PRESSURES: INPUT TO EMISSIVITY CALCULATIONS.
!> @param[in] TEMP   - TEMPERATURE
!> @param[in] T      - TEMPERATURE AT FLUX LEVELS OF MODEL
!> @param[out] G1     - ARGUMENT LIST,FOR 1ST FREQ. RANG
!> @param[out] G2     - ARGUMENT LIST,FOR 1ST FREQ. RANGE
!> @param[out] G3     - ARGUMENT LIST,FOR 1ST FREQ. RANGE
!> @param[out] G4     - ARGUMENT LIST,FOR 2ND FREQ. RANGE
!> @param[out] G5     - ARGUMENT LIST,FOR 2ND FREQ. RANGE
!> @param[inout] EMISS - Significado de EMISS
!> @details <b>Use Module:</b>
!! @arg @c F77KINDS
!! @arg @c HCON
!! @arg @c MPPSTAFF
!! @arg @c PARMETA
!! @arg @c RDPARM
!! @arg @c TABCOM
!> @details <b>Driver:</b> 
!! @arg @c FST88
!> @details <b>Calls:</b>
!<
!--------------------------------------------------------------------------------------------------    
	SUBROUTINE E1E290(G1, G2, G3, G4, G5, EMISS, FXOE1, DTE1, FXOE2, DTE2, AVEPHI, TEMP, T)
!--------------------------------------------------------------------------------------------------
! SUBROUTINE E1E290
!
! SUBPROGRAM: E1E290 - ?????
! PROGRAMMER: ?????
! ORG: ?????
! DATE: ??-??-??
!
! ABSTRACT:
! SUBROUTINE E1E290 COMPUTES THE EXCHANGE TERMS IN THE FLUX EQUATION FOR LONGWAVE RADIATION FOR ALL
! TERMS EXCEPT THE EXCHANGE WITH THE TOP OF THE ATMOSPHERE.
! THE METHOD IS A TABLE LOOKUP ON A PRE-COMPUTED E2 FUNCTION (DEFINED IN REF. (4)).
! THE E1 FUNCTION  CALCULATIONS (FORMERLY DONE IN SUBROUTINE E1V88 COMPUTE THE FLUX RESULTING FROM
! THE EXCHANGE OF PHOTONS BETWEEN A LAYER AND THE TOP OF THE ATMOSPHERE.
! THE METHOD IS A TABLE LOOKUP ON A PRE-COMPUTED E1 FUNCTION.
! CALCULATIONS ARE DONE IN TWO FREQUENCY RANGES:
! 1)   0-560, 1200-2200 CM-1   FOR Q(APPROX)
! 2) 160-560            CM-1   FOR Q(APPROX,CTS).
! MOTIVATION FOR THESE CALCULATIONS IS IN REFERENCES (1) AND (4).
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
! FXOE1  - TEMPERATURE INDEX USED FOR E1 FUNCTION
! FXOE2  - TEMPERATURE INDEX USED FOR E2 FUNCTION 
! DTE1   - TEMP. DIFF. BETWEEN MODEL TEMP. AND TEMPS. AT TABULAR VALUES OF E1 FUNCTION.
! DTE2   - TEMP. DIFF. BETWEEN MODEL TEMP. AND TEMPS. AT TABULAR VALUES OF E2 FUNCTION.
! AVEPHI - H2O OPTICAL PATHS BET. FLUX PRESSURES: INPUT TO EMISSIVITY CALCULATIONS.
! TEMP   - TEMPERATURE
! T      - TEMPERATURE AT FLUX LEVELS OF MODEL
!
! OUTPUT ARGUMENT LIST:
! G1     - ARGUMENT LIST,FOR 1ST FREQ. RANGE
! G2     - ARGUMENT LIST,FOR 1ST FREQ. RANGE
! G3     - ARGUMENT LIST,FOR 1ST FREQ. RANGE
! G4     - ARGUMENT LIST,FOR 2ND FREQ. RANGE
! G5     - ARGUMENT LIST,FOR 2ND FREQ. RANGE
!
! INPUT/OUTPUT ARGUMENT LIST:
! EMISS  - 
!
! USE MODULES: F77KINDS 
!              HCON
!              MPPSTAFF
!              PARMETA
!              RDPARM
!              TABCOM
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
    USE TABCOM
!
    IMPLICIT NONE
!
    REAL   (KIND=R4)    , DIMENSION(IM, LP1)                              , INTENT(IN)          ::&
    & TEMP    , T       , AVEPHI 
!
    INTEGER(KIND=I4)    , DIMENSION(IM, LL3P)                                                   ::&
    & IT1 
!
    REAL   (KIND=R4)    , DIMENSION(IM, LP1)                                                    ::&
    & FYO     ,                                                                                   &
    & DU      , WW1     ,  WW2    ,                                                               &
    & TMP3    
!
    REAL   (KIND=R4)    , DIMENSION(IM, LP1)                              , INTENT(INOUT)       ::&
    & EMISS
!
    REAL   (KIND=R4)    , DIMENSION(IM)                                                         ::&
    & TMP5    , TMP9
!
    INTEGER(KIND=I4)    , DIMENSION(IM, LP1)                                                    ::&
    & IVAL
!------------------------------- 
! VARIABLES IN THE ARGUMENT LIST
!------------------------------- 
    REAL   (KIND=R4)    , DIMENSION(IM, LP1)                              , INTENT(IN)          ::&
    & FXOE1   , DTE1    ,                                                                         &
    & FXOE2   , DTE2
!
    REAL   (KIND=R4)    , DIMENSION(IM, LP1)                              , INTENT(OUT)         ::&
    & G1      , G3      , G4
!
    REAL   (KIND=R4)    , DIMENSION(IM, LM)                               , INTENT(OUT)         ::&
    & G2      , G5
!------------------------
! IMPLICIT NONE VARIABLES
!------------------------
    INTEGER(KIND=I4)                                                                            ::&
    & I       , K       , KP 
!--------------------------------------------------------------------------------------------------
! FIRST WE OBTAIN THE EMISSIVITIES AS A FUNCTION OF TEMPERATURE (INDEX FXO) AND WATER AMOUNT 
! (INDEX FYO). 
! THIS PART OF THE CODE THUS GENERATES THE E2 FUNCTION. THE FXO INDICES HAVE BEEN OBTAINED IN FST88
! FOR CONVENIENCE.
!
! THIS SUBROUTINE EVALUATES THE K=1 CASE ONLY
!
! THIS LOOP REPLACES LOOPS GOING FROM I=1,IMAX AND KP=2,LP1 
! PLUS THE SPECIAL CASE FOR THE LP1TH LAYER
!--------------------------------------------------------------------------------------------------
    DO  K=1,LP1
        DO  I=1,IM
            IF (AVEPHI(I,K) > 0) THEN
                TMP3(I,K) = LOG10(AVEPHI(I,K)) + H16E1
            ELSE
                TMP3(I,K) = 0.
            END IF
!
              FYO(I,K) = AINT(TMP3(I,K) * TEN)
               DU(I,K) = TMP3(I,K) - HP1 * FYO(I,K)
              FYO(I,K) = H28E1 * FYO(I,K)
             IVAL(I,K) = FYO(I,K) + FXOE2(I,K)
            EMISS(I,K) = T1(IVAL(I,K)) + DU(I,K) * T2(IVAL(I,K)) + DTE2(I,K) * T4(IVAL(I,K))
        END DO 
    END DO
!--------------------------------------------------------------------------------------------------
! THE SPECIAL CASE EMISS(I,L) (LAYER KP) IS OBTAINED NOW BY AVERAGING THE VALUES FOR L AND LP1:
!--------------------------------------------------------------------------------------------------
    DO  I=1,IM
      EMISS(I,LM) = HAF * (EMISS(I,LM) + EMISS(I,LP1))
    END DO 
!--------------------------------------------------------------------------------------------------
! CALCULATIONS FOR THE KP=1 LAYER ARE NOT PERFORMED, AS THE RADIATION CODE ASSUMES THAT THE TOP 
! FLUX LAYER (ABOVE THE TOP DATA LEVEL) IS ISOTHERMAL, AND HENCE CONTRIBUTES NOTHING TO THE FLUXES 
! AT OTHER LEVELS.
!
! THE FOLLOWING IS THE CALCULATION FOR THE E1 FUNCTION, FORMERLY DONE IN SUBROUTINE E1V88. 
! THE MOVE TO E1E288 IS DUE TO THE SAVINGS IN OBTAINING INDEX VALUES (THE TEMP. INDICES HAVE BEEN 
! OBTAINED IN FST88, WHILE THE U-INDICES ARE OBTAINED IN THE E2 CALCS., WITH K=1).
!
! FOR TERMS INVOLVING TOP LAYER, DU IS NOT KNOWN; IN FACT, WE USE INDEX 2 TO REPERSENT INDEX 1 IN 
! PREV. CODE. THIS MEANS THAT THE IT1 INDEX 1 AND LLP1 HAS TO BE CALCULATED SEPARATELY. 
! THE INDEX LLP2 GIVES THE SAME VALUE AS 1; IT CAN BE OMITTED.
!--------------------------------------------------------------------------------------------------
    DO I=1,IM
        IT1(I,1) = FXOE1(I,1)
        WW1(I,1) = TEN - DTE1(I,1)
        WW2(I,1) = HP1
    END DO 
!
    DO K=1,LM
        DO I=1,IM
            IT1(I,     K+1) = FYO(I,K) + FXOE1(I,K+1)
            IT1(I, LP2+K-1) = FYO(I,K) + FXOE1(I,K  )
            WW1(I,     K+1) = TEN - DTE1(I,K+1)
            WW2(I,     K+1) = HP1 - DU(I,K)
        END DO 
    END DO 
!
    DO KP=1,LM
        DO I=1,IM
            IT1(I,KP+LLP1) = FYO(I,KP) + FXOE1(I,1)
        END DO 
    END DO
!------------------------------------------------------ 
! G3(I,1) HAS THE SAME VALUES AS G1 (AND DID ALL ALONG)
!------------------------------------------------------ 
    DO I=1,IM
        G1(I,1) = WW1(I,1) * WW2(I,1) * EM1V(IT1(I,1)) + WW2(I,1) * DTE1(I,1) * EM1V(IT1(I,1) + 1)
        G3(I,1) =  G1(I,1)
    END DO
!
    DO K=1,LM
        DO I=1,IM
            G1(I,K+1)  =  WW1(I,K+1) *  WW2(I,K+1)  *  EM1V(IT1(I,K+1))                           &
    &                  +  WW2(I,K+1) * DTE1(I,K+1)  *  EM1V(IT1(I,K+1) +  1)                      &
    &                  +  WW1(I,K+1) *   DU(I,K)    *  EM1V(IT1(I,K+1) + 28)                      &
    &                  + DTE1(I,K+1) *   DU(I,K)    *  EM1V(IT1(I,K+1) + 29)
!
            G2(I,K)   =   WW1(I,K)   *  WW2(I,K+1)  *  EM1V(IT1(I,K+LP2-1))                       &
    &                 +   WW2(I,K+1) * DTE1(I,K)    *  EM1V(IT1(I,K+LP2-1) +  1)                  &
    &                 +   WW1(I,K)   *   DU(I,K)    *  EM1V(IT1(I,K+LP2-1) + 28)                  &
    &                 +  DTE1(I,K)   *   DU(I,K)    *  EM1V(IT1(I,K+LP2-1) + 29)
        END DO 
    END DO
!
    DO KP=2,LP1
        DO I=1,IM
            G3(I,KP)  =   WW1(I,1)   *  WW2(I,KP)   *  EM1V(IT1(I,LL+KP))                         &
    &                 +   WW2(I,KP)  * DTE1(I,1)    *  EM1V(IT1(I,LL+KP) +  1)                    &
    &                 +   WW1(I,1)   *   DU(I,KP-1) *  EM1V(IT1(I,LL+KP) + 28)                    &
    &                 +  DTE1(I,1)   *   DU(I,KP-1) *  EM1V(IT1(I,LL+KP) + 29)
        END DO 
    END DO
!
    DO I=1,IM
        G4(I,1) = WW1(I,1) * WW2(I,1) * EM1VW(IT1(I,1)) + WW2(I,1) * DTE1(I,1) * EM1VW(IT1(I,1)+1)
    END DO
!
    DO K=1,LM
        DO I=1,IM
            G4(I,K+1) =   WW1(I,K+1) *  WW2(I,K+1)  *  EM1VW(IT1(I,K+1))                          &
    &                 +   WW2(I,K+1) * DTE1(I,K+1)  *  EM1VW(IT1(I,K+1) +  1)                     &
    &                 +   WW1(I,K+1) *   DU(I,K)    *  EM1VW(IT1(I,K+1) + 28)                     &
    &                 +  DTE1(I,K+1) *   DU(I,K)    *  EM1VW(IT1(I,K+1) + 29)
!
            G5(I,K)   =   WW1(I,K)   *  WW2(I,K+1)  *  EM1VW(IT1(I,K+LP2-1))                      &
    &                 +   WW2(I,K+1) * DTE1(I,K)    *  EM1VW(IT1(I,K+LP2-1) +  1)                 &
    &                 +   WW1(I,K)   *   DU(I,K)    *  EM1VW(IT1(I,K+LP2-1) + 28)                 &
    &                 +  DTE1(I,K)   *   DU(I,K)    *  EM1VW(IT1(I,K+LP2-1) + 29)
        END DO
    END DO
!
    RETURN
!
    END SUBROUTINE E1E290
