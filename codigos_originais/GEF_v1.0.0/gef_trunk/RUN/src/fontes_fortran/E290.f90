!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief COMPUTES THE EXCHANGE TERMS IN THE FLUX EQUATION FOR LONGWAVE RADIATION
!> @details COMPUTES THE EXCHANGE TERMS IN THE FLUX EQUATION FOR LONGWAVE RADIATION FOR ALL 
!! TERMS EXCEPT THE EXCHANGE WITH THE TOP OF THE ATMOSPHERE. THE METHOD IS A TABLE LOOKUP ON A PRE-
!! COMPUTED E2 FUNCTION (DEFINED IN REF. (4)).
!! CALCULATIONS ARE DONE IN THE FREQUENCY RANGE:
!! 1) 0-560, 1200-2200 CM-1   FOR Q(APPROX)
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
!> @param[in] AVEPHI - H2O OPTICAL PATHS BET. FLUX PRESSURES: INPUT TO EMISSIVITY CALCULATIONS. 
!> @param[in] KLEN   - Significado de KLEN
!> @param[in] FXOE2  - TEMPERATURE INDEX USED FOR E2 FUNCTION 
!> @param[in] DTE2   - TEMP. DIFF. BETWEEN MODEL TEMP. AND TEMPS. AT TABULAR VALUES OF E2 FUNCTION.
!> @param[out] EMISSB - Significado de EMISSB
!> @param[inout] EMISS  - E2 EMISSIVITYY FCTN FOR H2O LINES (0-560,1200-2200 CM-1). OBTAINED IN E1E288.
!> @details <b>Use Module:</b>
!! @arg @c F77KINDS
!! @arg @c HCON
!! @arg @c PARMETA
!! @arg @c RDPARM
!! @arg @c TABCOM
!> @details <b>Driver:</b> 
!! @arg @c FST88
!> @details <b>Calls:</b>
!<
!--------------------------------------------------------------------------------------------------
    SUBROUTINE E290(EMISSB, EMISS, AVEPHI, KLEN, FXOE2, DTE2)
!--------------------------------------------------------------------------------------------------
! SUBROUTINE E290
!
! SUBPROGRAM: E290 - ?????
! PROGRAMMER: ?????
! ORG: ?????
! DATE: ??-??-??
!
! ABSTRACT:
! SUBROUTINE E290 COMPUTES THE EXCHANGE TERMS IN THE FLUX EQUATION FOR LONGWAVE RADIATION FOR ALL 
! TERMS EXCEPT THE EXCHANGE WITH THE TOP OF THE ATMOSPHERE. THE METHOD IS A TABLE LOOKUP ON A PRE-
! COMPUTED E2 FUNCTION (DEFINED IN REF. (4)).
! CALCULATIONS ARE DONE IN THE FREQUENCY RANGE:
! 1) 0-560, 1200-2200 CM-1   FOR Q(APPROX)
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
! AVEPHI - H2O OPTICAL PATHS BET. FLUX PRESSURES: INPUT TO EMISSIVITY CALCULATIONS. 
! KLEN   - 
! FXOE2  - TEMPERATURE INDEX USED FOR E2 FUNCTION 
! DTE2   - TEMP. DIFF. BETWEEN MODEL TEMP. AND TEMPS. AT TABULAR VALUES OF E2 FUNCTION. 
!
! OUTPUT ARGUMENT LIST:
! EMISSB - 
!
! INPUT/OUTPUT ARGUMENT LIST:
! EMISS  - E2 EMISSIVITYY FCTN FOR H2O LINES (0-560,1200-2200 CM-1). OBTAINED IN E1E288.
!
! USE MODULES: F77KINDS
!              HCON
!              PARMETA
!              RDPARM
!              TABCOM
! 
! DRIVER     : FST88
!
! CALLS      : -----
!-------------------------------------------------------------------------------------------------
    USE F77KINDS
    USE HCON
    USE PARMETA
    USE RDPARM
    USE TABCOM
!
    IMPLICIT NONE
!
    INTEGER(KIND=I4)                                                                            ::&
    & K       , I       
!
    REAL   (KIND=R4)    , DIMENSION(IM, LP1)                              , INTENT(OUT)         ::&
    & EMISSB 
!
    REAL   (KIND=R4)    , DIMENSION(IM, LP1)                              , INTENT(INOUT)       ::&
    & EMISS
!
    REAL   (KIND=R4)    , DIMENSION(IM, LP1)                              , INTENT(IN)          ::&
    & AVEPHI
!
    REAL   (KIND=R4)    , DIMENSION(IM,LP1)                                                     ::&
    & DT      , FYO     , DU
!
    INTEGER(KIND=I4)    , DIMENSION(IM,LP1)                                                     ::&
    & IVAL
!---------------------------------------- 
! TMP3 MAY BE EQUIVALENCED TO DT IN VTEMP
!---------------------------------------- 
    REAL   (KIND=R4)    , DIMENSION(IM,LP1)                                                     ::&
    & TMP3
!------------------------------- 
! VARIABLES IN THE ARGUMENT LIST
!------------------------------- 
    REAL   (KIND=R4)    , DIMENSION(IM, LP1)                              , INTENT(IN)          ::&
    & FXOE2   , DTE2
!
    EQUIVALENCE (TMP3,DT)
!
    INTEGER(KIND=I4)                                                      , INTENT(IN)          ::&
    & KLEN 
!--------------------------------------------------------------------------------------------------
! FIRST WE OBTAIN THE EMISSIVITIES AS A FUNCTION OF TEMPERATURE (INDEX FXO) AND WATER AMOUNT 
! (INDEX FYO). THIS PART OF THE CODE THUS GENERATES THE E2 FUNCTION.
!
! CALCULATIONS FOR VARYING KP (FROM KP=K+1 TO LP1, INCLUDING SPECIAL CASE: RESULTS ARE IN EMISS
!--------------------------------------------------------------------------------------------------
    DO K=1,LP2-KLEN
        DO I=1,IM
!
            IF (AVEPHI(I,KLEN+K-1) > 0) THEN
                TMP3(I,K) = LOG10(AVEPHI(I,KLEN+K-1)) + H16E1
            END IF
!
              FYO(I,K) = AINT(TMP3(I,K) * TEN)
               DU(I,K) = TMP3(I,K) - HP1 * FYO(I,K)
              FYO(I,K) = H28E1 * FYO(I,K)
             IVAL(I,K) = FYO(I,K) + FXOE2(I,KLEN+K-1)
!
            EMISS(I,KLEN+K-1) = T1(IVAL(I,K)) +   DU(I,K)                                         &
    &                         * T2(IVAL(I,K)) + DTE2(I,KLEN+K-1)                                  &
    &                         * T4(IVAL(I,K))
        END DO
    END DO
!---------------------------------------------------------------------------------------------- 
! THE SPECIAL CASE EMISS(I,L) (LAYER KP) IS OBTAINED NOW BY AVERAGING THE VALUES FOR L AND LP1:
!---------------------------------------------------------------------------------------------- 
    DO I=1,IM
        EMISS(I,LM) = HAF * (EMISS(I,LM) + EMISS(I,LP1))
    END DO 
!------------------------------------------------------- 
! NOTE THAT EMISS(I,LP1) IS NOT USEFUL AFTER THIS POINT.
!------------------------------------------------------- 
!
!--------------------------------------------------------------------------------------------------
! CALCULATIONS FOR KP=KLEN AND VARYING K; RESULTS ARE IN EMISSB.
! IN THIS CASE, THE TEMPERATURE INDEX IS UNCHANGED, ALWAYS BEING FXO(I,KLEN-1); THE WATER INDEX 
! CHANGES, BUT IS SYMMETRICAL WITH THAT FOR THE VARYING KP CASE.
! NOTE THAT THE SPECIAL CASE IS NOT INVOLVED HERE. (FIXED LEVEL) K VARIES FROM (KLEN+1) TO LP1; 
! RESULTS ARE IN EMISSB(I,(KLEN) TO L)
!--------------------------------------------------------------------------------------------------
    DO K=1,LP1-KLEN
        DO I=1,IM
            DT(I,K) = DTE2(I,KLEN-1)
          IVAL(I,K) = FYO(I,K) + FXOE2(I,KLEN-1)
        END DO 
    END DO
!
    DO K=1,LP1-KLEN
        DO I=1,IM
            EMISSB(I,KLEN+K-1) = T1(IVAL(I,K)) + DU(I,K) * T2(IVAL(I,K)) + DT(I,K) * T4(IVAL(I,K))
        END DO 
    END DO
!
    RETURN
!
    END SUBROUTINE E290
