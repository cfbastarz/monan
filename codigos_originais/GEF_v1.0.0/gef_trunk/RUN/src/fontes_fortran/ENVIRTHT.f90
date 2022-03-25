!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief CALCULATE ENVIRONMENTAL EQUIVALENT POTENTIAL TEMPERATURE.
!> @details CALCULATE ENVIRONMENTAL EQUIVALENT POTENTIAL TEMPERATURE.
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
!> @param[in] P1 - Significado de P1
!> @param[in] T1 - Significado de T1
!> @param[in] Q1 - Significado de Q1
!> @param[in] ALIQ - Significado de ALIQ
!> @param[in] BLIQ - Significado de BLIQ
!> @param[in] CLIQ - Significado de CLIQ
!> @param[in] DLIQ - Significado de DLIQ
!> @param[in] R1 - Não utilizado
!> @param[in] RL - Não utilizado
!> @param[in] AICE - Não utilizado
!> @param[in] BICE - Não utilizado
!> @param[in] CICE - Não utilizado
!> @param[in] DICE - Não utilizado
!> @param[inout] THT1 - Significado de THT1 
!> @details <b>Use Module:</b>
!! @arg @c F77KINDS
!! @arg @c KFLUT
!> @details <b>Driver:</b> 
!! @arg @c KFPARA
!> @details <b>Calls:</b>
!<
!--------------------------------------------------------------------------------------------------
    SUBROUTINE ENVIRTHT(P1, T1, Q1, THT1, R1, RL, ALIQ, BLIQ, CLIQ, DLIQ, AICE, BICE, CICE, DICE)
!--------------------------------------------------------------------------------------------------
! SUBROUTINE ENVIRTHT
!
! SUBPROGRAM: ENVIRTHT - CALCULATE ENVIRONMENTAL EQUIVALENT POTENTIAL TEMPERATURE.
! PROGRAMMER: ?????
! ORG: ?????
! DATE: ??-??-??
!
! ABSTRACT:
! CALCULATE ENVIRONMENTAL EQUIVALENT POTENTIAL TEMPERATURE.
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
! P1   - 
! T1   -
! Q1   - 
! ALIQ - 
! BLIQ -
! CLIQ - 
! DLIQ - 
!
! OUTPUT ARGUMENT LIST:
! THT1 -
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
!
    IMPLICIT NONE
!
    INTEGER(KIND=I4)                                                                            ::&
    & INDLU
!
    REAL   (KIND=R4)                                                                            ::&
    & AICE    , BICE     , CICE   , DICE    ,                                                     &
    & R1      , RL 
!
    REAL   (KIND=R4)                                                                            ::&
    & EMIX    , ASTRT    , AINC   , A1      , TP      , VALUE   , AINTRP  , TLOG    , TDPT    ,   &
    & TLCL    , TSAT     , THT  
!
    REAL   (KIND=R4)                                                      , INTENT(INOUT)       ::&
    & P1      , T1      , Q1      ,                                                               &
    & ALIQ    , BLIQ    , CLIQ    , DLIQ
!
    REAL   (KIND=R4)                                                      , INTENT(OUT)         ::&
    & THT1
!
    REAL   (KIND=R4)                                                                            ::&
    & T00     , P00     , C1      , C2      , C3      , C4      , C5                             
!
    DATA T00 /  273.16      /  
    DATA P00 /    1.E5      /
    DATA C1  / 3374.6525    /
    DATA C2  /    2.5403    /
    DATA C3  / 3114.834     / 
    DATA C4  /    0.278296  /
    DATA C5  /    1.0723E-3 /                                      
!---------------------------------------------------------- 
! CALCULATE ENVIRONMENTAL EQUIVALENT POTENTIAL TEMPERATURE.
!----------------------------------------------------------            
    EMIX = Q1 * P1 / (0.622 + Q1)
!------------------------------------------------
! FIND THE TEMPERATURE OF THE MIXTURE AT ITS LCL.
!------------------------------------------------
    ASTRT  = 1.E-3
    AINC   = 0.075
    A1     = EMIX / ALIQ
    TP     = (A1 - ASTRT) / AINC
    INDLU  = INT(TP) + 1
    VALUE  = (INDLU - 1) * AINC + ASTRT
    AINTRP = (A1 - VALUE) / AINC
    TLOG   = AINTRP * ALU(INDLU + 1) + (1 - AINTRP) * ALU(INDLU)
    TDPT   = (CLIQ - DLIQ * TLOG) / (BLIQ - TLOG)
    TLCL   = TDPT - (.212 + 1.571E-3 * (TDPT - T00) - 4.36E-4 * (T1 - T00)) * (T1 - TDPT)
    TSAT   = AMIN1(TLCL,T1)
    THT    = T1 * (P00 / P1) ** (0.2854 * (1. - 0.28 * Q1))                          
    THT1   = THT * EXP((C1 / TSAT - C2) * Q1 * (1. + 0.81 * Q1))                      
!
    RETURN
!
    END SUBROUTINE ENVIRTHT              
