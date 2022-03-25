!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief ALLOW THE FREEZING OF LIQUID WATER IN THE UPDRAFT
!> @details ALLOW THE FREEZING OF LIQUID WATER IN THE UPDRAFT TO PROCEED AS AN APPROXIMATELY LINEAR 
!! FUNCTION OF TEMPERATURE IN THE TEMPERATURE RANGE TTFRZ TO TBFRZ. FOR COLDER TERMPERATURES, 
!! FREEZE ALL LIQUID WATER. THERMODYNAMIC PROPERTIES ARE STILL CALCULATED WITH RESPECT TO LIQUID 
!! WATER TO ALLOW THE USE OF LOOKUP TABLE TO EXTRACT TMP FROM THETAE.
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
!> @param[in] P - Significado de P
!> @param[in] QFRZ - Significado de GM
!> @param[in] ALIQ - Significado de ALIQ
!> @param[in] BLIQ - Significado de BLIQ
!> @param[in] CLIQ  - Significado de CLIQ
!> @param[in] DLIQ  - Significado de DLIQ
!> @param[out] TU - Significado de TU
!> @param[out] QU - Significado de QU
!> @param[out] QICE - Significado de QICE
!> @param[inout] THTEU - Significado de THTEU
!> @details <b>Use Module:</b>
!! @arg @c F77KINDS
!> @details <b>Driver:</b> 
!<
!> @details <b>Calls:</b>
!<
!--------------------------------------------------------------------------------------------------
    SUBROUTINE DTFRZNEW(TU, P, THTEU, QU, QFRZ, QICE, ALIQ, BLIQ, CLIQ, DLIQ)
!--------------------------------------------------------------------------------------------------
! SUBROUTINE DTFRZNEW
! 
! SUBPROGRAM: DTFRZNEW - ALLOW THE FREEZING OF LIQUID WATER IN THE UPDRAFT 
! PROGRAMMER: ????? 
! ORG: W/NP22
! DATE: ??-??-??
!
! ABSTRACT:  
! ALLOW THE FREEZING OF LIQUID WATER IN THE UPDRAFT TO PROCEED AS AN APPROXIMATELY LINEAR FUNCTION
! OF TEMPERATURE IN THE TEMPERATURE RANGE TTFRZ TO TBFRZ.
! FOR COLDER TERMPERATURES, FREEZE ALL LIQUID WATER.
! THERMODYNAMIC PROPERTIES ARE STILL CALCULATED WITH RESPECT TO LIQUID WATER TO ALLOW THE USE OF 
! LOOKUP TABLE TO EXTRACT TMP FROM THETAE.
!
! PROGRAM HISTORY LOG:
! 87-06-??  ?????   - ORIGINATOR
! 18-03-20  LUCCI   - MODERNIZATION OF THE CODE, INCLUDING:
!                     * F77 TO F90/F95
!                     * INDENTATION & UNIFORMIZATION CODE
!                     * REPLACEMENT OF COMMONS BLOCK FOR MODULES
!                     * DOCUMENTATION WITH DOXYGEN
!                     * OPENMP FUNCTIONALITY 
!
!
! INPUT ARGUMENT LIST:
! P     - 
! QFRZ  -
! ALIQ  - 
! BLIQ  -
! CLIQ  -
! DLIQ  -  
!
! OUTPUT ARGUMENT LIST:
! THTEU - 
!
! INPUT/OUTPUT ARGUMENT LIST:
! TU    -
! QU    -
! QICE  -  
!
! INPUT FILES:
! NONE
!
! OUTPUT FILES:
! NONE
!
! USE MODULES: F77KINDS
!
! DRIVER     : -----
!
! CALLS      : -----
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    REAL   (KIND=R4)                                                      , INTENT(INOUT)       ::&
    & TU      , QU      , QICE     
!
    REAL   (KIND=R4)                                                      , INTENT(IN)          ::&
    & P       , QFRZ    , ALIQ    , BLIQ    , CLIQ    , DLIQ
!
    REAL   (KIND=R4)                                                      , INTENT(OUT)         ::&
    & THTEU
!----------------
! LOCAL VARIABLES
!----------------
    REAL   (KIND=R4)                                                                            ::&
    & RLC     , RLS     , RLF     , CP      , A       , DTFRZ   , ES      , QS      , DQEVAP  ,   &
    & PI
!
    RLC =       2.5E6 - 2369.276 * (TU - 273.16)
    RLS = 2833922.    -  259.532 * (TU - 273.16)
!
    RLF = RLS - RLC
!
    CP = 1005.7 * (1. + 0.89 * QU)
!-------------------------------------------------- 
! A = D(ES)/DT IS THAT CALCULATED FROM BUCKS (1981)
! EMPIRICAL FORMULAS FOR SATURATION VAPOR PRESSURE
!--------------------------------------------------
    A = (CLIQ - BLIQ * DLIQ) / ((TU - DLIQ) * (TU - DLIQ))
!
    DTFRZ = RLF * QFRZ / (CP + RLS * QU * A)
!
    TU = TU + DTFRZ
!      
    ES = ALIQ * EXP((BLIQ * TU - CLIQ) / (TU - DLIQ))
    QS = ES * 0.622 / (P - ES)
!--------------------------------------------------------------------------------------------------
! FREEZING WARMS THE AIR AND IT BECOMES UNSATURATED. ASSUME THAT SOME OF THE LIQUID WATER THAT IS 
! AVAILABLE FOR FREEZING EVAPORATES TO MAINTAIN SATURATION. SINCE THIS WATER HAS ALREADY BEEN TRANS
! FERRED TO THE ICE CATEGORY, SUBTRACT IT FROM ICE CONCENTRATION, THEN SET UPDRAFT MIXING RATIO AT
! THE NEW TEMPERATURE TO THE SATURATION VALUE.
!--------------------------------------------------------------------------------------------------
    DQEVAP = QS   - QU
    QICE   = QICE - DQEVAP
    QU     = QU   + DQEVAP
    PI     = (1.E5 / P) ** (0.2854 * (1. - 0.28 * QU))
    THTEU  = TU * PI * EXP((3374.6525 / TU - 2.5403) * QU * (1. + 0.81 * QU))
!
    RETURN
!
    END SUBROUTINE DTFRZNEW
