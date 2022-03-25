!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Q2 PRODUCTION/DISSIPATION
!> @details Q2 PRODUCTION/DISSIPATION.
!> @author ORIGINATOR - JANJIC
!> @date 87-06-?? \n
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
!> @param[in]    DTQ2  - Significado de DTQ2
!> @param[in]    USTAR - Significado de USTAR
!> @param[in]    GM    - Significado de GM
!> @param[in]    GH    - Significado de GH
!> @param[out]   EL    - Significado de EL
!> @param[inout] Q2    - Significado de Q2
!> @details <b>Use Module:</b>
!! @arg @c CTLBLK
!! @arg @c F77KINDS
!! @arg @c MPPSTAFF
!! @arg @c PARMETA
!> @details <b>Driver:</b> 
!! @arg @c TURBL
!> @details <b>Calls:</b>
!--------------------------------------------------------------------------------------------------     
	SUBROUTINE PRODQ2(LMHK, DTQ2, USTAR, GM, GH, EL, Q2)
!--------------------------------------------------------------------------------------------------
! SUBROUTINE PRODQ2
!
! SUBROUTINE: PRODQ2 - Q2 PRODUCTION/DISSIPATION
! PROGRAMMER: ?????
! ORG: ?????
! DATE: ??-??-??
!
! ABSTRACT:
! LEVEL 2.5 Q2 PRODUCTION/DISSIPATION 
!
! PROGRAM HISTORY LOG:
! 94-??-??  ?????      - ORIGINATOR
! 18-01-15  LUCCI      - MODERNIZATION OF THE CODE, INCLUDING:
!                        * F77 TO F90/F95
!                        * INDENTATION & UNIFORMIZATION CODE
!                        * REPLACEMENT OF COMMONS BLOCK FOR MODULES
!                        * DOCUMENTATION WITH DOXYGEN
!                        * OPENMP FUNCTIONALITY
!
! INPUT ARGUMENT LIST:
! LMHK  - 
! DTQ2  -
! USTAR -
! GM    -
! GH    - 
!
! OUTPUT ARGUMENT LIST:
! EL    -
!
! INPUT/OUTPUT ARGUMENT LIST:
! Q2    -
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
!--------------------------------------------------------------------------------------------------
    USE CTLBLK
    USE F77KINDS
    USE MPPSTAFF
    USE PARMETA
!
    IMPLICIT NONE
!
    REAL   (KIND=R4)    , PARAMETER :: EPSQ2  =  0.2
    REAL   (KIND=R4)    , PARAMETER :: EPSL   =  0.32
    REAL   (KIND=R4)    , PARAMETER :: EPSTRB =  1.E-24
    REAL   (KIND=R4)    , PARAMETER :: EPS1   =  1.E-12
    REAL   (KIND=R4)    , PARAMETER :: EPS2   =  0.
!
    REAL   (KIND=R4)    , PARAMETER :: G      =  9.8
    REAL   (KIND=R4)    , PARAMETER :: BETA   =  1. / 270.
    REAL   (KIND=R4)    , PARAMETER :: BTG    = BETA * G 
    REAL   (KIND=R4)    , PARAMETER :: PRT    =  1.0 
    REAL   (KIND=R4)    , PARAMETER :: GAM1   =  0.2222222222222222222
    REAL   (KIND=R4)    , PARAMETER :: A1     =  0.659888514560862645
    REAL   (KIND=R4)    , PARAMETER :: A2     =  0.6574209922667784586 
    REAL   (KIND=R4)    , PARAMETER :: B1     = 11.87799326209552761
    REAL   (KIND=R4)    , PARAMETER :: B2     =  7.226971804046074028
    REAL   (KIND=R4)    , PARAMETER :: C1     =  0.000830955950095854396
    REAL   (KIND=R4)    , PARAMETER :: RB1    = 1. / B1    
!------------------------------------------- 
! COEFFICIENTS OF THE TERMS IN THE NUMERATOR
!-------------------------------------------
    REAL   (KIND=R4)    , PARAMETER :: ANMM  = -3. * A1 * A2                                      &
    &                                        * (3. * A2 + 3. * B2 * C1 + 18. * A1 * C1 - B2) * BTG
!
    REAL   (KIND=R4)    , PARAMETER :: ANMH  = -9. * A1 * A2 * A2 * BTG * BTG
! 
    REAL   (KIND=R4)    , PARAMETER :: BNMM  =  A1 * (1. - 3. * C1) 
    REAL   (KIND=R4)    , PARAMETER :: BNMH  = -A2 * BTG 
!---------------------------------------------                                                                                                                                     
! COEFFICIENTS OF THE TERMS IN THE DENOMINATOR
!--------------------------------------------- 
    REAL   (KIND=R4)    , PARAMETER :: ADNM  = 18. * A1 * A1 * A2 * (B2 - 3. * A2) * BTG              
    REAL   (KIND=R4)    , PARAMETER :: ADNH  =  9. * A1 * A2 * A2 * (12. * A1 + 3. * B2) * BTG * BTG 
    REAL   (KIND=R4)    , PARAMETER :: BDNM  =  6. * A1 * A1            
    REAL   (KIND=R4)    , PARAMETER :: BDNH  =  3. * A2 * (7. * A1 + B2) * BTG
!-----------------------------------------                                                                                                                                        
! COEFFICIENTS OF THE EQUILIBRIUM EQUATION 
!-----------------------------------------                                                                                                                                 
    REAL   (KIND=R4)    , PARAMETER :: AEQM  =  3. * A1 * A2 * B1                                 &
    &                                        * (3. * A2 + 3. * B2 * C1 + 18. * A1 * C1 - B2)      &
    &                                        * BTG + 18. * A1 * A1 * A2 * (B2 - 3. * A2) * BTG
!
    REAL   (KIND=R4)    , PARAMETER :: AEQH  =  9. * A1 * A2 * A2 * B1 * BTG * BTG                &
    &                                        +  9. * A1 * A2 * A2 * (12. * A1 + 3. * B2)          &
    &                                        * BTG * BTG
!
    REAL   (KIND=R4)    , PARAMETER :: BEQM  =- A1 * B1 * (1. - 3. * C1) + 6. * A1 * A1
    REAL   (KIND=R4)    , PARAMETER :: BEQH  =  A2 * B1 * BTG + 3. * A2 * (7. * A1 + B2) * BTG
!-------------------------- 
! FORBIDDEN TURBULENCE AREA
!--------------------------                                                                                                                                       
    REAL   (KIND=R4)    , PARAMETER :: REQU  = -AEQH / AEQM * 1.02
    REAL   (KIND=R4)    , PARAMETER :: EPSGH = 1.E-9
!
    REAL   (KIND=R4)    , DIMENSION(LM)                                   , INTENT(OUT)         ::&
    & Q2
!
    REAL   (KIND=R4)    , DIMENSION(LM1)                                  , INTENT(IN)          ::&
    & GM      , GH
!
    REAL   (KIND=R4)    , DIMENSION(LM1)                                  , INTENT(OUT)         ::&
    & EL
!------------------------
! IMPLICIT NONE VARIABLES
!------------------------
    INTEGER(KIND=I4)                                                      , INTENT(IN)          ::&
    & LMHK
!
    REAL   (KIND=R4)                                                      , INTENT(IN)          ::&
    & DTQ2    , USTAR
!
    INTEGER(KIND=I4)                                                                            ::&
    & LMHM    , L        
!
    REAL   (KIND=R4)                                                                            ::&
    & GML     , GHL     , AEQU    , BEQU    , EQOL2   , ANUM    , BNUM    , ADEN    , BDEN    ,   &
    & CDEN    , ARHS    , BRHS    , CRHS    , DLOQ1   , ELOQ21  , ELOQ11  , ELOQ31  , ELOQ41  ,   &
    & ELOQ51  , RDEN1   , RHSP1   , ELOQ12  , ELOQ22  , ELOQ32  , ELOQ42  , ELOQ52  , RDEN2   ,   &
    & RHS2    , RHSP2   , RHST2   , ELOQ13  , ELOQN 
!
    LMHM = LMHK - 1
!
    DO 150 L=1,LMHM
        GML = GM(L)
        GHL = GH(L)
!-----------------------------------------
! COEFFICIENTS OF THE EQUILIBRIUM EQUATION
!-----------------------------------------
        AEQU =(AEQM * GML + AEQH * GHL) * GHL
        BEQU = BEQM * GML + BEQH * GHL
!----------------------------- 
! EQUILIBRIUM SOLUTION FOR L/Q
!-----------------------------  
        EQOL2 = -0.5 * BEQU + SQRT(BEQU * BEQU * 0.25 - AEQU)
!---------------------------------- 
! IS THERE PRODUCTION/DISSIPATION ?
!----------------------------------
        IF ((GML+GHL*GHL <= EPSTRB) .OR. (GHL >= EPSGH .AND. GML/GHL <= REQU)                     &
    &                               .OR. (EQOL2 <= EPS2))                      THEN
!-------------- 
! NO TURBULENCE
!--------------
            Q2(L) = EPSQ2
            EL(L) = EPSL
!--------------------------------
! END OF THE NO TURBULENCE BRANCH
!--------------------------------
        ELSE
!-------------------------------------------
! COEFFICIENTS OF THE TERMS IN THE NUMERATOR
!-------------------------------------------
            ANUM = (ANMM * GML + ANMH * GHL) * GHL
            BNUM =  BNMM * GML + BNMH * GHL
!---------------------------------------------
! COEFFICIENTS OF THE TERMS IN THE DENOMINATOR
!---------------------------------------------
            ADEN = (ADNM * GML + ADNH * GHL) * GHL
            BDEN =  BDNM * GML + BDNH * GHL
            CDEN =  1.
!----------------------------------------------------
! COEFFICIENTS OF THE NUMERATOR OF THE LINEARIZED EQ.
!----------------------------------------------------
            ARHS = -(ANUM * BDEN - BNUM * ADEN) * 2.
            BRHS = - ANUM * 4.
            CRHS = - BNUM * 2.
!--------------------- 
! INITIAL VALUE OF L/Q
!--------------------- 
            DLOQ1 = EL(L) / SQRT(Q2(L))
!------------------------------- 
! FIRST ITERATION FOR L/Q, RHS=0
!-------------------------------
            ELOQ21 = 1. / EQOL2
            ELOQ11 = SQRT(ELOQ21)
            ELOQ31 = ELOQ21 * ELOQ11
            ELOQ41 = ELOQ21 * ELOQ21
            ELOQ51 = ELOQ21 * ELOQ31
!--------------- 
! 1./DENOMINATOR
!--------------- 
            RDEN1 = 1. / (ADEN * ELOQ41 + BDEN * ELOQ21 + CDEN)
!--------------  
! D(RHS)/D(L/Q)
!-------------- 
            RHSP1 = (ARHS * ELOQ51 + BRHS * ELOQ31 + CRHS * ELOQ11) * RDEN1 * RDEN1
!--------------------- 
! FIRST-GUESS SOLUTION 
!---------------------
            ELOQ12 = ELOQ11 + (DLOQ1 - ELOQ11) * EXP(RHSP1 * DTQ2)
!
            ELOQ12 = AMAX1(ELOQ12, EPS1)
!-------------------------
! SECOND ITERATION FOR L/Q 
!-------------------------
            ELOQ22 = ELOQ12 * ELOQ12
            ELOQ32 = ELOQ22 * ELOQ12
            ELOQ42 = ELOQ22 * ELOQ22
            ELOQ52 = ELOQ22 * ELOQ32
!--------------- 
! 1./DENOMINATOR 
!---------------
            RDEN2 = 1. / (ADEN * ELOQ42 + BDEN * ELOQ22 + CDEN)
!
            RHS2  = -(ANUM * ELOQ42 + BNUM * ELOQ22) * RDEN2 + RB1
            RHSP2 =  (ARHS * ELOQ52 + BRHS * ELOQ32  + CRHS  * ELOQ12) * RDEN2 * RDEN2
            RHST2 =   RHS2 / RHSP2
!-------------------
! CORRECTED SOLUTION 
!-------------------
            ELOQ13 = ELOQ12 - RHST2 + (RHST2 + DLOQ1 - ELOQ12) * EXP(RHSP2 * DTQ2)
!
            ELOQ13 = AMAX1(ELOQ13, EPS1)
!---------------------------------------
! TWO ITERATIONS IS ENOUGH IN MOST CASES
!---------------------------------------
            ELOQN = ELOQ13
!
            IF (ELOQN > EPS1) THEN
                Q2(L) = EL(L) * EL(L) / (ELOQN * ELOQN)
                Q2(L) = AMAX1(Q2(L), EPSQ2)
            ELSE
                Q2(L) = EPSQ2
            END IF
!------------------------ 
! END OF TURBULENT BRANCH
!------------------------
        END IF
!-----------------------------------
! END OF PRODUCTION/DISSIPATION LOOP
!-----------------------------------
150 END DO
!-------------------------------- 
! LOWER BOUNDARY CONDITION FOR Q2
!--------------------------------
    Q2(LMHK) = AMAX1(B1 ** (2. / 3.) * USTAR * USTAR, EPSQ2)
!
    RETURN
!
    END SUBROUTINE PRODQ2
