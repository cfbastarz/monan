!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief DIFFUSION COEFICIENTS
!> @details DIFFUSION COEFICIENTS
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
!> @param[in] LMHK - MASS POINT MODEL SURFACE (LOWEST MODEL HEIGHT)
!> @param[in] GM - WIND SHEAR
!> @param[in] GH - VERTICAL GRADIENT OS POTENTIAL TEMPERATURE
!> @param[in] EL - 
!> @param[in] Q2 - TURBULENT KINETIC ENERGY (TRANSPOSED)
!> @param[in] Z  - INTERFACE HEIGHT (TRANSPOSED)
!> @param[out] AKM - SURFACE EXCHANGE COEFFICIENTS FOR U AND V
!> @param[out] AKH - SURFACE EXCHANGE COEFFICIENTS FOR T AND Q 
!> @details <b>Use Module:</b>
!! @arg @c F77KINDS
!! @arg @c PARMETA
!> @details <b>Driver:</b> 
!! @arg @c TURBL
!> @details <b>Calls:</b>
!<
!--------------------------------------------------------------------------------------------------
    SUBROUTINE DIFCOF(LMHK, GM, GH, EL, Q2, Z, AKM, AKH)
!--------------------------------------------------------------------------------------------------
! SUBROUTINE DIFCOF
! 
! SUBPROGRAM: DIFCOF - DIFFUSION COEFICIENTS
! PROGRAMMER: ?????
! ORG: ??????
! DATE: ??-??-??       
!     
! ABSTRACT:
! DIFFUSION COEFICIENTS
!
! PROGRAM HISTORY LOG:
! ??-??-??  ?????      - ORIGINATOR
! 18-03-20  LUCCI      - MODERNIZATION OF THE CODE, INCLUDING:
!                        * F77 TO F90/F95
!                        * INDENTATION & UNIFORMIZATION CODE
!                        * REPLACEMENT OF COMMONS BLOCK FOR MODULES
!                        * DOCUMENTATION WITH DOXYGEN
!                        * OPENMP FUNCTIONALITY
!
! INPUT ARGUMENT LIST:
! LMHK  - MASS POINT MODEL SURFACE (LOWEST MODEL HEIGHT)
! GM    - WIND SHEAR
! GH    - VERTICAL GRADIENT OS POTENTIAL TEMPERATURE
! EL    - 
! Q2    - TURBULENT KINETIC ENERGY (TRANSPOSED)
! Z     - INTERFACE HEIGHT (TRANSPOSED) 
!
! OUTPUT ARGUMENT LIST:
! AKM   - SURFACE EXCHANGE COEFFICIENTS FOR U AND V
! AKH   - SURFACE EXCHANGE COEFFICIENTS FOR T AND Q 
!
! INPUT/OUTPUT ARGUMENT LIST:
! NONE
!
! USE MODULES: F77KINDS
!              PARMETA
!  
! DRIVER     : TURBL
!
! CALLS      : -----
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
    USE PARMETA
!
    IMPLICIT NONE
!
    REAL   (KIND=R4)    , PARAMETER :: G    =  9.80616
    REAL   (KIND=R4)    , PARAMETER :: BETA =  1. / 270.
    REAL   (KIND=R4)    , PARAMETER :: BTG  = BETA * G
    REAL   (KIND=R4)    , PARAMETER :: PRT  =  1.0
    REAL   (KIND=R4)    , PARAMETER :: GAM1 =  0.2222222222222222222
    REAL   (KIND=R4)    , PARAMETER :: A1   =  0.659888514560862645
    REAL   (KIND=R4)    , PARAMETER :: A2   =  0.6574209922667784586
    REAL   (KIND=R4)    , PARAMETER :: B1   = 11.87799326209552761
    REAL   (KIND=R4)    , PARAMETER :: B2   =  7.226971804046074028
    REAL   (KIND=R4)    , PARAMETER :: C1   =  0.000830955950095854396                           
!--------------------------------------------
! COEFFICIENTS FOR THE SM AND SH DETERMINANTS
!--------------------------------------------
    REAL   (KIND=R4)    , PARAMETER :: BSMH = -3.*A1*A2*(3.*A2+3.*B2*C1+12.*A1*C1-B2)*BTG 
    REAL   (KIND=R4)    , PARAMETER :: CESM =  A1  * (1. - 3. * C1)
    REAL   (KIND=R4)    , PARAMETER :: BSHM =  18. * A1 * A1 * A2 * C1
    REAL   (KIND=R4)    , PARAMETER :: BSHH =   9. * A1 * A2 * A2 * BTG
    REAL   (KIND=R4)    , PARAMETER :: CESH =  A2  
!---------------------------------------------
! COEFFICIENTS OF THE TERMS IN THE DENOMINATOR
!---------------------------------------------
    REAL   (KIND=R4)    , PARAMETER :: ADNM = 18. * A1 * A1 * A2 * (B2 - 3. * A2) * BTG
    REAL   (KIND=R4)    , PARAMETER :: ADNH =  9. * A1 * A2 * A2 * (12. * A1 + 3. * B2) * BTG * BTG
    REAL   (KIND=R4)    , PARAMETER :: BDNM =  6. * A1 * A1
    REAL   (KIND=R4)    , PARAMETER :: BDNH =  3. * A2 * (7. * A1 + B2) * BTG                          
!
    INTEGER(KIND=I4)                                                      , INTENT(IN)          ::&
    & LMHK
!
    REAL   (KIND=R4)    , DIMENSION(LM1)                                  , INTENT(IN)          ::&
    & GM      , GH      , EL
!
    REAL   (KIND=R4)    , DIMENSION(LM)                                   , INTENT(IN)          ::&
    & Q2
!
    REAL   (KIND=R4)    , DIMENSION(LP1)                                  , INTENT(IN)          ::&
    & Z
!
    REAL   (KIND=R4)    , DIMENSION(LM1)                                  , INTENT(OUT)         ::&
    & AKM     , AKH
!
    INTEGER(KIND=I4)                                                                            ::&
    & LMHM    , LMHP    , L
!
    REAL   (KIND=R4)                                                                            ::&
    & ELL     , ELOQ2   , ELOQ4   , GML     , GHL     , ADEN    , BDEN    , CDEN    , BESM    ,   &
    & BESH    , RDEN    , ESM     , ESH     , RDZ     , Q1L     , ELQDZ   , X      , Y
!
    LMHM = LMHK - 1
    LMHP = LMHK + 1
!
    DO L=1,LMHM
        ELL = EL(L)
!
        ELOQ2 = ELL   * ELL  / Q2(L)
        ELOQ4 = ELOQ2 * ELOQ2
!
        GML = GM(L)
        GHL = GH(L)
!---------------------------------------------
! COEFFICIENTS OF THE TERMS IN THE DENOMINATOR
!---------------------------------------------
        ADEN = (ADNM * GML + ADNH * GHL) * GHL
        BDEN =  BDNM * GML + BDNH * GHL
        CDEN =  1.
!------------------------------------
! COEFFICIENTS FOR THE SM DETERMINANT
!------------------------------------
        BESM = BSMH * GHL
!------------------------------------
! COEFFICIENTS FOR THE SH DETERMINANT
!------------------------------------
        BESH = BSHM * GML + BSHH * GHL
!---------------
! 1./DENOMINATOR 
!---------------
        X = (ADEN * ELOQ4 + BDEN * ELOQ2 + CDEN)
!      
        IF (X == 0) THEN
            PRINT *, '(ADEN*ELOQ4+BDEN*ELOQ2+CDEN)=0', X, ADEN, ELOQ4, BDEN, ELOQ2, CDEN
!       
            X = 0.2
        END IF
!
        RDEN = 1. / X
!----------
! SM AND SH
!----------
        ESM = (BESM * ELOQ2 + CESM) * RDEN
        ESH = (BESH * ELOQ2 + CESH) * RDEN
!-----------------------
! DIFFUSION COEFFICIENTS 
!-----------------------
        Y = Z(L) - Z(L+2)
!-------
! DRAGAN
!-------
        IF (Y == 0) THEN
            PRINT *, 'Z(L)-Z(L+2)=0', Y, Z(L), Z(L+2), L
!
            Y = Z(L-1) - Z(L+1)
!
        END IF
!
        RDZ = 2. / Y
!-------
! DRAGAN
!-------
        IF (Q2(L) < 0) THEN
            PRINT*, 'Q2(L)<0', Q2(L), L
        END IF
!      
        Q1L = SQRT(Q2(L))
!       
        ELQDZ = ELL * Q1L * RDZ
!             
        AKM(L) = ELQDZ * ESM
        AKH(L) = ELQDZ * ESH
!
    END DO
!
    RETURN
!
    END SUBROUTINE DIFCOF
