!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Inserir Brief de VDIFQ
!> @details Inserir Details de VDIFQ 
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
!> @param[in] LMHK  - MASS POINT MODEL SURFACE (LOWEST MODEL HEIGHT)
!> @param[in] KTM   -
!> @param[in] DTQ2  - PHYSICS TINE STEP
!> @param[in] EL    - 
!> @param[in] Z     - INTERFACE HEIGHT
!> @param[out] Q2   - TURBULENT KINETIC ENERGY
!> @details <b>Use Module:</b>
!! @arg @c F77KINDS
!! @arg @c PARMETA
!> @details <b>Driver:</b> 
!! @arg @c TURBL
!> @details <b>Calls:</b>
!--------------------------------------------------------------------------------------------------     
    SUBROUTINE VDIFQ(LMHK, KTM, DTQ2, Q2, EL, Z)
!--------------------------------------------------------------------------------------------------
! SUBROUTINE VDIFQ
!
! SUBPROGRAM: VDIFQ - ?????
! PROGRAMMER: ?????   
! ORG: ?????
! DATE: ??-??-??
! 
! ABSTRACT: 
! ?????
!
! PROGRAM HISTORY LOG:
! ??-??-??  ?????       - ORIGINATOR
! 18-01-15  LUCCI       - MODERNIZATION OF THE CODE, INCLUDING:
!                         * F77 TO F90/F95
!                         * INDENTATION & UNIFORMIZATION CODE
!                         * REPLACEMENT OF COMMONS BLOCK FOR MODULES
!                         * DOCUMENTATION WITH DOXYGEN
!                         * OPENMP FUNCTIONALITY
!
! INPUT ARGUMENT LIST:
! LMHK  - MASS POINT MODEL SURFACE (LOWEST MODEL HEIGHT)
! KTM   -
! DTQ2  - PHYSICS TINE STEP
! EL    - 
! Z     - INTERFACE HEIGHT
!
! OUTPUT ARGUMENT LIST:
! NONE
!
! INPUT/OUTPUT ARGUMENT LIST:
! Q2    - TURBULENT KINETIC ENERGY
!
! OUTPUT FILES:
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
    REAL   (KIND=R4)    , PARAMETER :: ESQ  = 0.20
    REAL   (KIND=R4)    , PARAMETER :: ELZ0 = 0.
!
    REAL   (KIND=R4)    , DIMENSION(LM)                                   , INTENT(INOUT)       ::&
    & Q2
!
    REAL   (KIND=R4)    , DIMENSION(LM1)                                  , INTENT(IN)          ::&
    & EL
!
    REAL   (KIND=R4)    , DIMENSION(LP1)                                  , INTENT(IN)          ::&
    & Z
!
    REAL   (KIND=R4)    , DIMENSION(LM-2)                                                       ::&
    & CM      , CR      , RSQ2    , AKQ     , DTOZ    
!------------------------
! IMPLICIT NONE VARIABLES
!------------------------
    INTEGER(KIND=I4)                                                      , INTENT(IN)          ::&
    & LMHK    , KTM  
!
    REAL   (KIND=R4)                                                      , INTENT(IN)          ::&
    & DTQ2 
!
    INTEGER(KIND=I4)                                                                            ::&
    & LMHM    , LMH2    , LMHP    , KT      , L       , IVI
!
    REAL   (KIND=R4)                                                                            ::&
    & DTDIF   , CF      , DTOZS   , AKQS
!
    DTDIF = DTQ2 / FLOAT(KTM)
    LMHM  = LMHK - 1
    LMH2  = LMHK - 2
    LMHP  = LMHK + 1
!
    DO KT=1,KTM
!
        DO L=1,LMH2
            DTOZ(L) = (DTDIF + DTDIF) / (Z(L) - Z(L+2))
!
             AKQ(L) = SQRT((Q2(L  ) +  Q2(L+1)) * 0.5) * (EL(L) + EL(L+1)) * 0.5 * ESQ            &
    &               /       (Z(L+1) -   Z(L+2))
!
              CR(L) =    -DTOZ(L)   * AKQ(L)
        END DO

          CM(1) = DTOZ(1) * AKQ(1) + 1.
        RSQ2(1) =   Q2(1)
!
        DO L=2,LMH2
                 CF = -DTOZ(L  ) *       AKQ(L-1) /  CM(L-1)
              CM(L) =   -CR(L-1) * CF + (AKQ(L-1) + AKQ(L  )) * DTOZ(L) + 1.
            RSQ2(L) = -RSQ2(L-1) * CF +   Q2(L  )
        END DO
!
        DTOZS = (DTDIF + DTDIF) / (Z(LMHM) - Z(LMHP))
        AKQS  = SQRT((Q2(LMHM) + Q2(LMHK)) * 0.5) * (EL(LMHM) + ELZ0) * 0.5 * ESQ                 &
    &         /       (Z(LMHK) -  Z(LMHP))
!
           CF  =-DTOZS * AKQ(LMH2) / CM(LMH2)
!
        Q2(LMHM) = (DTOZS * AKQS * Q2(LMHK) - RSQ2(LMH2) * CF + Q2(LMHM))                         &
    &            / ((AKQ(LMH2) + AKQS) * DTOZS - CR(LMH2) * CF + 1.)
!
        DO IVI=1,LMH2
            L     = LMHM - IVI
            Q2(L) = (-CR(L) * Q2(L+1) + RSQ2(L)) / CM(L)
        END DO
!
    END DO 
!
    RETURN
!
    END SUBROUTINE VDIFQ

