!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Inserir Brief de VDIFV
!> @details Inserir Details de VDIFV 
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
!> @param[in] LMVK  - MASS POINT MODEL SURFACE (LOWEST MODEL HEIGHT)
!> @param[in] KTM   - 
!> @param[in] DTQ2  - PHYSICS TINE STEP
!> @param[in] UZ0   -
!> @param[in] VZ0   -
!> @param[in] AKMS  - SURFACE COEF. FOR U AND V DIVIDED BY DELTA Z
!> @param[in] AKM   - SURFACE EXCHANGE COEF. FOR U AND V
!> @param[in] Z     - INTERFACE HEIGHT
!> @param[out] U    - U AT H POINT (TRANSPOSED)
!> @param[out] V    - V AT H POINT (TRANSPOSED)
!> @details <b>Use Module:</b>
!! @arg @c CTLBLK
!! @arg @c F77KINDS
!! @arg @c MPPSTAFF
!! @arg @c PARMETA
!> @details <b>Driver:</b> 
!! @arg @c TURBL
!> @details <b>Calls:</b>
!--------------------------------------------------------------------------------------------------       
    SUBROUTINE VDIFV(LMVK, KTM, DTQ2, UZ0, VZ0, AKMS, U, V, AKM, Z)
!--------------------------------------------------------------------------------------------------
! SUBROUTINE VDIFV
!
! SUBPROGRAM: VDIFV - VERTICAL DIFFUSION
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
! LMVK  - MASS POINT MODEL SURFACE (LOWEST MODEL HEIGHT)
! KTM   - 
! DTQ2  - PHYSICS TINE STEP
! UZ0   -
! VZ0   -
! AKMS  - SURFACE COEF. FOR U AND V DIVIDED BY DELTA Z
! AKM   - SURFACE EXCHANGE COEF. FOR U AND V
! Z     - INTERFACE HEIGHT
!
! OUTPUT ARGUMENT LIST:
! NONE
!
! INPUT/OUTPUT ARGUMENT LIST:
! U     - U AT H POINT (TRANSPOSED)
! V     - V AT H POINT (TRANSPOSED)
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
    REAL   (KIND=R4)    , DIMENSION(LM)                                   , INTENT(INOUT)       ::&
    & U       , V
!
    REAL   (KIND=R4)    , DIMENSION(LM1)                                                        ::&
    & AKM     , CM      , CR      , RSU     , RSV     , DTOZ
!
    REAL   (KIND=R4)    , DIMENSION(LP1)                                  , INTENT(IN)          ::&
    & Z
!
    INTEGER(KIND=I4)                                                      , INTENT(IN)          ::&
    & LMVK    , KTM
!
    INTEGER(KIND=I4)                                                                            ::&
    & LMVM    , LMVP    , KT      , L       , IVI
!
    REAL   (KIND=R4)                                                      , INTENT(IN)          ::&
    & DTQ2    , UZ0     , VZ0     , AKMS    
!
    REAL   (KIND=R4)                                                                            ::&
    & DTDIF   , CF      , DTOZL   , DTOZS   , AKMH    , RCMVB   , DTOZAK  , RCML
!
    DTDIF = DTQ2 / FLOAT(KTM)
    LMVM  = LMVK - 1
    LMVP  = LMVK + 1
!
    DO KT=1,KTM
!
        DO L=1,LMVM
            DTOZ(L) =  DTDIF   /  (Z(L) - Z(L+1))
              CR(L) = -DTOZ(L) * AKM(L)
        END DO
!
         CM(1) = DTOZ(1) * AKM(1) + 1.
        RSU(1) =    U(1)
        RSV(1) =    V(1)
!
        DO L=2,LMVM
            DTOZL=DTOZ(L)
                CF = -DTOZL * AKM(L-1) / CM(L-1)
             CM(L) =  -CR(L-1) * CF + (AKM(L-1) + AKM(L)) * DTOZL + 1.
            RSU(L) = -RSU(L-1) * CF + U(L)
            RSV(L) = -RSV(L-1) * CF + V(L)
        END DO
!
        DTOZS = DTDIF / (Z(LMVK) - Z(LMVP))
         AKMH = AKM(LMVM)
!
        CF     = -DTOZS * AKMH / CM(LMVM)
        RCMVB  = 1. / ((AKMH + AKMS) * DTOZS - CR(LMVM) * CF + 1.)
        DTOZAK = DTOZS * AKMS
!
        U(LMVK) = (DTOZAK * UZ0 - RSU(LMVM) * CF + U(LMVK)) * RCMVB
        V(LMVK) = (DTOZAK * VZ0 - RSV(LMVM) * CF + V(LMVK)) * RCMVB
!
        DO IVI=1,LMVM
            L    = LMVK - IVI
            RCML = 1. / CM(L)
            U(L)=(-CR(L) * U(L+1) + RSU(L)) * RCML
            V(L)=(-CR(L) * V(L+1) + RSV(L)) * RCML
        END DO
!
    END DO
!
    RETURN
!
    END SUBROUTINE VDIFV
