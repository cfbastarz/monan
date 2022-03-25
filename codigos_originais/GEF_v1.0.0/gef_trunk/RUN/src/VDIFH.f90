!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief VERTICAL DIFFUSION OF MASS VARIABLES 
!> @details VERTICAL DIFFUSION OF MASS VARIABLES 
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
!> @param[in] APE  -
!> @param[in] DTQ2 -
!> @param[in] THZ0 - 
!> @param[in] QZ0  -
!> @param[in] AKHS - 
!> @param[in] CT   - 
!> @param[in] CKLQ -
!> @param[in] AKH  -
!> @param[in] LMHK -
!> @param[in] KTM  -
!> @param[in] Z    -
!> @param[out] T -
!> @param[out] Q -
!> @details <b>Use Module:</b>
!! @arg @c CTLBLK
!! @arg @c F77KINDS
!! @arg @c MPPSTAFF
!! @arg @c PARMETA
!> @details <b>Driver:</b> 
!! @arg @c TURBL
!> @details <b>Calls:</b>
!--------------------------------------------------------------------------------------------------     
    SUBROUTINE VDIFH(LMHK, KTM, DTQ2, THZ0, QZ0, AKHS, CT, CKLQ, T, Q, AKH, APE, Z)
!--------------------------------------------------------------------------------------------------
! SUBROUTINE VDIFH
!
! SUBROUTINE: VDIFH - VERTICAL DIFFUSION OF MASS VARIABLES 
! PROGRAMMER: ?????
! ORG: ?????
! DATE: ??-??-??
!
! ABSTRACT:
! VERTICAL DIFFUSION OF MASS VARIABLES
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
! APE  -
! DTQ2 -
! THZ0 - 
! QZ0  -
! AKHS - 
! CT   - 
! CKLQ -
! AKH  -
! LMHK -
! KTM  -
! Z    -
!
! OUTPUT ARGUMENT LIST:
! NONE
!
! INPUT/OUTPUT ARGUMENT LIST:
! T    -
! Q    -
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
    REAL   (KIND=R4)    , DIMENSION(LM)                                   , INTENT(IN)          ::&
    & APE
!
    REAL   (KIND=R4)                                                      , INTENT(IN)          ::&
    &  DTQ2   , THZ0    , QZ0     , AKHS    , CT      , CKLQ
!
    REAL   (KIND=R4)    , DIMENSION(LM1)                                  , INTENT(IN)          ::&
    & AKH
!
    REAL   (KIND=R4)    , DIMENSION(LM)                                   , INTENT(INOUT)       ::&
    & T       , Q
!
    INTEGER(KIND=I4)                                                      , INTENT(IN)          ::&
    & LMHK    , KTM  
!         
    REAL   (KIND=R4)    , DIMENSION(LM1)                                                        ::&
    & CM      , CR      , RST     , RSQ     , DTOZ    , AKCT
!
    REAL   (KIND=R4)    , DIMENSION(LP1)                                  , INTENT(IN)          ::&
    & Z
!
    INTEGER(KIND=I4)                                                                            ::&
    & LMHM    , LMHP    , L       , KT
!
    REAL   (KIND=R4)                                                                            ::&
    & DTDIF   , CF      , DTOZS   , AKHH    , AKQS    , CMB     , CMTB    , CMQB    , RSTB    ,   &
    & RSQB    , RCML    , DTOZL
!
    DTDIF = DTQ2 / FLOAT(KTM)
    LMHM  = LMHK - 1
    LMHP  = LMHK + 1
!
    DO L=1,LMHM
        DTOZ(L) = DTDIF    /  (Z(L) - Z(L+1))
          CR(L) = -DTOZ(L) * AKH(L)
        AKCT(L) =   AKH(L) *  (Z(L) - Z(L+2)) * 0.5 * CT
    END DO
!
    CM(1) = DTOZ(1) * AKH(1) + 1.
!
    DO KT=1,KTM
!
        RST(1) = -AKCT(1) * DTOZ(1) + T(1) * APE(1)
        RSQ(1) =     Q(1)
!
        DO L=2,LMHM
            DTOZL=DTOZ(L)
            CF     = -DTOZL    *        AKH(L-1) / CM(L-1)
             CM(L) = -CR(L-1)  * CF + ( AKH(L-1) +  AKH(L)) * DTOZL + 1.
            RST(L) = -RST(L-1) * CF + (AKCT(L-1) - AKCT(L)) * DTOZL + T(L) * APE(L)
            RSQ(L) = -RSQ(L-1) * CF +     Q(L)
        END DO
!
        DTOZS = DTDIF / (Z(LMHK) - Z(LMHP))
        AKHH  = AKH(LMHM)
! 
        CF   = -DTOZS * AKHH / CM(LMHM)
        AKQS =  AKHS  * CKLQ
! 
        CMB  =  CR(LMHM) * CF
        CMTB = -CMB + (AKHH + AKHS) * DTOZS + 1.
        CMQB = -CMB + (AKHH + AKQS) * DTOZS + 1.
!   
        RSTB = -RST(LMHM) * CF + (AKCT(LMHM) - AKHS * CT) * DTOZS + T(LMHK) * APE(LMHK)
        RSQB = -RSQ(LMHM) * CF +     Q(LMHK)
!
        T(LMHK) = (DTOZS * AKHS * THZ0 + RSTB) / (APE(LMHK) * CMTB)
        Q(LMHK) = (DTOZS * AKQS * QZ0  + RSQB) / CMQB
!
        DO L=LMHM,1,-1
            RCML = 1. / CM(L)
            T(L) = (-CR(L) * T(L+1) * APE(L+1) + RST(L)) * RCML / APE(L)
            Q(L) = (-CR(L) * Q(L+1)            + RSQ(L)) * RCML
        END DO
!
    END DO
!
    RETURN
!
    END SUBROUTINE VDIFH

