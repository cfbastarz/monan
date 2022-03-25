!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Inserir Brief de INVERT
!> @details Inserir Details de INVERT
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
!> @param[in] IMAX  -
!> @param[in] LMAX  -
!> @param[inout] ARR  -
!> @details <b>Use Module:</b>
!! @arg @c F77KINDS
!> @details <b>Driver:</b> 
!! @arg @c BOCOVMPI
!! @arg @c BOCOV_HMPI
!> @details <b>Calls:</b>
!-------------------------------------------------------------------------------------------------- 
    SUBROUTINE INVERT(ARR, IMAX, LMAX)
!--------------------------------------------------------------------------------------------------
! SUBROUTINE INVERT
!
! SUBROUTINE: INVERT - 
! PROGRAMMER: ?????
! ORG: ?????
! DATE: ??-??-??
!
! ABSTRACT:
! ?????
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
! IMAX -
! LMAX -
!
! OUTPUT ARGUMENT LIST:
! ARR -
!
! INPUT/OUTPUT ARGUMENT LIST:
! NONE
!
! USE MODULES: F77KINDS   
!
! DRIVER     : GEF
!
! CALLS      : -----
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    INTEGER(KIND=I4)                                                      , INTENT(IN)          ::&
    & IMAX    , LMAX
!
    REAL   (KIND=R4)    , DIMENSION(IMAX, LMAX)                           , INTENT(INOUT)       ::&
    & ARR
!
    REAL   (KIND=R4)    , DIMENSION(IMAX)                                                       ::&
    & TPARR
!
    INTEGER(KIND=I4)                                                                            ::&
    & I       , L
!
    DO L=1,LMAX
        TPARR = ARR(:,L)
!
        DO I=1,IMAX
            ARR(I,L) = TPARR(IMAX-I+1)
        END DO
!
    END DO
!
    END SUBROUTINE INVERT
!
!
!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief AVERAGING OF FOUR SCALAR POINTS => RESULT IN THE WIND POINTS
!> @details AVERAGING OF FOUR SCALAR POINTS => RESULT IN THE WIND POINTS
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
!> @param[in] A  -
!> @param[in] IMAX  -
!> @param[in] JMAX  -
!> @param[inout] AXY  -
!> @details <b>Use Module:</b>
!! @arg @c F77KINDS
!> @details <b>Driver:</b> 
!! @arg @c ADJUST
!! @arg @c HZADV
!! @arg @c HZADVQ
!! @arg @c OUTSD
!! @arg @c VTADV
!> @details <b>Calls:</b>
!-------------------------------------------------------------------------------------------------- 
    SUBROUTINE AVRH(A, AXY, IMAX, JMAX)
!--------------------------------------------------------------------------------------------------
! SUBROUTINE AVRH
!
! SUBROUTINE: AVRH - AVERAGING OF FOUR SCALAR POINTS => RESULT IN THE WIND POINTS
! PROGRAMMER: ?????
! ORG: ?????
! DATE: ??-??-??
!
! ABSTRACT:
! AVERAGING OF FOUR SCALAR POINTS => RESULT IN THE WIND POINTS
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
! A    - 
! IMAX - 
! JMAX - 
!
! OUTPUT ARGUMENT LIST:
! NONE
!
! INPUT/OUTPUT ARGUMENT LIST:
! AXY  -
!
! USE MODULES: F77KINDS   
!
! DRIVER     :  ADJUST
!               HZADV
!               HZADVQ
!               OUTSD
!               VTADV
!
! CALLS      : -----
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE

    INTEGER(KIND=I4)                                                      , INTENT(IN)          ::&
    & IMAX    , JMAX
!
    INTEGER(KIND=I4)                                                                            ::&
    & I       , J
!
    REAL   (KIND=R4)    , DIMENSION(0:IMAX+1, 0:JMAX+1)                                         ::&
    & AXP     
!
!    REAL   (KIND=R4)    , DIMENSION(0:IMAX+1, 0:JMAX+1)                   , INTENT(IN)          ::&
    REAL   (KIND=R4)    , DIMENSION(0:IMAX+1, 0:JMAX+1)                                         ::&
    & A       
!
!    REAL   (KIND=R4)    , DIMENSION(0:IMAX+1, 0:JMAX+1)                   , INTENT(INOUT)       ::&
    REAL   (KIND=R4)    , DIMENSION(0:IMAX+1, 0:JMAX+1)                                         ::&
    & AXY
!
    DO J=0,JMAX+1
        DO I=0,IMAX
            AXP(I,J) = A(I,J) + A(I+1,J)
        END DO
    END DO
!
    DO J=0,JMAX
        DO I=0,IMAX
            AXY(I,J) = AXP(I,J) + AXP(I,J+1)
        END DO
    END DO
!
    RETURN
!
    END SUBROUTINE AVRH        
!
!
!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief AVERAGING OF FOUR WIND POINTS => RESULT IN THE SCALAR POINTS
!> @details AVERAGING OF FOUR WIND POINTS => RESULT IN THE SCALAR POINTS
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
!> @param[in] V  -
!> @param[in] IMAX  -
!> @param[in] JMAX  -
!> @param[inout] VXY  -
!> @details <b>Use Module:</b>
!! @arg @c F77KINDS
!> @details <b>Driver:</b> 
!! @arg @c HZADV
!! @arg @c KINEN
!> @details <b>Calls:</b>
!-------------------------------------------------------------------------------------------------- 
    SUBROUTINE AVRV(V, VXY, IMAX, JMAX)
!--------------------------------------------------------------------------------------------------
! SUBROUTINE AVRV
!
! SUBROUTINE: AVRV - AVERAGING OF FOUR WIND POINTS => RESULT IN THE SCALAR POINTS
! PROGRAMMER: ?????
! ORG: ?????
! DATE: ??-??-??
!
! ABSTRACT:
! AVERAGING OF FOUR WIND POINTS => RESULT IN THE SCALAR POINTS
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
! V    - 
! IMAX - 
! JMAX - 
!
! OUTPUT ARGUMENT LIST:
! NONE
!
! INPUT/OUTPUT ARGUMENT LIST:
! VXY  - 
!
! USE MODULES: F77KINDS   
!
! DRIVER     : HZADV
!              KINEN
!
! CALLS      : -----
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    INTEGER(KIND=I4)                                                      , INTENT(IN)          ::&
    & IMAX    , JMAX
!
    INTEGER(KIND=I4)                                                                            ::&
    & I       , J
!
    REAL   (KIND=R4)    , DIMENSION(0:IMAX+1,0:JMAX+1)                                          ::&
    & VXP 
!
    REAL   (KIND=R4)    , DIMENSION(0:IMAX+1,0:JMAX+1)                    , INTENT(IN)          ::&
    & V
!
    REAL   (KIND=R4)    , DIMENSION(0:IMAX+1,0:JMAX+1)                    , INTENT(INOUT)       ::&
    & VXY
!
    LOGICAL(KIND=L4)                                                                            ::&
    & ISCORNER
!
    DO J=0,JMAX
        DO I=1,IMAX
            VXP(I,J) = V(I,J) + V(I-1,J)
        END DO
    END DO
!
    DO J=1,JMAX
        DO I=1,IMAX
            VXY(I,J) = VXP(I,J) + VXP(I,J-1)
        END DO
    END DO
!
    RETURN
!
    END SUBROUTINE AVRV       
!
!
!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief AVERAGING OF FOUR SCALAR POINTS => RESULT IN THE SCALAR POINTS
!> @details AVERAGING OF FOUR SCALAR POINTS => RESULT IN THE SCALAR POINTS
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
!> @param[in] H  -
!> @param[in] IMAX  -
!> @param[in] JMAX  -
!> @param[inout] HXY  -
!> @details <b>Use Module:</b>
!! @arg @c F77KINDS
!> @details <b>Driver:</b> 
!> @details <b>Calls:</b>
!-------------------------------------------------------------------------------------------------- 
    SUBROUTINE AVRHH(H, HXY, IMAX, JMAX)
!--------------------------------------------------------------------------------------------------
! SUBROUTINE AVRV
!
! SUBROUTINE: AVRV - AVERAGING OF FOUR SCALAR POINTS => RESULT IN THE SCALAR POINTS
! PROGRAMMER: ?????
! ORG: ?????
! DATE: ??-??-??
!
! ABSTRACT:
! AVERAGING OF FOUR SCALAR POINTS => RESULT IN THE SCALAR POINTS
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
! H    - 
! IMAX - 
! JMAX - 
!
! OUTPUT ARGUMENT LIST:
! NONE
!
! INPUT/OUTPUT ARGUMENT LIST:
! HXY  -
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
    INTEGER(KIND=I4)                                                      , INTENT(IN)          ::&
    & IMAX    , JMAX
!
    REAL   (KIND=R4)    , DIMENSION(0:IMAX+1, 0:JMAX+1)                   , INTENT(IN)          ::&
    & H
!
    REAL   (KIND=R4)    , DIMENSION(0:IMAX+1, 0:JMAX+1)                   , INTENT(OUT)         ::&
    & HXY
!
    REAL   (KIND=R4)    , DIMENSION(0:IMAX+1, 0:JMAX+1)                                         ::&
    & HY
!
    INTEGER(KIND=I4)                                                                            ::&
    & I       , J
!
    DO J=0,JMAX
        DO I=0,IMAX
            HY(I,J) = H(I,J) + H(I+1,J+1)
        END DO
    END DO
!
    DO J=1,JMAX
        DO I=1,IMAX
            HXY(I,J) = HY(I-1,J) + HY(I,J-1)
        END DO
    END DO
!
    RETURN
!
    END SUBROUTINE AVRHH   
!
!
!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Inserir Brief de INVERTV
!> @details Inserir Details de INVERTV
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
!> @param[in] IM1X2  -
!> @param[in] LMAX  -
!> @param[inout] ARR  -
!> @details <b>Use Module:</b>
!! @arg @c F77KINDS
!> @details <b>Driver:</b> 
!! @arg @c BOCOVMPI
!! @arg @c BOCOV_HMPI
!> @details <b>Calls:</b>
!-------------------------------------------------------------------------------------------------- 
    SUBROUTINE INVERTV(ARR, IM1X2, LMAX)
!--------------------------------------------------------------------------------------------------
! SUBROUTINE INVERTV
!
! SUBROUTINE: INVERTV - 
! PROGRAMMER: ?????
! ORG: ?????
! DATE: ??-??-??
!
! ABSTRACT:
! ?????
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
! LMAX  - 
! IM1X2 -
!
! OUTPUT ARGUMENT LIST:
! NONE
!
! INPUT/OUTPUT ARGUMENT LIST:
! ARR   -
!
! USE MODULES: F77KINDS   
!
! DRIVER     : BOCOVMPI
!              BOCOV_HMPI
!
! CALLS      : -----
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    INTEGER(KIND=I4)                                                      , INTENT(IN)          ::&
    & IM1X2   , LMAX
!
    REAL   (KIND=R4)    , DIMENSION(IM1X2, LMAX)                          , INTENT(INOUT)       ::&
    & ARR
!
    REAL   (KIND=R4)    , DIMENSION(IM1X2/2)                                                    ::&
    & TPARR
!
    INTEGER(KIND=I4)                                                                            ::&
    & I       , L       , IMAX
!
    IMAX = IM1X2 / 2 + 1
!
    DO L=1,LMAX
        TPARR = ARR(1:IMAX-1, L)
!
        DO I=1,IMAX-1
            ARR(I,L) = TPARR(IMAX-I)
        END DO
!
        TPARR = ARR(IMAX:IM1X2, L)
!
        DO I=1,IMAX-1
            ARR(I+IMAX-1,L) = TPARR(IMAX-I)
        END DO
!
    END DO
!
    END SUBROUTINE INVERTV
!
!
!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Inserir Brief de SWAPUV
!> @details Inserir Details de SWAPUV
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
!> @param[in] IM1X2  -
!> @param[in] LMAX  -
!> @param[inout] ARR  -
!> @details <b>Use Module:</b>
!! @arg @c F77KINDS
!> @details <b>Driver:</b> 
!! @arg @c BOCOV_HMPI
!> @details <b>Calls:</b>
!--------------------------------------------------------------------------------------------------
    SUBROUTINE SWAPUV(ARR, IM1X2, LMAX)
!--------------------------------------------------------------------------------------------------
! SUBROUTINE SWAPUV
!
! SUBROUTINE: SWAPUV - 
! PROGRAMMER: ?????
! ORG: ?????
! DATE: ??-??-??
!
! ABSTRACT:
! ?????
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
! IM1X2 -
! LMAX  -
!
! OUTPUT ARGUMENT LIST:
! NONE
!
! INPUT/OUTPUT ARGUMENT LIST:
! ARR   -
!
! USE MODULES: F77KINDS   
!
! DRIVER     : BOCOV_HMPI
!
! CALLS      : -----
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    INTEGER(KIND=I4)                                                      , INTENT(IN)          ::&
    & IM1X2, LMAX
!
    REAL   (KIND=R4)    , DIMENSION(IM1X2, LMAX)                          , INTENT(INOUT)       ::&
    & ARR
!
    REAL   (KIND=R4)    , DIMENSION(IM1X2/2)                                                    ::&
    & TPARR
!
    INTEGER(KIND=I4)                                                                            ::&
    & IM1     , L
!
    DO L=1,LMAX
        IM1                 = IM1X2 / 2
        TPARR               = ARR(1:IM1      , L)
        ARR(1:IM1      , L) = ARR(IM1+1:IM1X2, L)
        ARR(IM1+1:IM1X2, L) = TPARR
    END DO
!
    END SUBROUTINE SWAPUV
!
!
!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief LAGRANGIAN 2ND ORDER INTERPOLATION
!> @details LAGRANGIAN 2ND ORDER INTERPOLATION
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
!> @param[in] X1  -
!> @param[in] X2  - 
!> @param[in] X3  - 
!> @param[in] Y1  - 
!> @param[in] Y2  - 
!> @param[in] Y3  - 
!> @param[in] XST - 
!> @param[out] YST -
!> @details <b>Use Module:</b>
!! @arg @c F77KINDS
!> @details <b>Driver:</b> 
!<
!> @details <b>Calls:</b>
!--------------------------------------------------------------------------------------------------
    SUBROUTINE LAGRANG3(X1, X2, X3, XST,  Y1, Y2, Y3, YST) 
!--------------------------------------------------------------------------------------------------
! SUBROUTINE LAGRANG3
!
! SUBROUTINE: LAGRANG3 - LAGRANGIAN 2ND ORDER INTERPOLATION
! PROGRAMMER: ?????
! ORG: ?????
! DATE: ??-??-??
!
! ABSTRACT:
! LAGRANGIAN 2ND ORDER INTERPOLATION
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
! X1  -
! X2  - 
! X3  - 
! Y1  - 
! Y2  - 
! Y3  - 
! XST -
!
! OUTPUT ARGUMENT LIST:
! YST -
!
! INPUT/OUTPUT ARGUMENT LIST:
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
    REAL   (KIND=R4)                                                                            ::&
    & X1_X2   , X1_X3   , X2_X3   , XST_X1  , XST_X2  , XST_X3  , P1      , P2      , P3
!
    REAL   (KIND=R4)                                                      , INTENT(IN)          ::&
    & X1      , X2      , X3      , Y1      , Y2      , Y3      , XST
!
    REAL   (KIND=R4)                                                      , INTENT(OUT)          ::&
    & YST
!
    X1_X2  = X1 - X2
    X1_X3  = X1 - X3
    X2_X3  = X2 - X3
!
    XST_X1 = XST - X1
    XST_X2 = XST - X2
    XST_X3 = XST - X3
!
    P1     = XST_X1 / X2_X3
    P2     = XST_X2 / X1_X3
    P3     = XST_X3 / X1_X2
!
    YST = P2 * P3 * Y1  -  P1 * P3 * Y2   +  P1 * P2 * Y3
!
    END SUBROUTINE LAGRANG3
!
!
!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Inserir Brief de ISCORNER
!> @details Inserir Details de ISCORNER
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
!> @param[in] I  -
!> @param[in] J  -
!> @details <b>Use Module:</b>
!! @arg @c F77KINDS
!! @arg @c DOM
!! @arg @c PARMETA
!> @details <b>Driver:</b> 
!! @arg @c CORNERHM2
!! @arg @c CORNERHM4
!! @arg @c HDIFF
!! @arg @c HZADV
!! @arg @c ISCORNER2
!! @arg @c UTIL
!> @details <b>Calls:</b>
!--------------------------------------------------------------------------------------------------
    LOGICAL FUNCTION ISCORNER(I,J)
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
    USE DOM
    USE PARMETA
!
    IMPLICIT NONE
!
    INTEGER(KIND=I4)                                                      , INTENT(IN)          ::&
    & I       , J
!
    ISCORNER = .FALSE.
!
    IF      (I ==  1 .AND. J ==  1 .AND. LSWC) THEN
        ISCORNER = .TRUE.
!
    ELSE IF (I ==  1 .AND. J == JM .AND. LNWC) THEN
        ISCORNER = .TRUE.
!
    ELSE IF (I == IM .AND. J ==  1 .AND. LSEC) THEN
        ISCORNER = .TRUE.
!
    ELSE IF (I == IM .AND. J == JM .AND. LNEC) THEN
        ISCORNER = .TRUE.
!
    END IF
!
    END FUNCTION ISCORNER
!
!
!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief FALULATE SECOND DERIVATIVES OF TABULATED FUNCTION Y(X)
!> @details FALULATE SECOND DERIVATIVES OF TABULATED FUNCTION Y(X) DEFINED AT N POINTS, GIVEN FIRST 
!! DERIVATIVES AT THE BOUNDARIES (YP1 AND YPN).
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
!> @param[in] X -
!> @param[in] Y -
!> @param[in] N -
!> @param[in] YP1 -
!> @param[in] YPN -
!> @param[out] Y2 -
!> @details <b>Use Module:</b>
!! @arg @c F77KINDS
!> @details <b>Driver:</b> 
!! @arg @c OUTSD
!> @details <b>Calls:</b>
!--------------------------------------------------------------------------------------------------
    SUBROUTINE SPLINE(X, Y, N, YP1, YPN, Y2)
!--------------------------------------------------------------------------------------------------
! SUBROUTINE SPLINE
!
! SUBROUTINE: SPLINE - FALULATE SECOND DERIVATIVES OF TABULATED FUNCTION Y(X)
! PROGRAMMER: ?????
! ORG: ?????
! DATE: ??-??-??
!
! ABSTRACT:
! FALULATE SECOND DERIVATIVES OF TABULATED FUNCTION Y(X) DEFINED AT N POINTS, GIVEN FIRST 
! DERIVATIVES AT THE BOUNDARIES (YP1 AND YPN).
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
! X - 
! Y - 
! N - 
! YP1 -
! YPN -
!
! OUTPUT ARGUMENT LIST:
! Y2  -
!
! INPUT/OUTPUT ARGUMENT LIST:
! NONE
!
! USE MODULES: F77KINDS   
!
! DRIVER     : OUTSD
!
! CALLS      : -----
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    INTEGER(KIND=I4)    , PARAMETER :: NMAX = 100
    INTEGER(KIND=I4)                                                                            ::&
    & N       , I       , K       , YP1     , YPN     , QN      , UN      , SIG     , P
!
    REAL   (KIND=R4)    , DIMENSION(N)                                                          ::&
    & X       , Y       , Y2
!
    REAL   (KIND=R4)    , DIMENSION(NMAX)                                                       ::&
    & U
!
    IF (YP1 > .99E30) THEN
        Y2(1) = 0.
         U(1) = 0.
    ELSE
        Y2(1) = -0.5
         U(1) = (3. / (X(2) - X(1))) * ((Y(2) - Y(1)) / (X(2) - X(1)) - YP1)
    END IF
!
    DO I=2,N-1
        SIG   = (X(I) -  X(I-1)) / (X(I+1) - X(I))
        P     =  SIG  * Y2(I-1)  + 2.
        Y2(I) = (SIG  - 1.)      / P
         U(I) = (6. * ((Y(I+1) - Y(I))    / (X(I+1) - X(I))   - (Y(I) - Y(I-1))                   &
    &         /        (X(I)   - X(I-1))) / (X(I+1) - X(I-1)) - SIG * U(I-1)) / P
    END DO
!
    IF (YPN > .99E30) THEN
        QN = 0.
        UN = 0.
    ELSE
        QN =  0.5
        UN = (3. / (X(N) - X(N-1))) * (YPN - (Y(N) - Y(N-1)) / (X(N) - X(N-1)))
    END IF
!      
    Y2(N) = (UN - QN * U(N-1)) / (QN * Y2(N-1) + 1.)
!
    DO K=N-1,1,-1
        Y2(K) = Y2(K) * Y2(K+1) + U(K)
    END DO
!
    END SUBROUTINE SPLINE
!
!
!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief GIVEN TABULATED FUNCTION YA(XA) DEFINED AT N POINTS
!> @details GIVEN TABULATED FUNCTION YA(XA) DEFINED AT N POINTS, AND ITS SECOND ORDER DERIVATIVES Y2A AS 
!! DERIVED FROM SPLINE, AND GIVEN A VALUE OF X, THIS ROUTINE RETURNS A CUBIC-SPLINE INTERPOLATED 
!! VALUE Y.
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
!> @param[in] XA -
!> @param[in] YA -
!> @param[in] N -
!> @param[in] Y2A -
!> @param[in] X -
!> @param[out] Y -
!> @details <b>Use Module:</b>
!! @arg @c F77KINDS
!> @details <b>Driver:</b> 
!! @arg @c OUTSD
!> @details <b>Calls:</b>
!--------------------------------------------------------------------------------------------------
    SUBROUTINE SPLINT(XA, YA, Y2A, N, X, Y)
!--------------------------------------------------------------------------------------------------
! SUBROUTINE SPLINT
!
! SUBROUTINE: SPLINT - GIVEN TABULATED FUNCTION YA(XA) DEFINED AT N POINTS
! PROGRAMMER: ?????
! ORG: ?????
! DATE: ??-??-??
!
! ABSTRACT:
! GIVEN TABULATED FUNCTION YA(XA) DEFINED AT N POINTS, AND ITS SECOND ORDER DERIVATIVES Y2A AS 
! DERIVED FROM SPLINE, AND GIVEN A VALUE OF X, THIS ROUTINE RETURNS A CUBIC-SPLINE INTERPOLATED 
! VALUE Y.
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
! XA  - 
! YA  - 
! Y2A -
! N   -
! X   - 
!
! OUTPUT ARGUMENT LIST:
! Y   -
!
! INPUT/OUTPUT ARGUMENT LIST:
! NONE
!
! USE MODULES: F77KINDS   
!
! DRIVER     : GEF
!
! CALLS      : -----
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    INTEGER(KIND=I4)                                                                            ::&
    & N       , KL0     , KHI     , K
!
    REAL   (KIND=R4)                                                                            ::&
    & X       , Y       , H       , A       , B
!
    REAL   (KIND=R4)    , DIMENSION(N)                                                          ::&
    & XA      , YA      , Y2A
!
    KL0 = 1.
    KHI = N
!
 LOOP:  DO 
            IF(KHI-KL0 <= 1) EXIT LOOP
            K = (KHI + KL0) / 2
            IF (XA(K)> X) THEN
                KHI = K
            ELSE
                KL0 = K
            END IF
!
        END DO LOOP
!
    H = XA(KHI) - XA(KL0)
!
    A = (XA(KHI) - X) / H
    B = (X - XA(KL0)) / H
!
    Y = A * YA(KL0) + B * YA(KHI) + ((A ** 3 - A) * Y2A(KL0)                                      &
    & +                              (B ** 3 - B) * Y2A(KHI)) * (H ** 2) / 6
!
    END SUBROUTINE SPLINT
!
! 
!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief TRUE FOR SWAP LEFT AND RIGHT & FALSE FOR SWAP UP AND DOWN
!> @details TRUE FOR SWAP LEFT AND RIGHT & FALSE FOR SWAP UP AND DOWN.
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
!> @param[in] IMAX -
!> @param[in] JMAX -
!> @param[in] LMAX -
!> @param[in] CHOICE -
!> @param[inout] ARR -
!> @details <b>Use Module:</b>
!! @arg @c F77KINDS
!> @details <b>Driver:</b> 
!> @details <b>Calls:</b>
!-------------------------------------------------------------------------------------------------- 
    SUBROUTINE SWAPUV_LN(ARR, IMAX, JMAX, LMAX, CHOICE)
!--------------------------------------------------------------------------------------------------
! SUBROUTINE SWAPUV_LN
!
! SUBROUTINE: SWAPUV_LN - TRUE FOR SWAP LEFT AND RIGHT & FALSE FOR SWAP UP AND DOWN
! PROGRAMMER: ?????
! ORG: ?????
! DATE: ??-??-??
!
! ABSTRACT:
! TRUE FOR SWAP LEFT AND RIGHT & FALSE FOR SWAP UP AND DOWN
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
! IMAX - 
! JMAX - 
! LMAX -
! CHOICE - 
!
! OUTPUT ARGUMENT LIST:
!
! INPUT/OUTPUT ARGUMENT LIST:
! ARR -
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
    INTEGER(KIND=I4)                                                      , INTENT(IN)          ::&
    & IMAX    , JMAX    , LMAX
!
    LOGICAL(KIND=L4)                                                      , INTENT(IN)          ::&
    & CHOICE
!
    REAL   (KIND=R4)    , DIMENSION(IMAX, JMAX, LMAX)                     , INTENT(INOUT)       ::&
    & ARR
!
    REAL   (KIND=R4)    , DIMENSION(:,:,:)  , ALLOCATABLE                                       ::&
    & TMPARR
!
    IF (CHOICE) THEN
!
        ALLOCATE (TMPARR(IMAX/2, JMAX, LMAX))
!
        TMPARR = ARR(1:IMAX/2, :, :)
!
        ARR(1:IMAX/2     , :, :) = ARR(IMAX/2+1:IMAX, :, :)
!
        ARR(IMAX/2+1:IMAX, :, :) = TMPARR
!
    ELSE
!
        ALLOCATE (TMPARR(IMAX, JMAX/2, LMAX))
!
        TMPARR = ARR(:, 1:JMAX/2, :)
!
        ARR(:, 1:JMAX/2     , :) = ARR(:, JMAX/2+1:JMAX, :)
!
        ARR(:, JMAX/2+1:JMAX, :) = TMPARR
!
    END IF
!
    END SUBROUTINE SWAPUV_LN

