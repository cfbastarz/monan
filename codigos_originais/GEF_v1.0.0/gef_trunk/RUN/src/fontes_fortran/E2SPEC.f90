!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief COMPUTES THE EXCHANGE TERMS IN THE FLUX EQUATION FOR LONGWAVE RADIATION
!> @details COMPUTES THE EXCHANGE TERMS IN THE FLUX EQUATION FOR LONGWAVE RADIATION FOR 2 
!! TERMS USED FOR NEARBY LAYER COMPUTATIONS. THE METHOD IS A TABLE LOOKUP ON A PRE-COMPUTED E2 
!! FUNCTION (DEFINED IN REF. (4)).
!! CALCULATIONS ARE DONE IN THE FREQUENCY RANGE:
!! 0-560, 1200-2200 CM-1
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
!> @param[in] AVEPHI  - Significado de AVEPHI
!> @param[in] DTSP - Significado de DTSP
!> @param[in] FXOSP - Significado de FXOSP
!> @param[inout] EMISS - Significado de EMISS
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
    SUBROUTINE E2SPEC(EMISS, AVEPHI, FXOSP, DTSP)
!--------------------------------------------------------------------------------------------------
! SUBROUTINE E2SPEC
!
! SUBPROGRAM: E2SPEC - ?????
! PROGRAMMER: ?????
! ORG: ?????
! DATE: ??-??-??
!
! ABSTRACT:
! SUBROUTINE E2SPEC COMPUTES THE EXCHANGE TERMS IN THE FLUX EQUATION FOR LONGWAVE RADIATION FOR 2 
! TERMS USED FOR NEARBY LAYER COMPUTATIONS. THE METHOD IS A TABLE LOOKUP ON A PRE-COMPUTED E2 
! FUNCTION (DEFINED IN REF. (4)).
! CALCULATIONS ARE DONE IN THE FREQUENCY RANGE:
! 0-560, 1200-2200 CM-1
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
! AVEPHI -
! FXOSP  - 
! DTSP   - 
!
! OUTPUT ARGUMENT LIST:
! EMISS  - 
!
! INPUT/OUTPUT ARGUMENT LIST:
! NONE
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
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
    USE HCON
    USE PARMETA
    USE RDPARM
    USE TABCOM
!
    IMPLICIT NONE
    REAL   (KIND=R4)    , DIMENSION(IM, LP1)                              , INTENT(OUT)         ::&
    & EMISS
!
    REAL   (KIND=R4)    , DIMENSION(IM, LP1)                              , INTENT(IN)          ::&
    & AVEPHI 
!
    INTEGER(KIND=I4)    , DIMENSION(IM, LP1)                                                    ::&
    & IVAL 
!
    REAL   (KIND=R4)    , DIMENSION(IM, LP1)                                                    ::&
    & FYO     , DU      , TMP3 
!------------------------------- 
! VARIABLES IN THE ARGUMENT LIST
!------------------------------- 
    REAL   (KIND=R4)    , DIMENSION(IM, 2)                                , INTENT(IN)          ::&
    & FXOSP   , DTSP
!------------------------
! IMPLICIT NONE VARIABLES
!------------------------
    INTEGER(KIND=I4)                                                                            ::&
    & I       , K 
!--------------------------------------------------------------------------------------------------
! FIRST WE OBTAIN THE EMISSIVITIES AS A FUNCTION OF TEMPERATURE (INDEX FXO) AND WATER AMOUNT 
! (INDEX FYO). THIS PART OF THE CODE THUS GENERATES THE E2 FUNCTION.
!--------------------------------------------------------------------------------------------------
    DO K=1,2
        DO I=1,IM
!
            IF (AVEPHI(I,K) > 0) THEN
                TMP3(I,K) = LOG10(AVEPHI(I,K)) + H16E1
            END IF
!
            FYO  (I,K) = AINT(TMP3(I,K)  * TEN)
            DU   (I,K) = TMP3(I,K) - HP1 * FYO(I,K)
            IVAL (I,K) = H28E1 * FYO(I,K) + FXOSP(I,K)
            EMISS(I,K) = T1(IVAL(I,K)) + DU(I,K) * T2(IVAL(I,K)) + DTSP(I,K) * T4(IVAL(I,K))
!
        END DO
    END DO
!
    RETURN
!
    END SUBROUTINE E2SPEC
