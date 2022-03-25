!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Inserir Brief de E3V88
!> @details Inserir Details de E3V88
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
!> @param[in] AV  - Significado de AV
!> @param[in] TV - Significado de TV
!> @param[out] EMV - Significado de EMV
!> @details <b>Use Module:</b>
!! @arg @c F77KINDS
!! @arg @c HCON
!! @arg @c MPPSTAFF
!! @arg @c PARMETA
!! @arg @c RDPARM
!! @arg @c TABCOM
!> @details <b>Driver:</b> 
!! @arg @c FST88
!> @details <b>Calls:</b>
!<
!--------------------------------------------------------------------------------------------------
    SUBROUTINE E3V88(EMV, TV, AV)
!--------------------------------------------------------------------------------------------------
! SUBROUTINE E3V88
!
! SUBPROGRAM: E3V88 - ?????
! PROGRAMMER: ?????
! ORG: ?????
! DATE: ??-??-??
!
! ABSTRACT:
! SUBROUTINE E3V88 COMPUTES NEARBY LAYER TRANSMISSIVITIES FOR H2O USING A TABLE LOOKUP OF THE PRE-
! COMPUTED E3 FUNCTION (DESCRIBED IN REF. (4)).
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
! TV  - 
! AV  -
!
! OUTPUT ARGUMENT LIST:
! EMV -
!
! INPUT/OUTPUT ARGUMENT LIST:
! NONE
!
! USE MODULES: F77KINDS
!              HCON
!              MPPSTAFF
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
    USE MPPSTAFF
    USE PARMETA
    USE RDPARM
    USE TABCOM
!
    IMPLICIT NONE
!
    INTEGER(KIND=I4)    , DIMENSION(IM, LLP1)                                                   ::&
    & IT
!
    REAL   (KIND=R4)    , DIMENSION(IM, LLP1)                                                   ::&  
    & WW1     , DT      ,                                                                         &
    & WW2     , DU
!------------------------------------------------------ 
! THE FOLLOWING ARRAYS ARE EQUIVALENCED TO VTEMP ARRAYS
!------------------------------------------------------ 
    REAL   (KIND=R4)    , DIMENSION(IM, LLP1)                                                   ::&
    & FXO     , FYO     , TMP3
!-------------------------------------- 
! DIMENSIONS OF ARRAYS IN ARGUMENT LIST
!--------------------------------------
    REAL   (KIND=R4)    , DIMENSION(IM, LLP1)                             , INTENT(OUT)         ::& 
    & EMV
!
    REAL   (KIND=R4)    , DIMENSION(IM, LLP1)                             , INTENT(IN)          ::& 
    & TV      , AV
!------------------------
! IMPLICIT NONE VARIABLES
!------------------------
    INTEGER(KIND=I4)                                                                            ::&
    & I       , K 
!
    EQUIVALENCE (FXO,WW1), (FYO,WW2), (IT,TMP3)
!-------------------------------------------------------------------------
! THE FOLLOWING LOOP REPLACES A DOUBLE LOOP OVER I (1-IMAX) AND K (1-LLP1)
!-------------------------------------------------------------------------
    DO K=1,LLP1
        DO I=1,IM
            FXO(I,K) = AINT(TV(I,K) * HP1)
!
            IF (AV(I,K) /= 0) THEN
                TMP3(I,K) = LOG10(AV(I,K)) + H16E1
            ELSE
                TMP3(I,K) = 0.
            END IF
!
             DT(I,K) = TV(I,K)   - TEN * FXO(I,K)
            FYO(I,K) = AINT(TMP3(I,K)  * TEN)
             DU(I,K) = TMP3(I,K) - HP1 * FYO(I,K)
!--------------------------------------------------------------------------------------------------
! OBTAIN INDEX FOR TABLE LOOKUP; THIS VALUE WILL HAVE TO BE DECREMENTED BY 9 TO ACCOUNT FOR TABLE 
! TEMPS STARTING AT 100K.
!--------------------------------------------------------------------------------------------------
             IT(I,K) = FXO(I,K) + FYO(I,K) * H28E1
            WW1(I,K) = TEN - DT(I,K)
            WW2(I,K) = HP1 - DU(I,K)
            EMV(I,K) = WW1(I,K) * WW2(I,K) * EM3V(IT(I,K) -  9)                                   &
            &        + WW2(I,K) *  DT(I,K) * EM3V(IT(I,K) -  8)                                   &
            &        + WW1(I,K) *  DU(I,K) * EM3V(IT(I,K) + 19)                                   &
            &        +  DT(I,K) *  DU(I,K) * EM3V(IT(I,K) + 20)
        END DO
    END DO
!
    RETURN
!
    END SUBROUTINE E3V88
