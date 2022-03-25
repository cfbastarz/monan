!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Inserir Brief de FOUR1
!> @details Inserir Details de FOUR1
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
!> @param[in] NN    - Significado de NN
!> @param[in] ISIGN  - Significado de ISIGN
!> @param[inout] DATA - Significado de DATA
!> @details <b>Use Module:</b>
!! @arg @c F77KINDS
!> @details <b>Driver:</b> 
!! @arg @c REALFT
!> @details <b>Calls:</b>
!--------------------------------------------------------------------------------------------------
    SUBROUTINE FOUR1(DATA, NN, ISIGN)
!--------------------------------------------------------------------------------------------------
! SUBROUTINE FOUR1
!
! SUBPROGRAM: FOUR1 - ?????
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
! NN    - 
! ISIG  -  
!
! OUTPUT ARGUMENT LIST:
! NONE
!
! INPUT/OUTPUT ARGUMENT LIST:
! DATA -   
!
! USE MODULES: F77KINDS
!  
! DRIVER     : REALFT
!
! CALLS      : -----
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    INTEGER(KIND=I4)                                                      , INTENT(IN)          ::&
    & ISIGN   , NN
!
    REAL   (KIND=R4)    , DIMENSION(2*NN)                                 , INTENT(INOUT)       ::&
    & DATA
!
    INTEGER(KIND=I4)                                                                            ::&
    & I       , ISTEP   , J       , M       , MMAX    , N
!
    REAL   (KIND=R4)                                                                            ::&
    & TEMPI   , TEMPR
!
    REAL   (KIND=R8)                                                                            ::&
    & THETA   , WI      , WPI     , WPR     , WR      , WTEMP
!
    N = 2 * NN
    J = 1
!
    DO I=1,N,2
        IF (J > I) THEN
!
            TEMPR = DATA(J)
            TEMPI = DATA(J+1)
!
            DATA(J)   = DATA(I)
            DATA(J+1) = DATA(I+1)
            DATA(I)   = TEMPR
            DATA(I+1) = TEMPI
!
        END IF
!
        M = NN
!
 1      IF ((M >= 2) .AND. (J > M)) THEN
	    J = J - M
	    M = M / 2
!
            GOTO 1
!
        END IF
!
        J = J + M
!
    END DO
!
    MMAX = 2
!
 2  IF (N > MMAX) THEN
        ISTEP = 2 * MMAX
        THETA = 6.28318530717959D0 / (ISIGN * MMAX)
        WPR   = -2.D0 * SIN(0.5D0 * THETA) ** 2
        WPI   = SIN(THETA)
        WR    = 1.D0
        WI    = 0.D0
!
        DO M=1,MMAX,2
            DO I=M,N,ISTEP
                J = I + MMAX
!
                TEMPR = SNGL(WR) * DATA(J  ) - SNGL(WI) * DATA(J+1)
                TEMPI = SNGL(WR) * DATA(J+1) + SNGL(WI) * DATA(J  ) 
!
                DATA(J  ) = DATA(I  ) - TEMPR
                DATA(J+1) = DATA(I+1) - TEMPI
                DATA(I  ) = DATA(I  ) + TEMPR
                DATA(I+1) = DATA(I+1) + TEMPI
!
            END DO
!
            WTEMP = WR
            WR    = WR * WPR - WI    * WPI + WR
            WI    = WI * WPR + WTEMP * WPI + WI
        END DO
!
	MMAX = ISTEP
!
        GOTO 2
!
    END IF
!	
    RETURN
!
    END SUBROUTINE FOUR1
