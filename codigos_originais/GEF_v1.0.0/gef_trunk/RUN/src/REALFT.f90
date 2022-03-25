!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief CALCULATE THE FOURIER TRANSFROM OF A SET OF N REAL-VALUED DATA POINTS.
!> @details CALCULATE THE FOURIER TRANSFROM OF A SET OF N REAL-VALUED DATA POINTS. REPLACES THIS DATA (WHICH
!! IS STROED IN ARRAY DATA (1:N) BY THE POSITIVE FREQUENCY HALF OF ITS COMPLEX FOURIER TRANSFORM. 
!! THE REAL-VALUED FIRST AND LAST COMPONENTS OF THE COMPLEX TRANSFORM ARE RETURNED AS ELEMENSTS 
!! DATA(1) AND DATA(2), RESPECTIVELY. N MUST BE A POWER OF 2. THIS ROUTINE ALSO CALCULATES THE 
!! INVERSE TRANSFORM OF A COMPLEX DATA ARRAY IF IT IS THE TRANSFORM OF REAL DATA. 
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
!> @param[in] ISIGN  - Significado de ISIGN
!> @param[in] N      - Significado de N
!> @param[inout] DATA  - Significado de DATA
!> @details <b>Use Module:</b>
!! @arg @c F77KINDS
!> @details <b>Driver:</b> 
!<
!> @details <b>Calls:</b>
!! @arg @c FOUR1
!--------------------------------------------------------------------------------------------------     
	SUBROUTINE REALFT(DATA, N, ISIGN)
!--------------------------------------------------------------------------------------------------
! SUBROUTINE REALFT
! 
! SUBPROGRAM: REALFT - CALCULATE THE FOURIER TRANSFROM OF A SET OF N REAL-VALUED DATA POINTS.
! PROGRAMMER: ????? 
! ORG: ?????
! DATE: ??-??-??
!
! ABSTRACT:  
! CALCULATE THE FOURIER TRANSFROM OF A SET OF N REAL-VALUED DATA POINTS. REPLACES THIS DATA (WHICH
! IS STROED IN ARRAY DATA (1:N) BY THE POSITIVE FREQUENCY HALF OF ITS COMPLEX FOURIER TRANSFORM. 
! THE REAL-VALUED FIRST AND LAST COMPONENTS OF THE COMPLEX TRANSFORM ARE RETURNED AS ELEMENSTS 
! DATA(1) AND DATA(2), RESPECTIVELY. N MUST BE A POWER OF 2. THIS ROUTINE ALSO CALCULATES THE 
! INVERSE TRANSFORM OF A COMPLEX DATA ARRAY IF IT IS THE TRANSFORM OF REAL DATA.  
!
! PROGRAM HISTORY LOG:
! ??-??-??  ?????    - ORIGINATOR
! 18-03-20  LUCCI    - MODERNIZATION OF THE CODE, INCLUDING:
!                      * F77 TO F90/F95
!                      * INDENTATION & UNIFORMIZATION CODE
!                      * REPLACEMENT OF COMMONS BLOCK FOR MODULES
!                      * DOCUMENTATION WITH DOXYGEN
!                      * OPENMP FUNCTIONALITY 
!
!
! INPUT ARGUMENT LIST:
! ISIGN -
! N     -
!
! OUTPUT ARGUMENT LIST:
! 
!
! INPUT/OUTPUT ARGUMENT LIST:
! DATA -
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
! CALLS      : FOUR1           
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    INTEGER(KIND=I4)                                                      , INTENT(IN)          ::&
    & ISIGN   , N
!
    REAL   (KIND=R4)    , DIMENSION(N)                                    , INTENT(INOUT)       ::&
    & DATA
!
    INTEGER(KIND=I4)                                                                            ::&
    & I       , II1      , II2      , I3      , I5      , N2P3
!
    REAL   (KIND=R4)                                                                            ::&
    & C1      , C2      , H1I     , H1R     , H2I     , H2R     , WIS     , WRS
!
    REAL   (KIND=R8)                                                                            ::&
    & THETA   , WI      , WPI     , WPR     , WR      , WTEMP
!      
    THETA = 3.141592653589793D0 / DBLE(N / 2)
!
    C1 = 0.5
!
    IF (ISIGN == 1) THEN
        C2    = -0.5
	CALL FOUR1(DATA, N/2, 1)
    ELSE
        C2    =  0.5
	THETA = -THETA
    END IF
!
    WPR  = -2.0D0 * SIN(0.5D0 * THETA) ** 2
    WPI  = SIN(THETA)
    WR   = 1.0D0 + WPR
    WI   = WPI
    N2P3 = N + 3
!
    DO I=2,N/4
        II1 = 2    *   I  - 1
        II2 = II1  +   1
        I3  = N2P3 - II2
        I5  = I3   +   1
!
        WRS = SNGL(WR)
        WIS = SNGL(WI)
!
        H1R =  C1 * (DATA(II1) + DATA(I3))
        H1I =  C1 * (DATA(II2) - DATA(I5))
        H2R = -C2 * (DATA(II2) + DATA(I5))
        H2I =  C2 * (DATA(II1) - DATA(I3))
!
        DATA(II1) =  H1R + WRS * H2R - WIS * H2I
        DATA(II2) =  H1I + WRS * H2I + WIS * H2R
        DATA(I3)  =  H1R - WRS * H2R + WIS * H2I
        DATA(I5)  = -H1I + WRS * H2I + WIS * H2R
!
	WTEMP = WR
	WR    = WR * WPR - WI    * WPI + WR
	WI    = WI * WPR + WTEMP * WPI + WI
    END DO
!
    IF (ISIGN == 1) THEN
        H1R = DATA(1)
        DATA(1) = H1R + DATA(2)
        DATA(2) = H1R - DATA(2)
    ELSE
        H1R = DATA(1)
        DATA(1) = C1 * (H1R + DATA(2))
        DATA(2) = C1 * (H1R - DATA(2))
!
        CALL FOUR1(DATA, N/2,-1)
    END IF
!
    RETURN
!
    END SUBROUTINE REALFT
