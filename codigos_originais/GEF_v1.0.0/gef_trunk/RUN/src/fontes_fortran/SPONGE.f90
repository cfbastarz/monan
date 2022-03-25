!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Inserir Brief de SPONGE
!> @details Inserir Details de SPONGE
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
!> @details <b>Use Module:</b>
!! @arg @c F77KINDS
!! @arg @c HS
!! @arg @c MASKS
!! @arg @c METRCS
!! @arg @c PARMETA
!! @arg @c VRBLS
!> @details <b>Driver:</b> 
!<
!> @details <b>Calls:</b>
!! @arg @c BOCOVMPI
!--------------------------------------------------------------------------------------------------      
	SUBROUTINE SPONGE
!--------------------------------------------------------------------------------------------------
! SUBROUTINE SPONGE
!
! SUBPROGRAM: SPONGE - ?????
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
! NONE                                       
!  
! OUTPUT ARGUMENT LIST:
! NONE 
!
! INPUT/OUTPUT ARGUMENT LIST:
! NONE                                                 
!        
! USE MODULES: F77KINDS
!              HS
!              MASKS
!              METRCS
!              PARMETA
!              VRBLS
!
! DRIVER     : ------
!
! CALLS      : BOCOVMPI
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
    USE HS
    USE MASKS
    USE METRCS
    USE PARMETA
    USE VRBLS
!
    IMPLICIT NONE
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1)                                             ::&
    & UT      , VT      , UT1     , VT1
!
    INTEGER(KIND=I4)                                                                            ::&
    & I       , J       , K
!
    INTEGER(KIND=I4)    , DIMENSION(4)                                                          ::&
    & IT      , JT
!
    REAL   (KIND=R4)                                                                            ::&
    & TMPU    ,  TMPV   , UNEB    , VNEB    , DIFN
!
    REAL   (KIND=R4)    , DIMENSION(4)                                                          ::&
    & UNEBT   , VNEBT
!
    REAL   (KIND=R4)    , PARAMETER :: SPC = 0.5
!
    U(:, :, 1  ) = UTOP0
    V(:, :, 1  ) = VTOP0
    T(:, :, 1:2) = TTOP0
!
    UT = U(:, :, 1) - UTOP0
    VT = V(:, :, 1) - VTOP0
!
    UT1 = UT
    VT1 = VT
!
    DO J=1,JM1
        DO I=1,IM1
!
            IT = (/I-1, I+1, I  , I  /)
            JT = (/J  , J  , J-1, J+1/)
!
            TMPU = 0.
            TMPV = 0.
            DIFN = 0.
!
            DO K=1,4
                UNEB = UT1(IT(K), JT(K))
                VNEB = VT1(IT(K), JT(K))
!
                UNEBT(K) = UNEB * QD11(I,J,K) + VNEB * QD12(I,J,K)
                VNEBT(K) = UNEB * QD21(I,J,K) + VNEB * QD22(I,J,K)
!
                TMPU = TMPU + UNEBT(K) * VTM(IT(K), JT(K), 1)
                TMPV = TMPV + VNEBT(K) * VTM(IT(K), JT(K), 1)
                DIFN = DIFN +            VTM(IT(K), JT(K), 1)
            END DO
!
            IF (DIFN /= 0) THEN 
                UT(I,J) = -(TMPU - DIFN * UT1(I,J)) * 0.25
                VT(I,J) = -(TMPV - DIFN * VT1(I,J)) * 0.25
            END IF
!
        END DO
    END DO
!
    CALL BOCOVMPI(UT,VT,1)  
!
    END SUBROUTINE SPONGE
