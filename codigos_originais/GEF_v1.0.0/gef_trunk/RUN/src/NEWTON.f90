!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief HELD SUAREZ FORCE
!> @details HELD SUAREZ FORCE.
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
!! @arg @c CTLBLK
!! @arg @c DYNAM
!! @arg @c F77KINDS
!! @arg @c HS
!! @arg @c MASKS
!! @arg @c MPPSTAFF
!! @arg @c PARMETA
!! @arg @c VRBLS
!> @details <b>Driver:</b> 
!<
!> @details <b>Calls:</b>
!--------------------------------------------------------------------------------------------------    
    SUBROUTINE NEWTON
!--------------------------------------------------------------------------------------------------
! SUBROUTINE NEWTON
!
! SUBPROGRAM: NEWTON - SUBROUTINE FOR HELD SUAREZ FORCE
! PROGRAMMER: ?????
! ORG: ?????
! DATE: ??-??-??
!
! ABSTRACT:
! SUBROUTINE FOR HELD SUAREZ FORCE 
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
! USE MODULES: CTLBLK
!              DYNAM
!              F77KINDS
!              HS
!              MASKS
!              MPPSTAFF
!              PARMETA
!              VRBLS
!
! DRIVER     : -----
!
! CALLS      : -----
!--------------------------------------------------------------------------------------------------
    USE CTLBLK
    USE DYNAM
    USE F77KINDS
    USE HS
    USE MASKS
    USE MPPSTAFF
    USE PARMETA
    USE VRBLS
!
    IMPLICIT NONE
!
    REAL   (KIND=R4)                                                                            ::&
    & PRES    , DMP
!
    INTEGER(KIND=I4)                                                                            ::&
    & I       , J       , L
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1, LM)                                         ::&
    & TEQ
!
    DO L=1,LM
        DO I=0,IM+1
            DO J=0,JM+1
                U(I,J,L) = U(I,J,L) - KVDT(L) * U(I,J,L) * VTM(I,J,L)
                V(I,J,L) = V(I,J,L) - KVDT(L) * V(I,J,L) * VTM(I,J,L)
            END DO
        END DO
    END DO
!
    DO L=1,LM
        DO I=0,IM+1
            DO J=0,JM+1
                PRES       = PT + AETA(L) *    PD(I,J)           !ASSUME ETAS=1.0
                TEQ(I,J,L) = (315 - DELTY * HSINP(I,J) * HSINP(I,J)  - DELTHZ * ALOG(PRES/P0)     &
    &                      *                HCOSP(I,J) * HCOSP(I,J)) * (PRES/P0) ** KAPPA 
          
                IF (TEQ(I,J,L) < 200) THEN
                    TEQ(I,J,L) = 200.
                END IF
!
               T(I,J,L) = T(I,J,L) + (T(I,J,L) - TEQ(I,J,L)) * (-KTDT(I,J,L)) * HTM(I,J,L)
!
            END DO
        END DO
    END DO
!
    END SUBROUTINE NEWTON
