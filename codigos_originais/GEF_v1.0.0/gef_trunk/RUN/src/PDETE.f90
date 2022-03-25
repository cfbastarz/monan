!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief VERTICALLY INTEGRATES THE MASS FLUX DIVERGENCE
!> @details VERTICALLY INTEGRATES THE MASS FLUX DIVERGENCE TO OBTAIN THE SURFACE PRESSURE TENDENCY AND 
!! ETADOT ON THE LAYER INTERFACES.
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
!> @param[in] IM   - Significado de IM
!> @param[in] JM   - Significado de JM
!> @param[in] LM   - Significado de LM
!> @details <b>Use Module:</b>
!! @arg @c CONTIN
!! @arg @c CTLBLK
!! @arg @c DYNAM
!! @arg @c F77KINDS
!! @arg @c MASKS
!! @arg @c MPPSTAFF
!! @arg @c REALPAR
!! @arg @c VRBLS
!> @details <b>Driver:</b> 
!! @arg @c DIGFLT
!> @details <b>Calls:</b>
!! @arg @c BOCOHMPI
!--------------------------------------------------------------------------------------------------     
    SUBROUTINE PDETE
!--------------------------------------------------------------------------------------------------
! SUBROUTINE PDETE
!
! SUBROUTINE: PDETE - VERTICALLY INTEGRATES THE MASS FLUX DIVERGENCE
! PROGRAMMER: ?????
! ORG: ?????
! DATE: ??-??-??
!
! ABSTRACT:
! VERTICALLY INTEGRATES THE MASS FLUX DIVERGENCE TO OBTAIN THE SURFACE PRESSURE TENDENCY AND 
! ETADOT ON THE LAYER INTERFACES
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
! IM -
! JM -
! LM - 
!
! OUTPUT ARGUMENT LIST:
! NONE
!
! INPUT/OUTPUT ARGUMENT LIST:
! NONE
!
! USE MODULES: CONTIN
!              CTLBLK
!              DYNAM
!              F77KINDS
!              MASKS
!              MPPSTAFF
!              REALPAR
!              VRBLS
!
! DRIVER     : DIGFLT
!
! CALLS      : BOCOHMPI
!--------------------------------------------------------------------------------------------------
    USE CONTIN
    USE CTLBLK
    USE DYNAM
    USE F77KINDS
    USE MASKS
    USE MPPSTAFF
    USE PARMETA , ONLY: IM, JM, LM
    USE REALPAR
    USE VRBLS
!
    IMPLICIT NONE
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1)                                             ::&
    & PRET
!
    REAL   (KIND=R4)    , DIMENSION(0:LM)                                                       ::&
    & DIVLOC
!
    INTEGER(KIND=I4)                                                                            ::&
    & I       , J       , L
!
    REAL   (KIND=R4)                                                                            ::&
    & RPSL    , PRAX
!
    REAL   (KIND=R4)    , DIMENSION(LM)                                                         ::&
    & EFAX
!
    DO J=1,JM
        DO I=1,IM
!------------------------------------------------
! COMPUTATION OF PRESSURE TENDENCY & PREPARATIONS
!------------------------------------------------
            DIVLOC(0) = 0.
!
            DO L=1,LM
                DIVLOC(L) = DIVLOC(L-1) + DIV(I,J,L)
            END DO
!
            PSDT(I,J) = -DIVLOC(LM)
            PRET(I,J) =  PSDT(I,J) * RES(I,J)
!
            RPSL      = H1 / PDSL(I,J)
!---------------------
! COMPUTATION OF ETADT
!---------------------
            DO L=1,LM-1
                ETADT(I,J,L) = -(PRET(I,J) * ETA(L+1) + DIVLOC(L)) * HTM(I,J,L+1) * RPSL
            END DO
!
            DO L=1,LM
                EFAX(L) = RTOP(I,J,L) * EF4T * HTM(I,J,L)
            END DO
!----------------------------------------------
! KINETIC ENERGY GENERATION TERMS IN T EQUATION
!----------------------------------------------
            PRAX = DIVLOC(1) * EFAX(1)
            OMGALF(I,J,1) = OMGALF(I,J,1) - PRAX
                 T(I,J,1) =      T(I,J,1) - PRAX
!
            DO L=2,LM-1
                PRAX          = (DIVLOC(L-1) + DIVLOC(L)) * EFAX(L)
                OMGALF(I,J,L) = OMGALF(I,J,L) - PRAX
                     T(I,J,L) =      T(I,J,L) - PRAX
            END DO
!
            PRAX           = (PRET(I,J) - DIVLOC(LM-1)) * EFAX(LM)
            OMGALF(I,J,LM) = OMGALF(I,J,LM) + PRAX
                 T(I,J,LM) =      T(I,J,LM) + PRAX
!
        END DO
    END DO
!
    CALL BOCOHMPI(T,LM)
    CALL BOCOHMPI(ETADT(:,:,1:LM-1),LM-1)
!
    END SUBROUTINE PDETE
