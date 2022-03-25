!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief UPDATE MOMENTUM DUE TO KINETIC ENERGY TERMS
!> @details UPDATE MOMENTUM DUE TO KINETIC ENERGY TERMS.
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
!! @arg @c CONTIN
!! @arg @c CTLBLK
!! @arg @c DYNAM
!! @arg @c F77KINDS
!! @arg @c MASKS
!! @arg @c METRCS
!! @arg @c MPPSTAFF
!! @arg @c PARMETA
!! @arg @c REALPAR
!! @arg @c SET_ZERO
!! @arg @c VRBLS
!> @details <b>Driver:</b> 
!<
!> @details <b>Calls:</b>
!! @arg @c AVRV
!! @arg @c BOCOVMPI
!! @arg @c ZERO
!--------------------------------------------------------------------------------------------------
    SUBROUTINE KINEN
!--------------------------------------------------------------------------------------------------
! SUBROUTINE KINEN
!
! SUBPROGRAM: KINEN - UPDATE MOMENTUM DUE TO KINETIC ENERGY TERMS
! PROGRAMMER: ?????
! ORG: ?????
! DATE: ??-??-??
!
! ABSTRACT:
! UPDATE MOMENTUM DUE TO KINETIC ENERGY TERMS
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
! USE MODULES: CONTIN
!              CTLBLK
!              DYNAM
!              F77KINDS
!              MASKS
!              METRCS
!              MPPSTAFF
!              PARMETA
!              REALPAR
!              SET_ZERO
!              VRBLS
!  
! DRIVER     : -----
!
! CALLS      : AVRV
!              BOCOVMPI
!              ZERO
!--------------------------------------------------------------------------------------------------
    USE CONTIN
    USE CTLBLK
    USE DYNAM
    USE F77KINDS
    USE MASKS
    USE METRCS
    USE MPPSTAFF
    USE PARMETA
    USE REALPAR
    USE SET_ZERO
    USE VRBLS
!
    IMPLICIT NONE
! 
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1)                                             ::&
    & EKN     , AKN
!
    INTEGER(KIND=I4)                                                                            ::&
    & I       , J       , L
!
    REAL   (KIND=R4)                                                                            ::&
    & UST     , VST     , HXYGV   , HXYGH   , UTLD    , VTLD    , VM
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1)                                             ::&
    & FILO    , DPDE
!
    CALL ZERO(EKN, AKN)
!
    IF (SIGMA) THEN
        DO J=0,JM+1
            DO I=0,IM+1
                FILO(I,J) = FIS(I,J)
                PDSL(I,J) =  PD(I,J)
            END DO
        END DO
    ELSE
        DO J=0,JM+1
            DO I=0,IM+1
                FILO(I,J) = D00
                PDSL(I,J) = RES(I,J) * PD(I,J)
            END DO
        END DO
    END IF
!
    DO L=LM,1,-1
!
        DO J=0,JM+1
            DO I=0,IM+1
                DPDE(I,J) = DETA(L) * PDSL(I,J)
            END DO
        END DO
!
        DO I=0,IM
            DO J=0,JM
                UST = U(I,J,L)
                VST = V(I,J,L)
!
                UTLD = Q11(I,J) * UST + Q12(I,J) * VST
                VTLD = Q12(I,J) * UST + Q22(I,J) * VST
!
                EKN(I,J) = (UTLD * UST + VTLD * VST) * SQV(I,J)
            END DO
        END DO
!
        CALL AVRV(EKN, AKN, IM, JM)      
!
        DO J=1,JM1
            DO I=1,IM1
                VM = VTM(I,J,L)
!
                U(I,J,L) = U(I,J,L) + FKIN * (AKN(I+1,J  ) / SQH(I+1,J  )                         &
    &                    -                    AKN(I,J+1)   / SQH(I  ,J+1)) * VM
!
                V(I,J,L) = V(I,J,L) + FKIN * (AKN(I+1,J+1) / SQH(I+1,J+1)                         &
    &                    -                    AKN(I  ,J  ) / SQH(I  ,J  )) * VM
!
	        HXYGV = (DPDE(I,J) + DPDE(I+1,J) + DPDE(I,J+1) + DPDE(I+1,J+1)) * SQV(I,J)
!
	        HXYGH = DPDE(I,J  ) * SQH(I,J  ) + DPDE(I+1,J  ) * SQH(I+1,J  )                   &
    &                 + DPDE(I,J+1) * SQH(I,J+1) + DPDE(I+1,J+1) * SQH(I+1,J+1)
!
            END DO
        END DO
!
    END DO   
!
    CALL BOCOVMPI(U,V,LM)
!
    END SUBROUTINE KINEN  

