!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief UPDATES WINDS
!> @details UPDATES WINDS DUE TO THE PRESSURE GRADIENT AND CORIOIS FORCE AND CALCULATE DIVERGENCE 
!! INCLUDING MODIFICATION AND UPDATE TEMPERATURE BY CONTRIBUTION OF HORIZONTAL OMEGA-ALFA TERM.
!> @author ORIGINATOR - ????? 
!> @date 87-06-?? \n
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
!> @param[in] IM - Significado de IM
!> @param[in] JM - Significado de JM
!> @param[in] LM - Significado de LM
!> @details <b>Use Module:</b>   
!! @arg @c CONTIN
!! @arg @c CTLBLK
!! @arg @c DYNAM
!! @arg @c F77KINDS
!! @arg @c MASKS
!! @arg @c METRCS
!! @arg @c MPPSTAFF
!! @arg @c REALPAR
!! @arg @c SET_ZERO
!! @arg @c VRBLS 
!> @details <b>Driver:</b> 
!! @arg @c DIGFLT
!! @arg @c GEF
!> @details <b>Calls:</b>
!! @arg @c AVRH
!! @arg @c BOCOVMPI
!! @arg @c ZERO 
!<
!--------------------------------------------------------------------------------------------------
    SUBROUTINE ADJUST
!--------------------------------------------------------------------------------------------------
!  SUBROUTINE ADJUST
!  
!  SUBPROGRAM: ADJUST - UPDATES WINDS
!  PROGRAMMER: ????? 
!  ORG: W/NP22
!  DATE: ??-??-??
! 
!  ABSTRACT:  
!  UPDATES WINDS DUE TO THE PRESSURE GRADIENT AND CORIOIS FORCE AND CALCULATE DIVERGENCE INCLUDING 
!  MODIFICATION AND UPDATE TEMPERATURE BY CONTRIBUTION OF HORIZONTAL OMEGA-ALFA TERM.
! 
!  PROGRAM HISTORY LOG:
!  87-06-??  ?????   - ORIGINATOR
!  18-03-20  LUCCI   - MODERNIZATION OF THE CODE, INCLUDING:
!                      * F77 TO F90/F95
!                      * INDENTATION & UNIFORMIZATION CODE
!                      * REPLACEMENT OF COMMONS BLOCK FOR MODULES
!                      * DOCUMENTATION WITH DOXYGEN
!                      * OPENMP FUNCTIONALITY 
! 
! 
!  INPUT ARGUMENT LIST:
!  IM - 
!  JM -
!  LM - 
! 
!  OUTPUT ARGUMENT LIST:
!  NONE
! 
!  INPUT/OUTPUT ARGUMENT LIST:
!  NONE
! 
!  INPUT FILES:
!  NONE
! 
!  OUTPUT FILES:
!  NONE
! 
!  USE MODULES: CONTIN
!               CTLBLK
!               DYNAM
!               F77KINDS
!               MASKS
!               METRCS
!               MPPSTAFF
!               REALPAR
!               SET_ZERO
!               VRBLS
! 
!  DRIVER     : DIGFLT
!               GEF
! 
!  CALLS      : AVRH 
!               BOCOVMPI
!               ZERO
!--------------------------------------------------------------------------------------------------
    USE CONTIN
    USE CTLBLK
    USE DYNAM
    USE F77KINDS
    USE MASKS
    USE METRCS
    USE MPPSTAFF
    USE PARMETA , ONLY: IM, JM, LM
    USE REALPAR , ONLY: H1, D608, D00, D25, D50
    USE SET_ZERO
    USE VRBLS
!
    IMPLICIT NONE
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1, LM+1)                                       ::&
    & PINTLG
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1)                                             ::&
    & FILO    , GDPD    , AVGD    , FIM     , APEL    , XNPGF   , YNPGF   , XDPGF   , YDPGF   ,   &
    & FXE     , FYE     , FXB     , FYB     , TXE     , TYE     , TXB     , TYB     , ALP1    ,   &
    & DFDZ    , SQBV11  , SQBV12  , SQBV22
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1)                                             ::&
    & C1N     , C1U     , C1D     , C2N     , C2R     , C2L     , C3      , C4
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1)                                             ::&
    & UT      , VT
!
    REAL   (KIND=R4)                                                                            ::&
    & PRES    , ALP1P   , DFI     , ALP1PL  , ALP2P   , ALP2PL  , VST     , UST     , PGX     ,   &
    & PGY     , AVX     , AVY     , VM      , HM      , FIUPK   , DPDE    , RTVIJ   , DIVM    ,   &
    & CLAP    , PLAP    , FOM     , FOMADD  , OMALFXE , OMALFYE , OMALFXB , OMALFYB
!
    INTEGER(KIND=I4)                                                                            ::&
    & I       , J       , L
!
    DIVM = D00
!
    CALL ZERO(PDSL, GDPD, FIM, APEL, XNPGF, YNPGF, XDPGF, YDPGF)
    CALL ZERO(FXE , FYE , FXB, FYB , TXE  , TYE  , TXB  , TYB  )
!------------------------- 
! PREPARATORY CALCULATIONS
!------------------------- 
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
    DO L=1,LM+1
        DO I=0,IM+1
            DO J=0,JM+1
                PRES = ETA(L) * PDSL(I,J) + PT
!
                IF (PRES /= D00) THEN
                    PINTLG(I,J,L) = ALOG(PRES)
                ELSE
                    PINTLG(I,J,L) = D00
                END IF
!
            END DO
        END DO
    END DO
!
    DO I=0,IM+1
        DO J=0,JM+1
            ALP1(I,J) = PINTLG(I,J,LM+1)
        END DO
    END DO
!
    DIV(:,:,:) = D00 
!
!----------------
! PARALLELIZATION
!----------------
!
    DO L=LM,1,-1
!
    DO J=0,JM+1
        DO I=0,IM+1
            RTVIJ     = R * T(I,J,L) * (1. + Q(I,J,L) * D608)
            DPDE      = DETA(L) * PDSL(I,J)
            GDPD(I,J) = DPDE 
            ALP1P     = PINTLG(I,J,L)
            ALP1PL    = ALP1(I,J)
            DFI       = RTVIJ * (ALP1PL - ALP1P)
!
            IF (DPDE /= D00) THEN
                RTOP(I,J,L) = DFI / DPDE
            ELSE
                RTOP(I,J,L) = D00
            END IF
!
            FIUPK     = FILO(I,J) + DFI
             FIM(I,J) = FILO(I,J) + FIUPK
            FILO(I,J) = DFL(L) + HTM(I,J,L) * (FIUPK - DFL(L))
!
            ALP2P  = ALP1P  * ALP1P
            ALP2PL = ALP1PL * ALP1PL
!
            IF (ALP1PL + ALP1P /= D00) THEN
                DFDZ(I,J) = RTVIJ / (ALP1PL + ALP1P)
            ELSE
                DFDZ(I,J) = D00
            END IF
!
            APEL(I,J) = (ALP2PL + ALP2P) * D50
            ALP1(I,J) = ALP1P
        END DO
    END DO
!--------------------------------------
! COMPONENTS OF PRESSURE GRADIENT FORCE
!--------------------------------------
    DO J=0,JM
        DO I=0,IM
            XNPGF(I,J) =   FIM(I+1,J) -  FIM(I,J+1) + (DFDZ(I+1,J) + DFDZ(I,J+1))                 &
    &                  * (APEL(I+1,J) - APEL(I,J+1))
!
            YNPGF(I,J) =   FIM(I+1,J+1) -  FIM(I,J) + (DFDZ(I+1,J+1)+DFDZ(I,J))                   &
    &                  * (APEL(I+1,J+1) - APEL(I,J))
        END DO
    END DO
!
    DO J=0,JM+1
        DO I=0,IM
            XDPGF(I,J) = FIM(I+1,J) - FIM(I,J) + (DFDZ(I+1,J)+DFDZ(I,J)) * (APEL(I+1,J)-APEL(I,J))
        END DO
    END DO
!
    DO J=0,JM
        DO I=0,IM+1
            YDPGF(I,J) = FIM(I,J+1) - FIM(I,J) + (DFDZ(I,J+1)+DFDZ(I,J)) * (APEL(I,J+1)-APEL(I,J))
        END DO
    END DO
!-----------------------------------
! PRESSURE GRADIENT & CORIOLIS FORCE
!-----------------------------------
    DO J=1,JM-1
        DO I=1,IM-1
            VM = VTM(I,J,L)
            AVX = XDPGF(I,J) + XDPGF(I  ,J+1)
            AVY = YDPGF(I,J) + YDPGF(I+1,J  )
            PGX = XNPGF(I,J) + AVX - AVY
            PGY = YNPGF(I,J) + AVX + AVY
            UST = U(I,J,L)
            VST = V(I,J,L)
            UT(I,J) = (F11(I,J) * UST + F12(I,J) * VST + P11(I,J) * PGX + P12(I,J) * PGY) * VM
            VT(I,J) = (F21(I,J) * UST + F22(I,J) * VST + P21(I,J) * PGX + P22(I,J) * PGY) * VM
        END DO
    END DO
!
    CALL BOCOVMPI(UT,VT,1)
!------------------------------------------
! MODIFICATION'S CONTRIBUTION TO DIVERGENCE
!------------------------------------------
    DO J=0,JM
        DO I=0,IM
            XNPGF(I,J) = XNPGF(I,J) * HTM(I+1,J  ,L) * HTM(I,J+1,L)
            YNPGF(I,J) = YNPGF(I,J) * HTM(I+1,J+1,L) * HTM(I,J  ,L)
        END DO
    END DO
!
    DO J=0,JM+1
        DO I=0,IM
            XDPGF(I,J) = XDPGF(I,J) * HTM(I+1,J,L) * HTM(I,J,L)
        END DO
    END DO
!
    DO J=0,JM
        DO I=0,IM+1
            YDPGF(I,J) = YDPGF(I,J) * HTM(I,J+1,L) * HTM(I,J,L)
        END DO
    END DO
!-----------------------------------------------
! M: THIS HAS TO BE DIFINED IN PRP AND JUST READ
!-----------------------------------------------
    DO J=0,JM+1
        DO I=0,IM+1
            SQBV11(I,J) = SQV(I,J) * QBV11(I,J)
            SQBV12(I,J) = SQV(I,J) * QBV12(I,J)
            SQBV22(I,J) = SQV(I,J) * QBV22(I,J)
        END DO
    END DO
!
    DO J=1,JM
        DO I=0,IM
            C1N(I,J) = (SQBV11(I,J) + SQBV11(I,J-1)) * XDPGF(I,J)
        END DO
    END DO
!
    DO J=1,JM
        DO I=0,IM
            C1U(I,J) = (SQBV12(I,J) + SQBV12(I,J-1)) * (YDPGF(I+1,J) + YDPGF(I,J)) * VTM(I,J,L)
        END DO
    END DO
!
    DO J=0,JM-1
        DO I=0,IM
            C1D(I,J) = (SQBV12(I,J+1) + SQBV12(I,J)) * (YDPGF(I+1,J) + YDPGF(I,J)) * VTM(I,J,L)
        END DO
    END DO
!
    DO J=0,JM
        DO I=1,IM
            C2N(I,J) = (SQBV22(I,J) + SQBV22(I-1,J)) * YDPGF(I,J)  
        END DO
    END DO
!
    DO J=0,JM
        DO I=1,IM
            C2R(I,J) = (SQBV12(I,J) + SQBV12(I-1,J)) * (XDPGF(I,J+1) + XDPGF(I,J)) * VTM(I,J,L)    
        END DO
    END DO
!
    DO J=0,JM
        DO I=0,IM-1
            C2L(I,J) = (SQBV12(I+1,J) + SQBV12(I,J)) * (XDPGF(I,J+1) + XDPGF(I,J)) * VTM(I,J,L)
        END DO
    END DO
!
    DO J=0,JM
        DO I=0,IM
            VM      = SQV(I,J) * VTM(I,J,L)
!
            C3(I,J) = (Q11(I,J) * XNPGF(I,J) + Q12(I,J) * YNPGF(I,J)) * VM
            C4(I,J) = (Q12(I,J) * XNPGF(I,J) + Q22(I,J) * YNPGF(I,J)) * VM
        END DO
    END DO
!
    DO J=1,JM
        DO I=1,IM
            CLAP =        C1N(I,J) - C1N(I-1,J  ) + C2N(I  ,J  ) - C2N(I  ,J-1)                   &
    &            + D25 * (C1U(I,J) + C1D(I  ,J-1) - C1U(I-1,J  ) - C1D(I-1,J-1)                   &
    &            +        C2R(I,J) + C2L(I-1,J  ) - C2R(I  ,J-1) - C2L(I-1,J-1))
!
            PLAP = C3(I,J-1) - C3(I-1,J) + C4(I,J) - C4(I-1,J-1)
            DIV(I,J,L) = WPDAR(I,J) * (CLAP - PLAP) * DETA(L)
        END DO  
    END DO  
!-------------------
! NORMAL MASS FLUXES
!-------------------
    GDPD = GDPD * SQH
!
    CALL AVRH(GDPD, AVGD, IM, JM)
!
    DO J=0,JM
        DO I=0,IM
            UST = UT(I,J) 
            VST = VT(I,J)
!
            FXE(I,J) = AVGD(I,J) * (Q11(I,J) * UST + Q12(I,J) * VST)
            FYE(I,J) = AVGD(I,J) * (Q12(I,J) * UST + Q22(I,J) * VST)
        END DO
    END DO
!---------------------
! DIAGONAL MASS FLUXES 
!---------------------
    DO J=1,JM
        DO I=0,IM
            FXB(I,J) =  FXE(I,J) + FYE(I,J) + FXE(I,J-1) + FYE(I,J-1)
        END DO
    END DO
!
    DO J=0,JM
        DO I=1,IM
            FYB(I,J) = -FXE(I,J) + FYE(I,J) - FXE(I-1,J) + FYE(I-1,J)
        END DO
    END DO
!------------------------------------
! COMPONENTS OF HORIZONTAL OMEGA-ALFA
!------------------------------------
    DO J=0,JM
        DO I=0,IM
            TXE(I,J) = FXE(I,J) * (APEL(I+1,J  ) - APEL(I,J+1)) * (DFDZ(I+1,J  ) + DFDZ(I,J+1))
            TYE(I,J) = FYE(I,J) * (APEL(I+1,J+1) - APEL(I,J  )) * (DFDZ(I+1,J+1) + DFDZ(I,J  ))
        END DO
    END DO
!
    DO J=1,JM
        DO I=0,IM
            TXB(I,J) = FXB(I,J) * (APEL(I+1,J  ) - APEL(I,J  )) * (DFDZ(I+1,J  ) + DFDZ(I,J  ))
        END DO
    END DO
!
    DO J=0,JM
        DO I=1,IM
            TYB(I,J) = FYB(I,J) * (APEL(I  ,J+1) - APEL(I,J  )) * (DFDZ(I  ,J+1) + DFDZ(I,J  ))
        END DO
    END DO
!------------------------------------------
! HORIZONTAL OMEGA-ALFA TERM AND DIVERGENCE
!------------------------------------------
    DO J=1,JM
        DO I=1,IM 
            HM  = HTM(I,J,L)
            FOM = FCP / (GDPD(I,J))
!
            OMALFXE = TXE(I, J-1) + TXE(I-1, J  )
            OMALFYE = TYE(I, J  ) + TYE(I-1, J-1)
            OMALFXB = TXB(I, J  ) + TXB(I-1, J  )
            OMALFYB = TYB(I, J  ) + TYB(I  , J-1)
!
            FOMADD  = FOM * (OMALFXE + OMALFYE + OMALFXB + OMALFYB) * HM
!
            OMGALF(I,J,L) =               FOMADD
            T     (I,J,L) =    T(I,J,L) + FOMADD
            DIV   (I,J,L) = (DIV(I,J,L) + FDIV(I,J)                                               &
    &                     * (FXE(I,J-1) - FXE(I-1,J  )                                            &
    &                     +  FYE(I,J  ) - FYE(I-1,J-1))) * HM
!
            IF (MYPE == 0 .AND. ABS(DIV(I,J,L)) > DIVM) THEN
                DIVM = DIV(I,J,L)
            END IF
!
        END DO 
    END DO 
!
    DO J=0,JM
        DO I=0,IM
            U(I,J,L) = UT(I,J)
            V(I,J,L) = VT(I,J)
        END DO
    END DO
!
    END DO
!                               
    END SUBROUTINE ADJUST
