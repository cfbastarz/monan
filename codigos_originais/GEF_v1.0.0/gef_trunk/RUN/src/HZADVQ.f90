!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Inserir Brief de HZADVQ
!> @details Inserir Details de HZADVQ
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
!> @param[in] IM - Significado de IM
!> @param[in] JM - Significado de JM
!> @param[in] LM - Significado de LM
!> @details <b>Use Module:</b>
!! @arg @c CLDWTR
!! @arg @c CONTIN
!! @arg @c CTLBLK
!! @arg @c DYNAM
!! @arg @c F77KINDS
!! @arg @c MASKS
!! @arg @c METRCS
!! @arg @c MPPSTAFF
!! @arg @c PVRBLS
!! @arg @c REALPAR , ONLY: H1, H2, H4, D00
!! @arg @c VRBLS
!> @details <b>Driver:</b> 
!! @arg @c DIGFLT
!! @arg @c GEF
!> @details <b>Calls:</b>
!! @arg @c AVRH
!! @arg @c BOCOHMPI
!! @arg @c BOCOVMPI
!! @arg @c CORNERHM2
!! @arg @c FLUX_CORRECTION
!--------------------------------------------------------------------------------------------------
    SUBROUTINE HZADVQ
!--------------------------------------------------------------------------------------------------
! SUBROUTINE HZADVQ
! 
! SUBPROGRAM: HZADVQ - ?????
! PROGRAMMER: ?????          
! ORG: W/NP22
! DATE: ??-??-??
!
! ABSTRACT:
! ?????
!
! PROGRAM HISTORY LOG:
! ??-??-??  ?????     - ORIGINATOR
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
! USE MODULES: CLDWTR
!              CONTIN
!              CTLBLK
!              DYNAM
!              F77KINDS
!              MASKS
!              METRCS
!              MPPSTAFF
!              PVRBLS
!              REALPAR
!              VRBLS 
! 
! DRIVER     : DIGFLT
!              GEF
!
! CALLS      : AVRH
!              BOCOHMPI
!              BOCOVMPI
!              CORNERHM2
!              FLUX_CORRECTION
!--------------------------------------------------------------------------------------------------
    USE CLDWTR
    USE CONTIN
    USE CTLBLK
    USE DYNAM
    USE F77KINDS
    USE MASKS
    USE METRCS
    USE MPPSTAFF
    USE PARMETA , ONLY: IM, JM, LM
    USE PVRBLS
    USE REALPAR , ONLY: H1, H2, H4, D00
    USE VRBLS
!
    IMPLICIT NONE
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1)                                             ::&
    & DPDE    , GDPD    , AGDP    , UT      , VT      , QT      ,                                 &
    & UTLD    , VTLD    , FXE     , FYE     , QXE     , QYE     , ADQ     ,                       &
    & AGQT    , FAC1    , GDPDN   , GDPD2   ,                                                     &
    & QXEL    , QYEL    , QXEH    , QYEH    , HM      ,                                           &
    & WXEH    , WYEH    , WXEL    , WYEL    , ADW     , WT      , AGWT
!
!Foi modificado de 4 para 2 elementos na dimensao 3 para evitar passagem de valores espurios nos elementos 3 e 4
!GSM    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1, 4)                                          ::&
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1, 2)                                          ::&
    & ANTIDX  , ANTIDY  , QPRE
!
    INTEGER(KIND=I4)                                                                            ::&
    & L       , I       , J       , IT
!
    REAL   (KIND=R4)                                                                            ::&
    & TADXE   , TADYE   , RGDPDN  , Q_TEMP  , CWM_TEMP
!
    REAL   (KIND=R4)    , PARAMETER :: EPSQ   = 2.E-12
    REAL   (KIND=R4)    , PARAMETER :: CLIMIT = 1.E-20
!
    DO L=1,LM
!
        HM    = HTM(:,:,L)
        GDPD  = PDSL * DETA(L)
        GDPD2 = GDPD * SQH
        QT    =   Q(:,:,L)
        WT    = CWM(:,:,L)
        UT    =   U(:,:,L)
        VT    =   V(:,:,L)
!
        CALL AVRH(QT    ,AGQT, IM, JM)
        CALL AVRH(WT   , AGWT, IM, JM)
        CALL AVRH(GDPD2, AGDP, IM, JM)
!
        CALL CORNERHM2(AGDP, GDPD2)
!
        DO J=0,JM
            DO I=0,IM
                UTLD(I,J) = Q11(I,J) * UT(I,J) + Q12(I,J) * VT(I,J)
                VTLD(I,J) = Q12(I,J) * UT(I,J) + Q22(I,J) * VT(I,J)
!
                FXE(I,J) = UTLD(I,J) * AGDP(I,J) 
                FYE(I,J) = VTLD(I,J) * AGDP(I,J)
!  
                QXEH(I,J) = FXE(I,J) * AGQT(I,J)
                QYEH(I,J) = FYE(I,J) * AGQT(I,J)

                WXEH(I,J) = FXE(I,J) * AGWT(I,J)
                WYEH(I,J) = FYE(I,J) * AGWT(I,J)
!
                IF (FXE(I,J) > 0) THEN
                    QXEL(I,J) = H4 * FXE(I,J) * QT(I,J+1)
                    WXEL(I,J) = H4 * FXE(I,J) * WT(I,J+1)
                ELSE
                    QXEL(I,J) = H4 * FXE(I,J) * QT(I+1,J)
                    WXEL(I,J) = H4 * FXE(I,J) * WT(I+1,J)
                END IF
!
                IF (FYE(I,J) > 0) THEN
                    QYEL(I,J) = H4 * FYE(I,J) * QT(I,J)
                    WYEL(I,J) = H4 * FYE(I,J) * WT(I,J)
                ELSE
                    QYEL(I,J) = H4 * FYE(I,J) * QT(I+1,J+1)
                    WYEL(I,J) = H4 * FYE(I,J) * WT(I+1,J+1)
                END IF
!
                ANTIDX(I,J,1) = QXEH(I,J) - QXEL(I,J)
                ANTIDY(I,J,1) = QYEH(I,J) - QYEL(I,J)
                ANTIDX(I,J,2) = WXEH(I,J) - WXEL(I,J)
                ANTIDY(I,J,2) = WYEH(I,J) - WYEL(I,J)
            END DO
        END DO
!
        DO J=1,JM
            DO I=1,IM 
                GDPDN(I,J) = GDPD(I,J  ) - FDIV(I  ,J) * H2                                       &
    &                      * (FXE(I,J-1) -  FXE(I-1,J) + FYE(I,J) - FYE(I-1,J-1)) *HTM(I,J,L)
!
                IF (GDPDN(I,J) /= D00) THEN
                    RGDPDN    = H1 / GDPDN(I,J)
                    FAC1(I,J) = FADTQ * RSQH(I,J) * RGDPDN
!
                    ADQ(I,J) =  FAC1(I,J  ) *   HM(I  ,J)                                         &
    &                        * (QXEL(I,J-1) - QXEL(I-1,J) + QYEL(I,J) - QYEL(I-1,J-1))
!
                    ADW(I,J) =  FAC1(I,J  ) *   HM(I  ,J)                                         &
    &                        * (WXEL(I,J-1) - WXEL(I-1,J) + WYEL(I,J) - WYEL(I-1,J-1))
!
                    QPRE(I,J,1) = QT(I,J) * GDPD(I,J) * RGDPDN + ADQ(I,J)
                    QPRE(I,J,2) = WT(I,J) * GDPD(I,J) * RGDPDN + ADW(I,J)
                END IF
            END DO 
        END DO
!
!GSM       CALL BOCOHMPI(QPRE, 4)
       CALL BOCOHMPI(QPRE, 2)
!
       CALL FLUX_CORRECTION(QPRE(:,:,1), QT, FAC1, ANTIDX(:,:,1), ANTIDY(:,:,1))
       CALL FLUX_CORRECTION(QPRE(:,:,2), WT, FAC1, ANTIDX(:,:,2), ANTIDY(:,:,2))
!
!GSM       CALL BOCOVMPI(ANTIDX, ANTIDY, 4)
       CALL BOCOVMPI(ANTIDX, ANTIDY, 2)
!
        DO J=1,JM
            DO I=1,IM
                ADQ(I,J) = FAC1(I,J) * (ANTIDX(I,J-1,1) - ANTIDX(I-1,J  ,1)                       &
    &                    +              ANTIDY(I,J  ,1) - ANTIDY(I-1,J-1,1))
!
                ADW(I,J) = FAC1(I,J) * (ANTIDX(I,J-1,2) - ANTIDX(I-1,J  ,2)                       &
    &                    +              ANTIDY(I,J  ,2) - ANTIDY(I-1,J-1,2))
            END DO
        END DO
!
        DO J=1,JM
            DO I=1,IM
                Q_TEMP     =  QPRE(I,J,1) + ADQ(I,J)
                Q(I,J,L)   = AMAX1(Q_TEMP  , EPSQ ) * HTM(I,J,L)
                CWM_TEMP   =  QPRE(I,J,2) + ADW(I,J)
                CWM(I,J,L) = AMAX1(CWM_TEMP,CLIMIT) * HTM(I,J,L)
            END DO
        END DO
!----------------
! CLOSE DO L=1,LM
!----------------
    END DO
!
    CALL BOCOHMPI(Q  , LM)
    CALL BOCOHMPI(CWM, LM)
!
    END SUBROUTINE HZADVQ
