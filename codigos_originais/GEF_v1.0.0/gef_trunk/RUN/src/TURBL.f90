!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief VERTICAL TURBULENT EXCHANGE
!> @details UPDATES THE TURBULENT KINETIC ENERGY WITH THE PRODUCTION/DISSIPATION TERM AND THE VERTICAL 
!! DIFFUSION TERM DIFFUSION TERM (USING AN IMPLICIT FORMULATION). EXCHANGE COEFFICIENTS FOR THE 
!! SURFACE AND FOR ALL LAYER INTERFACES ARE THEN COMPUTED AND THE EXCHANGE IS EXECUTED.
!> @author ORIGINATOR - JANJIC
!> @date 95-03-20 \n
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
!! @arg @c LOOPS
!! @arg @c MASKS
!! @arg @c METRCS
!! @arg @c MPPSTAFF
!! @arg @c PARMETA
!! @arg @c PHYS
!! @arg @c PVRBLS
!! @arg @c VRBLS
!> @details <b>Driver:</b> 
!! @arg @c GEF
!> @details <b>Calls:</b>
!! @arg @c BOCOHMPI
!! @arg @c BOCOVMPI
!! @arg @c DIFCOF
!! @arg @c MIXLEN
!! @arg @c PRODQ2
!! @arg @c SFCDIF
!! @arg @c SURFCE
!! @arg @c VDIFH
!! @arg @c VDIFQ
!! @arg @c VDIFV
!--------------------------------------------------------------------------------------------------      
    SUBROUTINE TURBL
!--------------------------------------------------------------------------------------------------
! SUBROUTINE TURBL
!
! SUBROUTINE: TURBL - VERTICAL TURBULENT EXCHANGE
! PROGRAMMER: JANJIC
! ORG: W/NP2
! DATE: 95-03-20
!
! ABSTRACT:
! TURBL UPDATES THE TURBULENT KINETIC ENERGY WITH THE PRODUCTION/DISSIPATION TERM AND THE VERTICAL 
! DIFFUSION TERM DIFFUSION TERM (USING AN IMPLICIT FORMULATION). EXCHANGE COEFFICIENTS FOR THE 
! SURFACE AND FOR ALL LAYER INTERFACES ARE THEN COMPUTED AND THE EXCHANGE IS EXECUTED.
!
! PROGRAM HISTORY LOG:
! 95-03-15  JANJIC     - ORIGINATOR
! 95-03-28  BLACK      - CONVERSION FROM 1-D TO 2-D IN HORIZONTAL
! 96-03-29  BLACK      - ADDED EXTERNAL EDGE; REMOVED SCRCH COMMON
! 96-07-19  MESINGER   - ADDED Z0 EFFECTIVE
! 98-??-??  TUCCILLO   - MODIFIED FOR CLASS VIII PARALLELISM
! 98-10-27  BLACK      - PARALLEL CHANGES INTO MOST RECENT CODE
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
!              LOOPS
!              MASKS
!              METRCS
!              MPPSTAFF
!              PARMETA
!              PHYS
!              PVRBLS
!              VRBLS     
!
! DRIVER     : GEF
!
! CALLS      : BOCOHMPI
!              BOCOVMPI
!              DIFCOF
!              MIXLEN
!              PRODQ2
!              SFCDIF
!              SURFCE
!              VDIFH
!              VDIFQ
!              VDIFV
!--------------------------------------------------------------------------------------------------
    USE CTLBLK
    USE DYNAM
    USE F77KINDS
    USE LOOPS
    USE MASKS
    USE METRCS
    USE MPPSTAFF
    USE PARMETA
    USE PHYS
    USE PVRBLS
    USE VRBLS
!
    IMPLICIT NONE
!
    INTEGER(KIND=I4)    , PARAMETER :: KTMQ2 =   1
!
    REAL   (KIND=R4)    , PARAMETER :: CAPA  =   0.28589641
    REAL   (KIND=R4)    , PARAMETER :: G     =   9.80616
    REAL   (KIND=R4)    , PARAMETER :: RG    =   1.   /  G 
    REAL   (KIND=R4)    , PARAMETER :: ROG   = 287.04 /  G
    REAL   (KIND=R4)    , PARAMETER :: EPSZ  =   1.E-4
! 
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1)                                             ::&
    & CKLQ    , CT      , UZ0H    ,  VZ0H   , AKMSV
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1, LM)                                         ::&
    & APE     , UCOL    , VCOL
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1, LM-1)                                       ::&
    & AKH     , AKM     , AKMCOL  , AKHCOL
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1, LM+1)                                       ::&
    & ZINT    , ZCOL
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1)                                             ::&
    & DUM
!
    REAL   (KIND=R4)    , DIMENSION(LM1)                                                        ::&
    & GM      , GH      , EL
! 
    REAL   (KIND=R4)    , DIMENSION(4)                                                          ::&
    & ZEFF
!
    INTEGER(KIND=I4)    , DIMENSION(4)                                                          ::&
    & II1     , J1
!------------------------
! IMPLICIT NONE VARIABLES
!------------------------
    INTEGER(KIND=I4)                                                                            ::&
    & I       , J       , L       , K       , LMHK    , LMHP    , LMHM    , LMVK  
!  
    REAL   (KIND=R4)                                                                            ::&
    & PDSL    , APESTS  , WMSK    , RWMSK   , HPBL    , ULM     , VLM   
!-------------------------------------------------------------------     
! COMPUTE THE HEIGHTS OF THE LAYER INTERFACES AND THE EXNER FUNCTION
!------------------------------------------------------------------- 
    DO J=1,JM          
        DO I=1,IM
            ZINT(I,J,LP1) = EPSZ 
            IF (SIGMA) ZINT(I,J,LP1) = RG * FIS(I,J)
        END DO
    END DO
!
    DO L=LM,1,-1
        DO J=1,JM
            DO I=1,IM
                PDSL        = PD(I,J) * RES(I,J)
                APESTS      = PDSL    * AETA(L)  + PT
                ZINT(I,J,L) =  ZINT(I,J,L+1) + T(I,J,L)  / APESTS     * PDSL * DETA(L) * ROG      &
    &                       *    (Q(I,J,L)   * 0.608     + 1.)
!
                ZINT(I,J,L) = (ZINT(I,J,L)   - DFRLG(L)) * HTM(I,J,L) + DFRLG(L)
!            
                 APE(I,J,L) = (1.E5/APESTS)  ** CAPA
            END DO
        END DO
    END DO
!------------------- 
! REMOVE NEGATIVE Q2
!------------------- 
    DO L=1,LM
        DO J=1,JM
            DO I=1,IM
                Q2(I,J,L) = AMAX1(Q2(I,J,L),EPSQ2)
            END DO
        END DO
    END DO
!
    DO J=1,JM
        DO I=1,IM
            WMSK  = VTM(I,J,1) + VTM(I,J-1,1) + VTM(I-1,J,1) + VTM(I-1,J-1,1)
            RWMSK = 1. / WMSK
!
            II1 = (/I-1, I  , I-1, I/)
            J1  = (/J-1, J-1, J  , J/)
!
            UZ0H(I,J) = 0.
	    VZ0H(I,J) = 0.
!
	    DO K=1,4
	        UZ0H(I,J) = UZ0H(I,J) + (UZ0(II1(K), J1(K)) * QVH(I,J,K,1,1)                        &
    &                     +              VZ0(II1(K), J1(K)) * QVH(I,J,K,1,2)) * VTM(II1(K), J1(K), 1)
!
                VZ0H(I,J) = VZ0H(I,J) + (UZ0(II1(K), J1(K)) * QVH(I,J,K,2,1)                        &
                          +              VZ0(II1(K), J1(K)) * QVH(I,J,K,2,2)) * VTM(II1(K), J1(K), 1)
            END DO
!
            UZ0H(I,J) = UZ0H(I,J) * RWMSK
            VZ0H(I,J) = VZ0H(I,J) * RWMSK
        END DO
    END DO
!----------------------------------
! PREPARE THE EXCHANGE COEFFICIENTS
!----------------------------------
!
!----------------------------------------
! COMPUTE VELOCITY COMPONENTS AT H POINTS
!----------------------------------------
    DO L=1,LM
        DO J=1,JM
            DO I=1,IM
                WMSK = VTM(I,J,L) + VTM(I,J-1,L) + VTM(I-1,J,L) + VTM(I-1,J-1,L)
!
                IF (WMSK > 0.) THEN
                    RWMSK = 1. / WMSK
	            II1    = (/I-1, I  , I-1, I/)
	            J1     = (/J-1, J-1, J  , J/)
!
                    UCOL(I,J,L) = 0.
	            VCOL(I,J,L) = 0.
!
                    DO K=1,4
                        UCOL(I,J,L) = UCOL(I,J,L) + (U(II1(K), J1(K), L) * QVH(I,J,K,1,1)         &
    &                               +                V(II1(K), J1(K), L) * QVH(I,J,K,1,2))        &
    &                               *              VTM(II1(K), J1(K), L)
!
                        VCOL(I,J,L) = VCOL(I,J,L) + (U(II1(K), J1(K), L) * QVH(I,J,K,2,1)         &
    &                               +                V(II1(K), J1(K), L) * QVH(I,J,K,2,2))        &
    &                               *              VTM(II1(K), J1(K), L)
                    END DO
!
                    UCOL(I,J,L) = UCOL(I,J,L) * RWMSK
                    VCOL(I,J,L) = VCOL(I,J,L) * RWMSK
!
                ELSE
                    UCOL(I,J,L) = 0.
                    VCOL(I,J,L) = 0.
                END IF
!
            END DO
        END DO
    END DO
!-----------------------
! FIND THE MIXING LENGTH
!-----------------------
    DO J=1,JM
        DO I=1,IM
!
            LMHK = LMH(I,J)
            LMHP = LMHK + 1
            LMHM = LMHK - 1
!
            CALL MIXLEN(LMHK, HPBL, UCOL(I,J,1:LM), VCOL(I,J,1:LM), T(I,J,1:LM), Q(I,J,1:LM),     &
    &                                 Q2(I,J,1:LM),  APE(I,J,1:LM), ZINT(I,J,1:LP1),              &
    &                   GM(1:LM1)      , GH(1:LM1)      , EL(1:LM1))
!--------------------------------------------------------------------- 
! SOLVE FOR THE PRODUCTION/DISSIPATION OF THE TURBULENT KINETIC ENERGY
!---------------------------------------------------------------------    
            CALL PRODQ2(LMHK, DTQ2, USTAR(I,J), GM(1:LM1), GH(1:LM1), EL(1:LM1), Q2(I,J,1:LM))
!------------------------------------------------------  
! FIND THE EXCHANGE COEFFICIENTS IN THE FREE ATMOSPHERE
!------------------------------------------------------ 
            CALL DIFCOF(LMHK, GM(1:LM1), GH(1:LM1), EL(1:LM1), Q2(I,J,1:LM) , ZINT(I,J,1:LP1),    &
    &                                                         AKM(I,J,1:LM1),  AKH(I,J,1:LM1))
!------------------------------------------------------------- 
! CARRY OUT THE VERTICAL DIFFUSION OF TURBULENT KINETIC ENERGY
!-------------------------------------------------------------
            CALL VDIFQ(LMHK, KTMQ2, DTQ2, Q2(I,J,1:LM), EL(1:LM1), ZINT(I,J,1:LP1))
!---------------------- 
! FIND THE Z0 EFFECTIVE
!----------------------
            ZEFF(1) = ZEFFIJ(I,J,1)
            ZEFF(2) = ZEFFIJ(I,J,2)
            ZEFF(3) = ZEFFIJ(I,J,3)
            ZEFF(4) = ZEFFIJ(I,J,4)
!---------------------------------------  
! FIND THE SURFACE EXCHANGE COEFFICIENTS
!--------------------------------------- 
            ULM = UCOL(I,J,LMHK)
            VLM = VCOL(I,J,LMHK)
!
            CALL SFCDIF(LMHK,   SM(I,J)  ,    THS(I,J)  ,    QS(I,J),   UZ0H(I,J), VZ0H(I,J),     &
    &                         THZ0(I,J)  ,    QZ0(I,J)  , USTAR(I,J),     Z0(I,J), ZEFF(1:4),     & 
    &                         AKMS(I,J)  ,   AKHS(I,J)  , HPBL      ,     CT(I,J),  U10(I,J),     &
    &                          V10(I,J)  , TSHLTR(I,J)  ,  TH10(I,J), QSHLTR(I,J),  Q10(I,J),     &
    &                   ULM , VLM        ,                                                        &
    &                            T(I,J,1:LM),      Q(I,J,1:LM ),                                  &
    &                          APE(I,J,1:LM),   ZINT(I,J,1:LP1),    PD(I,J), PT,    T(I,J,LMHK))
!                         
        END DO
    END DO
!-------------------------------------
! AVERAGE UZ0 AND VZ0 BACK TO V POINTS
!-------------------------------------
    DO 125 J=1,JM-1
        DO 125 I=1,IM-1
            UZ0(I,J) = 0.
	    VZ0(I,J) = 0.
!
            II1 = (/I, I+1, I  , I+1/)
            J1  = (/J, J  , J+1, J+1/)
!
            DO K=1,4
                UZ0(I,J) = UZ0(I,J) + (UZ0H(II1(K), J1(K)) * QHV(I,J,K,1,1)                       &
    &                    +             VZ0H(II1(K), J1(K)) * QHV(I,J,K,1,2))
!
                VZ0(I,J) = VZ0(I,J) + (UZ0H(II1(K), J1(K)) * QHV(I,J,K,2,1)                       &
    &                    +             VZ0H(II1(K), J1(K)) * QHV(I,J,K,2,2))
            END DO
!
            UZ0(I,J) = UZ0(I,J) * 0.25
            VZ0(I,J) = VZ0(I,J) * 0.25
125 CONTINUE
!
    CALL BOCOVMPI(UZ0 , VZ0, 1)
    CALL BOCOHMPI(AKHS, 1)
!-----------------------------
! EXECUTE THE GROUND PROCESSES
!-----------------------------
    CALL SURFCE(APE, ZINT, CKLQ)
!------------------------------
! EXECUTE THE VERTICAL EXCHANGE
!------------------------------
    CALL BOCOHMPI(AKM , LM1)
    CALL BOCOHMPI(AKH , LM1)
    CALL BOCOHMPI(AKMS,   1)
!
    DO L=1,LM1
        DO J=1,JM
            DO I=1,IM
                AKMCOL(I,J,L) = (AKM(I,J,L) + AKM(I+1,J,L) + AKM(I,J+1,L) + AKM(I+1,J+1,L))       &
    &                         *  VTM(I,J,L) * 0.25
                AKHCOL(I,J,L) =  AKH(I,J,L) * HTM(I,J,L)
            END DO
        END DO
    END DO
!
    DO J=1,JM
        DO I=1,IM
             THZ0(I,J) = (1. - SM(I,J)) * THS(I,J) + SM(I,J) * THZ0(I,J)
             QZ0 (I,J) = (1. - SM(I,J)) * QS (I,J) + SM(I,J) * QZ0 (I,J)
!
            AKMSV(I,J) = (AKMS(I,J) + AKMS(I+1,J) + AKMS(I,J+1) + AKMS(I+1,J+1)) * 0.25
        END DO
    END DO
!      
    DO L=1,LP1
        DO J=1,JM-1
            DO I=1,IM-1
                ZCOL(I,J,L) = 0.25 * (ZINT(I,J,L) + ZINT(I+1,J,L) + ZINT(I,J+1,L) + ZINT(I+1,J+1,L))
            END DO
        END DO
    END DO
!
    DO J=1,JM
        DO I=1,IM
            LMHK = LMH(I,J)
!----------------------------------------------------------------
! CARRY OUT THE VERTICAL DIFFUSION OF TEMPERATURE AND WATER VAPOR
!----------------------------------------------------------------
            CALL VDIFH(LMHK, KTMQ2, DTQ2, THZ0(I,J)  ,  QZ0(I,J)  , AKHS(I,J)  ,                  &
    &                                       CT(I,J)  , CKLQ(I,J)  ,                               &
    &                                        T(I,J,:),    Q(I,J,:),  AKH(I,J,:),                  &
    &                                      APE(I,J,:), ZINT(I,J,:))                 
        END DO
    END DO
!
    DO J=1,JM-1
        DO I=1,IM-1
            LMVK = LMV(I,J)
!--------------------------------------------------------
! CARRY OUT THE VERTICAL DIFFUSION OF VELOCITY COMPONENTS
!--------------------------------------------------------
            CALL VDIFV(LMVK, KTMQ2, DTQ2,   UZ0(I,J),    VZ0(I,J)  ,                              &
    &                                     AKMSV(I,J),      U(I,J,:),    V(I,J,:),                 &
    &                                                 AKMCOL(I,J,:), ZCOL(I,J,:))

        END DO
    END DO
!------------------
!REMOVE NEGATIVE Q2
!------------------
    DO L=1,LM
        DO J=1,JM
            DO I=1,IM
                Q2(I,J,L) = AMAX1(Q2(I,J,L), EPSQ2)
            END DO
        END DO
    END DO
!
    CALL BOCOHMPI(T , LM)
    CALL BOCOHMPI(Q2, LM)
    CALL BOCOHMPI(Q , LM)
    CALL BOCOVMPI(U , V ,LM)
!
    RETURN
!
    END SUBROUTINE TURBL
