!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief HORIZONTAL ADVECTION
!> @details CALCULATES THE CONTRIBUTION OF THE HORIZONTAL ADVECTION TO THE TENDENCIES OF TEMPERATURE, 
!! WIND COMPONENTS, AND TURBULENT KINETIC ENERGY AND THEN UPDATES THOSE VARIABLES.
!! THE JANJIC ADVECTION SCHEME FOR THE ARAKAWA E GRID IS USED FOR ALL VARIABLES INSIDE THE FIFTH ROW
!! AN UPSTREAM SCHEME IS USED ON ALL VARIABLES IN THE THIRD, FOURTH, AND FIFTH OUTERMOST ROWS. 
!! A MODIFIED EULER-BACKWARD TIME SCHEME (HEUN) IS USED.
!! UNDERGROUND WINDS MUST BE EQUAL TO ZERO SINCE THEY ARE USED EXPLICITLY WITHOUT THE VELOCITY MASK 
!! IN THE FLUX CALCULATIONS.
!> @author ORIGINATOR - JANJIC 
!> @date 93-10-28 \n
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
!! @arg @c LOOPS
!! @arg @c MASKS
!! @arg @c METRCS
!! @arg @c MPPSTAFF
!! @arg @c PVRBLS
!! @arg @c REALPAR
!! @arg @c VRBLS
!> @details <b>Driver:</b> 
!! @arg @c DIGFLT
!! @arg @c GEF
!> @details <b>Calls:</b>
!! @arg @c AVRH
!! @arg @c BOCOHMPI
!! @arg @c BOCOVMPI
!! @arg @c CORNERHM2
!--------------------------------------------------------------------------------------------------
    SUBROUTINE HZADV
!--------------------------------------------------------------------------------------------------
! SUBROUTINE HZADV
! 
! SUBPROGRAM: HZADV - HORIZONTAL ADVECTION
! PROGRAMMER: JANJIC          
! ORG: W/NP22
! DATE: 93-10-28
!
! ABSTRACT:
! HZADV CALCULATES THE CONTRIBUTION OF THE HORIZONTAL ADVECTION TO THE TENDENCIES OF TEMPERATURE, 
! WIND COMPONENTS, AND TURBULENT KINETIC ENERGY AND THEN UPDATES THOSE VARIABLES.
! THE JANJIC ADVECTION SCHEME FOR THE ARAKAWA E GRID IS USED FOR ALL VARIABLES INSIDE THE FIFTH ROW
! AN UPSTREAM SCHEME IS USED ON ALL VARIABLES IN THE THIRD, FOURTH, AND FIFTH OUTERMOST ROWS. 
! A MODIFIED EULER-BACKWARD TIME SCHEME (HEUN) IS USED.
! UNDERGROUND WINDS MUST BE EQUAL TO ZERO SINCE THEY ARE USED EXPLICITLY WITHOUT THE VELOCITY MASK 
! IN THE FLUX CALCULATIONS.
!
! PROGRAM HISTORY LOG:
! 87-06-??  JANJIC     - ORIGINATOR
! 95-03-25  BLACK      - CONVERSION FROM 1-D TO 2-D IN HORIZONTAL
! 96-03-28  BLACK      - ADDED EXTERNAL EDGE
! 98-10-30  BLACK      - MODIFIED FOR DISTRIBUTED MEMORY
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
!              LOOPS
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
!              AVRV
!              BOCOHMPI
!              BOCOVMPI
!              CORNERHM2
!--------------------------------------------------------------------------------------------------
    USE CONTIN
    USE CTLBLK
    USE DYNAM
    USE F77KINDS
    USE LOOPS
    USE MASKS
    USE METRCS
    USE MPPSTAFF
    USE PARMETA , ONLY: IM, JM, LM
    USE PVRBLS
    USE REALPAR
    USE VRBLS
!
    IMPLICIT NONE
!
    REAL   (KIND=R4)    , PARAMETER :: R3 = 1. / 3.
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1)                                             ::&
    & HM      , VM      , DPDE    , GDPD    , RGDP    , AGDP    ,                                 &
    & UT      , VT      , TT      , UTLD    , VTLD    , ZET     ,                                 & 
    & FXE     , FYE     , FXB     , FYB     , AZT     , VYZ     , UXZ     ,                       &
    & TXE     , TYE     , TXB     , TYB     , ADU     , ADV     , ADT     ,                       &
    & EKN     , AKN     , Q2M     , Q2L     , Q2MXE   , Q2LXE   ,                                 &
    & Q2MYE   , Q2LYE   , Q2MXB   , Q2LXB   , Q2MYB   , Q2LYB   , ADQ2M   , ADQ2L   , QXE     ,   &
    & QYE     , QT      , UKIN    , VKIN
!
    INTEGER(KIND=I4)                                                                            ::&
    & I       , J       , L       , IT
!
    REAL   (KIND=R4)                                                                            ::&
    & ROTU1   , ROTU2   , ROTV1   , ROTV2   , FAC     , TADXE   , TADYE   , TADXB   , TADYB
!
    LOGICAL(KIND=L4)                                                                            ::&
    & ISCORNER2
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1, LM+1)                                       ::&
    & Q2ML    , ADQ2HL
!
    Q2ML(0:IM+1, 0:JM+1, 1) = 0.
!
    DO L=2,LM+1
        Q2ML(0:IM+1, 0:JM+1, L) = Q2(0:IM+1, 0:JM+1, L-1)
    END DO
!
    DO L=1,LM
!
          HM(:,:) =  HTM(0:IM+1, 0:JM+1, L)
          VM(:,:) =  VTM(0:IM+1, 0:JM+1, L)    
!
        DPDE(:,:) = PDSL(0:IM+1, 0:JM+1) * DETA(L)
        GDPD(:,:) = DPDE(:,:) * SQH(:,:)
!
        DO I=1,IM
            DO J=1,JM
                RGDP(I,J) = 1. / GDPD(I,J)
            END DO
        END DO
!    
        CALL BOCOHMPI(RGDP,1)
!
        UT(:,:) = U(0:IM+1, 0:JM+1, L)
        VT(:,:) = V(0:IM+1, 0:JM+1, L)
        TT(:,:) = T(0:IM+1, 0:JM+1, L)
!
        CALL AVRH(GDPD, AGDP, IM, JM)
!
        CALL CORNERHM2(AGDP, GDPD)
!
        Q2M(:,:) = Q2ML(0:IM+1, 0:JM+1, L  )
        Q2L(:,:) = Q2ML(0:IM+1, 0:JM+1, L+1)
!
        DO IT = 1, 2                   
!-------
! FLUXES
!-------
        DO J=0,JM
            DO I=0,IM
                UTLD(I,J) = Q11(I,J)  * UT(I,J) + Q12(I,J) * VT(I,J)
                VTLD(I,J) = Q12(I,J)  * UT(I,J) + Q22(I,J) * VT(I,J)
!
                 FXE(I,J) = AGDP(I,J) * UTLD(I,J)  
                 FYE(I,J) = AGDP(I,J) * VTLD(I,J)
!  
                 EKN(I,J) = (UTLD(I,J) * UT(I,J) + VTLD(I,J) * VT(I,J)) 
            END DO
        END DO
!
        CALL AVRV(EKN, AKN, IM, JM)      
!
        DO J=1,JM
            DO I=0,IM
                FXB(I,J) = FXE(I,J) + FYE(I,J) + FXE(I,J-1) + FYE(I,J-1)
            END DO
        END DO
!
        DO J=0,JM
            DO I=1,IM
                FYB(I,J) =-FXE(I,J) + FYE(I,J) - FXE(I-1,J) + FYE(I-1,J)
            END DO
        END DO
!---------
! MOMENTUM
!---------
        DO J=1,JM
            DO I=1,IM
                ZET(I,J) =  RGDP(I,J) * (VT(I,J-1) - VT(I-1,J) - UT(I,J) + UT(I-1,J-1))
            END DO
        END DO
!                
        CALL AVRH(ZET, AZT, IM, JM)  
!
        DO J=1,JM
            DO I=1,IM
                VYZ(I,J) = ZET(I,J) * (FYE(I-1, J-1) + FYE(I,J  ))
                UXZ(I,J) = ZET(I,J) * (FXE(I-1, J  ) + FXE(I,J-1))
            END DO
        END DO
!
        DO J=1,JM-1
            DO I=1,IM-1
                ROTU1 = 2. * FYE(I,J) * AZT(I,J)
                ROTV1 = 2. * FXE(I,J) * AZT(I,J)
!
                ROTU2 = VYZ(I  , J+1) + VYZ(I+1, J)
                ROTV2 = UXZ(I+1, J+1) + UXZ(I  , J)
!
                ADU(I,J) = ( FADV * (ROTU1 + ROTU2) + FKIN * (AKN(I+1,J  ) - AKN(I,J+1))) * VM(I,J)
                ADV(I,J) = (-FADV * (ROTV1 + ROTV2) + FKIN * (AKN(I+1,J+1) - AKN(I,J  ))) * VM(I,J)
            END DO
        END DO
!------------
! TEMPERATURE
!------------
        DO J=0,JM
            DO I=0,IM
                  TXE(I,J) = FXE(I,J) * ( TT(I+1, J  ) -  TT(I, J+1))
                  TYE(I,J) = FYE(I,J) * ( TT(I+1, J+1) -  TT(I, J  ))
                Q2MXE(I,J) = FXE(I,J) * (Q2M(I+1, J  ) - Q2M(I, J+1))
                Q2MYE(I,J) = FYE(I,J) * (Q2M(I+1, J+1) - Q2M(I, J  ))
                Q2LXE(I,J) = FXE(I,J) * (Q2L(I+1, J  ) - Q2L(I, J+1))
                Q2LYE(I,J) = FYE(I,J) * (Q2L(I+1, J+1) - Q2L(I, J  ))
            END DO
        END DO
!
        DO J=1,JM
            DO I=0,IM
                  TXB(I,J) = FXB(I,J) * ( TT(I+1, J) -  TT(I, J))
                Q2MXB(I,J) = FXB(I,J) * (Q2M(I+1, J) - Q2M(I, J))
                Q2LXB(I,J) = FXB(I,J) * (Q2L(I+1, J) - Q2L(I, J))
            END DO
        END DO
!
        DO J=0,JM
            DO I=1,IM
                  TYB(I,J) = FYB(I,J) * ( TT(I, J+1) -  TT(I, J))
                Q2MYB(I,J) = FYB(I,J) * (Q2M(I, J+1) - Q2M(I, J))
                Q2LYB(I,J) = FYB(I,J) * (Q2L(I, J+1) - Q2L(I, J))
            END DO
        END DO
!
        DO J=1,JM
            DO I=1,IM 
                FAC = FADT * RGDP(I,J)
!
                TADXE = TXE(I, J-1) + TXE(I-1, J  )
                TADYE = TYE(I, J  ) + TYE(I-1, J-1)
                TADXB = TXB(I, J  ) + TXB(I-1, J  )
                TADYB = TYB(I, J  ) + TYB(I,J- 1  )
!
                ADT(I,J) = FAC * HM(I,J) * (TADXE + TADYE + TADXB + TADYB)
!
                TADXE = Q2MXE(I, J-1) + Q2MXE(I-1, J  )
                TADYE = Q2MYE(I, J  ) + Q2MYE(I-1, J-1)
                TADXB = Q2MXB(I, J  ) + Q2MXB(I-1, J  )
                TADYB = Q2MYB(I, J  ) + Q2MYB(I  , J-1)
!
                ADQ2M(I,J) = FAC * HM(I,J) * (TADXE + TADYE + TADXB + TADYB) 
!  
                TADXE = Q2LXE(I, J-1) + Q2LXE(I-1, J  )
                TADYE = Q2LYE(I, J  ) + Q2LYE(I-1, J-1)
                TADXB = Q2LXB(I, J  ) + Q2LXB(I-1, J  )
                TADYB = Q2LYB(I, J  ) + Q2LYB(I  , J-1)
!
                ADQ2L(I,J) = FAC * HM(I,J) * (TADXE + TADYE + TADXB + TADYB)
            END DO 
        END DO 
!-------------
! FIRST UPDATE 
!-------------
        IF (IT == 1) THEN
            DO J=1,JM-1
                DO I=1,IM-1
                    UT(I,J) = UT(I,J) + ADU(I,J) * 0.707
                    VT(I,J) = VT(I,J) + ADV(I,J) * 0.707
                END DO
            END DO
!
            DO J=1,JM
                DO I=1,IM
                     TT(I,J) =  TT(I,J) +   ADT(I,J) * 0.707
                    Q2M(I,J) = Q2M(I,J) + ADQ2M(I,J) * 0.707
                    Q2L(I,J) = Q2L(I,J) + ADQ2L(I,J) * 0.707
	        END DO
            END DO
!
            CALL BOCOVMPI(UT , VT, 1)
            CALL BOCOHMPI(TT , 1)
            CALL BOCOHMPI(Q2M, 1)
            CALL BOCOHMPI(Q2L, 1)
!
        END IF
!
    END DO                         
!     
    DO J=1,JM-1
        DO I=1,IM-1
            U(I,J,L) = U(I,J,L) + ADU(I,J)
            V(I,J,L) = V(I,J,L) + ADV(I,J)
	END DO
    END DO
!
    DO J=1,JM
        DO I=1,IM
            T(I,J,L) = T(I,J,L) + ADT(I,J)
        END DO
    END DO
!
    DO J=1,JM
        DO I=1,IM
            ADQ2HL(I,J,L) = ADQ2L(I,J)
        END DO
    END DO
!
    IF (L > 1) THEN
        DO J=1,JM
            DO I=1,IM
                Q2(I,J,L-1) = Q2(I,J,L-1) + ADQ2M(I,J) * HM(I,J) * 0.5
            END DO
        END DO
    END IF
!
    END DO
! 
    DO L=2,LM
        DO I=1,IM
            DO J=1,JM
                Q2(I,J,L-1) = Q2(I,J,L-1) + ADQ2HL(I,J,L-1) * HTM(I,J,L) * 0.5
            END DO
        END DO
    END DO
!
    DO I=1,IM
        DO J=1,JM
            DO L=1,LMH(I,J)-1
                Q2(I,J,L) = AMAX1(Q2(I,J,L), EPSQ2)
            END DO
        END DO
    END DO
!               
    CALL BOCOVMPI(U , V , LM)
    CALL BOCOHMPI(T , LM)
    CALL BOCOHMPI(Q2, LM)
!
    END SUBROUTINE HZADV
