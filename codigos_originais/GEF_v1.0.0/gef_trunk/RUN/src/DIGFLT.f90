!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Inserir Brief de DIGFLT
!> @details Inserir Details de DIGFLT
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
!! @arg @c DGNSOUT
!! @arg @c DYNAM
!! @arg @c F77KINDS
!! @arg @c MASKS
!! @arg @c MPPSTAFF
!! @arg @c PARMETA
!! @arg @c PHYS
!! @arg @c PVRBLS
!! @arg @c VRBLS
!> @details <b>Driver:</b> 
!<
!> @details <b>Calls:</b>
!! @arg @c ADJUST
!! @arg @c CHECKMXMN
!! @arg @c HZADV
!! @arg @c HZADVQ
!! @arg @c PDETE
!! @arg @c PDNEW
!--------------------------------------------------------------------------------------------------
    SUBROUTINE DIGFLT
!--------------------------------------------------------------------------------------------------
! SUBROUTINE DIGFLT
! 
! SUBPROGRAM: DIGFLT - ?????
! PROGRAMMER: ?????
! ORG: ??????
! DATE: ??-??-??       
!     
! ABSTRACT:
! ?????
!
! PROGRAM HISTORY LOG:
! ??-??-??  ?????      - ORIGINATOR
! 18-01-15  LUCCI      - MODERNIZATION OF THE CODE, INCLUDING:
!                       * F77 TO F90/F95
!                       * INDENTATION & UNIFORMIZATION CODE
!                       * REPLACEMENT OF COMMONS BLOCK FOR MODULES
!                       * DOCUMENTATION WITH DOXYGEN
!                       * OPENMP FUNCTIONALITY
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
!              DGNSOUT
!              DYNAM
!              F77KINDS
!              MASKS
!              MPPSTAFF
!              PARMETA
!              PHYS
!              PVRBLS
!              VRBLS
!
! DRIVER     : -----
!
! CALLS      : BOCOHF
!              BOCOV
!              DIVHOA
!              DSTRB
!              EXCH
!              HZADV
!              LOC2GLB
!              MPI_BARRIER
!              MPI_BCAST
!              PDTEDT
!              PGCOR
!              VTADVF
!              ZERO2            
!-------------------------------------------------------------------------------------------------- 
    USE CTLBLK
    USE DGNSOUT
    USE DYNAM
    USE F77KINDS
    USE MASKS
    USE MPPSTAFF
    USE PARMETA
    USE PHYS
    USE PVRBLS
    USE VRBLS
!
    IMPLICIT NONE
!
    INTEGER(KIND=I4)    , PARAMETER :: NTIM =   60
!
    REAL   (KIND=R4)    , PARAMETER :: CM1  = 2937.4
    REAL   (KIND=R4)    , PARAMETER :: CM2  =    4.9283
    REAL   (KIND=R4)    , PARAMETER :: CM3  =   23.5518
    REAL   (KIND=R4)    , PARAMETER :: EPS  =    0.622
    REAL   (KIND=R4)    , PARAMETER :: PIQ  =    3.141592654
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1)                                             ::&
    & PDSL0   , PDQ
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1, LM)                                         ::&
    & TQ      , UQ      , VQ
!
    REAL   (KIND=R4)    , DIMENSION(:,:,:)  , ALLOCATABLE                                       ::&
    & TT      , UT      , VT      , RELHUM
!
    REAL   (KIND=R4)    , DIMENSION(:,:)    , ALLOCATABLE                                       ::&
    & PDT
!
    INTEGER(KIND=I4)                                                                            ::&
    & I       , J       , L       , NT
!
    REAL   (KIND=R4)                                                                            ::&
    & CLOGES  , ESE     , PRS     , QIJ     , E       , TIME    , QNT     , TETC    , CSTT    ,   &
    & SQ      , FNT     , AS1     , AS2     , ASS1    , ASS2    , BNT     , BS1     , BS2     ,   &
    & BSS1    , BSS2    , HNTSD   , DUM
!
    LOGICAL(KIND=L4)                                                                            ::&
    & LADV    , LADH    , LCKMM
!--------------------------------------------
! COMPUTE AND WRITE OUT THE RELATIVE HUMIDITY
!--------------------------------------------
    DO J=0,JM+1
        DO I=0,IM+1
            PDSL0(I,J) = RES(I,J) * PD(I,J)
        END DO
    END DO
!
    ALLOCATE (RELHUM(0:IM+1, 0:JM+1, LM))
!
    RELHUM = 0.
!
    DO L=1,LM
!
        DO J=0,JM+1
            DO I=0,IM+1
!
                IF (HTM(I,J,L) > 0.5) THEN
                    CLOGES        = -CM1 / T(I,J,L) - CM2 * ALOG10(T(I,J,L)) + CM3
                    ESE           = 10.  ** (CLOGES + 2.)
                    PRS           = AETA(L) * PDSL0(I,J) + PT
                    QIJ           = Q(I,J,L)
                    E             = PRS * QIJ / (EPS * (1. - QIJ) + QIJ)
                    RELHUM(I,J,L) = AMIN1(E / ESE, 0.98)
                ELSE
                    RELHUM(I,J,L) = 0.
                END IF
!
            END DO
        END DO
!
        DO J=0,JM+1
            DO I=0,IM+1
                RELHUM(I,J,L) = AMIN1(RELHUM(I,J,L), 0.98)
                RELHUM(I,J,L) = AMAX1(RELHUM(I,J,L), 0.02)
            END DO
        END DO
!
    END DO
!----------------------------------------------
! SET UP ARRAYS TO HOLD SUMS FOR DIGITAL FILTER
!----------------------------------------------
    PDQ = PD
    TQ  = T
    UQ  = U
    VQ  = V
!
    ALLOCATE (PDT(0:IM+1, 0:JM+1))
    ALLOCATE ( TT(0:IM+1, 0:JM+1, LM), UT(0:IM+1, 0:JM+1, LM), VT(0:IM+1, 0:JM+1, LM))
!
    PDT = PD
    TT  = T
    UT  = U
    VT  = V
!-------------------------------------------------------------
! RUN THE FORECAST MODEL FORWARD AND BACKWARD (DIGITAL FILTER)
!
! FIRST, FORWARD INTEGRATION
!-------------------------------------------------------------
!
!--------------------------------------------------------------------------------------------------
! CALCULATE NORM NEEDED FOR LANCZOS WINDOW
!
! 'TETC' DEFINES CUT-OFF FREQUENCY FOR THE WINDOW (FACTOR 2 APPEARS ONLY TO SHOW GENERAL FORMULA)
! (IT SHOULD BE TETC=PIQ/FLOAT(NTIM))
!
! 'NTIM' IS A NUMBER OF TIME STEPS IN A HALF-SPAN
!
! 'TIME' CORRESPONDS TO NUMBER OF TIME STEPS OF A SPAN
!--------------------------------------------------------------------------------------------------
    TIME = 2. * FLOAT(NTIM)
    QNT  = FLOAT(NTIM + 1)
    TETC = 2. * PIQ / TIME 
    CSTT = TETC / PIQ
    SQ   = CSTT
!
    DO NT=1,NTIM
        FNT  = FLOAT(NT)
        AS1  = PIQ * FNT / QNT
        AS2  = FNT * TETC
        ASS1 = SIN(AS1)  / AS1
        ASS2 = SIN(AS2)  / AS2
        SQ   = SQ  + 2.  * CSTT * ASS1 * ASS2
    END DO
!-------------------------------------
! NORMALIZATION OF THE WINDOW FUNCTION
!-------------------------------------
    CSTT = CSTT / SQ
!
    DO J=0,JM+1
        DO I=0,IM+1
            PDQ(I,J) = PDQ(I,J) * CSTT
        END DO
    END DO
!
    DO L=1,LM
        DO J=0,JM+1
            DO I=0,IM+1
                TQ(I,J,L) = TQ(I,J,L) * CSTT
                UQ(I,J,L) = UQ(I,J,L) * CSTT
                VQ(I,J,L) = VQ(I,J,L) * CSTT
            END DO
        END DO
    END DO
!
    DO NTSD=1,NTIM
        LADV = MOD(NTSD-1, IDTAD) .EQ. 0
        LADH = MOD(NTSD  , IDTAD) .EQ. 0
        LCKMM= MOD(NTSD  ,     2) .EQ. 0
!
        IF (MYPE == 0) WRITE(6,2015) NTSD, NTIM
2015    FORMAT(' NTSD=', I5, '  NTSTM=', I4)
!
        CALL ADJUST
        CALL PDETE
!
        IF (LADV) CALL VTADV
!
        CALL PDNEW
!
        IF (LADH) THEN
            CALL HZADVQ
            CALL HZADV
        END IF
!--------------------------- 
! CALCULATE WINDOW PARAMETER
!---------------------------
        BNT   = FLOAT(NTSD)
        BS1   = BNT * PIQ / QNT
        BS2   = BNT * TETC
        BSS1  = SIN(BS1) / BS1
        BSS2  = SIN(BS2) / BS2
        HNTSD = CSTT * BSS1 * BSS2
!
        DO L=1,LM
            DO J=0,JM+1
                DO I=0,IM+1
                    TQ(I,J,L) = TQ(I,J,L) + HNTSD * T(I,J,L)
                    UQ(I,J,L) = UQ(I,J,L) + HNTSD * U(I,J,L)
                    VQ(I,J,L) = VQ(I,J,L) + HNTSD * V(I,J,L)
                END DO
            END DO
        END DO
!
        DO J=0,JM+1
            DO I=0,IM+1
                PDQ(I,J) = PDQ(I,J) + HNTSD * PD(I,J)
            END DO
        END DO
!
        IF (LCKMM) CALL CHECKMXMN

    END DO
!--------------------------------------------------------- 
! NOW BACKWARD INTEGRATION, STARTING FROM THE INITIAL TIME
!---------------------------------------------------------
    PD = PDT
    T  = TT
    U  = UT
    V  = VT
!
    DEALLOCATE (PDT, TT, UT, VT)
!-----------------------------------------------
! CHANGE (SIGN ONLY OF) IMPORTANT TIME CONSTANTS
!-----------------------------------------------
    DT   =-DT
    FCP  =-FCP
    FADT =-FADT
    FADV =-FADV
    FKIN =-FKIN
!
    DO J=0,JM+1
        DO I=0,IM+1
            FDIV(I,J) =  -FDIV(I,J)
            DUM       =    F11(I,J)
             F11(I,J) =    F22(I,J)
             F22(I,J) =  DUM
            DUM       =    P11(I,J)
             P11(I,J) =   -P22(I,J)
             P22(I,J) = -DUM
             F12(I,J) =   -F12(I,J)
             F21(I,J) =   -F21(I,J)
        END DO
    END DO
!
    DO NTSD=1,NTIM 
!
        IF (MYPE == 0) WRITE(6,2015) NTSD, NTIM
!
        LADV  = MOD(NTSD-1, IDTAD) .EQ. 0
        LADH  = MOD(NTSD  , IDTAD) .EQ. 0
        LCKMM = MOD(NTSD  ,     2) .EQ. 0
!
        CALL ADJUST
        CALL PDETE
!
        IF (LADV) CALL VTADV
!
        CALL PDNEW
!
        IF (LADH) THEN
            CALL HZADVQ
            CALL HZADV
        END IF
!--------------------------- 
! CALCULATE WINDOW PARAMETER
!---------------------------
        BNT   = FLOAT(NTSD)
        BS1   = BNT  * PIQ  / QNT
        BS2   = BNT  * TETC
        BSS1  = SIN(BS1) / BS1
        BSS2  = SIN(BS2) / BS2
        HNTSD = CSTT * BSS1 * BSS2
!
        DO L=1,LM
            DO J=0,JM+1
                DO I=0,IM+1
                    TQ(I,J,L) = TQ(I,J,L) + HNTSD * T(I,J,L)
                    UQ(I,J,L) = UQ(I,J,L) + HNTSD * U(I,J,L)
                    VQ(I,J,L) = VQ(I,J,L) + HNTSD * V(I,J,L)
                END DO
            END DO
        END DO
!
        DO J=0,JM+1
            DO I=0,IM+1
                PDQ(I,J) = PDQ(I,J) + HNTSD * PD(I,J)
            END DO
        END DO
!
        IF (LCKMM) CALL CHECKMXMN
!
    END DO
!-----------------------------------------------
! CHANGE (SIGN ONLY OF) IMPORTANT TIME CONSTANTS
!-----------------------------------------------
    DT   = -DT
    FCP  = -FCP
    FADT = -FADT
    FADV = -FADV
    FKIN = -FKIN
!
    DO J=0,JM+1
        DO I=0,IM+1
            FDIV(I,J) = -FDIV(I,J)
            DUM       =   F11(I,J)
             F11(I,J) =   F22(I,J)
             F22(I,J) =  DUM
            DUM       =   P11(I,J)
             P11(I,J) =  -P22(I,J)
             P22(I,J) = -DUM
             F12(I,J) =  -F12(I,J)
             F21(I,J) =  -F21(I,J)
        END DO
    END DO
!----------------------------------------
! UPDATE INITIALIZED PROGNOSTIC VARIALBES
!----------------------------------------
    DO J=0,JM+1
        DO I=0,IM+1
            PD(I,J) = PDQ(I,J)
        END DO
    END DO
!
    DO L=1,LM
        DO J=0,JM+1
            DO I=0,IM+1
                T(I,J,L) = TQ(I,J,L)
                U(I,J,L) = UQ(I,J,L)
                V(I,J,L) = VQ(I,J,L)
            END DO
        END DO
    END DO
!--------------------------------------------------------------------------------------------
! RETRIEVE THE INITIAL RELATIVE HUMIDITY AND COMPUTE Q SO AS TO MAINTAIN THE RH GIVEN THE NEW 
! TEMPERATURES
!--------------------------------------------------------------------------------------------
    DO J=0,JM+1
        DO I=0,IM+1
            PDSL0(I,J) = RES(I,J) * PD(I,J)
        END DO
    END DO
!
    DO L=1,LM
        DO J=0,JM+1
            DO I=0,IM+1
!
                IF (HTM(I,J,L) > 0.5) THEN
                    CLOGES   = -CM1 / T(I,J,L) - CM2 * ALOG10(T(I,J,L)) + CM3
                    ESE      = 10. ** (CLOGES + 2.)
                    PRS      = AETA(L) * PDSL0(I,J) + PT
                    E        = RELHUM(I,J,L) * ESE
                    Q(I,J,L) = EPS * E / (PRS + E * (EPS - 1.))
                END IF
!
            END DO
        END DO
    END DO
!
    DEALLOCATE (RELHUM)
!
    END SUBROUTINE DIGFLT
