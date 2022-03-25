!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief CONVECTIVE PRECIPITATION PARAMETERIZATION
!> @details CALCULATES THE SUB-GRID SCALE CONVECTION INCLUDING DEEP AND SHALLOW CONVECTIVE CLOUDS 
!! FOLLOWING THE SCHEME DESCRIBED BY JANJIC (1994) BUT WITH SIGNIFICANT MODIFICATIONS.
!! IN ADDITION, THE LATENT HEAT RELEASE AND MOISTURE CHANGE DUE TO PRECIPITATING AND NON -
!! PRECIPITATING CLOUDS ARE COMPUTED.
!> @author ORIGINATOR - JANJIC 
!> @date 87-09-?? \n
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
!! @arg @c ACMCLH
!! @arg @c CNVCLD
!! @arg @c CTLBLK
!! @arg @c CUPARM
!! @arg @c DYNAM
!! @arg @c F77KINDS
!! @arg @c LOOPS
!! @arg @c MASKS
!! @arg @c MPPSTAFF
!! @arg @c PARMETA
!! @arg @c PARM_TBL
!! @arg @c PHYS
!! @arg @c PPTASM
!! @arg @c PVRBLS
!! @arg @c VRBLS
!> @details <b>Driver:</b> 
!<
!> @details <b>Calls:</b>
!! @arg @c TTBLEX
!--------------------------------------------------------------------------------------------------
    SUBROUTINE CUCNVC
!--------------------------------------------------------------------------------------------------
! SUBROUTINE CUCNVC
!
! SUBPROGRAM: CUCNVC - CONVECTIVE PRECIPITATION PARAMETERIZATION
! PROGRAMMER: JANJIC
! ORG: W/NP2 
! DATE: 93-11-02
!
! ABSTRACT:
! CUCNVC CALCULATES THE SUB-GRID SCALE CONVECTION INCLUDING DEEP AND SHALLOW CONVECTIVE CLOUDS 
! FOLLOWING THE SCHEME DESCRIBED BY JANJIC (1994) BUT WITH SIGNIFICANT MODIFICATIONS.
! IN ADDITION, THE LATENT HEAT RELEASE AND MOISTURE CHANGE DUE TO PRECIPITATING AND NON -
! PRECIPITATING CLOUDS ARE COMPUTED.
!
! PROGRAM HISTORY LOG:
! 87-09-??  JANJIC     - ORIGINATOR
! 90-11-21  JANJIC     - TWO SETS OF DSP PROFILES (FAST AND SLOW) REPLACE THE ORIGINAL ONE SET
! 95-03-25  BLACK      - CONVERSION FROM 1-D TO 2-D IN HORIZONTAL
! 96-03-28  BLACK      - ADDED EXTERNAL EDGE
! 98-11-02  BLACK      - MODIFIED FOR DISTRIBUTED MEMORY
! 18-03-20  LUCCI      - MODERNIZATION OF THE CODE, INCLUDING:
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
! USE MODULES: ACMCLH
!              CNVCLD
!              CTLBLK
!              CUPARM
!              DYNAM
!              F77KINDS
!              LOOPS
!              MASKS
!              MPPSTAFF
!              PARMETA
!              PARM_TBL
!              PHYS
!              PPTASM
!              PVRBLS
!              VRBLS
!  
! DRIVER     : -----
!
! CALLS      : TTBLEX
!-------------------------------------------------------------------------------------------------- 
!
!-----------------------------------------------------------------------------------------
! REFERENCES:                                                 
!
! JANJIC, Z.I., 1994:  THE STEP-MOUNTAIN ETA COORDINATE MODEL:
! FURTHER DEVELOPMENTS OF THE CONVECTION, VISCOUS SUBLAYER AND TURBULENCE CLOSURE SCHEMES.  
! MONTHLY WEATHER REVIEW, VOL. 122, 927-945.
!-----------------------------------------------------------------------------------------
!
!-----------------------------------------------------------------------
! WARNING: THIS SUBROUTINE WILL NOT WORK IF (LM .LT. 12);
! MUST BE CALLED IN THE SAME STEP WITH PROFQ2 BECAUSE PROFQ DEFINES APE;
!-----------------------------------------------------------------------
    USE ACMCLH
    USE CNVCLD
    USE CTLBLK
    USE CUPARM
    USE DYNAM
    USE F77KINDS
    USE LOOPS
    USE MASKS
    USE MPPSTAFF
    USE PARMETA
    USE PARM_TBL
    USE PHYS
    USE PPTASM
    USE PVRBLS
    USE VRBLS
!
    INTEGER(KIND=I4)    , PARAMETER :: IMJM_LOC = (IM + 2) * (JM + 2)
    INTEGER(KIND=I4)    , PARAMETER :: KSMUD    = 0
    INTEGER(KIND=I4)    , PARAMETER :: NROW     = 0   
!
    REAL   (KIND=R4)    , DIMENSION(LM)                                                         ::&
    & TREFK   , QREFK   , PK      , APEK    , TK      ,                                           &
    & THSK    , PSK     , APESK   , QK      , THERK   ,                                           &
    & THVREF  , THEVRF  , THVMOD  , DIFT    , DIFQ    ,                                           &
    & QSATK   , FPK
!
    INTEGER(KIND=I4)    , DIMENSION(LM)                                                         ::&
    & NTOPD   , NBOTD   ,                                                                         &
    & NTOPS   , NBOTS   ,                                                                         &
    & NDPTHD  , NDPTHS
!
    INTEGER(KIND=I4)    , DIMENSION(0:IM+1, 0:JM+1)                                             ::&
    & LTOP    , LBOT    ,                                                                         &
    & IPTB    , ITHTB
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1)                                             ::&
    & PTOP    , PBOT    ,                                                                         &
    & PDSL    , APEBT   ,                                                                         &
    & TBT     , Q2BT    ,                                                                         &
    & QQ      , PP      ,                                                                         &
    & PSP     , THBT    ,                                                                         &
    & THESP   , P       ,                                                                         &
    & BTH     , STH     ,                                                                         &
    & T00     , T10     ,                                                                         &
    & T01     , T11     ,                                                                         &
    & WF1     , WF2     ,                                                                         &
    & WF3     , WF4     ,                                                                         &
    & PRECOL
!
    INTEGER(KIND=I4)    , DIMENSION(IMJM_LOC)                                                   ::&
    &  IBUOY  , JBUOY   ,                                                                         &
    &  IDEEP  , JDEEP   ,                                                                         &
    &  ISHAL  , JSHAL   ,                                                                         &
    &  ILRES  , JLRES   ,                                                                         &
    &  IHRES  , JHRES
!                                                     
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1, LM)                                         ::&
    & APE     ,                                                                                   &
    & TREF    ,                                                                                   &
    & TMOD    ,                                                                                   &
    & QMOD
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1)                                             ::&
    & DSPB    , DSP0    ,                                                                         &
    & DSPT    ,                                                                                   &
    & TL      ,   QL    ,                                                                         &
    & TNE     ,  TSE    ,                                                                         &
    & QNE     ,  QSE                     
!
    DSP0 = 0.
    DSPB = 0.
    DSPT = 0.
    PSP  = 0.
!
    AVCNVC = AVCNVC + 1.
    ACUTIM = ACUTIM + 1.
!
    DTCNVC  = NCNVC * DT
    RDTCNVC = 1. / DTCNVC
    TAUK    = DTCNVC / TREL 
!------------ 
! DIAGNOSTICS
!------------ 
    DO L=1,LM
        NTOPD (L) = 0
        NBOTD (L) = 0
        NTOPS (L) = 0
        NBOTS (L) = 0
        NDPTHS(L) = 0
        NDPTHD(L) = 0
    END DO
!-------------
! PREPARATIONS
!-------------
!
!$omp parallel do 
!
    DO 120 J=0,JM+1
        DO 120 I=0,IM+1
              LBOT(I,J)   = LMH(I,J)
             THESP(I,J)   = 0.
              PDSL(I,J)   = RES(I,J) * PD(I,J)
            PRECOL(I,J)   = 0.
              TREF(I,J,1) = T(I,J,1)
  120 CONTINUE
!--------------------------------------------------------------------
! PADDING SPECIFIC HUMIDITY IF TOO SMALL RESTORE APE TO SCRATCH ARRAY
!--------------------------------------------------------------------
!
!$omp parallel do private (APESTS)
!
    DO 130 L=1,LM
        DO J=0,JM+1
            DO I=0,IM+1
                APESTS     = PDSL(I,J) * AETA(L) + PT
                APE(I,J,L) = (1.E5 / APESTS) ** CAPA
!
                IF (Q(I,J,L) < EPSQ) Q(I,J,L) = HTM(I,J,L) * EPSQ
            END DO
        END DO
130 CONTINUE
!---------------------------------- 
! SEARCH FOR MAXIMUM BUOYANCY LEVEL 
!---------------------------------- 
    DO 170 KB=1,LM
!--------------------------------------- 
! TRIAL MAXIMUM BUOYANCY LEVEL VARIABLES 
!--------------------------------------- 
!
!$omp parallel do private                                                                         &
!$omp         (APESP    , BQ      , BQS00K  , BQS10K  , IQ      , IT      , ITTB    , ITTBK   ,   &
!$omp          IQTB     , LMHK    , P00K    , P01K    , P10K    , P11K    , PKL     , PP1     ,   &
!$omp          PSFCK    , QBT     , QQ1     , SQ      , SQS00K  , SQS10K  , TPSP    , TQ      ,   &
!$omp          TTH      , TTHBT   , TTHES)
!
!
        DO 155 J=0,JM+1
            DO 150 I=0,IM+1
!            
                PKL   = AETA(KB)   * PDSL(I,J) + PT
                LMHK  = LMH(I,J)
                PSFCK = AETA(LMHK) * PDSL(I,J) + PT
!--------------------------------------------------------------------------------------------------
! NOW SEARCHING OVER A SCALED DEPTH IN FINDING THE PARCEL WITH THE MAX THETA-E INSTEAD OF THE OLD 
! 130 MB
!--------------------------------------------------------------------------------------------------
                IF (KB <= LMHK .AND. PKL >= 0.80*PSFCK) THEN
                    QBT   = Q(I,J,KB)
                    TTHBT = T(I,J,KB) * APE(I,J,KB)
                    TTH   = (TTHBT-THL) * RDTH
                    QQ1   = TTH - AINT(TTH)
                    ITTB  = INT(TTH) + 1
!---------------------------------
! KEEPING INDICES WITHIN THE TABLE
!---------------------------------
                    IF (ITTB < 1) THEN
                        ITTB = 1
                        QQ1  = 0.
                    END IF
!
                    IF (ITTB >= JTB) THEN
                        ITTB = JTB - 1
                        QQ1  = 0.
                    END IF
!
                    CONTINUE
!-------------------------------------------
! BASE AND SCALING FACTOR FOR SPEC. HUMIDITY
!-------------------------------------------
                    ITTBK  = ITTB
                    BQS00K = QS0(ITTBK)
                    SQS00K = SQS(ITTBK)
                    BQS10K = QS0(ITTBK+1)
                    SQS10K = SQS(ITTBK+1)
!---------------------------------------
! SCALING SPEC. HUMIDITY AND TABLE INDEX
!---------------------------------------
                    BQ  = (BQS10K-BQS00K) * QQ1 + BQS00K
                    SQ  = (SQS10K-SQS00K) * QQ1 + SQS00K
                    TQ  = (QBT-BQ) / SQ * RDQ
                    PP1 = TQ - AINT(TQ)
                    IQTB = INT(TQ) + 1
!---------------------------------
! KEEPING INDICES WITHIN THE TABLE
!---------------------------------
                    IF (IQTB < 1) THEN
                        IQTB = 1
                        PP1  = 0.
                    END IF
!
                    IF (IQTB >= ITB) THEN
                        IQTB = ITB - 1 
                        PP1  = 0.
                    END IF
!---------------------------------------------------
! SATURATION PRESSURE AT FOUR SURROUNDING TABLE PTS.
!---------------------------------------------------
                    IQ = IQTB
                    IT = ITTB
                    P00K = PTBL(IQ  ,IT  )
                    P10K = PTBL(IQ+1,IT  )
                    P01K = PTBL(IQ  ,IT+1)
                    P11K = PTBL(IQ+1,IT+1)
!-----------------------------------------
! SATURATION POINT VARIABLES AT THE BOTTOM
!-----------------------------------------
                    TPSP  = P00K + (P10K-P00K) * PP1 + (P01K-P00K) * QQ1 + (P00K-P10K-P01K+P11K)  &
    &                     * PP1 * QQ1
                    APESP = (1.E5/TPSP) ** CAPA
                    TTHES = TTHBT * EXP(ELOCP * QBT * APESP / TTHBT)
!---------------------------
! CHECK FOR MAXIMUM BUOYANCY
!---------------------------
                    IF (TTHES > THESP(I,J)) THEN
                          PSP(I,J) = TPSP
                         THBT(I,J) = TTHBT
                        THESP(I,J) = TTHES
                    END IF
!
                END IF
!
        150 END DO
    155 END DO
170 END DO
!------------------------------------------------
! CHOOSE CLOUD BASE AS MODEL LEVEL JUST BELOW PSP
!------------------------------------------------
    DO 240 L=1,LM1
        AETAL = AETA(L)
!------- 
! OPENMP
!------- 
!
!$omp parallel do
!
        DO J=0,JM+1
            DO I=0,IM+1
                P(I,J) = PDSL(I,J) * AETAL + PT
                IF (P(I,J) < PSP(I,J) .AND. P(I,J) >= PQM) LBOT(I,J) = L + 1
            END DO
        END DO
!  
240 END DO
!--------------------------------------------------------------
! WARNING: LBOT MUST NOT BE GT LMH(I,J)-1 IN SHALLOW CONVECTION
! MAKE SURE CLOUD BASE IS AT LEAST PONE ABOVE THE SURFACE
!--------------------------------------------------------------
!
!------- 
! OPENMP
!------- 
!
!$omp parallel do private (LMHIJ  , PSFCK)
!
    DO 250 J=0,JM+1
        DO 250 I=0,IM+1
            LMHIJ = LMH(I,J)
            PBOT(I,J) = AETA(LBOT(I,J)) * PDSL(I,J) + PT
            PSFCK     = AETA(LMHIJ)     * PDSL(I,J) + PT
            IF (PBOT(I,J) >= PSFCK-PONE .OR. LBOT(I,J) >= LMHIJ) THEN
!            
                DO L=1,LMHIJ-1
                    P(I,J) = AETA(L) * PDSL(I,J) + PT
                    IF (P(I,J) < PSFCK-PONE) LBOT(I,J) = L
                END DO
!            
                PBOT(I,J) = AETA(LBOT(I,J)) * PDSL(I,J) + PT
            END IF
250 END DO
!----------------------   
! CLOUD TOP COMPUTATION
!----------------------
!
!------- 
! OPENMP
!------- 
!
!$omp parallel do
!
    DO J=0,JM+1
        DO I=0,IM+1
            LTOP(I,J) = LBOT(I,J)
            PTOP(I,J) = PBOT(I,J)
        END DO
    END DO
!------- 
! OPENMP
!------- 
!
!$omp parallel do private                                                                         &
!$omp         (IHRES    , ILRES   , IPTB    , ITHTB   , JHRES   , JLRES   , KNUMH   , KNUML   ,   &
!$omp          PP       , PRESK   , QQ      )
!
    DO 290 L=LM,1,-1
!------------------------------------    
! SCALING PRESSURE AND TT TABLE INDEX
!------------------------------------
        KNUML = 0
        KNUMH = 0
!    
        DO 270 J=0,JM+1
            DO 270 I=0,IM+1
                PRESK = PDSL(I,J) * AETA(L) + PT
                IF (PRESK < PLQ) THEN
                    KNUML = KNUML + 1
                    ILRES(KNUML) = I
                    JLRES(KNUML) = J
                ELSE
                    KNUMH = KNUMH + 1
                    IHRES(KNUMH) = I
                    JHRES(KNUMH) = J
                END IF
    270 END DO
!---------------------------------------------------------------
! COMPUTE PARCEL TEMPERATURE ALONG MOIST ADIABAT FOR PRESSURE<PL
!---------------------------------------------------------------
        IF (KNUML > 0) THEN
            CALL TTBLEX(TREF(:,:,L)                                                               &
    &        ,          TTBL , ITB, JTB, KNUML, ILRES, JLRES, PDSL                                &
    &        ,          AETA(L)                                                                   &
    &        ,          HTM(:,:,L)                                                                &
    &        ,          PT   , PL                                                                 &
    &        ,          QQ   , PP                                                                 &
    &        ,          RDP  , THE0         , STHE, RDTHE                                         &
    &        ,          THESP, IPTB                                                               &
    &        ,          ITHTB)
        END IF
!---------------------------------------------------------------
! COMPUTE PARCEL TEMPERATURE ALONG MOIST ADIABAT FOR PRESSURE>PL
!---------------------------------------------------------------
        IF (KNUMH > 0) THEN
            CALL TTBLEX(TREF(:,:,L)                                                               &
    &       ,           TTBLQ, ITBQ, JTBQ , KNUMH, IHRES, JHRES, PDSL                             &
    &       ,           AETA(L)                                                                   &
    &       ,           HTM(:,:,L)                                                                &
    &       ,           PT   , PLQ                                                                &
    &       ,           QQ   , PP                                                                 &
    &       ,           RDPQ , THE0Q      , STHEQ, RDTHEQ                                         &
    &       ,           THESP, IPTB                                                               &
    &       ,           ITHTB)
        END IF
290 END DO
!---------------    
! BUOYANCY CHECK
!---------------
    DO 295 L=LM,1,-1
!------- 
! OPENMP
!------- 
!
!$omp parallel do
!
        DO J=0,JM+1
            DO I=0,IM+1
                IF (TREF(I,J,L) > T(I,J,L)-DTTOP) LTOP(I,J) = L
            END DO
        END DO
!
295 END DO
!-------------------
! CLOUD TOP PRESSURE
!-------------------
!
!------- 
! OPENMP
!-------
! 
!$omp parallel do
!
    DO J=0,JM+1
        DO I=0,IM+1
            PTOP(I,J) = AETA(LTOP(I,J)) * PDSL(I,J) + PT
        END DO
    END DO
!----------------------------------
! DEFINE AND SMOOTH DSPS AND CLDEFI
! UNIFIED OR SEPARATE LAND/SEA CONV 
!----------------------------------
    IF (UNIS) THEN
!------- 
! OPENMP
!------- 
!
!$omp parallel do private (EFI)
!
        DO J=0,JM+1
            DO I=0,IM+1
                EFI = CLDEFI(I,J)
                DSPB(I,J) = (EFI-EFIMN) * SLOPBS + DSPBSS
                DSP0(I,J) = (EFI-EFIMN) * SLOP0S + DSP0SS
                DSPT(I,J) = (EFI-EFIMN) * SLOPTS + DSPTSS
            END DO
        END DO
!
    ELSE IF (.NOT. UNIL) THEN
!------- 
! OPENMP
!------- 
!
!$omp parallel do private (EFI)
!
        DO J=0,JM+1
            DO I=0,IM+1
                EFI=CLDEFI(I,J)
                DSPB(I,J) = ((EFI-EFIMN)*SLOPBS+DSPBSS) * SM(I,J) + ((EFI-EFIMN)*SLOPBL+DSPBSL)   &
    &                     * (1.-SM(I,J))
!
                DSP0(I,J) = ((EFI-EFIMN)*SLOP0S+DSP0SS) * SM(I,J) + ((EFI-EFIMN)*SLOP0L+DSP0SL)   &
    &                     * (1.-SM(I,J))
!
                DSPT(I,J) = ((EFI-EFIMN)*SLOPTS+DSPTSS) * SM(I,J) + ((EFI-EFIMN)*SLOPTL+DSPTSL)   &
    &                     * (1.-SM(I,J))
            END DO
        END DO
!
    ELSE
!------- 
! OPENMP
!-------
! 
!$omp parallel do private (EFI)
!
        DO J=0,JM+1
            DO I=0,IM+1
                EFI=CLDEFI(I,J)
                DSPB(I,J) = ((EFI-EFIMN) * SLOPBL + DSPBSL)
                DSP0(I,J) = ((EFI-EFIMN) * SLOP0L + DSP0SL)
                DSPT(I,J) = ((EFI-EFIMN) * SLOPTL + DSPTSL)
            END DO
        END DO
!
    END IF
!------------------------------------------------
! EXTENDING SEA STRUCTURES INLAND ALONG COASTLINE
!------------------------------------------------
    IF (NROW > 0 .AND. .NOT. UNIS .AND. .NOT. UNIL) THEN
!------- 
! OPENMP
!------- 
!
!$omp parallel do
!
        DO J=0,JM+1
            DO I=0,IM+1
                WF1(I,J) = 0.
                WF2(I,J) = 0.
                WF3(I,J) = 0.
                WF4(I,J) = 0.
            END DO
        END DO
!    
        KROW = NROW
!    
        DO 350 KOUNT=1,KROW
!------- 
! OPENMP
!------- 
!
!$omp parallel do
!
            DO 345 J=1,JM
                DO 345 I=1,IM
                    WF1(I,J) = (DSPB(I  ,J-1)  + DSPB(I  ,J+1)                                    &
    &                        +  DSPB(I+1,J  )  + DSPB(I-1,J  )                                    &
    &                        + 4. * DSPB(I,J)) * 0.125
!
                    WF2(I,J) = (DSP0(I  ,J-1)  + DSP0(I  ,J+1)                                    &
    &                        +  DSP0(I+1,J  )  + DSP0(I-1,J  )                                    &
    &                        + 4. * DSP0(I,J)) * 0.125
!
                    WF3(I,J) = (DSPT(I  ,J-1)  + DSPT(I  ,J+1)                                    &
    &                        +  DSPT(I+1,J  )  + DSPT(I-1,J  )                                    &
    &                        + 4. * DSPT(I,J)) * 0.125
!
                    WF4(I,J) = (CLDEFI(I  ,J-1)  + CLDEFI(I  ,J+1)                                &
    &                        +  CLDEFI(I+1,J  )  + CLDEFI(I-1,J  )                                &
    &                        + 4. * CLDEFI(I,J)) * 0.125
        345 END DO
!------- 
! OPENMP
!-------
! 
!$omp parallel do private (RSMK   , SMK)
!
            DO J=0,JM+1
                DO I=0,IM+1
                    SMK = SM(I,J)
                    RSMK = 1. - SMK
                      DSPB(I,J) =   DSPB(I,J) * SMK + WF1(I,J) * RSMK
                      DSP0(I,J) =   DSP0(I,J) * SMK + WF2(I,J) * RSMK
                      DSPT(I,J) =   DSPT(I,J) * SMK + WF3(I,J) * RSMK
                    CLDEFI(I,J) = CLDEFI(I,J) * SMK + WF4(I,J) * RSMK
                END DO
            END DO
!        
    350 END DO
!
    END IF
!-----------------------------------------------
! NITIALIZE CHANGES OF T AND Q DUE TO CONVECTION
!-----------------------------------------------
!
!------- 
! OPENMP
!------- 
!
!$omp parallel do
!
    DO 360 K=1,LM
        DO J=0,JM+1
            DO I=0,IM+1
                TMOD(I,J,K) = 0.
                QMOD(I,J,K) = 0.
            END DO
        END DO
360 END DO
!-------------------------------------------
! CLEAN UP AND GATHER DEEP CONVECTION POINTS 
!-------------------------------------------
!
!------- 
! OPENMP
!------- 
!
!$omp parallel do
!
    DO 380 J=0,JM+1
        DO 380 I=0,IM+1
            IF (LTOP(I,J) >= LBOT(I,J)) THEN
                LBOT(I,J) = 0
                LTOP(I,J) = LBOT(I,J)
                PTOP(I,J) = PBOT(I,J)
            END IF
!
            IF (PTOP(I,J) > PBOT(I,J)-PNO .OR. LTOP(I,J) > LBOT(I,J)-2)                           &
    &           CLDEFI(I,J) = AVGEFI * SM(I,J) + STEFI * (1.-SM(I,J))
!
380 END DO
!
    KHDEEP = 0
    PSHNEW = 20000.
!
    DO J=0,JM+1
        DO I=0,IM+1
            PSFCIJ = PD(I,J) + PT
!--------------------------------------------------------------------------------------------------
! DEPTH OF CLOUD REQUIRED TO MAKE THE POINT A DEEP CONVECTION POINT IS NOW A SCALED VALUE OF THE 
! PSFC INSTEAD OF 290 MB EVERYWHERE
!--------------------------------------------------------------------------------------------------
            DEPMIN = PSHNEW * PSFCIJ * 1.E-5
            DEPTH  = PBOT(I,J) - PTOP(I,J)
!
            IF (DEPTH >= DEPMIN) THEN
                KHDEEP = KHDEEP + 1
                IDEEP(KHDEEP) = I
                JDEEP(KHDEEP) = J
            END IF
        END DO
    END DO
!------------------------------------
! HORIZONTAL LOOP FOR DEEP CONVECTION
!------------------------------------
!
!------- 
! OPENMP
!------- 
!
!$omp parallel do private                                                                         &
!$omp         (APEK     , APEKL   , APEKXX  , APEKXY  , APESK   , AVRGT   , AVTGTL  , DENTPY  ,   &
!$omp          DEPMIN   , DEPTH   , DEPWL   , DHDT    , DIFQ    , DIFQL   , DIFT    , DIFTL   ,   &
!$omp          DRHEAT   , DRHDP   , DSP     , DSP0K   , DSPBK   , DSPTK   , DTHEM   , EFI     ,   &
!$omp          FEFI     , HCORR   , I       , J       , L0      , L0M1    , LB      , LBM1    ,   &
!$omp          LBTK     , LCOR    , LQM     , LSHU    , LTP1    , LTP2    , LTPK    , LTSH    ,   &
!$omp          PBTK     , PK      , PK0     , PKB     , PKL     , PKT     , PRECK   , PSFCIJ  ,   &
!$omp          PSK      , PTHRS   , PTPK    , QK      , QKL     , QREFK   , QSATK   , RDP0T   ,   &
!$omp          RHH      , RHL     , RHMAX   , SUMDE   , SUMDP   , THERK   , THERKX  , THERKY  ,   &
!$omp          THSK     , THSKL   , TK      , TKL     , TREFK   , TREFKX  , TSKL    )
!
    DO 600 N=1,KHDEEP
!
        I = IDEEP(N)
        J = JDEEP(N)
        PSFCIJ = PD(I,J) + PT
        LTPK = LTOP(I,J)
        LBTK = LBOT(I,J)
!---------------
!DEEP CONVECTION
!---------------
        LB    = LBTK
        EFI   = CLDEFI(I,J)
        DSPBK =   DSPB(I,J)
        DSP0K =   DSP0(I,J)
        DSPTK =   DSPT(I,J)
!--------------------------------------------------------------------------------------------------
! INITIALIZE VARIABLES IN THE CONVECTIVE COLUMN 
!
! ONE SHOULD NOTE THAT THE VALUES ASSIGNED TO THE ARRAY TREFK IN THE 410 LOOP ARE REALLY ONLY 
! RELEVANT IN ANCHORING THE REFERENCE TEMPERATURE PROFILE AT LEVEL LB.
! WHEN BUILDING THE REFERENCE PROFILE FROM CLOUD BASE, THEN ASSIGNING THE AMBIENT TEMPERATURE TO 
! TREFK IS ACCEPTABLE.
! HOWEVER, WHEN BUILDING THE REFERENCE PROFILE FROM SOME OTHER LEVEL (SUCH AS ONE LEVEL ABOVE THE 
! GROUND), THEN TREFK SHOULD BE FILLED WITH THE TEMPERATURES IN TREF(I,J,L) WHICH ARE THE 
! TEMPERATURES OF THE MOIST ADIABAT THROUGH CLOUD BASE.
! BY THE TIME THE LINE NUMBERED 450 HAS BEEN REACHED, TREFK ACTUALLY DOES HOLD THE REFERENCE 
! TEMPERATURE PROFILE.
!--------------------------------------------------------------------------------------------------
        DO 410 K=1,LM
             DIFT(K) =  0.
             DIFQ(K) =  0.
            TKL      =    T(I,J,K)
               TK(K) =  TKL
            TREFK(K) =  TKL
            QKL      =    Q(I,J,K)
               QK(K) =  QKL
            QREFK(K) =  QKL
            PKL      =  AETA(K) * PDSL(I,J) + PT
               PK(K) =  PKL
              PSK(K) =  PKL
            APEKL    =  APE(I,J,K)
             APEK(K) = APEKL
            THERK(K) = TREF(I,J,K) * APEKL
    410 END DO
!----------------------------------------------
! DEEP CONVECTION REFERENCE TEMPERATURE PROFILE
!----------------------------------------------
        LTP1 = LTPK + 1
        LBM1 = LB   - 1
        PKB  = PK(LB)
        PKT  = PK(LTPK)
!---------------------------------------------------
! TEMPERATURE REFERENCE PROFILE BELOW FREEZING LEVEL
!---------------------------------------------------
        L0     = LB
        PK0    =    PK(LB)
        TREFKX = TREFK(LB)
        THERKX = THERK(LB)
        APEKXX =  APEK(LB)
        THERKY = THERK(LBM1)
        APEKXY =  APEK(LBM1)
!    
        DO 420 K=LBM1,LTPK,-1
            IF (T(I,J,K+1) < TFRZ) GOTO 430
            STABDL   = STABD
            TREFKX   = ((THERKY-THERKX) * STABDL + TREFKX * APEKXX) / APEKXY
            TREFK(K) = TREFKX
            APEKXX   = APEKXY
            THERKX   = THERKY
            APEKXY   =  APEK(K-1)
            THERKY   = THERK(K-1)
            L0       = K
            PK0      = PK(L0)
    420 END DO
!-----------------------------------------
! FREEZING LEVEL AT OR ABOVE THE CLOUD TOP
!-----------------------------------------       
        L0M1 = L0 - 1
!
        GOTO 450
!---------------------------------------------------
! TEMPERATURE REFERENCE PROFILE ABOVE FREEZING LEVEL 
!---------------------------------------------------
        430 L0M1 = L0 - 1
        RDP0T = 1. / (PK0 - PKT)
        DTHEM = THERK(L0) - TREFK(L0) * APEK(L0)
!    
        DO K=LTPK,L0M1
            TREFK(K) = (THERK(K) - (PK(K) - PKT) * DTHEM * RDP0T) / APEK(K)
        END DO
!-------------------------------------------
! DEEP CONVECTION REFERENCE HUMIDITY PROFILE 
!-------------------------------------------
!
!------------------------------------------------------------------------------------------------
! REFERENCE PROFILE HAD BEEN TOO DRY IN THE CASE WHERE THE CLOUD BASE WAS CLOSE TO FREEZING LEVEL 
!------------------------------------------------------------------------------------------------
        450 DEPTH = PFRZ * PSFCIJ * 1.E-5
        DEPWL = PKB - PK0
!
        DO 460 K=LTPK,LB
!-------------------------------
! SATURATION PRESSURE DIFFERENCE
!-------------------------------
            IF (DEPWL >= DEPTH) THEN
                IF (K < L0) THEN
                    DSP = ((PK0 - PK(K)) * DSPTK + (PK(K) - PKT) * DSP0K) / (PK0 - PKT)
                ELSE
                    DSP = ((PKB - PK(K)) * DSP0K + (PK(K) - PK0) * DSPBK) / (PKB - PK0)
                END IF
            ELSE
                DSP = DSP0K
                IF (K < L0) DSP = ((PK0 - PK(K)) * DSPTK + (PK(K) - PKT) * DSP0K) / (PK0 - PKT)
            END IF
!-----------------
! HUMIDITY PROFILE
!-----------------
            IF (PK(K) > PQM) THEN
                  PSK(K) = PK(K) + DSP
                APESK(K) = (1.E5 / PSK(K)) ** CAPA
                 THSK(K) = TREFK(K) * APEK(K)
                QREFK(K) = PQ0 / PSK(K) * EXP(A2 * (THSK(K) - A3 * APESK(K))                      &
    &                    /                         (THSK(K) - A4 * APESK(K)))
            ELSE
                QREFK(K) = Q(I,J,K)
            END IF
    460 END DO
!-------------------------------
! ENTHALPY CONSERVATION INTEGRAL
!-------------------------------
        DO 520 ITER=1,2
!
            SUMDE = 0.
            SUMDP = 0.
!        
            DO K=LTPK,LB
                SUMDE = ((TK(K) - TREFK(K)) * CP + (QK(K) - QREFK(K)) * ELWV) * DETA(K) + SUMDE
                SUMDP = SUMDP   +  DETA(K)
            END DO
!        
            HCORR = SUMDE / (SUMDP - DETA(LTPK))
            LCOR  = LTPK + 1
!--------- 
! FIND LQM 
!---------
            DO K=1,LB
                IF (PK(K) <= PQM) LQM = K
            END DO
!-----------------------------------
! ABOVE LQM CORRECT TEMPERATURE ONLY
!-----------------------------------
            IF (LCOR <= LQM) THEN
                DO K=LCOR,LQM
                    TREFK(K) = TREFK(K) + HCORR * RCP
                END DO
                LCOR = LQM + 1
            END IF
!------------------------------------------------
! BELOW LQM CORRECT BOTH TEMPERATURE AND MOISTURE
!------------------------------------------------
            DO 510 K=LCOR,LB
                TSKL     = TREFK(K) * APEK(K) / APESK(K)
                DHDT     = QREFK(K) * A23M4L  / (TSKL-A4) ** 2 + CP
                TREFK(K) = HCORR / DHDT + TREFK(K)
                THSKL    = TREFK(K) * APEK(K)
                QREFK(K) = PQ0 / PSK(K) * EXP(A2 * (THSKL - A3 * APESK(K))                        &
    &                    /                         (THSKL - A4 * APESK(K)))
        510 END DO
!
    520 END DO
!-----------------------------------
! HEATING, MOISTENING, PRECIPITATION
!-----------------------------------
        DENTPY = 0.
        AVRGT  = 0.
        PRECK  = 0.
!    
        DO 530 K=LTPK,LB
            TKL     = TK(K)
            DIFTL   = (TREFK(K) - TKL  ) * TAUK
            DIFQL   = (QREFK(K) - QK(K)) * TAUK
            AVRGTL  = (TKL + TKL + DIFTL)
            DENTPY  = (DIFTL * CP + DIFQL * ELWV) * DETA(K) / AVRGTL + DENTPY
            AVRGT   = AVRGTL * DETA(K) + AVRGT
            PRECK   = DETA(K) * DIFTL + PRECK
            DIFT(K) = DIFTL
            DIFQ(K) = DIFQL
    530 END DO
!    
        DENTPY = DENTPY + DENTPY
        AVRGT  = AVRGT / (SUMDP + SUMDP)
!-------------------------------------
! SWAP IF ENTROPY AND/OR PRECIP .LT. 0
!-------------------------------------
        IF (DENTPY < EPSNTP .OR. PRECK < 0.) THEN
            IF (OCT90) THEN
                CLDEFI(I,J) = EFIMN
            ELSE
                CLDEFI(I,J) = EFIMN * SM(I,J) + STEFI * (1. - SM(I,J))
            END IF
!-----------------------------         
! SEARCH FOR SHALLOW CLOUD TOP
!-----------------------------
            LBTK = LBOT(I,J)
            LTSH = LBTK
            LBM1 = LBTK - 1
            PBTK = PK(LBTK)
!----------------------------------
! USE NEW THRESHOLD FOR CLOUD DEPTH
!----------------------------------
            PSFCIJ = PD(I,J) + PT
            DEPMIN = PSHNEW * PSFCIJ * 1.E-5
            PTPK   = PBTK - DEPMIN
!-------------------------------------------
! CLOUD TOP IS THE LEVEL JUST BELOW PBTK-PSH
!-------------------------------------------
            DO K=1,LM
                IF (PK(K) <= PTPK) LTPK = K + 1
            END DO
            PTPK=PK(LTPK)
!-----------------------------------------------
! HIGHEST LEVEL ALLOWED IS LEVEL JUST BELOW PSHU
!-----------------------------------------------
            IF (PTPK <= PSHU) THEN
                DO K=1,LM
                    IF (PK(K) <= PSHU) LSHU = K + 1
                END DO
                LTPK = LSHU
                PTPK = PK(LTPK)
            END IF
!        
            LTP1 = LTPK + 1
            LTP2 = LTPK + 2
!
            DO K=LTPK,LBTK
                QSATK(K) = PQ0 / PK(K) * EXP(A2 * (TK(K) - A3) / (TK(K) - A4))
            END DO
!
            RHH = QK(LTPK) / QSATK(LTPK)
            RHMAX = 0.
!        
            DO 570 K=LTP1,LBM1
                RHL = QK(K) / QSATK(K)
                DRHDP = (RHH-RHL) / (PK(K-1) - PK(K))
                IF (DRHDP > RHMAX) THEN
                    LTSH  = K - 1
                    RHMAX = DRHDP
                END IF
                RHH = RHL
        570 END DO
!        
            LTOP(I,J) = LTSH
!----------------------------------------
! CLOUD MUST BE AT LEAST TWO LAYERS THICK
!----------------------------------------
            IF (LBOT(I,J)-LTOP(I,J) < 2) LTOP(I,J) = LBOT(I,J) - 2
!        
            PTOP(I,J) = PK(LTOP(I,J))
            GOTO 600
!
        END IF
!--------------------------
! DEEP CONVECTION OTHERWISE
!--------------------------
        DRHEAT = (PRECK * SM(I,J) + AMAX1(EPSP,PRECK) * (1. - SM(I,J))) * CP / AVRGT
        EFI    = EFIFC * DENTPY / DRHEAT
!-----------------------------------
! UNIFIED OR SEPARATE LAND/SEA CONV.
!-----------------------------------
        IF (OCT90) THEN
            IF (UNIS) THEN
                EFI = CLDEFI(I,J) * FCB + EFI * FCC
            ELSE IF ( .NOT. UNIL) THEN
                EFI = (CLDEFI(I,J) * FCB + EFI * FCC) * SM(I,J) + 1. - SM(I,J)
            ELSE
                EFI = 1.
            END IF
        ELSE
            EFI = CLDEFI(I,J) * FCB + EFI * FCC
        END IF
!    
        IF(EFI > 1.   )    EFI = 1.
        IF(EFI < EFIMN)    EFI = EFIMN
        IF(PRECK == 0.)    EFI = 1.
!
        CLDEFI(I,J) = EFI
!    
        FEFI  = EFMNT + SLOPE * (EFI - EFIMN)
        PRECK = PRECK * FEFI
!-----------------------------------------------    
! UPDATE PRECIPITATION, TEMPERATURE AND MOISTURE
!-----------------------------------------------   
        PRECOL(I,J) = PDSL(I,J) * PRECK * CPRLG
        PREC  (I,J) = PDSL(I,J) * PRECK * CPRLG + PREC  (I,J)
        CUPREC(I,J) = PDSL(I,J) * PRECK * CPRLG + CUPREC(I,J)
        ACPREC(I,J) = PDSL(I,J) * PRECK * CPRLG + ACPREC(I,J)
         CUPPT(I,J) = PDSL(I,J) * PRECK * CPRLG + CUPPT (I,J)
!    
        DO K=LTPK,LB
              TMOD(I,J,K) = DIFT(K) * FEFI
              QMOD(I,J,K) = DIFQ(K) * FEFI
            TLATCU(I,J,K) = DIFT(K) * FEFI
        END DO
!-----------------------
! END OF DEEP CONVECTION
!-----------------------
    600 END DO
!
    NDEEP = 0
!
    DO 620 J=0,JM+1
        DO 620 I=0,IM+1
            LTPK = LTOP(I,J)
            LBTK = LBOT(I,J)
            LB   =  LMH(I,J) - 1
            PSFCIJ = PD(I,J) + PT
            DEPMIN = PSHNEW * PSFCIJ * 1.E-5
!
            IF (PTOP(I,J) < PBOT(I,J)-DEPMIN) THEN
                NDEEP = NDEEP + 1
                NDEPTH = LB - LTPK
                NTOPD (LTPK  ) = NTOPD (LTPK  ) + 1
                NBOTD (LB    ) = NBOTD (LB    ) + 1
                NDPTHD(NDEPTH) = NDPTHD(NDEPTH) + 1
            END IF
620 END DO
!
    NNEG = KHDEEP - NDEEP
!--------------------------------- 
! GATHER SHALLOW CONVECTION POINTS
!---------------------------------
    KHSHAL = 0
    NDSTN  = 0
    NDSTP  = 0
!
    DO 630 J=0,JM+1
        DO 630 I=0,IM+1
            IF (PTOP(I,J) > PBOT(I,J)-PNO .OR. LTOP(I,J) > LBOT(I,J)-2) GOTO 630
            PSFCIJ = PD(I,J) + PT
            DEPMIN = PSHNEW * PSFCIJ * 1.E-5
!
            IF (PTOP(I,J)+1. >= PBOT(I,J)-DEPMIN) THEN
                KHSHAL = KHSHAL + 1
                ISHAL(KHSHAL) = I
                JSHAL(KHSHAL) = J
            END IF
!
630 END DO
!----------------------------------------
! HORIZONTAL LOOP FOR SHALLOW CONVECTION
!----------------------------------------
!
!------- 
! OPENMP
!------- 
!
!$omp parallel do private                                                                         &
!$omp         (APEK     , APEKL   , APEKXX  , APEKXY  , BQK     , BQS00K  , BQS10K  , DEN     ,   &
!$omp          DENTPY   , DPKL    , DPMIX   , DQREF   , DST     , DSTQ    , DTDETA  , FPK     ,   &
!$omp          FPTK     , I       , IQ      , IT      , J       , LBM1    , LBTK    , LTP1    ,   &
!$omp          LTPK     , OTSUM   , PART1   , PART2   , PART3   , PK      , PKL     , PKXXXX  ,   &
!$omp          PKXXXY   , POTSUM  , PPK     , PSUM    , PTPK    , PZ0     , QK      , QKL     ,   &
!$omp          QNEW     , QOTSUM  , QQK     , QREFK   , QRFKL   , QRFTP   , QSATK   , QSUM    ,   &
!$omp          RDPSUM   , RTBAR   , SMIX    , SQK     , SQS00K  , SQS10K  , SUMDP   , SUMDT   ,   &
!$omp          TCORR    , THVMKL  , THVREF  , TK      , TKL     , TQK     , TREFK   , TREFKX  ,   &
!$omp          TRFKL    , TTHK)
!
    DO 800 N=1,KHSHAL
!
        I = ISHAL(N)
        J = JSHAL(N)
!------------------- 
! SHALLOW CONVECTION
!------------------- 
        PZ0  =  PD(I,J) + PT
        LLMH = LMH(I,J)
!    
        DO 650 K=1,LLMH
            TKL      = T(I,J,K)
            TK   (K) = TKL
            TREFK(K) = TKL
            QKL      = Q(I,J,K)
            QK   (K) = QKL
            QREFK(K) = QKL
            PKL      = AETA(K) * PDSL(I,J) + PT
            PK   (K) = PKL
            QSATK(K) = PQ0 / PK(K) * EXP(A2 * (TK(K) - A3) / (TK(K) - A4))
            APEKL    = APE(I,J,K)
!-----------------------------
! CHOOSE THE PRESSURE FUNCTION
!
! FPK  (K) =ALOG(PKL)
! FPK  (K) =PKL
! FPK  (K) =-1./PKL
!-----------------------------
            APEK (K)  = APEKL
            THVMKL    = TKL * APEKL * (QKL * 0.608 + 1.)
            THVREF(K) = THVMKL
        650 END DO
!------------------ 
! SHALLOW CLOUD TOP
!------------------ 
        LBTK = LBOT(I,J)
        LBM1 = LBTK - 1
        PTPK = PTOP(I,J)
        LTP1 = LTOP(I,J)
        LTPK = LTOP(I,J) - 1
!
        IF (PTOP(I,J) > PBOT(I,J)-PNO .OR. LTOP(I,J) > LBOT(I,J)-2) THEN
            LBOT(I,J) = 0
            LTOP(I,J) = LBOT(I,J)
            PTOP(I,J) = PBOT(I,J)
            GOTO 800
        END IF
!-----------------------------------------------------
! SCALING POTENTIAL TEMPERATURE AND TABLE INDEX AT TOP
!-----------------------------------------------------
        THTPK = T(I,J,LTPK) * APE(I,J,LTPK)
!    
        TTHK = (THTPK - THL) * RDTH
        QQK  = TTHK - AINT(TTHK)
        IT   = INT(TTHK) + 1
!
        IF (IT < 1) THEN
            IT  = 1
            QQK = 0.
        END IF
!
        IF (IT >= JTB) THEN
            IT  = JTB - 1
            QQK = 0.
        END IF
!--------------------------------------------------
! BASE AND SCALING FACTOR FOR SPEC. HUMIDITY AT TOP
!--------------------------------------------------
        BQS00K = QS0(IT)
        SQS00K = SQS(IT)
        BQS10K = QS0(IT+1)
        SQS10K = SQS(IT+1)
!----------------------------------------------
! SCALING SPEC. HUMIDITY AND TABLE INDEX AT TOP
!----------------------------------------------
        BQK = (BQS10K - BQS00K) * QQK + BQS00K
        SQK = (SQS10K - SQS00K) * QQK + SQS00K
!
        TQK = (Q(I,J,LTPK) - BQK) / SQK * RDQ
!    
        PPK = TQK - AINT(TQK)
        IQ = INT(TQK) + 1
!
        IF (IQ < 1) THEN
            IQ  = 1
            PPK = 0.
        END IF
!
        IF (IQ >= ITB) THEN
            IQ  = ITB - 1
            PPK = 0.
        END IF
!------------------------------------
! CLOUD TOP SATURATION POINT PRESSURE
!------------------------------------
        PART1 = (PTBL(IQ+1,IT) - PTBL(IQ  ,IT)) * PPK
        PART2 = (PTBL(IQ,IT+1) - PTBL(IQ  ,IT)) * QQK
        PART3 = (PTBL(IQ,IT  ) - PTBL(IQ+1,IT) - PTBL(IQ,IT+1) + PTBL(IQ+1,IT+1)) * PPK * QQK
        PTPK  =  PTBL(IQ,IT  ) + PART1 + PART2 + PART3
!
        DPMIX = PTPK - PSP(I,J)
        IF (ABS(DPMIX) < 3000.) DPMIX = -3000.
!-------------------------- 
! TEMPERATURE PROFILE SLOPE
!-------------------------- 
        SMIX = (THTPK - THBT(I,J)) / DPMIX * STABS
!    
        TREFKX = TREFK(LBTK+1)
        PKXXXX =    PK(LBTK+1)
        PKXXXY =    PK(LBTK)
        APEKXX =  APEK(LBTK+1)
        APEKXY =  APEK(LBTK)
!    
        DO 670 K=LBTK,LTP1,-1
            TREFKX   = ((PKXXXY - PKXXXX) * SMIX + TREFKX * APEKXX) / APEKXY
            TREFK(K) = TREFKX
            APEKXX   = APEKXY
            PKXXXX   = PKXXXY
            APEKXY   = APEK(K-1)
            PKXXXY   =   PK(K-1)
    670 END DO
!-----------------------------------------
! TEMPERATURE REFERENCE PROFILE CORRECTION 
!-----------------------------------------
        SUMDT = 0.
        SUMDP = 0.
!    
        DO K=LTP1,LBTK
            SUMDT = (TK(K) - TREFK(K)) * DETA(K) + SUMDT
            SUMDP = SUMDP + DETA(K)
        END DO
!    
        RDPSUM = 1. / SUMDP
        FPK(LBTK) = TREFK(LBTK)
!    
        TCORR=SUMDT*RDPSUM
!    
        DO K=LTP1,LBTK
            TRFKL    = TREFK(K)+TCORR
            TREFK(K) = TRFKL
            FPK  (K) = TRFKL
        END DO
!--------------------------- 
! HUMIDITY PROFILE EQUATIONS
!---------------------------
        PSUM   = 0.
        QSUM   = 0.
        POTSUM = 0.
        QOTSUM = 0.
        OTSUM  = 0.
        DST    = 0.
        FPTK   = FPK(LTP1)
!    
        DO 700 K=LTP1,LBTK
            DPKL   = FPK(K) - FPTK
            PSUM   = DPKL  * DETA(K) + PSUM
            QSUM   = QK(K) * DETA(K) + QSUM
            RTBAR  = 2. / (TREFK(K) + TK(K))
            OTSUM  = DETA(K) * RTBAR + OTSUM
            POTSUM = DPKL   * RTBAR * DETA(K) + POTSUM
            QOTSUM = QK(K)  * RTBAR * DETA(K) + QOTSUM
            DST    = (TREFK(K) - TK(K)) * RTBAR * DETA(K) + DST
    700 END DO
!    
        PSUM   = PSUM * RDPSUM
        QSUM   = QSUM * RDPSUM
        ROTSUM = 1. / OTSUM
        POTSUM = POTSUM * ROTSUM
        QOTSUM = QOTSUM * ROTSUM
        DST    = DST    * ROTSUM * CP / ELWV
!------------------------------- 
! ENSURE POSITIVE ENTROPY CHANGE 
!-------------------------------
        IF (DST > 0.) THEN
            LBOT(I,J) = 0
            LTOP(I,J) = LBOT(I,J)
            PTOP(I,J) = PBOT(I,J)
            GOTO 800       
        ELSE
            DSTQ = DST * EPSDN
        END IF
!-------------------------------- 
! CHECK FOR ISOTHERMAL ATMOSPHERE
!--------------------------------
        DEN = POTSUM - PSUM
!    
        IF (-DEN/PSUM < 5.E-5) THEN
            LBOT(I,J) = 0
            LTOP(I,J) = LBOT(I,J)
            PTOP(I,J) = PBOT(I,J)
            GOTO 800
!----------------------------------------        
! SLOPE OF THE REFERENCE HUMIDITY PROFILE
!----------------------------------------
        ELSE
            DQREF = (QOTSUM-DSTQ-QSUM) / DEN
        END IF
!--------------------------------------- 
! HUMIDITY DOES NOT INCREASE WITH HEIGHT
!---------------------------------------
        IF (DQREF < 0.) THEN
            LBOT(I,J) = 0
            LTOP(I,J) = LBOT(I,J)
            PTOP(I,J) = PBOT(I,J)
            GOTO 800
        END IF
!--------------------------
! HUMIDITY AT THE CLOUD TOP
!--------------------------
        QRFTP = QSUM - DQREF * PSUM
!----------------- 
! HUMIDITY PROFILE
!-----------------
        DO 720 K=LTP1,LBTK
            QRFKL = (FPK(K) - FPTK) * DQREF + QRFTP
!------------------------------------------
! SUPERSATURATION OR NEGATIVE Q NOT ALLOWED
!-------------------------------------------    
            QNEW = (QRFKL - QK(K)) * TAUK + QK(K)
!
            IF (QNEW > QSATK(K)*STRESH .OR. QNEW < 0.) THEN
                LBOT(I,J) = 0
                LTOP(I,J) = LBOT(I,J)
                PTOP(I,J) = PBOT(I,J)
                GOTO 800
            END IF
!
            THVREF(K) = TREFK(K) * APEK(K) * (QRFKL * 0.608 + 1.)
             QREFK(K) = QRFKL
    720 END DO
!-----------------------------------------------
! ELIMINATE IMPOSSIBLE SLOPES (BETTS, DTHETA/DQ)
!----------------------------------------------- 
        DO 730 K=LTP1,LBTK
            DTDETA = (THVREF(K-1) - THVREF(K)) / (AETA(K) - AETA(K-1))
            IF (DTDETA < EPSTH) THEN
                LBOT(I,J) = 0
                LTOP(I,J) = LBOT(I,J)
                PTOP(I,J) = PBOT(I,J)
                GOTO 800
            END IF
    730 END DO
!-------------------- 
! DIAGNOSTICS 
!
! IF (DST.GT.0.) THEN
! NDSTP = NDSTP + 1
! ELSE
! NDSTN = NDSTN + 1
! END IF
!--------------------
        DENTPY = 0.
!    
        DO K=LTP1,LBTK
            DENTPY = ((TREFK(K) - TK(K)) * CP + (QREFK(K) - QK(K)) * ELWV)                        &
    &              / (TK(K) + TREFK(K)) * DETA(K) + DENTPY
        END DO
!--------------------------------------    
! RELAXATION TOWARDS REFERENCE PROFILES 
!-------------------------------------- 
        DO 750 K=LTP1,LBTK
            TMOD(I,J,K) = (TREFK(K) - TK(K)) * TAUK
            QMOD(I,J,K) = (QREFK(K) - QK(K)) * TAUK
    750 END DO
!-------------------------- 
! END OF SHALLOW CONVECTION
!-------------------------- 
800 END DO
!------------
! DIAGNOSTICS
!------------
    NSHAL = 0
!
    DO 820 J=0,JM
        DO 820 I=0,IM+1
            LTPK = LTOP(I,J)
            LBTK = LBOT(I,J)
            PTPK = PTOP(I,J)
            PBTK = PBOT(I,J)
!
            IF (PTPK > PBTK-PNO .OR. LTPK > LBTK-2) GOTO 820
            PSFCIJ = PD(I,J) + PT
            DEPMIN = PSHNEW * PSFCIJ * 1.E-5
!
            IF (PTPK >= PBTK-DEPMIN) THEN
                NSHAL          = NSHAL + 1
                NTOPS(LTPK)    = NTOPS(LTPK) + 1
                NBOTS(LBTK)    = NBOTS(LBTK) + 1
                NDEPTH         = LBTK - LTPK
                NDPTHS(NDEPTH) = NDPTHS(NDEPTH) + 1
            END IF
!
820 END DO
!
    NEGDS = KHSHAL - NSHAL
!---------------------------------------------
! SMOOTHING TEMPERATURE & HUMIDITY CORRECTIONS
!---------------------------------------------
    IF (KSMUD == 0) THEN
!------- 
! OPENMP
!-------
! 
!$omp parallel do
!    
        DO 900 K=1,LM
!-----------------------------------------
! UPDATE THE FUNDAMENTAL PROGNOSTIC ARRAYS
!-----------------------------------------
            DO 830 J=0,JM+1
                DO 830 I=0,IM+1
                    T(I,J,K) = T(I,J,K) + TMOD(I,J,K)
                    Q(I,J,K) = Q(I,J,K) + QMOD(I,J,K)
!---------------------------------------------------------------------------------------------------------------
! ACCUMULATE LATENT HEATING DUE TO CONVECTION.
! SCALE BY THE RECIPROCAL OF THE PERIOD AT WHICH THIS ROUTINE IS CALLED. THIS PERIOD IS THE CONVECTION TIMESTEP.
!---------------------------------------------------------------------------------------------------------------
                    TCUCN(I,J,K) = TCUCN(I,J,K) + TMOD(I,J,K) * RDTCNVC
        830 END DO
    900 END DO
!
    ELSE
!------- 
! OPENMP
!------- 
!
!$omp parallel do private (QL     , QNE     , QSE     , TL      , TNE     , TSE)
!    
        DO 910 K=1,LM
!        
            QL  = 0.
            QNE = 0.
            QSE = 0.
            TL  = 0.
            TNE = 0.
            TSE = 0.
!
            DO J=0,JM+1
                DO I=0,IM+1
                    TL(I,J) = TMOD(I,J,K)
                    QL(I,J) = QMOD(I,J,K)
                END DO
            END DO
!
            NSMUD = KSMUD
!        
            DO 870 KS=1,NSMUD
!            
                DO J=0,JM+1
                    DO I=0,IM+1
                        TNE(I,J) = (TL(I,J+1) - TL(I,J)) * HTM(I,J,K) * HTM(I,J+1,K)
                        QNE(I,J) = (QL(I,J+1) - QL(I,J)) * HTM(I,J,K) * HTM(I,J+1,K)
                    END DO
                END DO
!            
                DO J=1,JM+1
                    DO I=0,IM+1
                        TSE(I,J) = (TL(I+1,J) - TL(I,J)) * HTM(I+1,J,K) * HTM(I,J,K)
                        QSE(I,J) = (QL(I+1,J) - QL(I,J)) * HTM(I+1,J,K) * HTM(I,J,K)
                    END DO
                END DO
!            
                DO J=1,JM
                    DO I=0,IM+1
                        TL(I,J) = (TNE(I,J) - TNE(I,J-1) + TSE(I,J) - TSE(I-1,J)) * 0.125 + TL(I,J)
                        QL(I,J) = (QNE(I,J) - QNE(I,J-1) + QSE(I,J) - QSE(I-1,J)) * 0.125 + QL(I,J)
                    END DO
                END DO
!            
        870 END DO
!-----------------------------------------
! UPDATE THE FUNDAMENTAL PROGNOSTIC ARRAYS
!-----------------------------------------
            DO J=0,JM+1
                DO I=0,IM+1
                    T(I,J,K) = T(I,J,K) + TL(I,J)
                    Q(I,J,K) = Q(I,J,K) + QL(I,J)
                END DO
            END DO
!---------------------------------------------------------------------------------------------------------------
! ACCUMULATE LATENT HEATING DUE TO CONVECTION.
! SCALE BY THE RECIPROCAL OF THE PERIOD AT WHICH THIS ROUTINE IS CALLED. THIS PERIOD IS THE CONVECTION TIMESTEP.
!---------------------------------------------------------------------------------------------------------------
            DO J=0,JM+1
                DO I=0,IM+1
                    TCUCN(I,J,K) = TCUCN(I,J,K) + TL(I,J) * RDTCNVC
                END DO
            END DO
!        
    910 END DO
!    
    END IF
!----------------------------------------
! SAVE CLOUD TOP AND BOTTOM FOR RADIATION
!----------------------------------------
!
!------- 
! OPENMP
!------- 
!
!$omp parallel do
!
    DO J=0,JM+1
        DO I=0,IM+1
            IF (LTOP(I,J) > 0 .AND. LTOP(I,J) < LBOT(I,J)) THEN
!
                CUTOP = FLOAT(LTOP(I,J))
                CUBOT = FLOAT(LBOT(I,J))
!
                  HTOP(I,J) = MIN(CUTOP,  HTOP(I,J))
                  HBOT(I,J) = MAX(CUBOT,  HBOT(I,J))
                CNVTOP(I,J) = MIN(CUTOP,CNVTOP(I,J))
                CNVBOT(I,J) = MAX(CUBOT,CNVBOT(I,J))
            END IF
        END DO
    END DO
!
    RETURN
!
    END SUBROUTINE CUCNVC
