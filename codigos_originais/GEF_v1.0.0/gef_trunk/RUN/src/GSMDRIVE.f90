!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief GRID-SCALE MICROPHYSICAL PROCESSES - CONDENSATION AND PRECIPITATION
!> @details MERGES ORIGINAL GSCOND & PRECPD SUBROUTINES.
!! ODE HAS BEEN SUBSTANTIALLY STREAMLINED AND RESTRUCTURED.
!! EXCHANGE BETWEEN WATER VAPOR & SMALL CLOUD CONDENSATE IS CALCULATED USING THE ORIGINAL 
!! ASAI (1965, J. JAPAN) ALGORITHM.
!! SEE ALSO REFERENCES TO YAU AND AUSTIN (1979, JAS), RUTLEDGE AND HOBBS (1983, JAS), AND 
!! TAO ET AL. (1989, MWR).
!! THIS ALGORITHM REPLACES THE SUNDQVIST ET AL. (1989, MWR) PARAMETERIZATION.
!! DRIVER OF THE NEW MICROPHYSICS 
!!
!! NOTE:  CODE IS CURRENTLY SET UP W/O THREADING
!> @author ORIGINATOR - FERRIER 
!> @date 01-02-?? \n
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
!! @arg @c CLDWTR 
!! @arg @c CMICRO_START
!! @arg @c CMICRO_STATS
!! @arg @c CTLBLK 
!! @arg @c C_FRACN
!! @arg @c DYNAM
!! @arg @c F77KINDS
!! @arg @c LOOPS 
!! @arg @c MASKS 
!! @arg @c MPPSTAFF
!! @arg @c PARM_TBL 
!! @arg @c PHYS 
!! @arg @c PPTASM
!! @arg @c PVRBLS
!! @arg @c VRBLS 
!> @details <b>Driver:</b> 
!! @arg @c GEF
!> @details <b>Calls:</b>
!! @arg @c GSMCOLUMN
!! @arg @c GSMCONST
!! @arg @c MPI_REDUCE
!--------------------------------------------------------------------------------------------------
    SUBROUTINE GSMDRIVE
!--------------------------------------------------------------------------------------------------
! SUBROUTINE GSMCONST
! 
! SUBPROGRAM: GSMDRIVE - GRID-SCALE MICROPHYSICAL PROCESSES - CONDENSATION AND PRECIPITATION
! PROGRAMMER: FERRIER
! ORG: W/NP22     
! DATE: 01-02-??
!
! ABSTRACT:
! MERGES ORIGINAL GSCOND & PRECPD SUBROUTINES.
! ODE HAS BEEN SUBSTANTIALLY STREAMLINED AND RESTRUCTURED.
! EXCHANGE BETWEEN WATER VAPOR & SMALL CLOUD CONDENSATE IS CALCULATED USING THE ORIGINAL 
! ASAI (1965, J. JAPAN) ALGORITHM.
! SEE ALSO REFERENCES TO YAU AND AUSTIN (1979, JAS), RUTLEDGE AND HOBBS (1983, JAS), AND 
! TAO ET AL. (1989, MWR).
! THIS ALGORITHM REPLACES THE SUNDQVIST ET AL. (1989, MWR) PARAMETERIZATION.
! DRIVER OF THE NEW MICROPHYSICS 
!
! NOTE:  CODE IS CURRENTLY SET UP W/O THREADING
!
! PROGRAM HISTORY LOG:
! 01-04-XX    FERRIER  - BETA-TESTED VERSION
! 01-05-21    FERRIER  - ADDED GRADUAL LATENT HEATING TO REMOVE EXTERNAL WAVES
! 01-05-30    FERRIER  - CHANGED DEFAULT TO UNIFORM MARITIME CONDITIONS FOR TESTING
! 18-01-15    LUCCI    - MODERNIZATION OF THE CODE, INCLUDING:
!                        * F77 TO F90/F95
!                        * INDENTATION & UNIFORMIZATION CODE
!                        * REPLACEMENT OF COMMONS BLOCK FOR MODULES
!                        * DOCUMENTATION WITH DOXYGEN
!                        * OPENMP FUNCTIONALITY
!
! PRIOR PROGRAM HISTORY LOG:
! HERITAGE AS SUBROUTINE GSCOND:
! 94-??-??  ZHAO       - ORIGINATOR
! 95-03-25  BLACK      - CONVERSION FROM 1-D TO 2-D IN HORIZONTAL
! 95-03-28  BLACK      - ADDED EXTERNAL EDGE
! 98-11-02  BLACK      - MODIFIED FOR DISTRIBUTED MEMORY
!
! HERITAGE AS SUBROUTINE PRECPD:
! 94-??-??  ZHAO       - ORIGINATOR
! 95-03-25  BLACK      - CONVERSION FROM 1-D TO 2-D IN HORIZONTAL
! 95-11-20  ABELES     - PARALLEL OPTIMIZATION
! 96-03-29  BLACK      - REMOVED SCRCH COMMON
! 96-07-18  ZHAO       - NEW WMIN CALCULATION
! 96-09-25  BALDWIN    - NEW SR CALCULATION
! 98-11-02  BLACK      - MODIFICATION FOR DISTRIBUTED MEMORY
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
! USE MODULES:  ACMCLH
!               CLDWTR
!               CMICRO_START
!               CMICRO_STATS
!               CTLBLK
!               C_FRACN
!               DYNAM
!               F77KINDS
!               LOOPS
!               MASKS
!               MPPSTAFF
!               PARM_TBL
!               PHYS
!               PPTASM
!               PVRBLS
!               VRBLS 
!
! DRIVER      : GEF
!
! CALLS       : GSMCOLUMN
!               GSMCONST
!               MPI_REDUCE
!--------------------------------------------------------------------------------------------------
    USE ACMCLH 
    USE CLDWTR 
    USE CMICRO_START
    USE CMICRO_STATS
    USE CTLBLK 
    USE C_FRACN
    USE DYNAM
    USE F77KINDS
    USE LOOPS 
    USE MASKS 
    USE MPPSTAFF
    USE PARMETA , ONLY: IM, JM, LM
    USE PARM_TBL 
    USE PHYS 
    USE PPTASM
    USE PVRBLS
    USE VRBLS 
!
    IMPLICIT NONE
!
    INCLUDE "mpif.h"
!
    REAL   (KIND=R4)                                                                            ::&
    & CONST   , DEL_HYD , DEL_QT  , DTPH    , DUM     , FICE    , FRAIN   , HDTPH   , PDSL    ,   &
    & QI      , QR      , QW      , RIMEF_BULK        , TC      , WC      , TIME_MODEL        ,   &
    & GSMPRECTEMP     ! Chou 3/6/2019
!
    INTEGER(KIND=I4)                                                                            ::&
    & I       , II      , JJ      , KK      , I_INDEX , J       , J_INDEX , K       , L       ,   &
    & LSFC    , IRTN
!
    INTEGER(KIND=I4)                                                                            ::&
    & II1, II2, II3, II4, II5, II6, II7, II8, II9, II10, II11, II12, II13, II14, II15,            &
    & JJ1, JJ2, JJ3, JJ4, JJ5, JJ6, JJ7, JJ8, JJ9, JJ10, JJ11, JJ12, JJ13, JJ14, JJ15,            &
    & KK1, KK2, KK3, KK4, KK5, KK6, KK7, KK8, KK9, KK10, KK11, KK12, KK13, KK14, KK15,            &
    & LSFC1, LSFC2, LSFC3, LSFC4, LSFC5
!
    INTEGER(KIND=I4)    , PARAMETER :: ITHILO    = ITHI   - ITLO + 1
    INTEGER(KIND=I4)    , PARAMETER :: ITHILO_N  = ITHILO *  4
    INTEGER(KIND=I4)    , PARAMETER :: ITHILO_QM = ITHILO *  5
    INTEGER(KIND=I4)    , PARAMETER :: ITHILO_QT = ITHILO * 22
!------------------------------------------------------------ 
! KEY PARAMETERS PASSED TO COLUMN MICROPHYSICS (COLUMN_MICRO)
!------------------------------------------------------------ 
!
!----------------------------------------------------------------------
! FLAG FROM INIT.F AT START OF MODEL RUN, USED IN INITIATING STATISTICS
!----------------------------------------------------------------------
!----------------------------------------------------
! THIS VARIABLE IS FOR DEBUGGING PURPOSES (IF .TRUE.)
!----------------------------------------------------
    LOGICAL(KIND=L4)    , PARAMETER :: PRINT_DIAG = .FALSE.
!
    INTEGER(KIND=I4)    , DIMENSION(ITLO:ITHI,4)                                                ::&
    & NSTATS_0
!
    REAL   (KIND=R4)    , SAVE                                                                  ::&
    & THOUR_PRINT
!
    REAL   (KIND=R4)    , DIMENSION(2)      , SAVE                                              ::&
    & PRECMAX , PRECTOT , PRECMAX_0         , PRECTOT_0
!
    REAL   (KIND=R4)    , PARAMETER :: DTHOUR_PRINT = 1.  ! PRINT STATISTICS EVERY 3 H
!---------------------------------------
! BEGIN SECTION ON HYDROMETEOR FRACTIONS
!---------------------------------------
!
!--------------------------------------------------------------------------------------------------
! PARAMETERS USED IN CONVERTING FROM REAL TO INTEGER*2 FIELDS FOR FRACTION OF ICE (F_ICE), FRACTION 
! OF RAIN (F_RAIN), AND MASS RATIO OF RIMED ICE ("RIME FACTOR", F_RIMEF).
!--------------------------------------------------------------------------------------------------
!
!------------------------------------- 
! END SECTION ON HYDROMETEOR FRACTIONS
!------------------------------------- 
!
!---------------------------------------- 
! LOCAL ARRAYS AND PARAMETERS IN GSMDRIVE 
!---------------------------------------- 
!
!--------------------------------------------------------------------------------------------------
! COMMENTS ON 23 AUGUST 2001
! EPSQ=1.E-20 IS THE LOWER LIMIT FOR SPECIFIC HUMIDITY AND CLOUD CONDENSATE.
! THE VALUE OF EPSQ WILL NEED TO BE CHANGED IN THE OTHER SUBROUTINES IN ORDER TO MAKE IT CONSISTENT
! THROUGHOUT THE ETA CODE.
!--------------------------------------------------------------------------------------------------  
    REAL   (KIND=R4)    , PARAMETER :: EPSQ   =    1.E-20
    REAL   (KIND=R4)    , PARAMETER :: GRAV   =    9.80616
    REAL   (KIND=R4)    , PARAMETER :: RHOL   = 1000.
    REAL   (KIND=R4)    , PARAMETER :: T0C    =  273.15
    REAL   (KIND=R4)    , PARAMETER :: T_ICE  =  -10.
    REAL   (KIND=R4)    , PARAMETER :: T_ICEK = T0C + T_ICE
    REAL   (KIND=R4)    , PARAMETER :: RRHOL  = 1.  / RHOL
    REAL   (KIND=R4)    , PARAMETER :: CLIMIT =    1.E-12
!
    REAL   (KIND=R4)                                                                            ::&
    & ARAIN   , ASNOW
!
    REAL   (KIND=R4)    , DIMENSION(LM)                                                         ::&
    & P_COL   , QI_COL  , QR_COL  , QV_COL    , QW_COL  , RIMEF_COL, T_COL   , THICK_COL ,        &
    & WC_COL
!
    REAL   (KIND=R4)    , DIMENSION(ITLO:ITHI,  5)                                              ::&
    & QMAX_0 
!
    REAL   (KIND=R4)    , DIMENSION(ITLO:ITHI, 22)                                              ::&
    & QTOT_0
!---------------- 
! BEGIN EXECUTION 
!---------------- 
!
!------------------------  
! MICROPHYSICAL CONSTANTS 
!------------------------ 
    DTPH = DTQ2 ! PHYSICS TIME STEP (S)
!------------------------------------ 
! INITIALIZE CONSTANTS FOR STATISTICS 
!------------------------------------ 
    IF (MICRO_START) THEN
        MICRO_START = .FALSE. ! NO NEED TO CALCULATE THESE PARAMETERS AGAIN
!------------------------------------------------------------------------     
! BEGIN: INITIALIZE ARRAYS IF NOT INITIALIZED IN RESTART FILES (01/16/02)
!------------------------------------------------------------------------  
        DUM = 0.
        DO JJ1=0,JM+1
            DO II1=0,IM+1
                LSFC1 = LMH(II1,JJ1) ! "K" OF SURFACE
                DO KK1=1,LSFC1
                    DUM = MAX(DUM, F_RAIN(KK1,II1,JJ1), F_ICE(KK1,II1,JJ1), F_RIMEF(KK1,II1,JJ1))
                    IF (DUM > 0.) GOTO 110
                END DO
            END DO
        END DO
!
    110 IF (DUM <= 0.) THEN
            IF (MYPE == 0) WRITE(0, "(A,2I3)" ) 'INITIALIZE INTERNAL MICROPHYSICS ARRAYS'
            DO JJ2=0,JM+1
                DO II2=0,IM+1
                    LSFC2 = LMH(II2,JJ2) ! "K" OF SURFACE
                    DO KK2=1,LSFC2
                        F_RAIN(KK2,II2,JJ2) = 0.
!
                        IF (T(II2,JJ2,KK2) <= T_ICEK) THEN
                            F_ICE(KK2,II2,JJ2) = 1.
                        ELSE
                            F_ICE(KK2,II2,JJ2) = 0.
                        END IF
!
                        F_RIMEF(KK2,II2,JJ2) = 1.
                    END DO
                END DO
            END DO
        END IF
!
        CALL GSMCONST(DTPH) ! INITIALIZE LOOKUP TABLES & CONSTANTS
!
        THOUR_PRINT = -DTPH / 3600.
!
        IF (PRINT_DIAG) THEN
!-----------------------------         
! TOTAL AND MAXIMUM QUANTITIES
!-----------------------------          
            NSTATS  = 0     ! MICROPHYSICAL STATISTICS DEALING W/ GRID-POINT COUNTS
            QMAX    = 0.    ! MICROPHYSICAL STATISTICS DEALING W/ HYDROMETEOR MASS
            QTOT    = 0.    ! MICROPHYSICAL STATISTICS DEALING W/ HYDROMETEOR MASS
            PRECMAX = 0.    ! MAXIMUM PRECIP RATES (RAIN, SNOW) AT SURFACE (MM/H)
            PRECTOT = 0.    ! TOTAL PRECIPITATION (RAIN, SNOW) ACCUMULATION AT SURFACE
        END IF
    END IF
!---------------------------------  
! LOOP HORIZONTALLY THROUGH DOMAIN 
!--------------------------------- 
    DO JJ3=0,JM+1
        DO II3=0,IM+1
!
            LSFC3  = LMH(II3,JJ3)                    ! "K" OF SURFACE
            PDSL   =  PD(II3,JJ3) * RES(II3,JJ3)     ! (PSFC-PTOP)/ETA_SFC
            CONST  = PDSL / GRAV                     ! (PSFC-PTOP)/(G*ETA_SFC), USED FOR THICK BELOW
!-----------------------------------         
! INITIALIZE COLUMN DATA (1D ARRAYS)
!----------------------------------- 
            IF (CWM(II3,JJ3,1) <= CLIMIT) CWM(II3,JJ3,1) = EPSQ
              F_ICE(1,II3,JJ3) = 1.
             F_RAIN(1,II3,JJ3) = 0.
            F_RIMEF(1,II3,JJ3) = 1.
!
            DO KK3=1,LSFC3
!-----------------------------------------------             
! PRESSURE (PA) = (PSFC-PTOP)*(ETA/ETA_SFC)+PTOP
!----------------------------------------------- 
                P_COL(KK3) = PDSL * AETA(KK3) + PT
!-----------------------------------------------------------------            
! LAYER THICKNESS = RHO*DZ = -DP/G = (PSFC-PTOP)*D_ETA/(G*ETA_SFC)
!-----------------------------------------------------------------             
                THICK_COL(KK3) = CONST * DETA(KK3)
                    T_COL(KK3) = T(II3,JJ3,KK3)
!
    IF (T(II3,JJ3,KK3) < 150.00 .OR. T(II3,JJ3,KK3) > 350) WRITE(*,*) "DENTRO GSMDRIVE.f90", NTSD, II3, JJ3, KK3, T(II3,JJ3,KK3)
                          TC = T_COL(KK3) - T0C
                   QV_COL(KK3) = MAX(EPSQ, Q(II3,JJ3,KK3))
!
                IF (CWM(II3,JJ3,KK3) <= CLIMIT) THEN
                    WC_COL(KK3) = 0.
!
                    IF (TC < T_ICE) THEN
                        F_ICE(KK3,II3,JJ3) = 1.
                    ELSE
                        F_ICE(KK3,II3,JJ3) = 0.
                    END IF
!
                       F_RAIN(KK3,II3,JJ3) = 0.
                      F_RIMEF(KK3,II3,JJ3) = 1.
                ELSE
                    WC_COL(KK3) = CWM(II3,JJ3,KK3)
                END IF
!---------------------------------------------------------------------------             
! DETERMINE COMPOSITION OF CONDENSATE IN TERMS OF CLOUD WATER, ICE, AND RAIN
 !---------------------------------------------------------------------------           
                WC = WC_COL(KK3)
                QI = 0.
                QR = 0.
                QW = 0.
!
                FICE =   F_ICE(KK3,II3,JJ3)
                FRAIN = F_RAIN(KK3,II3,JJ3)
!---------------------             
! REAL*4 ARRAY STORAGE
!---------------------             
                IF (FICE >= 1.) THEN
                    QI = WC
                ELSE IF (FICE <= 0.) THEN
                    QW = WC
                ELSE
                    QI = FICE * WC
                    QW = WC   - QI
                END IF
!
                IF (QW > 0. .AND. FRAIN > 0.) THEN
                    IF (FRAIN >= 1.) THEN
                        QR = QW
                        QW = 0.
                    ELSE
                        QR = FRAIN * QW
                        QW = QW    - QR
                    END IF
                END IF
!
                RIMEF_COL(KK3) = F_RIMEF(KK3,II3,JJ3) ! (REAL)
!
                QI_COL(KK3) = QI
                QR_COL(KK3) = QR
                QW_COL(KK3) = QW
            END DO
!------------------------------------------------------ 
! PERFORM THE MICROPHYSICAL CALCULATIONS IN THIS COLUMN
!------------------------------------------------------        
            I_INDEX = II3
            J_INDEX = JJ3
!
            CALL GSMCOLUMN(ARAIN , ASNOW , DTPH  , I_INDEX  , J_INDEX, LSFC3    ,                 &
    &                          P_COL(:),                                                          &
    &                         QI_COL(:),                                                          &
    &                         QR_COL(:),                                                          &
    &                         QV_COL(:),                                                          &
    &                         QW_COL(:),                                                          &
    &                      RIMEF_COL(:),                                                          &
    &                          T_COL(:),                                                          &
    &                      THICK_COL(:),                                                          &
    &                         WC_COL(:))
!----------------------  
! UPDATE STORAGE ARRAYS
!----------------------        
            DO KK4=1,LSFC3
                 TRAIN(II3,JJ3,KK4) = (T_COL(KK4) - T(II3,JJ3,KK4)) / DTPH
                     T(II3,JJ3,KK4) =  T_COL(KK4)
!                IF (T_COL(KK4) < 150.00 .OR. T_COL(KK4) > 320) WRITE(*,*) "DENTRO GSMDRIVE.f90 TCOL", NTSD, II3, JJ3, KK4, T(II3,JJ3,KK4)
                     Q(II3,JJ3,KK4) = QV_COL(KK4)
                TLATGS(II3,JJ3,KK4) =  T_COL(KK4) - T(II3,JJ3,KK4)
                   CWM(II3,JJ3,KK4) = WC_COL(KK4)
!---------------------             
! REAL*4 ARRAY STORAGE
!---------------------            
                F_RIMEF(KK4,II3,JJ3) = MAX(1., RIMEF_COL(KK4))
!
                IF (QI_COL(KK4) <= CLIMIT) THEN
                    F_ICE(KK4,II3,JJ3) = 0.
                    IF (T_COL(KK4) < T_ICEK) F_ICE(KK4,II3,JJ3) = 1.
                ELSE
                    F_ICE(KK4,II3,JJ3) = MAX(0., MIN(1., QI_COL(KK4) / WC_COL(KK4)))
                END IF
!
                IF (QR_COL(KK4) <= CLIMIT) THEN
                    DUM = 0.
                ELSE
                    DUM = QR_COL(KK4) / (QR_COL(KK4) + QW_COL(KK4))
                END IF
!
                   F_RAIN(KK4,II3,JJ3) = DUM
!
            END DO
!--------------------------------------------------------------------------------------------------        
! UPDATE ACCUMULATED PRECIPITATION STATISTICS
!       
! SURFACE PRECIPITATION STATISTICS; 
! SR IS FRACTION OF SURFACE PRECIPITATION (IF >0) ASSOCIATED WITH SNOW
!--------------------------------------------------------------------------------------------------       

            GSMPRECTEMP = (ARAIN + ASNOW) * RRHOL 

            APREC(II3,JJ3)   = APREC(II3,JJ3) + (ARAIN + ASNOW) * RRHOL   !    added aprec(i,j), Chou 3/6/2019 - 
                                                                          !    becomes ACC. mycrophysics prec.

!!!             APREC(II3,JJ3) = (ARAIN + ASNOW) * RRHOL  ! ACCUMULATED SURFACE PRECIP (DEPTH IN M) - YING
!
              PREC(II3,JJ3) =   PREC(II3,JJ3) + APREC(II3,JJ3)
!!!            ACPREC(II3,JJ3) = ACPREC(II3,JJ3) + APREC(II3,JJ3)
            ACPREC(II3,JJ3) = ACPREC(II3,JJ3) + GSMPRECTEMP 
!
!!!commented Chou 3/6/2019
!!!            IF (APREC(II3,JJ3) < 1.E-8) THEN
!!!                SR(II3,JJ3) = 0.
!!!            ELSE
!!!                SR(II3,JJ3) = RRHOL * ASNOW / APREC(II3,JJ3)
!!!            END IF

            IF (GSMPRECTEMP < 1.E-8) THEN
                SR(II3,JJ3) = 0.
            ELSE
                SR(II3,JJ3) = RRHOL * ASNOW / GSMPRECTEMP
            END IF


!-----------------         
! DEBUG STATISTICS
!-----------------         
            IF (PRINT_DIAG) THEN
                PRECTOT(1) = PRECTOT(1) + ARAIN
                PRECTOT(2) = PRECTOT(2) + ASNOW
!
                PRECMAX(1) = MAX(PRECMAX(1), ARAIN)
                PRECMAX(2) = MAX(PRECMAX(2), ASNOW)
            END IF
!
        END DO       ! END "I" & "J" LOOPS
    END DO
!------------------------------ 
! END OF MAIN MICROPHYSICS LOOP 
!------------------------------ 
!GSM    TIME_MODEL = FLOAT(NTSD-1) * DT / 3600.
    TIME_MODEL = FLOAT(NTSD) * DT / 3600.
!
    IF (PRINT_DIAG .AND. TIME_MODEL >= THOUR_PRINT) THEN
        CALL MPI_REDUCE(NSTATS , NSTATS_0 , ITHILO_N , MPI_INTEGER, MPI_SUM, 0,MPI_COMM_WORLD,IRTN)
        CALL MPI_REDUCE(QMAX   , QMAX_0   , ITHILO_QM, MPI_REAL   , MPI_MAX, 0,MPI_COMM_WORLD,IRTN)
        CALL MPI_REDUCE(PRECMAX, PRECMAX_0, 2        , MPI_REAL   , MPI_MAX, 0,MPI_COMM_WORLD,IRTN)
        CALL MPI_REDUCE(QTOT   , QTOT_0   , ITHILO_QT, MPI_REAL   , MPI_SUM, 0,MPI_COMM_WORLD,IRTN)
        CALL MPI_REDUCE(PRECTOT, PRECTOT_0, 2        , MPI_REAL   , MPI_SUM, 0,MPI_COMM_WORLD,IRTN)
!
        IF (MYPE == 0) THEN
            HDTPH = 3600. / DTPH ! CONVERT PRECIP RATES TO MM/H
!
            DO KK5=ITLO,ITHI
                QMAX_0(KK5,1) = 1000. * QMAX_0(KK5,1)
                QMAX_0(KK5,2) = 1000. * QMAX_0(KK5,2)
                QMAX_0(KK5,3) = 1000. * QMAX_0(KK5,3)
                QMAX_0(KK5,4) = HDTPH * QMAX_0(KK5,4)
                QMAX_0(KK5,5) = HDTPH * QMAX_0(KK5,5)
            END DO
!
            PRECMAX_0(1) = HDTPH * PRECMAX_0(1)
            PRECMAX_0(2) = HDTPH * PRECMAX_0(2)
!
            WRITE(6,"(A,F5.2,4(A,G11.4))") '{ TIME(H)=',TIME_MODEL,'  TRAIN_SFC=',PRECTOT_0(1),   &
    &       '  TSNOW_SFC=',PRECTOT_0(2),'  RRMAX_SFC(MM/H)=',PRECMAX_0(1),                        &
    &       '  SRMAX_SFC(MM/H)=',PRECMAX_0(2)
!  
            WRITE(6,"(3A)") '{ (C) <--------- COUNTS ----------> ',                               &
    &       '<----------- G/KG ----------> <----- MM/H ------>'   ,                               &
            ' <---- KG/M**2 * # GRIDS ---->'
!
            WRITE(6,"(3A)") '{  T     NCICE  NCMIX  NCWAT NCRAIN  '       ,                       &
    &       'QIMAX     QWMAX     QRMAX     SRMAX     RRMAX     QITOT     ',                       &
    &       'QWTOT     QRTOT'
!
            DO KK6=ITLO,ITHI
                WRITE(6,"(A,I3,I9,3I7,8G11.4)") '{ ', K, (NSTATS_0(KK6,II7), II7=1,4),            &
    &                                                      (QMAX_0(KK6,JJ7), JJ7=1,5),            &
    &                                                      (QTOT_0(KK6,KK7), KK7=1,3)
            END DO
 !       
            WRITE(6,"(3A)") '{  T   TCOND     TICND     TIEVP     TIDEP     TREVP     ',          &
    &       'TRAUT     TRACW     TIMLT     TIACW     TIACWI    TIACWR    ','TIACR'
!
            DO KK8=ITLO,ITHI
                WRITE(6,"(A,I3,12G11.4)") '{ ',KK8,(QTOT_0(KK8,II8), II8=4,15)
            END DO
!        
            WRITE(6,"(2A)") '{  T   DEL_QT   TVDIF   DEL_HYD        TWDIF  TIDIF       ',         &
    &       'TRDIF    DARAIN   DASNOW    RIMEF'
!
            DO KK9=ITLO,ITHI
                DEL_HYD = 0.
!
                DO II9=17,19
                    DEL_HYD = DEL_HYD + QTOT_0(KK9,II9)
                END DO
!
                DEL_QT = 0.
!
                DO II10=16,21
                    DEL_QT = DEL_QT + QTOT_0(KK9,II10)
                END DO
!
                IF (QTOT_0(KK9,22) > 0.) THEN
                    RIMEF_BULK = QTOT_0(KK9,1) / QTOT_0(KK9,22)
                ELSE
                    RIMEF_BULK = 1.
                END IF
!
                WRITE(6,"(A,I3,9G11.4)") '{ ', K, DEL_QT, QTOT_0(KK9,16),            DEL_HYD   ,  &
    &                                                    (QTOT_0(KK9,II11), II11=17,21), RIMEF_BULK
            END DO
!       
        END IF
!
        NSTATS  = 0     ! MICROPHYSICAL STATISTICS DEALING W/ GRID-POINT COUNTS
        QMAX    = 0.    ! MICROPHYSICAL STATISTICS DEALING W/ HYDROMETEOR MASS
        QTOT    = 0.    ! MICROPHYSICAL STATISTICS DEALING W/ HYDROMETEOR MASS
        PRECMAX = 0.    ! MAXIMUM PRECIP RATES (RAIN, SNOW) AT SURFACE (MM/H)
        PRECTOT = 0.    ! TOTAL PRECIPITATION (RAIN, SNOW) ACCUMULATION AT SURFACE
!
        THOUR_PRINT = THOUR_PRINT + DTHOUR_PRINT
    END IF
!----------------------- 
! RETURN TO MAIN PROGRAM 
!----------------------- 
    RETURN
!
200 FORMAT(A2,I5,F6.2,4(1X,A10,G11.4))
210 FORMAT(A2,I5,F6.2,4(1X,A10,I7))
!
    END SUBROUTINE GSMDRIVE
