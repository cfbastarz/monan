      PROGRAM CONVERTE_R4_TO_R8
!
      USE ACMCLD
      USE ACMCLH
      USE ACMPRE
      USE ACMRDL
      USE ACMRDS
      USE ACMSFC
      USE CTLBLK
      USE DYNAM
      USE DGNSOUT
      USE HS
      USE LOOPS
      USE MASKS
      USE METRCS
      USE MPPSTAFF
      USE PARMSOIL
      USE PHYS
      USE PVRBLS
      USE SOIL
      USE VRBLS
!      
       IMPLICIT NONE
!
    SAVE
!
!
    REAL   (KIND=R8)                                                                                     ::&
    & DT1      , TSRFC1   , TRDLW1   , TPREC1   , THEAT1      , TCLOD1  , TRDSW1
!
    REAL   (KIND=R8)    , DIMENSION(:)                , ALLOCATABLE                                      ::&
    & DETA1    , RDETA1   , AETA1    , DAETA1   , F4Q2_1     , ETA1     , DFL1     ,KVDT1
!
!
    REAL   (KIND=R8)    , DIMENSION (:,:)             , ALLOCATABLE                                      ::&
    & PD1     , TG1     , SQV1     , SQH1     , Q11_1     , Q12_1     , Q22_1     , QBV11_1    ,           &          
    & QBV12_1 , QBV22_1 , QH11_1   , QH12_1   , QH22_1    , SQV_NORM1 , WPDAR1    , F11_1      ,           &
    & F12_1   , F21_1   , F22_1    , P11_1    , P12_1     , P21_1     , P22_1     , FDIV1      ,           &
    & FDDMP1  , HSINP1  , HCOSP1   , FVDIFF1  , HBMSK1    , GLAT1     , GLON1     , FIS1       ,           &
    & RES1    , LMH1    , LMV1     , SST_1    , EPSR1     , SM1       , SICE1     , SI1        ,           & 
    & SNO1    , VEGFRC1 , IVGTYP1  , ISLTYP1  , ISLOPE1   , ALBEDO1   , MXSNAL1   , ALBASE1    
!
!
    REAL   (KIND=R8)    , DIMENSION (:,:,:)           , ALLOCATABLE                                      ::&
    & T1       , Q1       , U1       , V1      , QD11_1    , QD12_1    , QD21_1    , QD22_1    ,           &
    & KTDT1    , HTM1     , VTM1     , SH2O1   , SMC1      , STC1      , ZEFFIJ1        
!
!
    REAL   (KIND=R8)    , DIMENSION(:,:,:,:)          , ALLOCATABLE                                      ::&
    & QB1
!
!
    REAL   (KIND=R8)    , DIMENSION(:,:,:,:,:)        , ALLOCATABLE                                      ::&
    & QVH1     , QHV1     , QVH2_1   , QHV2_1 , QA1        
!
!
    REAL   (KIND=R8)                                                                                     ::&
    & FADV1    , FADV2_1   , FADT1    , R1       , PT1      , F4D1     , EF4T1    , FKIN1      ,           &
    & FCP1     , DELTY1    , DELTHZ1  , KAPPA1   , P0_1     , RTDF1    , CLOGES
!
!
    INTEGER(KIND=I4)                                                                                     ::&
    & I        , I3       , J3       , N         , IX      , IY        , JMIN    , JMAX        ,           & 
    & IMIN     , IMAX     , I5       , J5        , J
!
!
    CHARACTER(LEN=80)                                                                                    ::&
    & CVRBLS   , CVRBLS1  , CCONST   , CCONST1  , CHS      , CHS1     , CSFCETA     , CSFCETA1 ,           &  
    & CDGNS    , CDGNS1
!
!
    INTEGER(KIND=I8)                                                                                     ::&
    & DGNNUM1   , DGNSTR1 , IDGNS1   , ICKMM1   , CKNUM1
!
!
    INTEGER(KIND=I4), PARAMETER                                                                          ::&
    & ILM = (IM - 1)/IXM +1     , JLM = (JM - 1)/JYM +1
!
    REAL   (KIND=R4)    , PARAMETER :: CM1    = 2937.4
    REAL   (KIND=R4)    , PARAMETER :: CM2    = 4.9283
    REAL   (KIND=R4)    , PARAMETER :: CM3    = 23.5518
!
!    
!
!  CONST
    ALLOCATE (DETA1    (LM))
    ALLOCATE (RDETA1   (LM))
    ALLOCATE (AETA1    (LM))
    ALLOCATE (DAETA1   (LM))
    ALLOCATE (F4Q2_1   (LM))
    ALLOCATE (ETA1     (LM+1))
    ALLOCATE (DFL1     (LM+1))
!
! HS
    ALLOCATE (KVDT1    (LM))
!
! VRBLS
    ALLOCATE (TG1      (0:IM+1, 0:JM+1))
    ALLOCATE (PD1      (0:IM+1, 0:JM+1))
!
! CONST
    ALLOCATE (SQV1     (0:IM+1, 0:JM+1))
    ALLOCATE (SQH1     (0:IM+1, 0:JM+1))
    ALLOCATE (Q11_1    (0:IM+1, 0:JM+1))
    ALLOCATE (Q12_1    (0:IM+1, 0:JM+1))
    ALLOCATE (Q22_1    (0:IM+1, 0:JM+1))
    ALLOCATE (QBV11_1  (0:IM+1, 0:JM+1))
    ALLOCATE (QBV12_1  (0:IM+1, 0:JM+1))
    ALLOCATE (QBV22_1  (0:IM+1, 0:JM+1))
    ALLOCATE (QH11_1   (0:IM+1, 0:JM+1))
    ALLOCATE (QH12_1   (0:IM+1, 0:JM+1))
    ALLOCATE (QH22_1   (0:IM+1, 0:JM+1))
    ALLOCATE (SQV_NORM1(0:IM+1, 0:JM+1))
    ALLOCATE (WPDAR1   (0:IM+1, 0:JM+1))
    ALLOCATE (F11_1    (0:IM+1, 0:JM+1))
    ALLOCATE (F12_1    (0:IM+1, 0:JM+1))
    ALLOCATE (F21_1    (0:IM+1, 0:JM+1))
    ALLOCATE (F22_1    (0:IM+1, 0:JM+1))
    ALLOCATE (P11_1    (0:IM+1, 0:JM+1))
    ALLOCATE (P12_1    (0:IM+1, 0:JM+1))
    ALLOCATE (P21_1    (0:IM+1, 0:JM+1))
    ALLOCATE (P22_1    (0:IM+1, 0:JM+1))
    ALLOCATE (FDIV1    (0:IM+1, 0:JM+1))
    ALLOCATE (FDDMP1   (0:IM+1, 0:JM+1))
    ALLOCATE (HSINP1   (0:IM+1, 0:JM+1))
    ALLOCATE (HCOSP1   (0:IM+1, 0:JM+1))
    ALLOCATE (FVDIFF1  (0:IM+1, 0:JM+1))
    ALLOCATE (HBMSK1   (0:IM+1, 0:JM+1))
    ALLOCATE (GLAT1    (0:IM+1, 0:JM+1))
    ALLOCATE (GLON1    (0:IM+1, 0:JM+1))
!
! SFCETA3
    ALLOCATE (FIS1    (0:IM+1, 0:JM+1))
    ALLOCATE (RES1    (0:IM+1, 0:JM+1))    
    ALLOCATE (LMH1    (0:IM+1, 0:JM+1))
    ALLOCATE (LMV1    (0:IM+1, 0:JM+1))
    ALLOCATE (SST_1   (0:IM+1, 0:JM+1))
    ALLOCATE (EPSR1   (0:IM+1, 0:JM+1))
    ALLOCATE (SM1     (0:IM+1, 0:JM+1))
    ALLOCATE (SICE1   (0:IM+1, 0:JM+1))
    ALLOCATE (SI1     (0:IM+1, 0:JM+1))
    ALLOCATE (SNO1    (0:IM+1, 0:JM+1))
    ALLOCATE (VEGFRC1 (0:IM+1, 0:JM+1))
    ALLOCATE (IVGTYP1 (0:IM+1, 0:JM+1))
    ALLOCATE (ISLTYP1 (0:IM+1, 0:JM+1))
    ALLOCATE (ISLOPE1 (0:IM+1, 0:JM+1))
    ALLOCATE (ALBEDO1 (0:IM+1, 0:JM+1))
    ALLOCATE (MXSNAL1 (0:IM+1, 0:JM+1))
    ALLOCATE (ALBASE1 (0:IM+1, 0:JM+1))
!
! VRBLS
    ALLOCATE (T1       (0:IM+1, 0:JM+1, LM))
    ALLOCATE (Q1       (0:IM+1, 0:JM+1, LM))
    ALLOCATE (U1       (0:IM+1, 0:JM+1, LM))
    ALLOCATE (V1       (0:IM+1, 0:JM+1, LM))
!
! HS
    ALLOCATE (KTDT1    (0:IM+1, 0:JM+1, LM))
!
! CONST
    ALLOCATE (QD11_1   (0:IM+1, 0:JM+1,  4))
    ALLOCATE (QD12_1   (0:IM+1, 0:JM+1,  4))
    ALLOCATE (QD21_1   (0:IM+1, 0:JM+1,  4))
    ALLOCATE (QD22_1   (0:IM+1, 0:JM+1,  4))
!
! SFCETA3
    ALLOCATE (HTM1    (0:IM+1, 0:JM+1, LM))
    ALLOCATE (VTM1    (0:IM+1, 0:JM+1, LM))
    ALLOCATE (SH2O1   (0:IM+1, 0:JM+1, NSOIL))
    ALLOCATE (SMC1    (0:IM+1, 0:JM+1, NSOIL))
    ALLOCATE (STC1    (0:IM+1, 0:JM+1, NSOIL))
    ALLOCATE (ZEFFIJ1 (0:IM+1, 0:JM+1,  4))
!
! CONST
    ALLOCATE (QB1      (0:IM+1, 0:JM+1,  2, 2))
!
! CONST
    ALLOCATE (QVH1     (0:IM+1, 0:JM+1,  4, 2, 2))
    ALLOCATE (QHV1     (0:IM+1, 0:JM+1,  4, 2, 2))
    ALLOCATE (QVH2     (0:IM+1, 0:JM+1,  4, 2, 2))
    ALLOCATE (QVH2_1   (0:IM+1, 0:JM+1,  4, 2, 2))
    ALLOCATE (QHV2_1   (0:IM+1, 0:JM+1,  4, 2, 2))
    ALLOCATE (QA1      (0:IM+1, 0:JM+1, 13, 2, 2))

   CALL INITMPI
   CALL ALLOC

!  
!------
! 
!!   DO MYPE = 0 , 2399
 
!
        WRITE(C_MYPE, 1000) MYPE
 1000    FORMAT(I4.4)


        print*,C_MYPE

!!!!!!!!!!!!!!!!!!!!!!! READ/WRITE   SFCETA3 !!!!!!!!!!!!!!!!!!!!!!!!!!
!  
    CSFCETA1 = 'sfceta3_new.' // C_MYPE
!
    OPEN (21, FILE=CSFCETA1, STATUS='OLD', FORM='UNFORMATTED')  

!
    READ(21) FIS1	 
    READ(21) RES1	
    READ(21) HTM1     !    SFCETA(OUT4MPI.F90)
    READ(21) VTM1     
    READ(21) LMH1     
    READ(21) LMV1     
    READ(21) SST_1 
    READ(21) EPSR1    
    READ(21) SM1      
    READ(21) SICE1 
    READ(21) SI1   
    READ(21) SNO1  
    READ(21) VEGFRC1  !   SFCETA2(OUT4MPI.F90)
    READ(21) IVGTYP1  
    READ(21) ISLTYP1  
    READ(21) ISLOPE1  
    READ(21) SH2O1    
    READ(21) ALBEDO1  
    READ(21) ALBASE1  
    READ(21) MXSNAL1  
    READ(21) SMC1     
    READ(21) STC1     !  INIT_OUT(OUT4MPI.F90)   
    READ(21) ZEFFIJ1  !      ZEFF(OUT4MPI.F90)


    CLOSE (21)
!

!!!!!!!!!!!!!!!!!!!!!!! READ/WRITE   SFCETA3 !!!!!!!!!!!!!!!!!!!!!!!!!!

    CALL BOCOHMPI(SST_1,1)


!
    DO J=1,JM
        DO I=1,IM
!      
            IF (SM1(I,J) > 0.5) THEN
                CLOGES  = -CM1 / SST_1(I,J) - CM2 * ALOG10(SST_1(I,J)) + CM3
                    IF (MYPE == 0) THEN
                       PRINT *,'CLOGES:', CLOGES
                    END IF

            END IF
!
        END DO
    END DO


!!   END DO

END 















     
      
      
      
      
      
      
      
      
   
