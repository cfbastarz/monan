      PROGRAM CONVERTE_R4_TO_R8
!
    USE CO2DTA
    USE F77KINDS
    USE HCON
    USE MPPSTAFF
    USE PARMETA
!
!      
    IMPLICIT NONE
!
    SAVE
!
    INTEGER(KIND=I4)                                                                            ::&
    & I       , II1     , II2     , J       , K       , KK      , N       , RSIZE
!
    INTEGER(KIND=I4)    , PARAMETER :: LP12 = LP1 * LP1
!
    INTEGER(KIND=I4)    , DIMENSION(3)                                                          ::&
    & RSZE
!
    INTEGER(KIND=I4)                                                                            ::&
    & NFILE
!
!
!
    REAL   (KIND=R4)    , DIMENSION(LP1, 2)                                                     ::&
    & SGTMP
!
    REAL   (KIND=R4)    , DIMENSION(LM, 6)                                                      ::&
    & CO21D
!
    REAL   (KIND=R4)    , DIMENSION(LP1, LP1, 6)                                                ::&
    & CO22D
!
    REAL   (KIND=R4)    , DIMENSION(LP1, 6)                                                     ::&
    & CO21D3  , CO21D7
!
    REAL   (KIND=R4)    , DIMENSION(LP12)                                                       ::&
    & DATA2
!
!
!
!
    REAL   (KIND=R8)    , DIMENSION(LP1, 2)                                                     ::&
    & SGTMP1
!
    REAL   (KIND=R8)    , DIMENSION(LM, 6)                                                      ::&
    & CO21D1
!
    REAL   (KIND=R8)    , DIMENSION(LP1, LP1, 6)                                                ::&
    & CO22D1
!
    REAL   (KIND=R8)    , DIMENSION(LP1, 6)                                                     ::&
    & CO21D3_1  , CO21D7_1
!
    REAL   (KIND=R8)    , DIMENSION(LP12)                                                       ::&
    & DATA2_1
!
!
    REAL   (KIND=R8)    , DIMENSION(LP1, LP1)                                                   ::&
    & CO251_1   , CO258_1   , CDT51_1   , CDT58_1   , C2D51_1   , C2D58_1
!
    REAL   (KIND=R8)    , DIMENSION(LM)                                                         ::&
    & CO2M51_1  , CO2M58_1  , CDTM51_1  , CDTM58_1  , C2DM51_1  , C2DM58_1
!
    REAL   (KIND=R8)    , DIMENSION(LP1)                                                        ::&
    & STEMP1   , GTEMP1
!
!
!
!
    CHARACTER(LEN=180)                                                                          ::&
    & CFILE

!  
!------
! 
!!!!!!!!!!!!!!!!!!!!!!! READ/WRITE   co2.dat !!!!!!!!!!!!!!!!!!!!!!!!!!

    NFILE = 100

!----------------
! CFILE='CO2.DAT'
!----------------
    CFILE = 'co2.dat'
!
    OPEN(UNIT=NFILE, FORM='UNFORMATTED', STATUS='OLD', FILE=CFILE)
!
    REWIND NFILE
!----------------------------------------------------------------------
! READ IN PRE-COMPUTED CO2 TRANSMISSION DATA AND CONVERT TO CYBER WORDS
!----------------------------------------------------------------------
    RSZE(1) = LP1
    RSZE(2) = LM
    RSZE(3) = LP1 * LP1
!
    RSIZE = RSZE(1)
!
    DO KK=1,2
        READ(NFILE) (SGTMP(I,KK), I=1,RSIZE)
    END DO
!
      RSIZE = RSZE(2)
!
    DO KK=1,6
        READ(NFILE) (CO21D(I,KK), I=1,RSIZE)
    END DO
!
      RSIZE = RSZE(3)
!
    DO KK=1,6
        READ(NFILE) (DATA2(I),I=1, RSIZE)
!
        N = 0
!
        DO II1=1,LP1
            DO II2=1,LP1
                N = N + 1
                CO22D(II1,II2,KK) = DATA2(N)
            END DO
        END DO
!
    END DO
!
      RSIZE = RSZE(1)
!
    DO KK=1,6
        READ(NFILE) (CO21D3(I,KK), I=1,RSIZE)
    END DO
!
    DO KK=1,6
      READ(NFILE) (CO21D7(I,KK), I=1,RSIZE)
    END DO
!
    REWIND NFILE
!
    DO K=1,LP1
        STEMP(K) = SGTMP(K,1)
        GTEMP(K) = SGTMP(K,2)
    END DO
!
    DO K=1,LM
        CDTM51(K) = CO21D(K,1)
        CO2M51(K) = CO21D(K,2)
        C2DM51(K) = CO21D(K,3)
        CDTM58(K) = CO21D(K,4)
        CO2M58(K) = CO21D(K,5)
        C2DM58(K) = CO21D(K,6)
    END DO
!
    DO J=1,LP1
        DO I=1,LP1
            CDT51(I,J) = CO22D(I,J,1)
            CO251(I,J) = CO22D(I,J,2)
            C2D51(I,J) = CO22D(I,J,3)
            CDT58(I,J) = CO22D(I,J,4)
            CO258(I,J) = CO22D(I,J,5)
            C2D58(I,J) = CO22D(I,J,6)
        END DO 
    END DO
!
    DO K=1,LP1
        CDT31(K) = CO21D3(K,1)
        CO231(K) = CO21D3(K,2)
        C2D31(K) = CO21D3(K,3)
        CDT38(K) = CO21D3(K,4)
        CO238(K) = CO21D3(K,5)
        C2D38(K) = CO21D3(K,6)
    END DO 
!
    DO K=1,LP1
        CDT71(K) = CO21D7(K,1)
        CO271(K) = CO21D7(K,2)
        C2D71(K) = CO21D7(K,3)
        CDT78(K) = CO21D7(K,4)
        CO278(K) = CO21D7(K,5)
        C2D78(K) = CO21D7(K,6)
    END DO
!
!    IF (MYPE == 0) PRINT 66, NFILE
! 66 FORMAT (1H ,'----READ CO2 TRANSMISSION FUNCTIONS FROM UNIT ', I3)
! 
    CLOSE (NFILE)



!!!!!!!!!!!!!!!!!!!!!!! READ/WRITE   co2.dat !!!!!!!!!!!!!!!!!!!!!!!!!!



    NFILE = 100

!----------------
! CFILE='CO2.DAT'
!----------------
    CFILE = 'co2_new.dat'
!
    OPEN(UNIT=NFILE, FORM='UNFORMATTED', STATUS='UNKNOWN', FILE=CFILE)
!
    REWIND NFILE
!----------------------------------------------------------------------
! READ IN PRE-COMPUTED CO2 TRANSMISSION DATA AND CONVERT TO CYBER WORDS
!----------------------------------------------------------------------
    RSZE(1) = LP1
    RSZE(2) = LM
    RSZE(3) = LP1 * LP1
!
    RSIZE = RSZE(1)
!
    DO KK=1,2
        WRITE(NFILE) (SGTMP1(I,KK), I=1,RSIZE)
    END DO
!
      RSIZE = RSZE(2)
!
    DO KK=1,6
        WRITE(NFILE) (CO21D1(I,KK), I=1,RSIZE)
    END DO
!
      RSIZE = RSZE(3)
!
    DO KK=1,6
        WRITE(NFILE) (DATA2_1(I),I=1, RSIZE)
!
        N = 0
!
        DO II1=1,LP1
            DO II2=1,LP1
                N = N + 1
                CO22D1(II1,II2,KK) = DATA2_1(N)
            END DO
        END DO
!
    END DO
!
      RSIZE = RSZE(1)
!
    DO KK=1,6
        WRITE(NFILE) (CO21D3_1(I,KK), I=1,RSIZE)
    END DO
!
    DO KK=1,6
      WRITE(NFILE) (CO21D7_1(I,KK), I=1,RSIZE)
    END DO
!
    REWIND NFILE
!
    DO K=1,LP1
        STEMP1(K) = SGTMP1(K,1)
        GTEMP1(K) = SGTMP1(K,2)
    END DO
!
    DO K=1,LM
        CDTM51_1(K) = CO21D1(K,1)
        CO2M51_1(K) = CO21D1(K,2)
        C2DM51_1(K) = CO21D1(K,3)
        CDTM58_1(K) = CO21D1(K,4)
        CO2M58_1(K) = CO21D1(K,5)
        C2DM58_1(K) = CO21D1(K,6)
    END DO
!
    DO J=1,LP1
        DO I=1,LP1
            CDT51_1(I,J) = CO22D_1(I,J,1)
            CO251_1(I,J) = CO22D_1(I,J,2)
            C2D51_1(I,J) = CO22D_1(I,J,3)
            CDT58_1(I,J) = CO22D_1(I,J,4)
            CO258_1(I,J) = CO22D_1(I,J,5)
            C2D58_1(I,J) = CO22D_1(I,J,6)
        END DO 
    END DO
!
    DO K=1,LP1
        CDT31_1(K) = CO21D3_1(K,1)
        CO231_1(K) = CO21D3_1(K,2)
        C2D31_1(K) = CO21D3_1(K,3)
        CDT38_1(K) = CO21D3_1(K,4)
        CO238_1(K) = CO21D3_1(K,5)
        C2D38_1(K) = CO21D3_1(K,6)
    END DO 
!
    DO K=1,LP1
        CDT71_1(K) = CO21D7_1(K,1)
        CO271_1(K) = CO21D7_1(K,2)
        C2D71_1(K) = CO21D7_1(K,3)
        CDT78_1(K) = CO21D7_1(K,4)
        CO278_1(K) = CO21D7_1(K,5)
        C2D78_1(K) = CO21D7_1(K,6)
    END DO
!
!    IF (MYPE == 0) PRINT 66, NFILE
! 66 FORMAT (1H ,'----READ CO2 TRANSMISSION FUNCTIONS FROM UNIT ', I3)
! 
    CLOSE (NFILE)




    

END     
      
   
