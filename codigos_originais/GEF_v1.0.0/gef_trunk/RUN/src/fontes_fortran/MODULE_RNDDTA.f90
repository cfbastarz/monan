!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module RNDDTA...
!! @details Details of Module RNDDTA...
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS
!! @arg @c RDPARM
!! @details <b>Driver:</b> 
!! @arg @c FST88
!! @arg @c INIT
!! @arg @c INIT_RNDDTA
!! @arg @c LWR88
!! @arg @c RADFS
!! @arg @c SPA88
!! @arg @c TABLE
!<
!--------------------------------------------------------------------------------------------------
    MODULE RNDDTA
!--------------------------------------------------------------------------------------------------
! MODULE RNDDTA
!
! USE MODULES: F77KINDS
!              RDPARM
!       
! DRIVER     : FST88
!              INIT
!              INIT_RNDDTA
!              LWR88
!              RADFS
!              SPA88
!              TABLE
!--------------------------------------------------------------------------------------------------
      USE F77KINDS
      USE RDPARM
!
      IMPLICIT NONE
!
      SAVE
!--------------------------------------------------------------------------------------------------
! COMMON BLOCK BANDTA CONTAINS RANDOM BAND PARAMETERS FOR THE LW CALCULATIONS USING 10 CM-1 WIDE 
! BANDS. THE 15 UM CO2 COMPLEX IS 2 BANDS,560-670 AND 670-800 CM-1. OZONE COEFFICIENTS ARE IN 3 
! BANDS, 670-800 (14.1 UM),990-1070 AND 1070-1200 (9.6 UM).
! THE  (NBLW) BANDS NOW INCLUDE:
!                56 BANDS, 10  CM-1 WIDE    0  -   560  CM-1
!                 2 BANDS, 15 UM COMPLEX  560  -   670  CM-1
!                                         670  -   800  CM-1
!                 3 "CONTINUUM" BANDS     800  -   900  CM-1
!                                         900  -   990  CM-1
!                                        1070  -   1200 CM-1
!                 1 BAND FOR 9.6 UM BAND  990  -   1070 CM-1
!               100 BANDS, 10 CM-1 WIDE  1200  -   2200 CM-1
!                 1 BAND FOR 4.3 UM SRC  2270  -   2380 CM-1
! THUS NBLW PRESENTLY EQUALS    163
! ALL BANDS ARE ARRANGED IN ORDER OF INCREASING WAVENUMBER
!        ARNDM   =   RANDOM "A" PARAMETER FOR (NBLW) BANDS
!        BRNDM   =   RANDOM "B" PARAMETER FOR (NBLW) BANDS
!        BETAD   =   CONTINUUM COEFFICIENTS FOR (NBLW) BANDS
!        AP,BP   =   CAPPHI COEFFICIENTS FOR (NBLW) BANDS
!        ATP,BTP =   CAPPSI COEFFICIENTS FOR (NBLW) BANDS
!        BANDLO  =   LOWEST  FREQUENCY IN EACH OF (NBLW) FREQ. BANDS
!        BANDHI  =   HIGHEST FREQUENCY IN EACH OF (NBLW) FREQ. BANDS
!        AO3RND  =   RANDOM "A" PARAMETER FOR OZONE IN (3) OZONE BANDS
!        BO3RND  =   RANDOM "B" PARAMETER FOR OZONE IN (3) OZONE BANDS
!        AB15    =   THE PRODUCT ARNDM*BRNDM FOR THE TWO BANDS REPRESENTING THE 15 UM BAND COMPLEX
!                    OF CO2
!
! DATA FOR ARNDM, BRNDM, AP, BP, ATP, BTP, AO3RND, BO3RND ARE OBTAINED BY USING THE AFGL 1982 
! CATALOG. CONTINUUM COEFFICIENTS ARE FROM ROBERTS (1976).
!--------------------------------------------------------------------------------------------------
    REAL   (KIND=R4)    , DIMENSION(NBLW)                                                       ::&
    & ARNDM   , BRNDM   , BETAD   , AP      , BP      , ATP     , BTP     , BANDLO  , BANDHI
!
    REAL   (KIND=R4)    , DIMENSION(3)                                                          ::&
    & AO3RND  , BO3RND   
!
    REAL   (KIND=R4)    , DIMENSION(2)                                                          ::&
    & AB15
!--------------------------------------------------------------------------------------------------
! COMMON BLOCK BDWIDE CONTAINS RANDOM BAND PARAMETERS FOR SPECIFIC WIDE BANDS. AT PRESENT,THE 
! INFORMATION CONSISTS OF:
! 1) RANDOM MODEL PARAMETERS FOR THE 15 UM BAND,560-800 CM-1; 
! 2) THE CONTINUUM COEFFICIENT FOR THE 800-990,1070-1200 CM-1 BAND SPECIFICALLY:
!        AWIDE       =   RANDOM "A" PARAMETER FOR  BAND
!        BWIDE       =   RANDOM "B" PARAMETER FOR  BAND
!        BETAWD      =   CONTINUUM COEFFICIENTS FOR BAND
!        APWD,BPWD   =   CAPPHI COEFFICIENTS FOR  BAND
!        ATPWD,BTPWD =   CAPPSI COEFFICIENTS FOR BAND
!        BDLOWD      =   LOWEST FREQUENCY IN EACH  FREQ  BAND
!        BDHIWD      =   HIGHEST FREQUENCY IN EACH FREQ  BAND
!        AB15WD      =   THE PRODUCT ARNDM*BRNDM FOR THE ONE BAND
!                        REPRESENTING THE 15 UM BAND COMPLEX OF CO2
!        BETINW      =   CONT.COEFFICIENT FOR A SPECIFIED WIDE
!                        FREQ.BAND (800-990 AND 1070-1200 CM-1).
!        SKO2D       =   1./BETINW, USED IN SPA88 FOR CONT. COEFFS
!        SKC1R       =   BETAWD/BETINW, USED FOR CONT. COEFF. FOR
!                        15 UM BAND IN FST88
!        SKO3R       =   RATIO OF CONT. COEFF. FOR 9.9 UM BAND TO
!                        BETINW, USED FOR 9.6 UM CONT COEFF IN FST88
!
! DATA FOR AWIDE, BWIDE, APWD, BPWD, ATPWD, BTPWD, AO3WD, BO3WD ARE OBTAINED BY USING THE AFGL 1982
! CATALOG. CONTINUUM COEFFICIENTS ARE FROM ROBERTS (1976).
!--------------------------------------------------------------------------------------------------
    REAL   (KIND=R4)                                                                            ::&
    & AWIDE   , BWIDE   ,                                                                         &
    & BETAWD  , APWD    , BPWD    , ATPWD   , BTPWD   , BDLOWD  , BDHIWD  ,                       &
    & BETINW  ,                                                                                   &
    & AB15WD  , SKO2D   ,                                                                         &
    & SKC1R   , SKO3R
!--------------------------------------------------------------------------------------------------
! MODULE BDCOMB CONTAINS RANDOM BAND PARAMETERS FOR THE LW CALCULATIONS USING COMBINED WIDE 
! FREQUENCY BANDS BETWEEN 160 AND 1200 CM-1,AS WELL AS THE 2270-2380 BAND FOR SOURCE CALC.
! BANDS 1-8:  COMBINED WIDE FREQUENCY BANDS FOR 160-560 CM-1
! BANDS 9-14: FREQUENCY BANDS,AS IN BANDTA (NARROW BANDS) FOR 560-1200 CM-1
! BAND  15:   FREQUENCY BAND 2270-2380 CM-1,USED FOR SOURCE CALCULATION ONLY THUS NBLY PRESENTLY
!             EQUALS 15
!
! BANDS ARE ARRANGED IN ORDER OF INCREASING WAVENUMBER
!
! ACOMB        - RANDOM "A" PARAMETER FOR (NBLY) BANDS
! BCOMB        - RANDOM "B" PARAMETER FOR (NBLY) BANDS
! BETACM       - CONTINUUM COEFFICIENTS FOR (NBLY) BANDS
! APCM , BPCM  - CAPPHI COEFFICIENTS FOR (NBLY) BANDS
! ATPCM, BTPCM - CAPPSI COEFFICIENTS FOR (NBLY) BANDS
! BDLOCM       - LOWEST FREQUENCY IN EACH OF (NBLY) FREQ. BANDS
! BDHICM       - HIGHEST FREQUENCY IN EACH OF (NBLY) FREQ. BANDS
! AO3CM        - RANDOM "A" PARAMETER FOR OZONE IN (3) OZONE BANDS
! BO3CM        - RANDOM "B" PARAMETER FOR OZONE IN (3) OZONE BANDS
! AB15CM       - THE PRODUCT ARNDM*BRNDM FOR THE TWO BANDS REPRESENTING THE 15 UM BAND COMPLEX 
!                OF CO2
! BETINC       - CONT.COEFFICIENT FOR A SPECIFIED WIDE FREQ.BAND (800-990 AND 1070-1200 CM-1).
! IBAND        - INDEX NO OF THE 40 WIDE BANDS USED IN COMBINED WIDE BAND CALCULATIONS. IN OTHER
!                WORDS, INDEX TELLING WHICH OF THE 40 WIDE BANDS BETWEEN 160-560 CM-1 ARE INCLUDED
!                IN EACH OF THE FIRST 8 COMBINED WIDE BANDS
!
! DATA FOR ACOMB, BCOMB, APCM, BPCM, ATPCM, BTPCM, AO3CM, BO3CM ARE OBTAINED BY USING THE AFGL 
! 1982 CATALOG. 
! CONTINUUM COEFFICIENTS ARE FROM ROBERTS 1976. 
! IBAND INDEX VALUES ARE OBTAINED BY EXPERIMENTATION.
!--------------------------------------------------------------------------------------------------
    INTEGER(KIND=I4)    , DIMENSION(40)                                                         ::&
    & IBAND
!
    REAL   (KIND=R4)    , DIMENSION(NBLY)                                                       ::&
    & ACOMB   , BCOMB   , BETACM  , APCM    , BPCM    , ATPCM   , BTPCM   , BDLOCM  , BDHICM
!
    REAL   (KIND=R4)    , DIMENSION(2)                                                          ::&
    & AB15CM
!
    REAL   (KIND=R4)    , DIMENSION(3)                                                          ::&
    & AO3CM   , BO3CM
!
    REAL   (KIND=R4)                                                                            ::&
    & BETINC 
!
    END MODULE RNDDTA
