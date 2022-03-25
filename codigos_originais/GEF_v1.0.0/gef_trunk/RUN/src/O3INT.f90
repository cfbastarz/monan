!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief COMPUTE ZONAL MEAN OZONE FOR ETA LYRS
!> @details CALCULATES SEASONAL ZONAL MEAN OZONE, EVERY 5 DEG OF LATITUDE, FOR CURRENT MODEL VERTICAL 
!! COORDINATE. OUTPUT DATA IN G/G * 1.E4 CODE IS CALLED ONLY ONCE.
!> @author ORIGINATOR - MICHAEL BALDWIN 
!> @date 92-06-08 \n
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
!> @param[inout] PHALF - MID LAYER PRESSURE (K=LM+1 IS MODEL SURFACE)
!> @details <b>Use Module:</b>
!! @arg @c CTLBLK
!! @arg @c F77KINDS
!! @arg @c MPPSTAFF
!! @arg @c PARMETA
!> @details <b>Driver:</b> 
!! @arg @c GRADFS
!> @details <b>Calls:</b>
!--------------------------------------------------------------------------------------------------     
	SUBROUTINE O3INT(PHALF)
!-------------------------------------------------------------------------------------------------- 
! SUBROUTINE O3INT
!
! SUBROUTINE: O3INT - COMPUTE ZONAL MEAN OZONE FOR ETA LYRS
! PROGRAMMER: KENNETH CAMPANA  
! ORG: W/NMC23
! DATE: 89-07-07
!
! PROGRAMMER: MICHAEL BALDWIN
! ORG: W/NMC22
! DATE: 92-06-08
!
! ABSTRACT: 
! THIS CODE WRITTEN AT GFDL...
! CALCULATES SEASONAL ZONAL MEAN OZONE, EVERY 5 DEG OF LATITUDE, FOR CURRENT MODEL VERTICAL 
! COORDINATE. OUTPUT DATA IN G/G * 1.E4 CODE IS CALLED ONLY ONCE.
!
! PROGRAM HISTORY LOG:
! 84-01-01  FELS AND SCHWARZKOPF,GFDL.
! 89-07-07  K. CAMPANA - ADAPTED STAND-ALONE CODE FOR IN-LINE USE.
! 92-06-08  M. BALDWIN - UPDATE TO RUN IN ETA MODEL
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
! PHALF - MID LAYER PRESSURE (K=LM+1 IS MODEL SURFACE)
!
! PROGRAM O3INT FROM DAN SCHWARZKOPF-GETS ZONAL MEAN O3
! OUTPUT O3 IS WINTER, SPRING, SUMMER, FALL (NORTHERN HEMISPHERE)
! DDUO3N - ZONAL MEAN OZONE DATA IN ALL MODEL LAYERS (G / G * 1.E4)
! DDO3N2 - DIMENSIONED(L,N), WHERE (L = 37) IS LATITUDE BETWEEN
! DDO3N3 - N AND S POLES, N = NUM OF VERTICAL LYRS (K=1 IS TOP LYR)
! DDO3N4 - AND SEASON-WIN, SPR, SUM, FALL.
!
! OUTPUT FILES:
! OUTPUT - PRINT FILE.
!
! USE MODULES: F77KINDS
!              PARMETA
!              SAVMEM
!
! DRIVER     : GRADFS
!
! CALLS      : ----- 
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
    USE PARMETA
    USE SAVMEM
!
    IMPLICIT NONE
!
    INTEGER(KIND=I4)    , PARAMETER :: N   = LM
    INTEGER(KIND=I4)    , PARAMETER :: NP  = N  + 1
    INTEGER(KIND=I4)    , PARAMETER :: NP2 = N  + 2
    INTEGER(KIND=I4)    , PARAMETER :: NM1 = N  - 1
!--------------------------------------------------------------------------------------------------
! SEASONAL CLIMATOLOGIES OF O3 (OBTAINED FROM A PREVIOUSLY RUN CODE WHICH INTERPOLATES O3 TO USER 
! VERTICAL COORDINATE).
! DEFINED AS 5 DEG LAT MEANS N.P.->S.P.
!--------------------------------------------------------------------------------------------------
    REAL   (KIND=R4)    , DIMENSION(82)                                                         ::&
    & QI
!
    REAL   (KIND=R4)    , DIMENSION(19, N)                                                      ::&
    & DDUO3
!
    REAL   (KIND=R4)    , DIMENSION(10, 41)                                                     ::&
    & RO31    , RO32  
!
    REAL   (KIND=R4)    , DIMENSION(19, 41)                                                     ::&
    & DUO3N
!
    REAL   (KIND=R4)    , DIMENSION(19)                                                         ::&
    & TEMPN
!
    REAL   (KIND=R4)    , DIMENSION(10, 25)                                                     ::&
    & O3HI
!
    REAL   (KIND=R4)    , DIMENSION(10, 16)                                                     ::&
    & O3LO1   , O3LO2   , O3LO3   , O3LO4
!
    REAL   (KIND=R4)    , DIMENSION(10, 16)                                                     ::&
    & O3HI1
!
    REAL   (KIND=R4)    , DIMENSION(10, 9)                                                      ::&
    & O3HI2
!
    REAL   (KIND=R4)    , DIMENSION(45)                                                         ::&
    & PH1
!
    REAL   (KIND=R4)    , DIMENSION(37)                                                         ::&
    & PH2
!
    REAL   (KIND=R4)    , DIMENSION(48)                                                         ::&
    & P1 
!
    REAL   (KIND=R4)    , DIMENSION(33)                                                         ::&
    & P2
!
    REAL   (KIND=R4)    , DIMENSION(37, N)                                                      ::&
    & O35DEG
!
    REAL   (KIND=R4)    , DIMENSION(81)                                                         ::&
    & RSTD    , RDATA   , P        
!
    REAL   (KIND=R4)    , DIMENSION(10, 41)                                                     ::&
    & RO3
!
    REAL   (KIND=R4)    , DIMENSION(10, 40)                                                     ::&
    & RO3M
!
    REAL   (KIND=R4)    , DIMENSION(N)                                                          ::&
    & RBAR
!
    REAL   (KIND=R4)    , DIMENSION(NP)                                   , INTENT(INOUT)       ::&
    & PHALF 
!
    REAL   (KIND=R4)    , DIMENSION(82)                                                         ::&
    & PH
!------------------------
! IMPLICIT NONE VARIABLES
!------------------------
    INTEGER(KIND=I4)                                                                            ::&
    & NK      , NKK     , NKP     , K       , L       , NCASE   , ITAPE   , IPLACE  , KK      ,   &
    & NKM     , NKMM    , KI      , KQ      , JJ      , KEN
!
    REAL   (KIND=R4)                                                                            ::&
    & O3RD    , O3TOT   , O3DU 
!
    EQUIVALENCE (O3HI1(1,1), O3HI(1, 1))
    EQUIVALENCE (O3HI2(1,1), O3HI(1,17))
!
    EQUIVALENCE (PH1(1), PH( 1))
    EQUIVALENCE (PH2(1), PH(46))
!
    EQUIVALENCE ( P1(1),  P(1 ))
    EQUIVALENCE ( P2(1),  P(49))
!
    DATA PH1/                                                                                     &
    &    0.,0.1027246E-04, 0.1239831E-04, 0.1491845E-04, 0.1788053E-04,                           &
    &       0.2135032E-04, 0.2540162E-04, 0.3011718E-04, 0.3558949E-04,                           &
    &       0.4192172E-04, 0.4922875E-04, 0.5763817E-04, 0.6729146E-04,                           &
    &       0.7834518E-04, 0.9097232E-04, 0.1053635E-03, 0.1217288E-03,                           &
    &       0.1402989E-03, 0.1613270E-03, 0.1850904E-03, 0.2119495E-03,                           &
    &       0.2423836E-03, 0.2768980E-03, 0.3160017E-03, 0.3602623E-03,                           &
    &       0.4103126E-03, 0.4668569E-03, 0.5306792E-03, 0.6026516E-03,                           &
    &       0.6839018E-03, 0.7759249E-03, 0.8803303E-03, 0.9987843E-03,                           &
    &       0.1133178E-02, 0.1285955E-02, 0.1460360E-02, 0.1660001E-02,                           &
    &       0.1888764E-02, 0.2151165E-02, 0.2452466E-02, 0.2798806E-02,                           &
    &       0.3197345E-02, 0.3656456E-02, 0.4185934E-02, 0.4797257E-02/
!
    DATA PH2/                                                                                     &
    &       0.5503893E-02, 0.6321654E-02, 0.7269144E-02, 0.8368272E-02,                           &
    &       0.9644873E-02, 0.1112946E-01, 0.1285810E-01, 0.1487354E-01,                           &
    &       0.1722643E-01, 0.1997696E-01, 0.2319670E-01, 0.2697093E-01,                           &
    &       0.3140135E-01, 0.3660952E-01, 0.4274090E-01, 0.4996992E-01,                           &
    &       0.5848471E-01, 0.6847525E-01, 0.8017242E-01, 0.9386772E-01,                           &
    &       0.1099026E00 , 0.1286765E00 , 0.1506574E00 , 0.1763932E00 ,                           &
    &       0.2065253E00 , 0.2415209E00 , 0.2814823E00 , 0.3266369E00 ,                           &
    &       0.3774861E00 , 0.4345638E00 , 0.4984375E00 , 0.5697097E00 ,                           &
    &       0.6490189E00 , 0.7370409E00 , 0.8344896E00 , 0.9421190E00 ,                           &
    &       0.1000000E01/
!
    DATA P1/                                                                                      &
    &       0.9300000E-05, 0.1129521E-04, 0.1360915E-04, 0.1635370E-04,                           &
    &       0.1954990E-04, 0.2331653E-04, 0.2767314E-04, 0.3277707E-04,                           &
    &       0.3864321E-04, 0.4547839E-04, 0.5328839E-04, 0.6234301E-04,                           &
    &       0.7263268E-04, 0.8450696E-04, 0.9793231E-04, 0.1133587E-03,                           &
    &       0.1307170E-03, 0.1505832E-03, 0.1728373E-03, 0.1982122E-03,                           &
    &       0.2266389E-03, 0.2592220E-03, 0.2957792E-03, 0.3376068E-03,                           &
    &       0.3844381E-03, 0.4379281E-03, 0.4976965E-03, 0.5658476E-03,                           &
    &       0.6418494E-03, 0.7287094E-03, 0.8261995E-03, 0.9380076E-03,                           &
    &       0.1063498E-02, 0.1207423E-02, 0.1369594E-02, 0.1557141E-02,                           &
    &       0.1769657E-02, 0.2015887E-02, 0.2295520E-02, 0.2620143E-02,                           &
    &       0.2989651E-02, 0.3419469E-02, 0.3909867E-02, 0.4481491E-02,                           &
    &       0.5135272E-02, 0.5898971E-02, 0.6774619E-02, 0.7799763E-02/
!
    DATA P2/                                                                                      &
    &       0.8978218E-02, 0.1036103E-01, 0.1195488E-01, 0.1382957E-01,                           &
    &       0.1599631E-01, 0.1855114E-01, 0.2151235E-01, 0.2501293E-01,                           &
    &       0.2908220E-01, 0.3390544E-01, 0.3952926E-01, 0.4621349E-01,                           &
    &       0.5403168E-01, 0.6330472E-01, 0.7406807E-01, 0.8677983E-01,                           &
    &       0.1015345E00 , 0.1189603E00 , 0.1391863E00 , 0.1630739E00 ,                           &
    &       0.1908004E00 , 0.2235461E00 , 0.2609410E00 , 0.3036404E00 ,                           &
    &       0.3513750E00 , 0.4055375E00 , 0.4656677E00 , 0.5335132E00 ,                           &
    &       0.6083618E00 , 0.6923932E00 , 0.7845676E00 , 0.8875882E00 ,                           &
    &       0.1000000E01/
    DATA O3HI1/                                                                                   &
    &  .55,  .50,  .45,  .45,  .40,  .35,  .35,  .30,  .30,  .30,  .55,  .51,  .46,  .47,  .42,   &
    &  .38,  .37,  .36,  .35,  .35,  .55,  .53,  .48,  .49,  .44,  .42,  .41,  .40,  .38,  .38,   &
    &  .60,  .55,  .52,  .52,  .50,  .47,  .46,  .44,  .42,  .41,  .65,  .60,  .55,  .56,  .53,   &
    &  .52,  .50,  .48,  .45,  .45,  .75,  .65,  .60,  .60,  .55,  .55,  .55,  .50,  .48,  .47,   &
    &  .80,  .75,  .75,  .75,  .70,  .70,  .65,  .63,  .60,  .60,  .90,  .85,  .85,  .80,  .80,   &
    &  .75,  .75,  .74,  .72,  .71, 1.10, 1.05, 1.00,  .90,  .90,  .90,  .85,  .83,  .80,  .80,   &
    & 1.40, 1.30, 1.25, 1.25, 1.25, 1.20, 1.15, 1.10, 1.05, 1.00, 1.7 , 1.7 , 1.6 , 1.6 , 1.6 ,   &
    & 1.6 , 1.6 , 1.6 , 1.5 , 1.5 , 2.1 , 2.0 , 1.9 , 1.9 , 1.9 , 1.8 , 1.8 , 1.8 , 1.7 , 1.7 ,   &
    & 2.4 , 2.3 , 2.2 , 2.2 , 2.2 , 2.1 , 2.1 , 2.1 , 2.0 , 2.0 , 2.7 , 2.5 , 2.5 , 2.5 , 2.5 ,   &
    & 2.5 , 2.4 , 2.4 , 2.3 , 2.3 , 2.9 , 2.8 , 2.7 , 2.7 , 2.7 , 2.7 , 2.7 , 2.7 , 2.6 , 2.6 ,   &
    & 3.1 , 3.1 , 3.0 , 3.0 , 3.0 , 3.0 , 3.0 , 3.0 , 2.9 , 2.8/
!
    DATA O3HI2/                                                                                   &
    &  3.3,  3.4,  3.4,  3.6,  3.7,  3.9,  4.0,  4.1,  4.0,  3.8,  3.6,  3.8,  3.9,  4.2,  4.7,   &
    &  5.3,  5.6,  5.7,  5.5,  5.2,  4.1,  4.3,  4.7,  5.2,  6.0,  6.7,  7.0,  6.8,  6.4,  6.2,   &
    &  5.4,  5.7,  6.0,  6.6,  7.3,  8.0,  8.4,  7.7,  7.1,  6.7,  6.7,  6.8,  7.0,  7.6,  8.3,   &
    & 10.0,  9.6,  8.2,  7.5,  7.2,  9.2,  9.3,  9.4,  9.6, 10.3, 10.6, 10.0,  8.5,  7.7,  7.3,   &
    & 12.6, 12.1, 12.0, 12.1, 11.7, 11.0, 10.0,  8.6,  7.8,  7.4, 14.2, 13.5, 13.1, 12.8, 11.9,   &
    & 10.9,  9.8,  8.5,  7.8,  7.5, 14.3, 14.0, 13.4, 12.7, 11.6, 10.6,  9.3,  8.4,  7.6,  7.3/
!
    DATA O3LO1/                                                                                   &
    & 14.9 , 14.2 , 13.3 , 12.5 , 11.2 , 10.3 ,  9.5 ,  8.6 ,  7.5 ,  7.4 , 14.5 , 14.1 , 13.0 ,  &
    & 11.8 , 10.5 ,  9.8 ,  9.2 ,  7.9 ,  7.4 ,  7.4 , 11.8 , 11.5 , 10.9 , 10.5 ,  9.9 ,  9.6 ,  &
    &  8.9 ,  7.5 ,  7.2 ,  7.2 ,  7.3 ,  7.7 ,  7.8 ,  8.4 ,  8.4 ,  8.5 ,  7.9 ,  7.4 ,  7.1 ,  &
    &  7.1 ,  4.1 ,  4.4 ,  5.3 ,  6.6 ,  6.9 ,  7.5 ,  7.4 ,  7.2 ,  7.0 ,  6.9 ,  1.8 ,  1.9 ,  &
    &  2.5 ,  3.3 ,  4.5 ,  5.8 ,  6.3 ,  6.3 ,  6.4 ,  6.1 ,  0.4 ,  0.5 ,  0.8 ,  1.2 ,  2.7 ,  &
    &  3.6 ,  4.6 ,  4.7 ,  5.0 ,  5.2 ,   .10,  .15 ,   .20,   .50,  1.4 ,  2.1 ,  3.0 ,  3.2 ,  &
    &  3.5 ,  3.9 ,   .07,   .10,   .12,   .30,  1.0 ,  1.4 ,  1.8 ,  1.9 ,  2.3 ,  2.5 ,   .06,  &
    &   .08,   .10,   .15,   .60,   .80,  1.4 ,  1.5 ,  1.5 ,  1.6 ,   .05,   .05,   .06,   .09,  &
    &   .20,   .40,   .70,   .80,   .90,   .90,   .05,   .05,   .06,   .08,   .10,   .13,   .20,  &
    &   .25,   .30,   .40,   .05,   .05,   .05,   .06,   .07,   .07,   .08,   .09,   .10,   .13,  &
    &   .05,   .05,   .05,   .05,   .06,   .06,   .06,   .06,   .07,   .07,   .05,   .05,   .05,  &
    &   .05,   .05,   .05,   .05,   .06,   .06,   .06,   .04,   .04,   .04,   .04,   .04,   .04,  &
    &   .04,   .05,   .05,   .05/
!
    DATA O3LO2/                                                                                   &
    & 14.8 , 14.2 , 13.8 , 12.2 , 11.0 ,  9.8 ,  8.5 ,  7.8 ,  7.4 ,  6.9 , 13.2 , 13.0 , 12.5 ,  &
    & 11.3 , 10.4 ,  9.0 ,  7.8 ,  7.5 ,  7.0 ,  6.6 , 10.6 , 10.6 , 10.7 , 10.1 ,  9.4 ,  8.6 ,  &
    &  7.5 ,  7.0 ,  6.5 ,  6.1 ,  7.0 ,  7.3 ,  7.5 ,  7.5 ,  7.5 ,  7.3 ,  6.7 ,  6.4 ,  6.0 ,  &
    &  5.8 ,  3.8 ,  4.0 ,  4.7 ,  5.0 ,  5.2 ,  5.9 ,  5.8 ,  5.6 ,  5.5 ,  5.5 ,  1.4 ,  1.6 ,  &
    &  2.4 ,  3.0 ,  3.7 ,  4.1 ,  4.6 ,  4.8 ,  5.1 ,  5.0 ,   .40,   .50,   .90,  1.2 ,  2.0 ,  &
    &  2.7 ,  3.2 ,  3.6 ,  4.3 ,  4.1 ,   .07,   .10,   .20,   .30,   .80,  1.4 ,  2.1 ,  2.4 ,  &
    &  2.7 ,  3.0 ,   .06,   .07,   .09,   .15,   .30,   .70,  1.2 ,  1.4 ,  1.6 ,  2.0 ,   .05,  &
    &   .05,   .06,   .12,   .15,   .30,   .60,   .70,   .80,   .80,   .04,   .05,   .06,   .08,  &
    &   .09,   .15,   .30,   .40,   .40,   .40,   .04,   .04,   .05,   .055,  .06,   .09,   .12,  &
    &   .13,   .15,   .15,   .03,   .03,   .045,  .052,  .055,  .06,   .07,   .07,   .06,   .07,  &
    &   .03,   .03,   .04,   .051,  .052,  .052,  .06,   .06,   .05,   .05,   .02,   .02,   .03,  &
    &   .05,   .05,   .05,   .04,   .04,   .04,   .04,   .02,   .02,   .02,   .04,   .04,   .04,  &
    &   .03,   .03,   .03,   .03/
!
    DATA O3LO3/                                                                                   &
    & 14.5 , 14.0 , 13.5 , 11.3 , 11.0 , 10.0 ,  9.0 ,  8.3 ,  7.5 ,  7.3 , 13.5 , 13.2 , 12.5 ,  &
    & 11.1 , 10.4 ,  9.7 ,  8.2 ,  7.8 ,  7.4 ,  6.8 , 10.8 , 10.9 , 11.0 , 10.4 , 10.0 ,  9.6 ,  &
    &  7.9 ,  7.5 ,  7.0 ,  6.7 ,  7.3 ,  7.5 ,  7.8 ,  8.5 ,  9.0 ,  8.5 ,  7.7 ,  7.4 ,  6.9 ,  &
    &  6.5 ,  4.1 ,  4.5 ,  5.3 ,  6.2 ,  7.3 ,  7.7 ,  7.3 ,  7.0 ,  6.6 ,  6.4 ,  1.8 ,  2.0 ,  &
    &  2.2 ,  3.8 ,  4.3 ,  5.6 ,  6.2 ,  6.2 ,  6.4 ,  6.2 ,   .30,   .50,   .60,  1.5 ,  2.8 ,  &
    &  3.7 ,  4.5 ,  4.7 ,  5.5 ,  5.6 ,   .09,   .10,   .15,   .60,  1.2 ,  2.1 ,  3.0 ,  3.5 ,  &
    &  4.0 ,  4.3 ,   .06,   .08,   .10,   .30,   .60,  1.1 ,  1.9 ,  2.2 ,  2.9 ,  3.0 ,   .04,  &
    &   .05,   .06,   .15,   .45,   .60,  1.1 ,  1.3 ,  1.6 ,  1.8 ,   .04,   .04,   .04,   .08,  &
    &   .20,   .30,   .55,   .60,   .75,   .90,   .04,   .04,   .04,   .05,   .06,   .10,   .12,  &
    &   .15,   .20,   .25,   .04,   .04,   .03,   .04,   .05,   .06,   .07,   .07,   .07,   .08,  &
    &   .03,   .03,   .04,   .05,   .05,   .05,   .05,   .05,   .05,   .05,   .03,   .03,   .03,  &
    &   .04,   .04,   .04,   .05,   .05,   .04,   .04,   .02,   .02,   .02,   .04,   .04,   .04,  &
    &   .04,   .04,   .03,   .03/
!
    DATA O3LO4/                                                                                   &
    & 14.2 , 13.8 , 13.2 , 12.5 , 11.7 , 10.5 ,  8.6 ,  7.8 ,  7.5 ,  6.6 , 12.5 , 12.4 , 12.2 ,  &
    & 11.7 , 10.8 ,  9.8 ,  7.8 ,  7.2 ,  6.5 ,  6.1 , 10.6 , 10.5 , 10.4 , 10.1 ,  9.6 ,  9.0 ,  & 
    &  7.1 ,  6.8 ,  6.1 ,  5.9 ,  7.0 ,  7.4 ,  7.9 ,  7.8 ,  7.6 ,  7.3 ,  6.2 ,  6.1 ,  5.8 ,  &
    &  5.6 ,  4.2 ,  4.6 ,  5.1 ,  5.6 ,  5.9 ,  5.9 ,  5.9 ,  5.8 ,  5.6 ,  5.3 ,  2.1 ,  2.3 ,  &
    &  2.6 ,  2.9 ,  3.5 ,  4.3 ,  4.8 ,  4.9 ,  5.1 ,  5.1 ,  0.7 ,  0.8 ,  1.0 ,  1.5 ,  2.0 ,  &
    &  2.8 ,  3.5 ,  3.6 ,  3.7 ,  4.0 ,   .15,   .20,   .40,   .50,   .60,  1.4 ,  2.1 ,  2.2 ,  &
    &  2.3 ,  2.5 ,   .08,   .10,   .15,   .25,   .30,   .90,  1.2 ,  1.3 ,  1.4 ,  1.6 ,   .07,  &
    &   .08,   .10,   .14,   .20,   .50,   .70,   .90,   .90,   .80,   .05,   .06,   .08,   .12,  &
    &   .14,   .20,   .35,   .40,   .60,   .50,   .05,   .05,   .08,   .09,   .09,   .09,   .11,  &
    &   .12,   .15,   .18,   .04,   .05,   .06,   .07,   .07,   .08,   .08,   .08,   .08,   .08,  &
    &   .04,   .04,   .05,   .07,   .07,   .07,   .07,   .07,   .06,   .05,   .02,   .02,   .04,  &
    &   .05,   .05,   .05,   .05,   .05,   .04,   .04,   .02,   .02,   .03,   .04,   .04,   .04,  &
    &   .04,   .04,   .03,   .03/
!
    NKK = 41
    NK  = 81
    NKP = NK + 1
!
    DO 24 K=1,NP
        PHALF(K) = PHALF(K) * 1.0E03
 24 END DO
!
    DO 25 K=1,NK
        PH(K) = PH(K) * 1013250.
         P(K) =  P(K) * 1013250.
 25 END DO
!
    PH(NKP) = PH(NKP) * 1013250.
!
    DO 26 K=1,25
        DO 26 L=1,10
            RO31(L,K) = O3HI(L,K)
            RO32(L,K) = O3HI(L,K)
 26 END DO
!
    DO 27 NCASE=1,4
        ITAPE  = NCASE + 50
        IPLACE = 2
        IF (NCASE == 2) IPLACE = 4
        IF (NCASE == 3) IPLACE = 1
        IF (NCASE == 4) IPLACE = 3
!--------------------------
! NCASE=1: SPRING (IN N.H.)
! NCASE=2: FALL   (IN N.H.)
! NCASE=3: WINTER (IN N.H.)
! NCASE=4: SUMMER (IN N.H.)
!--------------------------
        IF (NCASE == 1 .OR. NCASE == 2) THEN
            DO 1011 K=26,41
                DO 1011 L=1,10
                    RO31(L,K) = O3LO1(L,K-25)
                    RO32(L,K) = O3LO2(L,K-25)
        1011 END DO
        END IF
!
        IF (NCASE == 3 .OR. NCASE == 4) THEN
            DO 1031 K=26,41
                DO 1031 L=1,10
                    RO31(L,K) = O3LO3(L,K-25)
                    RO32(L,K) = O3LO4(L,K-25)
        1031 END DO
        END IF
!
        DO 30 KK=1,NKK
            DO 31 L=1,10
                DUO3N(L  ,KK) = RO31(11-L,KK)
                DUO3N(L+9,KK) = RO32(   L,KK)
         31 END DO
            DUO3N(10,KK) = .5 * (RO31(1,KK) + RO32(1,KK))
     30 END DO
!--------------------------------------------------------------------
! FOR NCASE=2 OR NCASE=4,REVERSE LATITUDE ARRANGEMENT OF CORR. SEASON
!--------------------------------------------------------------------
        IF (NCASE == 2 .OR. NCASE == 4) THEN
            DO 1024 KK=1,NKK
                DO 1025 L=1,19
                    TEMPN(L) = DUO3N(20-L,KK)
           1025 END DO
!
                DO 1026 L=1,19
                    DUO3N(L,KK) = TEMPN(L)
           1026 END DO
!
       1024 END DO
        END IF
!-----------------------------------------------------------------------
! DUO3N NOW IS O3 PROFILE FOR APPROPRIATE SEASON,AT STD. PRESSURE LEVELS
! AC  WRITE (6,800) DUO3N
! BEGIN LATITUDE (10 DEG) LOOP
!-----------------------------------------------------------------------
        DO 33 L=1,19
            DO 22 KK=1,NKK
                RSTD(KK) = DUO3N(L,KK)
         22 END DO
!
            NKM  = NK - 1
            NKMM = NK - 3
!----------------------------------------- 
! BESSELS HALF-POINT INTERPOLATION FORMULA
!----------------------------------------- 
            DO 60 K=4,NKMM,2
                KI       = K / 2
                RDATA(K) = .5 * (RSTD(KI  ) + RSTD(KI+1))                                          &
    &                    -      (RSTD(KI+2) - RSTD(KI+1) - RSTD(KI) + RSTD(KI-1)) / 16.
         60 END DO
!
            RDATA(2)   = .5 * (RSTD(2)   + RSTD(1))
            RDATA(NKM) = .5 * (RSTD(NKK) + RSTD(NKK-1))
!---------------------------------- 
! PUT UNCHANGED DATA INTO NEW ARRAY
!----------------------------------
            DO 61 K=1,NK,2
                KQ       = (K + 1) / 2
                RDATA(K) = RSTD(KQ)
         61 END DO
!-------------------------------------------------------------
! NOTE TO NMC: THIS WRITE IS COMMENTED OUT TO REDUCE PRINTOUT
! WRITE (6,798) RDATA
! CALCULATE LAYER-MEAN OZONE MIXING RATIO FOR EACH MODEL LEVEL
!-------------------------------------------------------------
            DO 99 KK=1,N
                RBAR(KK) = 0.
!----------------------------------------------- 
! LOOP TO CALCULATE SUMS TO GET LAYER OZONE MEAN
!-----------------------------------------------
                DO 98 K=1,NK
                    IF (PH(K+1) < PHALF(KK)) GOTO 98
                    IF (PH(K) > PHALF(KK+1)) GOTO 98
!
                    IF (PH(K+1) < PHALF(KK+1) .AND. PH(K) <  PHALF(KK))                           &
    &                   RBAR(KK) = RBAR(KK) + RDATA(K) * (PH(K+1) - PHALF(KK))
!
                    IF (PH(K+1) < PHALF(KK+1) .AND. PH(K) >= PHALF(KK))                           &
    &                   RBAR(KK) = RBAR(KK) + RDATA(K) * (PH(K+1) - PH(K))
!
                    IF (PH(K+1) > PHALF(KK+1) .AND. PH(K) >  PHALF(KK))                           &
    &                   RBAR(KK) = RBAR(KK) + RDATA(K) * (PHALF(KK+1) - PH(K))
!
             98 CONTINUE
!
                RBAR(KK) = RBAR(KK) / (PHALF(KK+1) - PHALF(KK))
!
                IF (RBAR(KK) > .0000) GOTO 99
!--------------------------------------------------------------------------------------------------------------------------------    
! CODE TO COVER CASE WHEN MODEL RESOLUTION IS SO FINE THAT NO VALUE OF P(K) IN THE OZONE DATA ARRAY FALLS BETWEEN PHALF(KK+1) AND
! PHALF(KK). PROCEDURE IS TO SIMPLY GRAB THE NEAREST VALUE FROM RDATA
!--------------------------------------------------------------------------------------------------------------------------------
                DO 29 K=1,NK
                    IF (PH(K) < PHALF(KK) .AND. PH(K+1) >= PHALF(KK+1)) RBAR(KK) = RDATA(K)
             29 END DO
!
         99 CONTINUE
!---------------------- 
! CALCULATE TOTAL OZONE
!----------------------
            O3RD = 0.
!
            DO 89 KK=1,80
                O3RD = O3RD + RDATA(KK) * (PH(KK+1) - PH(KK))
         89 END DO
!
            O3RD  = O3RD + RDATA(81) * (P(81) - PH(81))
            O3RD  = O3RD / 980.
            O3TOT = 0.
!
            DO 88 KK=1,N
                O3TOT = O3TOT + RBAR(KK) * (PHALF(KK+1) - PHALF(KK))
         88 END DO
!
            O3TOT = O3TOT / 980.
!---------------------------
! UNITS ARE MICROGRAMS/CM**2
!---------------------------
            O3DU = O3TOT / 2.144
!----------------------------------------------------
! O3DU UNITS ARE DOBSON UNITS (10**-3 ATM-CM)
! NOTE TO NMC: THIS IS COMMENTED OUT TO SAVE PRINTOUT
! WRITE (6,796) O3RD,O3TOT,O3DU
!----------------------------------------------------
            DO 23 KK=1,N
                DDUO3(L,KK) = RBAR(KK) * .01
         23 END DO
     33 END DO
!-------------------- 
!END OF LATITUDE LOOP
!-------------------- 
!
!-----------------------------------------------------------------------
! CREATE 5 DEG OZONE QUANTITIES BY LINEAR INTERPOLATION OF 10 DEG VALUES
!-----------------------------------------------------------------------
        DO 1060 KK=1,N
            DO 1061 L=1,19
                O35DEG(2*L-1,KK) = DDUO3(L,KK)
       1061 END DO
!
            DO 1062 L=1,18
                O35DEG(2*L  ,KK) = 0.5 * (DDUO3(L,KK) + DDUO3(L+1,KK))
       1062 END DO
   1060 END DO
!------------------------------------------------------ 
! OUTPUT TO UNIT (ITAPE) THE OZONE VALUES FOR LATER USE
!------------------------------------------------------ 
        IF (IPLACE == 1) THEN
            DO 302 JJ=1,37
                DO 302 KEN=1,N
                    DDUO3N(JJ,KEN) = O35DEG(JJ,KEN)
        302 END DO
!
        ELSE IF (IPLACE == 2) THEN
            DO 312 JJ=1,37
                DO 312 KEN=1,N
                    DDO3N2(JJ,KEN) = O35DEG(JJ,KEN)
        312 END DO
!
        ELSE IF (IPLACE == 3) THEN
            DO 322 JJ=1,37
                DO 322 KEN=1,N
                    DDO3N3(JJ,KEN) = O35DEG(JJ,KEN)
        322 END DO
!
        ELSE IF (IPLACE == 4) THEN
            DO 332 JJ=1,37
                DO 332 KEN=1,N
                    DDO3N4(JJ,KEN) = O35DEG(JJ,KEN)
        332 END DO
        END IF
!
 27 END DO
!----------------------- 
! END OF LOOP OVER CASES
!----------------------- 
    RETURN
!
       1 FORMAT(10F4.2)
       2 FORMAT(10X,E14.7,1X,E14.7,1X,E14.7,1X,E14.7,1X)
       3 FORMAT(10E12.5)
     797 FORMAT(10F7.2)
     799 FORMAT(19F6.4)
     800 FORMAT(19F6.2)
     102 FORMAT(' O3 IPLACE=',I4)
    1033 FORMAT(19F6.5)
!
    END SUBROUTINE O3INT
