!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief INITIALIZE CONSTANTS AND LOOKUP TABLES FOR MICROPHYSICS
!> @details READS VARIOUS MICROPHYSICAL LOOKUP TABLES USED IN COLUMN_MICRO
!! LOOKUP TABLES WERE CREATED "OFFLINE" AND ARE READ IN DURING EXECUTION
!! CREATES LOOKUP TABLES FOR SATURATION VAPOR PRESSURE W/R/T WATER & ICE
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
!> @param[in] DTPH - PHYSICS TIME STEP (S)
!> @details <b>Use Module:</b>
!! @arg @c CMICRO_CONS
!! @arg @c CMY600
!! @arg @c DYNAM
!! @arg @c F77KINDS
!! @arg @c IACCR_TABLES
!! @arg @c IMASS_TABLES
!! @arg @c IRIME_TABLES
!! @arg @c IVENT_TABLES
!! @arg @c MAPOT
!! @arg @c MPPSTAFF
!! @arg @c PARMETA
!! @arg @c RACCR_TABLES
!! @arg @c RMASS_TABLES
!! @arg @c RRATE_TABLES
!! @arg @c RVELR_TABLES
!! @arg @c RVENT_TABLES
!! @arg @c SDENS_TABLES
!> @details <b>Driver:</b> 
!! @arg @c GSMDRIVE
!> @details <b>Calls:</b>
!! @arg @c GPVS
!! @arg @c MY_GROWTH_RATES
!--------------------------------------------------------------------------------------------------
    SUBROUTINE GSMCONST(DTPH)
!--------------------------------------------------------------------------------------------------
! SUBROUTINE GSMCONST
!
! SUBPROGRAM: GSMCONST - INITIALIZE CONSTANTS AND LOOKUP TABLES FOR MICROPHYSICS
! PROGRAMMER: FERRIER
! ORG: W/NP22
! DATE: 01-02-??
!
! ABSTRACT:
! READS VARIOUS MICROPHYSICAL LOOKUP TABLES USED IN COLUMN_MICRO
! LOOKUP TABLES WERE CREATED "OFFLINE" AND ARE READ IN DURING EXECUTION
! CREATES LOOKUP TABLES FOR SATURATION VAPOR PRESSURE W/R/T WATER & ICE
!
! PROGRAM HISTORY LOG:
! 01-02-??  FERRIER    - ORIGINATOR
! 18-01-15  LUCCI      - MODERNIZATION OF THE CODE, INCLUDING:
!                        * F77 TO F90/F95
!                        * INDENTATION & UNIFORMIZATION CODE
!                        * REPLACEMENT OF COMMONS BLOCK FOR MODULES
!                        * DOCUMENTATION WITH DOXYGEN
!                        * OPENMP FUNCTIONALITY
!
! INPUT ARGUMENT LIST:
! DTPH - PHYSICS TIME STEP (S)
!
! OUTPUT ARGUMENT LIST:
! NONE
!
! INPUT/OUTPUT ARGUMENT LIST:
! NONE
!
! USE MODULES: CMICRO_CONS
!              CMY600
!              DYNAM
!              F77KINDS
!              IACCR_TABLES
!              IMASS_TABLES 
!              IRATE_TABLES
!              IRIME_TABLES
!              IVENT_TABLES
!              MAPOT
!              PARMETA
!              RACCR_TABLES
!              RMASS_TABLES
!              RRATE_TABLES
!              RVELR_TABLES
!              RVENT_TABLES
!              SDENS_TABLES
! 
! DRIVER     : GSMDRIVE
!
! CALLS      : GPVS
!              MY_GROWTH_RATES
!--------------------------------------------------------------------------------------------------
    USE CMICRO_CONS
    USE CMY600
    USE DYNAM
    USE F77KINDS
    USE IACCR_TABLES
    USE IMASS_TABLES
    USE IRIME_TABLES
    USE IVENT_TABLES
    USE MAPOT
    USE MPPSTAFF
    USE PARMETA
    USE RACCR_TABLES
    USE RMASS_TABLES
    USE RRATE_TABLES
    USE RVELR_TABLES
    USE RVENT_TABLES
    USE SDENS_TABLES
! 
    IMPLICIT NONE
!-----------------------------------------------------
! PARAMETERS AND DATA STATEMENT FOR LOCAL CALCULATIONS
!-----------------------------------------------------
    REAL   (KIND=R4)    , PARAMETER :: C1   =    1.      / 3.
    REAL   (KIND=R4)    , PARAMETER :: DMR1 =     .1E-3    
    REAL   (KIND=R4)    , PARAMETER :: DMR2 =     .2E-3    
    REAL   (KIND=R4)    , PARAMETER :: DMR3 =     .32E-3   
    REAL   (KIND=R4)    , PARAMETER :: N0R0 =    8.E6     
    REAL   (KIND=R4)    , PARAMETER :: N0S0 =    4.E6     
    REAL   (KIND=R4)    , PARAMETER :: RHOL = 1000.    
    REAL   (KIND=R4)    , PARAMETER :: RHOS =  100.     
    REAL   (KIND=R4)    , PARAMETER :: T0C  =  273.15   
    REAL   (KIND=R4)    , PARAMETER :: XMR1 =    1.E6    * DMR1
    REAL   (KIND=R4)    , PARAMETER :: XMR2 =    1.E6    * DMR2
    REAL   (KIND=R4)    , PARAMETER :: XMR3 =    1.E6    * DMR3
!
    INTEGER(KIND=I4)    , PARAMETER :: MDR1 = XMR1
    INTEGER(KIND=I4)    , PARAMETER :: MDR2 = XMR2
    INTEGER(KIND=I4)    , PARAMETER :: MDR3 = XMR3
!------------------------
! IMPLICIT NONE VARIABLES
!------------------------
    REAL   (KIND=R4)                                                      , INTENT(IN)          ::&
    & DTPH  
!
    REAL   (KIND=R4)                                                                            ::&
    & PI      , BBFR    , FPVS0   , XNCW    , VSNOWI
!
    INTEGER(KIND=I4)                                                                            ::&
    & I
!
    CHARACTER(LEN=150)                                                                          ::&
    & ADDRESS01, ADDRESS02
!
    RHGRD = 0.90
!--------------------------------------------------------------------- 
! CREATE LOOKUP TABLES FOR SATURATION VAPOR PRESSURE W/R/T WATER & ICE
!--------------------------------------------------------------------- 
    CALL GPVS
!------------------------------ 
! READ IN VARIOUS LOOKUP TABLES
!------------------------------
    ADDRESS01 = 'eta_micro_lookup.dat'                     
    OPEN (UNIT=1, FILE=TRIM(ADDRESS01), STATUS='OLD', FORM='UNFORMATTED')
    READ(1) VENTR1
    READ(1) VENTR2
    READ(1) ACCRR
    READ(1) MASSR
    READ(1) VRAIN
    READ(1) RRATE
    READ(1) VENTI1
    READ(1) VENTI2
    READ(1) ACCRI
    READ(1) MASSI
    READ(1) VSNOWI
    READ(1) VEL_RF
!    READ(1) MY_GROWTH    ! APPLICABLE ONLY FOR DTPH = 180 S
    CLOSE (1)
!--------------------------------------------------------------------------------------------------
! CALCULATES COEFFICIENTS FOR GROWTH RATES OF ICE NUCLEATED IN WATER SATURATED CONDITIONS, SCALED 
! BY PHYSICS TIME STEP (LOOKUP TABLE)
!--------------------------------------------------------------------------------------------------
    CALL MY_GROWTH_RATES(DTPH)
!
    PI = ACOS(-1.)
!--------------------------------------------------------------------------------------------------
! CONSTANTS ASSOCIATED WITH BIGGS (1953) FREEZING OF RAIN, AS PARAMETERIZED FOLLOWING LIN ET AL. 
! (JCAM, 1983) AND REISNER ET AL. (1998, QJRMS).
!--------------------------------------------------------------------------------------------------
    ABFR = -  0.66
    BBFR =  100.
    CBFR =   20.   * PI * PI * BBFR * RHOL * 1.E-42
!--------------------------------------------------------------------------------------------------
! CIACW IS USED IN CALCULATING RIMING RATES THE ASSUMED EFFECTIVE COLLECTION EFFICIENCY OF CLOUD 
! WATER RIMED ONTO ICE IS =0.5 BELOW:
!--------------------------------------------------------------------------------------------------
    CIACW = DTPH * 0.25 * PI * 0.5 * (1.E5) ** C1
!--------------------------------------------------------------------------------------------------
! CIACR IS USED IN CALCULATING FREEZING OF RAIN COLLIDING WITH LARGE ICE THE ASSUMED COLLECTION 
! EFFICIENCY IS 1.0
!--------------------------------------------------------------------------------------------------
    CIACR = PI * DTPH
!--------------------------------------------------------------------------------------------------
! BASED ON RAIN LOOKUP TABLES FOR MEAN DIAMETERS FROM 0.05 TO 0.45 MM
! FOUR DIFFERENT FUNCTIONAL RELATIONSHIPS OF MEAN DROP DIAMETER AS A FUNCTION OF RAIN RATE (RR), 
! DERIVED BASED ON SIMPLE FITS TO MASS-WEIGHTED FALL SPEEDS OF RAIN AS FUNCTIONS OF MEAN DIAMETER 
! FROM THE LOOKUP TABLES.
!--------------------------------------------------------------------------------------------------
    RR_DRMIN  = N0R0 * RRATE(MDRMIN)     ! RR FOR MEAN DROP DIAMETER OF .05 MM
    RR_DR1    = N0R0 * RRATE(MDR1)       ! RR FOR MEAN DROP DIAMETER OF .10 MM
    RR_DR2    = N0R0 * RRATE(MDR2)       ! RR FOR MEAN DROP DIAMETER OF .20 MM
    RR_DR3    = N0R0 * RRATE(MDR3)       ! RR FOR MEAN DROP DIAMETER OF .32 MM
    RR_DRMAX  = N0R0 * RRATE(MDRMAX)     ! RR FOR MEAN DROP DIAMETER OF .45 MM
!
    RQR_DRMIN = N0R0 * MASSR(MDRMIN)     ! RAIN CONTENT FOR MEAN DROP DIAMETER OF .05 MM
    RQR_DR1   = N0R0 * MASSR(MDR1)       ! RAIN CONTENT FOR MEAN DROP DIAMETER OF .10 MM
    RQR_DR2   = N0R0 * MASSR(MDR2)       ! RAIN CONTENT FOR MEAN DROP DIAMETER OF .20 MM
    RQR_DR3   = N0R0 * MASSR(MDR3)       ! RAIN CONTENT FOR MEAN DROP DIAMETER OF .32 MM
    RQR_DRMAX = N0R0 * MASSR(MDRMAX)     ! RAIN CONTENT FOR MEAN DROP DIAMETER OF .45 MM
!
    C_N0R0 = PI * RHOL * N0R0
    CN0R0  = 1.E6 / C_N0R0 ** .25
!
    CN0R_DMRMIN = 1. / (PI * RHOL * DMRMIN ** 4)
    CN0R_DMRMAX = 1. / (PI * RHOL * DMRMAX ** 4)
!--------------------------------------------------------------- 
! CRACW IS USED IN CALCULATING COLLECTION OF CLOUD WATER BY RAIN 
! (AN ASSUMED COLLECTION EFFICIENCY OF 1.0)
!--------------------------------------------------------------- 
    CRACW = DTPH * 0.25 * PI * 1.0
!
    ESW0  = 1000.  *  FPVS0(T0C)     ! SATURATION VAPOR PRESSURE AT 0C
    RFMAX =    1.1 ** NRIME          ! MAXIMUM RIME FACTOR ALLOWED
!--------------------------------------- 
! CONSTANTS PASSED THROUGH ARGUMENT LIST 
!--------------------------------------- 
!
!--------------------------------------------------------------------------------------------------
! IMPORTANT PARAMETERS FOR SELF COLLECTION (AUTOCONVERSION) OF CLOUD WATER TO RAIN.
!
! CRAUT IS PROPORTIONAL TO THE RATE THAT CLOUD WATER IS CONVERTED BY SELF COLLECTION TO RAIN 
! (AUTOCONVERSION RATE)
!--------------------------------------------------------------------------------------------------
    CRAUT = 1. -EXP(-1.E-3 * DTPH)
!--------------------------------------------------------------------------------------------------
! QAUT0 IS THE THRESHOLD CLOUD CONTENT FOR AUTOCONVERSION TO RAIN NEEDED FOR DROPLETS TO REACH A 
! DIAMETER OF 20 MICRONS (FOLLOWING MANTON AND COTTON, 1977; BANTA AND HANSON, 1987, JCAM).
! QAUT0=1.2567, 0.8378, OR 0.4189 G/M**3 FOR DROPLET NUMBER CONCENTRATIONS OF 300, 200, AND 
! 100 CM**-3, RESPECTIVELY
!--------------------------------------------------------------------------------------------------
    XNCW  = 200.E6                               ! 300 CM**-3 DROPLET CONCENTRATION
    QAUT0 = PI * RHOL * XNCW * (20.E-6) ** 3/6.
!--------------------------------------------------------------------------------------------------
! FOR CALCULATING SNOW OPTICAL DEPTHS BY CONSIDERING BULK DENSITY OF SNOW BASED ON EMAILS FROM 
! Q. FU (6/27-28/01), WHERE OPTICAL DEPTH (T) = 1.5*SWP/(REFF*DENS), SWP IS SNOW WATER PATH, REFF 
! IS EFFECTIVE RADIUS, AND DENS IS THE BULK DENSITY OF SNOW.
!
! SWP (KG/M**2)=(1.E-3 KG/G)*SWPRAD, SWPRAD IN G/M**2 USED IN RADIATION
! T = 1.5*1.E3*SWPRAD/(REFF*DENS)
!
! SEE DERIVATION FOR MASSI(INDEXS), NOTE EQUAL TO RHO*QSNOW/NSNOW
!
! SDENS=1.5E3/DENS, DENS=MASSI(INDEXS)/[PI*(1.E-6*INDEXS)**3]
!--------------------------------------------------------------------------------------------------
    DO I=MDIMIN,MDIMAX
        SDENS(I) = PI * 1.5E-15 * FLOAT(I*I*I) / MASSI(I)
    END DO
!
    RETURN
!
    END SUBROUTINE GSMCONST
!
!
!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief SETS UP LOOKUP TABLE FOR CALCULATING INITIAL ICE CRYSTAL GROWTH
!> @details BELOW ARE TABULATED VALUES FOR THE PREDICTED MASS OF ICE CRYSTALS AFTER 600 S OF GROWTH
!! IN WATER SATURATED CONDITIONS, BASED ON CALCULATIONS FROM MILLER AND YOUNG (JAS, 1979). 
!! THESE VALUES ARE CRUDELY ESTIMATED FROM TABULATED CURVES AT 600 S FROM FIG. 6.9 OF YOUNG (1993). 
!! VALUES AT TEMPERATURES COLDER THAN -27C WERE ASSUMED TO BE INVARIANT WITH TEMPERATURE.
!! USED TO NORMALIZE MILLER & YOUNG (1979) CALCULATIONS OF ICE GROWTH OVER LARGE TIME STEPS USING 
!! THEIR TABULATED VALUES AT 600 S.
!! ASSUMES 3D GROWTH WITH TIME**1.5 FOLLOWING EQ. (6.3) IN YOUNG (1993).
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
!> @param[in] DTPH - PHYSICS TIME STEP (S)
!> @details <b>Use Module:</b>
!! @arg @c CMY600
!! @arg @c F77KINDS
!> @details <b>Driver:</b> 
!! @arg @c GSMCONST
!> @details <b>Calls:</b>
!--------------------------------------------------------------------------------------------------
!---------------------------------------------------------------- 
! SETS UP LOOKUP TABLE FOR CALCULATING INITIAL ICE CRYSTAL GROWTH 
!---------------------------------------------------------------- 
    SUBROUTINE MY_GROWTH_RATES(DTPH)
!--------------------------------------------------------------------------------------------------
! SUBROUTINE MY_GROWTH_RATES
!
! SUBPROGRAM: MY_GROWTH_RATES - SETS UP LOOKUP TABLE FOR CALCULATING INITIAL ICE CRYSTAL GROWTH
! PROGRAMMER: ?????
! ORG: ?????
! DATE: ??-??-??
!
! ABSTRACT:
! BELOW ARE TABULATED VALUES FOR THE PREDICTED MASS OF ICE CRYSTALS AFTER 600 S OF GROWTH IN WATER 
! SATURATED CONDITIONS, BASED ON CALCULATIONS FROM MILLER AND YOUNG (JAS, 1979). THESE VALUES ARE 
! CRUDELY ESTIMATED FROM TABULATED CURVES AT 600 S FROM FIG. 6.9 OF YOUNG (1993). 
! VALUES AT TEMPERATURES COLDER THAN -27C WERE ASSUMED TO BE INVARIANT WITH TEMPERATURE.
! USED TO NORMALIZE MILLER & YOUNG (1979) CALCULATIONS OF ICE GROWTH OVER LARGE TIME STEPS USING 
! THEIR TABULATED VALUES AT 600 S.
! ASSUMES 3D GROWTH WITH TIME**1.5 FOLLOWING EQ. (6.3) IN YOUNG (1993).
!
! PROGRAM HISTORY LOG:
! ??-??-??  FERRIER    - ORIGINATOR
! 18-01-15  LUCCI      - MODERNIZATION OF THE CODE, INCLUDING:
!                        * F77 TO F90/F95
!                        * INDENTATION & UNIFORMIZATION CODE
!                        * REPLACEMENT OF COMMONS BLOCK FOR MODULES
!                        * DOCUMENTATION WITH DOXYGEN
!                        * OPENMP FUNCTIONALITY
!
! INPUT ARGUMENT LIST:
! DTPH - PHYSICS TIME STEP (S)
!
! OUTPUT ARGUMENT LIST:
! NONE
!
! INPUT/OUTPUT ARGUMENT LIST:
! NONE
!
! USE MODULES: CMY600
!              F77KINDS
!  
! DRIVER     : GSMCONST
! 
! CALLS      : -----
!--------------------------------------------------------------------------------------------------
    USE CMY600
    USE F77KINDS
!
    IMPLICIT NONE
!
    REAL   (KIND=R4)    , DIMENSION(MY_T1:MY_T2)                                                ::&
    & MY_600
!------------------------
! IMPLICIT NONE VARIABLES
!------------------------
    REAL   (KIND=R4)                                                      , INTENT(IN)          ::&
    & DTPH
!
    REAL   (KIND=R4)                                                                            ::&
    & DT_ICE
!
    DATA MY_600 /                                                                                 &
!------------------ 
!  - 1 TO  -5 DEG C   
!------------------
    & 5.5E-8 , 1.4E-7 , 2.8E-7, 6.E-7  , 3.3E-6 ,                                                 & 
!------------------ 
!  - 6 TO -10 DEG C   
!------------------
    & 2.E-6  , 9.E-7  , 8.8E-7, 8.2E-7 , 9.4E-7 ,                                                 & 
!------------------ 
!  -11 TO -15 DEG C   
!------------------
    & 1.2E-6 , 1.85E-6, 5.5E-6, 1.5E-5 , 1.7E-5 ,                                                 & 
!------------------ 
!  -16 TO -20 DEG C   
!------------------
    & 1.5E-5 , 1.E-5  , 3.4E-6, 1.85E-6, 1.35E-6,                                                 & 
!------------------ 
!  -21 TO -25 DEG C   
!------------------
    & 1.05E-6, 1.E-6  , 9.5E-7, 9.0E-7 , 9.5E-7 ,                                                 & 
!------------------ 
!  -26 TO -30 DEG C   
!------------------
    & 9.5E-7 , 9.E-7  , 9.E-7 , 9.E-7  , 9.E-7  ,                                                 & 
!------------------ 
!  -31 TO -35 DEG C   
!------------------
    & 9.E-7  , 9.E-7  , 9.E-7 , 9.E-7  , 9.E-7 /           
!
    DT_ICE    = (DTPH / 600.) ** 1.5
    MY_GROWTH = DT_ICE * MY_600
!
    RETURN
!
    END SUBROUTINE MY_GROWTH_RATES
!
!
!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief COMPUTE SATURATION VAPOR PRESSURE TABLE
!> @details COMPUTE SATURATION VAPOR PRESSURE TABLE AS A FUNCTION OF TEMPERATURE FOR THE TABLE 
!! LOOKUP FUNCTION FPVS.
!! EXACT SATURATION VAPOR PRESSURES ARE CALCULATED IN SUBPROGRAM FPVSX.
!! THE CURRENT IMPLEMENTATION COMPUTES A TABLE WITH A LENGTH OF 7501 FOR TEMPERATURES RANGING FROM 
!! 180.0 TO 330.0 KELVIN.
!> @author ORIGINATOR - N. PHILLIPS
!> @date 82-12-30 \n
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
!! @arg @c COMPVS0
!! @arg @c COMPVS
!! @arg @c F77KINDS
!> @details <b>Driver:</b> 
!! @arg @c GSMCONST
!> @details <b>Calls:</b>
!--------------------------------------------------------------------------------------------------
!-------------------------------------------------------------------- 
! LOOKUP TABLES FOR THE SATURATION VAPOR PRESSURE W/R/T WATER AND ICE
!-------------------------------------------------------------------- 
    SUBROUTINE GPVS
!--------------------------------------------------------------------------------------------------
! SUBROUTINE GPVS
!
! SUBPROGRAM: GPVS - COMPUTE SATURATION VAPOR PRESSURE TABLE
! PROGRAMMER: N. PHILLIPS
! ORG: W/NP2
! DATE: 82-12-30
!
! ABSTRACT:
! COMPUTE SATURATION VAPOR PRESSURE TABLE AS A FUNCTION OF TEMPERATURE FOR THE TABLE LOOKUP 
! FUNCTION FPVS.
! EXACT SATURATION VAPOR PRESSURES ARE CALCULATED IN SUBPROGRAM FPVSX.
! THE CURRENT IMPLEMENTATION COMPUTES A TABLE WITH A LENGTH OF 7501 FOR TEMPERATURES RANGING FROM 
! 180.0 TO 330.0 KELVIN.
!
! PROGRAM HISTORY LOG:
! 91-05-07  IREDELL
! 04-12-30  IREDELL - EXPAND TABLE
! 06-02-19  HONG    - ICE EFFECT
! 18-01-15  LUCCI   - MODERNIZATION OF THE CODE, INCLUDING:
!                     * F77 TO F90/F95
!                     * INDENTATION & UNIFORMIZATION CODE
!                     * REPLACEMENT OF COMMONS BLOCK FOR MODULES
!                     * DOCUMENTATION WITH DOXYGEN
!                     * OPENMP FUNCTIONALITY
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
! USE MODULES: COMPVS0
!              COMPVS
!              F77KINDS
!  
! DRIVER     : GSMCONST
!
! CALLS      : -----
!-------------------------------------------------------------------------------------------------
    USE COMPVS0
    USE COMPVS
    USE F77KINDS
!
    IMPLICIT NONE
!------------------------
! IMPLICIT NONE VARIABLES
!------------------------
    REAL   (KIND=R4)                                                                            ::&
    & XMIN    , XMAX    , XINC    , X       , T       , FPVSX   , FPVSX0  
!
    INTEGER(KIND=I4)                                                                            ::&
    & JX      
!    
    XMIN = 180.0
    XMAX = 330.0
!
    XINC = (XMAX - XMIN) / (NX - 1)
!
    C1XPVS  = 1. - XMIN / XINC
    C2XPVS  = 1. / XINC
    C1XPVS0 = 1. - XMIN / XINC
    C2XPVS0 = 1. / XINC
!
    DO JX=1,NX
        X = XMIN + (JX - 1) * XINC
        T = X
!
         TBPVS(JX) =  FPVSX(T)
        TBPVS0(JX) = FPVSX0(T)
    END DO
!
    RETURN
!
    END SUBROUTINE GPVS
!
!
!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief COMPUTE SATURATION VAPOR PRESSURE
!> @details COMPUTE SATURATION VAPOR PRESSURE FROM THE TEMPERATURE.
!! A LINEAR INTERPOLATION IS DONE BETWEEN VALUES IN A LOOKUP TABLE COMPUTED IN GPVS. 
!! SEE DOCUMENTATION FOR FPVSX FOR DETAILS.
!! INPUT VALUES OUTSIDE TABLE RANGE ARE RESET TO TABLE EXTREMA.
!! THE INTERPOLATION ACCURACY IS ALMOST 6 DECIMAL PLACES.
!! ON THE CRAY, FPVS IS ABOUT 4 TIMES FASTER THAN EXACT CALCULATION.
!! THIS FUNCTION SHOULD BE EXPANDED INLINE IN THE CALLING ROUTINE
!> @author ORIGINATOR - N. PHILLIPS
!> @date 82-12-30 \n
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
!> @param[in] T - REAL TEMPERATURE IN KELVIN
!> @details <b>Use Module:</b>
!! @arg @c COMPVS
!! @arg @c F77KINDS
!> @details <b>Driver:</b> 
!! @arg @c GSMCONST
!> @details <b>Calls:</b>
!--------------------------------------------------------------------------------------------------
    FUNCTION FPVS(T)
!--------------------------------------------------------------------------------------------------
! FUNCTION FPVS
!
! SUBPROGRAM: FPVS - COMPUTE SATURATION VAPOR PRESSURE
! PROGRAMMER: N. PHILLIPS
! ORG: W/NP2
! DATE: 82-12-30
!
! ABSTRACT: 
! COMPUTE SATURATION VAPOR PRESSURE FROM THE TEMPERATURE.
! A LINEAR INTERPOLATION IS DONE BETWEEN VALUES IN A LOOKUP TABLE COMPUTED IN GPVS. 
! SEE DOCUMENTATION FOR FPVSX FOR DETAILS.
! INPUT VALUES OUTSIDE TABLE RANGE ARE RESET TO TABLE EXTREMA.
! THE INTERPOLATION ACCURACY IS ALMOST 6 DECIMAL PLACES.
! ON THE CRAY, FPVS IS ABOUT 4 TIMES FASTER THAN EXACT CALCULATION.
! THIS FUNCTION SHOULD BE EXPANDED INLINE IN THE CALLING ROUTINE.
!
! PROGRAM HISTORY LOG:
! 91-05-07  IREDELL - MADE INTO INLINABLE FUNCTION
! 94-12-30  IREDELL - EXPAND TABLE
! 96-02-19  HONG    - ICE EFFECT
! 18-01-15  LUCCI   - MODERNIZATION OF THE CODE, INCLUDING:
!                     * F77 TO F90/F95
!                     * INDENTATION & UNIFORMIZATION CODE
!                     * REPLACEMENT OF COMMONS BLOCK FOR MODULES
!                     * DOCUMENTATION WITH DOXYGEN
!                     * OPENMP FUNCTIONALITY
!
! INPUT ARGUMENT LIST:
! T    - REAL TEMPERATURE IN KELVIN
!
! OUTPUT ARGUMENT LIST:
! FPVS - REAL SATURATION VAPOR PRESSURE IN KILOPASCALS (CB)
!
! INPUT/OUTPUT ARGUMENT LIST:
! NONE
!
! USE MODULES: COMPVS
!              F77KINDS
!  
! DRIVER     : GSMCONST
!
! CALLS      : -----
!--------------------------------------------------------------------------------------------------
    USE COMPVS
    USE F77KINDS
!
    IMPLICIT NONE
!------------------------
! IMPLICIT NONE VARIABLES
!------------------------
    REAL   (KIND=R4)                                                      , INTENT(IN)          ::&
    & T
!
    REAL   (KIND=R4)                                                                            ::&
    & FPVS    , XJ
!
    INTEGER(KIND=I4)                                                                            ::&
    & JX
!
    XJ = MIN(MAX(C1XPVS + C2XPVS * T, 1.), FLOAT(NX))
    JX = MIN(XJ, NX - 1.)
!
    FPVS = TBPVS(JX) + (XJ - JX) * (TBPVS(JX + 1) - TBPVS(JX))
!
    RETURN
!
    END FUNCTION FPVS
!
!
!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief COMPUTE SATURATION VAPOR PRESSURE
!> @details COMPUTE SATURATION VAPOR PRESSURE FROM THE TEMPERATURE.
!! A LINEAR INTERPOLATION IS DONE BETWEEN VALUES IN A LOOKUP TABLE COMPUTED IN GPVS. 
!! SEE DOCUMENTATION FOR FPVSX FOR DETAILS.
!! INPUT VALUES OUTSIDE TABLE RANGE ARE RESET TO TABLE EXTREMA.
!! THE INTERPOLATION ACCURACY IS ALMOST 6 DECIMAL PLACES.
!! ON THE CRAY, FPVS IS ABOUT 4 TIMES FASTER THAN EXACT CALCULATION.
!! THIS FUNCTION SHOULD BE EXPANDED INLINE IN THE CALLING ROUTINE
!> @author ORIGINATOR - N. PHILLIPS
!> @date 82-12-30 \n
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
!> @param[in] T - REAL TEMPERATURE IN KELVIN
!> @details <b>Use Module:</b>
!! @arg @c COMPVS0
!! @arg @c F77KINDS
!> @details <b>Driver:</b> 
!! @arg @c GSMCONST
!> @details <b>Calls:</b>
!--------------------------------------------------------------------------------------------------
    FUNCTION FPVS0(T)
!--------------------------------------------------------------------------------------------------
    USE COMPVS0
    USE F77KINDS
!
    IMPLICIT NONE
!------------------------
! IMPLICIT NONE VARIABLES
!------------------------
    REAL   (KIND=R4)                                                      , INTENT(IN)          ::&
    & T
!
    REAL   (KIND=R4)                                                                            ::&
    & FPVS0, XJ1
!
    INTEGER(KIND=I4)                                                                            ::&
    & JX1
!
    XJ1 = MIN(MAX(C1XPVS0 + C2XPVS0 * T, 1.), FLOAT(NX))
    JX1 = MIN(XJ1, NX - 1.)
!
    FPVS0 = TBPVS0(JX1) + (XJ1 - JX1) * (TBPVS0(JX1 + 1) - TBPVS0(JX1))
!
    RETURN
!
    END FUNCTION FPVS0
!
!
!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief COMPUTE SATURATION VAPOR PRESSURE
!> @details EXACTLY COMPUTE SATURATION VAPOR PRESSURE FROM TEMPERATURE.
!! THE WATER MODEL ASSUMES A PERFECT GAS, CONSTANT SPECIFIC HEATS FOR GAS AND LIQUID, AND NEGLECTS 
!! THE VOLUME OF THE LIQUID.
!! THE MODEL DOES ACCOUNT FOR THE VARIATION OF THE LATENT HEAT OF CONDENSATION WITH TEMPERATURE. 
!! THE ICE OPTION IS NOT INCLUDED.
!! THE CLAUSIUS-CLAPEYRON EQUATION IS INTEGRATED FROM THE TRIPLE POINT TO GET THE FORMULA 
!! PVS = PSATK * (TR ** XA) * EXP(XB * (1. - TR))
!! WHERE TR IS TTP/T AND OTHER VALUES ARE PHYSICAL CONSTANTS THIS FUNCTION SHOULD BE EXPANDED INLINE
!! IN THE CALLING ROUTINE.
!! REFERENCE: EMANUEL (1994), 116 - 117.
!> @author ORIGINATOR - N. PHILLIPS
!> @date 82-12-30 \n
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
!> @param[in] T - REAL TEMPERATURE IN KELVIN
!> @details <b>Use Module:</b>
!! @arg @c F77KINDS
!> @details <b>Driver:</b> 
!! @arg @c GSMCONST
!> @details <b>Calls:</b>
!--------------------------------------------------------------------------------------------------
    FUNCTION FPVSX(T)
!--------------------------------------------------------------------------------------------------
! FUNCTION FPVSX
! 
! SUBPROGRAM: FPVSX - COMPUTE SATURATION VAPOR PRESSURE
! PROGRAMMER: N. PHILLIPS
! ORG: W/NP2
! DATE: 82-12-30
!
! ABSTRACT: EXACTLY COMPUTE SATURATION VAPOR PRESSURE FROM TEMPERATURE.
! THE WATER MODEL ASSUMES A PERFECT GAS, CONSTANT SPECIFIC HEATS FOR GAS AND LIQUID, AND NEGLECTS 
! THE VOLUME OF THE LIQUID.
! THE MODEL DOES ACCOUNT FOR THE VARIATION OF THE LATENT HEAT OF CONDENSATION WITH TEMPERATURE. 
! THE ICE OPTION IS NOT INCLUDED.
! THE CLAUSIUS-CLAPEYRON EQUATION IS INTEGRATED FROM THE TRIPLE POINT TO GET THE FORMULA 
! PVS = PSATK * (TR ** XA) * EXP(XB * (1. - TR))
! WHERE TR IS TTP/T AND OTHER VALUES ARE PHYSICAL CONSTANTS THIS FUNCTION SHOULD BE EXPANDED INLINE
! IN THE CALLING ROUTINE.
! REFERENCE: EMANUEL (1994), 116 - 117.
!
! PROGRAM HISTORY LOG:
! 91-05-07  IREDELL - MADE INTO INLINABLE FUNCTION
! 94-12-30  IREDELL - EXACT COMPUTATION
! 96-02-19  HONG    - ICE EFFECT
! 18-01-15  LUCCI   - MODERNIZATION OF THE CODE, INCLUDING:
!                     * F77 TO F90/F95
!                     * INDENTATION & UNIFORMIZATION CODE
!                     * REPLACEMENT OF COMMONS BLOCK FOR MODULES
!                     * DOCUMENTATION WITH DOXYGEN
!                     * OPENMP FUNCTIONALITY
!
! INPUT ARGUMENT LIST:
! T     - REAL TEMPERATURE IN KELVIN
!
! OUTPUT ARGUMENT LIST:
! FPVSX - REAL SATURATION VAPOR PRESSURE IN KILOPASCALS (CB)
!
! INPUT/OUTPUT ARGUMENT LIST:
! NONE
!
! USE MODULES: F77KINDS
!  
! DRIVER     : GSMCONST
! 
! CALLS      : -----
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE 
!
    REAL   (KIND=R4)    , PARAMETER :: CP    =   1.0046E+3
    REAL   (KIND=R4)    , PARAMETER :: RD    = 287.04   
    REAL   (KIND=R4)    , PARAMETER :: RV    =   4.6150E+2
    REAL   (KIND=R4)    , PARAMETER :: TTP   =   2.7316E+2
    REAL   (KIND=R4)    , PARAMETER :: HVAP  =   2.5000E+6
    REAL   (KIND=R4)    , PARAMETER :: PSAT  =   6.1078E+2
    REAL   (KIND=R4)    , PARAMETER :: CLIQ  =   4.1855E+3
    REAL   (KIND=R4)    , PARAMETER :: CVAP  =   1.8460E+3
    REAL   (KIND=R4)    , PARAMETER :: CICE  =   2.1060E+3
    REAL   (KIND=R4)    , PARAMETER :: HSUB  =   2.8340E+6
!
    REAL   (KIND=R4)    , PARAMETER :: PSATK = PSAT * 1.E-3
    REAL   (KIND=R4)    , PARAMETER :: DLDT  = CVAP - CLIQ
    REAL   (KIND=R4)    , PARAMETER :: XA    = - DLDT  / RV
    REAL   (KIND=R4)    , PARAMETER :: XB    = XA  + HVAP / (RV*TTP)
    REAL   (KIND=R4)    , PARAMETER :: DLDTI = CVAP - CICE
    REAL   (KIND=R4)    , PARAMETER :: XAI   = - DLDTI / RV
    REAL   (KIND=R4)    , PARAMETER :: XBI   = XAI + HSUB / (RV*TTP)
!------------------------
! IMPLICIT NONE VARIABLES
!------------------------
    REAL   (KIND=R4)                                                      , INTENT(IN)          ::&
    & T
!
    REAL   (KIND=R4)                                                                            ::&
    & FPVSX   , TR
!
    TR = TTP / T
!
    IF (T >= TTP) THEN
        FPVSX = PSATK * (TR ** XA)  * EXP(XB  * (1. - TR))
    ELSE
        FPVSX = PSATK * (TR ** XAI) * EXP(XBI * (1. - TR))
    END IF
!
    RETURN
!
    END FUNCTION FPVSX
!
!
!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief COMPUTE SATURATION VAPOR PRESSURE
!> @details EXACTLY COMPUTE SATURATION VAPOR PRESSURE FROM TEMPERATURE.
!! THE WATER MODEL ASSUMES A PERFECT GAS, CONSTANT SPECIFIC HEATS FOR GAS AND LIQUID, AND NEGLECTS 
!! THE VOLUME OF THE LIQUID.
!! THE MODEL DOES ACCOUNT FOR THE VARIATION OF THE LATENT HEAT OF CONDENSATION WITH TEMPERATURE. 
!! THE ICE OPTION IS NOT INCLUDED.
!! THE CLAUSIUS-CLAPEYRON EQUATION IS INTEGRATED FROM THE TRIPLE POINT TO GET THE FORMULA 
!! PVS = PSATK * (TR ** XA) * EXP(XB * (1. - TR))
!! WHERE TR IS TTP/T AND OTHER VALUES ARE PHYSICAL CONSTANTS THIS FUNCTION SHOULD BE EXPANDED INLINE
!! IN THE CALLING ROUTINE.
!! REFERENCE: EMANUEL (1994), 116 - 117.
!> @author ORIGINATOR - N. PHILLIPS
!> @date 82-12-30 \n
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
!> @param[in] T - REAL TEMPERATURE IN KELVIN
!> @details <b>Use Module:</b>
!! @arg @c F77KINDS
!> @details <b>Driver:</b> 
!! @arg @c GSMCONST
!> @details <b>Calls:</b>
!--------------------------------------------------------------------------------------------------
    FUNCTION FPVSX0(T)
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    REAL   (KIND=R4)    , PARAMETER :: CP    =   1.0046E+3
    REAL   (KIND=R4)    , PARAMETER :: RD    = 287.04   
    REAL   (KIND=R4)    , PARAMETER :: RV    =   4.6150E+2
    REAL   (KIND=R4)    , PARAMETER :: TTP   =   2.7316E+2
    REAL   (KIND=R4)    , PARAMETER :: HVAP  =   2.5000E+6
    REAL   (KIND=R4)    , PARAMETER :: PSAT  =   6.1078E+2
    REAL   (KIND=R4)    , PARAMETER :: CLIQ  =   4.1855E+3
    REAL   (KIND=R4)    , PARAMETER :: CVAP  =   1.8460E+3
    REAL   (KIND=R4)    , PARAMETER :: CICE  =   2.1060E+3
    REAL   (KIND=R4)    , PARAMETER :: HSUB  =   2.8340E+6
!
    REAL   (KIND=R4)    , PARAMETER :: PSATK  = PSAT  * 1.E-3
    REAL   (KIND=R4)    , PARAMETER :: DLDT   = CVAP  - CLIQ
    REAL   (KIND=R4)    , PARAMETER :: XA     = -DLDT / RV
    REAL   (KIND=R4)    , PARAMETER :: XB     = XA    + HVAP / (RV * TTP)
    REAL   (KIND=R4)    , PARAMETER :: DLDTI  = CVAP  - CICE
    REAL   (KIND=R4)    , PARAMETER :: XAI    = -DLDT / RV
    REAL   (KIND=R4)    , PARAMETER :: XBI    = XA    + HSUB / (RV * TTP)
!------------------------
! IMPLICIT NONE VARIABLES
!------------------------
    REAL   (KIND=R4)                                                      , INTENT(IN)          ::&
    & T
!
    REAL   (KIND=R4)                                                                            ::&
    & FPVSX0  , TR 
!
    TR = TTP / T
    FPVSX0 = PSATK * (TR ** XA) * EXP(XB * (1. - TR))
!
    RETURN
!
    END FUNCTION FPVSX0
