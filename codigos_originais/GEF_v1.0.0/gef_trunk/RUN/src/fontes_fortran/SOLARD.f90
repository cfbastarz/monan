!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief  COMPUTE THE SOLAR-EARTH DISTANCE
!> @details CALCULATES THE SOLAR-EARTH DISTANCE ON EACH DAY FOR USE IN SHORT-WAVE RADIATION.
!> @author ORIGINATOR - Q.ZHAO
!> @date 96-07-23 \n
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
!> @param[out] R1  - THE NON-DIMENSIONAL DISTANCE BETWEEN SUN AND THE EARTH (LESS THAN 1.0 IN 
!! SUMMER AND LARGERTHAN 1.0 IN WINTER).
!> @details <b>Use Module:</b>
!! @arg @c CTLBLK
!! @arg @c F77KINDS
!> @details <b>Driver:</b> 
!! @arg @c INIT
!> @details <b>Calls:</b>
!--------------------------------------------------------------------------------------------------     
    SUBROUTINE SOLARD(R1)
!--------------------------------------------------------------------------------------------------
! SUBROUTINE SOLARD
!
! SUBPROGRAM: SOLARD - COMPUTE THE SOLAR-EARTH DISTANCE
! PROGRAMMER: Q.ZHAO
! ORG: W/NMC2
! DATE: 96-07-23
!
! ABSTRACT:
! SOLARD CALCULATES THE SOLAR-EARTH DISTANCE ON EACH DAY FOR USE IN SHORT-WAVE RADIATION.
!
! PROGRAM HISTORY LOG:
! 96-07-23  Q.ZHAO      - ORIGINATOR
! 98-10-09  Q.ZHAO      - CHANGED TO USE IW3JDN IN W3LIB TO CALCULATE JD.
! 18-01-15  LUCCI       - MODERNIZATION OF THE CODE, INCLUDING:
!>                         * F77 TO F90/F95
!                         * INDENTATION & UNIFORMIZATION CODE
!                         * REPLACEMENT OF COMMONS BLOCK FOR MODULES
!                         * DOCUMENTATION WITH DOXYGEN
!                         * OPENMP FUNCTIONALITY
!
! INPUT ARGUMENT LIST:
! NONE
!
! OUTPUT ARGUMENT LIST:
! R1 - THE NON-DIMENSIONAL DISTANCE BETWEEN SUN AND THE EARTH (LESS THAN 1.0 IN SUMMER AND LARGER
!      THAN 1.0 IN WINTER).
!
! INPUT/OUTPUT ARGUMENT LIST:
! NONE
!
! USE MODULES: CTLBLK
!              F77KINDS
!
! DRIVER     : INIT
! 
! CALLS      : -----
!--------------------------------------------------------------------------------------------------
    USE CTLBLK
    USE F77KINDS
!
    IMPLICIT NONE
!
    REAL   (KIND=R4)    , PARAMETER :: PI  = 3.1415926
    REAL   (KIND=R4)    , PARAMETER :: PI2 = 2. * PI
!
    LOGICAL(KIND=L4)                                                                            ::&
    & LEAP
!
    INTEGER(KIND=I4)    , DIMENSION(12)                                                         ::&
    & NDM
!------------------------
! IMPLICIT NONE VARIABLES
!------------------------
    INTEGER(KIND=I4)                                                                            ::&
    & ITER    , JD      , JDAY    , JHR     , JMNTH   , JYR     , JYR19   , JMN     , JDOR1   ,   &
    & JDOR2   , IW3JDN
!
    REAL   (KIND=R4)                                                                            ::&
    & CCR     , TPP     , DAYINC  , DAT     , T       , YEAR    , EC      , DATE    , FJD     ,   &
    & EM      , E       , EP      , CR      
!
    REAL   (KIND=R4)                                                      , INTENT(OUT)         ::&
    & R1
!
!
    DATA JYR19 / 1900 /
    DATA JMN   /    0 /
    DATA CCR   /1.3E-6/
!
    DATA NDM   /0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334/
!
!------------------------------------------------------------
! TPP   = DAYS BETWEEN EPOCH AND PERIHELION PASSAGE OF 1900
! JDOR1 = JD OF DECEMBER 30, 1899 AT 12 HOURS UT
! JDOR2 = JD OF EPOCH WHICH IS JANUARY 0, 1990 AT 12 HOURS UT
!------------------------------------------------------------
    DATA TPP   / 1.55  /
    DATA JDOR1 /2415019/
    DATA JDOR2 /2415020/
!--------------------------------------------------------------------------------------------------
! COMPUTES JULIAN DAY AND FRACTION FROM YEAR, MONTH, DAY AND TIME UT ACCURATE ONLY BETWEEN MARCH 1,
! 1900 AND FEBRUARY 28, 2100 BASED ON JULIAN CALENDAR CORRECTED TO CORRESPOND TO GREGORIAN CALENDAR
! DURING THIS PERIOD
!--------------------------------------------------------------------------------------------------
    JYR   = IDAT(3)
    JMNTH = IDAT(1)
    JDAY  = IDAT(2)
    JHR   = IHRST
!
    JD = IW3JDN(JYR, JMNTH, JDAY)
!
    IF (JHR >= 12) THEN
        JD  = JD   - 1
        FJD = .5E0 + .041666667E0 * FLOAT(JHR   ) + .00069444444E0 * FLOAT(JMN)
    ELSE
        FJD =        .041666667E0 * FLOAT(JHR-12) + .00069444444E0 * FLOAT(JMN)
    END IF
!
    DAYINC = JHR / 24.0
    JD     = JD  + FJD + DAYINC
    FJD    = JD  + FJD + DAYINC - JD
!----------------------------------- 
! CALCULATE THE SOLAR-EARTH DISTANCE
!----------------------------------- 
    DAT = FLOAT(JD - JDOR2) - TPP + FJD
!---------------------------------------------- 
! COMPUTES TIME IN JULIAN CENTURIES AFTER EPOCH
!---------------------------------------------- 
    T   = FLOAT(JD - JDOR2) / 36525.E0
!-------------------------------------------------------------------
! COMPUTES LENGTH OF ANOMALISTIC AND TROPICAL YEARS (MINUS 365 DAYS)
!-------------------------------------------------------------------
    YEAR = .25964134E0 + .304E-5 * T
!------------------------------------ 
! COMPUTES ORBIT ECCENTRICITY  FROM T
!------------------------------------ 
    EC   = .01675104E0 - (.418E-4 + .126E-6 * T) * T
    YEAR = YEAR + 365.E0
!------------------------------------------ 
! DATE = DAYS SINCE LAST PERIHELION PASSAGE
!------------------------------------------ 
    DATE = MOD(DAT, YEAR)
!---------------------------------------- 
! SOLVE ORBIT EQUATIONS BY NEWTONS METHOD
!---------------------------------------- 
    EM = PI2 * DATE / YEAR
    E = 1.E0
    ITER = 0
 31 EP = E - (E - EC * SIN(E) - EM) / (1.E0 - EC * COS(E))
    CR = ABS(E - EP)
    E = EP
    ITER = ITER + 1
!
    IF (ITER >  10)  GOTO 931
    IF (CR   > CCR)  GOTO  31
!
931 CONTINUE
!
    R1 = 1.E0 - EC * COS(E)
!
!    WRITE(6,999) JYR, JMNTH, JDAY, JHR, R1
!
999 FORMAT('SUN-EARTH DISTANCE CALCULATION FINISHED IN SOLARD'/'YEAR=',I5,'  MONTH=',I3,         &
    &      '  DAY=',I3,' HOUR=',I3,' R1=',F9.4)
!---------------- 
! RETURN TO RADTN
!----------------
    RETURN
!
    END SUBROUTINE SOLARD 
