!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief COMPUTE THE SOLAR ZENITH ANGLE
!> @details CALCULATES THE COSINE OF THE SOLAR ZENITH ANGLES AT EACH POINT FOR USE IN SWRAD.
!> @author ORIGINATOR - BLACK
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
!> @param[in] TIMES - THE FORECAST TIME IN SECONDS
!> @param[out] DAYI - THE DAY OF THE YEAR
!> @param[out] HOUR - THE HOUR OF THE DAY
!> @details <b>Use Module:</b>
!! @arg @c CTLBLK
!! @arg @c F77KINDS
!! @arg @c MPPSTAFF
!! @arg @c PARMETA
!! @arg @c PHYS
!> @details <b>Driver:</b> 
!! @arg @c INIT
!! @arg @c RADTN
!! @arg @c RDTEMP
!> @details <b>Calls:</b>
!--------------------------------------------------------------------------------------------------      
    SUBROUTINE ZENITH(TIMES, DAYI, HOUR)
!--------------------------------------------------------------------------------------------------
! SUBROUTINE ZENITH
!
! SUBPROGRAM: ZENITH - COMPUTE THE SOLAR ZENITH ANGLE
! PROGRAMMER: BLACK   
! ORG: W/NMC2 
! DATE: 93-10-28
!
! ABSTRACT:
! ZENITH CALCULATES THE COSINE OF THE SOLAR ZENITH ANGLES AT EACH POINT FOR USE IN SWRAD
!
! PROGRAM HISTORY LOG:
! 93-10-28  ?????      - ORIGINATOR
! 18-01-15  LUCCI      - MODERNIZATION OF THE CODE, INCLUDING:
!                        * F77 TO F90/F95
!                        * INDENTATION & UNIFORMIZATION CODE
!                        * REPLACEMENT OF COMMONS BLOCK FOR MODULES
!                        * DOCUMENTATION WITH DOXYGEN
!                        * OPENMP FUNCTIONALITY
!
! INPUT ARGUMENT LIST:
! TIMES  - THE FORECAST TIME IN SECONDS
!
! OUTPUT ARGUMENT LIST:
! DAYI   - THE DAY OF THE YEAR
! HOUR   - THE HOUR OF THE DAY
!
! INPUT/OUTPUT ARGUMENT LIST:
! NONE
!
! USE MODULES: CTLBLK
!              F77KINDS
!              MPPSTAFF
!              PARMETA
!              PHYS
!
! DRIVER     : INIT
!              RADTN
!              RDTEMP
!
! CALLS      : -----
!--------------------------------------------------------------------------------------------------
    USE CTLBLK
    USE F77KINDS
    USE MPPSTAFF
    USE PARMETA
    USE PHYS
!
    IMPLICIT NONE
!
    REAL   (KIND=R4)    , PARAMETER :: GSTC1  =   24110.54841
    REAL   (KIND=R4)    , PARAMETER :: GSTC2  = 8640184.812866 
    REAL   (KIND=R4)    , PARAMETER :: GSTC3  =       9.3104E-2 
    REAL   (KIND=R4)    , PARAMETER :: GSTC4  =      -6.2E-6
    REAL   (KIND=R4)    , PARAMETER :: PI     =       3.1415926 
    REAL   (KIND=R4)    , PARAMETER :: PI2    =       2.    * PI
    REAL   (KIND=R4)    , PARAMETER :: PIH    =       0.5   * PI
    REAL   (KIND=R4)    , PARAMETER :: DEG2RD =       1.745329E-2
    REAL   (KIND=R4)    , PARAMETER :: OBLIQ  =      23.440 * DEG2RD
    REAL   (KIND=R4)    , PARAMETER :: ZEROJD = 2451545.0
!
    REAL   (KIND=R4)                                                                            ::&
    & ADDDAY  , ANOM    , DATJ0   , DATJUL  , DAY     , DAYI    , DEC     , DIFJD   , HOUR    ,   &
    & HRANG   , HRLCL   , RA      , SIDTIM  , SINALT  , SLON    , SLONM   , STARTYR , STIM0   ,   &
    & TU      , YFCTR   , TIMES
!
    INTEGER(KIND=I4)                                                                            ::&
    & I       , J       , IDIFYR  , KMNTH   , KNT
!
    LOGICAL(KIND=L4)                                                                            ::&
    & LEAP  
!
    INTEGER(KIND=I4)    , DIMENSION(12)                                                         ::&
    & MONTH
!  
    DATA  MONTH /31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/
!
    SAVE MONTH
!
    DAY  = 0.
    LEAP = .FALSE.
!
    IF (MOD(IDAT(3),4) == 0) THEN
        MONTH(2) = 29
        LEAP     = .TRUE.
    END IF
!
    IF (IDAT(1) > 1) THEN
        KMNTH = IDAT(1) - 1
        DO 10 KNT=1,KMNTH
            DAY = DAY + REAL(MONTH(KNT))
     10 CONTINUE
    END IF
!-----------------------------------------------------------------------------------
! CALCULATE EXACT NUMBER OF DAYS FROM BEGINNING OF YEAR TO FORECAST TIME OF INTEREST 
!-----------------------------------------------------------------------------------
    DAY   = DAY + REAL(IDAT(2) - 1) + (REAL(IHRST) + TIMES / 3600.) / 24.
    DAYI  = REAL(INT(DAY) + 1)
    HOUR  = (DAY - DAYI + 1.) * 24.
    YFCTR = 2000. - IDAT(3)
!------------------------------------------------------------------------------------
! FIND CELESTIAL LONGITUDE OF THE SUN THEN THE SOLAR DECLINATION AND RIGHT ASCENSION.
!------------------------------------------------------------------------------------
    IDIFYR = IDAT(3) - 2000
!-----------------------------------------------------------------------------
! FIND JULIAN DATE OF START OF THE RELEVANT YEAR ADDING IN LEAP DAYS AS NEEDED
!-----------------------------------------------------------------------------
    IF (IDIFYR < 0) THEN
        ADDDAY = REAL(IDIFYR / 4)
    ELSE
        ADDDAY = REAL((IDIFYR + 3) / 4)
    END IF
!
    STARTYR = ZEROJD + IDIFYR * 365. + ADDDAY - 0.5
!----------------------------------------
! THE JULIAN DATE OF THE TIME IN QUESTION
!----------------------------------------
    DATJUL = STARTYR + DAY
!------------------------------------------------------------------------
! DIFFERENCE OF ACTUAL JULIAN DATE FROM JULIAN DATE AT 00H 1 JANUARY 2000
!------------------------------------------------------------------------
    DIFJD = DATJUL - ZEROJD
!------------------------------------
! MEAN GEOMETRIC LONGITUDE OF THE SUN
!------------------------------------
    SLONM = (280.460 + 0.9856474 * DIFJD) * DEG2RD + YFCTR * PI2
!-----------------
! THE MEAN ANOMOLY
!-----------------
    ANOM  = (357.528 + 0.9856003 * DIFJD) * DEG2RD
!----------------------------------------
! APPARENT GEOMETRIC LONGITUDE OF THE SUN
!----------------------------------------
    SLON = SLONM + (1.915 * SIN(ANOM) + 0.020 * SIN(2. * ANOM)) * DEG2RD
!
    IF (SLON > PI2) SLON = SLON - PI2
!--------------------------------
! DECLINATION AND RIGHT ASCENSION
!-------------------------------- 
    DEC = ASIN(SIN(SLON) * SIN(OBLIQ))
     RA = ACOS(COS(SLON) / COS(DEC)  )
!
    IF (SLON > PI) RA = PI2 - RA
!------------------------------------------------------------------
! FIND THE GREENWICH SIDEREAL TIME THEN THE LOCAL SOLAR HOUR ANGLE.
!------------------------------------------------------------------
    DATJ0  = STARTYR + DAYI - 1.
    TU     = (DATJ0 - 2451545.) / 36525.
    STIM0  = GSTC1 + GSTC2 * TU + GSTC3 * TU ** 2 + GSTC4 * TU ** 3
    SIDTIM = STIM0 / 3600. + YFCTR * 24. + 1.00273791 * HOUR
    SIDTIM = SIDTIM * 15. * DEG2RD
!
    IF (SIDTIM < 0. ) SIDTIM = SIDTIM + PI2
    IF (SIDTIM > PI2) SIDTIM = SIDTIM - PI2
!
    HRANG = SIDTIM - RA
!
    DO 100 J=1,JM
        DO 100 I=1,IM
            HRLCL = HRANG + GLON(I,J)
!--------------------------------------------------------------------------------------------------
! THE ZENITH ANGLE IS THE COMPLEMENT OF THE ALTITUDE THUS THE COSINE OF THE ZENITH ANGLE EQUALS THE
! SINE OF THE ALTITUDE.
!--------------------------------------------------------------------------------------------------
    SINALT = SIN(DEC) * SIN(GLAT(I,J)) + COS(DEC) * COS(HRLCL) * COS(GLAT(I,J))
!
    IF (SINALT < 0.) SINALT = 0.
!
    CZEN(I,J) = SINALT
!
100 CONTINUE
!--------------------------------------------------------------------------------------------------
! IF THE FORECAST IS IN A DIFFERENT YEAR THAN THE START TIME, RESET DAYI TO THE PROPER DAY OF THE 
! NEW YEAR (IT MUST NOT BE RESET BEFORE THE SOLAR ZENITH ANGLE IS COMPUTED).
!--------------------------------------------------------------------------------------------------
    IF (DAYI > 365.) THEN
        IF (.NOT. LEAP) THEN
            DAYI = DAYI - 365.
        ELSE IF (LEAP .AND. DAYI > 366.) THEN
            DAYI = DAYI - 366.
        END IF
    END IF
!
    RETURN
! 
    END SUBROUTINE ZENITH
