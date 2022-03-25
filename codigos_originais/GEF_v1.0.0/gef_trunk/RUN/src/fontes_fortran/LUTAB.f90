!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief THIS SUBROUTINE IS A LOOKUP TABLE
!> @details THIS SUBROUTINE IS A LOOKUP TABLE. 
!! GIVEN A SERIES OF SERIES OF SATURATION EQUIVALENT POTENTIAL TEMPERATURES, THE TEMPERATURE IS 
!! CALCULATED.
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
!! @arg @c F77KINDS
!! @arg @c KFLUT
!> @details <b>Driver:</b> 
!! @arg @c INIT
!> @details <b>Calls:</b>
!--------------------------------------------------------------------------------------------------       
    SUBROUTINE LUTAB
!--------------------------------------------------------------------------------------------------
! SUBROUTINE LUTAB
! 
! SUBPROGRAM: LUTAB - THIS SUBROUTINE IS A LOOKUP TABLE
! PROGRAMMER: ????? 
! ORG: W/NP22
! DATE: ??-??-??
!
! ABSTRACT:  
! THIS SUBROUTINE IS A LOOKUP TABLE. 
! GIVEN A SERIES OF SERIES OF SATURATION EQUIVALENT POTENTIAL TEMPERATURES, THE TEMPERATURE IS 
! CALCULATED.
!
! PROGRAM HISTORY LOG:
! ??-??-??  ?????  - ORIGINATOR
! 18-03-20  LUCCI  - MODERNIZATION OF THE CODE, INCLUDING:
!                    * F77 TO F90/F95
!                    * INDENTATION & UNIFORMIZATION CODE
!                    * REPLACEMENT OF COMMONS BLOCK FOR MODULES
!                    * DOCUMENTATION WITH DOXYGEN
!                    * OPENMP FUNCTIONALITY 
!
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
! INPUT FILES:
! NONE
!
! OUTPUT FILES:
! NONE
!
! USE MODULES: F77KINDS
!              KFLUT
!
! DRIVER     : INIT
!
! CALLS      : -----           
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
    USE KFLUT
!------------------------------------------- 
! EQUIVALENT POTENTIAL TEMPERATURE INCREMENT
!-------------------------------------------
    DATA DTH   /   1.    /
!---------------------- 
! MINIMUM STARTING TEMP
!----------------------
    DATA TMIN  / 150.    /
!-------------------------------------- 
! TOLERANCE FOR ACCURACY OF TEMPERATURE
!--------------------------------------
    DATA TOLER /   0.001 /
!-----------------------
! TOP PRESSURE (PASCALS)
!-----------------------
!    PTOP =   5000.0
    PTOP =   2500.0
!-------------------------- 
! BOTTOM PRESSURE (PASCALS)
!--------------------------
    PBOT = 110000.0
!---------------------------------------------------------------------------------
! DEFINE CONSTANTS FOR CALCULATION OF SATURATION VAPOR PRESSURE ACCORDING TO BUCK.
! (J. APPL. METEO., DECEMBER, 1981)
!---------------------------------------------------------------------------------
    ALIQ =  613.300
    BLIQ =   17.502
    CLIQ = 4780.800
    DLIQ =   32.190
!------------------- 
! COMPUTE PARAMETERS
!------------------- 
!
!-----------------------------------
! 1. / (SAT. EQUIV. THETA INCREMENT)
!-----------------------------------
    RDTHK = 1. / DTH
!------------------- 
! PRESSURE INCREMENT
!-------------------
    DPR = (PBOT - PTOP) / FLOAT(KFNP - 1)
!-------------------------- 
! 1. / (PRESSURE INCREMENT)
!--------------------------
    RDPR = 1. / DPR
!----------------------------------------- 
! CALCULATE THE STARTING SAT. EQUIV. THETA
!-----------------------------------------
    TEMP = TMIN
    P    = PTOP - DPR
!
    DO KP=1,KFNP
        P  = P + DPR
        ES = ALIQ  * EXP((BLIQ * TEMP - CLIQ) / (TEMP - DLIQ))
        QS = 0.622 * ES / (P - ES)
        PI = (1.E5 / P) ** (0.2854 * (1.-0.28 * QS))
!
        THE0K(KP) = TEMP * PI * EXP((3374.6525 / TEMP - 2.5403) * QS * (1. + 0.81 * QS))
!
    END DO
!----------------------------------------------------------
! COMPUTE TEMPERATURES FOR EACH SAT. EQUIV. POTENTIAL TEMP.
!----------------------------------------------------------
    P = PTOP - DPR
!
    DO 60 KP=1,KFNP
        THES = THE0K(KP) - DTH
        P    = P + DPR
!
        DO 50 IT=1,KFNT
!------------------------------
! DEFINE SAT. EQUIV. POT. TEMP.
!------------------------------
            THES = THES + DTH
!---------------------------- 
! ITERATE TO FIND TEMPERATURE
! FIND INITIAL GUESS
!----------------------------
            IF (IT == 1) THEN
                TGUES = TMIN
            ELSE
                TGUES = TTAB(IT-1,KP)
            END IF
!
            ES = ALIQ  * EXP((BLIQ * TGUES - CLIQ) / (TGUES - DLIQ))
            QS = 0.622 * ES / (P - ES)
            PI = (1.E5 / P) ** (0.2854 * (1. - 0.28 * QS))
!
            THGUES = TGUES  * PI * EXP((3374.6525 / TGUES - 2.5403) * QS * (1. + 0.81 * QS))
            F0     = THGUES - THES
            T1     = TGUES  - 0.5 * F0
            T0     = TGUES
            ITCNT  = 0
!--------------
!ITERATION LOOP
!--------------
         30 CONTINUE
!
            ES = ALIQ  * EXP((BLIQ * T1 - CLIQ) / (T1 - DLIQ))
            QS = 0.622 * ES / (P - ES)
            PI = (1.E5 / P) ** (0.2854 * (1. - 0.28 * QS))
!
            THTGS = T1 * PI * EXP((3374.6525 / T1 - 2.5403) * QS * (1. + 0.81 * QS))
            F1    = THTGS - THES
!
            IF (ABS(F1) < TOLER) GOTO 40
!
            ITCNT = ITCNT + 1
!
            IF (ITCNT > 10) THEN
                PRINT*,' ITCNT > 10',' IT=',IT,' P=',P,' T1=',T1,' THES=',THES
                GOTO 40
            END IF
!
            DT = F1 * (T1 - T0) / (F1 - F0)
            T0 = T1
            F0 = F1
            T1 = T1 - DT
!
            GOTO 30
!
         40 CONTINUE
!
             TTAB(IT,KP) = T1
            QSTAB(IT,KP) = QS
     50 CONTINUE
 60 CONTINUE
!-------------------------------------- 
! LOOKUP TABLE FOR TLOG(EMIX/ALIQ)
!
!SET UP INTIAL VALUES FOR LOOKUP TABLES
!--------------------------------------
    ASTRT = 1.E-3
    AINC  = 0.075
!
    A1    = ASTRT - AINC
!
    DO I=1,200
        A1     = A1 + AINC
        ALU(I) = ALOG(A1)
    END DO
!
    RETURN
!
    END SUBROUTINE LUTAB
