!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Inserir Brief de TPMIX2
!> @details Inserir Details de TPMIX2
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
!> @param[in] P      -
!> @param[in] THES   -
!> @param[in] RATIO2 -
!> @param[in] XLV1   - 
!> @param[in] XLV0   -
!> @param[out] TU     - 
!> @param[out] QNEWLQ -
!> @param[out] QNEWIC -
!> @param[inout] QU     -
!> @param[inout] QLIQ   -
!> @param[inout] QICE   -
!> @details <b>Use Module:</b>
!! @arg @c F77KINDS
!! @arg @c KFLUT
!> @details <b>Driver:</b> 
!! @arg @c KFPARA
!> @details <b>Calls:</b>
!--------------------------------------------------------------------------------------------------     
	SUBROUTINE TPMIX2(P, THES, TU, QU, QLIQ, QICE, QNEWLQ, QNEWIC, RATIO2, XLV1, XLV0)
!--------------------------------------------------------------------------------------------------
! SUBROUTINE TPMIX2
!
! SUBPROGRAM: TPMIX2 - ?????
! PROGRAMMER: ?????   
! ORG: ?????
! DATE: ??-??-??
! 
! ABSTRACT:
! ?????
!
! PROGRAM HISTORY LOG:
! ??-??-??  ?????       - ORIGINATOR
! 18-01-15  LUCCI       - MODERNIZATION OF THE CODE, INCLUDING:
!                         * F77 TO F90/F95
!                         * INDENTATION & UNIFORMIZATION CODE
!                         * REPLACEMENT OF COMMONS BLOCK FOR MODULES
!                         * DOCUMENTATION WITH DOXYGEN
!                         * OPENMP FUNCTIONALITY
!
! INPUT ARGUMENT LIST:
! P      -
! THES   -
! RATIO2 -
! XLV1   - 
! XLV0   -
!
! OUTPUT ARGUMENT LIST:
! TU     - 
! QNEWLQ -
! QNEWIC -
!
! INPUT/OUTPUT ARGUMENT LIST:
! QU     -
! QLIQ   -
! QICE   -
!
! USE MODULES: F77KINDS
!              KFLUT
!
! DRIVER     : KFPARA
!
! CALLS      : -----
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
    USE KFLUT
!
    IMPLICIT NONE
!
    REAL   (KIND=R4)                                                      , INTENT(IN)          ::&
    & P       , THES    , RATIO2  , XLV1    , XLV0
!
    REAL   (KIND=R4)                                                      , INTENT(OUT)         ::&
    & TU      , QNEWLQ  , QNEWIC
!
    REAL   (KIND=R4)                                                      , INTENT(INOUT)       ::&
    & QU      , QLIQ    , QICE     
! 
    REAL   (KIND=R4)                                                                            ::&
    & TP      , QQ      , BTH     , TTH     , PP      , T00     , T10     , T01     , T11     ,   &
    & Q00     , Q10     , Q01     , Q11     , TEMP    , QS      , QNEW    , DQ      , QTOT    ,   &
    & RLL     , CP    
!
    INTEGER(KIND=I4)                                                                            ::&
    & IPTB    , ITHTB
!----------------------------------
! SCALING PRESSURE & TT TABLE INDEX  
!----------------------------------
    TP   = (P - PTOP) * RDPR
    QQ   = TP - AINT(TP)
    IPTB = INT(TP) + 1
!--------------------------------
! BASE AND SCALING FACTOR FOR THE                           
!--------------------------------
!
!-----------------------------
! SCALING THE & TT TABLE INDEX                                        
!-----------------------------
    BTH   = (THE0K(IPTB+1) - THE0K(IPTB)) * QQ + THE0K(IPTB)
    TTH   = (THES - BTH) * RDTHK
    PP    = TTH - AINT(TTH)
    ITHTB = INT(TTH) + 1
!
    T00   =  TTAB(ITHTB  , IPTB  )
    T10   =  TTAB(ITHTB+1, IPTB  )
    T01   =  TTAB(ITHTB  , IPTB+1)
    T11   =  TTAB(ITHTB+1, IPTB+1)
!
    Q00   = QSTAB(ITHTB  , IPTB  )
    Q10   = QSTAB(ITHTB+1, IPTB  )
    Q01   = QSTAB(ITHTB  , IPTB+1)
    Q11   = QSTAB(ITHTB+1, IPTB+1)
!-------------------
! PARCEL TEMPERATURE
!-------------------
    TEMP = (T00 + (T10 - T00) * PP + (T01 - T00) * QQ + (T00 - T10 - T01 + T11) * PP * QQ)
!
    QS   = (Q00 + (Q10 - Q00) * PP + (Q01 - Q00) * QQ + (Q00 - Q10 - Q01 + Q11) * PP * QQ)
!
    IF (QS <= QU) THEN
        QNEW = QU - QS
        QU   = QS
        GOTO 100
    END IF
!------------------------------------------------------------------------------
! IF THE PARCEL IS SUBSATURATED, TEMPERATURE AND MIXING RATIO MUST BE ADJUSTED.
! IF LIQUID WATER IS PRESENT, IT IS ALLOWED TO EVAPORATE.
!------------------------------------------------------------------------------
    QNEW = 0.
    DQ   = QS   - QU
    QTOT = QLIQ + QICE
!--------------------------------------------------------------------------------------------------
! IF THERE IS ENOUGH LIQUID OR ICE TO SATURATE THE PARCEL, TEMP STAYS AT ITS WET BULB VALUE, VAPOR 
! MIXING RATIO IS AT SATURATED LEVEL, AND THE MIXING RATIOS OF LIQUID AND ICE ARE ADJUSTED TO MAKE 
! UP THE ORIGINAL SATURATION  DEFICIT.  OTHERWISE, ANY AVAILABLE LIQ OR ICE VAPORIZES AND 
! APPROPRIATE ADJUSTMENTS TO PARCEL TEMP, VAPOR, LIQUID, AND ICE MIXING RATIOS ARE MADE.
!
! NOTE THAT THE LIQ AND ICE MAY BE PRESENT IN PROPORTIONS SLIGHTLY DIFFERENT THAN SUGGESTED BY THE
! VALUE OF RATIO2. CHECK TO MAKE SURE THAT LIQUID AND ICE CONCENTRATIONS ARE NOT REDUCED TO BELOW 
! ZERO WHEN EVAPORATION/SUBLIMATION OCCURS.
!
! SUBSATURATED VALUES ONLY OCCUR IN CALCULATIONS INVOLVING VARIOUS MIXTURES OF UPDRAFT AND 
! ENVIRONMENTAL AIR FOR ESTIMATION OF ENTRAINMENT AND DETRAINMENT. FOR THESE PURPOSES, ASSUME THAT 
! REASONABLE ESTIMATES CAN BE GIVEN USING LIQUID WATER SATURATION CALCULATIONS ONLY - I.E., IGNORE
! THE EFFECT OF THE ICE PHASE IN THIS PROCESS ONLY.
!--------------------------------------------------------------------------------------------------
    IF (QTOT >= DQ) THEN
        QLIQ = QLIQ - DQ * QLIQ / QTOT
        QICE = QICE - DQ * QICE / QTOT
        QU   = QS
        GOTO 100
    ELSE
        RLL = XLV0   - XLV1 * TEMP
        CP  = 1005.7 * (1.  + 0.89 * QU)
!-----------------------------------------------------------------
! IF NO LIQUID WATER OR ICE IS AVAILABLE, TEMPERATURE IS GIVEN BY:
!-----------------------------------------------------------------
        IF (QTOT < 1.E-10) THEN
            TEMP = TEMP + RLL * (DQ / (1. + DQ)) / CP
            GOTO 100
!--------------------------------------------------------------------------------------------------
! IF SOME LIQ WATER/ICE IS AVAILABLE, BUT NOT ENOUGH TO ACHIEVE SATURATION, THE TEMPERATURE IS 
! GIVEN BY:
!--------------------------------------------------------------------------------------------------
        ELSE
            TEMP = TEMP + RLL * ((DQ - QTOT) / (1 + DQ - QTOT)) / CP
            QU   = QU   + QTOT
            QTOT = 0.
        END IF
!
        QLIQ = 0.
        QICE = 0.
    END IF
!
100 TU     = TEMP
    QNEWLQ = QNEW
    QNEWIC = 0.
!
    RETURN
!
    END SUBROUTINE TPMIX2
