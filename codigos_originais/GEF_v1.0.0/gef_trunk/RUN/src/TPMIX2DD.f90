!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Inserir Brief de TPMIX2DD
!> @details Inserir Details de TPMIX2DD
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
!> @param[out] TS     - 
!> @param[out] QS
!> @details <b>Use Module:</b>
!! @arg @c F77KINDS
!! @arg @c KFLUT
!> @details <b>Driver:</b> 
!! @arg @c KFPARA
!> @details <b>Calls:</b>
!--------------------------------------------------------------------------------------------------     
    SUBROUTINE TPMIX2DD(P, THES, TS, QS)
!--------------------------------------------------------------------------------------------------
! FUNCTION TPMIX2DD
!
! FUNCTION: TPMIX2DD - ?????
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
! P    -
! THES -
!
! OUTPUT ARGUMENT LIST:
! TS   - 
! QS   -
!
! INPUT/OUTPUT ARGUMENT LIST:
! NONE
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
    REAL   (KIND=R4)                                                                            ::&
    & P       , THES    , TS      , QS      , TP      , QQ      , BTH     , TTH     , PP      ,   &
    & T00     , T10     , T01     , T11     , Q00     , Q10     , Q01     , Q11     
!
    INTEGER(KIND=I4)                                                                            ::&
    & IPTB    , ITHTB
!----------------------------------
! SCALING PRESSURE & TT TABLE INDEX  
!----------------------------------
    TP   = (P - PTOP) * RDPR
    QQ   = TP - AINT(TP)
    IPTB =       INT(TP) + 1
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
    TS = (T00 + (T10 - T00) * PP + (T01 - T00) * QQ + (T00 - T10 - T01 + T11) * PP * QQ)
!
    QS = (Q00 + (Q10 - Q00) * PP + (Q01 - Q00) * QQ + (Q00 - Q10 - Q01 + Q11) * PP * QQ)
!
    RETURN
!
    END SUBROUTINE TPMIX2DD
