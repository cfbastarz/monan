!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief THIS PRECIPITATION FALLOUT SCHEME IS BASED ON THE SCHEME
!> @details THIS PRECIPITATION FALLOUT SCHEME IS BASED ON THE SCHEME USED BY OGURA AND CHO (1973).  
!! LIQUID WATER FALLOUT FROM A PARCEL IS CALCULATED USING THE EQUATION DQ = -RATE * Q * DT, BUT TO
!! SIMULATE A QUASI-CONTINUOUS PROCESS, AND TO ELIMINATE A DEPENDENCY ON VERTICAL RESOLUTION THIS 
!! IS EXPRESSED AS Q = Q * EXP(-RATE * DZ). 
!> @author ORIGINATOR - FERRIER 
!> @date 01-09-26 \n
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
!> @param[in] DZ - Significado de DZ
!> @param[in] BOTERM - Significado de BOTERM
!> @param[in] ENTERM - Significado de ENTERM
!> @param[in] RATE - Significado de RATE
!> @param[out] QLQOUT - Significado de QLQOUT
!> @param[out] QICOUT - Significado de QICOUT
!> @param[inout] QLIQ - Significado de QLIQ
!> @param[inout] QICE - Significado de QICE
!> @param[inout] WTW - Significado de WTW
!> @param[inout] QNEWLQ - Significado de QNEWLQ
!> @param[inout] QNEWIC - Significado de QNEWIC
!> @details <b>Use Module:</b> 
!! @arg @c F77KINDS
!> @details <b>Driver:</b> 
!! @arg @c KFPARA
!> @details <b>Calls:</b>
!<
!--------------------------------------------------------------------------------------------------
    SUBROUTINE CONDLOAD(QLIQ, QICE, WTW, DZ, BOTERM, ENTERM, RATE, QNEWLQ, QNEWIC, QLQOUT, QICOUT)
!--------------------------------------------------------------------------------------------------
! SUBROUTINE CONDLOAD
!
! SUBPROGRAM: CONDLOAD - THIS PRECIPITATION FALLOUT SCHEME IS BASED ON THE SCHEME
! PROGRAMMER: FERRIER 
! ORG: W/NP22
! DATE: 01-09-26
!     
! ABSTRACT: 
! THIS PRECIPITATION FALLOUT SCHEME IS BASED ON THE SCHEME USED BY OGURA AND CHO (1973).  
! LIQUID WATER FALLOUT FROM A PARCEL IS CALCULATED USING THE EQUATION DQ = -RATE * Q * DT, BUT TO
! SIMULATE A QUASI-CONTINUOUS PROCESS, AND TO ELIMINATE A DEPENDENCY ON VERTICAL RESOLUTION THIS 
! IS EXPRESSED AS Q = Q * EXP(-RATE * DZ).     
!
! PROGRAM HISTORY LOG:
! 01-09-26  FERRIER  - ORIGINATOR            
! 18-03-20  LUCCI    - MODERNIZATION OF THE CODE, INCLUDING:
!                      * F77 TO F90/F95
!                      * INDENTATION & UNIFORMIZATION CODE
!                      * REPLACEMENT OF COMMONS BLOCK FOR MODULES
!                      * DOCUMENTATION WITH DOXYGEN
!                      * OPENMP FUNCTIONALITY
!
! INPUT  ARGUMENT LIST:
! DZ     -
! BOTERM -
! ENTERM - 
! RATE   - 
!
! OUTPUT ARGUMENT LIST:
! QLQOUT - 
! QICOUT -
!
! INPUT/OUTPUT ARGUMENT LIST:
! QLIQ   - 
! QICE   -
! WTW    -
! QNEWLQ - 
! QNEWIC - 
!     
! USE MODULES: F77KINDS
!
! DRIVER     : KFPARA
!
! CALLS      : -----
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    REAL   (KIND=R4)    , PARAMETER :: G = 9.80616
!
    REAL   (KIND=R4)                                                      , INTENT(INOUT)       ::&
    & QLIQ    , QICE    , WTW     , QNEWLQ  , QNEWIC
!
    REAL   (KIND=R4)                                                      , INTENT(IN)          ::&
    & DZ      , BOTERM  , ENTERM  , RATE    
!
    REAL   (KIND=R4)                                                      , INTENT(OUT)         ::&
    & QLQOUT  , QICOUT 
!
    REAL   (KIND=R4)                                                                            ::&
    & QTOT    , QNEW    , G1      , WAVG    , CONV    , RATIO3  , OLDQ    , RATIO4  , DQ      ,   &
    & PPTDRG  , QEST
!
    QTOT = QLIQ   + QICE                                                    
    QNEW = QNEWLQ + QNEWIC                                                
!--------------------------------------------------------------------------------------------------                                                                      
! ESTIMATE THE VERTICAL VELOCITY SO THAT AN AVERAGE VERTICAL VELOCITY CAN BE CALCULATED TO ESTIMATE
! THE TIME REQUIRED FOR ASCENT BETWEEN MODEL LEVELS.
!--------------------------------------------------------------------------------------------------                                                                      
    QEST = 0.5 * (QTOT  + QNEW)                                              
    G1   = WTW + BOTERM - ENTERM - 2. * G * DZ * QEST / 1.5
!                             
    IF (G1 < 0.0) G1 = 0.
!                                                
    WAVG = (SQRT(WTW) + SQRT(G1)) / 2.                                      
    CONV = RATE * DZ / WAVG                                                 
    CONV = AMIN1(CONV,  50.)
    CONV = AMAX1(CONV, -50.)
!--------------------------------------------------------------------------------------------------                                                                       
! RATIO3 IS THE FRACTION OF LIQUID WATER IN FRESH CONDENSATE, RATIO4 IS THE FRACTION OF LIQUID 
! WATER IN THE TOTAL AMOUNT OFCONDLOAD_OLD.F90 CONDENSATE INVOLVED IN THE PRECIPITATION PROCESS.
! NOTE THAT ONLY 60% OF THE FRESH CONDENSATE IS ALLOWED TO PARTICIPATE IN THE CONVERSION PROCESS.
!--------------------------------------------------------------------------------------------------                                                                      
    RATIO3 = QNEWLQ / (QNEW + 1.E-10)                                  
    QTOT   = QTOT   + 0.6   * QNEW 
!                                               
    OLDQ = QTOT
!                                                         
    RATIO4 = (0.6 * QNEWLQ + QLIQ) / (QTOT + 1.E-10)                            
    QTOT   = QTOT * EXP(-CONV)                                              
!------------------------------------------------------------------------------------------
! DETERMINE THE AMOUNT OF PRECIPITATION THAT FALLS OUT OF THE UPDRAFT PARCEL AT THIS LEVEL.
!------------------------------------------------------------------------------------------                                                                       
    DQ     = OLDQ - QTOT                                                      
    QLQOUT = RATIO4 * DQ                                                  
    QICOUT = (1. - RATIO4) * DQ                                             
!-----------------------------------------------------------------------------------------------                                 
! ESTIMATE THE MEAN LOAD OF CONDENSATE ON THE UPDRAFT IN THE LAYER, CALCULATE VERTICAL VELOCITY.
!-----------------------------------------------------------------------------------------------
    PPTDRG = 0.5 * (OLDQ  + QTOT   - 0.2 * QNEW)                                   
    WTW    = WTW + BOTERM - ENTERM - 2.  * G * DZ * PPTDRG / 1.5                          
!--------------------------------------------------------------------------------------------
! DETERMINE THE NEW LIQUID WATER AND ICE CONCENTRATIONS INCLUDING LOSSES DUE TO PRECIPITATION 
! AND GAINS FROM CONDENSATION.
!--------------------------------------------------------------------------------------------
    QLIQ =       RATIO4  * QTOT +       RATIO3  * 0.4 * QNEW                                  
    QICE = (1. - RATIO4) * QTOT + (1. - RATIO3) * 0.4 * QNEW   
!                     
    QNEWLQ = 0.                                                         
    QNEWIC = 0.                                                         
!
    RETURN  
!                                                          
    END SUBROUTINE CONDLOAD
