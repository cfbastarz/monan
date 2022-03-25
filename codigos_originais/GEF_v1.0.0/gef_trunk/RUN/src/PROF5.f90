!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Q2 PRODUCTION/DISSIPATION
!> @details Q2 PRODUCTION/DISSIPATION.
!> @author ORIGINATOR - JANJIC
!> @date 87-06-?? \n
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
!> @param[in]  EQ  - Significado de EQ
!> @param[out] EE  - Significado de EE
!> @param[out] UD  - Significado de UD
!> @details <b>Use Module:</b>
!! @arg @c CTLBLK
!! @arg @c F77KINDS
!! @arg @c MPPSTAFF
!! @arg @c PARMETA
!> @details <b>Driver:</b> 
!! @arg @c KFPARA
!> @details <b>Calls:</b>
!--------------------------------------------------------------------------------------------------     
	SUBROUTINE PROF5(EQ, EE, UD)
!--------------------------------------------------------------------------------------------------
! SUBROUTINE PROF5
!
! SUBROUTINE: PROF5 - INTEGRATES THE AREA UNDER THE CURVE IN THE GAUSSIAN DISTRIBUTION. 
! PROGRAMMER: ?????
! ORG: ?????
! DATE: ??-??-??
!
! ABSTRACT:
! THIS SUBROUTINE INTEGRATES THE AREA UNDER THE CURVE IN THE GAUSSIAN DISTRIBUTION. 
! THE NUMERICAL APPROXIMATION TO THE INTEGRAL IS TAKEN FROM "HANDBOOK OF MATHEMATICAL FUNCTIONS
! WITH FORMULAS, GRAPHS AND MATHEMATICAL TABLES" ED. BY ABRAMOWITZ AND STEGUN, NATL BUREAU OF 
! STANDARDS AND APPLIED MATHEMATICS SERIES.  JUNE, 1964., MAY, 1968.                         
! JACK KAIN 
!
! PROGRAM HISTORY LOG:
! 94-??-??  ?????      - ORIGINATOR
! 18-01-15  LUCCI      - MODERNIZATION OF THE CODE, INCLUDING:
!                        * F77 TO F90/F95
!                        * INDENTATION & UNIFORMIZATION CODE
!                        * REPLACEMENT OF COMMONS BLOCK FOR MODULES
!                        * DOCUMENTATION WITH DOXYGEN
!                        * OPENMP FUNCTIONALITY
!
! INPUT ARGUMENT LIST:
! EQ -
!
! OUTPUT ARGUMENT LIST:
! EE - 
! UD - 
!
! INPUT/OUTPUT ARGUMENT LIST:
! NONE
!
! OUTPUT FILES:
! NONE
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
    REAL   (KIND=R4)                                                                            ::&
    & SQRT2P  , A1      , A2      , A3      , P       , SIGMA   , FE      , X       , Y       ,   &
    & EY      , E45     , T2      , T1      , C1      , C2      , EE      , UD      , EQ
!----------------------------- 
! GAUSSIAN TYPE MIXING PROFILE
!-----------------------------
    DATA SQRT2P / 2.506628    /
    DATA A1     / 0.4361836   /
    DATA A2     /-0.1201676   /
    DATA A3     / 0.9372980   / 
    DATA P      / 0.33267     /
    DATA SIGMA  / 0.166666667 /
    DATA FE     / 0.202765151 /                        
!
    X = (EQ - 0.5) / SIGMA                                                  
    Y =  6. * EQ - 3.
!                                                       
    EY  = EXP(Y * Y / (-2))                                                  
    E45 = EXP(-4.5)
!                                                     
    T2 = 1. / (1. + P * ABS(Y))                                               
    T1 = 0.500498
!                                                       
    C1 = A1 * T1 + A2 * T1 * T1 + A3 * T1 * T1 * T1                                     
    C2 = A1 * T2 + A2 * T2 * T2 + A3 * T2 * T2 * T2
!                                     
    IF (Y >= 0.) THEN                                                   
        EE = SIGMA * (0.5 * (SQRT2P - E45 * C1 - EY * C2)                                         &
    &      + SIGMA * (E45 - EY)) - E45 * EQ * EQ / 2.
!
        UD = SIGMA * (0.5 * (EY * C2 - E45 * C1)                                                  &
    &      + SIGMA * (E45 - EY)) - E45 * (0.5 + EQ * EQ / 2. - EQ)
!                                                           
    ELSE
!                                                              
        EE = SIGMA * (0.5 * (EY * C2 - E45 * C1) + SIGMA * (E45 - EY)) - E45 * EQ * EQ / 2.      
! 
        UD = SIGMA * (0.5 * (SQRT2P - E45 * C1 - EY * C2)                                         &
    &      + SIGMA * (E45 - EY)) - E45 * (0.5 + EQ * EQ / 2. - EQ)                                                    
    END IF                                                             
!
    EE = EE / FE                                                          
    UD = UD / FE                                                          
!
    RETURN
!                                                            
    END SUBROUTINE PROF5                                                               
