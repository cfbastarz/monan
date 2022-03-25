!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Inserir Brief de GRADFS
!> @details Inserir Details de GRADFS
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
!> @param[in] SIGL    - MIDLAYER PRESSURES IN PA
!> @details <b>Use Module:</b>
!! @arg @c F77KINDS
!! @arg @c HCON
!! @arg @c MPPSTAFF
!! @arg @c PARMETA
!! @arg @c RDFSAV
!! @arg @c RDPARM
!! @arg @c SAVMEM
!> @details <b>Driver:</b> 
!! @arg @c INIT
!> @details <b>Calls:</b>
!! @arg @c CONRAD
!! @arg @c HCONST
!! @arg @c O3INT
!--------------------------------------------------------------------------------------------------
    SUBROUTINE GRADFS(SIGL)  
!--------------------------------------------------------------------------------------------------
! SUBROUTINE GRADFS
! 
! SUBPROGRAM: GRADFS - ?????
! PROGRAMMER: ????? 
! ORG: W/NP22
! DATE: ??-??-??
!
! ABSTRACT:  
! ?????
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
! SIGL - MIDLAYER PRESSURES IN PA
!
! INPUT FILES:
! NONE
!
! OUTPUT FILES:
! NONE
!
! USE MODULES: F77KINDS
!              HCON
!              MPPSTAFF
!              PARMETA
!              RDFSAV
!              RDPARM
!              SAVMEM
!
! DRIVER     : INIT
!
! CALLS      : CONRAD
!              HCONST
!              O3INT            
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
    USE HCON
    USE MPPSTAFF
    USE PARMETA
    USE RDFSAV
    USE RDPARM
    USE SAVMEM
!
    IMPLICIT NONE
!
    INTEGER(KIND=I4)                                                                            ::&
    & I       , LV
!
    REAL   (KIND=R4)                                                                            ::&
    & A1      , AVG     , B1      , B2      , PI  
!--------------------------------------------------------------------------------------------------
! SEASONAL CLIMATOLOGIES OF O3 (OBTAINED FROM A PREVIOUSLY RUN CODE WHICH INTERPOLATES O3 TO USER 
! VERTICAL COORDINATE).
!--------------------------------------------------------------------------------------------------
    REAL   (KIND=R4)    , DIMENSION(LP1)                                  , INTENT(INOUT)       ::&
    & SIGL
!
    REAL   (KIND=R4)    , DIMENSION(5)                                                          ::&
    & XAO3SW  , XAH2SW  , XBSW
!
    DATA XAO3SW / 0., .690, .480, .210, 0./
    DATA XAH2SW / 0., .690, .480, .210, 0./ 
    DATA XBSW   / 0., .035, .020, .005, 0./
!---------------------------------------------
! ONE TIME COMPUTATION OF NECESSARY QUANTITIES
!---------------------------------------------
!
!-----------------------------------------
! INITIALIZE ARRAYS, GET CONSTANTS, ETC...
!-----------------------------------------
    PI     =   3.1415927
    Q19001 =  19.001
    HP98   =   0.98
    H3M6   =   3.0E-6
    HP537  =   0.537
    H74E1  =  74.0
    H15E1  =  15.0
    Q14330 =   1.43306E-6
    HP2    =   0.2
    TWENTY =  20.0
    HNINE  =   9.0
    DEGRAD = 180.0 / PI
    HSIGMA =   5.673E-5
    DAYSEC =   1.1574E-5
!--------------------------------------------------------------------------------------------------
! ATMOSPERIC CARBON DIOXIDE CONCENTRATION IS NOW READ BY CONRAD, BUT IT DEFAULTS TO 330 PPM FOR  
! BACKWARD COMPATIBILITY.
!--------------------------------------------------------------------------------------------------
    RCO2 = 3.3E-4
!
    CALL HCONST
!-------------------------------------------------------- 
! INTERPOLATE CLIMO O3 TO THE CURRENT VERTICAL COORDINATE 
! NEED LAYER SIGMA, GET FROM PSFC AND LAYER P FOR I=1 
!--------------------------------------------------------
    DO I=1,5
        CAO3SW(I) = XAO3SW(I)
        CAH2SW(I) = XAH2SW(I)
          CBSW(I) =   XBSW(I)
    END DO
!-----------------------------------------------
! CONVERT SIGL FROM PA TO MB TO BE USED IN O3INT
!-----------------------------------------------
    DO LV=1,LP1
        SIGL(LV) = 0.01 * SIGL(LV)
    END DO
!
    CALL O3INT(SIGL)
!
    CALL CONRAD
!--------------------------------------------------------------------------------------------------
! AVERAGE CLIMATOLOGICAL VALUS OF O3 FROM 5 DEG LAT MEANS, SO THAT TIME AND SPACE INTERPOLATION 
! WILL WORK (DONE ELSEWHERE IN RADFS)
!--------------------------------------------------------------------------------------------------
    DO I=1,LNGTH
        AVG = .25E0 * ( RAD1(I) + RAD2(I)  +  RAD3(I) + RAD4(I))
!
        A1  = .50E0 * ( RAD2(I) - RAD4(I))
        B1  = .50E0 * ( RAD1(I) - RAD3(I))
        B2  = .25E0 * ((RAD1(I) + RAD3(I)) - (RAD2(I) + RAD4(I)))
!
        RAD1(I) = AVG
        RAD2(I) = A1
        RAD3(I) = B1
        RAD4(I) = B2
    END DO
!
    EMIST =   .6E0
    EMISP =   .3E0
    XLATP = 60.0E0
    XLATT = 30.0E0
 !
    RETURN
!
    END SUBROUTINE GRADFS
