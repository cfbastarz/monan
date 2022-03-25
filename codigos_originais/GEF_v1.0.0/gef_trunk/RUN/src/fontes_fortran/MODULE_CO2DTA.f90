!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module CO2DTA... 
!! @details Details of Module CO2DTA... 
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS 
!! @arg @c RDPARM
!! @details <b>Driver:</b> 
!! @arg @c CONRAD  
!! @arg @c LWR88
!! @arg @c RADFS
!<
!--------------------------------------------------------------------------------------------------
      MODULE CO2DTA
!--------------------------------------------------------------------------------------------------
! MODULE CO2DTA
!
! USE MODULES: F77KINDS
!              RDPARM
!
! DRIVER     : CONRAD
!              LWR88
!              RADFS
!--------------------------------------------------------------------------------------------------
      USE F77KINDS
      USE RDPARM
!
      IMPLICIT NONE
!
      SAVE
!--------------------------------------------------------------------------------------------------
! MODULE CO2BD3
!
! ABSTRACT:
! THE FOLLOWING MODULE CONTAIN PRETABULATED CO2 TRANSMISSION FUNCTIONS, EVALUATED USING THE 
! METHODS OF FELS AND SCHWARZKOPF (1981) AND SCHWARZKOPF AND FELS (1985), MODULE CO2BD3 CONTAINS 
! CO2 TRANSMISSION FUNCTIONS AND TEMPERATURE AND PRESSURE DERIVATIVES FOR THE 560-800 CM-1 BAND. 
! ALSO INCLUDED ARE THE STANDARD TEMPERATURES AND THE WEIGHTING FUNCTION. THESE DATA ARE IN BLOCK
! DATA BD3:
!         
! CO251  - TRANSMISSION FCTNS. FOR T0 (STD. PROFILE) WITH P(SFC)=1013.25 MB
! CO258  - TRANSMISSION FCTNS. FOR T0 (STD. PROFILE) WITH P(SFC)= ^810 MB
! CDT51  - FIRST  TEMPERATURE DERIVATIVE OF CO251
! CDT58  - FIRST  TEMPERATURE DERIVATIVE OF CO258
! C2D51  - SECOND TEMPERATURE DERIVATIVE OF CO251
! C2D58  - SECOND TEMPERATURE DERIVATIVE OF CO251
! CO2M51 - TRANSMISSION FCTNS FOR T0 FOR ADJACENT PRESSURE LEVELS, WITH NO PRESSURE QUADRATURE.
!          USED FOR NEARBY LAYER COMPUTATIONS. P(SFC)=1013.25 MB
! CO2M58 - SAME AS CO2M51,WITH P(SFC)= ^810 MB
! CDTM51 - FIRST  TEMPERATURE DERIVATIVE OF CO2M51
! CDTM58 - FIRST  TEMPERATURE DERIVATIVE OF CO2M58
! C2DM51 - SECOND TEMPERATURE DERIVATIVE OF CO2M51
! C2DM58 - SECOND TEMPERATURE DERIVATIVE OF CO2M58
! STEMP  - STANDARD TEMPERATURES FOR MODEL PRESSURE LEVEL STRUCTURE WITH P(SFC)=1013.25 MB
! GTEMP  - WEIGHTING FUNCTION FOR MODEL PRESSURE LEVEL STRUCTURE WITH P(SFC)=1013.25 MB.
! B0     - TEMP. COEFFICIENT USED FOR CO2 TRANS. FCTN. CORRECTION FOR T(K). (SEE REF. 4 AND BD3)
! B1     - TEMP. COEFFICIENT, USED ALONG WITH B0
! B2     - TEMP. COEFFICIENT, USED ALONG WITH B0
! B3     - TEMP. COEFFICIENT, USED ALONG WITH B0
!---------------------------------------------------------------------------------------------------
    REAL   (KIND=R4)    , DIMENSION(LP1, LP1)                                                   ::&
    & CO251   , CO258   , CDT51   , CDT58   , C2D51   , C2D58
!
    REAL   (KIND=R4)    , DIMENSION(LM)                                                         ::&
    & CO2M51  , CO2M58  , CDTM51  , CDTM58  , C2DM51  , C2DM58
!
    REAL   (KIND=R4)    , DIMENSION(LP1)                                                        ::&
    & STEMP   , GTEMP
!   
    REAL   (KIND=R4)                                                                            ::&
    & B0      , B1      , B2      , B3 
!--------------------------------------------------------------------------------------------------
! MODULE CO2BD2
!
! ABSTRACT:
! MODULE CO2BD2 CONTAINS CO2 TRANSMISSION FUNCTIONS AND TEMPERATURE AND PRESSURE DERIVATIVES FOR 
! THE 560-670 CM-1 PART OF THE 15 UM CO2 BAND. THESE DATA ARE IN BLOCK DATA BD2.
!
! CO231 - TRANSMISSION FCTNS. FOR T0 (STD. PROFILE) WITH P(SFC)=1013.25 MB
! CO238 - TRANSMISSION FCTNS. FOR T0 (STD. PROFILE) WITH P(SFC)= ^810   MB
! CDT31 - FIRST  TEMPERATURE DERIVATIVE OF CO231
! CDT38 - FIRST  TEMPERATURE DERIVATIVE OF CO238
! C2D31 - SECOND TEMPERATURE DERIVATIVE OF CO231
! C2D38 - SECOND TEMPERATURE DERIVATIVE OF CO231
!--------------------------------------------------------------------------------------------------
    REAL   (KIND=R4)    , DIMENSION(LP1)                                                        ::&
    & CO231   , CO238   ,                                                                         &
    & CDT31   , CDT38   ,                                                                         &
    & C2D31   , C2D38
!--------------------------------------------------------------------------------------------------
! MODULE CO2BD4
!
! ABSTRACT:
! MODULE CO2BD4 CONTAINS CO2 TRANSMISSION FUNCTIONS AND TEMPERATURE AND PRESSURE DERIVATIVES FOR 
! THE 670-800 CM-1 PART OF THE 15 UM CO2 BAND. THESE DATA ARE IN BLOCK DATA BD4.
!
! CO271 - TRANSMISSION FCTNS. FOR T0 (STD. PROFILE) WITH P(SFC)=1013.25 MB
! CO278 - TRANSMISSION FCTNS. FOR T0 (STD. PROFILE) WITH P(SFC)= ^810 MB
! CDT71 - FIRST  TEMPERATURE DERIVATIVE OF CO271
! CDT78 - FIRST  TEMPERATURE DERIVATIVE OF CO278
! C2D71 - SECOND TEMPERATURE DERIVATIVE OF CO271
! C2D78 - SECOND TEMPERATURE DERIVATIVE OF CO271
!--------------------------------------------------------------------------------------------------
    REAL   (KIND=R4)    , DIMENSION(LP1)                                                        ::&
    & CO271   , CO278   ,                                                                         &
    & CDT71   , CDT78   ,                                                                         &
    & C2D71   , C2D78
!--------------------------------------------------------------------------------------------------
! MODULE CO2BD5
!
! ABSTRACT:
! MODULE CO2BD5 CONTAINS CO2 TRANSMISSION FUNCTIONS FOR THE 2270-2380 PART OF THE 4.3 UM CO2 BAND. 
! THESE DATA ARE IN BLOCK DATA BD5.
!
! CO211    -  TRANSMISSION FCTNS. FOR T0 (STD. PROFILE) WITH P(SFC)=1013.25 MB
! CO218    -  TRANSMISSION FCTNS. FOR T0 (STD. PROFILE) WITH P(SFC)= ^810 MB
!--------------------------------------------------------------------------------------------------
    REAL   (KIND=R4)    , DIMENSION(LP1)                                                        ::&
    & CO211   , CO218
!
    END MODULE CO2DTA
