!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module RDFSAV...
!! @details Details of Module RDFSAV...
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS
!! @arg @c PARMETA
!! @details <b>Driver:</b> 
!! @arg @c CLO89
!! @arg @c E1E290
!! @arg @c E290
!! @arg @c E2SPEC
!! @arg @c E3V88
!! @arg @c FST88
!! @arg @c GFDLRD
!! @arg @c GRADFS
!! @arg @c LWR88
!! @arg @c MODULE_CO2DTA
!! @arg @c MODULE_RNDDTA
!! @arg @c MODULE_SCRTCH
!! @arg @c MODULE_SWRSAV
!! @arg @c MODULE_TABCOM
!! @arg @c MODULE_TBLTMP
!! @arg @c RADFS
!! @arg @c SPA88
!! @arg @c SWR93
!! @arg @c TABLE
!<
!--------------------------------------------------------------------------------------------------
    MODULE RDPARM
!--------------------------------------------------------------------------------------------------
! MODULE RDPARM
!
! USE MODULES: F77KINDS
!              PARMETA
!
! DRIVER     : CLO89
!              E1E290
!              E290
!              E2SPEC
!              E3V88
!              FST88
!              GFDLRD
!              GRADFS
!              LWR88
!              MODULE_CO2DTA
!              MODULE_RNDDTA
!              MODULE_SCRTCH
!              MODULE_SWRSAV
!              MODULE_TABCOM
!              MODULE_TBLTMP
!              RADFS
!              SPA88
!              SWR93
!              TABLE
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
    USE PARMETA
!
    IMPLICIT NONE
!
    SAVE
!------------------------------------------------------------------------------------------
! PARAMETER SETTINGS FOR THE LONGWAVE AND SHORTWAVE RADIATION CODE:
! IMAX    =  NO. POINTS ALONG THE LAT. CIRCLE USED IN CALCS.
! LM      =  NO. VERTICAL LEVELS (ALSO LAYERS) IN MODEL
! NOTE: THE USER NORMALLY WILL MODIFY ONLY THE IMAX AND L PARAMETERS
! NBLW   =  NO. FREQ. BANDS FOR APPROX COMPUTATIONS. SEE BANDTA FOR DEFINITION
! NBLX   =  NO. FREQ BANDS FOR APPROX CTS COMPUTATIONS
! NBLY   =  NO. FREQ. BANDS FOR EXACT CTS COMPUTATIONS. SEE BDCOMB FOR DEFINITION
! INLTE  =  NO. LEVELS USED FOR NLTE CALCS.
! NNLTE  =  INDEX NO. OF FREQ. BAND IN NLTE CALCS.
! NB, KO2 ARE SHORTWAVE PARAMETERS; OTHER QUANTITIES ARE DERIVED FROM THE ABOVE PARAMETERS.
!------------------------------------------------------------------------------------------
    INTEGER(KIND=I4)    , PARAMETER :: IMAX   = IM
    INTEGER(KIND=I4)    , PARAMETER :: NBLW   = 163
    INTEGER(KIND=I4)    , PARAMETER :: NBLX   =  47
    INTEGER(KIND=I4)    , PARAMETER :: NBLY   =  15
    INTEGER(KIND=I4)    , PARAMETER :: NBLM   = NBLY  - 1
!
    INTEGER(KIND=I4)    , PARAMETER :: LP2    = LM    + 2
    INTEGER(KIND=I4)    , PARAMETER :: LP3    = LM    + 3
    INTEGER(KIND=I4)    , PARAMETER :: LM2    = LM    - 2
    INTEGER(KIND=I4)    , PARAMETER :: LM3    = LM    - 3
!
    INTEGER(KIND=I4)    , PARAMETER :: LL     =   2   * LM
    INTEGER(KIND=I4)    , PARAMETER :: LLP1   = LL    + 1
    INTEGER(KIND=I4)    , PARAMETER :: LLP2   = LL    + 2
    INTEGER(KIND=I4)    , PARAMETER :: LLP3   = LL    + 3
!
    INTEGER(KIND=I4)    , PARAMETER :: LLM1   = LL    - 1
    INTEGER(KIND=I4)    , PARAMETER :: LLM2   = LL    - 2
    INTEGER(KIND=I4)    , PARAMETER :: LLM3   = LL    - 3
!
    INTEGER(KIND=I4)    , PARAMETER :: LP1M   = LP1   * LP1
    INTEGER(KIND=I4)    , PARAMETER :: LP1M1  = LP1M  - 1
    INTEGER(KIND=I4)    , PARAMETER :: LP1V   = LP1   * (1 + 2 * LM / 2)
    INTEGER(KIND=I4)    , PARAMETER :: LP121  = LP1   * NBLY
    INTEGER(KIND=I4)    , PARAMETER :: LL3P   =   3   * LM + 2
!
    INTEGER(KIND=I4)    , PARAMETER :: NB     =  12
    INTEGER(KIND=I4)    , PARAMETER :: INLTE  =   3
    INTEGER(KIND=I4)    , PARAMETER :: INLTEP = INLTE + 1
    INTEGER(KIND=I4)    , PARAMETER :: NNLTE  =  56
!
    INTEGER(KIND=I4)    , PARAMETER :: LP1I   = IMAX * LP1
    INTEGER(KIND=I4)    , PARAMETER :: LLP1I  = IMAX * LLP1
    INTEGER(KIND=I4)    , PARAMETER :: LL3PI  = IMAX * LL3P
!
    INTEGER(KIND=I4)    , PARAMETER :: NB1    = NB   - 1
    INTEGER(KIND=I4)    , PARAMETER :: KO2    = 12
    INTEGER(KIND=I4)    , PARAMETER :: KO21   = KO2  + 1
    INTEGER(KIND=I4)    , PARAMETER :: KO2M   = KO2  - 1
!
    END MODULE RDPARM
