!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module DGNSOUT... 
!! @details Details of Module DGNSOUT... 
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS
!! @arg @c PARMETA 
!! @details <b>Driver:</b> 
!! @arg @c CHECKMXMN
!! @arg @c DIGFLT
!! @arg @c FINISHMPI
!! @arg @c GEF
!! @arg @c INITCKMM
!! @arg @c INIT
!! @arg @c OUT
!! @arg @c OUTSD
!! @arg @c OUT2
!! @arg @c OUT_HEAT 
!<
!--------------------------------------------------------------------------------------------------
    MODULE DGNSOUT
!--------------------------------------------------------------------------------------------------
! MODULE DGNSOUT
!
! USE MODULES: F77KINDS
!              PARMETA
!
! DRIVER     : CHECKMXMN
!              DIGFLT
!              FINISHMPI
!              GEF
!              INITCKMM
!              INIT
!              OUT
!              OUTSD
!              OUT2
!              OUT_HEAT 
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
    USE PARMETA
!
    IMPLICIT NONE
!
    SAVE
!
    INTEGER(KIND=I4)    , PARAMETER :: LSD = 26
!
    REAL   (KIND=R4)    , DIMENSION(LSD)                                                        ::&
    & PSD
!
    DATA PSD /1000, 975, 950, 925, 900, 850, 800, 750, 700, 650, 600,                             &
    &          550, 500, 450, 400, 350, 300, 250, 200, 150, 100,  70,                             &
    &           50,  30,  20,  10/

    INTEGER(KIND=I4)                                                                            ::&
    & DGNNUM   , DGNSTR , IDGNS   , ICKMM   , CKNUM
!
    REAL   (KIND=R4)    , DIMENSION(:,:)              , ALLOCATABLE                             ::&
    & TMXMN   , PDMXMN  , UMXMN   , VMXMN
!
    INTEGER(KIND=I4)    ,DIMENSION(:,:,:)             , ALLOCATABLE                             ::&
    & LOCTMM  , LOCPDMM , LOCUMM  , LOCVMM
!
    INTEGER(KIND=I4)                                                                            ::&
    & NHRS
!      
    END MODULE DGNSOUT
