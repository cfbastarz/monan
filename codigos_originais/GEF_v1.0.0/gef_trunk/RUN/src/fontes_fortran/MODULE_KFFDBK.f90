!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module KFFDBK... 
!! @details Details of Module KFFDBK... 
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS 
!! @details <b>Driver:</b> 
!! @arg @c ALLOC
!! @arg @c INIT
!! @arg @c KFDRIVE
!! @arg @c KFPARA
!! @arg @c KFTEND
!<
!-------------------------------------------------------------------------------------------------- 
 MODULE KFFDBK
!--------------------------------------------------------------------------------------------------
! MODULE KFFDBK
!
! USE MODULES: F77KINDS
!
! DRIVER     : ALLOC
!              INIT
!              KFDRIVE
!              KFPARA
!              KFTEND
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    SAVE
!    
    REAL   (KIND=R4)    , DIMENSION(:,:,:)            , ALLOCATABLE                             ::&
    & DTDT    , DQDT    , DQCDT   , W0AVG
!
    REAL   (KIND=R4)    , DIMENSION(:,:)              , ALLOCATABLE                             ::&
    & RAINCV  , PPTKF   , PPTCA   , TNCA    , PSRC    , PCLB    , UMFB    , SUMFB   , SPSRC   ,   &
    & SPCLB   , CIN
! 
    REAL   (KIND=R4)                                                                            ::&
    & TST
!
    INTEGER(KIND=I4)    , DIMENSION(:,:)              , ALLOCATABLE                             ::&
    & NCAD    , NCA
!
    INTEGER(KIND=I4)                                                                            ::&
    & NCLDCK
!
    END MODULE KFFDBK
