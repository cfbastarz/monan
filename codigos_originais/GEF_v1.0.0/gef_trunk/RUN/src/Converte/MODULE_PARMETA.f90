    MODULE PARMETA
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!--------------------------------------- 
! SET RESOLUTION, GRID AND DECOMPOSITION
!--------------------------------------- 
!
!-----------------
! NUMBER OF FACES:
!-----------------
    INTEGER(KIND=I4)    , PARAMETER :: NM   = 6
!---------------------------- 
! DECOMPOSITION OF EACH FACE:
!----------------------------
    INTEGER(KIND=I4)    , PARAMETER :: NSUB = 20
    INTEGER(KIND=I4)    , PARAMETER :: IXM  = NSUB
    INTEGER(KIND=I4)    , PARAMETER :: JYM  = IXM
    INTEGER(KIND=I4)    , PARAMETER :: NXY  = IXM * JYM 
!------------------------------------ 
! NUMBER OF GRID POINTS ON EACH FACE:
!------------------------------------
    INTEGER(KIND=I4)    , PARAMETER :: IM0  = 401
    INTEGER(KIND=I4)    , PARAMETER :: JM0  = IM0
    INTEGER(KIND=I4)    , PARAMETER :: IM   = 401/NSUB+1
    INTEGER(KIND=I4)    , PARAMETER :: JM   = IM
    INTEGER(KIND=I4)    , PARAMETER :: IM1  = IM - 1
    INTEGER(KIND=I4)    , PARAMETER :: JM1  = JM - 1  
!--------------------------- 
! NUMBER OF VERTICAL LAYERS:
!---------------------------          
    INTEGER(KIND=I4)    , PARAMETER :: LM   = 38
    INTEGER(KIND=I4)    , PARAMETER :: LM1  = LM - 1
    INTEGER(KIND=I4)    , PARAMETER :: LP1  = LM + 1
    INTEGER(KIND=I4)    , PARAMETER :: LSM  = 39
!---------------------------
! RESTART:
!---------------------------
    INTEGER(KIND=I4)    , PARAMETER :: outnum=8      ! number of outputs
    INTEGER(KIND=I4)    , PARAMETER :: nday=2          ! number of days

    END MODULE PARMETA
