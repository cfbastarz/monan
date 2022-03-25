!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Inserir Brief de FLUX_CORRECTION1D
!> @details Inserir Details de FLUX_CORRECTION1D
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
!> @param[in] HPRE  - Significado de HPRE
!> @param[in] HOLD  - Significado de HOLD
!> @param[in] FDIV  - Significado de FDIV
!> @param[in] LLMH  - Significado de LLMH
!> @param[inout] ANTID - Significado de ANTID
!> @details <b>Use Module:</b>
!! @arg @c F77KINDS
!! @arg @c PARMETA
!> @details <b>Driver:</b> 
!! @arg @c VTADV
!> @details <b>Calls:</b>
!--------------------------------------------------------------------------------------------------
      SUBROUTINE FLUX_CORRECTION1D(HPRE, HOLD, FDIV, ANTID, LLMH)
!--------------------------------------------------------------------------------------------------
! SUBROUTINE FLUX_CORRECTION1D
!
! SUBPROGRAM: FLUX_CORRECTION1D - ?????
! PROGRAMMER: ?????
! ORG: ?????
! DATE: ??-??-??
!
! ABSTRACT:
! ?????
! 
! PROGRAM HISTORY LOG:
! ??-??-??  ?????      - ORIGINATOR
! 18-01-15  LUCCI      - MODERNIZATION OF THE CODE, INCLUDING:
!                        * F77 TO F90/F95
!                        * INDENTATION & UNIFORMIZATION CODE
!                        * REPLACEMENT OF COMMONS BLOCK FOR MODULES
!                        * DOCUMENTATION WITH DOXYGEN
!                        * OPENMP FUNCTIONALITY
!
! INPUT ARGUMENT LIST:
! HPRE  - 
! HOLD  - 
! FDIV  -
! LLMH  -  
!
! OUTPUT ARGUMENT LIST:
! NONE
!
! INPUT/OUTPUT ARGUMENT LIST:
! ANTID - 
!
! USE MODULES: F77KINDS
!              PARMETA
!  
! DRIVER     : VTADV
!
! CALLS      : -----
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
    USE PARMETA
!
    IMPLICIT NONE
!
    REAL   (KIND=R4)    , DIMENSION(LM)                                   , INTENT(IN)          ::&
    & HPRE    , HOLD    , FDIV    
!
    REAL   (KIND=R4)    , DIMENSION(LM)                                   , INTENT(INOUT)       ::&
    & ANTID
!
    REAL   (KIND=R4)    , DIMENSION(LM)                                                         ::&
    & FXN_PL  , FXN_MN  , HMAX    , HMIN    , BETA_UP , BETA_DOWN         , CXPL    , CXMN
!
    REAL   (KIND=R4)    , PARAMETER :: EPSP = 1.E-20
!
    REAL   (KIND=R4)                                                                            ::&
    & FIN     , FOUT
!
    INTEGER(KIND=I4)                                                      , INTENT(IN)          ::&
    & LLMH
!
    INTEGER(KIND=I4)                                                                            ::&
    & L       , IT
!
    DO L=1,LLMH
        FXN_PL(L) = MAX(ANTID(L), 0.)
        FXN_MN(L) = MIN(ANTID(L), 0.)
    END DO
!
    DO L=1,LLMH-1
        IF (L == 1) THEN
            HMAX(L) = MAX(HPRE(L)  , HPRE(L+1))
            HMAX(L) = MAX(HOLD(L)  , HMAX(L)  )
            HMAX(L) = MAX(HOLD(L+1), HMAX(L)  )
            HMIN(L) = MIN(HPRE(L)  , HPRE(L+1))
            HMIN(L) = MIN(HOLD(L)  , HMIN(L)  )
            HMIN(L) = MIN(HOLD(L+1), HMIN(L)  )
        ELSE IF (L == LLMH-1) THEN
            HMAX(L) = MAX(HPRE(L)  , HPRE(L-1))
            HMAX(L) = MAX(HOLD(L)  , HMAX(L)  )
            HMAX(L) = MAX(HOLD(L-1), HMAX(L)  )
            HMIN(L) = MIN(HPRE(L)  , HPRE(L-1))
            HMIN(L) = MIN(HOLD(L)  , HMIN(L)  )
            HMIN(L) = MIN(HOLD(L-1), HMIN(L)  )
        ELSE
            HMAX(L) = HPRE(L)
            HMIN(L) = HPRE(L)
!
            DO IT=-1,1,2
                HMAX(L) = MAX(HPRE(L+IT), HMAX(L))
                HMAX(L) = MAX(HOLD(L+IT), HMAX(L))
                HMIN(L) = MIN(HPRE(L+IT), HMIN(L))
                HMIN(L) = MIN(HOLD(L+IT), HMIN(L))
            END DO
        END IF
    END DO
!
    DO L=1,LLMH-1
        FOUT = -FXN_MN(L)   + FXN_PL(L+1)
        FIN  = -FXN_MN(L+1) + FXN_PL(L)
!
          BETA_UP(L) = (HMAX(L) - HPRE(L)) / (-FDIV(L) * FIN  + EPSP)
        BETA_DOWN(L) = (HPRE(L) - HMIN(L)) / (-FDIV(L) * FOUT + EPSP)
    END DO
!
    DO L=2,LLMH-1
        CXPL(L) = MIN(1., BETA_DOWN(L-1),   BETA_UP(L))
        CXMN(L) = MIN(1.,   BETA_UP(L-1), BETA_DOWN(L))
    END DO
!
    CXPL(1) = MIN(1.,   BETA_UP(1))
    CXMN(1) = MIN(1., BETA_DOWN(1))
!
    CXPL(LLMH) = MIN(1., BETA_DOWN(LLMH-1))
    CXMN(LLMH) = MIN(1.,   BETA_UP(LLMH-1))
!
    DO L=1,LLMH
        ANTID(L) = CXPL(L) * FXN_PL(L) + CXMN(L) * FXN_MN(L)
    END DO
!
    END SUBROUTINE FLUX_CORRECTION1D
