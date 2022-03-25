!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Inserir Brief de FLUX_CORRECTION
!> @details Inserir Details de FLUX_CORRECTION
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
!> @param[inout] ANTIDX - Significado de ANTIDX
!> @param[inout] ANTIDY - Significado de ANTIDY
!> @details <b>Use Module:</b>
!! @arg @c F77KINDS
!! @arg @c PARMETA
!> @details <b>Driver:</b> 
!! @arg @c HZADVQ
!> @details <b>Calls:</b>
!--------------------------------------------------------------------------------------------------
    SUBROUTINE FLUX_CORRECTION(HPRE, HOLD, FDIV, ANTIDX, ANTIDY)
!--------------------------------------------------------------------------------------------------
! SUBROUTINE FLUX_CORRECTION
!
! SUBPROGRAM: FLUX_CORRECTION - ?????
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
!
! OUTPUT ARGUMENT LIST:
! NONE
!
! INPUT/OUTPUT ARGUMENT LIST:
! ANTIDX -
! ANTIDY -  
!
! USE MODULES: F77KINDS
!              PARMETA
!  
! DRIVER     : HZADVQ
!
! CALLS      : -----
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
    USE PARMETA
!
    IMPLICIT NONE
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1)                        , INTENT(IN)         ::&
    & HPRE    , HOLD    , FDIV
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1)                        , INTENT(INOUT)      ::&
    & ANTIDX  , ANTIDY  
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1)                                             ::&
    & HMIN    , HMAX    , FXN_PL  , FXN_MN  , FYN_PL  , FYN_MN  ,                                 &
    & BETA_UP , BETA_DOWN         , CXPL    , CXMN    , CYPL    , CYMN
!
    INTEGER(KIND=I4)                                                                            ::&
    & I       , J       , IT      , JT
!
    REAL   (KIND=R4)                                                                            ::&
    & FOUT    , FIN     , BETA
!
    REAL   (KIND=R4), PARAMETER :: EPSP   = 1.E-20
    REAL   (KIND=R4), PARAMETER :: BIGNUM = 1.E20
!-------------------
! CLEANING VARIABLES
!-------------------
    DO I=0,IM+1
        DO J=0,JM+1
            FXN_PL(I,J) = 0.0
            FXN_MN(I,J) = 0.0
            FYN_PL(I,J) = 0.0
            FYN_MN(I,J) = 0.0
        END DO
    END DO
!
    DO I=0,IM+1
        DO J=0,JM+1
            FXN_PL(I,J) = MAX(ANTIDX(I,J), 0.)
            FXN_MN(I,J) = MIN(ANTIDX(I,J), 0.)
            FYN_PL(I,J) = MAX(ANTIDY(I,J), 0.)
            FYN_MN(I,J) = MIN(ANTIDY(I,J), 0.)
        END DO
    END DO

    DO I=1,IM
        DO J=1,JM
            HMAX(I,J) = HPRE(I,J)
            HMIN(I,J) = HPRE(I,J)
!
            DO IT=-1,1
                DO JT=-1,1
!
                    IF (HOLD(I+IT,J+JT) > 0) THEN
                        HMAX(I,J) = MAX(HPRE(I+IT,J+JT), HMAX(I,J))
                        HMAX(I,J) = MAX(HOLD(I+IT,J+JT), HMAX(I,J))
!
                        IF (HPRE(I+IT,J+JT) > 0) THEN
                            HMIN(I,J) = MIN(HPRE(I+IT,J+JT), HMIN(I,J))
                        END IF
!
                        IF (HOLD(I+IT,J+JT) > 0) THEN
                            HMIN(I,J) = MIN(HOLD(I+IT,J+JT), HMIN(I,J))
                        END IF
!
                    END IF
!
                END DO
            END DO
!
        END DO
    END DO
!
    DO I=1,IM
        DO J=1,JM
            FOUT = -FXN_MN(I-1,J  ) + FXN_PL(I  ,J-1) - FYN_MN(I-1,J-1) + FYN_PL(I  ,J  )
            FIN  = -FXN_MN(I  ,J-1) + FXN_PL(I-1,J  ) - FYN_MN(I  ,J  ) + FYN_PL(I-1,J-1)
!
              BETA_UP(I,J) = (HMAX(I,J) - HPRE(I,J)) / (-FDIV(I,J) * FIN  + EPSP)
            BETA_DOWN(I,J) = (HPRE(I,J) - HMIN(I,J)) / (-FDIV(I,J) * FOUT + EPSP)
        END DO
    END DO
!
    DO I=1,IM
        DO J=1,JM
            CXPL(I,J) = MIN(1., BETA_DOWN(I,J+1),   BETA_UP(I+1,J  ))
            CXMN(I,J) = MIN(1.,   BETA_UP(I,J+1), BETA_DOWN(I+1,J  ))
            CYPL(I,J) = MIN(1., BETA_DOWN(I,J  ),   BETA_UP(I+1,J+1))
            CYMN(I,J) = MIN(1.,   BETA_UP(I,J  ), BETA_DOWN(I+1,J+1))
        END DO
    END DO
!
    DO I=1,IM
        DO J=1,JM
            ANTIDX(I,J) = CXPL(I,J) * FXN_PL(I,J) + CXMN(I,J) * FXN_MN(I,J)
            ANTIDY(I,J) = CYPL(I,J) * FYN_PL(I,J) + CYMN(I,J) * FYN_MN(I,J)
        END DO
    END DO
!
    END SUBROUTINE FLUX_CORRECTION

