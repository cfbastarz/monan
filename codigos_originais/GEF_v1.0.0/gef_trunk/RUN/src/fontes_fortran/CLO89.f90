!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief COMPUTES CLOUD TRANSMISSION FUNCTIONS FOR THE LONGWAVE CODE
!> @details COMPUTES CLOUD TRANSMISSION FUNCTIONS FOR THE LONGWAVE CODE, USING CODE WRITTEN BY BERT
!! KATZ (301-763-8161) AND MODIFIED BY DAN SCHWARZKOPF IN DECEMBER, 1988.
!> @author ORIGINATOR - Q. ZHAO 
!> @date 95-03-22 \n
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
!> @param[in] CAMT - Significado de CAMT
!> @param[in] KTOP - Significado de KTOP
!> @param[in] KBTM - Significado de KBTM
!> @param[in] NCLDS - Significado de NCLDS
!> @param[out] CLDFAC - Significado de CLDFAC
!> @details <b>Use Module:</b> 
!! @arg @c F77KINDS
!! @arg @c HCON
!! @arg @c PARMETA
!! @arg @c RDPARM
!> @details <b>Driver:</b> 
!! @arg @c RADFS
!> @details <b>Calls:</b>
!<
!--------------------------------------------------------------------------------------------------
    SUBROUTINE CLO89(CLDFAC, CAMT, NCLDS, KBTM, KTOP)
!--------------------------------------------------------------------------------------------------
! SUBROUTINE CLO89
!
! SUBPROGRAM: CLO89 - COMPUTES CLOUD TRANSMISSION FUNCTIONS FOR THE LONGWAVE CODE
! PROGRAMMER: Q. ZHAO
! ORG: W/NP2
! DATE: 95-03-22
!
! ABSTRACT: 
! COMPUTES CLOUD TRANSMISSION FUNCTIONS FOR THE LONGWAVE CODE, USING CODE WRITTEN BY BERT
! KATZ (301-763-8161) AND MODIFIED BY DAN SCHWARZKOPF IN DECEMBER, 1988.
!
! PROGRAM HISTORY LOG:
! 95-03-22  Q. ZHAO  - ORIGINATOR            
! 18-03-20  LUCCI    - MODERNIZATION OF THE CODE, INCLUDING:
!                      * F77 TO F90/F95
!                      * INDENTATION & UNIFORMIZATION CODE
!                      * REPLACEMENT OF COMMONS BLOCK FOR MODULES
!                      * DOCUMENTATION WITH DOXYGEN
!                      * OPENMP FUNCTIONALITY
!
! INPUT  ARGUMENT LIST:
! CAMT   - CLOUD FRACTION OF EACH CLOUD LAYER
! KTOP   - HEIGHT OF CLOUD TOP OF EACH CLOUD LAYER (IN ETA LEVELS)
! KBTM   - BOTTOM OF EACH CLOUD LAYER
! NCLDS  - NUMBER OF CLOUD LAYERS
!
! OUTPUT ARGUMENT LIST:
! CLDFAC - CLOUD TRANSMISSION FUNCTIONS FOR THE LONGWAVE CODE
!
! INPUT/OUTPUT ARGUMENT LIST:
! NONE
!
! USE MODULES: F77KINDS
!              HCON
!              PARMETA
!              RDPARM
! 
! DRIVER     : RADFS
!
! CALLS      : -----
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
    USE HCON
    USE PARMETA
    USE RDPARM
!
    IMPLICIT NONE
!
    INTEGER(KIND=I4)                                                                            ::&
    & IQ      , ITOP    , JTOP    , IP      , IR      , J       , I       , K1      , K2      ,   &
    & KB      , K       , KP      , KT      , NC
!
    REAL   (KIND=R4)                                                                            ::&
    & XCLD
!
    INTEGER(KIND=I4)    , DIMENSION(IM)                                   , INTENT(IN)          ::&
    & NCLDS
!
    INTEGER(KIND=I4)    , DIMENSION(IM,LP1)                               , INTENT(IN)          ::&
    & KTOP    , KBTM
!
    REAL   (KIND=R4)    , DIMENSION(IM,LP1)                               , INTENT(IN)          ::&
    & CAMT
!
    REAL   (KIND=R4)    , DIMENSION(IM,LP1,LP1)                           , INTENT(OUT)         ::&
    & CLDFAC
!
    REAL   (KIND=R4)    , DIMENSION(LP1)                                                        ::&
    & CLDROW
! 
    REAL   (KIND=R4)    , DIMENSION(LP1, LP1, 64)                                               ::&
    & CLDIPT
 !
    DO IQ=1,IM,64
        ITOP = IQ + 63
!
        IF (ITOP > IM) ITOP = IM
!
        JTOP = ITOP - IQ + 1
!
        DO IP=1,JTOP
            IR = IQ + IP - 1
!
            IF (NCLDS(IR) == 0) THEN
                DO J=1,LP1
                    DO I=1,LP1
                        CLDIPT(I,J,IP) = 1.
                    END DO
                END DO 
            END IF
!
            IF (NCLDS(IR) >= 1) THEN
                XCLD = 1. - CAMT(IR,2)
                K1   = KTOP(IR,2) + 1
                K2   = KBTM(IR,2)
!
                DO J=1,LP1
                    CLDROW(J) = 1.
                END DO
!
                DO J=1,K2
                    CLDROW(J) = XCLD
                END DO
!
                KB = MAX(K1, K2+1)
!
                DO K=KB,LP1
                    DO KP=1,LP1
                        CLDIPT(KP,K,IP) = CLDROW(KP)
                    END DO
                END DO
!
                DO J=1,LP1
                    CLDROW(J) = 1.
                END DO
!
                DO J=K1,LP1
                    CLDROW(J) = XCLD
                END DO
!
                KT = MIN(K1-1, K2)
!
                DO K=1,KT
                    DO KP=1,LP1
                        CLDIPT(KP,K,IP) = CLDROW(KP)
                    END DO
                END DO
!    
                IF (K2+1 <= K1-1) THEN
!
                    DO J=K2+1,K1-1
                        DO I=1,LP1
                            CLDIPT(I,J,IP) = 1.
                        END DO
                    END DO
!
                ELSE IF (K1 <= K2) THEN
!
                    DO J=K1,K2
                        DO I=1,LP1
                            CLDIPT(I,J,IP) = XCLD
                        END DO
                    END DO
!
                END IF
!    
            END IF
!
            IF (NCLDS(IR) >= 2) THEN
!
                DO NC=2,NCLDS(IR)
                    XCLD = 1. - CAMT(IR,NC+1)
                    K1 = KTOP(IR,NC+1) + 1
                    K2 = KBTM(IR,NC+1)
!
                    DO J=1,LP1
                        CLDROW(J) = 1.
                    END DO
!
                    DO J=1,K2
                        CLDROW(J) = XCLD
                    END DO
!
                    KB = MAX(K1, K2+1)
!
                    DO K=KB,LP1
                        DO KP=1,LP1
                            CLDIPT(KP,K,IP) = CLDIPT(KP,K,IP) * CLDROW(KP)
                        END DO
                    END DO
!
                    DO J=1,LP1
                        CLDROW(J) = 1.
                    END DO
!
                    DO J=K1,LP1
                        CLDROW(J) = XCLD
                    END DO
!
                    KT = MIN(K1-1, K2)
!
                    DO K=1,KT
                        DO KP=1,LP1
                            CLDIPT(KP,K,IP) = CLDIPT(KP,K,IP) * CLDROW(KP)
                        END DO
                    END DO
!
                    IF (K1 <= K2) THEN
!
                        DO J=K1,K2
                            DO I=1,LP1
                                CLDIPT(I,J,IP) = CLDIPT(I,J,IP) * XCLD
                            END DO
                        END DO
!
                    END IF

                END DO
!
            END IF
!
        END DO
!
        DO J=1,LP1
            DO I=1,LP1
                DO IP=1,JTOP
                    IR = IQ + IP - 1
                    CLDFAC(IR,I,J) = CLDIPT(I,J,IP)
                END DO
            END DO
        END DO
!
    END DO
!
    RETURN
!
    END SUBROUTINE CLO89
