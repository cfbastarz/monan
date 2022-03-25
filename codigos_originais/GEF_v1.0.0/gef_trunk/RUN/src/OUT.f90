!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief OUTPUT OF FIELDS
!> @details PRINT IN A SEPARATE FILE FOR EACH PE AND EACH OUTPUT PERIOD.
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
!> @details <b>Use Module:</b>
!! @arg @c ACMCLH
!! @arg @c ACMPRE
!! @arg @c ACMRDL
!! @arg @c ACMRDS
!! @arg @c CTLBLK
!! @arg @c DGNSOUT
!! @arg @c F77KINDS
!! @arg @c LOOPS
!! @arg @c MPPSTAFF
!! @arg @c PARMETA
!! @arg @c PHYS
!! @arg @c PVRBLS
!! @arg @c SOIL
!! @arg @c VRBLS
!> @details <b>Driver:</b> 
!<
!> @details <b>Calls:</b>
!! @arg @c OUTSD
!--------------------------------------------------------------------------------------------------    
    SUBROUTINE OUT
!-------------------------------------------------------------------------------------------------- 
! SUBROUTINE OUT
!
! SUBROUTINE: OUT - OUTPUT OF FIELDS
! PROGRAMMER: ?????  
! ORG: ?????
! DATE: ??-??-??
!
! ABSTRACT: 
! PRINT IN A SEPARATE FILE FOR EACH PE AND EACH OUTPUT PERIOD. 
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
! NONE
!
! OUTPUT ARGUMENT LIST:
! NONE
!
! INPUT/OUTPUT ARGUMENT LIST:
! NONE
!
! OUTPUT FILES:
! OUTPUT - PRINT FILE.
!
! USE MODULES: ACMCLH
!              ACMPRE
!              ACMRDL
!              ACMRDS
!              CTLBLK
!              DGNSOUT
!              F77KINDS
!              LOOPS
!              MPPSTAFF
!              PARMETA
!              PHYS
!              PVRBLS
!              SOIL
!              VRBLS
!
! DRIVER     : -----
!
! CALLS      : OUTSD 
!--------------------------------------------------------------------------------------------------
    USE ACMCLH
    USE ACMPRE
    USE ACMRDL
    USE ACMRDS
    USE CTLBLK
    USE DGNSOUT
    USE F77KINDS
    USE LOOPS
    USE MPPSTAFF
    USE PARMETA
    USE PHYS
    USE PVRBLS
    USE SOIL
    USE VRBLS
!
    IMPLICIT NONE
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1)                                             ::&
    & POUT    ,TMPSFC
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1, LSD)                                        ::&
    & TOUT    , FOUT    , UOUT    , VOUT    , QOUT    , WOUT
!
    CHARACTER(LEN=3)                                                                            ::&
    & C_NHRS
!
    CHARACTER(LEN=80)                                                                           ::&
    & OUTFILE
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1)                                             ::&
    & DUM2
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1, LSD)                                        ::&
    & DUM3
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1, 12)                                         ::&
    & DUM4
!
    REAL   (KIND=R4)                                                                            ::&
    & FACTRS  , FACTRL  , TLMH    , RRNUM
!
    INTEGER(KIND=I4)                                                                            ::&
    & I       , J       , LLMH    , K       , L
!
    DO I=0,IM+1
        DO J=0,JM+1
            IF(LMH(I,J) /= 0) THEN
                TMPSFC(I,J) = T(I,J,LMH(I,J))
            END IF
        END DO
    END DO
! 
    NHRS = NHRS + 1
    WRITE (C_NHRS,'(I3.3)') NHRS
!       
    OUTFILE = "GEFfcst_"//C_MYPE//"."//C_NHRS
!
    OPEN(UNIT=LISTOUT, FILE= TRIM(OUTFILE), FORM = 'UNFORMATTED')
!
    CALL OUTSD(POUT, TOUT, FOUT, UOUT, VOUT ,QOUT,WOUT)
!   
    WRITE(LISTOUT) POUT
    WRITE(LISTOUT) FOUT
    WRITE(LISTOUT) TOUT
    WRITE(LISTOUT) UOUT
    WRITE(LISTOUT) VOUT
    WRITE(LISTOUT) QOUT
    WRITE(LISTOUT) WOUT
    WRITE(LISTOUT) ACPREC
    WRITE(LISTOUT) CUPREC
    WRITE(LISTOUT) ACSNOW
!
    ACPREC = 0.
    CUPREC = 0.
    ACSNOW = 0.
    ACSNOM = 0.
!--------------------------------------
! CURRENT SURFACE INCOMING SW RADIATION
!--------------------------------------
    DO I=0,IM+1
	DO J=0,JM+1
            IF (CZMEAN(I,J) > 1.E-6) THEN
                FACTRS = CZEN(I,J) / CZMEAN(I,J)
            ELSE
                FACTRS = 0.0
            END IF
!
            DUM2(I,J) = RSWIN(I,J) * FACTRS
!
        END DO
    END DO
!
    DO I=0,IM+1
        DO J=0,JM+1
           IF (CZMEAN(I,J) > 1.E-6) THEN
               FACTRS = CZEN(I,J) / CZMEAN(I,J)
           ELSE
               FACTRS = 0.0
           END IF
!
           DUM2(I,J) = RSWOUT(I,J) * FACTRS
        END DO
    END DO
!
    IF (ARDSW > 0.) THEN
        RRNUM = 1. / ARDSW
    ELSE
        RRNUM = 0.
    END IF
!
    DUM2 = ASWTOA * RRNUM
!
    DO I=0,IM+1
	DO J=0,JM+1
            IF (SIGT4(I,J) > 0.0) THEN
                LLMH   = LMH(I,J)
                TLMH   = T(I,J,LLMH)
                FACTRL = 5.67E-8 * TLMH * TLMH * TLMH * TLMH / SIGT4(I,J)
            ELSE
                FACTRL = 0.0
            END IF
!
            DUM2(I,J) = RLWIN(I,J) * FACTRL
!
        END DO
    END DO
!
    IF (ARDLW > 0.) THEN
        RRNUM = 1. / ARDLW
    ELSE
        RRNUM = 0.
    END IF
!
    DUM2 = ALWTOA * RRNUM
!
    CLOSE (LISTOUT)
!
    END SUBROUTINE OUT
     
