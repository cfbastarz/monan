!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief MAIN PROGRAM 
!> @details MAIN PROGRAM OF GLOBAL ETA FRAMEWORK
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
!! @arg @c CONTIN
!! @arg @c CTLBLK
!! @arg @c DGNSOUT
!! @arg @c F77KINDS
!! @arg @c LOOPS 
!! @arg @c MASKS
!! @arg @c MPPSTAFF
!! @arg @c NHYDRO
!! @arg @c PARMETA
!! @arg @c PHYS
!! @arg @c PVRBLS
!! @arg @c SOIL
!! @arg @c VRBLS
!> @details <b>Driver:</b> 
!<
!> @details <b>Calls:</b>
!! @arg @c ADJUST
!! @arg @c ALLOC
!! @arg @c CHECKMXMN
!! @arg @c CLTEND
!! @arg @c DDAMP
!! @arg @c FINISHMPI
!! @arg @c GSMDRIVE
!! @arg @c HDIFF
!! @arg @c HZADV
!! @arg @c HZADVQ
!! @arg @c INIT
!! @arg @c INITDOM
!! @arg @c INITMPI
!! @arg @c INITTOP1
!! @arg @c INITTOP2
!! @arg @c KFDRIVE
!! @arg @c KFTEND
!! @arg @c OUT2
!! @arg @c OUT_HEAT
!! @arg @c PDETE
!! @arg @c PDNEW
!! @arg @c RADTN
!! @arg @c RDTEMP
!! @arg @c SSTSTP
!! @arg @c TURBL
!! @arg @c VTADV
!--------------------------------------------------------------------------------------------------
    PROGRAM GEF
!--------------------------------------------------------------------------------------------------
! MAIN PROGRAM GEF
! 
! MAIN PROGRAM: GEF - GLOBAL VERSION OF ETA FRAMEWORK
! PROGRAMMER: ?????          
! ORG: W/NP22
! DATE: ??-??-??
!
! ABSTRACT:
! GLOBAL VERSION OF ETA MODEL ON CUBIC GRID
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
! USE MODULES: CONTIN
!              CTLBLK
!              DGNSOUT
!              F77KINDS
!              LOOPS
!              MASKS
!              MPPSTAFF
!              NHYDRO
!              PARMETA
!              PHYS
!              PVRBLS
!              SOIL
!              VRBLS 
! 
! DRIVER     : -----
!
! CALLS      : ADJUST
!              ALLOC
!              CHECKMXMN
!              CLTEND
!              DDAMP
!              FINISHMPI
!              GSMDRIVE
!              HDIFF
!              HZADV
!              HZADVQ
!              INIT
!              INITDOM
!              INITMPI
!              INITTOP1
!              INITTOP2
!              KFDRIVE
!              KFTEND
!              OUT2
!              OUT_HEAT
!              PDETE
!              PDNEW
!              RADTN
!              RDTEMP
!              SSTSTP
!              TURBL
!              VTADV		   
!--------------------------------------------------------------------------------------------------
    USE CONTIN
    USE CTLBLK
    USE DGNSOUT
    USE DYNAM
    USE F77KINDS
    USE LOOPS 
    USE MASKS   
    USE MPPSTAFF
    USE NHYDRO
    USE PARMETA
    USE PHYS
    USE PVRBLS
    USE SOIL    
    USE VRBLS
!    
    IMPLICIT NONE
!
    INCLUDE "mpif.h"
!
    LOGICAL(KIND=L4)                                                                            ::&
    & LOUT    , LADV    , LADH    , LCKMM   , LPHYS   , LCHSST  , LVSST   , LDASS   , LVEG 
!
    INTEGER(KIND=I4)                                                                            ::&
    & I       , J       , L       , IERR    , ICLTEND , NTSDDAY , ND      , M       
!
    INTEGER(KIND=I4)                                                                            ::&
    &    I18, I21, I3, I5, I6, I7, I9, I10, I11, I12, I13, I14, I15, I16, I17, I19, I20,   &
    &    J18, J21, J3, J5, J6, J7, J9, J10, J11, J12, J13, J14, J15, J16, J17, J19, J20,   &
    & K, K18, K21, K3, K5, K6, K7, K9, K10, K11, K12, K13, K14, K15, K16, K17, K19, K20
!
    CHARACTER(LEN=30)                                                                           ::&
    & NOME
!
    LVSST = .FALSE.
    LVEG  = .FALSE.  
    HYDRO = .TRUE.
!---------------
! INITIALIZE MPI
!---------------
    CALL INITMPI
!
    IF (MYPE < (NPES)) THEN
!
        CALL ALLOC
!
        CALL ZERO
!
        CALL INITDOM
!
        IF (NM == 6) THEN
            CALL INITTOP1
        ELSE IF (NM == 14) THEN
            CALL INITTOP2
        END IF
!---------------
! INITIALIZATION
!---------------
        CALL INIT
!
       IF (.NOT. RESTRT) THEN
	CALL OUT2
       ENDIF

!
        NTSDDAY = 3600 * 24 / DT
!-------------------------------------------------------- 
! SPECIAL CONSIDERATION WHEN NTSD = 0 AT START OF FORECAST
!-------------------------------------------------------- 
        IF (NTSD == 0) NTSD = 1
!-------------------------
! ENTRY INTO THE TIME LOOP
!-------------------------   
!        DO NTSD = 1,NTSTM       

!RESTART
          IF (.NOT. RESTRT) THEN
	      NTSD_INIT = 1
	  ELSE  
	      NTSD_INIT = NTSD + 1
	  ENDIF	
!!!	  
	  
	  
         DO NTSD = NTSD_INIT, NTSTM


!
            IF (MYPE == 0) THEN
                WRITE(0,*) ' NTSD= ', NTSD
            END IF
!
            LOUT   = MOD(NTSD - OUTSTR  , IOUT   ) == 0 .AND. NTSD >= OUTSTR
!            LADV   = MOD(NTSD - 1       , IDTAD  ) == 0    !DRAGAN 25/06/2019
            LADV   = MOD(NTSD            , IDTAD  ) == 0
            LADH   = MOD(NTSD           , IDTAD  ) == 0
            LCKMM  = MOD(NTSD           , 2      ) == 0
            LPHYS  = MOD(NTSD           , NPHS   ) == 0
            LCHSST = MOD(NTSD           , NTSDDAY) == 0
!
            LDASS  = NTSD <= NDASSIMM
	    
	    
!
!            IF (MYPE == 0) THEN
!                WRITE(0,*) ' LOUT= ', LOUT, IOUT, NTSD
!                WRITE(0,*) ' LADV= ', LADV, IDTAD, NTSD
!                WRITE(0,*) ' LADH= ', LADH, IDTAD, NTSD
!                WRITE(0,*) ' LCKMM= ', LCKMM, NTSD
!                WRITE(0,*) ' LPHYS= ', LPHYS, NPHS, NTSD
!                WRITE(0,*) ' LCHSST= ', LCHSST, NTSDDAY, NTSD
!            END IF	    
!	    
!-----------------
! SST-DAILY UPDATE
!-----------------
            IF (LVSST .AND. LCHSST) THEN   
                ND = NTSD / NTSDDAY
                PRINT *, 'ND=', ND, MYPE
!             
                CALL SSTSTP()
            END IF
!-------------------------- 
! VEG GREENESS-DAILY UPDATE 
!--------------------------
            IF (LVEG .AND. LCHSST) THEN
                ND = NTSD / NTSDDAY    
!        
                CALL VEGUPDT
!
            END IF
!
            DO I17 = 1,IM
              DO J17 = 1,JM
                DO K17 = 1,LM
                  IF (T(I17,J17,K17) > 330) THEN 
                    WRITE(*,*) "ANTES DO ADJUST"
                    WRITE(*,*) NTSD, MYPE, I17, J17, K17, T(I17,J17,K17)
                    WRITE(*,*) ""
                  END IF
                END DO
              END DO
           END DO
!
            CALL ADJUST
!
            DO I18 = 1,IM
              DO J18 = 1,JM
                DO K18 = 1,LM
                  IF (T(I18,J18,K18) > 330) THEN 
                    WRITE(*,*) "ADJUST"
                    WRITE(*,*) NTSD, MYPE, I18, J18, K18, T(I18,J18,K18)
                    WRITE(*,*) ""
                  END IF
                END DO
              END DO
           END DO               
! 
            CALL  PDETE
!
            DO I21 = 1,IM
              DO J21 = 1,JM
                DO K21 = 1,LM
                  IF (T(I21,J21,K21) > 330) THEN 
                    WRITE(*,*) "PDETE"
                    WRITE(*,*) NTSD, MYPE, I21, J21, K21, T(I21,J21,K21)
                    WRITE(*,*) ""
                  END IF
                END DO
              END DO
           END DO
!-------------------------------------------------------
! DO VERTICAL ADVECTION WITHIN THE FIRST ADJUSTMENT STEP
!-------------------------------------------------------	    
            IF (LADV) CALL VTADV
!
            DO I3 = 1,IM
              DO J3 = 1,JM
                DO K3 = 1,LM
                  IF (T(I3,J3,K3) > 330) THEN 
                    WRITE(*,*) "VTADV"
                    WRITE(*,*) NTSD, MYPE, I3, J3, K3, T(I3,J3,K3)
                    WRITE(*,*) ""
                  END IF
                END DO
              END DO
           END DO
!-----------------------------
! UPDATING PRESSURE DIFFERENCE
!-----------------------------
            CALL PDNEW
!
            DO I19 = 1,IM
              DO J19 = 1,JM
                DO K19 = 1,LM
                  IF (T(I19,J19,K19) > 330) THEN 
                    WRITE(*,*) "PDNEW"
                    WRITE(*,*) NTSD, MYPE, I19, J19, K19, T(I19,J19,K19)
                    WRITE(*,*) ""
                  END IF
                END DO
              END DO
           END DO
!-------------------
! DIVERGENCE DAMPING
!-------------------
            CALL DDAMP
!
            DO I5 = 1,IM
              DO J5 = 1,JM
                DO K5 = 1,LM
                  IF (T(I5,J5,K5) > 330) THEN 
                    WRITE(*,*) "DDAMP"
                    WRITE(*,*) NTSD, MYPE, I5, J5, K5, T(I5,J5,K5)
                    WRITE(*,*) ""
                  END IF
                END DO
              END DO
           END DO
!----------------------------------------------------------------------------------------------
! LATERAL DIFFUSION -- HDIFF WAS COMMENTED IN GEF VERSION V1.1.0 AND INSERTED A SHAPIRO FILTER 
!----------------------------------------------------------------------------------------------
!GSM	       CALL HDIFF

!GSM	    DO I6 = 1,IM
!GSM	      DO J6 = 1,JM
!GSM		DO K6 = 1,LM
!GSM		  IF (T(I6,J6,K6) > 330) THEN 
!GSM		    WRITE(*,*) "HDIFF"
!GSM		    WRITE(*,*) NTSD, MYPE, I6, J6, K6, T(I6,J6,K6)
!GSM		    WRITE(*,*) ""
!GSM		  END IF
!GSM		END DO
!GSM	     END DO
!GSM	   END DO
!----------------------------------------
! HORIZONTAL ADVECTION OF WATER SUBSTANCE 
!----------------------------------------
            IF (LADH) THEN
!
                CALL HZADVQ
!
            DO I7 = 1,IM
              DO J7 = 1,JM
                DO K7 = 1,LM
                  IF (T(I7,J7,K7) > 330) THEN 
                    WRITE(*,*) "HZADVQ"
                    WRITE(*,*) NTSD, MYPE, I7, J7, K7, T(I7,J7,K7)
                    WRITE(*,*) ""
                  END IF
                END DO
              END DO
           END DO
!---------------------
! HORIZONTAL ADVECTION
!---------------------	    
                CALL HZADV
!
            DO I20 = 1,IM
              DO J20 = 1,JM
                DO K20 = 1,LM
                  IF (T(I20,J20,K20) > 330) THEN 
                    WRITE(*,*) "HZADV"
                    WRITE(*,*) NTSD, MYPE, I20, J20, K20, T(I20,J20,K20)
                    WRITE(*,*) ""
                  END IF
                END DO
              END DO
           END DO
!
!---------------------------
! SHAPIRO FILTER FOR U AND V
!---------------------------
                CALL SHAPIRO
!
            END IF
!

!
!GSM            IF (MOD(NTSD, NRADS) == 1 .OR. MOD(NTSD, NRADL) == 1) THEN 
            IF (MOD(NTSD, NRADS) == 0 .OR. MOD(NTSD, NRADL) == 0 .OR. NTSD == 1) THEN
	    
!
!            IF (MYPE == 0) THEN
!                WRITE(0,*) ' NRADS= ', MOD(NTSD, NRADS), NTSD
!                WRITE(0,*) ' NRADL= ', MOD(NTSD, NRADL), NTSD
!            END IF	    
!	    
!
                CALL RADTN
!
            DO I9 = 1,IM
              DO J9 = 1,JM
                DO K9 = 1,LM
                  IF (T(I9,J9,K9) > 330) THEN 
                    WRITE(*,*) "RADTN"
                    WRITE(*,*) NTSD, MYPE, I9, J9, K9, T(I9,J9,K9)
                    WRITE(*,*) ""
                  END IF
                END DO
              END DO
           END DO 
!
            END IF
!
            CALL RDTEMP
!
            DO I10 = 1,IM
              DO J10 = 1,JM
                DO K10 = 1,LM
                  IF (T(I10,J10,K10) > 330) THEN 
                    WRITE(*,*) "RDTEMP"
                    WRITE(*,*) NTSD, MYPE, I10, J10, K10, T(I10,J10,K10)
                    WRITE(*,*) ""
                  END IF
                END DO
              END DO
           END DO 
!--------------------------------------------------------------------------------------------------
! IF THE TIME IS RIGHT, NOW DO VARIOUS PHYSICS CALLS (WARNING: TO AVOID ENDING THE INTEGRATION WITH
! PHYSICS CALLS WHICH HAVE NOT BEEN FOLLOWED BY ADJUSTMENT STEPS, PHYSICS CALLS ARE OFFSET BY 
! HALVES OF VARIOUS CALLING INTERVALS.
! IT IS ASSUMED THAT THE CALLING INTERVALS, NPHS AND NCNVC, ARE DIVISIBLE BY IDTAD.
! IF NOT, INTEGRATION WILL END WITH AN INCORRECT NUMBER OF CALLS HAVING BEEN MADE.
!--------------------------------------------------------------------------------------------------
!
!--------------------------------------
! TURBULENT PROCESSES AND PRECIPITATION 
!--------------------------------------
            IF (LPHYS) THEN
!
                CALL TURBL
!
            DO I11 = 1,IM
              DO J11 = 1,JM
                DO K11 = 1,LM
                  IF (T(I11,J11,K11) > 330) THEN 
                    WRITE(*,*) "TURBL"
                    WRITE(*,*) NTSD, MYPE, I11, J11, K11, T(I11,J11,K11)
                    WRITE(*,*) ""
                  END IF
                END DO
              END DO
           END DO
!---------------------------------
! STORE ORIGINAL TEMPERATURE ARRAY
!---------------------------------
                ICLTEND = -1
!
                CALL CLTEND(ICLTEND)
!
            DO I12 = 1,IM
              DO J12 = 1,JM
                DO K12 = 1,LM
                  IF (T(I12,J12,K12) > 330) THEN 
                    WRITE(*,*) "CLTEND"
                    WRITE(*,*) NTSD, MYPE, I12, J12, K12, T(I12,J12,K12)
                    WRITE(*,*) ""
                  END IF
                END DO
              END DO
           END DO 
!														
                CALL GSMDRIVE
!
            DO I13 = 1,IM
              DO J13 = 1,JM
                DO K13 = 1,LM
                  IF (T(I13,J13,K13) > 330) THEN 
                    WRITE(*,*) "GSMDRIVE"
                    WRITE(*,*) NTSD, MYPE, I13, J13, K13, T(I13,J13,K13)
                    WRITE(*,*) ""
                  END IF
                END DO
              END DO
           END DO 
!
                ICLTEND = 0
!
                CALL CLTEND(ICLTEND)
!
                CALL KFDRIVE
!
                CALL KFTEND
!
            DO I14 = 1,IM
              DO J14 = 1,JM
                DO K14 = 1,LM
                  IF (T(I14,J14,K14) > 330) THEN 
                    WRITE(*,*) "KFTEND"
                    WRITE(*,*) NTSD, MYPE, I14, J14, K14, T(I14,J14,K14)
                    WRITE(*,*) ""
                  END IF
                END DO
              END DO
           END DO 
!             
            END IF
!
!            DO I3 = 1,IM
!              DO J3 = 1,JM
!                DO K3 = 1,LM
!                  IF (T(I3,J3,K3) > 330.0 .OR. (NTSD > 25500 .AND. MYPE == 436 .AND. I3 == 39 .AND. J3 == 32 .AND. K3 ==38)) THEN
!
!                    WRITE(*,*) NTSD, MYPE, I3, J3, K3, &
!&                                T(I3,J3,K3),          &
!&                              HTM(I3,J3,K3),          &
!&                                Q(I3,J3,k3),          &
!&                                U(I3,J3,K3),          &
!&                                V(I3,J3,K3),          &
!&                               SM(I3,J3)   ,          &
!&                             SICE(I3,J3)   ,          &
!&                            HBMSK(I3,J3)   ,          &
!&                              LMH(I3,J3)   ,          &
!&                               PD(I3,J3)
!
!                  END IF
!               end do
!             end do
!          end do
!------------------------------------------------------------
! UPDATE TEMP TENDENCIES FROM CLOUD PROCESSES EVERY TIME STEP
!------------------------------------------------------------
            ICLTEND = 1
!
            CALL CLTEND(ICLTEND)
!
            DO I15 = 1,IM
              DO J15 = 1,JM
                DO K15 = 1,LM
                  IF (T(I15,J15,K15) > 330) THEN 
                    WRITE(*,*) "CLTEND 2"
                    WRITE(*,*) NTSD, MYPE, I15, J15, K15, T(I15,J15,K15)
                    WRITE(*,*) ""
                  END IF
                END DO
              END DO
           END DO 
!
            IF (LOUT) THEN
                IF (MYPE == 0) WRITE(0,*) ' OUTPUT AT ', (NTSD * DT) / 3600.0
                CALL OUT2
            END IF
!
            IF (LCKMM) THEN
                CALL CHECKMXMN
!
            DO I16 = 1,IM
              DO J16 = 1,JM
                DO K16 = 1,LM
                  IF (T(I16,J16,K16) > 330) THEN 
                    WRITE(*,*) "CHECKMXMN"
                    WRITE(*,*) NTSD, MYPE, I16, J16, K16, T(I16,J16,K16)
                    WRITE(*,*) ""
                  END IF
                END DO
              END DO
           END DO
!
           END IF
!---------------------------------------
! CLOSE THE TIME LOOP: DO NTSD = 1,NTSTM
!---------------------------------------
        END DO                
!----------------------------------
! CLOSE IF: IF (MYPE < (NPES)) THEN
!----------------------------------
    END IF
!
!GSM    CALL OUT_HEAT
!
    CALL FINISHMPI
!--------------------
! SUCESSFUL INTEGRATION END MESSAGE
!-------------------- 
    IF (MYPE == 0) THEN
    WRITE(0,*) 'GEF FINISHED SUCCESSFULLY'
    END IF
!
    END PROGRAM GEF
