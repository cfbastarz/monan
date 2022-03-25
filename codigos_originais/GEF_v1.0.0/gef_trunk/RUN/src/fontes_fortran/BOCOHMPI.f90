!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief SUPPLY ALL BOUNDARIES
!> @details SUPPLY ALL BOUNDARIES WITH H POINTS: ANY TOPOLOGY (MPI VERSION).
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
!> @param[in] LMAX - Significado de LMAX
!> @param[inout] HARRAY - Significado de HARRAY
!> @details <b>Use Module:</b>   
!! @arg @c CTLBLK
!! @arg @c DOM
!! @arg @c DYNAM
!! @arg @c F77KINDS
!! @arg @c MPPSTAFF
!! @arg @c NEBMAP
!! @arg @c PARMETA
!> @details <b>Driver:</b> 
!! @arg @c HDIFF
!! @arg @c HZADV
!! @arg @c HZADVQ
!! @arg @c INIT
!! @arg @c PDTE
!! @arg @c PDNEW
!! @arg @c RADTN
!! @arg @c RDTEMP
!! @arg @c SHAP_FILTER
!! @arg @c TURBL
!! @arg @c VTADV
!> @details <b>Calls:</b>
!! @arg @c MPI_BARRIER
!! @arg @c MPI_IRECV
!! @arg @c MPI_ISEND
!! @arg @c MPI_WAIT
!! @arg @c ROTCCW90
!! @arg @c ROTCW90
!! @arg @c ROT180 
!<
!--------------------------------------------------------------------------------------------------
    SUBROUTINE BOCOHMPI(HARRAY, LMAX)
!--------------------------------------------------------------------------------------------------
! SUBROUTINE BOCOHMPI
! 
! SUBPROGRAM: BOCOHMPI - SUPPLY ALL BOUNDARIES
! PROGRAMMER: ????? 
! ORG: W/NP22
! DATE: ??-??-??
!
! ABSTRACT:  
! SUPPLY ALL BOUNDARIES WITH H POINTS: ANY TOPOLOGY (MPI VERSION).
!
! PROGRAM HISTORY LOG:
! 87-06-??  ?????   - ORIGINATOR
! 18-03-20  LUCCI   - MODERNIZATION OF THE CODE, INCLUDING:
!                     * F77 TO F90/F95
!                     * INDENTATION & UNIFORMIZATION CODE
!                     * REPLACEMENT OF COMMONS BLOCK FOR MODULES
!                     * DOCUMENTATION WITH DOXYGEN
!                     * OPENMP FUNCTIONALITY 
!
!
! INPUT ARGUMENT LIST:
! LMAX   - 
!
! OUTPUT ARGUMENT LIST:
! NONE
!
! INPUT/OUTPUT ARGUMENT LIST:
! HARRAY - 
!
! INPUT FILES:
! NONE
!
! OUTPUT FILES:
! NONE
!
! USE MODULES: CTLBLK
!              DOM
!              DYNAM
!              F77KINDS
!              MPPSTAFF
!              NEBMAP
!              PARMETA
!
! DRIVER     : HDIFF
!              HZADV
!              HZADVQ
!              INIT
!              PDTE
!              PDNEW
!              RADTN
!              RDTEMP
!              SHAP_FILTER
!              TURBL
!              VTADV
!
! CALLS      : MPI_BARRIER
!              MPI_IRECV
!              MPI_ISEND
!              MPI_WAIT
!              ROTCCW90
!              ROTCW90
!              ROT180
!--------------------------------------------------------------------------------------------------
    USE CTLBLK  
    USE DOM
    USE DYNAM
    USE F77KINDS
    USE MPPSTAFF
    USE NEBMAP
    USE PARMETA
!
    IMPLICIT NONE
!
    INCLUDE "mpif.h"
!
    INTEGER(KIND=I4)                                                      , INTENT(IN)          ::&
    & LMAX
!
    INTEGER(KIND=I4)                                                                            ::&
    IAERR   , IERR    , IDERR   , NDATA   , L       , I       , J
!
    INTEGER(KIND=I4)                                                                            ::&
    & ISEND   , IRECV   , NEBPE
!
    INTEGER(KIND=I4)    , PARAMETER :: IMP1 = IM + 1
    INTEGER(KIND=I4)    , PARAMETER :: JMP1 = IMP1
!
    REAL   (KIND=R4)    , DIMENSION (0:IM+1, 0:JM+1, LMAX)                , INTENT(INOUT)       ::&
    & HARRAY
!
    REAL   (KIND=R4)    , DIMENSION(:,:,:)  , ALLOCATABLE                                       ::&
    & SBUF1   , SBUF2   , SBUF3   , SBUF4   ,                                                     &
    & RBUF1   , RBUF2   , RBUF3   , RBUF4   ,                                                     &
    & TMPBUF
!
    REAL   (KIND=R4)    , DIMENSION(2,2,LMAX)                                                   ::&
    & SBUF1_C , SBUF2_C , SBUF3_C , SBUF4_C ,                                                     &
    & RBUF1_C , RBUF2_C , RBUF3_C , RBUF4_C
!
    INTEGER(KIND=I4)    , DIMENSION(4)                                                          ::&
    & SHANDLE , RHANDLE 
!
    INTEGER(KIND=I4)    , DIMENSION(MPI_STATUS_SIZE)                                            ::&
    & ISTAT
!
    REAL   (KIND=R4)    , DIMENSION(2,2,LMAX)                                                   ::&
    & TMPBUF2
!----------------
! SEND BOUNDARIES
!----------------
    NDATA = IM * LMAX * 2
!-------------
! TOWARD NORTH
!-------------
    IF (MY_NEB(1) >= 0 ) THEN
        NEBPE = MY_NEB(1)
!
        ALLOCATE (TMPBUF(1:IM,2,1:LMAX), STAT = IAERR)
!
        DO L=1,LMAX
            DO I=1,IM
                DO J=JM-1,JM
                    TMPBUF(I,J-JM+2,L) = HARRAY(I,J,L) * HBMSK(I,J)
                END DO
            END DO
        END DO
!
        IF (LINV_NEB2(1) == 1) THEN
!
            ALLOCATE (SBUF1(IM,2,LMAX), STAT = IAERR)
!
            SBUF1 = TMPBUF
!
            ELSE IF (LINV_NEB2(1) == 2) THEN
!
                ALLOCATE (SBUF1(2,IM,LMAX), STAT = IAERR)
!
                CALL ROTCCW90(TMPBUF, SBUF1, IM, 2, LMAX)
!
            ELSE IF (LINV_NEB2(1) == 3) THEN
!
                ALLOCATE (SBUF1(IM,2,LMAX), STAT = IAERR)
!
                CALL ROT180(TMPBUF, SBUF1, IM, 2, LMAX)
            ELSE
!
                ALLOCATE (SBUF1(2,IM,LMAX), STAT = IAERR)
!
                CALL ROTCW90(TMPBUF, SBUF1, IM, 2, LMAX)
!
        END IF
!
        DEALLOCATE (TMPBUF)
!     
        CALL MPI_ISEND(SBUF1, NDATA, ITYPE, NEBPE, MYPE, MPI_COMM_WORLD, SHANDLE(1), ISEND)
!
    END IF
!------------       
! TOWARD EAST
!------------
    IF (MY_NEB(2) >= 0) THEN
        NEBPE = MY_NEB(2)
!
        ALLOCATE (TMPBUF(2,JM,LMAX), STAT = IAERR)
!
        DO L=1,LMAX
            DO I=IM-1,IM
                DO J=1,JM
                TMPBUF(I-IM+2,J,L) = HARRAY(I,J,L) * HBMSK(I,J)
                END DO
            END DO
        END DO
!
        IF (LINV_NEB2(2) == 1) THEN
!
            ALLOCATE (SBUF2(2,JM,LMAX), STAT = IAERR)
!
            SBUF2 = TMPBUF
!
        ELSE IF (LINV_NEB2(2) == 2) THEN
!
            ALLOCATE (SBUF2(JM,2,LMAX), STAT = IAERR)
!
            CALL ROTCCW90(TMPBUF, SBUF2, 2, JM, LMAX)
!
        ELSE IF (LINV_NEB2(2) == 3) THEN
!
            ALLOCATE (SBUF2(2,JM,LMAX), STAT = IAERR)
!
            CALL ROT180(TMPBUF, SBUF2, 2, JM, LMAX)
!
        ELSE
!
            ALLOCATE (SBUF2(JM,2,LMAX), STAT = IAERR)
!
            CALL ROTCW90(TMPBUF, SBUF2, 2, JM, LMAX)
!
        END IF
!
        DEALLOCATE (TMPBUF)
!
        CALL MPI_ISEND(SBUF2, NDATA, ITYPE, NEBPE, MYPE, MPI_COMM_WORLD, SHANDLE(2), ISEND)
!
    END IF
!-------------
! TOWARD SOUTH
!-------------
    IF (MY_NEB(3) >= 0) THEN
        NEBPE = MY_NEB(3)
!
        ALLOCATE (TMPBUF(IM,2,LMAX), STAT = IAERR)
!
        DO L=1,LMAX
            DO I=1,IM
                DO J=1,2
                    TMPBUF(I,J,L) = HARRAY(I,J,L) * HBMSK(I,J)
                END DO
            END DO
        END DO
!
        IF (LINV_NEB2(3) == 1) THEN
!
            ALLOCATE (SBUF3(IM,2,LMAX), STAT = IAERR)
!
            SBUF3 = TMPBUF
!
        ELSE IF (LINV_NEB2(3) == 2) THEN
!
            ALLOCATE (SBUF3(2,IM,LMAX), STAT = IAERR)
!
            CALL ROTCCW90(TMPBUF, SBUF3, IM, 2, LMAX)
!
        ELSE IF (LINV_NEB2(3) == 3) THEN
!
            ALLOCATE (SBUF3(IM,2,LMAX), STAT = IAERR)
!
            CALL ROT180(TMPBUF, SBUF3, IM, 2, LMAX)
!
        ELSE
!
            ALLOCATE (SBUF3(2,IM,LMAX), STAT = IAERR)
!
            CALL ROTCW90(TMPBUF, SBUF3, IM, 2, LMAX)
!
        END IF
!
        DEALLOCATE (TMPBUF)
!
        CALL MPI_ISEND(SBUF3, NDATA, ITYPE, NEBPE, MYPE, MPI_COMM_WORLD, SHANDLE(3), ISEND)
!
    END IF
!------------
! TOWARD WEST 
!------------
    IF (MY_NEB(4) >= 0 ) THEN
        NEBPE = MY_NEB(4)
!
        ALLOCATE (TMPBUF(2,JM,LMAX), STAT = IAERR)
!
        DO L=1,LMAX
            DO I=1,2
                DO J=1,JM
                    TMPBUF(I,J,L) = HARRAY(I,J,L) * HBMSK(I,J)
                END DO
            END DO
        END DO
!
        IF (LINV_NEB2(4) == 1) THEN
!
            ALLOCATE (SBUF4(2,JM,LMAX), STAT = IAERR)
!
            SBUF4 = TMPBUF
!
        ELSE IF (LINV_NEB2(4) == 2) THEN
!
            ALLOCATE (SBUF4(JM,2,LMAX), STAT = IAERR)
!
            CALL ROTCCW90(TMPBUF, SBUF4, 2, JM, LMAX)
!
        ELSE IF (LINV_NEB2(4) == 3) THEN
!
            ALLOCATE (SBUF4(2,JM,LMAX), STAT = IAERR)
!
            CALL ROT180(TMPBUF, SBUF4, 2, JM, LMAX)
!
        ELSE
!
            ALLOCATE (SBUF4(JM,2,LMAX), STAT = IAERR)
!
            CALL ROTCW90(TMPBUF, SBUF4, 2, JM, LMAX)
!
        END IF
!
        DEALLOCATE (TMPBUF)
!
        CALL MPI_ISEND(SBUF4, NDATA, ITYPE, NEBPE, MYPE, MPI_COMM_WORLD, SHANDLE(4), ISEND)
!
    END IF
!-------------------
! RECEIVE BOUNDARIES
!-------------------
!
!-----------
! FROM NORTH
!-----------
    IF (MY_NEB(1) >= 0) THEN
        NEBPE = MY_NEB(1)
!
        ALLOCATE (RBUF1(1:IM,2, 1:LMAX), STAT = IAERR)
!
        CALL MPI_IRECV(RBUF1, NDATA, ITYPE, NEBPE, NEBPE, MPI_COMM_WORLD, RHANDLE(1), IRECV)
!
        CALL MPI_WAIT(RHANDLE(1), ISTAT, IERR)
    END IF
!----------
! FROM EAST
!----------
    IF (MY_NEB(2) >= 0) THEN
        NEBPE = MY_NEB(2)
!
        ALLOCATE (RBUF2(2, 1:IM, 1:LMAX), STAT = IAERR)
!
        CALL MPI_IRECV(RBUF2, NDATA, ITYPE, NEBPE, NEBPE, MPI_COMM_WORLD, RHANDLE(2), IRECV)
!
        CALL MPI_WAIT(RHANDLE(2), ISTAT, IERR)
!
    END IF
!-----------
! FROM SOUTH
!-----------
    IF (MY_NEB(3) >= 0) THEN
        NEBPE = MY_NEB(3)
!
        ALLOCATE (RBUF3(1:IM, 2, 1:LMAX), STAT = IAERR)
!
        CALL MPI_IRECV(RBUF3, NDATA, ITYPE, NEBPE, NEBPE, MPI_COMM_WORLD, RHANDLE(3), IRECV)
!
        CALL MPI_WAIT(RHANDLE(3), ISTAT, IERR)
!
    END IF
!----------
! FROM WEST
!----------
    IF (MY_NEB(4) >= 0) THEN
        NEBPE = MY_NEB(4)
!
        ALLOCATE (RBUF4(2, 1:IM, 1:LMAX), STAT = IAERR)
!
        CALL MPI_IRECV(RBUF4, NDATA, ITYPE, NEBPE, NEBPE, MPI_COMM_WORLD, RHANDLE(4), IRECV)
!
        CALL MPI_WAIT(RHANDLE(4), ISTAT, IERR)
!
    END IF
!---------------------
! DEALLOCATE SBUFFERES
!---------------------
    IF (MY_NEB(1) >= 0) THEN
!
        CALL MPI_WAIT(SHANDLE(1), ISTAT, IERR)
!
        DEALLOCATE (SBUF1, STAT = IERR)
!
    END IF
!
    IF (MY_NEB(2) >= 0 ) THEN
!
        CALL MPI_WAIT(SHANDLE(2), ISTAT, IERR)
!
        DEALLOCATE (SBUF2, STAT = IERR)
!
    END IF
!
    IF (MY_NEB(3) >= 0 ) THEN
!
        CALL MPI_WAIT(SHANDLE(3), ISTAT, IERR)
!
        DEALLOCATE (SBUF3, STAT = IERR)
!
    END IF
!
    IF (MY_NEB(4) >= 0 ) THEN
!
        CALL MPI_WAIT(SHANDLE(4), ISTAT, IERR)
!
        DEALLOCATE (SBUF4, STAT = IERR)
!
    END IF
!
    CALL MPI_BARRIER(MPI_COMM_WORLD, IERR)
!------------- 
! SEND CORNERS
!-------------
!
!------------------
! TOWARD NORTH-EAST
!------------------
    IF (MY_NEB(5) >= 0 ) THEN
        NEBPE = MY_NEB(5)
!
        DO I=IM-1,IM
            DO J=JM-1,JM
                TMPBUF2(I-IM+2,J-JM+2,:) = HARRAY(I,J,:) * HBMSK(I,J)
            END DO
        END DO
!
        IF (CROT_NEB(5) == 1) THEN
            SBUF1_C = TMPBUF2
        ELSE IF (CROT_NEB(5) == 2) THEN
!
            CALL ROTCCW90(TMPBUF2, SBUF1_C, 2, 2, LMAX)
!
        ELSE IF (CROT_NEB(5) == 3) THEN
!
            CALL ROT180(TMPBUF2, SBUF1_C, 2, 2, LMAX)
!
        ELSE
! 
            CALL ROTCW90(TMPBUF2, SBUF1_C, 2, 2, LMAX)
!
        END IF
!
        CALL MPI_ISEND(SBUF1_C, 4*LMAX, ITYPE, NEBPE, MYPE, MPI_COMM_WORLD, SHANDLE(1), ISEND)
!
    END IF
!------------------       
! TOWARD SOUTH-EAST
!------------------
    IF (MY_NEB(6) >= 0) THEN
        NEBPE = MY_NEB(6)
!
        DO I=IM-1,IM
            DO J=1,2
                TMPBUF2(I-IM+2,J,:) = HARRAY(I,J,:) * HBMSK(I,J)
            END DO
        END DO
!
        IF (CROT_NEB(6) == 1) THEN
!
            SBUF2_C = TMPBUF2
!
        ELSE IF (CROT_NEB(6) == 2) THEN
!
            CALL ROTCCW90(TMPBUF2, SBUF2_C, 2, 2, LMAX)
!
        ELSE IF(CROT_NEB(6) == 3) THEN
!
          CALL ROT180(TMPBUF2, SBUF2_C, 2, 2, LMAX)
!
        ELSE
! 
          CALL ROTCW90(TMPBUF2, SBUF2_C, 2, 2, LMAX)
!
        END IF
!
        CALL MPI_ISEND(SBUF2_C, 4*LMAX, ITYPE, NEBPE, MYPE, MPI_COMM_WORLD, SHANDLE(2), ISEND)
!
    END IF
!------------------
! TOWARD SOUTH-WEST
!------------------
    IF (MY_NEB(7) >= 0 ) THEN
        NEBPE = MY_NEB(7)
!
        DO I=1,2
            DO J=1,2
                TMPBUF2(I,J,:) = HARRAY(I,J,:) * HBMSK(I,J)
            END DO
        END DO
!
        IF (CROT_NEB(7) == 1) THEN
!
            SBUF3_C = TMPBUF2
!
        ELSE IF (CROT_NEB(7) == 2) THEN
!
            CALL ROTCCW90(TMPBUF2, SBUF3_C, 2, 2, LMAX)
!
        ELSE IF (CROT_NEB(7) == 3) THEN
!
            CALL ROT180(TMPBUF2, SBUF3_C, 2, 2, LMAX)
!
        ELSE
! 
            CALL ROTCW90(TMPBUF2, SBUF3_C, 2, 2, LMAX)
!
        END IF
!
        CALL MPI_ISEND(SBUF3_C, 4*LMAX, ITYPE, NEBPE, MYPE, MPI_COMM_WORLD, SHANDLE(3), ISEND)
!
    END IF
!------------------
! TOWARD NORTH-WEST
!------------------ 
    IF (MY_NEB(8) >= 0 ) THEN
        NEBPE = MY_NEB(8)
!
        DO I=1,2
            DO J=JM-1,JM
                TMPBUF2(I,J-JM+2,:) = HARRAY(I,J,:) * HBMSK(I,J)
            END DO
        END DO
!
        IF (CROT_NEB(8) == 1) THEN
!
            SBUF4_C = TMPBUF2
!
        ELSE IF (CROT_NEB(8) == 2) THEN
!
            CALL ROTCCW90(TMPBUF2, SBUF4_C, 2, 2, LMAX)
!
        ELSE IF (CROT_NEB(8) == 3) THEN
!
            CALL ROT180(TMPBUF2, SBUF4_C, 2, 2, LMAX)
!
        ELSE
! 
            CALL ROTCW90(TMPBUF2, SBUF4_C, 2, 2, LMAX)
!
        END IF
!
       CALL MPI_ISEND(SBUF4_C, 4*LMAX, ITYPE, NEBPE, MYPE, MPI_COMM_WORLD, SHANDLE(4), ISEND)
!
    END IF
!----------------
! RECEIVE CORNERS
!----------------
!
!----------------
! FROM NORTH-EAST
!----------------
    IF (MY_NEB(5) >= 0 ) THEN
        NEBPE = MY_NEB(5)
!
        CALL MPI_IRECV(RBUF1_C, 4*LMAX, ITYPE, NEBPE, NEBPE, MPI_COMM_WORLD, RHANDLE(1), IRECV)
!
        CALL MPI_WAIT(RHANDLE(1), ISTAT, IERR)
!
    END IF
!----------------
! FROM SOUTH-EAST
!----------------
    IF (MY_NEB(6) >= 0 ) THEN
        NEBPE = MY_NEB(6)
!
        CALL MPI_IRECV(RBUF2_C, 4*LMAX, ITYPE, NEBPE, NEBPE, MPI_COMM_WORLD, RHANDLE(2), IRECV)
!
        CALL MPI_WAIT(RHANDLE(2), ISTAT, IERR)
!
    END IF
!----------------
! FROM SOUTH-WEST
!----------------
    IF (MY_NEB(7) >= 0 ) THEN
        NEBPE = MY_NEB(7)
!
        CALL MPI_IRECV(RBUF3_C, 4*LMAX, ITYPE, NEBPE, NEBPE, MPI_COMM_WORLD, RHANDLE(3), IRECV)
!
        CALL MPI_WAIT(RHANDLE(3), ISTAT, IERR)
!
    END IF
!----------------
! FROM NORTH-WEST
!----------------
    IF (MY_NEB(8) >= 0 ) THEN
        NEBPE = MY_NEB(8)
!
        CALL MPI_IRECV(RBUF4_C, 4*LMAX, ITYPE, NEBPE, NEBPE, MPI_COMM_WORLD, RHANDLE(4), IRECV)
!
        CALL MPI_WAIT(RHANDLE(4), ISTAT, IERR)
!
    END IF 
!-----------
! FROM NORTH
!-----------
    DO I=2,IM-1
        DO J=JM,JM+1
            HARRAY(I,J,:) = HARRAY(I,J,:) * HBMSK(I,J) + (1. - HBMSK(I,J)) * RBUF1(I,J-JM+1,:)
        END DO
    END DO
!----------
! FROM EAST
!----------
    DO I=IM,IM+1
        DO J=2,JM-1
            HARRAY(I,J,:) = HARRAY(I,J,:) * HBMSK(I,J) + (1. - HBMSK(I,J)) * RBUF2(I-IM+1,J,:)
        END DO
    END DO
!-----------
! FROM SOUTH
!-----------
    DO I=2,IM-1
        DO J=0,1
            HARRAY(I,J,: ) = HARRAY(I,J,:) * HBMSK(I,J) + (1. - HBMSK(I,J)) * RBUF3(I,J+1,:)
        END DO
    END DO
!----------
! FROM WEST
!----------
    DO I=0,1
        DO J=2,JM-1
            HARRAY(I,J,:) = HARRAY(I,J,:) * HBMSK(I,J) + (1. - HBMSK(I,J)) * RBUF4(I+1,J,:)
        END DO
    END DO
!       
    IF (LSWC) THEN
!
        HARRAY(0,0,:) = 0.
        HARRAY(0,1,:) =  RBUF4(1,1,:) + RBUF3(1,1,:)
        HARRAY(1,0,:) = HARRAY(0,1,:) 
        HARRAY(1,1,:) = HARRAY(1,1,:) * HBMSK(1,1) + (1. - HBMSK(1,1))                            &
    &                 * (RBUF3(1,2,:) + RBUF4(2,1,:))
!
    ELSE
!
        HARRAY(0,0,:) =  RBUF3_C(1,1,:)
        HARRAY(1,1,:) = (RBUF3_C(2,2,:) + RBUF4(2,1,:) + RBUF3(1,2,:)) * (1. - HBMSK(1,1))        &
    &                 +   HARRAY(1,1,:) * HBMSK(1,1)
!
        HARRAY(0,1,:) =    RBUF4(1,1,:) + RBUF3_C(1,2,:)
        HARRAY(1,0,:) =    RBUF3(1,1,:) + RBUF3_C(2,1,:)
!
    END IF
! 
    IF (LNWC) THEN
!
        HARRAY(0,JM+1,:) = 0.
        HARRAY(1,JM+1,:) =  RBUF4(1,JM  ,:) + RBUF1(1,2,:)
        HARRAY(0,JM  ,:) = HARRAY(1,JM+1,:)
        HARRAY(1,JM  ,:) = (RBUF4(2,JM  ,:) + RBUF1(1,1,:)) * (1.-HBMSK(1,JM))                    &
    &                    + HARRAY(1,JM  ,:) * HBMSK(1,JM)
!
    ELSE
!
        HARRAY(0,JM+1,:) =  RBUF4_C(1,2,:)
        HARRAY(0,JM  ,:) =  RBUF4_C(1,1,:) + RBUF4(1,JM,:)
        HARRAY(1,JM+1,:) =  RBUF4_C(2,2,:) + RBUF1(1,2 ,:)
        HARRAY(1,JM  ,:) = (RBUF4_C(2,1,:) + RBUF1(1,1 ,:) + RBUF4(2,JM,:)) * (1. - HBMSK(1,JM))  &
    &                    +  HARRAY(1,JM,:) * HBMSK(1,JM)
!
    END IF
      
    IF (LSEC) THEN
!
        HARRAY(IM+1,0,:) = 0.
        HARRAY(IM+1,1,:) =  RBUF3(IM  ,1,:) + RBUF2(2 ,1,:)
        HARRAY(IM  ,0,:) = HARRAY(IM+1,1,:)
        HARRAY(IM  ,1,:) = (RBUF2(1   ,1,:) + RBUF3(IM,2,:)) * (1. - HBMSK(IM,1))                 &
    &                    + HARRAY(IM  ,1,:) * HBMSK(IM,1)
!
    ELSE
!
        HARRAY(IM+1,0,:) = RBUF2_C(2 ,1,:)
        HARRAY(IM+1,1,:) = RBUF2_C(2 ,2,:) +   RBUF2(2 ,1,:)
        HARRAY(IM  ,0,:) =   RBUF3(IM,1,:) + RBUF2_C(1 ,1,:)
        HARRAY(IM  ,1,:) = ( RBUF2(1 ,1,:) +   RBUF3(IM,2,:) + RBUF2_C(1,2,:)) * (1.-HBMSK(IM,1)) &
    &                    +  HARRAY(IM,1,:) * HBMSK(IM,1)
!
    END IF
!       
    IF (LNEC) THEN
!
        HARRAY(IM+1,JM+1,:) = 0.
        HARRAY(IM+1,JM  ,:) =  RBUF1(IM  ,2 ,:) + RBUF2(2,JM,:)
        HARRAY(IM  ,JM+1,:) = HARRAY(IM+1,JM,:)
        HARRAY(IM  ,JM  ,:) = (RBUF1(IM  ,1 ,:) + RBUF2(1,JM,:)) * (1. - HBMSK(IM,JM))            &
    &                       + HARRAY(IM  ,JM,:) * HBMSK(IM,JM)
!
    ELSE
!
        HARRAY(IM+1,JM+1,:) = RBUF1_C(2 ,2,:)
        HARRAY(IM+1,JM  ,:) = RBUF1_C(2 ,1,:)   +   RBUF2(2,JM,:)
        HARRAY(IM  ,JM+1,:) =   RBUF1(IM,2,:)   + RBUF1_C(1, 2,:)
        HARRAY(IM  ,JM  ,:) = ( RBUF1(IM,1,:)   +   RBUF2(1,JM,:) + RBUF1_C(1,1,:))               &
    &                       * (1.-HBMSK(IM,JM)) + HARRAY(IM,JM,:) * HBMSK(IM,JM)
!
    END IF  
!
    DEALLOCATE (RBUF1, STAT = IDERR)
    DEALLOCATE (RBUF2, STAT = IDERR)
    DEALLOCATE (RBUF3, STAT = IDERR)
    DEALLOCATE (RBUF4, STAT = IDERR)
!---------------------
! DEALLOCATE SBUFFERES
!---------------------
    IF (MY_NEB(5) >= 0 ) THEN
        CALL MPI_WAIT(SHANDLE(1), ISTAT, IERR)
    END IF
!
    IF (MY_NEB(6) >= 0 ) THEN
        CALL MPI_WAIT(SHANDLE(2), ISTAT, IERR)
    END IF
!
    IF (MY_NEB(7) >= 0 ) THEN
        CALL MPI_WAIT(SHANDLE(3), ISTAT, IERR)
    END IF
!
    IF (MY_NEB(8) >= 0 ) THEN
        CALL MPI_WAIT(SHANDLE(4), ISTAT, IERR)
    END IF
!
    END SUBROUTINE BOCOHMPI
