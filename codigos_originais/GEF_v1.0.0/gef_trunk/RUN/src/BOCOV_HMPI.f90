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
!> @param[inout] UARRAY - Significado de UARRAY
!> @param[inout] VARRAY - Significado de VARRAY
!> @details <b>Use Module:</b>
!! @arg @c DOM
!! @arg @c F77KINDS
!! @arg @c MPPSTAFF
!! @arg @c NEBMAP
!! @arg @c PARMETA
!> @details <b>Driver:</b> 
!<
!> @details <b>Calls:</b>
!! @arg @c INVERTV
!! @arg @c MPI_IRECV
!! @arg @c MPI_ISEND
!! @arg @c MPI_WAIT
!! @arg @c SWAPUV 
!<
!--------------------------------------------------------------------------------------------------
    SUBROUTINE BOCOV_HMPI(UARRAY, VARRAY, LMAX)
!--------------------------------------------------------------------------------------------------
! SUBROUTINE BOCOV_HMPI
! 
! SUBPROGRAM: BOCOV_HMPI - SUPPLY ALL BOUNDARIES WITH V VALUES AT H POINTS
! PROGRAMMER: ????? 
! ORG: W/NP22
! DATE: ??-??-??
!
! ABSTRACT:  
! SUPPLY ALL BOUNDARIES WITH V VALUES AT H POINTS
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
! UARRAY -
! VARRAY -  
!
! INPUT FILES:
! NONE
!
! OUTPUT FILES:
! NONE
!
! USE MODULES: DOM
!              F77KINDS
!              MPPSTAFF
!              NEBMAP
!              PARMETA
!
! DRIVER     : -----
!
! CALLS      : INVERTV
!              MPI_IRECV
!              MPI_ISEND
!              MPI_WAIT
!              SWAPUV
!--------------------------------------------------------------------------------------------------
    USE DOM
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
    INTEGER(KIND=I4)                                                                            ::&
    & LMAX2   , I       , J       , L       , NDATA   , NEBPE   , IERR    , IAERR   , IDERR   ,   &
    & ISEND   , IRECV
!
    INTEGER(KIND=I4)    , PARAMETER :: IMP1 = IM + 1
    INTEGER(KIND=I4)    , PARAMETER :: JMP1 = IMP1
    INTEGER(KIND=I4)    , PARAMETER :: IMX2 = IM * 2
    INTEGER(KIND=I4)    , PARAMETER :: JMX2 = IMX2
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1, LMAX)                 , INTENT(INOUT)       ::&
    & UARRAY  , VARRAY
!
    REAL   (KIND=R4)    , ALLOCATABLE, DIMENSION(:,:)                                           ::&
    & SBUF1   , SBUF2   , SBUF3   , SBUF4   ,                                                     &
    & RBUF1   , RBUF2   , RBUF3   , RBUF4
!  
    REAL   (KIND=R4)    , ALLOCATABLE, DIMENSION(:,:)                                           ::&
    & SBUF1_C , SBUF2_C , SBUF3_C , SBUF4_C ,                                                     &
    & RBUF1_C , RBUF2_C , RBUF3_C , RBUF4_C
!
    INTEGER(KIND=I4)    , DIMENSION(8)                                                          ::&
    & SHANDLE , RHANDLE 
!
    INTEGER(KIND=I4)    , DIMENSION(MPI_STATUS_SIZE)                                            ::&
    & ISTAT
!
    REAL   (KIND=R4)    , DIMENSION(LMAX)                                                       ::&
    & DUMMY
!----------------
! SEND BOUNDARIES
!----------------
    NDATA = IMX2 * LMAX
!-------------
! TOWARD NORTH
!-------------
    IF (MY_NEB(1) >= 0) THEN
!
        NEBPE = MY_NEB(1)
!
        ALLOCATE (SBUF1(1:IMX2, 1:LMAX), STAT = IAERR)
!
        SBUF1(1:IM     , 1:LMAX) = UARRAY(1:IM, JM1, 1:LMAX) * ISGNU_NEB(1)
        SBUF1(IMP1:IMX2, 1:LMAX) = VARRAY(1:IM, JM1, 1:LMAX) * ISGNV_NEB(1)
!
        IF (LINV_NEB(1)) CALL INVERTV(SBUF1, IMX2, LMAX)
        IF (LEXC_NEB(1)) CALL SWAPUV (SBUF1, IMX2, LMAX)
!
        CALL MPI_ISEND(SBUF1, NDATA, ITYPE, NEBPE, MYPE, MPI_COMM_WORLD, SHANDLE(1), ISEND)
!
        WRITE(0,*)' SEND NORTH:', IMX2, LMAX, 'DATA'
!
    END IF
!------------ 
! TOWARD EAST 
!------------
    IF (MY_NEB(2) >= 0) THEN
!
        NEBPE = MY_NEB(2)
!
        ALLOCATE (SBUF2(1:IMX2, 1:LMAX), STAT = IAERR)
!
        SBUF2(1:JM     , 1:LMAX) = UARRAY(IM1, 1:JM, 1:LMAX) * ISGNU_NEB(2)
        SBUF2(JMP1:JMX2, 1:LMAX) = VARRAY(IM1, 1:JM, 1:LMAX) * ISGNV_NEB(2)
!
        IF (LINV_NEB(2)) CALL INVERTV(SBUF2, IMX2, LMAX)
        IF (LEXC_NEB(2)) CALL SWAPUV (SBUF2, IMX2, LMAX)
!
        CALL MPI_ISEND(SBUF2, NDATA, ITYPE, NEBPE, MYPE, MPI_COMM_WORLD, SHANDLE(2), ISEND)
!
    END IF
!-------------
! TOWARD SOUTH 
!-------------
    IF (MY_NEB(3) >= 0) THEN
!
        NEBPE = MY_NEB(3)
!
        ALLOCATE (SBUF3(1:IMX2, 1:LMAX), STAT = IAERR)
!
        SBUF3(1   :IM  , 1:LMAX) = UARRAY(1:IM, 1, 1:LMAX) * ISGNU_NEB(3)
        SBUF3(IMP1:IMX2, 1:LMAX) = VARRAY(1:IM, 1, 1:LMAX) * ISGNV_NEB(3)
!
        IF (LINV_NEB(3)) CALL INVERTV(SBUF3, IMX2, LMAX)
        IF (LEXC_NEB(3)) CALL SWAPUV (SBUF3, IMX2, LMAX)
!
        CALL MPI_ISEND(SBUF3, NDATA, ITYPE, NEBPE, MYPE, MPI_COMM_WORLD, SHANDLE(3), ISEND)
!
    END IF
!------------
! TOWARD WEST 
!------------
    IF (MY_NEB(4) >= 0) THEN
!
        NEBPE = MY_NEB(4)
!
        ALLOCATE (SBUF4(1:IMX2, 1:LMAX), STAT = IAERR)
!
        SBUF4(1:JM     , 1:LMAX) = UARRAY(1, 1:JM, 1:LMAX) * ISGNU_NEB(4)
        SBUF4(JMP1:IMX2, 1:LMAX) = VARRAY(1, 1:JM, 1:LMAX) * ISGNV_NEB(4)
!
        IF (LINV_NEB(4)) CALL INVERTV(SBUF4,IMX2, LMAX)
        IF (LEXC_NEB(4)) CALL SWAPUV (SBUF4,IMX2, LMAX)
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
!
        NEBPE = MY_NEB(1)
!
        ALLOCATE (RBUF1(1:IMX2, 1:LMAX), STAT = IAERR)
!
        CALL MPI_IRECV(RBUF1, NDATA, ITYPE, NEBPE, NEBPE, MPI_COMM_WORLD, RHANDLE(1), IRECV)
!
        CALL MPI_WAIT(RHANDLE(1), ISTAT, IERR)
!
        UARRAY(1:IM, JMP1, 1:LMAX) = RBUF1(1   :IM  , 1:LMAX)
        VARRAY(1:IM, JMP1, 1:LMAX) = RBUF1(IMP1:IMX2, 1:LMAX)
!
        DEALLOCATE (RBUF1, STAT = IDERR)
!
    END IF
!----------
! FROM EAST
!----------
    IF (MY_NEB(2) >= 0) THEN
!
        NEBPE = MY_NEB(2)
!
        ALLOCATE (RBUF2(1:IMX2, 1:LMAX), STAT = IAERR)
!
        CALL MPI_IRECV(RBUF2, NDATA, ITYPE, NEBPE, NEBPE, MPI_COMM_WORLD, RHANDLE(2), IRECV)
!
        CALL MPI_WAIT(RHANDLE(2), ISTAT, IERR)
!
        UARRAY(IMP1, 1:JM, 1:LMAX) = RBUF2(1   :JM  , 1:LMAX)
        VARRAY(IMP1, 1:JM, 1:LMAX) = RBUF2(JMP1:JMX2, 1:LMAX)
!
        DEALLOCATE (RBUF2, STAT = IDERR)
!
    END IF
!-----------
! FROM SOUTH
!-----------
    IF (MY_NEB(3) >= 0) THEN
!
        NEBPE = MY_NEB(3)
!
        ALLOCATE (RBUF3(1:IMX2, 1:LMAX), STAT = IAERR)
!
        CALL MPI_IRECV(RBUF3, NDATA, ITYPE, NEBPE, NEBPE, MPI_COMM_WORLD, RHANDLE(3), IRECV)
!
        CALL MPI_WAIT(RHANDLE(3), ISTAT, IERR)
!
        UARRAY(1:IM, 0, 1:LMAX) = RBUF3(1   :IM  , 1:LMAX)
        VARRAY(1:IM, 0, 1:LMAX) = RBUF3(IMP1:IMX2, 1:LMAX)
!
        WRITE(0,*) 'RECEIVE FROM SOUTH:', IMX2, LMAX, 'DATA'
!
        DEALLOCATE (RBUF3, STAT = IDERR)
!
    END IF
!----------
! FROM WEST
!----------
    IF (MY_NEB(4) >= 0) THEN
!
        NEBPE = MY_NEB(4)
!
        ALLOCATE (RBUF4(1:IMX2, 1:LMAX), STAT = IAERR)
!
        CALL MPI_IRECV(RBUF4, NDATA, ITYPE, NEBPE, NEBPE, MPI_COMM_WORLD, RHANDLE(4), IRECV)
!
        CALL MPI_WAIT(RHANDLE(4), ISTAT, IERR)
!
        UARRAY(0, 1:JM, 1:LMAX) = RBUF4(1   :JM  , 1:LMAX)
        VARRAY(0, 1:JM, 1:LMAX) = RBUF4(JMP1:JMX2, 1:LMAX)
!
        DEALLOCATE (RBUF4, STAT = IDERR)
!
    END IF
!---------------------
! DEALLOCATE SBUFFERES
!---------------------
    IF (MY_NEB(1) >= 0) THEN
        CALL MPI_WAIT(SHANDLE(1), ISTAT, IERR)
        DEALLOCATE (SBUF1, STAT = IERR)
    END IF
!
    IF (MY_NEB(2) >= 0) THEN
        CALL MPI_WAIT(SHANDLE(2), ISTAT, IERR)
        DEALLOCATE (SBUF2, STAT = IERR)
    END IF
!
    IF (MY_NEB(3) >= 0) THEN
        CALL MPI_WAIT(SHANDLE(3), ISTAT, IERR)
        DEALLOCATE (SBUF3, STAT = IERR)
    END IF
!
    IF (MY_NEB(4) >= 0) THEN
        CALL MPI_WAIT(SHANDLE(4), ISTAT, IERR)
        DEALLOCATE (SBUF4, STAT = IERR)
    END IF
!-------------
! SEND CORNERS
!------------- 
    LMAX2 = LMAX * 2
!------------------
! TOWARD NORTH-EAST
!------------------
    IF (MY_NEB(5) >= 0) THEN
!
        NEBPE = MY_NEB(5)
!
        ALLOCATE (SBUF1_C(1:2, 1:LMAX), STAT = IAERR)
!
        SBUF1_C(1, 1:LMAX) = UARRAY(IM1, JM1, 1:LMAX) * ISGNU_NEB(5)
        SBUF1_C(2, 1:LMAX) = VARRAY(IM1, JM1, 1:LMAX) * ISGNV_NEB(5)
!
        IF (LEXC_NEB(5)) THEN
              DUMMY(:)   = SBUF1_C(1,:)
            SBUF1_C(1,:) = SBUF1_C(2,:)
            SBUF1_C(2,:) =   DUMMY(:)
        END IF
!
        CALL MPI_ISEND(SBUF1_C, LMAX2, ITYPE, NEBPE, MYPE, MPI_COMM_WORLD, SHANDLE(5), ISEND)
!
    END IF
!------------------       
! TOWARD SOUTH-EAST 
!------------------
    IF (MY_NEB(6) >= 0) THEN
!
        NEBPE = MY_NEB(6)
!
        ALLOCATE (SBUF2_C(1:2, 1:LMAX), STAT = IAERR)
!
        SBUF2_C(1,1:LMAX) = UARRAY(IM1, 2, 1:LMAX) * ISGNU_NEB(6)
        SBUF2_C(2,1:LMAX) = VARRAY(IM1, 2, 1:LMAX) * ISGNV_NEB(6)
!
        IF (LEXC_NEB(6)) THEN
              DUMMY(:)   = SBUF2_C(1,:)
            SBUF2_C(1,:) = SBUF2_C(2,:)
            SBUF2_C(2,:) =   DUMMY(:)
        END IF
!
        CALL MPI_ISEND(SBUF2_C, LMAX2, ITYPE, NEBPE, MYPE, MPI_COMM_WORLD, SHANDLE(6), ISEND)
!
    END IF
!------------------
! TOWARD SOUTH-WEST
!------------------
    IF (MY_NEB(7) >= 0) THEN
!
        NEBPE = MY_NEB(7)
!
        ALLOCATE (SBUF3_C(1:2, 1:LMAX), STAT = IAERR)
!
        SBUF3_C(1,:) = UARRAY(2, 2, :) * ISGNU_NEB(7)
        SBUF3_C(2,:) = VARRAY(2, 2, :) * ISGNV_NEB(7)
!
        IF (LEXC_NEB(7)) THEN
              DUMMY(:)   = SBUF3_C(1,:)
            SBUF3_C(1,:) = SBUF3_C(2,:)
            SBUF3_C(2,:) =   DUMMY(:)
        END IF
!
        CALL MPI_ISEND(SBUF3_C, LMAX2, ITYPE, NEBPE, MYPE, MPI_COMM_WORLD, SHANDLE(7), ISEND)
!
    END IF
!------------------
! TOWARD NORTH-WEST 
!------------------
    IF (MY_NEB(8) >= 0) THEN
!
        NEBPE = MY_NEB(8)
!
        ALLOCATE (SBUF4_C(1:2, 1:LMAX), STAT = IAERR)
!
        SBUF4_C(1,:) = UARRAY(2, JM1, :) * ISGNU_NEB(8)
        SBUF4_C(2,:) = VARRAY(2, JM1, :) * ISGNV_NEB(8)
!
        IF (LEXC_NEB(8)) THEN
              DUMMY(:)   = SBUF4_C(1,:)
            SBUF4_C(1,:) = SBUF4_C(2,:)
            SBUF4_C(2,:) =   DUMMY(:)
        END IF
!
        CALL MPI_ISEND(SBUF4_C, LMAX2, ITYPE, NEBPE, MYPE, MPI_COMM_WORLD, SHANDLE(8), ISEND)
!
    END IF
!----------------
! RECEIVE CORNERS
!!---------------
!
!----------------
! FROM NORTH-EAST
!----------------
    IF (MY_NEB(5) >= 0) THEN
!
        NEBPE = MY_NEB(5)
!
        ALLOCATE (RBUF1_C(1:2, 1:LMAX), STAT = IAERR)
!
        CALL MPI_IRECV(RBUF1_C, LMAX2, ITYPE, NEBPE, NEBPE, MPI_COMM_WORLD, RHANDLE(5), IRECV)
!
        CALL MPI_WAIT(RHANDLE(5), ISTAT, IERR)
!
        UARRAY(IMP1, JMP1, :) = RBUF1_C(1, :)
        VARRAY(IMP1, JMP1, :) = RBUF1_C(2, :)
!
        DEALLOCATE (RBUF1_C, STAT = IERR)
!
    END IF
!----------------
! FROM SOUTH-EAST
!----------------
    IF (MY_NEB(6) >= 0) THEN
!
        NEBPE = MY_NEB(6)
!
        ALLOCATE (RBUF2_C(1:2, 1:LMAX), STAT = IAERR)
!
        CALL MPI_IRECV(RBUF2_C, LMAX2, ITYPE, NEBPE, NEBPE, MPI_COMM_WORLD, RHANDLE(6), IRECV)
!
        CALL MPI_WAIT(RHANDLE(6), ISTAT, IERR)
!
        UARRAY(IMP1, 0, :) = RBUF2_C(1, :)
        VARRAY(IMP1, 0, :) = RBUF2_C(2, :)
!
        DEALLOCATE (RBUF2_C, STAT = IERR)
!
    END IF
!----------------
! FROM SOUTH-WEST 
!---------------
    IF (MY_NEB(7) >= 0) THEN
!
        ALLOCATE (RBUF3_C(1:2, 1:LMAX), STAT = IAERR)
!
        NEBPE = MY_NEB(7)
!
        CALL MPI_IRECV(RBUF3_C, LMAX2, ITYPE, NEBPE, NEBPE, MPI_COMM_WORLD, RHANDLE(7), IRECV)
!
        CALL MPI_WAIT(RHANDLE(7), ISTAT, IERR)
!
        UARRAY(0, 0, :) = RBUF3_C(1, :)
        VARRAY(0, 0, :) = RBUF3_C(2, :)
!
        DEALLOCATE (RBUF3_C, STAT = IERR)
!
    END IF
!----------------
! FROM NORTH-WEST
!----------------
    IF (MY_NEB(8) >= 0) THEN
!
        NEBPE = MY_NEB(8)
!
        ALLOCATE (RBUF4_C(1:2, 1:LMAX), STAT = IAERR)
!
        CALL MPI_IRECV(RBUF4_C, LMAX2, ITYPE, NEBPE, NEBPE, MPI_COMM_WORLD, RHANDLE(8), IRECV)
!
        CALL MPI_WAIT(RHANDLE(8), ISTAT, IERR)
!
        UARRAY(0, JMP1, :) = RBUF4_C(1, :)
        VARRAY(0, JMP1, :) = RBUF4_C(2, :)
!
        DEALLOCATE (RBUF4_C, STAT = IERR)
!
    END IF
!---------------------
! DEALLOCATE SBUFFERES
!---------------------
    IF (MY_NEB(5) >= 0) THEN
        CALL MPI_WAIT(SHANDLE(5), ISTAT, IERR)
        DEALLOCATE (SBUF1_C, STAT = IERR)
    END IF
!
    IF (MY_NEB(6) >= 0) THEN
        CALL MPI_WAIT(SHANDLE(6), ISTAT, IERR)
        DEALLOCATE (SBUF2_C, STAT = IERR)
    END IF
!
    IF (MY_NEB(7) >= 0) THEN
        CALL MPI_WAIT(SHANDLE(7), ISTAT, IERR)
        DEALLOCATE (SBUF3_C, STAT = IERR)
    END IF
!
    IF (MY_NEB(8) >= 0) THEN
        CALL MPI_WAIT(SHANDLE(8), ISTAT, IERR)
        DEALLOCATE(SBUF4_C, STAT = IERR)
    END IF
!
    END SUBROUTINE BOCOV_HMPI
