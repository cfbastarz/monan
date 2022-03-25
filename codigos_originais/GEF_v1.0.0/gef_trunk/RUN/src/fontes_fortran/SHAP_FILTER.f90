    SUBROUTINE SHAPIRO
!--------------------------------------------------------------------------------------------------
! SUBROUTINE SHAPIRO
! 
! SUBPROGRAM: SHAPIRO - ?????
! PROGRAMMER: LUCCI 
! ORG: GEF/CPTEC-INPE
! DATE: 19-07-11
!
! ABSTRACT:  
! SHAPIRO FILTER FOR 3D to 2D. 
!
! PROGRAM HISTORY LOG:
! ??-??-??  ?????   - ORIGINATOR
! 18-03-20  LUCCI   - MODERNIZATION OF THE CODE, INCLUDING:
!                     * F77 TO F90/F95
!                     * INDENTATION & UNIFORMIZATION CODE
!                     * REPLACEMENT OF COMMONS BLOCK FOR MODULES
!                     * DOCUMENTATION WITH DOXYGEN
!                     * OPENMP FUNCTIONALITY 
!
!
! INPUT ARGUMENT LIST:
! NORD  - 
!
! OUTPUT ARGUMENT LIST:
! NONE
!
! INPUT/OUTPUT ARGUMENT LIST:
! NONE
!
! INPUT FILES:
! NONE
!
! OUTPUT FILES:
! NONE
!
! USE MODULES: F77KINDS
!              MPPSTAFF
!              PARMETA
!              VRBLS
!
! DRIVER     : -----
!
! CALLS      : SHAPIRO_2D
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
    USE MPPSTAFF
    USE PARMETA
    USE VRBLS
!
    IMPLICIT NONE
!
    INCLUDE "mpif.h"
!
    INTEGER(KIND=I4)                                                 ::&
    & I       , J       , K       , IERR
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1, LM)              ::&
    & VRBU    , VRBV    , VRBT
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1, LM)              ::&
    & AU      , AV      , AT
!-----------------------------------------------------------------------
!  Shapiro filter requested 2D field.
!-----------------------------------------------------------------------
!
!  This subroutine will apply a Shapiro filter of order 2 (defined
!  as twice the order in Shapiro (1970), with N even) to an array, A.
!  The order of the filter is reduced at the boundaries and at the
!  mask edges, if any.
!
!  Initialize filter in the Y-direction.

    DO K=1,LM
!
        AU(:, :, K) = U(:, :, K)
        AV(:, :, K) = V(:, :, K)
!        AT(:, :, K) = T(:, :, K)
!
!        CALL SHAPIRO_2D(AU(:,:,K),AV(:,:,K),AT(:,:,K))
        CALL SHAPIRO_2D(AU(:,:,K),AV(:,:,K))
!
        U(:, :, K) =  U(:, :, K) + (AU(:,:,K) -  U(:, :, K)) * 0.02 
        V(:, :, K) =  V(:, :, K) + (AV(:,:,K) -  V(:, :, K)) * 0.02 
!        T(:, :, K) =  T(:, :, K) + (AT(:,:,K) -  T(:, :, K)) * 0.02
!
    END DO
!
!    CALL MPI_BARRIER(MPI_COMM_WORLD, IERR)
!
    CALL BOCOVMPI(U, V, LM)
!    CALL BOCOHMPI(T, LM)
!
    END SUBROUTINE SHAPIRO
!
!
!
!    SUBROUTINE SHAPIRO_2D(UU,VV,TT)
    SUBROUTINE SHAPIRO_2D(UU,VV)
!
    USE F77KINDS
    USE PARMETA
!
    IMPLICIT NONE
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1), INTENT(INOUT)   ::&
    & UU, VV !, TT
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1)                  ::&
    & AWRKU1, AWRKV1, AWRKT1,                                          &
    & AWRKU2, AWRKV2, AWRKT2
!
    INTEGER(KIND=I4)                                                 ::&
    & I, J    
!
!
      DO J=1,JM
        DO I=0,IM+1
          AWRKU1(I,J)=0.25 * (UU(I,J-1)+UU(I,J+1)-2.0*UU(I,J))  ! velocity u
          AWRKV1(I,J)=0.25 * (VV(I,J-1)+VV(I,J+1)-2.0*VV(I,J))	! velocity v
!          AWRKT1(I,J)=0.25 * (TT(I,J-1)+TT(I,J+1)-2.0*TT(I,J))	! temperature t
        END DO
      END DO
!
!  Add the changes to the field.
!
      DO J=1,JM
        DO I=0,IM+1
          AWRKU2(I,J)=UU(I,J)+AWRKU1(I,J) ! velocity u
          AWRKV2(I,J)=VV(I,J)+AWRKV1(I,J) ! velocity v
!          AWRKT2(I,J)=TT(I,J)+AWRKT1(I,J) ! temperature t
        END DO
      END DO
!
!  Initialize filter in the X-direction.
!
      DO J=1,JM
        DO I=1,IM
          AWRKU1(I,J)=0.25 * (AWRKU2(I-1,J)+AWRKU2(I+1,J)-2.0*AWRKU2(I,J))
          AWRKV1(I,J)=0.25 * (AWRKU2(I-1,J)+AWRKU2(I+1,J)-2.0*AWRKU2(I,J))
!          AWRKT1(I,J)=0.25 * (AWRKU2(I-1,J)+AWRKU2(I+1,J)-2.0*AWRKU2(I,J))
        END DO
      END DO
!
!  Add changes to field.
!
      DO J=1,JM
        DO I=1,IM
          UU(I,J)=AWRKU2(I,J)+AWRKU1(I,J)
          VV(I,J)=AWRKV2(I,J)+AWRKV1(I,J)
!          TT(I,J)=AWRKT2(I,J)+AWRKT1(I,J)
        END DO
      END DO

      RETURN
      END SUBROUTINE SHAPIRO_2D
