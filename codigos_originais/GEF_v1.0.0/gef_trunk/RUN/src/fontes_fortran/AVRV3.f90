!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief AVERAGING OF FOUR WIND POINTS
!> @details AVERAGING OF FOUR WIND POINTS, RESULTING IN SCALAR POINTS.
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
!> @param[in] IMAX - Significado de IMAX
!> @param[in] JMAX - Significado de JMAX
!> @param[in] V1 - Significado de V1
!> @param[in] V2 - Significado de V2
!> @param[out] VXY - Significado de VXY
!> @details <b>Use Module:</b> 
!! @arg @c F77KINDS
!> @details <b>Driver:</b> 
!<
!> @details <b>Calls:</b> 
!<
!--------------------------------------------------------------------------------------------------
    SUBROUTINE AVRV3(V1, V2, VXY, IMAX, JMAX)
!--------------------------------------------------------------------------------------------------
! SUBROUTINE AVRV3
! 
! SUBPROGRAM: AVRV3 - AVERAGING OF FOUR WIND POINTS
! PROGRAMMER: ????? 
! ORG: W/NP22
! DATE: ??-??-??
!
! ABSTRACT:  
! AVERAGING OF FOUR WIND POINTS, RESULTING IN SCALAR POINTS
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
! IMAX - 
! JMAX - 
! V1   - 
! V2   -
!
! OUTPUT ARGUMENT LIST:
! VXY - 
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
!
! DRIVER     : -----
!
! CALLS      : -----            
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    INTEGER(KIND=I4)                                                      , INTENT(IN)          ::&
    & IMAX    , JMAX
!
    INTEGER(KIND=I4)                                                                            ::&
    & I       , J
!
    REAL   (KIND=R4)    , DIMENSION(0:IMAX+1, 0:JMAX+1)                   , INTENT(IN)          ::&
    & V1      , V2      
!
    REAL   (KIND=R4)    , DIMENSION(0:IMAX+1, 0:JMAX+1)                                         ::&
    & VXP1    , VXP2    , VYP1    , VYP2    
!
    REAL   (KIND=R4)    , DIMENSION(0:IMAX+1, 0:JMAX+1)                   , INTENT(OUT)         ::&
    & VXY
!
    DO J=1,JMAX
        DO I=1,IMAX
            VXP1(I,J) = V1(I,J-1) + V1(I-1,J  )
            VXP2(I,J) = V2(I,J-1) + V2(I-1,J  )
            VYP1(I,J) = V1(I,J  ) + V1(I-1,J-1)
            VYP2(I,J) = V2(I,J  ) + V2(I-1,J-1)
        END DO
    END DO
!
    DO J=1,JMAX
        DO I=1,IMAX
            VXY(I,J) = (VXP1(I,J) + VYP1(I,J) * 2. + VXP2(I,J) * 2. + VYP2(I,J)) * 2. / 3.
        END DO
    END DO
!
    RETURN
!
    END SUBROUTINE AVRV3
