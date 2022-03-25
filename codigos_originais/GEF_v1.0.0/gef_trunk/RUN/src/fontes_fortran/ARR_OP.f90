!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief ALLOCATE ALL FIELDS
!> @details ALLOCATE ALL FIELDS DEFINED AS ALLOCATBLE IN ALL MODULES.
!> @author ORIGINATOR - ????? 
!> @date 87-06-?? \n
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
!> @param[in] LMAX - Significado de LMAX
!> @param[in] ARR1 - Significado de ARR1
!> @param[out] ARR2 - Significado de ARR2
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS
!> @details <b>Driver:</b> 
!! @arg @c BOCOHMPI
!! @arg @c BOCOVMPI_SC
!> @details <b>Calls:</b> 
!<
!--------------------------------------------------------------------------------------------------
    SUBROUTINE ROTCCW90(ARR1, ARR2, IMAX, JMAX, LMAX)
!--------------------------------------------------------------------------------------------------
! SUBROUTINE ROTCCW90
! 
! SUBPROGRAM: ROTCCW90 - ALLOCATE ALL FIELDS
! PROGRAMMER: ????? 
! ORG: W/NP22
! DATE: ??-??-??
!
! ABSTRACT:  
! ALLOCATE ALL FIELDS DEFINED AS ALLOCATBLE IN ALL MODULES
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
! LMAX - 
! ARR1 -
!
! OUTPUT ARGUMENT LIST:
! ARR2 - 
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
! DRIVER     : BOCOHMPI
!              BOCOVMPI_SC
!
! CALLS      : -----            
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    INTEGER(KIND=I4)                                                      , INTENT(IN)          ::&
    & IMAX    , JMAX    , LMAX
!
    REAL   (KIND=R4)    , DIMENSION(IMAX, JMAX, LMAX)                     , INTENT(IN)          ::&
    & ARR1
!
    REAL   (KIND=R4)    , DIMENSION(JMAX, IMAX, LMAX)                     , INTENT(OUT)         ::&
    & ARR2
!
    INTEGER(KIND=I4)                                                                            ::&
    & I       , J
!
    DO I=1,JMAX
        DO J=1,IMAX
            ARR2(I,J,:) = ARR1(J, JMAX-I+1, :)
        END DO
    END DO
!      

    END SUBROUTINE ROTCCW90
!
!
!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief ALLOCATE ALL FIELDS
!! @details ALLOCATE ALL FIELDS DEFINED AS ALLOCATBLE IN ALL MODULES.
!> @author ORIGINATOR - ????? 
!> @date 87-06-?? \n
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
!> @param[in] LMAX - Significado de LMAX
!> @param[in] ARR1 - Significado de ARR1
!> @param[out] ARR2 - Significado de ARR2
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS
!! @details <b>Driver:</b> 
!! @arg @c BOCOHMPI
!! @arg @c BOCOVMPI_SC
!! @details <b>Calls:</b> 
!<
!--------------------------------------------------------------------------------------------------
    SUBROUTINE ROTCW90(ARR1, ARR2, IMAX, JMAX, LMAX)
!--------------------------------------------------------------------------------------------------
! SUBROUTINE ROTCW90
! 
! SUBPROGRAM: ROTCW90 - ?????
! PROGRAMMER: ????? 
! ORG: W/NP22
! DATE: ??-??-??
!
! ABSTRACT:  
! ALLOCATE ALL FIELDS DEFINED AS ALLOCATBLE IN ALL MODULES
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
! LMAX - 
! ARR1 -
!
! OUTPUT ARGUMENT LIST:
! ARR2 - 
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
! DRIVER     : BOCOHMPI
!              BOCOVMPI_SC
!
! CALLS      : -----            
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    INTEGER(KIND=I4)                                                      , INTENT(IN)          ::&
    & IMAX    , JMAX    , LMAX
!
    REAL   (KIND=R4)    , DIMENSION(IMAX, JMAX, LMAX)                     , INTENT(IN)          ::&
    & ARR1
!
    REAL   (KIND=R4)    , DIMENSION(JMAX, IMAX, LMAX)                     , INTENT(OUT)         ::&
    & ARR2
!
    INTEGER(KIND=I4)                                                                            ::&
    & I       , J
!
    DO I=1,JMAX
        DO J=1,IMAX
            ARR2(I,J,:) = ARR1(IMAX-J+1, I, :)
        END DO
    END DO
!
    END SUBROUTINE ROTCW90
!
!
!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief ALLOCATE ALL FIELDS
!! @details ALLOCATE ALL FIELDS DEFINED AS ALLOCATBLE IN ALL MODULES.
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
!> @param[in] LMAX - Significado de LMAX
!> @param[in] ARR1 - Significado de ARR1
!> @param[out] ARR2 - Significado de ARR2
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS
!! @details <b>Driver:</b> 
!! @arg @c BOCOHMPI
!! @arg @c BOCOVMPI_SC
!! @details <b>Calls:</b> 
!<
!--------------------------------------------------------------------------------------------------
    SUBROUTINE ROT180(ARR1, ARR2, IMAX, JMAX, LMAX)
!--------------------------------------------------------------------------------------------------
! SUBROUTINE ROT180
! 
! SUBPROGRAM: ROT180 - ?????
! PROGRAMMER: ????? 
! ORG: W/NP22
! DATE: ??-??-??
!
! ABSTRACT:  
! ALLOCATE ALL FIELDS DEFINED AS ALLOCATBLE IN ALL MODULES
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
! LMAX - 
! ARR1 -
!
! OUTPUT ARGUMENT LIST:
! ARR2 - 
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
! DRIVER     : BOCOHMPI
!              BOCOVMPI_SC
!
! CALLS      : -----            
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    INTEGER(KIND=I4)                                                      , INTENT(IN)          ::&
    & IMAX    , JMAX    , LMAX
!
    REAL   (KIND=R4)    , DIMENSION(IMAX, JMAX, LMAX)                     , INTENT(IN)          ::&
    & ARR1
!
    REAL   (KIND=R4)    , DIMENSION(IMAX, JMAX, LMAX)                     , INTENT(OUT)         ::&
    & ARR2
!
    INTEGER(KIND=I4)                                                                            ::&
    & I       , J
!
    DO I=1,IMAX
        DO J=1,JMAX
            ARR2(I,J,:) = ARR1(IMAX-I+1, JMAX-J+1, :)
        END DO
    END DO
!      
    END SUBROUTINE ROT180
