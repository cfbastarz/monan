!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Inserir Brief de CORNER_MFL
!> @details Inserir Details de CORNER_MFL
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
!> @param[in] UST - Significado de UST
!> @param[in] VST - Significado de VST
!> @param[out] P - Significado de P
!> @details <b>Use Module:</b> 
!! @arg @c F77KINDS
!> @details <b>Driver:</b> 
!<
!> @details <b>Calls:</b>
!<
!--------------------------------------------------------------------------------------------------
     SUBROUTINE CORNER_MFL(UST,VST,P)
!--------------------------------------------------------------------------------------------------
! SUBROUTINE CORNER_MFL
!
! SUBPROGRAM: CORNER_MFL - ?????
! PROGRAMMER: ????? 
! ORG: W/NP22
! DATE: ??-??-??
!     
! ABSTRACT: 
! ?????    
!
! PROGRAM HISTORY LOG:
! ??-??-??  ?????    - ORIGINATOR            
! 18-03-20  LUCCI    - MODERNIZATION OF THE CODE, INCLUDING:
!                      * F77 TO F90/F95
!                      * INDENTATION & UNIFORMIZATION CODE
!                      * REPLACEMENT OF COMMONS BLOCK FOR MODULES
!                      * DOCUMENTATION WITH DOXYGEN
!                      * OPENMP FUNCTIONALITY
!
! INPUT  ARGUMENT LIST:
! UST -
! VST - 
!
! OUTPUT ARGUMENT LIST:
! P   -
!
! INPUT/OUTPUT ARGUMENT LIST:
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
    REAL   (KIND=R4)    , DIMENSION(6)                                    , INTENT(IN)          ::&
    & UST     , VST
!
    REAL   (KIND=R4)    , DIMENSION(3)                                    , INTENT(OUT)         ::&
    & P
!
    P(1) =  0.125 * ( 8. * UST(2) + 4. * UST(1) + 4. * VST(1) + 4. * UST(3) + 4. * VST(3)         &
    &    -            8. * UST(5) - 4. * UST(6) - 4. * VST(4))
            
    P(2) =  0.125 * (-8. * UST(3) - 4. * UST(1) + 4. * VST(1) - 4. * UST(2) + 4. * VST(2)         &
    &    +            8. * UST(6) - 4. * VST(4) + 4. * UST(5))
!
    P(3) = -0.125 * ( 8. * VST(1) + 4. * UST(2) + 4. * VST(2) - 4. * UST(3) + 4. * VST(3)         &
    &    -            8. * VST(4) - 4. * UST(5) + 4. * UST(6))
!
    END SUBROUTINE CORNER_MFL
