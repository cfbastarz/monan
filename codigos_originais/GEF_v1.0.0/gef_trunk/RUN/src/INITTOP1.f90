!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief DEFINE TOPOLOGY
!> @details DEFINE TOPOLOGY - CUBIC GRID.
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
!! @arg @c USE DOM
!! @arg @c F77KINDS
!! @arg @c MPPSTAFF
!! @arg @c NEBMAP
!! @arg @c PARMETA
!! @arg @c SET_ASSOC
!> @details <b>Driver:</b> 
!! @arg @c GEF
!> @details <b>Calls:</b>
!! @arg @c ASSOC
!--------------------------------------------------------------------------------------------------
    SUBROUTINE INITTOP1
!--------------------------------------------------------------------------------------------------
! MAIN PROGRAM INITTOP1
! 
! MAIN PROGRAM: INITTOP1 - DEFINE TOPOLOGY
! PROGRAMMER: ?????          
! ORG: W/NP22
! DATE: ??-??-??
!
! ABSTRACT:
! DEFINE TOPOLOGY - CUBIC GRID
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
! USE MODULES: DOM
!              F77KINDS
!              MPPSTAFF
!              NEBMAP
!              PARMETA
!              SET_ASSOC
! 
! DRIVER     : GEF
!
! CALLS      : AVRH
!              BOCOHMPI
!              BOCOVMPI
!              CORNERHM2
!              FLUX_CORRECTION
!--------------------------------------------------------------------------------------------------
    USE DOM
    USE F77KINDS
    USE MPPSTAFF
    USE NEBMAP
    USE PARMETA
    USE SET_ASSOC
!
    IMPLICIT NONE
!
    LOGICAL(KIND=L4)                                                                            ::&
    & F = .FALSE.       ,                                                                         &
    & T = .TRUE.
!
    INTEGER(KIND=I4)    , DIMENSION(6,4)                                                        ::&
    & MY_FACE_NEB
!
    INTEGER(KIND=I4)                                                                            ::&
    & MY_FACE_EAST      , MY_FACE_WEST      , MY_FACE_NORT      , MY_FACE_SOUT 
!-----------------------
!
! INDEXING FOR MY_NEB()
!
!    8  |  1  |  5
!   ____|_____|____
!       |     |
!    4  |     |  2
!   ____|_____|____
!       |     |  
!    7  |  3  |  6
!
!-----------------------
!
!------------------------------------------------
!
!            TOPOLOGY OF CUBIC GRID
!
!             _________ 
!            |         | 
!            |         |
!            |    5    |
!            |         |      
!   _________|_________|_________ _________ 
!  |         |         |         |         |
!  |         |         |         |         |
!  |    4    |    1    |    2    |    3    |
!  |         |         |         |         | 
!  |_________|_________|_________|_________|
!            |         |
!            |         |
!            |    6    |
!            |         |
!            |_________|
!
!------------------------------------------------
!
!------------------
! DESCRIBE TOPOLOGY 
!------------------
    MY_FACE_NEB(1,:) = (/2, 4, 5, 6/)
    MY_FACE_NEB(2,:) = (/3, 1, 5, 6/)
    MY_FACE_NEB(3,:) = (/4, 2, 5, 6/)
    MY_FACE_NEB(4,:) = (/1, 3, 5, 6/)
    MY_FACE_NEB(5,:) = (/2, 4, 3, 1/)
    MY_FACE_NEB(6,:) = (/2, 4, 1, 3/)
!
    MY_FACE_EAST = MY_FACE_NEB(MY_FACE, 1)
    MY_FACE_WEST = MY_FACE_NEB(MY_FACE, 2)
    MY_FACE_NORT = MY_FACE_NEB(MY_FACE, 3)
    MY_FACE_SOUT = MY_FACE_NEB(MY_FACE, 4)
!----------
! SEND EAST
!----------
    ITARG_E  = MYPE + 1
     LINV_E  = F
     LINV_E2 = 1
    ISGNU_E  = 1
    ISGNV_E  = 1
     LEXC_E  = F
     CROT_E  = 1
!
    IF (LEAST) THEN
        SELECT CASE(MY_FACE)
            CASE(05)
                ITARG_E  = NW(MY_FACE_EAST) + JY - 1
                ISGNU_E  = -1
                 LEXC_E  =  T
                 CROT_E  =  4
                 LINV_E2 =  4
            CASE(06)
                ITARG_E  = SE(MY_FACE_EAST) - JY + 1
                 LINV_E  =  T
                 LINV_E2 =  2
                ISGNV_E  = -1
                 LEXC_E  =  T
                 CROT_E  =  2
            CASE DEFAULT
                ITARG_E = SW(MY_FACE_EAST) + (JY - 1) * IXM
        END SELECT
    END IF
!----------
! SEND WEST
!----------
    ITARG_W  = MYPE - 1
     LINV_W  = F
     LINV_W2 = 1
    ISGNU_W  = 1
    ISGNV_W  = 1
     LEXC_W  = F
     CROT_W  = 1
!
    IF (LWEST) THEN
        SELECT CASE(MY_FACE)
            CASE(05)
                ITARG_W= NE(MY_FACE_WEST) - JY + 1
                 LINV_W  =  T
                 LINV_W2 =  2
                ISGNV_W  = -1
                 LEXC_W  =  T
                 CROT_W  =  2
            CASE(06)
                ITARG_W= SW(MY_FACE_WEST) + JY - 1
                ISGNU_W  = -1
                 LEXC_W  =  T
                 CROT_W  =  4
                 LINV_W2 =  4
            CASE DEFAULT
                ITARG_W  = SE(MY_FACE_WEST) + (JY - 1) * IXM
        END SELECT
    END IF
!-----------
! SEND NORTH
!-----------
    ITARG_N  = MYPE + IXM
     LINV_N  = F
     LINV_N2 = 1
    ISGNU_N  = 1
    ISGNV_N  = 1
     LEXC_N  = F
     CROT_N  = 1
!
    IF (LNORTH) THEN
        SELECT CASE(MY_FACE)
            CASE(01)
                ITARG_N = SW(MY_FACE_NORT) + IX - 1
            CASE(02)
                ITARG_N  = SE( MY_FACE_NORT ) + (IX - 1) * IXM
                ISGNV_N  = -1
                 LEXC_N  =  T
                 CROT_N  =  2
                 LINV_N2 =  2
            CASE(03)
                ITARG_N  = NE(MY_FACE_NORT) - IX + 1
                ISGNU_N  = -1
                ISGNV_N  = -1
                 LINV_N  =  T
                 LINV_N2 =  3
                 CROT_N  =  3
            CASE(04)
                ITARG_N  = NW(MY_FACE_NORT) - (IX - 1) * IXM
                 LINV_N  =  T
                 LINV_N2 =  4
                ISGNU_N  = -1
                 LEXC_N  =  T
                 CROT_N  =  4
            CASE(05)
                ITARG_N  = NE(MY_FACE_NORT) - IX + 1
                 LINV_N  =  T
                 LINV_N2 =  3
                ISGNU_N  = -1
                ISGNV_N  = -1
                 CROT_N  =  3
            CASE DEFAULT
                ITARG_N  = SW(MY_FACE_NORT) + IX - 1
        END SELECT
    END IF
!-----------
! SEND SOUTH
!-----------
    ITARG_S  = MYPE - IXM
     LINV_S  = F
     LINV_S2 = 1
    ISGNU_S  = 1
    ISGNV_S  = 1
     LEXC_S  = F
     CROT_S  = 1
!
    IF (LSOUTH) THEN
        SELECT CASE(MY_FACE)
            CASE(01) 
                ITARG_S = NW(MY_FACE_SOUT) + IX - 1
            CASE(02) 
                 ITARG_S  = NE(MY_FACE_SOUT) - (IX - 1) * IXM
                 ISGNU_S  = -1
                  LINV_S  =  T
                  LINV_S2 =  4
                  LEXC_S  =  T
                  CROT_S  =  4
            CASE(03) 
                ITARG_S  = SE(MY_FACE_SOUT) - IX + 1
                 LINV_S  =  T
                 LINV_S2 =  3
                ISGNU_S  = -1
                ISGNV_S  = -1
                 CROT_S  =  3
            CASE(04) 
                ITARG_S  = SW(MY_FACE_SOUT) + (IX - 1) * IXM
                ISGNV_S  = -1
                 LEXC_S  =  T
                 CROT_S  =  2
                 LINV_S2 =  2
            CASE(05) 
                ITARG_S  = NW(MY_FACE_SOUT) + IX - 1
            CASE DEFAULT
                ITARG_S  = SE(MY_FACE_SOUT) - IX + 1
                 LINV_S  =  T
                 LINV_S2 =  3
                ISGNU_S  = -1
                ISGNV_S  = -1
                 CROT_S  =  3
            END SELECT
        END IF
!----------------
! SEND SOUTH-WEST
!----------------
    ITARG_SW = MYPE - IXM - 1
    ISGNU_SW = 1
    ISGNV_SW = 1
     LEXC_SW = F
     CROT_SW = 1
     LSWP_SW = F
!      
    IF (LSOUTH .OR. LWEST) THEN
!
        IF (LSOUTH .AND. LWEST) THEN
            ITARG_SW = -1
            ISGNU_SW =  0
            ISGNV_SW =  0
!
        ELSE IF (LSOUTH) THEN
!
            SELECT CASE(MY_FACE)
                CASE(01)
                    ITARG_SW = ITARG_S - 1
                CASE(02)
                    ITARG_SW = ITARG_S + IXM
                    ISGNU_SW = -1
                     LEXC_SW =  T
                     CROT_SW =  4
                     LSWP_SW =  T
                CASE(03)
                    ITARG_SW = ITARG_S + 1
                    ISGNU_SW = -1
                    ISGNV_SW = -1
                     CROT_SW =  3
                CASE(04)
                    ITARG_SW = ITARG_S - IXM
                    ISGNV_SW = -1
                     LEXC_SW =  T
                     CROT_SW =  2
                     LSWP_SW =  T
                CASE(05)
                    ITARG_SW = ITARG_S - 1
                CASE DEFAULT
                    ITARG_SW = ITARG_S + 1
                    ISGNU_SW = -1
                    ISGNV_SW = -1
                     CROT_SW =  3
            END SELECT
!
        ELSE IF (LWEST) THEN
!
            SELECT CASE(MY_FACE)
                CASE(05)
                    ITARG_SW = ITARG_W + 1
                    ISGNV_SW = -1
                     LEXC_SW =  T
                     CROT_SW =  2
                     LSWP_SW =  T
                CASE(06)
                    ITARG_SW = ITARG_W - 1
                    ISGNU_SW = -1
                     LEXC_SW =  T
                     CROT_SW =  4
                     LSWP_SW =  T
                CASE DEFAULT
                    ITARG_SW = ITARG_W - IXM
            END SELECT
!
        END IF
!
    END IF
!----------------
! SEND SOUTH-EAST
!----------------
    ITARG_SE = MYPE - IXM + 1
    ISGNU_SE = 1
    ISGNV_SE = 1
     LEXC_SE = F
     CROT_SE = 1
     LSWP_SE = F
!
    IF (LSOUTH .OR. LEAST) THEN
!
        IF (LSOUTH .AND. LEAST) THEN
            ITARG_SE = -1
            ISGNU_SE =  0
            ISGNV_SE =  0
!
        ELSE IF (LSOUTH) THEN
!
            SELECT CASE(MY_FACE)
                CASE(01)
                    ITARG_SE = ITARG_S + 1
                CASE(02)
                    ITARG_SE = ITARG_S - IXM
                    ISGNU_SE = -1
                     LEXC_SE =  T
                     CROT_SE =  4
                     LSWP_SE =  T
                CASE(03)
                    ITARG_SE = ITARG_S - 1
                    ISGNU_SE = -1
                    ISGNV_SE = -1
                     CROT_SE =  3
                CASE(04)
                    ITARG_SE = ITARG_S + IXM
                    ISGNV_SE = -1
                     LEXC_SE =  T
                     CROT_SE =  2
                     LSWP_SE =  T
                CASE(05)
                    ITARG_SE = ITARG_S + 1
                CASE DEFAULT
                    ITARG_SE = ITARG_S - 1
                    ISGNU_SE = -1
                    ISGNV_SE = -1
                     CROT_SE =  3
            END SELECT
!
        ELSE IF (LEAST) THEN
!
            SELECT CASE(MY_FACE)
                CASE(05)
                    ITARG_SE = ITARG_E - 1
                    ISGNU_SE = -1
                     LEXC_SE =  T
                     CROT_SE =  4
                     LSWP_SE =  T
                CASE(06)
                    ITARG_SE = ITARG_E + 1
                    ISGNV_SE = -1
                     LEXC_SE =  T
                     CROT_SE =  2
                     LSWP_SE =  T
                CASE DEFAULT
                    ITARG_SE = ITARG_E - IXM
            END SELECT
!
        END IF
!
    END IF
!----------------
! SEND NORTH-WEST
!----------------
    ITARG_NW = MYPE + IXM - 1
    ISGNU_NW = 1
    ISGNV_NW = 1
     LEXC_NW = F
     CROT_NW = 1
     LSWP_NW = T
!      
    IF (LNORTH .OR. LWEST) THEN
!
        IF (LNORTH .AND. LWEST) THEN
            ITARG_NW = -1
            ISGNU_NW =  0
            ISGNV_NW =  0
!
        ELSE IF (LNORTH) THEN
!
            SELECT CASE(MY_FACE)
                CASE(01)
                    ITARG_NW = ITARG_N - 1
                CASE(02)
                    ITARG_NW = ITARG_N - IXM
                    ISGNV_NW = -1
                     LEXC_NW =  T
                     CROT_NW =  2
                     LSWP_NW =  T
                CASE(03)
                    ITARG_NW = ITARG_N + 1
                    ISGNU_NW = -1
                    ISGNV_NW = -1
                     CROT_NW =  3
                CASE(04)
                    ITARG_NW = ITARG_N + IXM
                    ISGNU_NW = -1
                     LEXC_NW =  T
                     CROT_NW =  4
                     LSWP_NW =  T
                CASE(05)
                    ITARG_NW = ITARG_N + 1
                    ISGNU_NW = -1
                    ISGNV_NW = -1
                     CROT_NW =  3
                CASE DEFAULT
                    ITARG_NW = ITARG_N - 1
            END SELECT
!
        ELSE IF (LWEST) THEN
!
            SELECT CASE(MY_FACE)
                CASE(05)
                    ITARG_NW = ITARG_W - 1
                    ISGNV_NW = -1
                     LEXC_NW =  T
                     CROT_NW =  2
                     LSWP_NW =  T
                CASE(06)
                    ITARG_NW = ITARG_W + 1
                    ISGNU_NW = -1
                     LEXC_NW =  T
                     CROT_NW =  4
                     LSWP_NW =  T
                CASE DEFAULT
                    ITARG_NW = ITARG_W + IXM
            END SELECT
!
        END IF
!
    END IF
!----------------
! SEND NORTH-EAST
!----------------
    ITARG_NE=MYPE+IXM+1
    ISGNU_NE = 1
    ISGNV_NE = 1
     LEXC_NE = F
     CROT_NE = 1
     LSWP_NE = F
!      
    IF (LNORTH .OR. LEAST) THEN
!
        IF (LNORTH .AND. LEAST) THEN
            ITARG_NE = -1
            ISGNU_NE =  0
            ISGNV_NE =  0
!
        ELSE IF (LNORTH) THEN
!
            SELECT CASE(MY_FACE)
                CASE(01)
                    ITARG_NE = ITARG_N + 1
                CASE(02)
                    ITARG_NE = ITARG_N + IXM
                    ISGNV_NE = -1
                     LEXC_NE =  T
                     CROT_NE =  2
                     LSWP_NE =  T
                CASE(03)
                    ITARG_NE = ITARG_N - 1
                    ISGNU_NE = -1
                    ISGNV_NE = -1
                     CROT_NE =  3
                CASE(04)
                    ITARG_NE = ITARG_N - IXM
                    ISGNU_NE = -1
                     LEXC_NE =  T
                     CROT_NE =  4
                     LSWP_NE =  T
                CASE(05)
                    ITARG_NE = ITARG_N - 1
                    ISGNU_NE = -1
                    ISGNV_NE = -1
                     CROT_NE =  3
                CASE DEFAULT
                    ITARG_NE = ITARG_N + 1
            END SELECT
!
        ELSE IF (LEAST) THEN
!
            SELECT CASE(MY_FACE)
                CASE(05)
                    ITARG_NE = ITARG_E + 1
                    ISGNU_NE = -1
                     LEXC_NE =  T
                     CROT_NE =  4
                     LSWP_NE =  T
                CASE(06)
                    ITARG_NE = ITARG_E - 1
                    ISGNV_NE = -1
                     LEXC_NE =  T
                     CROT_NE =  2
                     LSWP_NE =  T
                CASE DEFAULT
                    ITARG_NE = ITARG_E + IXM
            END SELECT
!
        END IF
!
    END IF
!
    LSWC = F
    LNWC = F
    LSEC = F
    LNEC = F
!
    IF (LSOUTH .AND. LWEST) THEN
        LSWC=T
    END IF
!
    IF (LSOUTH .AND. LEAST) THEN
        LSEC=T
    END IF
!
    IF (LNORTH .AND. LWEST) THEN
        LNWC=T
    END IF
!
    IF (LNORTH .AND. LEAST) THEN
        LNEC=T
    END IF
!
    CALL ASSOC
!
    END SUBROUTINE INITTOP1

