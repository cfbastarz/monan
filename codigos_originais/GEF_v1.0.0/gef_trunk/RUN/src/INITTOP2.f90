!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief DEFINE TOPOLOGY
!> @details DEFINE TOPOLOGY - OCTA 1 GRID.
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
    SUBROUTINE INITTOP2
!--------------------------------------------------------------------------------------------------
! MAIN PROGRAM INITTOP2
! 
! MAIN PROGRAM: INITTOP2 - DEFINE TOPOLOGY
! PROGRAMMER: ?????          
! ORG: W/NP22
! DATE: ??-??-??
!
! ABSTRACT:
! DEFINE TOPOLOGY - OCTA 1 GRID
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
! CALLS      : ASSOC
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
    INTEGER(KIND=I4)    ,  DIMENSION(14,8)                                                      ::&
    & MY_FACE_NEB
!
    INTEGER(KIND=I4)                                                                            ::&
    & MY_FACE_EAST      , MY_FACE_WEST      , MY_FACE_NORT      , MY_FACE_SOUT      ,             &
    & MY_FACE_SW        , MY_FACE_SE        , MY_FACE_NW        , MY_FACE_NE
!--------------------------------------------------------------------------------
!
!                               INDEXING FOR MY_NEB()
!
!
!
!    ****************************************************************************
!    *                                                                          *
!    * DEFINITION OF ABSOLUTE COORDINATES ON THE UNIT SPHERE FOR OCTAGONAL GRID *
!    *                                                                          *
!    *                  -------------------------------------                   *
!    *                      SOUTH:                 NORTH:                       *
!    *                  ______________        ______________                    *
!    *                 |    |    |    |      |    |    |    |                   *
!    *                 | XX |  5 | XX |      | 12 | 13 | 14 |                   *
!    *                 |____|____|____|      |____|____|____|                   *
!    *                 |    |    |    |      |    |    |    |                   *
!    *                 | 2  | 3  | 4  |      |  9 | 10 | 11 |                   *
!    *                 |____|____|____|      |____|____|____|                   *
!    *                 |    |    |    |      |    |    |    |                   *
!    *                 | XX | 1  | XX |      |  6 |  7 |  8 |                   *
!    *                 |____|____|____|      |____|____|____|                   *
!    *                                                                          *
!    ****************************************************************************
!
!--------------------------------------------------------------------------------
!
!------------------
! DESCRIBE TOPOLOGY 
!------------------
!
!-------------------------------------
! E    W    N    S   SW   SE   NW   NE
!------------------------------------- 
    MY_FACE_NEB( 1,:) = (/ 6,  8,  3,  7,  0,  0,  2,  4/)
    MY_FACE_NEB( 2,:) = (/ 3, 11, 14,  8,  0,  1,  0,  5/)
    MY_FACE_NEB( 3,:) = (/ 4,  2,  5,  1,  8,  6, 14, 12/)
    MY_FACE_NEB( 4,:) = (/ 9,  3, 12,  6,  1,  0,  5,  0/)
    MY_FACE_NEB( 5,:) = (/12, 14, 13,  3,  2,  4,  0,  0/)
    MY_FACE_NEB( 6,:) = (/ 7,  4,  9,  1,  3,  0,  0, 10/)
    MY_FACE_NEB( 7,:) = (/ 8,  6, 10,  1,  0,  0,  9, 11/)
    MY_FACE_NEB( 8,:) = (/ 2,  7, 11,  1,  0,  3, 10,  0/)
    MY_FACE_NEB( 9,:) = (/10,  4, 12,  6,  0,  7,  0, 13/)
    MY_FACE_NEB(10,:) = (/11,  9, 13,  7,  6,  8, 12, 14/)
    MY_FACE_NEB(11,:) = (/ 2, 10, 14,  8,  7,  0, 13,  0/)
    MY_FACE_NEB(12,:) = (/13,  4,  5,  9,  0, 10,  3,  0/)
    MY_FACE_NEB(13,:) = (/14, 12,  5, 10,  9, 11,  0,  0/)
    MY_FACE_NEB(14,:) = (/ 2, 13,  5, 11, 10,  0,  0,  3/)
!
    MY_FACE_EAST = MY_FACE_NEB(MY_FACE, 1)
    MY_FACE_WEST = MY_FACE_NEB(MY_FACE, 2)
    MY_FACE_NORT = MY_FACE_NEB(MY_FACE, 3)
    MY_FACE_SOUT = MY_FACE_NEB(MY_FACE, 4)
    MY_FACE_SW   = MY_FACE_NEB(MY_FACE, 5)
    MY_FACE_SE   = MY_FACE_NEB(MY_FACE, 6)
    MY_FACE_NW   = MY_FACE_NEB(MY_FACE, 7)
    MY_FACE_NE   = MY_FACE_NEB(MY_FACE, 8)
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
    IF(LEAST) THEN
        SELECT CASE(MY_FACE)
            CASE(1,8)
                ITARG_E  = SW(MY_FACE_EAST) + JYM - JY 
                 LINV_E2 =  2
                ISGNV_E  = -1
                 LINV_E  =  T
                 LEXC_E  =  T
            CASE(5,14)
                ITARG_E  = NW(MY_FACE_EAST) + JY - 1
                 LINV_E2 =  4
                ISGNU_E  = -1
                 LEXC_E  =  T
                 CROT_E  =  2
            CASE DEFAULT
                ITARG_E = SW(MY_FACE_EAST) + (JY - 1) * IXM
        END SELECT
    END IF
!----------
! SEND WEST
!----------
    ITARG_W  =MYPE - 1
     LINV_W  = F
     LINV_W2 = 1
    ISGNU_W  = 1
    ISGNV_W  = 1
     LEXC_W  = F
     CROT_W  = 1
!
    IF (LWEST) THEN
        SELECT CASE(MY_FACE)
            CASE(1,6)
                ITARG_W  = SW(MY_FACE_WEST) + JY - 1
                 LINV_W2 =  4
                ISGNU_W  = -1
                 LEXC_W  =  T
                 CROT_W  =  2
            CASE(5,12)
                ITARG_W  = NW(MY_FACE_WEST) + JYM - JY 
                 LINV_W2 =  2
                ISGNV_W  = -1
                 LINV_W  =  T
                 LEXC_W  =  T
                 CROT_W  =  4
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
    IF(LNORTH) THEN
        SELECT CASE(MY_FACE)
            CASE(2,12)
                ITARG_N  = SE(MY_FACE_NORT) + (IX - 1) * IXM
                 LINV_N2 =  2
                ISGNV_N  = -1
                 LEXC_N  =  T
            CASE(4,14)
                ITARG_N  =  SW(MY_FACE_NORT) + (IXM - IX) * IXM
                 LINV_N2 =  4
                ISGNU_N  = -1
                 LINV_N  =  T
                 LEXC_N  =  T
                 CROT_N  =  2
            CASE(5,13)
                ITARG_N  = NW(MY_FACE_NORT) - IX + IXM
                 LINV_N2 =  3 
                ISGNU_N  = -1
                ISGNV_N  = -1
                 LINV_N  =  T
                 CROT_N  =  3
            CASE DEFAULT
                ITARG_N  = SW(MY_FACE_NORT) + IX - 1
        END SELECT
    END IF

!***
!*** SEND SOUTH
!***
          ITARG_S=MYPE-IXM
          LINV_S=F
          LINV_S2=1
          ISGNU_S=1
          ISGNV_S=1
          LEXC_S=F
          CROT_S=1

IF(LSOUTH) THEN
         SELECT CASE(MY_FACE)
                CASE( 01,7 ) 
          ITARG_S= SW( MY_FACE_SOUT ) +  IXM - IX
          LINV_S2=3
          ISGNU_S=-1
          ISGNV_S=-1
          LINV_S=T
          CASE( 2,6 ) 
          ITARG_S= SE( MY_FACE_SOUT ) + (IXM - IX) * IXM
          LINV_S2=4
          ISGNU_S=-1
          LINV_S=T
          LEXC_S=T
          CROT_S=4
                CASE( 4,8 ) 
          ITARG_S= SW( MY_FACE_SOUT ) +(IX-1)*IXM
          LINV_S2=2
          ISGNU_S=1
          ISGNV_S=-1
          LINV_S=F
          LEXC_S=T
          CROT_S=3
      CASE DEFAULT
          ITARG_S= NW( MY_FACE_SOUT ) +  IX - 1
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
!
            SELECT CASE(MY_FACE)
                CASE(3,6)
                    ITARG_SW = SW(MY_FACE_SW)
                     CROT_SW =  4
                    ISGNU_SW = -1
                     LEXC_SW =  T
                CASE(4,5,10,11,13,14)
                    ITARG_SW = NE(MY_FACE_SW)
                CASE DEFAULT
                    ITARG_SW = -1
            END SELECT
!
        ELSE IF (LSOUTH) THEN
!
            SELECT CASE(MY_FACE)
                CASE(01,7)
                    ITARG_SW = ITARG_S + 1
                     CROT_SW =  3
                    ISGNU_SW = -1
                    ISGNV_SW = -1
                CASE(02,6)
                    ITARG_SW = ITARG_S + IXM
                     CROT_SW =  4
                    ISGNU_SW = -1
                     LEXC_SW =  T
                     LSWP_SW =  T
                CASE(4,8)
                    ITARG_SW = ITARG_S - IXM
                     CROT_SW =  2
                    ISGNV_SW = -1
                     LEXC_SW =  T
                     CROT_SW =  3
                CASE DEFAULT
                    ITARG_SW = ITARG_S - 1
            END SELECT
!
        ELSE IF (LWEST) THEN
!
            SELECT CASE(MY_FACE)
                CASE(1,6)
                    ITARG_SW = ITARG_W - 1
                    ISGNU_SW = -1
                     LEXC_SW =  T
                     CROT_SW =  4
                     LSWP_SW =  T
                CASE( 5,12)
                    ITARG_SW = ITARG_W + 1
                    ISGNV_SW = -1
                     LEXC_SW =  T
                     CROT_SW =  2
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
!
            SELECT CASE(MY_FACE)
                CASE(2,5,9,10,12,13)
                    ITARG_SE = NW(MY_FACE_SE)
                CASE(3,8)
                    ITARG_SE = SW(MY_FACE_SE)
                     CROT_SE =  2
                    ISGNV_SE = -1
                     LEXC_SE =  T
                CASE DEFAULT
                    ITARG_SE = -1
            END SELECT
!
        ELSE IF (LSOUTH) THEN
!
            SELECT CASE(MY_FACE)
                CASE(1,7)
                    ITARG_SE = ITARG_S - 1
                     CROT_SE =  3
                    ISGNU_SE = -1
                    ISGNV_SE = -1
                CASE(2,6)
                    ITARG_SE = ITARG_S - IXM
                    ISGNU_SE = -1
                     LEXC_SE =  T
                     CROT_SE =  4
                     LSWP_SE =  T
                CASE(4,8)
                    ITARG_SE = ITARG_S + IXM
                    ISGNV_SE = -1
                     CROT_SE =  2
                     LEXC_SE =  T
                CASE DEFAULT
                    ITARG_SE = ITARG_S + 1
                    ISGNU_SE = -1
                    ISGNV_SE = -1
                     CROT_SE =  3
            END SELECT
!
        ELSE IF (LEAST) THEN
!
            SELECT CASE(MY_FACE)
                CASE(1,8)
                    ITARG_SE = ITARG_E + 1
                    ISGNV_SE = -1
                     LEXC_SE =  T
                     CROT_SE =  2
                     LSWP_SE =  T
                CASE(5,14)
                    ITARG_SE = ITARG_E - 1
                    ISGNU_SE = -1
                     LEXC_SE =  T
                     CROT_SE =  4
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
!
            SELECT CASE(MY_FACE)
                CASE(1,4,7,8,10,11)
                    ITARG_NW = SE(MY_FACE_NW)
                CASE(3,12)
                    ITARG_NW = NE(MY_FACE_NW)
                     CROT_NW =  2
                    ISGNV_NW = -1
                     LEXC_NW =  T
                CASE DEFAULT
                    ITARG_NW = -1
            END SELECT
!
        ELSE IF (LNORTH) THEN
!
            SELECT CASE(MY_FACE)
                CASE(2,12)
                    ITARG_NW = ITARG_N - IXM
                     CROT_NW =  2
                    ISGNV_NW = -1
                     LEXC_NW =  T
                CASE(4,14)
                    ITARG_NW = ITARG_N + IXM
                    ISGNU_NW = -1
                     LEXC_NW =  T
                     CROT_NW =  4
                     LSWP_NW =  T
                CASE(5,13)
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
                CASE(1,6)
                    ITARG_NW = ITARG_W + 1
                    ISGNU_NW = -1
                     LEXC_NW =  T
                     CROT_NW =  4
                     LSWP_NW =  T
                CASE(5,12)
                    ITARG_NW = ITARG_W - 1
                    ISGNV_NW = -1
                     LEXC_NW =  T
                     CROT_NW =  2
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
    ITARG_NE = MYPE + IXM + 1
    ISGNU_NE = 1
    ISGNV_NE = 1
     LEXC_NE = F
     CROT_NE = 1
     LSWP_NE = F
!
    IF (LNORTH .OR. LEAST) THEN
!
        IF (LNORTH .AND. LEAST) THEN
!
            SELECT CASE(MY_FACE)
                CASE(1,2,6,7,9,10)
                    ITARG_NE = SW(MY_FACE_NE)
                CASE(3,14)
                    ITARG_NE = NW(MY_FACE_NE)
                     CROT_NE =  4
                    ISGNU_NE = -1
                     LEXC_NE =  T
                CASE DEFAULT
                    ITARG_NE = -1
            END SELECT
!
        ELSE IF (LNORTH) THEN
!
            SELECT CASE(MY_FACE)
                CASE(2,12)
                    ITARG_NE = ITARG_N + IXM
                     CROT_NE =  2
                    ISGNV_NE = -1
                     LEXC_NE =  T
                CASE(4,14)
                    ITARG_NE = ITARG_N - IXM
                    ISGNU_NE = -1
                     LEXC_NE =  T
                     CROT_NE =  4
                     LSWP_NE =  T
                CASE(5,13)
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
                CASE(1,8)
                    ITARG_NE =  ITARG_E - 1
                    ISGNV_NE = -1
                     LEXC_NE =  T
                     CROT_NE =  2
                     LSWP_NE =  T
                CASE(5,14)
                    ITARG_NE = ITARG_E + 1
                    ISGNU_NE = -1
                     LEXC_NE =  T
                     CROT_NE =  4
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
    SELECT CASE(MY_FACE)
        CASE(1,2,7,8,9,12)
            IF (LSOUTH .AND. LWEST) THEN
                LSWC = T
            END IF
    END SELECT
!
    SELECT CASE(MY_FACE)
        CASE(1,4,6,7,11,14)
            IF (LSOUTH .AND. LEAST) THEN
                LSEC = T
            END IF
    END SELECT
!
    SELECT CASE(MY_FACE)
        CASE(2,5,6,9,13,14)
            IF (LNORTH .AND. LWEST) THEN
                LNWC = T
            END IF
    END SELECT
!
    SELECT CASE(MY_FACE)
        CASE(4,5,8,11,12,13)
            IF (LNORTH .AND. LEAST) THEN
                LNEC = T
            END IF
    END SELECT
!
    CALL ASSOC
!
    END SUBROUTINE INITTOP2

