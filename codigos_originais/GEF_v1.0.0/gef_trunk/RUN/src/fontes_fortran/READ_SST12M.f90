!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Inserir Brief de READ_SST12M
!> @details Inserir Details de READ_SST12M
!> @author ORIGINATOR - MEYS
!> @date 97-??-?? \n
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
!> @param[in]  NHB  - FILE NUMBER OF THE NHB FILE
!> @details <b>Use Module:</b>
!! @arg @c ACMCLD
!! @arg @c ACMCLH
!! @arg @c ACMPRE
!! @arg @c ACMRDL
!! @arg @c ACMRDS
!! @arg @c ACMSFC
!! @arg @c CLDWTR
!! @arg @c CNVCLD
!! @arg @c CONTIN
!! @arg @c CTLBLK
!! @arg @c DYNAM
!! @arg @c F77KINDS
!! @arg @c LOOPS
!! @arg @c MAPOT
!! @arg @c MASKS
!! @arg @c PARMETA
!! @arg @c PARMSOIL
!! @arg @c PARM_TBL
!! @arg @c PHYS
!! @arg @c PVRBLS
!! @arg @c SOIL
!! @arg @c VRBLS
!> @details <b>Driver:</b> 
!<
!> @details <b>Calls:</b>
!--------------------------------------------------------------------------------------------------     
    SUBROUTINE READ_SST12M(NHB)
!--------------------------------------------------------------------------------------------------
! SUBROUTINE READ_SST12M
! 
! SUBPROGRAM: READ_SST12M - ?????
! PROGRAMMER: MEYS 
! ORG: ?????
! DATE: 97-??-??
!
! ABSTRACT:  
! ?????
!
! PROGRAM HISTORY LOG:
! 97-??-??  MEYS     - ORIGINATOR
! 97-08-??  BLACK    - REWROTE FOR BENCHMARK
! 98-??-??  TUCCILLO - MODIFIED FOR SINGLE OR DOUBLE PRECISION
! 18-03-20  LUCCI    - MODERNIZATION OF THE CODE, INCLUDING:
!                      * F77 TO F90/F95
!                      * INDENTATION & UNIFORMIZATION CODE
!                      * REPLACEMENT OF COMMONS BLOCK FOR MODULES
!                      * DOCUMENTATION WITH DOXYGEN
!                      * OPENMP FUNCTIONALITY 
!
!
! INPUT ARGUMENT LIST:
! NHB - FILE NUMBER OF THE NHB FILE
!
! OUTPUT ARGUMENT LIST:
! 
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
! USE MODULES: ACMCLD
!              ACMCLH
!              ACMPRE
!              ACMRDL
!              ACMRDS
!              ACMSFC
!              CLDWTR
!              CNVCLD
!              CONTIN
!              CTLBLK
!              DYNAM
!              F77KINDS
!              LOOPS
!              MAPOT
!              MASKS
!              PARMETA
!              PARMSOIL
!              PARM_TBL
!              PHYS
!              PVRBLS
!              SOIL
!              VRBLS
!
! DRIVER     : -----
!
! CALLS      : -----           
!--------------------------------------------------------------------------------------------------
    USE ACMCLD
    USE ACMCLH
    USE ACMPRE
    USE ACMRDL
    USE ACMRDS
    USE ACMSFC
    USE CLDWTR
    USE CNVCLD
    USE CONTIN
    USE CTLBLK
    USE DYNAM
    USE F77KINDS
    USE LOOPS
    USE MAPOT
    USE MASKS
    USE PARMETA
    USE PARMSOIL
    USE PARM_TBL
    USE PHYS
    USE PVRBLS
    USE SOIL
    USE VRBLS
!
    REAL   (KIND=R4)    , PARAMETER :: G      =    9.8
    REAL   (KIND=R4)    , PARAMETER :: CM1    = 2937.4
    REAL   (KIND=R4)    , PARAMETER :: CM2    =    4.9283
    REAL   (KIND=R4)    , PARAMETER :: CM3    =   23.5518
    REAL   (KIND=R4)    , PARAMETER :: EPS    =    0.622
    REAL   (KIND=R4)    , PARAMETER :: Z0LAND =     .10  
    REAL   (KIND=R4)    , PARAMETER :: Z0SEA  =     .001
    REAL   (KIND=R4)    , PARAMETER :: FCM    =     .00001
    REAL   (KIND=R4)    , PARAMETER :: DTR    =    0.1745329E-1
    REAL   (KIND=R4)    , PARAMETER :: H360   =  360.0
    REAL   (KIND=R4)    , PARAMETER :: H1905  =  190.5
    REAL   (KIND=R4)    , PARAMETER :: H105   =  105.0
    REAL   (KIND=R4)    , PARAMETER :: Q2INI  =     .50
    REAL   (KIND=R4)    , PARAMETER :: EPSQ   =    2.E-12
    REAL   (KIND=R4)    , PARAMETER :: EPSWET =    0.0
!
    CHARACTER(LEN=32)                                                                           ::&
    & LABEL
!
    CHARACTER(LEN=80)                                                                           ::&
    & CONTRL  , FILALL  , FILMST  , FILTMP  , FILTKE  , FILUNV  , FILCLD  , FILRAD  , FILSFC
!
    INTEGER(KIND=I4)    , DIMENSION(3)                                                          ::&
    & IDATB
!
    REAL   (KIND=R4)    , DIMENSION(0:IM, 0:JM, 385)                                            ::&
    & GMS
!
    INTEGER(KIND=I8)    , DIMENSION(IM,JM)                                                      ::&
    & ITEMPX
!
    INTEGER(KIND=I8)                                                                            ::&
    & NFCSTX  , NBCX    , LISTX   , IDTADX  ,                                                     &
    & KHLAX   , KHHAX   , KVLAX   , KVHAX   , KHL2X   , KHH2X   , KVL2X   , KVH2X   ,             &
    & IXMX    , IYMX
!
    LOGICAL(KIND=L8)                                                                            ::&
    & SIGMAX
!-------------------------------------------------------------------------------------- 
! ADAPTADO PARA A LEITURA DO ARQUIVO DE SST MENSAL A DATA INICIAL DO ARQUIVO E JAN 1959
!-------------------------------------------------------------------------------------- 
    OPEN (1, FORM='UNFORMATTED', FILE='../data/data_in/monthly_sst.bin', ACCESS='SEQUENTIAL') 
!----------------
! DISTRIBUTE SSTM
!----------------
    DO M=1,356
        IF (MYPE == 0) THEN
            READ(1) TEMP1     
        END IF
    END DO
!
    OPEN (8, FORM='FORMATTED', FILE='../data/data_in/tempo_init_sst.txt')
!
    READ(8,*) INITSST      
!------------------------------ 
! END OF SUBROUTINE READ_SST12M
!------------------------------
    RETURN
!
    END SUBROUTINE READ_SST12M
