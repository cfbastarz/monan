!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module CTLBLK... 
!! @details Details of Module CTLBLK... 
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS 
!! @details <b>Driver:</b> 
!! @arg @c ADJUST
!! @arg @c BOCOHMPI
!! @arg @c BOCOVMPI_SC
!! @arg @c CHECKMXMN
!! @arg @c CLTEND
!! @arg @c CUCNVC
!! @arg @c DDAMP
!! @arg @c DIGFLT
!! @arg @c GEF
!! @arg @c GSMDRIVE
!! @arg @c HDIFF
!! @arg @c HZADV
!! @arg @c HZADVQ
!! @arg @c INIT
!! @arg @c INITCKMM
!! @arg @c KFDRIVE
!! @arg @c KFPARA
!! @arg @c KFTEND
!! @arg @c KINEN
!! @arg @c MIXLEN
!! @arg @c NEWTON
!! @arg @c OUT
!! @arg @c OUTSD
!! @arg @c OUT2
!! @arg @c OUT_HEAT
!! @arg @c PDETE
!! @arg @c PDNEW
!! @arg @c PRODQ2
!! @arg @c RADTN
!! @arg @c RDTEMP
!! @arg @c READ_SST12M
!! @arg @c SFCDIF
!! @arg @c SFLX
!! @arg @c SOLARD
!! @arg @c SSTCH
!! @arg @c SURFCE
!! @arg @c TURBL
!! @arg @c VDIFH
!! @arg @c VDIFV
!! @arg @c VEGUPDT
!! @arg @c VTADV
!! @arg @c ZENITH 
!<
!--------------------------------------------------------------------------------------------------
    MODULE CTLBLK
!--------------------------------------------------------------------------------------------------
! MODULE CTLBLK
!
! USE MODULES: F77KINDS
!
! DRIVER     : ADJUST
!              BOCOHMPI
!              BOCOVMPI_SC
!              CHECKMXMN
!              CLTEND
!              CUCNVC
!              DDAMP
!              DIGFLT
!              GEF
!              GSMDRIVE
!              HDIFF
!              HZADV
!              HZADVQ
!              INIT
!              INITCKMM
!              KFDRIVE
!              KFPARA
!              KFTEND
!              KINEN
!              MIXLEN
!              NEWTON
!              OUT
!              OUTSD
!              OUT2
!              OUT_HEAT
!              PDETE
!              PDNEW
!              PRODQ2
!              RADTN
!              RDTEMP
!              READ_SST12M
!              SFCDIF
!              SFLX
!              SOLARD
!              SSTCH
!              SURFCE
!              TURBL
!              VDIFH
!              VDIFV
!              VEGUPDT
!              VTADV
!              ZENITH  
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    SAVE 
!
    INTEGER(KIND=I4)    , DIMENSION(3)                                                          ::&
    & IDAT
!
    LOGICAL(KIND=L4)                                                                            ::&
    & RUN     , FIRST   , RESTRT  , SIGMA   , SUBPOST , LCORNERM
!
    INTEGER(KIND=I4)                                                                            ::&
    & IOUT    , IHRST   , NFCST   , NBC     , LIST    , NTSD    , NTSTM   , NDDAMP  , NPREC   ,   &
    & IDTAD   , NBOCO   , NSHDE   , NCP     , NPHS    , NCNVC   , NRADS   , NRADL   , NTDDMP  ,   &
    & NSTART  , OUTSTR  , NDASSIM , NDASSIMM, INITSS
!
    INTEGER(KIND=I4)                                                                            ::&
    & IDTAD2
!
!RESTART
    INTEGER(KIND=I4)                                                                            ::&
    & NTSD_INIT           
!!!
    REAL   (KIND=R4)                                                                            ::&
    & DT
!
    END MODULE CTLBLK
