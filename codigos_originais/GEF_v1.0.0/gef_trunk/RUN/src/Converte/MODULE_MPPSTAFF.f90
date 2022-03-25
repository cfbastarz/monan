!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module MPPSTAFF...
!! @details Details of Module MPPSTAFF...
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS
!! @details <b>Driver:</b> 
!! @arg @c ADJUST
!! @arg @c BOCOHMPI
!! @arg @c BOCOVMPI
!! @arg @c BOCOVMPI_SC
!! @arg @c BOCOV_HMPI
!! @arg @c CHECKMXMN
!! @arg @c CHSST
!! @arg @c CLTEND
!! @arg @c CONRAD
!! @arg @c CUCNVC
!! @arg @c DDAMP
!! @arg @c DIGFLT
!! @arg @c E1E290
!! @arg @c E3V88
!! @arg @c FINISHMPI
!! @arg @c FST88
!! @arg @c GEF
!! @arg @c GRADFS
!! @arg @c GSMCOLUMN
!! @arg @c GSMCONST
!! @arg @c GSMDRIVE
!! @arg @c HDIFF
!! @arg @c HZADV
!! @arg @c HZADVQ
!! @arg @c INIT
!! @arg @c INITDOM
!! @arg @c INITMPI
!! @arg @c INITTOP1
!! @arg @c INITTOP2
!! @arg @c KFDRIVE
!! @arg @c KFPARA
!! @arg @c KFTEND
!! @arg @c KINEN
!! @arg @c LWR88
!! @arg @c MIXLEN
!! @arg @c NEWTON
!! @arg @c O3CLIM
!! @arg @c OUT
!! @arg @c OUTSD
!! @arg @c OUT2
!! @arg @c OUT_HEAT
!! @arg @c OZON2D
!! @arg @c PDETE
!! @arg @c PDNEW
!! @arg @c PRODQ2
!! @arg @c RADFS
!! @arg @c RADTN
!! @arg @c RDTEMP
!! @arg @c SFCDIF
!! @arg @c SFLX
!! @arg @c SHAP_FILTER
!! @arg @c SPA88
!! @arg @c SSTCH
!! @arg @c SURFCE
!! @arg @c SWR93
!! @arg @c TABLE
!! @arg @c TURBL
!! @arg @c VDIFH
!! @arg @c VDIFV
!! @arg @c VEGUPDT
!! @arg @c VTADV
!! @arg @c ZENITH
!<
!--------------------------------------------------------------------------------------------------
    MODULE MPPSTAFF
!>--------------------------------------------------------------------------------------------------
!> MODULE MPPSTAFF
!>
!> USE MODULES: F77KINDS
!>
!> DRIVER     : ADJUST
!               BOCOHMPI
!               BOCOVMPI
!               BOCOVMPI_SC
!               BOCOV_HMPI
!               CHECKMXMN
!               CHSST
!               CLTEND
!               CONRAD
!               CUCNVC
!               DDAMP
!               DIGFLT
!               E1E290
!               E3V88
!               FINISHMPI
!               FST88
!               GEF
!               GRADFS
!               GSMCOLUMN
!               GSMCONST
!               GSMDRIVE
!               HDIFF
!               HZADV
!               HZADVQ
!               INIT
!               INITDOM
!               INITMPI
!               INITTOP1
!               INITTOP2
!               KFDRIVE
!               KFPARA
!               KFTEND
!               KINEN
!               LWR88
!               MIXLEN
!               NEWTON
!               O3CLIM
!               OUT
!               OUTSD
!               OUT2
!               OUT_HEAT
!               OZON2D
!               PDETE
!               PDNEW
!               PRODQ2
!               RADFS
!               RADTN
!               RDTEMP
!               SFCDIF
!               SFLX
!               SHAP_FILTER
!               SPA88
!               SSTCH
!               SURFCE
!               SWR93
!               TABLE
!               TURBL
!               VDIFH
!               VDIFV
!               VEGUPDT
!               VTADV
!               ZENITH 
!>--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    SAVE
!
    CHARACTER(LEN=4)                                                                            ::&
    & C_MYPE
!       
    INTEGER(KIND=I4)                                                                            ::&
    & MYPE    , NPES    , ITYPE   ,  MPI_COMM_COMP    , LISTINVRBLS       , LISTINCONST       ,   &
    & LISTOUT , DGNSNUM , DGNSTAG
!
    END MODULE MPPSTAFF
