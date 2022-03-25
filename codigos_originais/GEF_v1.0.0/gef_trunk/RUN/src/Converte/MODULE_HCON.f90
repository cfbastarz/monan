!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module HCON... 
!! @details Details of Module HCON... 
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS
!! @details <b>Driver:</b> 
!! @arg @c CLO89
!! @arg @c CONRAD
!! @arg @c E1E290
!! @arg @c E290
!! @arg @c E2SPEC
!! @arg @c E3V88
!! @arg @c FST88
!! @arg @c GRADFS
!! @arg @c HCONST
!! @arg @c LWR88
!! @arg @c RADFS
!! @arg @c SPA88
!! @arg @c SWR93
!! @arg @c TABLE
!<
!--------------------------------------------------------------------------------------------------
    MODULE HCON
!--------------------------------------------------------------------------------------------------
! MODULE HCON
!
! USE MODULES: F77KINDS
! 
! DRIVER     : CLO89
!              CONRAD
!              E1E290
!              E290
!              E2SPEC
!              E3V88
!              FST88
!              GRADFS
!              HCONST
!              LWR88
!              RADFS
!              SPA88
!              SWR93
!              TABLE
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
!
    IMPLICIT NONE
!
    SAVE
!
    REAL   (KIND=R4)                                                                            ::& 
    & AMOLWT  , CSUBP   , DIFFCTR , G       , GRAVDR  , O3DIFCTR , P0     , P0XZP2  , P0XZP8  ,   &
    & P0X2    , RADCON  , RGAS    , RGASSP  , SECPDA  , RATCO2MW , RATH2OMW         , RADCON1 ,   &
    & GINV    , P0INV   , GP0INV  , HUNDRED , HNINETY , SIXTY    , FIFTY  , TEN     , EIGHT   ,   &
    & FIVE    , FOUR    , THREE   , TWO     , ONE     , HAF      , QUARTR , ZERO    , H83E26  ,   &
    & H71E26  , H1E15   , H1E13   , H1E11   , H1E8    , H4E5     , H165E5 , H5725E4 , H488E4  ,   &
    & H1E4    , H24E3   , H20788E3, H2075E3 , H1224E3 , H5E2     , H3082E2, H3E2    , H2945E2 ,   &
    & H23E2   , H15E2   , H35E1   , H3P6    , H181E1  , H18E1    , H2P9   , H2P8    , H2P5    ,   &
    & H1P8    , H1P4387 , H1P4    , H1P25892, HP8     , HP518    , HP369  , HP1     , H44871M2,   &
    & H559M3  , H1M3    , H987M4  , H285M4  , H1M4    , H6938M5  , H394M5 , H37412M5, H1439M5 ,   &
    & H128M5  , H1M5    , H7M6    , H4999M6 , H25452M6, H1M6     , H391M7 , H1174M7 , H8725M8 ,   &
    & H327M8  , H257M8  , H1M8    , H23M10  , H14M10  , H11M10   , H1M10  , H83M11  , H82M11  ,   &
    & H8M11   , H77M11  , H72M11  , H53M11  , H48M11  , H44M11   , H42M11 , H37M11  , H35M11  ,   & 
    & H32M11  , H3M11   , H28M11  , H24M11  , H23M11  , H2M11    , H18M11 , H15M11  , H14M11  ,   &
    & H114M11 , H11M11  , H1M11   , H96M12  , H93M12  , H77M12   , H74M12 , H65M12  , H62M12  ,   &
    & H6M12   , H45M12  , H44M12  , H4M12   , H38M12  , H37M12   , H3M12  , H29M12  , H28M12  ,   &
    & H24M12  , H21M12  , H16M12  , H14M12  , H12M12  , H8M13    , H46M13 , H36M13  , H135M13 ,   &
    & H12M13  , H1M13   , H3M14   , H15M14  , H14M14  , H1M17    , H1M18  , H1M19   , H1M20   ,   &
    & H1M21   , H1M22   , H1M23   , H1M24   , H26M30  , H14M30   , H25M31 , H21M31  , H12M31  ,   &
    & H9M32   , H55M32  , H45M32  , H4M33   , H62M34  , H1M60    , HMP575 , HM13EZ  , HM19EZ  ,   &
    & HM1E1   , HM181E1 , HM1E2   , H1E6    , H2E6    , H1M2     , HMP66667         , HM6666M2,   &
    & HP166666, H41666M2, HMP5    , HM2M2   , H29316E2, H1226E1  , H3116E1, H9P94   , HP6     ,   &
    & H625M2  , HP228   , HP60241 , HM1797E1, H8121E1 , H2E2     , HM1EZ  , H26E2   , H44194M2,   &
    & H1P41819, HP219   , HP144   , HP816   , H69766E5, H235M3   , HP26   , H129M2  , H75826M4,   &
    & H1P082  , HP805   , H1386E2 , H658M2  , H1036E2 , H2118M2  , H42M2  , H323M4  , H67390E2,   &
    & HP3795  , HP5048  , H102M5  , H451M6  , H16E1   , HM161E1  , H161E1 , H3M3    , H101M16 ,   &
    & HM1597E1, H25E2   , HP118666, H15M5   , H3P5    , H18E3    , H6P08108         , HMP805  ,   &
    & HP602409, HP526315, H28571M2, H1M16   , H3M4    , HM8E1    , H28E1 
!
    END MODULE HCON 
