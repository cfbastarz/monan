!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief DEFINES VARIABLES TO REPRESENT FLOATING-POINT CONSTANTS.
!> @details  THE NAMING CONVENTIONS FOR THE FLOATING-POINT VARIABLES ARE AS FOLLOWS:
!!
!!   1) PHYSICAL AND MATHEMATICAL CONSTANTS WILL BE GIVEN NAMES RELEVANT TO THEIR MEANING
!!
!!   2) OTHER CONSTANTS WILL BE GIVEN NAMES RELEVANT TO THEIR VALUE AND ADHERING TO THE FOLLOWING
!!      CONVENTIONS:
!!
!!      A) THE FIRST LETTER WILL BE REPRESENTED WITH AN 'H' EXCEPT FOR I) AND J) BELOW
!!
!!      B) A DECIMAL POINT WILL BE REPRESENTED WITH A 'P'
!!
!!      C) THERE WILL BE NO EMBEDDED '0'(ZERO); ALL 0S WILL BE REPRESENTED WITH A 'Z'
!!
!!      D) A MINUS SIGN WILL BE REPRESENTED WITH AN 'M'
!!
!!      E) THE DECIMAL POINT IS ASSUMED AFTER THE FIRST DIGIT FOR NUMBERS WITH EXPONENTS
!!
!!      F) POSITIVE EXPONENTS ARE INDICATED WITH 'E';NEGATIVE EXPONENTS WITH 'M'
!!
!!      G) DIGITS ARE TRUNCATED IN ORDER TO HAVE NO MORE THAN 8 CHARACTERS PER NAME
!!
!!      H) NUMBERS LESS THAN 0.1 AND GREATER THAN 10. WILL BE REPRESENTED IN EXPONENT FORMAT 
!!         (EXCEPT A FEW SPECIAL CASES)
!!
!!      I) THE WHOLE NUMBERS FROM 0.0 THROUGH 10.,AND 20., 30., 40., 50., 60., 70., 80., 90., 100.,
!!         WILL BE SPELLED OUT
!!
!!      J) GOOD JUDGMENT WILL PREVAIL OVER ALL CONVENTIONS
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
!! @arg @c F77KINDS
!! @arg @c HCON
!> @details <b>Driver:</b> 
!! @arg @c GRADFS
!> @details <b>Calls:</b>
!! @arg @c AVRH
!! @arg @c BOCOHMPI
!! @arg @c CORNERHM2
!--------------------------------------------------------------------------------------------------
    SUBROUTINE HCONST
!--------------------------------------------------------------------------------------------------
! SUBROUTINE HCONST
! 
! SUBPROGRAM: HCONST - DEFINES VARIABLES TO REPRESENT FLOATING-POINT CONSTANTS.
! PROGRAMMER: ????? 
! ORG: ?????
! DATE: ??-??-??
!
! ABSTRACT:
! THE NAMING CONVENTIONS FOR THE FLOATING-POINT VARIABLES ARE AS FOLLOWS:
!
!   1) PHYSICAL AND MATHEMATICAL CONSTANTS WILL BE GIVEN NAMES RELEVANT TO THEIR MEANING
!   2) OTHER CONSTANTS WILL BE GIVEN NAMES RELEVANT TO THEIR VALUE AND ADHERING TO THE FOLLOWING
!      CONVENTIONS:
!      A) THE FIRST LETTER WILL BE REPRESENTED WITH AN 'H' EXCEPT FOR I) AND J) BELOW
!      B) A DECIMAL POINT WILL BE REPRESENTED WITH A 'P'
!      C) THERE WILL BE NO EMBEDDED '0'(ZERO); ALL 0S WILL BE REPRESENTED WITH A 'Z'
!      D) A MINUS SIGN WILL BE REPRESENTED WITH AN 'M'
!      E) THE DECIMAL POINT IS ASSUMED AFTER THE FIRST DIGIT FOR NUMBERS WITH EXPONENTS
!      F) POSITIVE EXPONENTS ARE INDICATED WITH 'E';NEGATIVE EXPONENTS WITH 'M'
!      G) DIGITS ARE TRUNCATED IN ORDER TO HAVE NO MORE THAN 8 CHARACTERS PER NAME
!      H) NUMBERS LESS THAN 0.1 AND GREATER THAN 10. WILL BE REPRESENTED IN EXPONENT FORMAT 
!         (EXCEPT A FEW SPECIAL CASES)
!      I) THE WHOLE NUMBERS FROM 0.0 THROUGH 10.,AND 20., 30., 40., 50., 60., 70., 80., 90., 100.,
!         WILL BE SPELLED OUT
!      J) GOOD JUDGMENT WILL PREVAIL OVER ALL CONVENTIONS
!
! EXAMPLES
!   CONSTANT             VARIABLE NAME           CONVENTION
!   600.                 LHEATC                  1)
!   680.                 LHEATS                  1)
!     1.4142             SQROOT2                 1)
!     2.0                TWO                     2)-(I)
!    -3.0                HM3PZ                   2)-(A,B,D)
!   310.                 C31E2                   2)-(A,E,F,H)
!    -0.7239E-9          HM723M1Z                2)-(A,C,D,E,F,G,H)
!     0.0                ZERO                    2)-(I)
!     0.1                HP1                     2)-(A,B,H)
!     0.01               H1M2                    2)-(A,E,F,H)
!    30.                 THIRTY                  2)-(H,I)
!     0.5                HAF                     2)-(J)
!     9.0                HNINE                   2)-(J)
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
! USE MODULES: F77KINDS
!              HCON
!          
! DRIVER     : GRADFS
!
! CALLS      : ----- 
!--------------------------------------------------------------------------------------------------
    USE F77KINDS
    USE HCON
!
    IMPLICIT NONE
!-------------------------------------------------------------------- 
! THE FOLLOWING ARE PHYSICAL CONSTANTS ARRANGED IN ALPHABETICAL ORDER
!--------------------------------------------------------------------
    AMOLWT   =      28.9644
    CSUBP    =       1.00484E7
    DIFFCTR  =       1.66
    G        =     980.665
    GINV     =       1.        /        G
    GRAVDR   =     980.0
    O3DIFCTR =       1.90
    P0       = 1013250.
    P0INV    =       1.        /        P0
    GP0INV   = GINV            *        P0INV
    P0XZP2   =  202649.902
    P0XZP8   =  810600.098
    P0X2     =       2.        *        1013250.
    RADCON   =       8.427
    RADCON1  =       1.        /        8.427
    RATCO2MW =       1.519449738
    RATH2OMW =        .622
    RGAS     =       8.3142E7
    RGASSP   =       8.31432E7
    SECPDA   =       8.64E4
!----------------------------------------------------------------------
! THE FOLLOWING ARE MATHEMATICAL CONSTANTS ARRANGED IN DECREASING ORDER
!----------------------------------------------------------------------
    HUNDRED  =   100.
    HNINETY  =    90.
    SIXTY    =    60.
    FIFTY    =    50.
    TEN      =    10.
    EIGHT    =     8.
    FIVE     =     5.
    FOUR     =     4.
    THREE    =     3.
    TWO      =     2.
    ONE      =     1.
    HAF      =     0.5
    QUARTR   =     0.25
    ZERO     =     0.
!---------------------------------------------------------------------------------
! FOLLOWING ARE POSITIVE FLOATING POINT CONSTANTS(HS) ARRANGED IN DECREASING ORDER
!---------------------------------------------------------------------------------
    H83E26   =     8.3E26
    H71E26   =     7.1E26
    H1E15    =     1.E15
    H1E13    =     1.E13
    H1E11    =     1.E11
    H1E8     =     1.E8
    H2E6     =     2.0E6
    H1E6     =     1.0E6
    H69766E5 =     6.97667E5
    H4E5     =     4.E5
    H165E5   =     1.65E5
    H5725E4  = 57250.
    H488E4   = 48800.
    H1E4     =     1.E4
    H24E3    =  2400.
    H20788E3 =  2078.8
    H2075E3  =  2075.
    H18E3    =  1800.
    H1224E3  =  1224.
    H67390E2 =   673.9057
    H5E2     =   500.
    H3082E2  =   308.2
    H3E2     =   300.
    H2945E2  =   294.5
    H29316E2 =   293.16
    H26E2    =   260.0
    H25E2    =   250.
    H23E2    =   230.
    H2E2     =   200.0
    H15E2    =   150.
    H1386E2  =   138.6
    H1036E2  =   103.6
    H8121E1  =    81.21
    H35E1    =    35.
    H3116E1  =    31.16
    H28E1    =    28.
    H181E1   =    18.1
    H18E1    =    18.
    H161E1   =    16.1
    H16E1    =    16.
    H1226E1  =    12.26
    H9P94    =     9.94
    H6P08108 =     6.081081081
    H3P6     =     3.6
    H3P5     =     3.5
    H2P9     =     2.9
    H2P8     =     2.8
    H2P5     =     2.5
    H1P8     =     1.8
    H1P4387  =     1.4387
    H1P41819 =     1.418191
    H1P4     =     1.4
    H1P25892 =     1.258925411
    H1P082   =     1.082
    HP816    =     0.816
    HP805    =     0.805
    HP8      =     0.8
    HP60241  =     0.60241
    HP602409 =     0.60240964
    HP6      =     0.6
    HP526315 =     0.52631579
    HP518    =     0.518
    HP5048   =     0.5048
    HP3795   =     0.3795
    HP369    =     0.369
    HP26     =     0.26
    HP228    =     0.228
    HP219    =     0.219
    HP166666 =      .166666
    HP144    =     0.144
    HP118666 =     0.118666192
    HP1      =     0.1
!----------------------------------- 
! (NEGATIVE EXPONENTIALS BEGIN HERE)
!----------------------------------- 
    H658M2    = 0.0658
    H625M2    = 0.0625
    H44871M2  = 4.4871E-2
    H44194M2  =  .044194
    H42M2     = 0.042
    H41666M2  = 0.0416666
    H28571M2  =  .02857142857
    H2118M2   = 0.02118
    H129M2    = 0.0129
    H1M2      =  .01
    H559M3    = 5.59E-3
    H3M3      = 0.003
    H235M3    = 2.35E-3
    H1M3      = 1.0E-3
    H987M4    = 9.87E-4
    H323M4    = 0.000323
    H3M4      = 0.0003
    H285M4    = 2.85E-4
    H1M4      = 0.0001
    H75826M4  = 7.58265E-4
    H6938M5   = 6.938E-5
    H394M5    = 3.94E-5
    H37412M5  = 3.7412E-5
    H15M5     = 1.5E-5
    H1439M5   = 1.439E-5
    H128M5    = 1.28E-5
    H102M5    = 1.02E-5
    H1M5      = 1.0E-5
    H7M6      = 7.E-6
    H4999M6   = 4.999E-6
    H451M6    = 4.51E-6
    H25452M6  = 2.5452E-6
    H1M6      = 1.E-6
    H391M7    = 3.91E-7
    H1174M7   = 1.174E-7
    H8725M8   = 8.725E-8
    H327M8    = 3.27E-8
    H257M8    = 2.57E-8
    H1M8      = 1.0E-8
    H23M10    = 2.3E-10
    H14M10    = 1.4E-10
    H11M10    = 1.1E-10
    H1M10     = 1.E-10
    H83M11    = 8.3E-11
    H82M11    = 8.2E-11
    H8M11     = 8.E-11
    H77M11    = 7.7E-11
    H72M11    = 7.2E-11
    H53M11    = 5.3E-11
    H48M11    = 4.8E-11
    H44M11    = 4.4E-11
    H42M11    = 4.2E-11
    H37M11    = 3.7E-11
    H35M11    = 3.5E-11
    H32M11    = 3.2E-11
    H3M11     = 3.0E-11
    H28M11    = 2.8E-11
    H24M11    = 2.4E-11
    H23M11    = 2.3E-11
    H2M11     = 2.E-11
    H18M11    = 1.8E-11
    H15M11    = 1.5E-11
    H14M11    = 1.4E-11
    H114M11   = 1.14E-11
    H11M11    = 1.1E-11
    H1M11     = 1.E-11
    H96M12    = 9.6E-12
    H93M12    = 9.3E-12
    H77M12    = 7.7E-12
    H74M12    = 7.4E-12
    H65M12    = 6.5E-12
    H62M12    = 6.2E-12
    H6M12     = 6.E-12
    H45M12    = 4.5E-12
    H44M12    = 4.4E-12
    H4M12     = 4.E-12
    H38M12    = 3.8E-12
    H37M12    = 3.7E-12
    H3M12     = 3.E-12
    H29M12    = 2.9E-12
    H28M12    = 2.8E-12
    H24M12    = 2.4E-12
    H21M12    = 2.1E-12
    H16M12    = 1.6E-12
    H14M12    = 1.4E-12
    H12M12    = 1.2E-12
    H8M13     = 8.E-13
    H46M13    = 4.6E-13
    H36M13    = 3.6E-13
    H135M13   = 1.35E-13
    H12M13    = 1.2E-13
    H1M13     = 1.E-13
    H3M14     = 3.E-14
    H15M14    = 1.5E-14
    H14M14    = 1.4E-14
    H101M16   = 1.01E-16
    H1M16     = 1.0E-16
    H1M17     = 1.E-17
    H1M18     = 1.E-18
    H1M19     = 1.E-19
    H1M20     = 1.E-20
    H1M21     = 1.E-21
    H1M22     = 1.E-22
    H1M23     = 1.E-23
    H1M24     = 1.E-24
    H26M30    = 2.6E-30
    H14M30    = 1.4E-30
    H25M31    = 2.5E-31
    H21M31    = 2.1E-31
    H12M31    = 1.2E-31
    H9M32     = 9.E-32
    H55M32    = 5.5E-32
    H45M32    = 4.5E-32
    H4M33     = 4.E-33
    H62M34    = 6.2E-34
    H1M60     = 1.0E-33
!-----------------------------------------------------------------------------------
! FOLLOWING ARE NEGATIVE FLOATING POINT CONSTANTS (HMS) ARRANGED IN DESCENDING ORDER
!-----------------------------------------------------------------------------------
    HM2M2    =    -.02
    HM6666M2 =    -.066667
    HMP5     =   -0.5
    HMP575   =   -0.575
    HMP66667 =    -.66667
    HMP805   =   -0.805
    HM1EZ    =   -1.
    HM13EZ   =   -1.3
    HM19EZ   =   -1.9
    HM1E1    =  -10.
    HM1597E1 =  -15.97469413
    HM161E1  =  -16.1
    HM1797E1 =  -17.97469413
    HM181E1  =  -18.1
    HM8E1    =  -80.
    HM1E2    = -100.
    G        =  980.665
!
    RETURN
!
    END SUBROUTINE HCONST
