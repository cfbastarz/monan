!--------------------------------------------------------------------------------------------------
!  DOXYGEN
!> @brief Brief of Module SET_ASSOC...
!! @details Details of Module SET_ASSOC...
!! @author Lucci 
!! @date 18-03-20 \n
!<
!> @details <b>Use Module:</b>   
!! @arg @c F77KINDS
!! @arg @c NEBMAP
!! @details <b>Driver:</b> 
!! @arg @c INITTOP1
!! @arg @c INITTOP2
!<
!--------------------------------------------------------------------------------------------------
    MODULE SET_ASSOC
!--------------------------------------------------------------------------------------------------
! MODULE SET_ASSOC
! 
! USE MODULES: F77KINDS
!              NEBMAP
!
! DRIVER     : INITTOP1
!              INITTOP2
!--------------------------------------------------------------------------------------------------
    IMPLICIT NONE
!
    CONTAINS
!
    SUBROUTINE ASSOC
!
    USE F77KINDS
    USE NEBMAP
!
    MY_NEB(1)    = ITARG_N  
    MY_NEB(2)    = ITARG_E  
    MY_NEB(3)    = ITARG_S  
    MY_NEB(4)    = ITARG_W  
!
    MY_NEB(5)    = ITARG_NE  
    MY_NEB(6)    = ITARG_SE  
    MY_NEB(7)    = ITARG_SW  
    MY_NEB(8)    = ITARG_NW  
!
    LINV_NEB(1)  = LINV_N  
    LINV_NEB(2)  = LINV_E  
    LINV_NEB(3)  = LINV_S  
    LINV_NEB(4)  = LINV_W  
!
    LINV_NEB2(1) = LINV_N2
    LINV_NEB2(2) = LINV_E2
    LINV_NEB2(3) = LINV_S2
    LINV_NEB2(4) = LINV_W2
!
    ISGNU_NEB(1) = ISGNU_N  
    ISGNU_NEB(2) = ISGNU_E  
    ISGNU_NEB(3) = ISGNU_S  
    ISGNU_NEB(4) = ISGNU_W  
!
    ISGNU_NEB(5) = ISGNU_NE  
    ISGNU_NEB(6) = ISGNU_SE  
    ISGNU_NEB(7) = ISGNU_SW  
    ISGNU_NEB(8) = ISGNU_NW  
!
    ISGNV_NEB(1) = ISGNV_N  
    ISGNV_NEB(2) = ISGNV_E  
    ISGNV_NEB(3) = ISGNV_S  
    ISGNV_NEB(4) = ISGNV_W  
!
    ISGNV_NEB(5) = ISGNV_NE  
    ISGNV_NEB(6) = ISGNV_SE  
    ISGNV_NEB(7) = ISGNV_SW  
    ISGNV_NEB(8) = ISGNV_NW  
!      
    LEXC_NEB(1) = LEXC_N  
    LEXC_NEB(2) = LEXC_E  
    LEXC_NEB(3) = LEXC_S  
    LEXC_NEB(4) = LEXC_W  
!
    LEXC_NEB(5) = LEXC_NE  
    LEXC_NEB(6) = LEXC_SE  
    LEXC_NEB(7) = LEXC_SW  
    LEXC_NEB(8) = LEXC_NW  
!
    CROT_NEB(1) = CROT_N  
    CROT_NEB(2) = CROT_E  
    CROT_NEB(3) = CROT_S  
    CROT_NEB(4) = CROT_W  
!             
    CROT_NEB(5) = CROT_NE  
    CROT_NEB(6) = CROT_SE  
    CROT_NEB(7) = CROT_SW  
    CROT_NEB(8) = CROT_NW  
!
    LSWP_NEB(5) = LSWP_NE
    LSWP_NEB(6) = LSWP_SE
    LSWP_NEB(7) = LSWP_SW
    LSWP_NEB(8) = LSWP_NW
!
    END SUBROUTINE ASSOC
!
    END MODULE SET_ASSOC
