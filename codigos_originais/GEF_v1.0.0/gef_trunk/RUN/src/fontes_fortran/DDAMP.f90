    SUBROUTINE DDAMP
!>-------------------------------------------------------------------------------------------------- 
!> SUBROUTINE DDAMP
!> 
!> SUBPROGRAM: DDAMP - DIVERGENCE DAMPING
!> PROGRAMMER: JANJIC
!> ORG: W/NP22
!> DATE: 94-03-08       
!>     
!> ABSTRACT:
!> DDAMP MODIFIES THE WIND COMPONENTS SO AS TO REDUCE THE HORIZONTAL DIVERGENCE.
!> A SWITCH PROVIDES THE OPTION OF ALSO MODIFYING THE TEMPERATURE FROM AN ENERGY VIEWPOINT.
!>     
!> PROGRAM HISTORY LOG:
!> 87-08-??  JANJIC     - ORIGINATOR
!> 95-03-25  BLACK      - CONVERSION FROM 1-D TO 2-D IN HORIZONTAL
!> 95-03-28  BLACK      - ADDED EXTERNAL EDGE
!> 98-10-30  BLACK      - MODIFIED FOR DISTRIBUTED MEMORY
!> 18-01-15  LUCCI      - MODERNIZATION OF THE CODE, INCLUDING:
!>                        * F77 TO F90/F95
!>                        * INDENTATION & UNIFORMIZATION CODE
!>                        * REPLACEMENT OF COMMONS BLOCK FOR MODULES
!>                        * DOCUMENTATION WITH DOXYGEN
!>                        * OPENMP FUNCTIONALITY 
!>
!> INPUT ARGUMENT LIST:
!> NONE
!>
!> OUTPUT ARGUMENT LIST:
!> NONE
!>
!> INPUT/OUTPUT ARGUMENT LIST:
!> NONE
!>    
!> USE MODULES: CONTIN
!>              CTLBLK
!>              DYNAM
!>              F77KINDS
!>              MASKS
!>              MPPSTAFF
!>              VRBLS
!> 
!> DRIVER     : GEF
!>
!> CALLS      : BOCOVMPI
!>-------------------------------------------------------------------------------------------------- 
    USE CONTIN
    USE CTLBLK
    USE DYNAM
    USE F77KINDS
    USE MASKS
    USE MPPSTAFF
    USE PARMETA , ONLY: IM, JM, LM
    USE VRBLS
!  
    IMPLICIT NONE
!
    INTEGER(KIND=I4)                                                                            ::&
    & I       , J       , L
!
    REAL   (KIND=R4)    , DIMENSION(0:IM+1, 0:JM+1)                                             ::&
    & DPDE    , RDPDX   , RDPDY
!
    REAL   (KIND=R4)                                                                            ::&
    & DDMPMX  ,VTM_DDMP  ! NOVO
!
    DDMPMX = 0.
!
    DO L=1,LM
!
        DO J=1,JM
            DO I=1,IM
                DPDE(I,J) = DETA(L) * PDSL(I,J)
            END DO
        END DO
!
        DO J=1,JM-1
            DO I=1,IM-1
                VTM_DDMP = VTM(I,J,L) * DDMP(I,J)
!
                RDPDX(I,J) = VTM_DDMP / (DPDE(I+1,J) + DPDE(I  ,J+1))  ! NOVO RDPDX
                RDPDY(I,J) = VTM_DDMP / (DPDE(I  ,J) + DPDE(I+1,J+1))  ! NOVO RDPDY
!
                U(I,J,L) = U(I,J,L) + (DIV(I+1,J  ,L) - DIV(I,J+1,L)) * RDPDX(I,J)  
                V(I,J,L) = V(I,J,L) + (DIV(I+1,J+1,L) - DIV(I,J  ,L)) * RDPDY(I,J)  
!
                IF (ABS((DIV(I+1,J,L) - DIV(I,J+1,L)) * RDPDX(I,J)) > DDMPMX) THEN
                    DDMPMX = ABS((DIV(I+1,J,L) - DIV(I,J+1,L)) * RDPDX(I,J))
                END IF
!
	
            END DO
        END DO
!
    END DO
!
    CALL BOCOVMPI(U,V,LM)
!
    END SUBROUTINE DDAMP
