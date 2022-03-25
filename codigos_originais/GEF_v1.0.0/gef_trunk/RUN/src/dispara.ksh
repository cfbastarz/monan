#!/bin/ksh


typeset -Z4 proc

proc=0
while (( ${proc} < 600 )) ; do


echo $proc


cat <<EOF> le_vrbls.f90
PROGRAM le_vrbls
      
      REAL, DIMENSION (:,:) , ALLOCATABLE :: PD, T     
      ALLOCATE (PD(0:42, 0:42))       
      ALLOCATE (T (0:42, 0:42))


      OPEN(1,FILE='/scratchout/grupos/grpeta/projetos/tempo/oper/gef_v1.0.0/initdata/vrbls01.${proc}',STATUS='old',FORM='unformatted')
      
          READ (1) PD    
          READ (1) T
          WRITE(15,*) T 

      CLOSE(1)
      
      
END PROGRAM le_vrbls
EOF

chmod 755 le_vrbls.f90
pgf90 -o le_vrbls.exe le_vrbls.f90
./le_vrbls.exe


cat fort.15 >> saida/T

rm -f le_vrbls.exe le_vrbls.f90


let proc=${proc}+1
done

exit

