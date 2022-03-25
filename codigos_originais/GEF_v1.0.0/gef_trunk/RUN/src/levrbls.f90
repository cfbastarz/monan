PROGRAM le_vrbls
      
      REAL, DIMENSION (:,:) , ALLOCATABLE :: PD      
      ALLOCATE (PD(0:42, 0:42))       

      OPEN(1,FILE='/scratchout/grupos/grpeta/projetos/tempo/oper/gef_v1.0.0/initdata/vrbls01.0000',STATUS='old',FORM='unformatted')
      
          READ (1) PD    
          WRITE(15,*) PD 

      CLOSE(1)
      
      
END PROGRAM le_vrbls
      
      
      
