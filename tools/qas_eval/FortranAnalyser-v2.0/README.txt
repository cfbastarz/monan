This is the README from FortranAnalyser

Author: Michael García Rodríguez
Environmental Physics Laboratory & Escuela Superior de Ingeniería Informática
Universidade de Vigo, Ourense.

================================================================================ 
=                               DESCRIPTION                                    =
================================================================================
   This application analyse all files from a specific directory and search all
 FORTRAN files. For each files the program:

    - count the number of lines from the file.
        It is a question of verifying the magnitude of the file, since the 
        greater the number of lines has a program, the more complex it is.

    - count the number of variables declared.
        Each declared variable will be a buffer of the system memory so that
        the fewer variables are declared in the class, the better.

    - count the number of subroutines calls.
        The greater the number of calls to subroutines, the greater the
        complexity of the code. Therefore, the execution time and the number of
        resources to be used to execute it will be greater.

    - count the number of subroutines declared.
        Each declared subroutine method is a good practice in programming, 
        because it help to structure the code and improves code readability.

    - check the use of the sentence "implicit none". 
        There is a convention for the implicit declaration of integer and real 
        variables. It is advisable, however, to declare all the variables of a 
        program, making use of the declarations corresponding to the type that 
        each object with which it works. Therefore, to avoid ambiguity and 
        risks, Fortran 90 has a statement to override any specification of the 
        implicit type. When using this statement, any occurrence of a variable 
        whose type has not been explicitly declared will be flagged as an error 
        by the compiler.

    - check if the document have comments.
        A good practice of programation is comment all the program. The use of 
        comments constitutes good programming practice. Comments are very 
        important when developing the code since it makes it easier for 
        developers to understand the code. This improves the readability of the 
        code, in addition to it is structuring, allowing a quicker and easier 
        understanding of what the code is being doing. This program check if 
        there are comments: 
            * at the begining of the document
            * in each function declaration
            * in each variable
            * in each subroutine declaration

    - check the number of nested loops use.
        If the number of nested loops is gretter than 3, it is considere a bad 
        programming practice. the complexity of this type of loop is O(n)^3.

    - check the use of EXIT sentence.
        This sentence is used in loops to optimize it. In some cases, the 
        solution is finded before the end of the loop, so it is more efficient 
        exit in this moment of the loop and save resources to use them for 
        another work.

    - check the use of CYCLE sentence.
        This sentence is used in loops to optimize it. In some cases, you don't 
        need to check a statement, so it is iterated to the next element. With 
        they use, the code is more efficient and it save resources to use them 
        for another work.


================================================================================ 
=                              INSTRUCTIONS                                    =
================================================================================

Execute the program and you can visualice the follow buttons: 

    - "Analyse": this button first check if a directory is selected. After that, 
                 it execute the analysis of the selected directory. It produce a
                 report file saved in the directory: 
                                                
                                                "./tmp/QualityInformReport.pdf".

    - "Exit": This button close the application.

    - "...": This button open a file explorer to select the directory to be 
             analyzed.

    - "language": This menu button deploy the options to select the programe 
                  language. In addition, the quality report is generated in 
                  this language too. By default, the language selected is 
                  spanish. You can switch to English, Galician, French and 
                  Spanish.