module modString
   !! Módulo com funções para tratar strings
   !!
   !! @note
   !!
   !! **Project**: fortran-utilities
   !! **Author(s)**: Rodrigues, L.F. [LFR]
   !! **e-mail**: <mailto:luiz.rodrigues@inpe.br>
   !! **Date**:  18Agosto2022 15:14
   !!
   !! **Full description**:
   !! Módulo com funções para tratar arquivos CSV
   !!
   !! @endnote
   !!
   !! @warning
   !!
   !!  [](https://www.gnu.org/graphics/gplv3-127x51.png'')
   !!
   !!     This program is free software: you can redistribute it and/or modify
   !!     it under the terms of the GNU General Public License as published by
   !!     the  Free  Software  Foundation, either version 3 of the License, or
   !!     (at your option) any later version.
   !!
   !!     This program is distributed in the hope that it  will be useful, but
   !!     WITHOUT  ANY  WARRANTY;  without  even  the   implied   warranty  of
   !!     MERCHANTABILITY or FITNESS FOR A  PARTICULAR PURPOSE.  See  the, GNU
   !!     GNU General Public License for more details.
   !!
   !!     You should have received a copy  of the GNU General  Public  License
   !!     along with this program.  If not, see <https://www.gnu.org/licenses/>.
   !!
   !! @endwarning

   use dump
   implicit none
   include 'constants.h'
   character(len=*), parameter :: sourceName = 'modString.f90' ! Nome do arquivo fonte
   character(len=*), parameter :: moduleName = 'modString' ! Nome do módulo

   private
   public :: parseLine, string2Real, lower, upper, string2Int

contains

   function parseLine(linha,separador) result(campos)
      !! faz um parse em uma linha fornecida separando os campos
      !!
      !! @note
      !!
      !! **Project**: fortran-utilities
      !! **Author(s)**: Rodrigues, L.F. [LFR]
      !! **e-mail**: <mailto:luiz.rodrigues@inpe.br>
      !! **Date**:  18Agosto2022 15:14
      !!
      !! **Full description**:
      !! faz um parse em uma linha fornecida separando os campos
      !!
      !! @endnote
      !!
      !! @warning
      !!
      !!  [](https://www.gnu.org/graphics/gplv3-127x51.png'')
      !!
      !!     Under the terms of the GNU General Public version 3
      !!
      !! @endwarning
   
      implicit none
      !Parameters:
      character(len=*), parameter :: procedureName = 'parseLine' ! Nome da função
      integer, parameter :: p_maxCampos = 32
      integer, parameter :: p_campSize = 256

      !Variables (input):
      character(len=*), intent(in) :: linha
      !! linha com os campos separados pelo separador
      character, intent(in),optional :: separador
      !! caracter que separa os campos

   
      !Local variables:
      character(len=p_campSize), allocatable :: campos(:)
      !! vetor com os campos retirados
      character :: sep
      integer :: nCampos
      character(len=256) :: resto
      integer :: iPos
      integer :: i
      integer :: sizeLin
      integer ::beg
   
      !Code:
      sep = ','
      if(present(separador)) sep = separador
      nCampos = size(campos)
      allocate(campos(nCampos))
      campos = ""
      resto = trim(linha)
      beg = 1
      do i = 1,nCampos
           sizeLin=len(resto)
           !print *,'resto Ini:',resto,sizelin,sep
           ipos = index(resto,sep)
           !print *,ipos
           campos(i)=resto(beg:ipos-1)
           !print *,'i,campos(i): ',i,campos(i)
           resto=trim(resto(ipos+1:sizeLin))
           !print *,'resto Fin:',resto,nCampos,iPos,len(trim(linha))
           if(i == nCampos .or. iPos==0) then
               campos(i)=resto
               exit
           endif
       enddo
   
   end function parseLine

   function string2Int(iInt) result(intVal)
      !! Retorna um inteiro a partir de um string
      !!
      !! @note
      !!
      !! **Project**: fortran-utilities
      !! **Author(s)**: Rodrigues, L.F. [LFR]
      !! **e-mail**: <mailto:luiz.rodrigues@inpe.br>
      !! **Date**:  19Agosto2022 13:12
      !!
      !! **Full description**:
      !! Retorna um inteiro a partir de um string
      !!
      !! @endnote
      !!
      !! @warning
      !!
      !!  [](https://www.gnu.org/graphics/gplv3-127x51.png'')
      !!
      !!     Under the terms of the GNU General Public version 3
      !!
      !! @endwarning
   
      implicit none
      !Parameters:
      character(len=*), parameter :: procedureName = 'string2Int' ! Nome da função
   
      !Variables (input):
      character(len=*), intent(in) :: iInt
      
   
      !Local variables:
      integer :: intVal
      !! Valor inteiro que retorna
   
      !Code:
      intVal = int(string2Real(iInt))
   
   end function string2Int

   function string2Real(strVal) result(realVal)
      !! Converte um número de string para real
      !!
      !! @note
      !!
      !! **Project**: fortran-utilities      
      !! **Author(s)**: Rodrigues, L.F. [LFR]
      !! **e-mail**: <mailto:luiz.rodrigues@inpe.br>
      !! **Date**:  18Agosto2022 16:29
      !!
      !! **Full description**:
      !! Converte um número de string para real.
      !!
      !! @endnote
      !!
      !! @warning
      !!
      !!  [](https://www.gnu.org/graphics/gplv3-127x51.png'')
      !!
      !!     Under the terms of the GNU General Public version 3
      !!
      !! @endwarning
   
      implicit none
      !Parameters:
      character(len=*), parameter :: procedureName = 'string2Real' ! Nome da função
   
      !Variables (input):
      character(len=*), intent(in) :: strVal
   
      !Local variables:
      real :: realVal
      !! real retornado
      character(len=256) :: numero(2)
      character(len=256) :: num
      character :: cnum
      integer :: nnum, factor, i, ipos
      real :: mult
      real :: frac,mantissa,expoente
      character(len=len(strVal)) :: lString
      logical :: hasExp
   
      !Code:
      hasExp = .false.

      lString = lower(strVal)
      if(index(lString,"e")/=0) hasExp = .true.

      if(hasExp) then
         !print *,'Num=',lString
         realVal = 0.0
         numero = parseLine(lString,"e")
         num = trim(adjustl(numero(1)))
         !print *,'n1: ',trim(num)
         mantissa = getRealVal(num)
         !print *,"man: ",mantissa
         num = trim(adjustl(numero(2)))
         !print *,'n2: ',trim(num)
         expoente = real(int(getRealVal(num)))
         !print *,'Exp: ',expoente
         if(expoente>0) then
            if(mantissa<0) then
               realval = -1*(abs(mantissa)*10**expoente)
            else 
               realval = abs(mantissa)*10**(expoente)
            endif
         else
            if(mantissa<0) then
               realval = -1*(abs(mantissa)*10**(-1*expoente))
            else
               realval = abs(mantissa)*10**(-1*expoente)
            endif
         endif
      else 
         realVal = getRealVal(strVal)
      endif

   end function string2Real

   function getRealVal(strVal) result(realVal)
      !! Pega o valor real de uma string
      !!
      !! @note
      !!
      !! **Project**: fortran-utilities
      !! **Author(s)**: Rodrigues, L.F. [LFR]
      !! **e-mail**: <mailto:luiz.rodrigues@inpe.br>
      !! **Date**:  19Agosto2022 12:28
      !!
      !! **Full description**:
      !! Pega o valor real de uma string
      !!
      !! @endnote
      !!
      !! @warning
      !!
      !!  [](https://www.gnu.org/graphics/gplv3-127x51.png'')
      !!
      !!     Under the terms of the GNU General Public version 3
      !!
      !! @endwarning
   
      implicit none
      !Parameters:
      character(len=*), parameter :: procedureName = 'getRealVal' ! Nome da função
   
      !Variables (input):
      character(len=*), intent(in) :: strVal
   
      !Local variables:
      real :: realVal
      !! real retornado
      character(len=256) :: numero(2)
      character(len=256) :: num
      character :: cnum
      integer :: nnum, factor, i, ipos
      real :: mult
      real :: frac
      character(len=len(strVal)) :: lString
   
      !Code:

      lString = lower(strVal)    
      numero = parseLine(lString,".")
      !print *,"Inteiro    : '",trim(numero(1))//"'"
      !print *,"Fracionario: '",trim(numero(2))//"'"
      
      realVal = 0.0
      num = trim(adjustl(numero(1)))
      mult = 1
      if(trim(numero(1))=='' .or. trim(numero(1))=='+') then
         factor = +1
      elseif(trim(numero(1))=='-') then
         factor = -1
      else
         do i=len_trim(num),1,-1
            cnum = num(i:i)
            if(i==1 .and. cnum=="-") then
               factor = -1
               cycle
            elseif(i==1 .and. cnum=="+") then
               factor = +1
               cycle
            else
               factor = +1
            endif
            if(iachar(cnum)<48 .or. iachar(cnum)>57) iErrNumber = dumpMessage(c_tty,c_yes,"","",c_fatal &
               ," invalid char: '"//cnum//"' in '"//trim(strVal)//"'")
            read(cnum,fmt="(I1)") nnum
            realVal = realVal + nnum*mult
            mult = mult*10
         end do
      endif

      !parte fracionária
      num = trim(adjustl(numero(2)))
      mult = 0.1   
      frac = 0.0
      do i=1,len_trim(num)
         cnum = num(i:i)
         if(iachar(cnum)<48 .or. iachar(cnum)>57) iErrNumber = dumpMessage(c_tty,c_yes,"","",c_fatal &
            ," invalid char: '"//cnum//"' in '"//trim(strVal)//"'")
         read(cnum,fmt="(I1)") nnum
         !print *,i,cnum,mult,nnum,nnum*mult
         frac = frac + nnum*mult
         !print *,i,cnum,mult,nnum,nnum*mult,frac
         mult = mult*0.1
      end do

      realVal = (realVal+frac)*factor
   
   end function getRealVal

   

   function lower(strIn) result(strOut)
      !# Convert case to lower case
      !#
      !# @note
      !# ![](http://brams.cptec.inpe.br/wp-content/uploads/2015/11/logo-brams.navigation.png "")
      !#
      !# **Brief**: Convert case to lower case
      !#
      !# **Documentation**: <http://brams.cptec.inpe.br/documentation/>
      !#
      !# **Author(s)**: Luiz Flavio Rodrigues **&#9993;**<luiz.rodrigues@inpe.br>
      !#
      !# **Date**: 26 August 2020 (Wednesday)
      !# @endnote
      !#
      !# @changes
      !# &#9744; <br/>
      !# @endchanges
      !# @bug
      !#
      !#@endbug
      !#
      !#@todo
      !#  &#9744; <br/>
      !# @endtodo
      !#
      !# @warning
      !# Now is under CC-GPL License, please see
      !# &copy; <https://creativecommons.org/licenses/GPL/2.0/legalcode.pt>
      !# @endwarning
      !#
      
      implicit none
      character(len=*),parameter :: procedureName='**lower**' !Name of this procedure
      !
      !Local Parameters
  
      !Input/Output variables
      character(*), intent(in) :: strIn
      !# String to be converted
      character(len=len(strIn)) :: strOut
      !# Return string converted
   
      !Local variables
      integer :: i
      !Code
      do i = 1, len(strIn)
          select case(strIn(i:i))
          case("A":"Z")
             strOut(i:i) = achar(iachar(strIn(i:i))+32)
          case default
             strOut(i:i) = strIn(i:i)
          end select
       end do
  
  end function lower

  function upper(strIn) result(strOut)
   !# Convert case to upper case
   !#
   !# @note
   !# ![](http://brams.cptec.inpe.br/wp-content/uploads/2015/11/logo-brams.navigation.png "")
   !#
   !# **Brief**: Convert case to upper case
   !#
   !# **Documentation**: <http://brams.cptec.inpe.br/documentation/>
   !#
   !# **Author(s)**: Luiz Flavio Rodrigues **&#9993;**<luiz.rodrigues@inpe.br>
   !#
   !# **Date**: 26 August 2020 (Wednesday)
   !# @endnote
   !#
   !# @changes
   !# &#9744; <br/>
   !# @endchanges
   !# @bug
   !#
   !#@endbug
   !#
   !#@todo
   !#  &#9744; <br/>
   !# @endtodo
   !#
   !# @warning
   !# Now is under CC-GPL License, please see
   !# &copy; <https://creativecommons.org/licenses/GPL/2.0/legalcode.pt>
   !# @endwarning
   !#
   
   implicit none
   character(len=*),parameter :: procedureName='**upper**' !Name of this procedure
   !
   !Local Parameters

   !Input/Output variables
   character(*), intent(in) :: strIn
   !# String to be converted
   character(len=len(strIn)) :: strOut
   !# Return string converted

   !Local variables
   integer :: i
   !Code
   do i = 1, len(strIn)
       select case(strIn(i:i))
       case("a":"z")
          strOut(i:i) = achar(iachar(strIn(i:i))-32)
       case default
         strOut(i:i) = strIn(i:i)
       end select
    end do

end function upper

end module modString