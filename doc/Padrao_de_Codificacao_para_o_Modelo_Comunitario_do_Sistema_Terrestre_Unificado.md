# Padrão de Codificação para o Model for Ocean-laNd-Atmosphere predictioN (MONAN)

## Documento Técnico Normativo - DTN.01 - Rev. Beta 0.2

##### Autores: Luiz Flávio Rodrigues, ...

### 1. Introdução

É sabido que a qualidade do produto de software é um objetivo do processo de desenvolvimento.

A qualidade de um software é composta por 3 partes, qualidade interna, qualidade externa e qualidade em uso.  Esse documento trata de algumas qualidades internas e externas e de especificar normas de codificação para um dos componentes de um modelo de qualidade: o produto. De forma mais específica esse documento relaciona regras obrigatórias e desejáveis a serem aplicadas durante a codificação, isto é, a escrita do código.

Em todas as qualidades internas e externas a conformidade, capacidade do produto de software de estar de acordo com normas, convenções e leis é de fundamental importância para garantir a qualidade do que se produz. A maturidade de um software só pode ser garantida se uma série de requisitos são atendidos. Dentre os requisitos há um que garante o futuro do software e de um desenvolvimento contínuo em um contexto de modelo de desenvolvimento de software: a manutebilidade.

Segundo a norma ISO/IEC 9126, revisada pela norma ISO/IEC 25010:2011, a manutebilidade é a capacidade (ou facilidade) do produto de software ser modificado, incluindo tanto as melhorias ou extensões de funcionalidade quanto as correções de defeitos, falhas ou erros. Ou em outras palavras, a capacidade de um software comportar modificações, melhorias, correções, ou adaptações a novos requisitos. A manutenabilidade é dividida em cinco subcaracterísticas:

* **Analisabilidade**: É fácil de encontrar uma falha, quando ocorre?
* **Modificabilidade**: É fácil modificar e adaptar?
* **Modularidade**: Existe independência funcional entre os módulos do programa?
* **Testabilidade**: É fácil validar o software modificado?
* **Reusabilidade**: É fácil reutilizar o software em outras aplicações?

Analisar, modificar e reusar um software requer que se conheça seu funcionamento e que haja uma padronização clara, compreensível em sua escrita. Dessa forma atuar sobre o produto fica mais simples, mais rápido e evita a introdução de erros (bugs) que comprometam o funcionamento da solução. 

Segundo o site bring.com.br,

>  Os padrões de codificação são os procedimentos que definem uma linguagem de programação específica. Eles ajudam a **especificar o estilo, os métodos e os procedimentos**. (BRING, 2021)

E continua:

> Melhorar a qualidade do código traz uma série de vantagens principais. Garantir que a qualidade do código seja boa pode aumentar a eficiência do seu projeto e reduzir o risco de falha do seu projeto. Um código de boa qualidade também é simples, o que significa que os níveis de complexidade são reduzidos e fáceis de manter. No geral, tudo isso resulta em projetos e software mais bem-sucedidos e econômicos. (BRING, 2021)

Dessa forma o Model for Ocean laNd and Atmosphere predictioN (MONAN) deve ter seu código escrito dentro de um padrão comum, auditável e sua manutenção deve ser facilitada por uma codificação limpa e bem documentada. 

### 2. Refatoração

Todos os softwares importados ou suas partes, isto é, softwares não desenvolvidos pela equipe de desenvolvimento do MONAN e que serão acrescentados ao modelo, deverão passar por uma refatoração de código de forma a garantir que o padrão de codificação destes pacotes estejam em conformidade com as regras estabelecidas neste documento. Existem ferramentas e ambientes de desenvolvimento (IDEs) que auxiliam no trabalho de refatoração, por exemplo, Eclipse Photran ([Photran - An Integrated Development Environment and Refactoring Tool for Fortran | The Eclipse Foundation](https://www.eclipse.org/photran/)), e podem auxiliar sobremaneira a tarefa de refatoração.  Além disso é saudável que todos os desenvolvedores envolvidos utilizem ferramentas modernas de codificação e escrita, como o Sublime Text ([Download - Sublime Text](https://www.sublimetext.com/3)) ou Atom ([Download - Atom](https://atom.io/)) configuráveis e programáveis, que facilitem tanto a edição dentro das regras quanto a refatoração de código. Também é recomendável que sejam desenvolvidas pelos participantes do projeto, preferencialmente com parcerias internacionais, ferramentas que facilitem a refatoração de códigos.

### 3. Padrão de Texto Utilizados Nesse Documento

Esse documento usa dois tipos de indicadores de regras, as regras **mandatórias**, ou seja, regras obrigatórias que devem ser seguidas pelos desenvolvedores e o indicador de regras **recomendadas**, isto é, regras que podem ser rejeitadas em casos especiais onde as mesmas não podem ser cumpridas pelos desenvolvedores. No texto as regras mandatórias são mostradas pela palavra <mark>mandatória</mark> em modo highlight e as regras <u>recomendada</u>s pela palavra sublinhada. 

Por exemplo,

<mark>mandatória</mark>: *As constantes físicas devem começar com um c\_. Exemplo: `c_stefan`.*

<u>recomendada</u>: *Mesmos as variáveis locais a um módulo devem, sempre que possível, ter sua funcionalidade descrita na declaração.*

Todos os trecho de código exemplo mostrados nesse documento serão mostrados em modo "code fences" conforme o exemplo abaixo:

```fortran
logical function checkAllInputs(inVal)

    real, intent(in) :: inVal

    return .true.

end function checkAllInputs 
```

### 4. Regras Mandatórias de Codificação Padrão (Coding Standards)

4.1 <mark>mandatória</mark>: *As palavras reservadas da linguagem devem ser todas declaradas com letras minúsculas.*

São palavras reservadas (104):

```fortran
abstract, allocatable, allocate, assign, associate, asynchronous, backspace,
bind, block, block data, call, case, class, close, codimension, common, concurrent,
contains, contiguous, continue, critical, cycle, data, deallocate, deferred,
dimension, do, elemental, else, elseif, elsewhere, end, endfile, endif, entry, enum,
enumerator, equivalence, error, exit, extends, external, final, flush, forall,
format, function, generic, goto, if, implicit, import, include, inquire, intent,
interface, intrinsic, lock, memory, module, namelist, non_overridable, nopass,
nullify, only, open, operator, optional, parameter, pass, pause, pointer, print,
private, procedure, program, protected, public, pure, read, recursive, result,
return, rewind, rewrite, save, select, sequence, stop, submodule, subroutine,
sync, sync all, sync images, target, then, unlock, use, value, volatile, wait,
where, while, write
```

4.2 <mark>mandatória</mark>: *Obrigatória a declaração de `implicit none` no início de programas, módulos ou na saída de uma unit. Ajuda ao programador na captura de erros como, por exemplo, saber se `l` (em destaque no exemplo a seguir) representa o número `1` ou uma variável.*

```fortran
implicit none
integer :: i

Integer :: l

real, dimension(:) :: x(10)

do i = 2, 10
 x(i) = 2.0 * x(i-l)
enddo
```

4.3 <mark>mandatória</mark>: *Nomes de variáveis compostos com mais de uma palavra devem usar o estilo camel case, isto é, a primeira palavra em minúscula e as demais iniciando com maiúsculas. Exemplo:*

```fortran
integer :: countParticles
!! Number of particles count [#]
real, allocatable :: aerMassCape(:,:,:)
!! Mass of aerosol on cape waves [g/m^3]
```

4.4 <mark>Mandatória</mark>: *O atributo `intent` deve ser usado para todos argumentos com exceção de ponteiros (não é definido pelo padrão). Deixa clara a intenção do argumento dentro da subrotina.*

```fortran
subroutine test(a, b)
 real, intent( in) :: a
 real, intent(out) :: b

 a = b

end subroutine test
```

4.5 <mark>Mandatória</mark>: *Constantes físicas precisam ser definidas com o atributo `parameter`. Para declarações, utilize sempre uma constante por linha para facilitar a documentação e entendimento.*

<mark>Mandatória</mark>:Iniciar com uma letra `c` seguida de um subscrito `c_`.

<mark>Mandatória</mark>: Sempre coloque um comentário explicando a constante e sua unidade.

```fortran
real, parameter      :: c_kb = 1.3806504e-23 
!! boltzmann constant [jk-1]
```

4.6 <mark>Mandatória</mark>: *Todas as constantes não físicas (`parameters`), precisam ser definidas com o atributo `parameter`. Para declarações, utilize sempre uma constante por linha para facilitar a documentação e entendimento.*

<mark>Mandatória</mark>:Iniciar com uma letra `p` seguida de um subscrito `p_`.

<mark>Mandatória</mark>: Sempre coloque um comentário explicando o parâmetro.

```fortran
   !Errors type for dump functions
   integer, parameter :: p_noError = 0
   !! No Error, just write
   integer, parameter :: p_notice = 1
   !! Notice Error
   integer, parameter :: p_warning = 2
   !! warning Error
   integer, parameter :: p_fatal = 3
   !! Fatal error
   integer, parameter :: p_kill = 4
   !! Kill the model Signal
   integer, parameter :: p_continue = 5
   !! Continue run Signal
   logical, parameter :: p_yes = .true.
   !! Yes is the .true. value
   logical, parameter :: p_no = .false.
   !! No is the false value
```

4.7 <mark>Mandatória</mark>: *Todas as constantes físicas e não físicas comuns a todos os códigos e subrotinas devem estar definidas em um arquivo `include` comum com o nome `constants.h` e estar disponíveis um um subdiretório chamado `include` dentro do subdiretório `src` (sources).*

4.8 <mark>Mandatória</mark>: *Deve-se introduzir em cada procedure duas constantes que serão necessárias para manutenção de código e arquivos de dump. São elas: `sourceName` e `procedureName`. A constante `sourceName` deve conter o nome do arquivo `.F90` em que ela está contida e a constante `procedureName` deve conter o nome da procedure (`function`, `subroutine`, `module`, etc).*

```fortran
integer function writeAHallo()
    implicit none

    character(len=*), parameter :: sourceName = 'utils.F90'
    character(len=*), parameter :: procedureName = 'writeAHello'

    write(*,*) 'Hello'

end function writeAHello
```

Essas constantes serão usadas em funções de manutenção de código e verificação de bugs sempre que necessárias.

4.9 <mark>Mandatória</mark>: *IDENTAÇÃO: Como regra geral para construção dos fontes, definiu-se a indentação com 3 espaços. **Não usar tabulação para esse fim**, pois cada editor pode abrir a indentação de forma errada. Definir uma indentação deixa o código mais claro.*

4.9 <mark>Mandatória</mark>: *O número máximo de comandos por linha de código é 1. Não se deve usar ponto-e-vírgula e separar mais de um comando por linha.*

4.10 <mark>Mandatória</mark>: *Uma linha não pode ter mais de 80 caracteres de tamanho no total e deve se usar a continuação de linhas para os casos que excedem esse tamanho.*

4.11 <mark>Mandatória</mark>:*Para casos em que o conteúdo ao fim da linha é um string deve-se dividi-lo usando a concatenação de strings (`//`) e usar a continuação de linha (`&`).*

4.12 <mark>Mandatória</mark>: *Continuidade em quebra de linha: Em fórmulas matemáticas, colocar o carácter de continuação `&` logo antes de um operador de matemática. Colocar o operador alinhado com o símbolo de `=`.*

```fortran
formula = (fibon(i,j) + c_pi*angulo) &
        + delta
```

4.13 <mark>Mandatória</mark>: *Continuidade em quebra de linha: Caso uma linha contenha um string que precise continuar na linha seguinte deve-se usar a concatenação de string como ferramenta de continuação. Fecha-se os delimitadores de string e coloca-se o `&`. Na linha seguinte alinhado com o início do string coloca-se o carácter de concatenação `//` e termina-se de escrever o string. Colocar o operador alinhado com o símbolo de `=`.*

```fortran
character(len=*), parameter :: nomeArquivo = ’diretorio/files/dados/input/’ &
                                         //’arquivoEntrada’
```

4.14 <mark>Mandatória</mark>: Havendo necessidade de quebrar linhas de argumentos de funções/subrotinas, colocar o carácter de continuidade de lina `&` logo antes de uma vírgula de separação de dado (`,`) e na linha seguinte começar com uma `,` alinhada com o primeiro carácter do nome do procedure. 

```fortran
subroutine executaAcao(param1, param2 &
 ,param3, param4)
```

 4.15 <mark>Mandatória</mark>: Desvios condicionais com blocos `if`/`end if` e `do`/`end` do devem usar o `end` colapsado, isto é, usar `endif` e `enddo` ao invés de `end if` e `end do`.

```fortran
outer: do iCount1 = 1,10  
   do iCount2 = 1,10  
      if ( iCount1 == 2  .and.  iCount2 == 3 ) cycle outer  
      z(iCount2, iCoutn1) = 1.0d0
      if(iCount1>7) then
         Print *,’Just for test: ‘,iCount2
         call adjustCount1Value(Icount1)
      endif  
   enddo  
enddo outer
```

 4.16 <mark>Mandatória</mark>:  *Variáveis de entrada e saída devem estar declaradas com `implicit` adequado (`in`, `inout` ou `out`) e devem usar explicit shape.* 

4.17 <mark>Mandatória</mark>:*Toda procedure deve estar sob declaração de `implicit none`.* 

4.18 <mark>Mandatória</mark>: *Para o caso dos módulos deve-se sempre considerar que todo o módulo é `private`, exceto as variáveis e procedures explicitamente declaradas como `public`.*

4.19 <mark>Mandatória</mark>: *é proibido que se inicialize uma variável na sua declaração.*

4.20 <mark>Mandatória</mark>: *Todos as parametrizações (códigos específicos) devem estar “encapsuladas” em um mesmo módulo (`module`). Os módulos devem conter suas próprias variáveis de memória no escopo do módulo. As variáveis e procedures devem ser declaradas levando-se em consideração seu escopo no nível do programa todo*. *Devendo ser:*

* *Pública (`public`): Pode ser visível em todo o código;*
* *Privada (`private`): Somente visível dentro do módulo.*

4.21 <mark>Mandatória</mark>: *O nome dos módulos deve começar com as letras `mod`. Caso exista apenas um módulo em um arquivo o nome do arquivo fonte deve ser o mesmo do módulo. Exemplo: `modRadiate.F90`. O restante do nome do módulo deve representar um nome que represente sua função principal.*

```fortran
module modRadiate

end module modRadiate
```

4.22 <mark>Mandatória</mark>: *Os módulos devem conter suas próprias funções de inicialização se necessárias e especialmente aquelas para zerar variáveis. Considerar sempre que a criação de uma variável não a inicia para valor algum. Essa função deve ter o nome começando com as letras `init`. Exemplo:`initModRadiat` e retornar o valor `0` quando a inicialização obtiver sucesso e outro valor indicando algum erro.*

 4.23 <mark>Mandatória</mark>: *Os módulos não podem fazer uso (`use`) de outros módulos externos a ele, exceto nos casos de includes globais como o de constantes.*

4.24 <mark>Mandatória</mark>: *Todos os erros que retornam de funções devem ser listados dentro do include `contants.h` e devem receber seus valores indicados bem como sua descrição deve estar definida. Os erros são parâmetros e devem começar com `e_`.*

```fortran
   !Errors numbers
   integer, parameter :: e_noError = 0
   !! No Error, just write
   integer, parameter :: e_notice = 1
   !! Notice Error
   integer, parameter :: e_warning = 2
   !! warning Error
   integer, parameter :: e_fatal = 3
   !! Fatal error
```

 4.25 <mark>Mandatória</mark>: *Módulos devem ser interfaciados com drivers. Os drivers servem para fazer conversões numéricas, de tipos de variáveis e adaptação entre os procedures que chamam o módulo e o próprio módulo chamado. O drive deve conter o nome que contenha o nome do módulo seguido da palavra `Driver`. Por exemplo, se um driver for criado para o módulo `modRadiate` este deverá se chamar `modRadiateDriver`. Os drivers podem conter `uses` de outros módulos.*

4.26 <mark>Mandatória</mark>: *Todos os `uses` dos drivers (se existirem) devem conter a cláusula `only` e conter a lista de variáveis e procedures a que ele se referencia.*

4.27 <mark>Mandatória</mark>: *Os drivers criados devem chamar os procedures do módulo sempre por parâmetros e nunca por memória (`use`, etc)*. *Os módulos não podem fazer uso de variáveis ou procedures com `use`, exceto para as funções de dump (debug e mensagens).*

4.28 <mark>Mandatória</mark>: *todas as variáveis, contantes e parameters devem receber uma documentação pelo padrão de documentação, Ford - Fortran Documentator, ([GitHub - Fortran-FOSS-Programmers/ford: Automatically generates FORtran Documentation from comments within the code.](https://github.com/Fortran-FOSS-Programmers/ford)) que é adotado pelo modelo. Essa documentação está descrita logo abaixo da declaração da variável com os caracteres `!!` antes da descrição. Essa documentação deve ser o mais abrangente possível e, caso exista, deve conter também a unidade e referência sobre a variável.*

4.29 <mark>Mandatória</mark>: *O controle de número de unidade para abertura de arquivo deve ser feito por função automática que deve retornar a unidade livre. Da mesma forma a unidade deve ser liberada por uma função específica que fecha o arquivo e libera a unidade.*

4.30 <mark>Mandatória</mark>: *Precedendo a abertura de arquivos uma função deve ser usada para verificar a existência de diretório (em caso de escrita e leitura) e a existência do arquivo (em caso de leitura) e informar erro - ou aviso - em caso de não existência dos mesmos. Essa função também deve receber os parâmetros `sourceName` e `procedureName` para que se identifique qual arquivo e rotina fez a tentativa de operação.*

4.31 <mark>Mandatória</mark>: *O início de procedures (`function`, `subroutine`, `module`, `program`) deve receber uma linha em branco, seguido de uma linha com comentário (`!`) seguido de 60 simbolos `=`, seguido da declaração do nome do procedure e seus parâmetros de entrada e saída e logo depois receber a documentação padrão de cabeçalho do Ford.*

```fortran
!============================================================
integer function getUnit()
   !! get a free unit to use
   !!
   !! @note
   !!
   !! **Project**: MONAN - Model for Ocean laNd and Atmosphere predictioN
   !! **Author(s)**: Rodrigues, L.F. [LFR]
   !! **e-mail**: <mailto:luiz.rodrigues@inpe.br>
   !! **Date**:  28Março2022 15:50
   !!
   !! **Full description**:
   !! This function returns a free unit number to manipulate file 
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
```

4.32 <mark>Mandatória</mark>: *Todos os arquivos de código em fortran terão sua extensão `.F90` e não `.f90`*

4.33 <mark>Mandatória</mark>: *Todos os arquivos de código, em qualquer linguagem, deverá conter um cabeçalho à partir de sua primeira linha, informando a referência ao **Model for Ocean laNd and Atmosphere predictioN (MONAN)**. A licença a ser adotada é a Free  Software  Foundation, GPL version 3, estando seus termos disponiveis em [https://www.gnu.org/licenses/](https://www.gnu.org/licenses/). Nos módulos e programas (program) a licença deverá ser mostrada por extenso:

```fortran
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
```

Nas demais procedures (functions ou subroutines), desde que internas aos módulos, pode-se apresentar a licença em formato resumido:

```fortran
   !! @warning
   !!
   !!  [](https://www.gnu.org/graphics/gplv3-127x51.png'')
   !!
   !!     Under the terms of the GNU General Public version 3
   !!
   !! @endwarning
```

4.34 <mark>Mandatória</mark>: *É proibido o uso de comandos descontinuados para fortran 90/95 como os listados abaixo:*

* `pause`: *Proibido devido a máquinas que usam submissão de jobs não o executarem sendo fonte de problemas de portabilidade*;
* `equivalence`: *Proibido por ser fonte de invasão de memória;*
* `common`: *Deve ser substituído por módulos (memória). Seu uso como o equivalence é fonte de invasão de memória;*
* `save`: *Deve ser substituído por variáveis em módulos. Seu uso torna o código não thread safe e não puro impedindo os recursos de OpenMP;*
* `data`: *Não deve ser usado. As variáveis devem ser inicializadas por subrotinas de inicialização em cada módulo. Em casos específicos de constantes usar `parameter`.*

4.35 <mark>Mandatória</mark>:*Todos os documentos relativos ao software devem ser editados em linguagem Markdown ([Getting Started | Markdown Guide](https://www.markdownguide.org/getting-started/)) para facilitar sua portabilidade para outros sistemas operaionais e máquinas.*

4.36 <mark>Mandatória</mark>: *Todos os comandos `stop` com mensagem, devem ser escritos com um espaço entre o comando e a mensagem. Exemplo: `stop "Mensagem."`.*

4.37 <mark>Mandatória</mark>: Ao alocar memória para uma variável, verificar se esta já não foi previamente alocada:

```fortran
if (.not. allocated(x)) allocate(x(i))
```

obs: quando a alocação for a um ponteiro deve-se verificar se o mesmo já se encontra associado:

```fortran
if (.not. associated(x)) allocate(x(i))
```

4.38 <mark>Mandatória</mark>: Ao desalocar memória de uma variável, verificar se está mesmo alocada:

```fortran
if (allocated(x)) deallocate(x)
```

4.39 <mark>Mandatória</mark>: Nas atribuições de valores a variáveis e constantes deve-se observar um espaço antes e depois do sinal de igual:

```fortran
real :: distance

distance = 500
```

4.40 <mark>Mandatória</mark>: nas chamadas de subrotinas ou funções os parâmetros devem ser passados separados por vírgula e um espaço em branco:

```fortran
    call doSomeCalc(x, y, 35.0)
```

4.41 <mark>Mandatória</mark>: **Nunca** deixar espaço entre o nome da função ou subrotina e a abertura do parenteses que o segue, exemplo de um erro abaixo:

```fortran
    call doSomeCalc (x, y, 35.0)
```

### 5. Regras recomendadas de codificação padrão (Coding Standards)

 5.1 **Recomendada**: *Os códigos em FORTRAN devem ser compatíveis com o padrão FORTRAN 2008 ou superior [https://www.iso.org/standard/50459.html](https://www.iso.org/standard/50459.html). Códigos legados (padrão FORTRAN 77, 90, 95 e anteriores) devem ser migrados ou trocados por novas implementações.*

5.2 **Recomendada** : *Para qualquer programa, subrotina, função, módulo, etc deve-se evitar nomes simples ou de apenas uma letra para variáveis como `T`, `P`, `U`. Usar nomes como `temp`, `press` e `uWind` por exemplo.*

5.3 **Recomendada**: *Para variáveis auxiliares e locais aos procedures utilize nomes que ajudem a identificá-las numa busca no código. Se possível identifique-as com a documentação Ford. Para contadores de laços use o sufixo `count` para identificá-los: Exemplo: `iCount`, `jCount`, etc.*

5.4 **Recomendada**: *Com arrays automáticos, potencialmente grandes, procure utilizar arrays alocáveis. Deste modo, caso não haja espaço suficiente durante a alocação do array na pilha, o programa não irá falhar.*

5.5 **Recomendada**: *Ao utilizar arrays alocáveis prefira dealocá-los explicitamente quando não forem mais necessários.*

5.6 **Recomendada**: *É altamente recomendada a utilização de arrays alocáveis em vez de ponteiros. Arrays alocáveis são agora permitidos como componentes de estruturas nas extensões de 2003 e 95 do Fortran. Deste modo, a utilização de ponteiros não se faz tão necessária, uma vez que são demasiadamente lentos e seus argumentos dummy são impraticáveis. Estes argumentos não são nada práticos, visto que os argumentos reais necessitam sempre ter o ponteiro ou o atributo de destino (ponteiros em C são o oposto).*

5.7 **Recomendada**: *Utilize sempre labels para identificar laços grandes de forma a permitir busca pelo fim do mesmo e verificar partes de código internas aos laços.*

5.8 **Recomendada**: *Sempre que possível prefira usar a construção `select case` ao invés de fluxos `if`/`then`/`elseif`/`endif`.*

5.9 **Recomendada**: *As funções, programas, subrotinas, módulos ou qualquer procedure devem ter nomes significativos que facilitem entender suas funcionalidades. Exemplo: ao invés de `function tu()` use `function toUpper()`.*

5.10 **Recomendada**: *O uso de linhas em branco no código são recomendadas. Mas seu uso excessivo pode tornar o código com aspecto visual ruim e de difícil compreensão. Por isso recomenda-se:*

*a) Duas linhas em branco:*

* *Entre seções importantes do código;*
* *Entre definições de dados e subrotinas dentro de um módulo.*

*b) Uma linha em branco:*

* *Entre funções / subrotinas;*
* *Dentro das funções / subrotinas, entre o escopo de variáveis e os blocos de código;*
* *Antes ou depois de linhas simples de comentário;*
* *Entre blocos lógicos dentro das funções / subrotinas.*

5.11 **Recomendada**: *Recomenda-se **fortemente** que não se use o comando `goto` ou `continue` sempre que possível substituindo-os por estruturas que usem condicionais e os comandos `cycle` e `exit`.*

5.12 **Recomendada**: *Sempre que possível substitua laços de teste `if`/`then`/`else` pelo uso da instrução `where`.*

```fortran
where(chemSpeciesConcentration(:,:,iSpc,iSrc) <0.) chemSpeciesConcentration(:,:,iSpc,iSrc)=0.
```

5.13 **Recomendada**: *Sempre que possível os arquivos de entrada e saída devem estar em formato **NetCDF** [(https://www.unidata.ucar.edu/software/netcdf/](https://www.unidata.ucar.edu/software/netcdf/). Um documento deve ser criado para referenciar cada uma das variáveis de cada um desses arquivos ao seus significado, sua unidade e outras informações necessárias. Quando os arquivos de entrada estiverem em formato distinto esses devem ser convertidos para NetCDF.*

5.14 **Recomendada**: *O software deve sempre reutilizar funções e procedures já existentes e sempre que possível essas funções/procedures devem ser unidades atômicas reaproveitáveis e testáveis.*  

5.15 **Recomendada**: *A passagem de parâmetros nas chamada de funções e subrotinas deverá ser referenciada pelo nome da variável interna na função ou subrotina. Usar a chamada dessa forma permite o uso da diretiva `optional` em qualquer ponto da chamada e é muito útil (veja o exemplo abaixo para a função **calorSensivel**).*   

5.16 **Recomendada**: *O retorno da função sempre deve ser indicado pela diretiva `result` pois permite trabalhar com retornos mais complexos como arrays, etc.*

```fortran
module tst
   implicit none
   private

   public bascara,calorSensivel

contains

   !================================================
   function bascara(aCoef, bCoef, cCoef) result(raiz)
      implicit none

      real, intent(in) :: aCoef
      !! Coef. a da equacao de seg. grau
      real, intent(in) :: bCoef
      !! Coef. b da equacao de seg. grau
      real, intent(in) :: cCoef
      !! Coef. c da equacao de seg. grau

      real ::raiz(2)
      !! raizes da equacao de segundo grau

      real :: delta

      delta = bCoef*bCoef-4*aCoef*cCoef
      raiz(1) = (-bCoef+sqrt(delta))/2*aCoef
      raiz(2) = (-bCoef-sqrt(delta))/2*aCoef

   end function bascara

   !================================================
   function calorSensivel(massa,calorEspecifico,deltaTemp) result (cs)
      implicit none

      real, intent(in) :: massa
      !! massa da substância [g]
      real,optional,intent(in) :: calorEspecifico
      !! calor específico da substancia [cal/gC]
      real, intent(in) :: deltaTemp
      !Temperatura de elevacao [C]

      real :: cs
      !! calor sensivel calculado

      real :: ce

      !Se não fornecido o valor do calor especifico usar o da agua
      if(present(calorEspecifico)) then
         ce = calorEspecifico
      else
         ce = 1.0
      endif

      cs = massa*ce*deltaTemp

   end function calorSensivel

end module tst

program teste
   use tst
   implicit none

   real, parameter :: a = 2.0
   real, parameter :: b = 4.0
   real, parameter :: c = 1.0
   real :: raiz(2)

   raiz = bascara(aCoef = a, bCoef = b, cCoef = c)

   write(*,*) calorSensivel(massa = 4000.0, deltaTemp = 100.0)
   write (*,*) raiz

end program teste
```

5.17 **Recomendada**: sempre que possível use tipos para agrupar variáveis, vetores ou arrays e facilitar a chamada de subrotinas e funções.

```fortran
module modEnergy
...
type en
    real, pointer :: calorSensivel(:,:,:)
    real, pointer :: calorLatente(:,:,:)
    integer :: xDimension
    integer :: yDimension
    integer :: zDimension
end type en
type(en) :: atmosData
...
end module modEnergy
...
call calcHeat(atmosData)

...
subroutine calcHet(atmosData)
    ...
    use modEnergy, only: &
        en

    type(en) :: atmosData

   ...
```

## Referências

* [Padrões de Codificação](bring.com.br);
* [ISO EIC 9126](https://pt.wikipedia.org/wiki/ISO/IEC_9126);
* [ISO EIC 25010:2011](https://pt.wikipedia.org/wiki/ISO/IEC_25010).
