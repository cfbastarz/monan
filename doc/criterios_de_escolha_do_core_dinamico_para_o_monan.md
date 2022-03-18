# Análise do Core Dinâmico base para o Model for Ocean-laNd-Atmosphere predictioN (MONAN)

## Documento Técnico Normativo - DTN.01 - Rev. Beta 0.1

##### Autores: Denis M. A. Eiras, ...

### 1. Introdução

A escolha do Core Dinâmico do MONAN deve ser feita levando em consideração alguns aspectos de qualidade de software. Segundo a norma [ISO/IEC 9126](https://pt.wikipedia.org/wiki/ISO/IEC_9126), revisada pela norma [ISO/IEC 25010:2011](https://pt.wikipedia.org/wiki/ISO/IEC_25010), quando a qualidade de software se refere ao produto, estas fornecem uma estrutura para especificar características de qualidade de software e realizar comparações entre produtos de software. Outras características e métricas de qualidade de software, encontrados em livros de engenharia de software, também foram introduzidas. Estas normas, características e métricas serão utilizadas como base para a construção dos critérios de escolha do core Dinâmico, descritos neste documento.

Dentro do contexto de qualidade interna, onde o software é avalidado para ser reutilizado e modificado, a definição das características e subcaracterísticas devem ser levantadas em função da área de aplicação do produto de software. E, esta definição deve ser feita antes do início do desenvolvimento do mesmo. Produtos de maior porte devem ser subdivididos em módulos e cada um destes deve ter seus próprios conjuntos de características e subcaracterísticas. Portanto, um Core Dinâmico de qualidade deve ser utilizado considerando alguns aspectos principais da qualidade de software que são inerentes ao Produto.


### 2. Caracterísiticas de Qualidade de Software

As caracterísitcas de qualidade selecionadas para a avaliação do Core Dinâmico estão listadas abaixo. Em todas as características listadas, temos uma subcaracterística com o nome de Conformidade. A conformidade é utilizada para avaliar o quanto o software obedece aos requisitos de legislação e todo o tipo de padronização ou normalização aplicável ao contexto.


**Funcionalidade**: A capacidade de um software prover funcionalidades que satisfaçam o usuário em suas necessidades declaradas e implícitas, dentro de um determinado contexto de uso. Suas subcaracterísticas são:

* Adequação: Capacidade do produto de software de prover um conjunto apropriado de funções para tarefas e objetivos do usuário especificados.
* Acurácia: Capacidade do produto de software de prover, com o grau de precisão necessário, resultados ou efeitos corretos ou conforme acordados.
* Interoperabilidade: Capacidade do produto de software de interagir com um ou mais sistemas especificados.
* Segurança: Capacidade do produto de software de proteger informações e dados, de forma que pessoas ou sistemas não autorizados não possam lê-los nem modificá-los e que não seja negado o acesso às pessoas ou sistemas autorizados.
* Conformidade.


**Confiabilidade**: A capacidade do produto de software de manter um nível de desempenho especificado, quando usado em condições especificadas. Suas subcaracterísticas são:

* Maturidade: Capacidade do produto de software de evitar falhas decorrentes de defeitos no software.
* Tolerância a Falhas: Capacidade do produto de software de manter um nível de desempenho especificado em casos de defeitos no software ou de violação de sua interface especificada.
* Recuperabilidade: Capacidade do produto de software de restabelecer seu nível de desempenho especificado e recuperar os dados diretamente afetados no caso de uma falha.
* Conformidade.


**Usabilidade**: A capacidade do produto de software de ser compreendido, aprendido, operado e atraente ao usuário, quando usado sob condições especificadas. Note que este conceito é bastante abrangente e se aplica mesmo a programas que não possuem uma interface para o usuário final. Por exemplo, um programa batch executado por uma ferramenta de programação de processos também pode ser avaliado quanto a sua usabilidade, no que diz respeito a ser facilmente compreendido, aprendido, etc. Suas subcaracterísticas são:
* Inteligibilidade: Capacidade do produto de software de possibilitar ao usuário compreender se o software é apropriado e como ele pode ser usado para tarefas e condições de uso específicas.
* Apreensibilidade: Capacidade do produto de software de possibilitar ao usuário aprender sua aplicação.
* Operacionalidade: Capacidade do produto de software de possibilitar ao usuário operá-lo e controlá-lo.
* Proteção frente a erros de usuários: como produto consegue prevenir erros dos usuários;
* Conformidade


**Eficiência**: O tempo de execução e os recursos envolvidos são compatíveis com o nível de desempenho do software. Suas subcaracterísticas são:

* Comportamento em Relação ao Tempo: avalia se os tempos de resposta (ou de processamento) estão dentro das especificações;
* Utilização de Recursos: mede tanto os recursos consumidos quanto a capacidade do sistema em utilizar os recursos disponíveis; exemplo: processador e memória.
* Conformidade


**Manutenibilidade**: A capacidade (ou facilidade) do produto de software ser modificado, incluindo tanto as melhorias ou extensões de funcionalidade quanto as correções de defeitos, falhas ou erros. Suas subcaracterísticas são:

* Analisabilidade: identifica a facilidade em se diagnosticar eventuais problemas e identificar as causas das deficiências ou falhas.
* Modificabilidade: caracteriza a facilidade com que o comportamento do software pode ser modificado.
* Reusabilidade: identifica a capacidade de se reutilizar o software por completo ou partes dele.
* Estabilidade: avalia a capacidade do software de evitar efeitos colaterais decorrentes de modificações introduzidas.
* Testabilidade: representa a capacidade de se testar o sistema modificado, tanto quanto as novas funcionalidades quanto as não afetadas diretamente pela modificação.
* Conformidade


**Portabilidade**: A capacidade do sistema ser transferido de um ambiente para outro. Como "ambiente", devemos considerar todos os fatores de adaptação, tais como diferentes condições de infraestrutura (sistemas operacionais, versões de bancos de dados, etc.), diferentes tipos e recursos de hardware (tal como aproveitar um número maior de processadores ou memória). Além destes, fatores como idioma ou a facilidade para se criar ambientes de testes devem ser considerados como características de portabilidade. Suas subcaracterísticas são:

* Adaptabilidade: representando a capacidade do software se adaptar a diferentes ambientes sem a necessidade de ações adicionais  configurações).
* Capacidade para ser Instalado: identifica a facilidade com que pode se instalar o sistema em um novo ambiente.
* Coexistência: mede o quão facilmente um software convive com outros instalados no mesmo ambiente.
* Capacidade para Substituir: representa a capacidade que o sistema tem de substituir outro sistema especificado, em um contexto de uso e ambiente específicos. Este atributo interage tanto com adaptabilidade quanto com a capacidade para ser instalado.
* Conformidade.

### 3. Métricas para avaliação de Características de Qualidade relacionadas à Manutebilidade e parte da Portabilidade

Métricas de Software podem ser utilizadas para a avaliação de algumas Características de Qualidade. Exitem métricas para avaliação de código estruturado e de código Orientado a Objeto. Serão utilizadas somente as métricas de código estruturado, por se tratar da técnica de codificação comum entre os códigos analisados.

Diveros tipos de métricas foram criadas para avaliar a qualidade dos Softwares. Algumas delas medem a complexidade do software e características da linguagem de programação, relacionados a Manutebilidade e parte da Portabilidade. 

#### 3.1 Métrica de Complexidade Ciclomática de McCabe

McCabe desenvolveu uma métrica que permite aos desenvolvedores identificar módulos difíceis de testar ou manter. Como resultado, ele desenvolveu uma métrica de software que iguala a complexidade ao número de decisões em um programa. Os desenvolvedores podem usar essa medida para determinar quais módulos de um programa são excessivamente complexos e precisam ser recodificados. A métrica também pode ser usada para definir o número mínimo de casos de teste necessários para testar adequadamente os caminhos do programa.

Os valores de referência são exibidos na tabela abaixo:

| Complexidade |	Avaliação |
| --- | --- |
|1-10 |	Método simples. Baixo risco |
|11-20 |	Método razoavelmente complexo. Moderado risco |
|21-50 |	Método muito complexo. Elevado risco |
|51-N |	Método de altíssimo risco e bastante instável |

#### 3.2 Métricas de Software da RADC

A metodologia RADC consiste em elementos métricos, métricas, critérios e fatores. As pontuações dos elementos de métrica são combinadas para formar uma pontuação de métrica e as pontuações de métrica são combinadas para formar uma pontuação de critério. As pontuações de critérios são então combinadas de várias maneiras para produzir várias pontuações de fatores de qualidade. Elementos métricos são as medidas quantitativas de objetos de software. Métricas são os detalhes orientados ao software das características do software. Critérios são as características orientadas ao software que contribuem para vários tipos de qualidade.

#### 3.3 Métricas utilizadas

As métricas abaixo foram selecionadas para avaliar as subcaracterísticas de Manutebilidade e algumas subcaracterísticas de Portabilidade. A forma de avaliação está descrita no item 4.

| Métrica | Descrição | Impactos Positivos | Impactos Negativos | Ferramentas | Unidade de medida |
| --- | --- | --- | --- | --- | --- |
| Comprimento de código | Essa é uma medida do tamanho de um programa. Geralmente, quanto maior o tamanho do código de um componente, mais complexo e sujeito a erros o componente é. O comprimento de código tem mostrado ser uma das métricas mais confiáveis para prever a propensão a erros em componentes | - | Analisabilidade, Modificabilidade, Estabilidade, Testabilidade, Adaptabilidade |  FortranAnalyser | linhas de código |
| Complexidade Ciclomática Média (McCabe)| Permite aos desenvolvedores identificar módulos difíceis de testar ou manter. Calcula o número de decisões em um programa, que pode ser usado para determinar quais módulos de um programa são excessivamente complexos e precisam ser recodificados. | - | Analisabilidade, Modificabilidade, , Estabilidade | FortranAnalyser | Ver tabela na seção 3.1 |
| Documentação total  | Mede a porcentagem de código comentado (todo o código) (vazios e "!" sem continuação estão excluídos). Quanto maior melhor. | Analisabilidade, Modificabilidade | - | Check.py | razão |
| Documentação de rotinas | Mede a porcentagem rotinas comentadas. Facilita o entendimento do código e registra alterações históricas. Quanto maior melhor. | Analisabilidade, Modificabilidade, Reusabilidade, Testabilidade | - | FortranAnalyser | razão |
| Documentação de arquivos | Mede a porcentagem arquivos com comentários no início. Facilita o entendimento do código e registra alterações históricas. Quanto maior melhor. | Analisabilidade, Modificabilidade | - | FortranAnalyser | razão |
| **TODO - CHECAR** Documentação de variáveis | Mede a porcentagem de variáveis comentadas. Facilita o entendimento do código. Quanto maior melhor. | Analisabilidade, Modificabilidade | - | FortranAnalyser | razão |
| **TODO** Parâmetros nas rotinas | Mede a quantidade de parâmetros nas rotinas. Quanto maior o número, mais difícil a manutenção. | - | Analisabilidade, Modificabilidade, Estabilidade | - | - |
| **TODO** Dependência de máquina | Mede a quantidade de declarações que são dependentes de máquina. | - | Modificabilidade, Reusabilidade, Adaptabilidade, Capacidade para Substituir | - | - |
| **TODO** Modificações em loops | Mede quantas variáveis de loop foram modificadas. Idealmente deve ser zero. | - |Analisabilidade, Modificabilidade, Estabilidade | - | - |
| Tamanho médio das rotinas  | Tamanho médio em linhas de rotinas, que afeta a compreensibilidade e Manutebilidade | - | Analisabilidade, Modificabilidade, Reusabilidade, Estabilidade, Testabilidade | Check.py | linhas de código |
| Tamanho médio dos módulos | Dado importante. Quando comparado com o número médio das rotinas, se menor, vai indicar que as rotinas não estão encapsuladas em módulos. | - | Analisabilidade, Modificabilidade, Reusabilidade, Estabilidade, Testabilidade | Check.py | linhas de código |
| Tamanho médio do nome das variáveis | Considera-se rotinas e módulos. | Analisabilidade, Modificabilidade, Testabilidade | - | Check.py | quantidade de caracteres |
| Razão de only em uses |  A falta do only é problemática, pois propaga todas as variáveis para a estrutura e comumente é fonte de bugs. | Analisabilidade, Modificabilidade, Estabilidade | - | Check.py | razão |
| Razão de "goto" e "continue" por laço | O uso de gotos e continues é desconselhado, pois torna o código imcompreensível e de difícil manutenção. Idealmente deve ser zero. | - | Analisabilidade, Modificabilidade, Estabilidade. | Check.py | razao |
| **TODO check** Razão de "exit" e "cycle" por laço | O baixo uso de exit e cycle indica estruturas de laços mal formadas e podem apontar para códigos "macarrônicos" | Analisabilidade, Modificabilidade, Estabilidade | - | Check.py, FortranAnalyser | razão |
| Razão do uso de "implicit | Todas as rotinas e módulos deveriam ter "implicit" pois impedem error por variáveis não declaradas e bugs potenciais. Idealmente deve ser 100% | Analisabilidade, Modificabilidade, Estabilidade | - | Check.py | razão |
| Total de "equivalence" ou "common" | Equivalence ou common não são recomendados. Indicam estruturas de codificação antigas e riscos de bugs não mapeados. Essas duas keywords devem ser proibidas pois levam a erros graves quando partes do código são modificadas e vão afetar outras. | - | Modificabilidade, Estabilidade | Check.py | soma de linhas com palavra chave |
| Profundidade média de laços | Mede a média de linhas em laços. Laços muito grandes devem ser evitados pois são de baixa compreensão | - | Analisabilidade, Modificabilidade, Reusabilidade, Estabilidade, Testabilidade | Check.py | número de linhas |
| Aninhamento médio de laços | Número de laços aninhados médio. | - | Analisabilidade, Modificabilidade, Reusabilidade, Estabilidade, Testabilidade | Check.py | número de laços aninhados médio |
| **TODO Check** Fan-in | Média de chamadas por subrotina.  Número de vezes que a mesma subrotina é chamada. Números altos indicam que erros podem ser corrigidos em área comum facilitando a manutenção, mas que pode afetar a estabilidade | Analisabilidade, Modificabilidade, Reusabilidade, Testabilidade | Estabilidade | Check.py | número de linhas |
| **TODO Check** Fan-out | Número de rotinas que são chamadas por cada rotina (digamos, X). Um valor alto para fan-out sugere que a complexidade geral do X pode ser alta, devido a complexidade da lógica de controle necessária para coordenar as rotinas chamadas | Modificabilidade, Reusabilidade | Estabilidade, Testabilidade | Check.py | número de linhas |

### 4. Avaliação da Qualidade 

As características de qualidade descritas no item 2 devem ser avaliadas no início do projeto para ajudar a escolher o Core Dinâmico.  

As características de Manutebilidade e parte da Portabilidade deverão ser mensuradas através as ferramentas descritas no item 5. As demais características de Portabilidade deverão ser mensuradas utilizando o Core Dinâmico instalado nos ambientes disponibilizados para testes, considerando diferentes compiladores e bibliotecas como critérios de pontuação, descritos no item 6.

Os critérios de pontuação definidos pelas normas ISO são:
* Três pontos para as subcategorias que atendem satisfatoriamente os requisitos de qualidade (excelente, bom e razoável).
* Um ponto para as subcategorias com pontuação Insatisfatória.

Esses critérios de pontuação serão utilizados como base para a avaliação que deve ser feita por métricas, da seguinte forma: Para cada métrica, os Cores Dinâmicos que apresentarem:
* a melhor pontuação da métrica: recebem três pontos para cada subcategoria de qualidade relacionada à métrica (ver tabela de métricas no item 3). 
* as piores pontuações: recebem um ponto para cada subcategoria de qualidade relacionada à métrica (ver tabela de métricas no item 3). 

Em caso de empate dos melhores, os empatados recebem três pontos. Analogamente, os piores empatados recebem um ponto.

Ao final da pontuação de métricas, para cada subcategoria, serão somados os pontos. Um maior número de pontos de subcategorias indica uma melhor pontuação para uma Categoria, que recebe três pontos. As piores Categorias recebem um ponto.

Cada categoria recebe um peso na avaliação. Os Cores Dinâmicos com maior pontuação, considerando o peso, são indicados como os melhores. No caso de empate, uma maior pontuação nas subcategorias decide qual Core Dinâmico se destacou na categoria. Os pesos para cada categoria são definidos na fórmula abaixo:
 
    Total = Q1 + Q2 + Q3 + Q4 + Q5 + Q6
onde

    Q1 = PtsFuncionalidade * 3
    Q2 = PtsConfiabilidade * 1
    Q3 = PtsUsabilidade * 1
    Q4 = PtsEficiência * 3
    Q5 = PtsManutebilidade * 2
    Q6 = PtsPortabilidade * 1

Os pesos devem ser determinados com o consenso de todo o grupo de avaliação, sinalizando as características consideradas mais importantes na availiação do Core Dinâmico, dentre elas: 
* Funcionalidade: deve atender as principais demandas do Monan. 
* Eficiência: fator que considera o atual cenário de recursos computacionais e de energia.
* Manutebilidade: fator que considera o atual cenário de recursos humanos e os prazos para implementação. 

#### Considerações sobre a avaliação de outras características de Qualidade de Software

Outros documentos devem ser criados para a avaliação de outras características não consideradas neste.

Subcaracterísticas subjetivas podem não usar métricas para avaliação. Nesses casos, três pontos devem ser dados para cada melhor subcaracterística. A forma de avaliação deve ser justificada.

 O mesmo ambiente computacional e plano de testes devem ser considerados na execução dos diferentes Cores Dinâmicos, quando avaliada uma característica dependente de execução, como nos casos de Usabilidade, Eficiência e Confiabilidade. Por exemplo, ao avaliar a característica de Eficiência, considerando a máquina "M" com GPU, todos os cores devem ser avaliados na mesma máquina com as "mesmas" configurações de otimização para GPU do compilador utilizado e mesmas configurações de submissão.

Confiabilidade e Usabilidade, também poderão usar métricas para nortear as pontuações, que devem ser definidas e devidamente documentadas.

A subcaracterísitca Adequação, de Funcionalidade, pode ser avaliada através das publicações de artigos e manuais do software. A subcaracterística Acurácia e Interoperabilidade, deverá ser avaliada através de testes nos ambientes disponibilizados.

### 5. Ferramentas para avaliação da Manutebilidade e parte da Portabilidade

As ferramentas necessárias para a avaliação estão versionadas em https://github.com/monanadmin/monan/tree/main/tools/qas_eval . Algumas ferramentas foram adaptadas, como o FortranAnalyser, para possibilitar o uso sem interface gráfica. Outras utiliza-se somente o código binário, como no caso do fortran-src.

A imagem do container singularity (qas_eval.sif) utiliza as ferramentas versionadas do repositório (externamente ao container) e as ferramentas binárias (fortran-src e FortranAnalyser.jar) sobre uma distribuição Linux compatível com as ferramentas. As ferramentas binárias estão disponíveis na pasta /home/qas_files/tools/ dentro da imagem. 

Ferramentas com código fonte no repositório:

* check.py (https://github.com/monanadmin/monan/tree/main/tools/qas_eval). Programa desenvolvido para ler estatísticas geradas pelo programa (binário) fortran-src. Também contabiliza outras estatísticas através da análise direta do código, gerando um relatório sintetizado de métricas.
* FortranAnalyser (http://fortrananalyser.ephyslab.uvigo.es/). Ferramenta escrita em java, modificada para ser executada em linha de comando. Gera um relatório em PDF contendo estatísticas e scores, que são gerados por arquivo e em forma sintética para todo o conjunto de arquivos.

Ferramentas com código binário:

* fortran-src (https://github.com/camfort/fortran-src). Ferramenta escrita na linguagem Haskell que gera diversas estatísticas do código. 
* FortranAnalyser.jar. Binário gerado pelo código fonte. Para gerar, baixe ou instale a ferramenta maven, entre na pasta principal do FortranAnalyser (onde se encontra o pom.xml) e execute "mvn clean install".

Para executar a avaliação:
1. Use o módulo singularity no servidor Rattler ou instale em outro local desejado
2. Crie uma pasta de nome qas_eval 
3. Baixe a imagem do singularity na pasta criada
4. Baixe o código inteiro do monan (git clone https://github.com/monanadmin/monan.git). 
5. Baixe os códigos dinâmicos desejados na pasta DinCore e configure o caminho deles no script principal (próximo passo)
5. execute o script principal monan/tools/qas_eval/run_eval.sh $(pwd)

O último passo executará as ferramentas para os códigos de Core Dinâmicos e gerará os seguintes relatórios:
- QualityReport_[MODELO].pdf : Relatório gerado pela ferramenta FortranAnalyser. As estatísticas sintetizadas estão no final do arquivo
- Check_Report_[MODEL].txt : Relatório gerado pela ferramenta Check.py.

Os relatórios informam os valores das métricas obtidas por cada Core Dinâmico, que serão usados para pontuar as Características de Qualidade mapeadas para cada métrica (tabela do item 3.3).

### 6. Compiladores e ambientes para avaliação da Manutebilidade e parte da Portabilidade

**TODO - Rever ambientes de testes e compiladores, abaixo**

Os seguintes ambientes e compiladores estão disponíveis para a avaliação das diferentes características de qualidade:

Ambientes:
* EGEON
    * ssh -YC username@egeon.cptec.inpe.br
* DELL - Máquina Rattler, acessível via máquina Archer
    * ssh -YC username@143.166.198.151
    * ssh -YC rattler

Compiladores:
* gfortran

Cada característica ou conjunto de subcaracterísticas devem ser avaliadas em um ou mais ambientes, a depender se a característica ou conjunto de subcaracterísticas referem-se à Portabilidade, Usabilidade e Eficiência.

| Características de Qualidade | Ambientes |
| --- | --- |
| Funcionalidade | DELL, EGEON |
| Confiabilidade | |
| Usabilidade | |
| Eficiência | |
| Manutebilidade | DELL, máquina pessoal | 
| Portabilidade | Adaptabilidade (métrica Dependência de máquina): DELL, máquina pessoal; Capacidade para ser Instalado, Coexistência, Capacidade para Substituir: DELL, EGEON |

...

...

...
 

### Referências

SOMMERVILLE, Ian. Engenharia de Software. 9th Edition. ISBN-10, v. 137035152, p. 456-458; 465-470, 2011.

Wikipedia. ISO/IEC 9126. Disponível em 
https://pt.wikipedia.org/wiki/ISO/IEC_9126

Wikipedia. ISO/IEC 25010. Disponível em 
https://pt.wikipedia.org/wiki/ISO/IEC_25010

Fortran-src. Disponível em https://github.com/camfort/fortran-src

FortranAnalyser. Disponível em http://fortrananalyser.ephyslab.uvigo.es/




