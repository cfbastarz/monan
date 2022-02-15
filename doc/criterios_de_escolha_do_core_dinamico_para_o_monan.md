# Análise do Core Dinâmico base para o Model for Ocean-laNd-Atmosphere predictioN (MONAN)

## Documento Técnico Normativo - DTN.01 - Rev. Beta 0.1

##### Autores: Denis M. A. Eiras, ...

### 1. Introdução

A escolha do Core Dinâmico do MONAN deve ser feita levando em consideração alguns aspectos de qualidade de software. Segundo a norma ISO/IEC 9126, revisada pela norma ISO/IEC 25010:2011, quando a qualidade de software se refere ao produto, estas fornecem uma estrutura para especificar características de qualidade de software e realizar comparações entre produtos de software. Outras características e métricas de qualidade de software, encontrados em livros de engenharia de software, também foram introduzidas. Estas normas, características e métricas serão utilizadas como base para a construção dos critérios de escolha do core Dinâmico, descritos neste documento.

Dentro do contexto de qualidade interna, onde o software é avalidado para ser reutilizado e modificado, a definição das características e sub características devem ser levantadas em função da área de aplicação do produto de software. E, esta definição deve ser feita antes do início do desenvolvimento do mesmo. Produtos de maior porte devem ser subdivididos em módulos e cada um destes deve ter seus próprios conjuntos de características e sub características. Portanto, um Core Dinâmico de qualidade deve ser utilizado considerando alguns aspectos principais da qualidade de software que são inerentes ao Produto.


### 2. Caracterísiticas de Qualidade de Software

As caracterísitcas de qualidade que serão utilizadas para a avaliação do Core Dinâmico estão listados abaixo. Em todas as características listadas, temos uma subcaracterística com o nome de Conformidade. A conformidade é utilizada para avaliar o quanto o software obedece aos requisitos de legislação e todo o tipo de padronização ou normalização aplicável ao contexto.


**Funcionalidade**: A capacidade de um software prover funcionalidades que satisfaçam o usuário em suas necessidades declaradas e implícitas, dentro de um determinado contexto de uso. Suas subcaracterísticas são:

* Adequação: Capacidade do produto de software de prover um conjunto apropriado de funções para tarefas e objetivos do usuário especificados.
* Acurácia: Capacidade do produto de software de prover, com o grau de precisão necessário, resultados ou efeitos corretos ou conforme acordados.
* Interoperabilidade: Capacidade do produto de software de interagir com um ou mais sistemas especificados.
Segurança: Capacidade do produto de software de proteger informações e dados, de forma que pessoas ou sistemas não autorizados não possam lê-los nem modificá-los e que não seja negado o acesso às pessoas ou sistemas autorizados.
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

### 3. Métricas para avaliação das Características de Qualidade

Métricas de Software podem ser utilizadas para a avaliação de algumas Características de Qualidade. Exitem métricas para avaliação de código estruturado e de código Orientado a Objeto. Serão utilizadas somente as métricas de código estruturado. As métricas abaixo foram selecionadas para avaliar algumas características. A forma de avaliação está descrita no item 4.

| Métrica de software | Descrição |
| --- | --- |
| Fan-in/Fan-out | Fan-in é a medida do número de funções ou métodos que chamam outra função ou método (digamos X). Fan-out é o número de funções que são chamadas pela função de X. Um valor alto para fan-in significa que X está fortemente acoplado ao resto do projeto e alterações em X terão repercussões extensas. Um valor alto para fan-out sugere que a complexidade geral do X pode ser alta por causa da complexidade da lógica de controle necessário para coordenar os componentes chamados |
| Comprimento de código | Essa é uma medida do tamanho de um programa. Geralmente, quanto maior o tamanho do código de um componente, mais complexo e sujeito a erros o componente é. O comprimento de código tem mostrado ser uma das métricas mais confiáveis para prever a propensão a erros em componentes |
| Complexidade ciclomática | Essa é uma medida da complexidade de controle de um programa. Essa complexidade de controle pode estar relacionada à compreensibilidade de programa |
| Comprimento de identificadores | Essa é uma medida do comprimento médio dos identificadores (nomes de variáveis, classes, métodos etc.) em um programa. Quanto mais longos os identificadores, mais provável que sejam significativos e, portanto, mais compreensível o programa |
| Profundidade de aninhamento condicional | Essa é uma medida da profundidade de aninhamento de declarações if em um programa. Declarações if profundamente aninhadas são difíceis de entender e potencialmente sujeitas a erros |



### 4. Avaliação da Qualidade

As características descritas no item 2 devem ser avaliadas no início do projeto, levando em consideração a infra estrutura disponibilizada para os testes. 

As subcaracterísticas de Manutebilidade e a subcaracterística Adaptabilidade (de Portabilidade), serão avaliadas através da leitura e interpretação do código e de sua estrutura. As métricas, descritas no item 3, serão usadas para avaliar quantativamente essas subcaracterísticas.

Outras características, mais subjetivas como Confiabilidade e Usabilidade, também usarão estas métricas para nortear as pontuações.

As caracterísitcas de Funcionalidade podem ser avaliadas através das publicações de artigos relacionadas mas também devem ser testadas, como exemplo a subcaracterística de Interoperabilidade.

As características de Usabilidade, Eficiência, Confiabilidade e Portabilidade devem ser avalidas utilizando o Core Dinâmico instalado nos ambientes disponibilizados para testes.

Os ambientes disponibilizados para testes devem ser o mais parecidos possível, em termos da arquitetura do hardware, infraestrutura e softwares base, como o sistema operacional, compiladores, bibliotecas de alta performance e etc. O mesmo ambiente e plano de testes devem ser considerados na execução dos diferentes Cores Dinâmicos. Ainda, a categoria de Portabilidade deve considerar diferentes compiladores e bibliotecas como critério de pontuação.

Os critérios de pontuação adotados serão os definidos pelas normas ISO:
* Três pontos para as subcategorias que atendem satisfatoriamente os requisitos de qualidade (excelente, bom e razoável
* Um ponto para as categorias com pontuação Insatisfatória.

Quando utilizadas métricas, a pontuação de cada subcaracterística será avaliada como uma média das métricas utilizadas para aquela subcaracterística. 

Ao finao da avaliação, a pontuação deverá ser somada para se obter o resultado final da escolha do Core Dinâmico mais apropriado para o MONAN.

### Referências

SOMMERVILLE, Ian. Software engineering 9th Edition. ISBN-10, v. 137035152, p. 18, 2011.

Wikipediia. ISO/IEC 9126. Disponível em 
https://pt.wikipedia.org/wiki/ISO/IEC_9126


