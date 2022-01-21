# Processo de Desenvolvimento de Software para o Model for Ocean laNd and Atmosphere predictioN (MONAN)

## Documento Técnico Normativo - DTN.03 - Rev. Beta 0.1

##### Autores: Luiz Flávio Rodrigues, ...

### 1. Introdução

Organizações que utilizam processos de desenvolvimento de software informais (sem caracterização de processos na realidade) ou processos muito rígidos (RUP, TSP, etc)  costumam introduzir erros relativos ao próprio processo nos softwares. Esses erros só podem ser minimizados com criação de processos gerenciados adequados que levem em conta a complexidade do software e a diversidade das equipes e pessoas envolvidas. No livro  Engenharia de Software: projetos e processos (...) lê-se 

> A capacitação de uma organização também requer a aplicação de processos. Esses processos têm etapas, resultados intermediários e pontos de controle. A aplicação dos processos de capacitação é realizada por meio de projetos, que precisam ter custos, prazos e responsáveis bem-definidos.

Esse documento normatiza um processo de desenvolvimento de software a ser aplicado no desenvolvimento do modelo MONAN. Ele é um modelo de processo inicial onde alguns pontos identificados como falhos no desenvolvimento de modelos numéricos são mitigados através do uso de uma metodologia baseada em processos ágeis. Esse modelo pode e deve ser revisto à medida que o desenvolvimento avança e que estágios intermediários indiquem a necessidade de mudança. 

As mudanças, se e quando houverem, deverão ser registradas com a atualização desse documento.

O Modelo MONAN é um modelo comunitário, isto é, desenvolvido com apoio efetivo de diversas instituições externas ao INPE. Com isso a gerência do processo de desenvolvimento conforme exigida acima precisa ser mais cuidadosa visto que pode haver diversas tarefas simultâneas. Para isso faz-se especialmente necessário uma padronização adequada.

### 2. Etapas identificadas no processo de desenvolvimento (legado)

As etapas identificadas no desenvolvimento de modelos numéricos até hoje observadas costumam ser as seguintes:

1. Requisitos 

2. Desenvolvimento de uma solução ou busca por soluções existentes

3. Testes 

4. Implementação operacional

#### 2.1 Requisitos

Os requisitos são originados por três fatores: 

a) necessidade específica identificada por um pesquisador. Essa necessidade pode ser advinda de sua própria pesquisa ou identificação de um problema físico, resultados ruins ou de alguma publicação ou apresentação onde melhorias ou problemas são verificados.

b) relatos de bugs ou mal funcionamento

c) Necessidade de aumento de performance dado os custos computacionais.

#### 2.2 Desenvolvimento de uma solução

Em geral o desenvolvimento das soluções para os requisitos são adotados pelo próprio pesquisador que altera o código do modelo. Essa alteração pode ser feita de algumas maneiras:

a) Editando o código e fazendo pequenos testes de forma a indentificar a causa do problema e solução.

b) Terceirizando a solução para bolsistas, alunos e pesquisadores/desenvolvedores externos.

c) Adotando um novo código que é identificado como mais moderno e confiável para o problema.

Observa-se aqui que em todos os casos há diferentes formas e níveis de controlar essas tarefas. Pode inclusive não ser adotado controle nenhum. Com isso sempre pode acontecer uma confusão de versões de código e perda de controle do mesmo. Ademais, ao usar código exótico existe sempre a possibilidade de que o código não tenha utilizado boas práticas de codificação o que, por um lado pode resolver um problema, e por outro acrescentar outros ao código.

#### 2.3 Testes

Em geral os testes não são automáticos ou padronizados. O pesquisador ou desenvolvedor realiza um teste dentro de alguns pacotes que ele possui. Como modelos numéricos tem de atender a diversas condições atmosféricas cujas condições iniciais são distintas, ou casos onde há diferentes topografias e outras condições sempre há a possibilidade de que o código funcione em determinadas condições e passe a falhar em diversas outras. Além disso os testes sempre são para funcionamento. Em raros casos são realizados testes que garantam a qualidade do software e respondam outras questões como:

a) O código funciona em outros compiladores?

b) O código tem performance mínima esperada para ser usado em operação?

c)  O código tem comportamento e funciona em arquiteturas distintas?

d) As modificações foram devidamente documentadas e versionadas?

e) Os manuais de uso, caso necessário, foram atualizados?  

#### 2.4 Implementação Operacional

Após a etapa de testes e sendo considerado funcional o modelo é então levado a ser implementado operacionalmente. Nos últimos tempos um grupo foi criado para realizar testes de acurácia do modelo e gerar relatórios com os resultados. Contudo não há na estrutura um fluxo padronizada e normas que determinem quando o modelo pode ir para a operação diária. 

### 3. Uso de ferramentas de controle de processo e de controle de versões (legado)

Atualmente alguns modelos numéricos usam o REDMINE, https://www.redmine.org/, baseado em site do CPTEC, para fazer o controle de processos. Alguns grupos de desenvolvimento e pesquisa fazem uso da ferramenta. Mas não há um padrão de acompanhamento contínuo e auditável e nem uma gerência centralizada. Além disso a ferramenta está desatualizada e sem pessoal disponível para fazer a manutenção e implementar as melhorias.

O REDMINE é conectado a um sistema de controle de versões no padrão subversion.  Os códigos de cada modelo tem desde um fraco controle sem nenhuma regra de versionamento até controles um pouco mais estruturados. Contudo há por excesso de códigos e falta de pessoal uma falta de convergência e de metodologia registrada para o controle de versões e do processo de desenvolvimento.

Não há um controle de requisitos efetivo que devem gerar as tarefas na maioria dos casos. Alguns grupos tentam manter o controle de registros de atividades e, em alguns casos, conseguem até manter um bom acompanhamento. Mas há diferentes níveis de uso e uma falta estrutural de cultura nesses processos.

### 4. Processo de Desenvolvimento de Software

O desenvolvimento de software para o MONAN, Model for Ocean laNd and Atmosphere predictioN, deverá seguir as regras que são demonstradas e descritas nesse documento. A adoção do processo deverá obrigatoriamente ser adotada por todas as intituições partícipes, internas e externas ao INPE, e ter a gerência de todas as tarefas, atividades e desenvolvimentos gerenciadas pelo INPE.

O processo de desenvolvimento é mostrado na figura 1 abaixo e terá seus tópicos descritos a seguir.

<img src="https://i.ibb.co/rxbgLFd/fluxodedesenvolvimento.png" title="" alt="" width="769">

Existem dois tipos de grupos a considerar: 

a) grupo de computação e 

b) grupos de Pesquisa/Desenvolvimento. 

O grupo de computação está dividido em 4 sub-grupos:  

i) Subgrupo de Revisão de código

ii) Subgrupo de Testes

iii)Subgrupo de PAD

iv)Subgrupo de Versionamento

Além desses 4 subgrupos há o grupo da gerência que mantem coesos todos os subgrupos de computação.

Os grupos de Pesquisa/Desenvolvimento são diversos e se dividem de acordo com as áreas de pesquisa envolvidas no desenvolvimento de modelos. Entre elas estão o grupo de dinâmica, grupo de radiação, grupo de superfície, grupo de oceano, etc. Para efeito desse documento trataremos todos esses grupos distintos como grupo de pesquisa apenas.



## Referências

* [Padrões de Codificação](bring.com.br);
* [ISO EIC 9126](https://pt.wikipedia.org/wiki/ISO/IEC_9126);
* [ISO EIC 25010:2011](https://pt.wikipedia.org/wiki/ISO/IEC_25010).