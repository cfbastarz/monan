# Processo de Desenvolvimento de _Software_ para o Model for Ocean-laNd-Atmosphere predictioN (MONAN)

## Documento Técnico Normativo - DTN.03 - Rev. Beta 0.1

##### Autores: Luiz Flávio Rodrigues, ...

### 1. Introdução

Organizações que utilizam processos de desenvolvimento de _software_ informais (sem caracterização de processos na realidade) ou processos muito rígidos (_Rational Unified Process_ - RUP, _Team Software Process_ - TSP, etc)  costumam introduzir erros relativos ao próprio processo nos _softwares_. Esses erros só podem ser minimizados com a criação de processos gerenciados adequados que levem em conta a complexidade do _software_ e a diversidade das equipes e pessoas envolvidas. No livro  "Engenharia de Software: Projetos e Processos (ESPP)", lê-se:

> A capacitação de uma organização também requer a aplicação de processos. Esses processos têm etapas, resultados intermediários e pontos de controle. A aplicação dos processos de capacitação é realizada por meio de projetos, que precisam ter custos, prazos e responsáveis bem-definidos.

Esse documento normatiza um processo de desenvolvimento de _software_ a ser aplicado no desenvolvimento do modelo _Model for Ocean-laNd-Atmosphere predictioN_ (MONAN). Na construção desse documento, pontos identificados como falhos no desenvolvimento de modelos numéricos são mitigados através do uso de metodologias baseadas em processos ágeis. Esse modelo pode e deve ser revisto à medida que o desenvolvimento avança e que estágios intermediários indiquem a necessidade de mudanças e aprimoramentos. 

As mudanças, se e quando houverem, deverão ser registradas com a atualização desse documento.

O MONAN é um modelo comunitário, isto é, desenvolvido com apoio efetivo de diversas instituições externas ao INPE. Com isso a gerência do processo de desenvolvimento conforme exigida acima precisa ser mais cuidadosa visto que podem haver diversas tarefas simultâneas. Para isso faz-se especialmente necessária uma padronização adequada nos procedimentos relacionados com o desenvolvimento geral e a interação entre as equipes de desenvolvimento das diversas componentes do modelo.

### 2. Etapas identificadas no processo de desenvolvimento (legado)

As etapas identificadas no desenvolvimento de modelos numéricos até hoje observadas, costumam ser as seguintes:

1. Levantamento dos requisitos;
2. Desenvolvimento de uma solução ou busca por soluções existentes;
3. Testes;
4. Implementação operacional.

#### 2.1 Levantamento dos requisitos

Os requisitos são originados por três fatores: 

1. Necessidade específica identificada por um pesquisador. Essa necessidade pode ser advinda de sua própria pesquisa ou identificação de um problema físico, resultados inadequados ou de alguma publicação ou apresentação onde melhorias ou problemas são verificados;
2. Relatos de _bugs_ ou mal funcionamento;
3. Necessidade de aumento de desempenho dados os custos computacionais.

#### 2.2 Desenvolvimento de uma solução

Em geral o desenvolvimento das soluções para os requisitos são adotados pelo próprio pesquisador que altera o código do modelo. Essa alteração pode ser feita de algumas maneiras:

* Editando o código e fazendo pequenos testes de forma a indentificar a causa do problema e solução;
* Terceirizando a solução para bolsistas, alunos e pesquisadores/desenvolvedores externos;
* Adotando um novo código que é identificado como mais moderno e confiável para o problema.

Observa-se aqui que em todos os casos há diferentes formas e níveis de controlar essas tarefas. Inclusive, pode não ser adotado controle nenhum acarretando na confusão das versões dos códigos e perda de controle dos mesmos. Ademais, ao usar código exótico (ie., escrito fora dos padrões recomendados pela linguagem) existe sempre a possibilidade de que o código não tenha utilizado boas práticas de codificação o que, por um lado pode resolver um problema, e por outro acrescentar outros ao código.

#### 2.3 Testes

Em geral os testes não são automáticos ou padronizados. O pesquisador ou desenvolvedor realiza um teste dentro de alguns pacotes que ele possui. Como modelos numéricos tem de atender a diversas condições físicas cujas condições iniciais são distintas, ou casos onde há diferentes topografias e outras condições, há sempre a possibilidade de que o código funcione em determinadas condições e passe a falhar em diversas outras. Além disso os testes sempre são focados no funcionamento. Em raros casos são realizados testes que garantam a qualidade do _software_ e respondam outras questões como:

* O código funciona em outros compiladores?
* O código tem desempenho mínimo esperado para ser usado em situação operacional?
*  O código tem comportamento e funciona em arquiteturas distintas?
* As modificações foram devidamente documentadas e versionadas?
* Os manuais de uso, caso necessário, foram atualizados?  

#### 2.4 Implementação operacional

Após a etapa de testes e sendo considerado funcional, o modelo é então levado a situação operacional. Recentemente, no CPTEC, um grupo foi criado para realizar testes de acurácia do modelo e gerar relatórios com os resultados. Contudo, nesta estrutura, não há um fluxo padronizado e normas que determinem quando o modelo está aprovado para a situação operacional diária. 

### 3. Uso de ferramentas de controle de processo e de controle de versões (legado)

Atualmente alguns modelos numéricos usam o [Redmine](https://www.redmine.org/) hospedado no _site_ do CPTEC, para fazer o gerenciamento dos projetos. Alguns grupos de desenvolvimento e pesquisa fazem uso da ferramenta, mas não há um padrão de acompanhamento contínuo e auditável e nem uma gerência centralizada. Além disso a ferramenta está desatualizada e sem pessoal disponível para fazer a manutenção e implementar as melhorias necessárias.

O Redmine é conectado a um sistema de controle de versões no padrão Subversion (SVN).  Os códigos dos modelos versionados por esta ferramenta, tem desde um fraco controle (sem nenhuma regra de versionamento) até controles um pouco mais estruturados. Contudo, por excesso de códigos e falta de pessoal, há uma falta de convergência e de metodologia de uso para o controle de versões e dos processos de desenvolvimento.

Nesse contexto, não há um controle de requisitos efetivos que deve gerar as tarefas na maioria dos casos. Alguns grupos tentam manter o controle de registros de atividades e, em alguns casos, conseguem até manter um bom acompanhamento. Mas há diferentes níveis de uso da ferramenta e uma falta de cultura sobre a importância e a aplicação desses procedimentos.

### 4. Processo de Desenvolvimento de _Software_

O desenvolvimento de _software_ para o MONAN, deverá seguir as regras estabelecidas nesse documento. A adoção do processo deverá obrigatoriamente ser adotada por todas as intituições partícipes, internas e externas ao INPE, e ter a gerência de todas as tarefas, atividades e desenvolvimentos gerenciadas pelo INPE.

O processo de desenvolvimento é mostrado na Figura 1, com seus tópicos descritos a seguir.

#### 4.1 Grupos

Existem dois tipos de grupos a considerar: 

1. Grupo de Computação e 
2. Grupos de Pesquisa/Desenvolvimento. 

O Grupo de Computação está dividido em 4 Subgrupos:

a) Subgrupo de Revisão de Código;

b) Subgrupo de Testes;

c) Subgrupo de Processamento de Alto Desempenho (PAD);

d) Subgrupo de Versionamento;

Além desses 4 subgrupos há o Grupo da Gerência que deve manter a coesão entre todos os subgrupos de computação.

Os grupos de Pesquisa e Desenvolvimento são diversos e se dividem de acordo com as áreas de pesquisa envolvidas no desenvolvimento de modelos. Entre elas estão o Grupo de Dinâmica, Grupo de Radiação, Grupo de Superfície e Camada Limite, Grupo de Oceano e Gelo Marinho, etc. Para efeito desse documento, todos esses grupos distintos serão tratados apenas como Grupos de Pesquisa.

#### 4.2 Ferramenta de Controle de Processos e Versionamento

Para uso do projeto MONAN, o controle de versionamento será baseado no padrão `git`. A razão para a adoção encontra-se no fato de que o `git` permite que sejam realizados _commits_ de código locais separados dos tronco principal de desenvolvimento (_trunk_) e dos galhos criados para cada fim (_branchs_). Assim é possível que apenas um pequeno grupo possa controlar os _pushes_ (os _uploads_ de modificações ao _trunk_) e _merges_ (as combinações dos códigos válidos entre os desenvolvimentos), garantindo a organização, coerência e a perpetuação dos códigos. 

Para isso adotou-se como ferramenta integradora o [GitHub](https://github.com/) que possui diversas funcionalidades altamente úteis para o controle do projeto, além de permitir que o código possa ser distribuído sem grandes problemas. O projeto está hospedado no endereço [GitHub - monanadmin/monan: MONAN - Model for Ocean-laNd-Atmosphere predictioN](https://github.com/monanadmin/monan).

A página principal do MONAN, também está hospedada no GitHub e está disponível em [MONAN | monan](https://monanadmin.github.io/monan/).

#### 4.3 Regras de desenvolvimento do modelo

Na Figura 1, estão diagramadas as etapas de desenvolvimento a serem adotadas para o MONAN. Cada etapa será descrita nas subseções a seguir.

##### ![Fluxo de desenvolvimento para o MONAN](https://i.ibb.co/rxbgLFd/fluxodedesenvolvimento.png)
<figcaption>Figura 1 - Fluxo de desenvolvimento para o MONAN.</figcaption>

##### 4.3.1 Clone de desenvolvimento

Para iniciar uma etapa de desenvolvimento torna-se necessário ter um _clone_ do projeto. Para isso utiliza-se o comando de `clone` do `git`. Para isso basta executar o comando:

```bash
git clone https://github.com/monanadmin/monan.git
```

##### 4.3.2 Criando uma requisição para um desenvolvimento

Antes de começar um desenvolvimento é necessário criar no repositório do [MONAN - Model for Ocean-laNd-Atmosphere predictioN](https://github.com/monanadmin/monan), na aba **Issues**, a requisição. Para isso clique na aba **Issues** e no botão **New Issue**. Preencha então o nome da requisição (nome curto e bem indentificável) e logo depois descreva na aba **Write** de forma completa, qual tarefa será realizada. No lado direito clique nas pessoas envolvidas (**Assignees**) e qual o rótulo adequado para a tarefa (**Labels**) e o projeto **MONAN**. A descrição permite o uso da linguagem de texto [Markdown](https://www.markdownguide.org/basic-syntax/). Assim é possível usar figuras, _links_, marcar usuários, etc. Não econonize na descrição da tarefa pois ela irá orientar o trabalho.

Um exemplo de abertura de tarefa para uso com o `git`:

> ### Descrição

> Adicione uma descrição do assunto que será abordado no pedido de abertura de tarefa no `git`. Inclua informações pertinentes ao que será realizado.

> ### Problemas abordados

> Adicione uma descrição sobre a motivação para a realização da tarefa.

> ### Dependências

> Descreva as dependências e potenciais impactos que a tarefa pode exercer sobre outras partes do código (incluindo interações com outras equipes e partes do código).

> ### Impactos

> Descreva os impactos e potenciais impactos que as modificações e desenvolvimentos podem exercer sobre outras partes do dódigo, incluindo outras equipes).

> ### Definição da Conclusão

> Descreva o resultado a ser encontrado a partir da realização da tarefa.

Ao criar a requisição o gerente será notificado. O gerente irá entrar em contato com você para tratar do _status_ do trabalho e determinar outras informações necessárias para a sua realização e acompanhamento.

Uma pequena reunião será agendada para determinar os detalhes e a tarefa pode passar de **To Do** para **In Progress**, adotando-se o sistema de controle de tarefas KANBAN (veja mais em https://www.totvs.com/blog/negocios/kanban/).

##### 4.3.3 Geração do branch de desenvolvimento

Todo novo desenvolvimento deverá receber um _branch_ próprio que deve ser feito à partir do _trunk_ principal do modelo. Assim o desenvolvedor poderá trabalhar em área distinta derivada de um ponto atual do _trunk_ de desenvolvimento. Após ter um _clone_ ativo, para a geração de um _branch_ usa-se o comando:

```bash
git branch radiation
git checkout radiation
Switched to branch 'radiation'
```

A partir desse ponto o ambiente passa para o _branch_ de desenvolvimento solicitado. O `git commit` é feito sempre da mesma maneira mas o `push` para o _branch_ irá pedir algumas opções. Por exemplo, imagine que você queira criar um arquivo chamado `file.txt`, fazer o `commit` e o `push`. O processo fica então assim:

```bash
echo "Some text" > file.txt 
git add file.txt
git commit -m 'apenas um teste'
git push --set-upstream origin radiation
```

No exemplo acima, observe que o comando `git push --set-upstream origin radiation`, entrega os arquivos adicionados ao _branch_ `radiation` que foi criado anteriormente.

##### 4.3.4 Manipulação do código

Para alterar ou criar novos códigos o pesquisador e/ou desenvolvedor, de agora em diante chamado apenas de desenvolvedor, deve estar atento ao [DTN.01 - Padrão de codificação (_Code patterns_)](https://github.com/monanadmin/monan/wiki/Padr%C3%A3o-de-codifica%C3%A7%C3%A3o---(Code-patterns)) adotado pelo MONAN. Conhecer o documento DTN.01 é fundamental para realizar tal tarefa.

##### 4.3.5 _Commits_ locais constantes

É importante que o desenvolvedor realize constantemente _commits_ locais no código de forma a controlar o desenvolvimento e poder fazer o _rollback_ (ie., voltar a edições anteriores do código) e manter a cópia segura fazendo _backups_ constantes do código.

Antes de fazer um `commit` é preciso estar atento ao _status_ do trabalho. Saber antecipadamente quais os arquivos foram alterados e destes escolher aqueles que devem ser adicionados no _commit_.

Para saber o _status_ da sua cópia de trabalho local, basta fazer:

```bash
git status
```

Uma lista com o _status_ dos arquivos será mostrada. Para cada arquivo que se deseja que faça parte do `commit` deve-se usar o comando `add`:

```bash
 git add arquivo1
 git add arquivo2

...

```

e depois pode-se fazer o `commit` passando a mensagem adequada com a opção `-m`:

```bash
git commit -m 'Mensagem sobre esse commit'
```

##### 4.3.6 Push nos _branchs_

Os _pushs_ para o _branch_ criado não devem ser feitos para quaisquer modificações. O desenvolvedor deve usar seus procedimentos pessoais de edição de código e de testes funcionais até que tenha algum código considerado pronto para testes robustos. Ao fim de um processo de desenvolvimento o desenvolvedor deve fazer o `push` para o _branch_ em edição. 

Um `push` é feito de forma simples: estando todos os `commits` realizados (ie. com todos os arquivos que foram alterados, considerados para a entrega) basta fazer:

```bash
git push
```

Ao realizar o `push` uma mensagem é enviada ao gerente informando que um _push_ no _branch_ em desenvolvimento foi feito e um sinal é ativado. Esse sinal serve para abrir uma tarefa específica de revisão de código que será criada pelo gerente e atribuída ao subgrupo de revisão. O subgrupo de revisão será informado por e-mail sobre a nova tarefa disponibilizada.

Ao ser encerrada, a tarefa sai do status **In Progress** para o status **To Review**.

##### 4.3.7 Revisão de código

Ao receber a tarefa e começar a sua realização, o subgrupo de revisão deve informar ao gerente por _e-mail_ para que a tarefa saia do _status_ de **To Review** para o _status_ de **Review in Progress**.

A revisão do código deve ser realizada conforme determinado no documento [DTN.01 - Padrão de codificação (_Code patterns_)](https://github.com/monanadmin/monan/wiki/Padr%C3%A3o-de-codifica%C3%A7%C3%A3o---(Code-patterns))

O grupo de revisão pode usar ferramentas próprias para aplicar as regras do documento e determinar se os códigos estão em conformidade. Há também a possibilidade de efetuar pequenas correções em caso de resolver problemas simples. Ao terminar a revisão, o subgrupo determina se o código foi aprovado, aprovado com ressalvas (ie., quando há uma dependência a ser verificada) ou rejeitado (ie., quando não atende aos padrões estabelecidos pelo DNT.01).

Um documento contendo as informações no caso de rejeição deverá ser criado e nele devem constar informações completas sobre os problemas de código e de onde se encontram as não conformidades. Neste caso, é importante informar as porções do código onde os problemas se encontram (ie., nome(s) do(s) arquivo(s) e linha(s) em que ocorrem).

Esse documento deve ser enviado para a gerência para que ela dê encaminhamento, realize a reunião necessária e mude adequadamente o _status_ das tarefas. Em caso de rejeição a tarefa sai do _status_ de **Review in Progress** para **To do** e recebe o acréscimo do documento de não conformidade.

Em caso de aprovação, essa sai do _status_ de **Review in Progress** para **Reviewer approved**. 

Nesse ponto o gerente envia uma mensagem ao grupo de testes para que o processo dê andamento.

##### 4.3.8 Testes de compilação e testes funcionais

O grupo de testes deve informar ao gerente quando a tarefa é colocada em testes. Nesse ponto a gerência coloca o _status_ em **In Test**.

Os testes a serem realizados serão de responsabilidade do subgrupo de testes. A definição dos testes está descrita no documento [DNT.02 - Documento Padrão de Testes (_Test Patterns_)](https://github.com/monanadmin/monan/wiki/Padr%C3%A3o-de-Testes--(Test-Pattern)).

São realizados testes de compilação e de funcionalidade conforme descritos no documento.

Estando completa a tarefa, o subgrupo de testes deverá enviar um _e-mail_ ao gerente informando o _status_ dos testes. Em caso de falha de compilação ou de não atendimento dos requisitos de testes, a tarefa volta ao desenvolvimento como relatório de testes e vai ao _status_ de **To do**.

Em caso de aprovação nos testes a tarefa passa ao _status_ de **Test Approved** e o subgrupo de PAD é informado para realizar a avaliação de desempenho.

##### 4.3.9 Avaliação de desempenho

O grupo de PAD avisa ao gerente quando iniciar a análise de desempenho do código. Nesse ponto a tarefa passa ao _status_ de **PAD analisys**.

O subgrupo avalia se o código consegue atender aos requisitos mínimos de desempenho determinado em documento de [DNT.05 - Requisitos, Análise e Otimização de Código](https://github.com/monanadmin/monan/wiki/Requisitos,-An%C3%A1lise-e-Otimiza%C3%A7%C3%A3o-de-C%C3%B3digo). A maneira como o grupo atua na análise e otimização do código é determinada no mesmo documento.

O grupo interage com os desenvolvedores e pode dar um parecer positivo para a finalização da tarefa de desenvolvimento ou enviá-lo para retrabalho. Em caso de parecer positivo, a tarefa entra no _status_ **Done** e o gerente é informado.

##### 4.3.10 Versionamento e _releases_

O gerente informa ao subgrupo de versionamento e uma reunião é marcada entre os líderes de cada subgrupo e o(s) desenvolvedor(es). Nessa etapa decide-se se será realizada apenas um _merge_ do _branch_ para o _trunk_ ou se será efetuado um _release_ da versão (_tag_).

## Referências

* [Padrões de Codificação](bring.com.br);
* ESPP - Engenharia de Software Projetos e Processos - V2. Paula Filho, W.P. - 4a Edição - UFMG - LTC Editora - 2019.