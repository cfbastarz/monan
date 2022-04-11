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
4. Melhoria de funcionalidade: melhor precisão e acurácia nos resultados.

#### 2.2 Desenvolvimento de uma solução

Em geral o desenvolvimento das soluções para os requisitos são adotados pelo próprio pesquisador que altera o código do modelo. Essa alteração pode ser feita de algumas maneiras:

* Editando o código e fazendo pequenos testes de forma a indentificar a causa do problema e solução;
* Terceirizando a solução para bolsistas, alunos e pesquisadores/desenvolvedores externos;
* Adotando um novo código que é identificado como mais moderno e confiável para o problema.

Observa-se aqui que em todos os casos há diferentes formas e níveis de controlar essas tarefas. Inclusive, pode não ser adotado controle nenhum acarretando na confusão das versões dos códigos e perda de controle dos mesmos. Ademais, ao usar código exótico (ie., escrito fora dos padrões recomendados pela linguagem) existe sempre a possibilidade de que o código não tenha utilizado boas práticas de codificação o que, por um lado pode resolver um problema, e por outro acrescentar outros ao código com problemas de manutebilidade, portabilidade, performance, confiabilidade entre outros.

#### 2.3 Testes

Em geral os testes não são automáticos ou padronizados. O pesquisador ou desenvolvedor realiza um teste dentro de alguns pacotes que ele possui. Como modelos numéricos tem de atender a diversas condições físicas cujas condições iniciais são distintas, ou casos onde há diferentes topografias e outras condições, há sempre a possibilidade de que o código funcione em determinadas condições e passe a falhar em diversas outras. Além disso os testes sempre são focados no funcionamento. Em raros casos são realizados testes que garantam a qualidade do _software_ e respondam outras questões como:

* O código funciona em outros compiladores?
* O código tem desempenho mínimo esperado para ser usado em situação operacional?
* O código tem bom comportamento e funciona em arquiteturas distintas?
* As modificações foram devidamente documentadas e versionadas?
* Os manuais de uso, caso necessário, foram atualizados?  
* As modificações permitem manutenções futuras com facilidade?

#### 2.4 Implementação operacional

Após a etapa de testes e sendo considerado funcional, o modelo é então levado a situação operacional. Recentemente, no INPE, um grupo foi criado para realizar testes de acurácia do modelo e gerar relatórios com os resultados. Contudo, nesta estrutura, não há um fluxo padronizado e as normas que determinem quando o modelo está aprovado para a situação operacional diária é ainda incipiente e de certa forma tratado de modo subjetivo. 

### 3. Uso de ferramentas de controle de processo e de controle de versões (legado)

Atualmente alguns modelos numéricos usam o [Redmine](https://www.redmine.org/) hospedado no _site_ do CPTEC, para fazer o gerenciamento dos projetos. Alguns grupos de desenvolvimento e pesquisa fazem uso da ferramenta, mas não há um padrão de acompanhamento contínuo e auditável e nem uma gerência centralizada. Além disso a ferramenta está desatualizada e sem pessoal disponível para fazer a manutenção e implementar as melhorias necessárias.

O Redmine é conectado a um sistema de controle de versões no padrão Subversion (SVN).  Os códigos dos modelos versionados por esta ferramenta, tem desde um fraco controle (sem nenhuma regra de versionamento) até controles um pouco mais estruturados. Contudo, por excesso de códigos e falta de pessoal, há uma falta de convergência e de metodologia de uso para o controle de versões e dos processos de desenvolvimento.

Nesse contexto, não há um controle de requisitos efetivos que deve gerar as tarefas na maioria dos casos. Alguns grupos tentam manter o controle de registros de atividades e, em alguns casos, conseguem até manter um bom acompanhamento. Mas há diferentes níveis de uso da ferramenta e uma falta de cultura sobre a importância e a aplicação desses procedimentos.

### 4. Processo de Desenvolvimento de _Software_

O desenvolvimento de _software_ para o MONAN, deverá seguir as regras estabelecidas nesse documento. A adoção do processo deverá obrigatoriamente ser adotada por todas as intituições partícipes, internas ao INPE, e ter a gerência de todas as tarefas, atividades e desenvolvimentos gerenciadas pelo INPE. Os membros externos da comunidade de desenvolvimento do MONAN podem adotar metododologia ou processos próprios ou adotar o provesso aqui desenvolvido.

O processo de desenvolvimento é mostrado na Figura 1, com seus tópicos descritos a seguir.

#### 4.1 Grupos

Existem três tipos de grupos a considerar: 

1. Grupo de Computação e 
2. Grupos de Pesquisa/Desenvolvimento. 
3. Grupo de testes funcionais e avaliação de modelos

O Grupo de Computação está dividido em 4 Subgrupos:

a) Subgrupo de Revisão de Código;

b) Subgrupo de Testes automáticos;

c) Subgrupo de Processamento de Alto Desempenho (PAD);

d) Subgrupo de Versionamento;

Além desses 4 subgrupos há uma Gerência que deve manter a coesão entre todos os subgrupos de computação.

Os grupos de Pesquisa e Desenvolvimento são diversos e se dividem de acordo com as áreas de pesquisa envolvidas no desenvolvimento de modelos. Entre elas estão o Grupo de Dinâmica, Grupo de Radiação, Grupo de Superfície e Camada Limite, Grupo de Oceano, Grupo de Criosfera, Grupo de IA,  etc. Para efeito desse documento, todos esses grupos distintos serão tratados apenas como Grupos de Pesquisa e não são escopo direto do grupo que cuida das questões computacionais do modelo, denominado internamente como GCC - Grupo de Computação Científica cuja missão, atribuições, visão, valores e membros natos e transversais estão determinados em documento interno ao INPE.

#### 4.2 Ferramenta de Controle de Processos e Versionamento

Para uso do projeto MONAN, o controle de versionamento será baseado no padrão `git`. A razão para a adoção encontra-se no fato de que o `git` permite que sejam realizados _commits_ de código locais separados dos tronco principal de desenvolvimento (_trunk_) e dos galhos criados para cada fim (_branchs_). Assim é possível que apenas um pequeno grupo possa controlar os _pushes_ (os _uploads_ de modificações ao _trunk_) e _merges_ (as combinações dos códigos válidos entre os desenvolvimentos), garantindo a organização, coerência e a perpetuação dos códigos. 

Para isso adotou-se como ferramenta integradora o [GitHub](https://github.com/) que possui diversas funcionalidades altamente úteis para o controle do projeto, além de permitir que o código possa ser distribuído sem grandes problemas. O projeto está hospedado no endereço [GitHub - monanadmin/monan: MONAN - Model for Ocean-laNd-Atmosphere predictioN](https://github.com/monanadmin/monan).

A página principal do MONAN, também está hospedada no GitHub e está disponível em [MONAN | monan](https://monanadmin.github.io/monan/).

#### 4.3 Regras de desenvolvimento do modelo

Na Figura 1, estão diagramadas as etapas de desenvolvimento a serem adotadas para o MONAN. Cada etapa será descrita nas subseções a seguir.

##### <img title="" src="https://i.ibb.co/MZWt5TF/desenv.png" alt="Fluxo de desenvolvimento para o MONAN" width="706">

<figcaption>Figura 1 - Fluxo de desenvolvimento para o MONAN.</figcaption>

##### 4.3.1 Clone de desenvolvimento

Para iniciar uma etapa de desenvolvimento torna-se necessário ter um _clone_ do projeto. Para isso utiliza-se o comando de `clone` do `git`. Para isso basta executar o comando:

```bash
git clone https://github.com/monanadmin/monan.git
```

##### 4.3.2 Criando uma requisição para um desenvolvimento

Antes de começar um desenvolvimento, correção de bug, melhoria ou outra atividade com o modelo é necessário criar no repositório do [MONAN - Model for Ocean-laNd-Atmosphere predictioN](https://github.com/monanadmin/monan), na aba **Issues**, a requisição. Para isso clique na aba **Issues** e no botão **New Issue**. Preencha então o nome da requisição (nome curto e bem indentificável) e logo depois descreva na aba **Write** de forma completa, qual tarefa será realizada. No lado direito clique nas pessoas envolvidas (**Assignees**) e qual o rótulo adequado para a tarefa (**Labels**) e o projeto **MONAN**. A descrição permite o uso da linguagem de texto [Markdown](https://www.markdownguide.org/basic-syntax/). Assim é possível usar figuras, _links_, marcar usuários, etc. Não econonize na descrição da tarefa pois ela irá orientar o trabalho.

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

Uma pequena reunião será agendada para determinar os detalhes e a tarefa pode passar de **To Do** para **In Progress**, adotando-se o sistema de controle de tarefas KANBAN (veja mais em https://www.totvs.com/blog/negocios/kanban/). Esse controle das tarefas (e sub-tarefas ora geradas da tarefa principal) será realizado no próprio github no setor de projetos: https://github.com/monanadmin/monan/projects/1 . Nessa área encontram-se as tarefas dispostas em *tickets* e distribuídas em *boards* para cada fase da mesma. 

Cada *issue* aberta recebe um número sequencial mostrado como **#n** onde **n** é o número. Esse número é fundamental para o rastreio das tarefas e suas fases e para controle de versionamento do código.

##### 4.3.3 Geração do branch de desenvolvimento

Todo novo desenvolvimento deverá receber um _branch_ próprio que deve ser feito à partir do _trunk_ principal do modelo. Assim o desenvolvedor poderá trabalhar em área distinta derivada de um ponto atual do _trunk_ de desenvolvimento. Após ter um _clone_ ativo, para a geração de um _branch_ usa-se o comando:

```bash
git branch monan_16
git checkout monan_16
Switched to branch 'monan_16'
```

Como regra o branch deve estar associado a tarefa de forma direta, o que torna o processo mais facilmente rasterável. No exemplo acima o índice 16 indica que esse *branch* se deriva da **issue #16** disponível e descrita no github. 

A partir desse ponto o ambiente passa para o _branch_ de desenvolvimento solicitado. O `git commit` é feito sempre da mesma maneira mas o `push` para o _branch_ irá pedir algumas opções. Por exemplo, imagine que você queira criar um arquivo chamado `file.txt`, fazer o `commit` e o `push`. O processo fica então assim:

```bash
echo "Some text" > file.txt 
git add file.txt
git commit -m 'Realizando um teste
>
>apenas um teste
>'
git push --set-upstream origin monan_16
```

No exemplo acima, observe que o comando `git push --set-upstream origin monan_16`, entrega os arquivos adicionados ao _branch_ `monan_16` que foi criado anteriormente.

##### 4.3.4 Manipulação do código

Para alterar ou criar novos códigos o pesquisador e/ou desenvolvedor, de agora em diante chamado apenas de desenvolvedor, deve estar atento ao [DTN.01 - Padrão de codificação (_Code patterns_)](https://github.com/monanadmin/monan/wiki/Padr%C3%A3o-de-codifica%C3%A7%C3%A3o---(Code-patterns)) adotado pelo MONAN. Conhecer o documento DTN.01 é fundamental para realizar tal tarefa.

O MONAN tem em suas partes códigos de terceiros. Esses códigos tem formatação a padrões próprios. Espera-se que com o tempo os códigos legados e de terceiros assumam uma mesma padronização. A técnica é ir gradativamente alterando os códigos à medida que os mesmos vão sendo manipulados refatorando continuamente com ou sem o uso de ferramentas para tal fim.

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
git commit -m 'Cabeçalho do commit
>
> Mensagem adequada sobre esse commit
>'
```

Observe que adota-se uma primeira linha com um cabeçalho do commit. Depois, pulando uma linha escreve-se a mensagem informando  suscintamente o que está sendo 'comitado', a razão e informações relevantes.

```bash
git commit -m 'Nova interface de chamada da radiação 
>
> a chamada da radiação foi alterada para passar os valores
> dos arrays de aerossóis apenas nos limites do processador
> sem as bordas da ghostzone.
>'
```

O exemplo acima mostra uma forma prática e padronizada da descrição do *commit*.

Em caso de *commit* muito relevante para o status da tarefa recomenda-se que o *ticket* da tarefa   no github seja atualizada com os detalhes. É possível usar diversos tipos de atualizações nesses *tickets* como figuras, fórmulas, tabelas, links, códigos, diagramas e outras informações.

##### 4.3.6 Push nos _branchs_

Os _pushs_ para o _branch_ criado não devem ser feitos para quaisquer modificações. O desenvolvedor deve usar seus procedimentos pessoais de edição de código e de testes funcionais até que tenha algum código considerado com desenvolvimento robusto. Pushs intermediários podem ser realizados desde que entenda-se que houve avanço significativo. Ao fim de um processo de desenvolvimento o desenvolvedor deve fazer o `push` para o _branch_ em edição. Esse seria um push de fim de tarefa por parte do desenvolvedor.

Um `push` é feito de forma simples: estando todos os `commits` realizados (ie. com todos os arquivos que foram alterados, considerados para a entrega) basta fazer:

```bash
git push
```

Ao realizar o `push` uma mensagem é enviada ao gerente informando que um _push_ no _branch_ em desenvolvimento foi feito e um sinal é ativado. Esse sinal serve para abrir uma tarefa específica de revisão de código que será criada pelo gerente e atribuída ao subgrupo de revisão. O subgrupo de revisão será informado por e-mail sobre a nova tarefa disponibilizada.

Ao ser encerrada, a tarefa sai do status **In Progress** para o status **To Review**.

##### 4.3.7 Atualização do _branch_

À medida em que os desenvolvimentos são realizados, o repositório principal `main` é atualizado. Para manter o seu _branch_ atualizado com as últimas modificações do `main`, pode-se utilizar os comandos `fetch` para receber as os deltas do repositório e `rebase` para incorporar as atualizações ao seu _branch_ de desenvolvimentos (o que pode incluir novos arquivos):

```bash
git fetch
git rebase origin/main
```

A depender da situação em que os desenvolvimentos ocorrem e da necessidade de atualização com o `main`, outras formas de _merge_ podem ser aplicáveis. As instruções do documento https://git-scm.com/book/pt-br/v2/Branches-no-Git-Rebase podem ser utilizadas para a tomada de decisões correta.

Em caso de dúvidas ou problemas é possível se contactar o responsável pelo sistema de versionamento.

##### 4.3.8 Revisão de código

Ao receber a tarefa e começar a sua realização, o subgrupo de revisão deve informar ao gerente por _e-mail_ para que a tarefa saia do _status_ de **To Review** para o _status_ de **Review in Progress**.

A revisão do código deve ser realizada conforme determinado no documento [DTN.01 - Padrão de codificação (_Code patterns_)](https://github.com/monanadmin/monan/wiki/Padr%C3%A3o-de-codifica%C3%A7%C3%A3o---(Code-patterns))

O grupo de revisão pode usar ferramentas próprias para aplicar as regras do documento e determinar se os códigos estão em conformidade. Há também a possibilidade de efetuar pequenas correções em caso de resolver problemas simples. Ao terminar a revisão, o subgrupo determina se o código foi aprovado, aprovado com ressalvas (ie., quando há uma dependência a ser verificada) ou rejeitado (ie., quando não atende aos padrões estabelecidos pelo DNT.01).

Um documento contendo as informações no caso de rejeição deverá ser criado e nele devem constar informações completas sobre os problemas de código e de onde se encontram as não conformidades. Neste caso, é importante informar as porções do código onde os problemas se encontram (ie., nome(s) do(s) arquivo(s) e linha(s) em que ocorrem).

Esse documento deve ser adicionado ao github e seu link acrescentado na tarefa específica. Deverá ser enviado um email  para a gerência geral dos processos para que ela dê encaminhamento, realize a reunião necessária e mude adequadamente o _status_ das tarefas. Em caso de rejeição a tarefa sai do _status_ de **Review in Progress** para **To do** e recebe o acréscimo do documento de não conformidade.

Em caso de aprovação, essa sai do _status_ de **Review in Progress** para **To Test**. 

Nesse ponto o gerente envia uma mensagem ao grupo de testes para que o processo dê andamento.

##### 4.3.9 Testes de compilação e testes funcionais

O grupo de testes deve informar ao gerente quando a tarefa é colocada em testes. Nesse ponto a gerência coloca o _status_ em **In Test**.

Os testes a serem realizados serão de responsabilidade do subgrupo de testes. A definição dos testes está descrita no documento [DNT.02 - Documento Padrão de Testes (_Test Patterns_)](https://github.com/monanadmin/monan/wiki/Padr%C3%A3o-de-Testes--(Test-Pattern)).

São realizados testes de compilação e de funcionalidades simples conforme descritos no documento.

Estando completa a tarefa de testagem, o subgrupo de testes deverá gerar um documento com os resultados do teste de compilação e funcionalidade. Esse documento deve subir ao github e ter um link na tarefa. Deve-se enviar um email ao  gerente informando o fim do teste e o documento. Em caso de falha de compilação ou de não atendimento dos requisitos de testes, a tarefa volta ao desenvolvimento como relatório de testes e vai novamente ao _status_ de **To do**.

Em caso de aprovação nos testes a tarefa passa ao _status_ de **To performance Analisys** e o subgrupo de PAD é informado para realizar a avaliação de desempenho.

##### 4.3.10 Avaliação de desempenho

O grupo de PAD avisa ao gerente quando iniciar a análise de desempenho do código. Nesse ponto a tarefa passa ao _status_ de **PAD analisys**.

O subgrupo avalia se o código consegue atender aos requisitos mínimos de desempenho determinado em documento de [DNT.05 - Requisitos, Análise e Otimização de Código](https://github.com/monanadmin/monan/wiki/Requisitos,-An%C3%A1lise-e-Otimiza%C3%A7%C3%A3o-de-C%C3%B3digo). A maneira como o grupo atua na análise e otimização do código é determinada no mesmo documento.

O subgrupo de PAD pode realizar retrabalhos no código em prol da melhoria de performance. Se o retrabalho apresentar sucesso e o código for modificado é preciso reenviá-lo para *Code Review* e testes novamente.

O grupo interage com os desenvolvedores e pode dar um parecer positivo para a finalização da tarefa de desenvolvimento ou enviá-lo para retrabalho.  Em todos os casos um documento de resultados é gerado e levado ao github. A tarefa recebe então o link desse documento. Em caso de parecer positivo, a tarefa entra no _status_ **To Full Tests** e o gerente é informado.

##### 4.3.11 Testes funcionais completos

Os testes funcionais completos serão executados pelo grupo de avaliação de modelos. Esse grupo tem regras próprias para a realização dos testes e não estão dentro do Grupo De COmputação Científica, GCC.

O gerente informa ao responsável dos testes funcionais completos que uma versão  do código, branch monan_xx está disponível para testes completos. O Grupo de Avaliação de modelos realiza os testes e, em caso de aprovação, faz um relatório de resultados e comunica a gerência do GCC. Com isso um pull request é solicitado para subir o código para o sistema de versão como um candidato a operação.

Em caso de falha nos testes o Grupo de Avaliação comunica aos desenvolvedores e para a gerência do GCC.

##### 4.3.12 Versionamento e _releases_

O gerente do grupo de avaliação de modelos informa ao subgrupo de versionamento do GCC que uma versão foi testada eé é estável e funcional.  Uma reunião é marcada entre os líderes de cada subgrupo e o(s) desenvolvedor(es). Nessa etapa decide-se se será realizada apenas um _merge_ do _branch_ para o _trunk_ ou se será efetuado um _release_ da versão (_tag_).

Esse novo _release_ será um candidato a se tornar operacional, um _Release Candidate_ (RC). Para isso a uma _tag_ com  será criada com o sufixo **.RCn** onde _n_ é o número inteiro sequencial de candidatos a se tornarem operacionais. Esse número é equivalente a issue original que gerou a tarefa principal. Por exemplo: **monan-RC16** para o caso exempleficado nesse documento (**issue #16**)

Um RC deve conter, além dos códigos e bibliotecas, um documento de entrega para a operação. Esse documento deverá conter todas as informações sobre a compilação do código, sua configuração, _namelists_ operacionais e uma tabela com os arquivos necessários para sua operação. Esse documento deverá ser elaborado em conjunto pelos desenvolvedores e equipe de computação.

## 5. Etapas de testes operacionais

Após a liberação de um RC cabe à operação realizar os testes contínuos diários para validar a nova versão. Esses testes devem ser feitos por um período predeterminado dias que será avaliado pelos especialistas na área. Para cada rodada diária um grupo ligado à operação fará a avaliação dos resultados de forma a comparar com a versão corrente em operação e a RC. Isso pode ser feito usnado ferramentas automáticas e avaliações subjetivas, a critério da operação. Os resultados devem mostrar a comparação dos erros entre os dados observados para os dois modelos, de forma a se determinar se houve ganho ou perdas no acurácia da RC. Em caso de _bugs_ ou erros funcionais, deve ser comunicado imediatamente aos desenvolvedores e analistas de computação.

Outro ponto a ser medido é a _performance_ computacional do modelo, isto é, o tempo gasto para as rodadas do modelo corrente e do RC. 

Ao fim do período de testes deverá ser gerado um relatório com os dados obtidos, com o aceite ou não da RC e em qualquer caso observações e recomendações. Em caso de rejeição por piora de resultados, tempos de rodadas muito longos ou combinações das duas informações o grupo de desenvolvimento e/ou o grupo de testes e computação serão informados de maneira a promoverem os ajustes na RC e dar continuidade no desenvolvimento/testes.

Em caso de aceite a RC será liberada para a operação como uma versão estável e deverá receber seu versionamento com a mudança do número da versão no primeiro dígito. Por exemplo: `monan-5`, `monan-6`, etc

O repositório `main` (_trunk_) é então atualizado (_merge_) com a nova versão estável.

Cabe a operação avaliar a confiabilidade do código, isto é, número de vezes em que o código não rodou, rodou após intervenções não previstas ou apresentou falha catastrófica. Esses dados devem ser comunicados ao grupo GCC para que sejam avaliado se é um bug corrigível ou se necessita de reenvio ao desenvolvimento.

Cabe ainda para a operação pontuar a acessibilidade do código (facilidade de uso). Todos os reseultados devem ser registrados e associados a RC específica. 

## 6. Manutenção Contínua de Código

O código, após ser levado a _status_  operacional, deve receber manutenções contínuas. Essas manutenções podem ter caráter de ajustes na funcionalidade ou causados por _bugs_ e mudanças no seu ambiente externo.

Testes operacionais positivos, isto é, modelo validado irá gerar uma solicitação de merge (ou push) para o GCC. Este então irá gerar uma versão estável que fará parte do sistema de distribuição do código. Essa versão receberá uma denominação numérica em sua tag. A numeração será sequencial e se a tarefa que a originou tiver uma grande modificação se altera a tag mudando o primeiro dígito, monan-6 irá virar monan-7. SE é uma pequena modificação ela receberá um dígito a mais, por exemplo, monan-6 irá virar monan-6.1. 

Em casos de manutenção deverá ser aberto, ou pelo grupo de desenvolvimento, ou pelo grupo computacional, uma _issue_ e um _branch_ de manutenção.   O trabalho deve ser realizado e a correção, ainda como um _branch_, passar pelos testes internos e posteriormente pelos testes operacionais para validação. Os mesmos caminhos de uma versão de desenvolvimento normal.

Após os testes serem aprovados, _tags_ de correção serão lançadas acrescentando um ponto a mais na versão. Esse número após o ponto deve ser idêntico a issue que originou. Por exemplo, para a versão 6.1 que recebeu uma correção de *bug* pela *issue* 16 teremos: `monan-6.1.16`.

O `main` (_trunk_) é então atualizado (_merge_) com a nova versão estável e corrigida.

## Referências

* [Padrões de Codificação](bring.com.br);
* ESPP - Engenharia de Software Projetos e Processos - V2. Paula Filho, W.P. - 4a Edição - UFMG - LTC Editora - 2019.