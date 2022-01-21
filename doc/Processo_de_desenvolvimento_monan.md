# Processo de Desenvolvimento de Software para o Model for Ocean laNd and Atmosphere predictioN (MONAN)

## Documento Técnico Normativo - DTN.03 - Rev. Beta 0.1

##### Autores: Luiz Flávio Rodrigues, ...

### 1. Introdução

Organizações que utilizam processos de desenvolvimento de software informais (sem caracterização de processos na realidade) ou processos muito rígidos (RUP, TSP, etc)  costumam introduzir erros relativos ao próprio processo nos softwares. Esses erros só podem ser minimizados com criação de processos gerenciados adequados que levem em conta a complexidade do software e a diversidade das equipes e pessoas envolvidas. No livro  Engenharia de Software: projetos e processos (ESPP) lê-se 

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

#### 4.1 Grupos

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

#### 4.2 Ferramenta de Controle de processo e versionamento

Para uso do projeto MONAN o controle de versionamento será baseado no GIT. A razão para a adoção encontra-se no fato de que o GIT permite que sejam realizados commits de código locais separados dos tronco principal de desenvolvimento (trunk) e dos galhos criados para cada fim (branchs). Assim é possível que apenas um pequeno grupo posso controlar os push (uploads de modificações ao trunk) e merge (mistura de códigos válidos entre desenvolvimentos) garantindo coerência e perpetuação dos códigos. 

Para isso adotou-se como ferramenta integradora o Github que possui diversas funcionalidades altamente úteis para o controle do projeto e permite ainda que o código possa ser distribuído sem grandes problemas. O projeto está hospedado  no endereço [GitHub - monanadmin/monan: MONAN - Model for Ocean laNd and Atmosphere predictioN](https://github.com/monanadmin/monan)

A página do Modelo está disponível em [MONAN | monan](https://monanadmin.github.io/monan/)

#### 4.3 Regras de desenvolvimento do modelo

##### ![](https://i.ibb.co/rxbgLFd/fluxodedesenvolvimento.png)

##### 4.3.0 Clone de desenvolvimento

Para iniciar uma etapa de desenvolvimento torna-se necessário ter um clone do projeto. Para isso usa-se o comando de clone do git. Para isso basta executar o comando

```bash
git clone https://github.com/monanadmin/monan.git
```

##### 4.3.1 Criando uma requisição para um desenvolvimento

Antes de começar um desenvolvimento é necessário criar no repositório do [MONAN - Model for Ocean laNd and Atmosphere predictioN](https://github.com/monanadmin/monan),  na aba **Issues**, a requisição. Para isso clique na aba **Issues** e no botão **New Issue**. Preencha então o nome da requisição (nome curto e bem indentificável) e logo depois descreva na aba **Write** de forma completa qual a tarefa a ser realizada. No lado direito clique nas pessoas envolvidas (**Assignees**) e qual label é adequado para a tarefa (**Labels**) e o projeto **MONAN**. A descrição permite o uso da linguagem de texto [Markdown](https://www.markdownguide.org/basic-syntax/). Assim é possível usar figuras, links, apontar a um suário, etc. Não econonize na descrição da tarefa pois ela irá orientar o trabalho.

Ao criar a requisição o gerente será notificado. O gerente irá entrar em contato com você para tratar do status do trabalho e determinar outras informações necessárias para sua realização e acompanhamento.

Uma pequena reunião será agendada para determinar os detalhes e a tarefa pode passar de "**To Do**" para "**In Progress**" no sistema de controle de tarefas (KANBAN).

##### 4.3.2 Geração do branch de desenvolvimento

Todo novo desenvolvimento deverá receber um branch próprio que deve ser feito à partir do trunk principal do modelo. Assim o desenvolvedor poderá trabalhar em área distinta derivada de um ponto atual do trunk de desenvolvimento. Após ter um clone ativo, para a geração de um branch usa-se o comando

```bash
git branch radiation
git checkout radiation
Switched to branch 'radiation'
```

A partir desse ponto o ambiente passa para o branch de desenvolvimento solicitado. O git commit é feito sempre da mesma maneira mas o push para o branch irá pedir algumas opções. Por exemplo, imagine que você queira criar  um arquivo chamado file.txt, fazer o commit e o push. O processo fica então assim:

```bash
echo "Some text" > file.txt 
git add file.txt
git commit -m 'apenas um teste'
git push --set-upstream origin teste
```

##### 4.3.3 Manipulação do código

Para alterar ou criar novos códigos o pesquisador e/ou desenvolvedor, de agora em diante chamado apenas de desenvolvedor, deve estar atento ao [Padrão de codificação (Code patterns)](https://github.com/monanadmin/monan/wiki/Padr%C3%A3o-de-codifica%C3%A7%C3%A3o---(Code-patterns)) adotado pelo MONAN. Conhecer o documento DN01 é fundamental para realizar tal tarefa.

##### 4.3.4 Commits locais constantes

É importante que o desenvolvedor faça contantemente commits locais no código de forma a controlar o desenvolvimento e poder fazer rollback (voltar a edições anteriores do código) e manter a cópia segura fazendo backups constantes do código.

Antes de fazer um commit é preciso estar atento ao status do trabalho. Saber antecipadamente quais os arquivos foram alterados e destes escolher aqueles que devem ser adicionados no commit.

Para saber o status do git basta fazer

```bash
git status
```

Uma lista com o status dos arquivos será mostrada. Para cada arquivo que se deseja que façaa parte do commit deve-se usar o comando add

```bash
 git add arquivo1
 git add arquivo2

...

```

e depois pode-se fazer o commit passando a mensagem adequada.

```bash
git commit -m 'Mensagem sobre esse commit'
```

##### 4.3.5 Push nos branchs

Os "pushs" para o branch criado não deve ser feito para qualquer modificação. O desenvolvedor deve usar seus procedimentos pessoais de edição de código e de testes funcionais até que tenha algum código considerado pronto para testes robustos. Ao fim de um processo de desenvolvimento o desenvolvedor deve fazer o **push** para o branch em edição. 

um push é feito de forma simples. Estando todos os commits realizados basta fazer

```bash
git push
```

Ao realizar o push uma mensagem é enviada ao gerente informando que um "push" no branch em desenvolvimento foi feita e um sinal é ativado. Esse sinal serve para abrir uma tarefa específica de revisão de código que será criada pelo gerente e atribuída ao sub-grupo de revisão. O subgrupo de revisão será informado por e-mail sobre a nova tarefa disponibilizada.

Ao ser encerrada a tarefa sai do status "**In Progress**" para o status "**To Review**"

##### 4.3.6 Revisão de código

Ao receber a tarefa e começar a sua realização o subgrupo de revisão deve informar ao gerente (e-mail) para que a tarefa saia do status de "**To Review**" para o status de "**Review in Progress**"

A revisão do código deve ser realizada conforme determinado no  [Padrão de codificação (Code patterns)](https://github.com/monanadmin/monan/wiki/Padr%C3%A3o-de-codifica%C3%A7%C3%A3o---(Code-patterns))

O grupo de revisão pode usar ferramentas próprias para aplicar as regras do documento e determinar se os códigos estão em conformidade. Há também a possibilidade de efetuar pequenas correções em caso de resolver problemas simples.  Ao terminar a revisão o subgrupo determina se o código foi aprovado, aprovado com ressalvas ou rejeitado.

Um documento contendo as informações no caso de rejeição deverá ser criado e nele deve conter informações completas sobre os problemas de código e de onde se encontram as não conformidades.

Esse documento deve ser enviado para a gerência para que ela dê encaminhamento, realize a reunião necessária e mude adequadamente o status das tarefas. Em caso de rejeição a tarefa sai do status de  "**Review in Progress**" para "**To do**" e recebe o acréscimos do documento de não conformidade.

Em caso de aprovação essa sai do status de "**Review in Progress**" para "**Reviewer approved**". 

Nesse ponto o gerente envia uma mensagem ao grupo de testes para que o processo dê andamento.

##### 4.3.7 Testes de compilação e testes funcionais

O grupo de testes deve informar ao gerente quando a tarefa é colocada em testes. Nesse ponto a gerência coloca o status em "**In Test**"

Os testes a serem realizados serão responsabilidade do subgrupo de testes. A <mark>definição dos testes está descrita do documento DT.02, Documento Padrão de Testes.</mark>

São realizados testes de compilação e de funcionalidade conforme descritos no documento.

 Estando completa a tarefa o subgrupo de testes deverá enviar um e-mail ao gerente informando o status dos testes. Em caso de falha de compilação ou de não atendimento dos requisitos de teste a tarefa volta ao desenvolvimento como relatório de testes e vai ao status de "**To do**".

Em caso de aprovação nos testes a tarefa passa ao status de "**Test Approved**" e o subgrupo de PAD é informado para realizar a avaliação de performance.

##### 4.3.8 Avaliação de performance

O grupo de PAD avisa ao gerente quando iniciar a análise da performance do código. Nesse ponto a tarefa passa ao status de "**PAD analisys**".

O subgrupo avalia se o código consegue atender aos requisitos mínimos de performance determinado em documento de Requisitos, Análise e Otimização de Código, DN.05. A maneira como o grupo atua na análise e otimização do código é determinado no mesmo documento.

O grupo interage com os desenvolvedores e pode dar um parecer positivo a finalização da tarefa de desenvolvimento  ou enviá-lo para retrabalho. Em caso de parecer positivo a tarefa entra no status "**Done**" e o gerente é informado.

##### 4.3.9 Versionamento e Releases

O gerente informa ao subgrupo de versionamento e uma reunião é marcada entre os líderes de cada subgrupo e o desenvolvedor. Nessa etapa decide-se se será realizada apenas um merge do branch para o trunk ou se será efetuado um release de versão (tag).







## Referências

* [Padrões de Codificação](bring.com.br);
* ESPP - Engenharia de Software Projetos e Processos - V2. Paula Filho, W.P. - 4a Edição - UFMG - LTC Editora -2019