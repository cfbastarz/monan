## Como instalar o pacote Fortran para o editor [Sublime Text 4](https://www.sublimetext.com/):

Por favor, consulte a página [Fortran - Packages - Package Control](https://packagecontrol.io/packages/Fortran) para esse fim.

## Como instalar os Snippets para o editor [Sublime Text 4](https://www.sublimetext.com/):

Pegar no diretório aux os arquivos:

- *add_date.py*
- *Default (Linux).sublime-keymap*

- *function.sublime-snippet*

- *module.sublime-snippet*

- *program.sublime-snippet*

- *subroutine.sublime-snippet*

e copiá-los para ~/.config/sublime-text/Packages/ 

## Como configurar para seu uso pessoal

Edite o arquivo ~/.config/sublime-text/Packages/add_date.py e altere  **seu_nome** e **seu_email** para os valores corretos.

## Como usar os snippets e funções:

Estando com o editor sublime aberto e editando um arquivo fortran,  para criar uma nova função, módulo, subrotina ou programa gasta digitar o nome do procedure (function, module, subroutine, program) e pressionar a tecla tab que tudo será preenchido de forma automática.

Após o preenchimento o cursor se posicionará em cada campo para preenchimento. Um tab fará que o cursor pule para os demais campos.

ALgumas teclas foram predefinidas em add_date.py para preenchimento automático durante a edição:

    ctrl+shift+, adiciona uma data no campo ou em qualquer lugar.
    ctrl+shift+. adiciona a hora no campo ou em qualquer lugar.
    ctrl+alt+n adiciona seu nome no campo ou em qualquer lugar.
    ctrl+alt+m adiciona seu e-mail no campo ou em qualquer lugar.
    ctrl+alt+s adiciona o site do projeto no campo ou em qualquer lugar.
    ctrl+alt+z adiciona o nome do projeto no campo ou em qualquer lugar. 

> <mark>OBS:</mark> Para realizar essas funções o editor precisa identificar o código como sendo Fortran. Se é a primeira vez que você o edita então é necessário salvá-lo com a extensão correta antes de usar as funções. 




