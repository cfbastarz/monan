## Gerador de html com dependências entre arquivos
## Autor: Luiz Flávio
## email: luflarois@gmail.com
## data: 18Nov2022
## Revisão: 0.1.0
##
## Resumo:
## Para gerar o arquivo de entrada com as dependências
## copie todos os arquivos fontes de interesse para um diretório e
## use o comando makedepf90 para gerar um arquivo. Exemplo abaixo:
##
## makedepf90 *.F -DCORE_ATMOSPHERE > dep.txt
##
## No caso acima usa-se um 'define' que será passado na geração das dependências (CORE_ATMOSPHERE)
##
## Exemplo de uma linha de um arquivo de dependência:
## mpas_decomp.o : mpas_decomp.F mpas_log.o mpas_stream_manager.o mpas_derived_types.o mpas_kind_types.o
##
## Licença: [CC BY-NC](https://creativecommons.org/licenses/by-nc/4.0/legalcode)
## ![](https://licensebuttons.net/l/by-nc/3.0/88x31.png)

########################
#Código principal - main 
########################
include("modUtil.jl")
import .modUtil

modUtil.header()

for x in ARGS
   if x == "-h" || x == "--help"
      modUtil.print_help()
      exit()
   elseif SubString(x, 1, 2) == "-i"
      global arquivoComAsDependencias = SubString(x,3,lastindex(x))
   elseif SubString(x, 1, 2) == "-o"
      global dir = SubString(x,3,lastindex(x))
   end
end

#Lê o arquivo de dependências
lines = modUtil.read_depend(arquivoComAsDependencias)

# Cria o arquivo dot (graphviz)
dot = open(dir*"dep.dot","w")
#Escreve o cabeçalho segundo o padrão dot
#Veja em https://graphviz.org/docs/layouts/dot/
write(dot,"digraph dependencias\n")
write(dot,"{\n")
write(dot,"   node[shape = box];\n")

#Laço que percorre as dependências
println("Fazendo o laço nos campos:")
for line in lines
   print("*")
   #  Pega os campos separados pelo ":"
   campos = split(line,":")
   # Define apenas o lado direito (após o :) e pega os campos separados por " "
   camposValidos = split(campos[2]," ")
   # O segundo valor é o prório nome do arquivo fonte
   fonte = camposValidos[2]
   #Preparando para criar o dot de cada fonte .F em separado
   file = replace(dir*fonte,".F" => ".dot")
   piece = open(file,"w")
   write(piece,"digraph dependencias\n")
   write(piece,"{\n")
   write(piece,"node[shape = box];")
   for dependencia in camposValidos
      # Se a dependência é o proprio fonte pule.
      if dependencia == ""
         continue
      end
      if dependencia==fonte
         write(dot,"   \""*fonte*"\";\n")
         continue
      end
      dependencia = replace(dependencia,".o" => ".F")
      write(dot  ,"   \""*fonte*"\" -> \""*dependencia*"\" [URL=\""*
         replace(dependencia,".F" => ".html")*"\", target=\"_blank\"];\n")
      write(piece,"   \""*fonte*"\" -> \""*dependencia*"\" [URL=\""*
         replace(dependencia,".F" => ".html")*"\", target=\"_blank\"];\n")
   end
   write(piece,"}\n")
   close(piece)
   #Cria o mapa, o png e o html de cada fonte
   modUtil.mapaBranch(dir, fonte, file)
end
println("")
write(dot,"}\n")
close(dot)
#Cria o mapa, o png e o html completo no index.html
println("Fazendo os mapas de referencia (HTML). Aguarde!")
modUtil.mapaPrincipal(dir)
println("")
println("Fim")