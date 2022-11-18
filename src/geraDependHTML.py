# Gerador de html com dependências entre arquivos
# Autor: Luiz Flávio
# email: luflarois@gmail.com
# data: 18Nov2022
# Resumo:
# Para gerar o arquivo de entrada com as dependências
# copie todos os arquivos fontes de interesse para um diretório e
# use o comando makedepf90 para gerar um arquivo. Exemplo abaixo:
#
# makedepf90 *.F -DCORE_ATMOSPHERE > dep.txt
#
# No caso acima usa-se um 'define' que será passado na geração das dependências (CORE_ATMOSPHERE)
#
# Exemplo de uma linha de um arquivo de dependência:
# mpas_decomp.o : mpas_decomp.F mpas_log.o mpas_stream_manager.o mpas_derived_types.o mpas_kind_types.o
#
import os

#Apenas lê o arquivo de dependências
def read_depend(dependFile):
    f = open(dependFile,"r")
    lines = f.readlines()
    return lines

def mapaPrincipal(dir):
    #Criando o html clicável com os mapas de fontes
    os.system("dot "+dir+"dep.dot -Tpng -o "+dir+"dep.png -Tcmapx -o "+dir+"dep.map")
    file = dir + "index.html"
    dep = open(file, "w")
    dep.write("<html>\n")
    dep.write('<img src='+dir+'dep.png'+' usemap="#dependencias"/>\n')
    map = open(dir + 'dep.map', "r")
    lines = map.readlines()
    for line in lines:
        dep.write(line)
    map.close()
    dep.write("</html>\n")
    dep.close()
    return 0

def mapaBranch(dir,fonte):
    # gera o arquivo png e o mapa usado no html
    os.system("circo " + file + " -Tpng -o" + dir + fonte.replace(".F", ".png") + " -Tcmapx -o " + dir + fonte.replace(".F",                                                                                                             ".map"))
    file = dir + fonte.replace(".F", ".html")
    #Abre e escreve o html
    piece = open(file, "w")
    piece.write("<html>\n")
    piece.write('<img src=' + dir + fonte.replace(".F", ".png") + ' usemap="#dependencias"/>\n')
    map = open(dir + fonte.replace(".F", ".map"), "r")
    mapLines = map.readlines()
    map.close()
    for mapLine in mapLines:
        # print(mapLine)
        piece.write(mapLine)
    piece.write("</html>\n")
    piece.close()
    return 0

if __name__ == '__main__':

    #Ajuste o arquivo com as dependências e o diretório de saída abaixo:
    arquivoComAsDependencias = "/home/lufla/desenv/INPE/chkMPAS/dyn/dep.dot"
    dir = "/home/lufla/desenv/INPE/chkMPAS/"
    #############################################

    #Le o arquivo com as dependências
    lines = read_depend(arquivoComAsDependencias)
    # Cria o arquivo dot (graphviz)
    dot = open(dir+"dep.dot","w")
    dot.write("digraph dependencias\n")
    dot.write("{\n")
    dot.write("node[shape = box];")

    for line in lines:
        #  Pega os campos separados pelo ":"
        campos = line.split(":")
        # Define apenas o lado direito (após o :) e pega os campos separados por " "
        campos = campos[1].split()
        # O primeiro valor é o prório nome do arquivo fonte
        fonte = campos[0]
        #Preparando para criar o dot de cada fonte .F em separado
        file = dir+fonte.replace(".F",".dot")
        piece = open(file,"w")
        piece.write("digraph dependencias\n")
        piece.write("{\n")
        piece.write("node[shape = box];")
        #laço em todos os fontes na lista de dependências (campos)
        for dependencia in campos:
            # Se a dependência é o proprio fonte pule.
            if dependencia==fonte:
                dot.write('   "'+fonte+'";\n')
                continue
            dependencia = dependencia.replace(".o",".F")
            dot.write('   "'+fonte+'" -> "'+dependencia+'" [URL="'+ dependencia.replace(".F", ".html")+'", target="_blank"];\n')
            piece.write('   "'+fonte+'" -> "'+dependencia+'" [URL="'+ dependencia.replace(".F", ".html")+'", target="_blank"];\n')
        piece.write("}\n")
        piece.close()
        #Cria o mapa, o png e o html de cada fonte
        mapaBranch(dir, fonte)
    dot.write("}\n")
    dot.close()
    #Cria o mapa, o png e o html completo no index.html
    mapaPrincipal(dir)

