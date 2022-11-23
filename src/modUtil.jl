module modUtil

   export read_depend, mapaBranch, mapaPrincipal, header, print_help

   function print_help()
      println("\nUso:")
      println("   julia ./geraDepend.jl -i<arqin> -o<dirout> \n")
      println("OPCOES META")
      println("-h, --help     Mostra a lista da linha de comando\n")
      println("OPCOES")
      println("-o             Define o diretorio de saida dos dados")
      println("-i             Define o arquivo de entrada\n")
      println("EXEMPLO")
      println("julia ./geraDepend.jl -i/home/lufla/data/dep.txt -o/home/lufla/html\n")
      println("OBSERVACAO")
      println("Eh preciso ter a linguagem Julia instalada. Para instalar visite")
      println("https://julialang.org/\n")
      println("Para quem usa Ubuntu e Unbuntu Flavours faca:")
      println("sudo apt install julia\n\n")
   end
   
   ################################
   function read_depend(dependFile)
      #Apenas lê o arquivo de dependências
       f = open(dependFile,"r")
       lines = readlines(f)
       return lines
   end

   ###################################
   function mapaBranch(dir,fonte,file)
      # gera o arquivo png e o mapa usado no html
      # Para isso cria o nome dos arquivos imagem e mapa
      imagem = replace(fonte,".F" => ".png")
      mapa  = replace(fonte,".F" => ".map")
      #Criando o system command (tipo do Julia) e rodando
      #Os "$" substituem as variáveis definidas
      cmd = `circo $file -Tpng -o $dir$imagem -Tcmapx -o $dir$mapa`
      run(cmd)
      #cria o arquivo html
      html = replace(dir * fonte,".F" => ".html")
      #Abre e escreve o html
      piece = open(html, "w")
      write(piece,"<html>\n")
      write(piece,"<img src=" * dir * replace(fonte,".F" => ".png") * " usemap=\"#dependencias\"/>\n")
      #Le e inclui o mapa clicável no html
      map = open(dir * replace(fonte,".F" => ".map"), "r")
      mapLines = readlines(map)
      close(map)
      for mapLine in mapLines
         write(piece,mapLine*"\n")
      end
      write(piece,"</html>\n")
      close(piece)
   end

   ###########################
   function mapaPrincipal(dir)
      #Criando o html clicável com os mapas de fontes
      # gera o arquivo png e o mapa usado no html
      # Para isso cria o nome dos arquivos imagem e mapa
      imagem = dir*"dep.png"
      mapa  = dir*"dep.map"
      dot = dir*"dep.dot"
      #Criando o system command (tipo do Julia) e rodando
      #Os "$" substituem as variáveis definidas
      cmd = `circo $dot -Tpng -o $imagem -Tcmapx -o $mapa`
      run(cmd)
      file = dir*"index.html"
      dep = open(file, "w")
      write(dep,"<html>\n")
      write(dep,"<img src="*dir*"dep.png"*" usemap=\"#dependencias\"/>\n")
      map = open(dir*"dep.map", "r")
      lines = readlines(map)
      for line in lines
         write(dep,line*"\n")
      end
      close(map)
      write(dep,"</html>\n")
      close(dep)
   end

   function header()
      println("+---------------------------------------------------+")
      println("| Gerador da Arvore de dependencia de arquivos HTML |")
      println("|               INPE - CGCT - DIMNT                 |")
      println("+---------------------------------------------------+")   
   end

end