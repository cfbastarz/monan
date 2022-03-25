import glob
import os
import sys


def get_subnames(tlines):

	subnames = []
	for tline in tlines:
		#É subroutine?
		if tline.find("Subroutine")>0:
			# Pega o nome da subrotina
			partes0 = tline.split()
			partes1 = partes0[0].split()
			partes2 = partes1[0].split("_")
			subnames.append(partes2[1])
	
	return subnames

def get_var_in_sub(subname,tlines):

	subnames = []
	for tline in tlines:
		#É subroutine?
		if tline.find("Subroutine")>0:
			# Pega o nome da subrotina
			partes0 = tline.split()
			partes1 = partes0[0].split()
			partes2 = partes1[0].split("_")
			subnames.append(partes2[1])
	
	return subnames	


def get_funcnames(tlines):

	funcnames = []
	for tline in tlines:
		#É subroutine?
		if tline.find("Function")>0 and tline[0]=="_":
			# Pega o nome da subrotina
			partes0 = tline.split()
			partes1 = partes0[0].split()
			partes2 = partes1[0].split("_")
			funcnames.append(partes2[1])
	
	return funcnames	

def get_var_in_sub(subname,tlines):

	vars = []
	for tline in tlines:
		#busca a subrotina no nome da var
		if tline.find(subname+"_")==0:
			# Pega o nome da variável
			partes = tline.split("_")
			vars.append(partes[1])
	
	return vars

def get_other_vars(tlines,procedures):

	vars = []
	for tline in tlines:
		partes0 = tline.split("_")
		if len(partes0)>1:
			#print("!"+partes0[0]+"!","!"+partes0[1]+"!")
			if partes0[0] == "":
				continue
			nome = partes0[0]
			if nome in procedures:
				continue
			#print("Esse é novo",nome)
			vars.append(partes0[1])
	
	return vars

# pega a lista de arquivos f90
sources_files = glob.glob("*.f90")
#print(sources_files)

count = 0
fmod = open("modules.txt",'r')
mlines = fmod.readlines()
fmod.close

modules=[]

for mline in mlines:
	partes = mline.split()
	modules.append(partes[3])

print("mods: ",len(modules),modules)

fmod = open("subroutines.txt",'r')
mlines = fmod.readlines()
fmod.close

subroutines=[]

for mline in mlines:
	partes = mline.split()
	subroutines.append(partes[3])

print("subs: ",len(subroutines),subroutines)

fmod = open("functions.txt",'r')
mlines = fmod.readlines()
fmod.close

functions=[]

for mline in mlines:
	partes = mline.split()
	functions.append(partes[3])

print("Funcs: ",len(functions),functions)

sys.exit()

for file_name in sources_files:
	count = count+1
	nvar = 0
	svar = 0

	ftyp=open(file_name+'.type.txt','r')
	tlines = ftyp.readlines()
	ftyp.close()

	procedures = []

	subnames = get_subnames(tlines)
	print("S: ",file_name,subnames)

	for subname in subnames:
		procedures.append(subname)
		print("Subroutine: ",subname,len(subnames))
		vars = get_var_in_sub(subname,tlines)
		print("S: ", subname,vars)

	funcnames = get_funcnames(tlines)
	print("F: ",file_name,funcnames)

	for funcname in funcnames:
		procedures.append(funcname)
		print("Function: ",funcname,len(funcnames))
		vars = get_var_in_sub(funcname,tlines)
		print("F: ", funcname,vars)

	vars = get_other_vars(tlines,procedures)
	print(vars)

	continue
	fvar.close()

	vazio = 0
	comentario = 0
	file = open(file_name,'r')
	lines = file.readlines()
	file.close()
	for line in lines:
		#if line=="":
		#	vazio = vazio+1
		#	continue
		#print(len(line))
		#print("line=",'"'+line)
		#print("Prim=",'"'+primeiro_char)
		#print(len(primeiro_char))
		if len(line.strip())==0:
			vazio = vazio+1
			continue
		primeiro_char = line.strip()[0]
		linhaDeComentario = line.strip()[1:]
		#print (linhaDeComentario)
		#print(primeiro_char)
		if primeiro_char=="!" and len(linhaDeComentario)>0:
			comentario = comentario+1
	if len(lines)>0:
		if nvar==0:
			nvar=1
		print(file_name,len(lines),vazio,comentario,(comentario/len(lines))*100,svar/nvar)
		


