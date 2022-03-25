import glob
import os
import sys 

def verifica_keywords(line):
	ls = line.split()
	if len(ls)>0:
		if ls[0].isdigit():
			return True
	if line[0:3]=="do ":
		return True
	elif line[0:5]=="enddo": 
		return True
	elif line[0:6]=="end do":
		return True
	else: 
		return False

def busca_end(li,label,do_data):
	aninhado = 0
	max_aninhamento=0
	for do in do_data:
		if do[0]<=li:
			continue
		if do[1]==label:
			return do[0],max_aninhamento
		if do[1]=="do":
			aninhado = aninhado+1
			max_aninhamento=max(max_aninhamento,aninhado)
	return 0,0

def busca_end_2(li,do_data):
	aninhado = 0
	max_aninhamento=0
	for do in do_data:
		if do[0]<=li:
			continue
		if do[1]=="do" and do[2]=='':
			aninhado = aninhado+1
			max_aninhamento=max(max_aninhamento,aninhado)
		if do[1]=="end" and aninhado==0:
			return do[0],max_aninhamento
		if do[1]=="end" and aninhado>0:
			aninhado = aninhado-1

	return 0,0

sources_files = glob.glob("*.f90")

for file in sources_files:
	
	fn = open(file,"r")
	lines = fn.readlines()
	fn.close()

	fo = open(file+".do","w")
	
	do_data = []

	line_count = 0
	for line in lines:
		line_count = line_count+1
		line = line.strip().lower()
		if verifica_keywords(line):
			if line[0]!="!":
				ls = line.split()
				if len(ls)>1:
					if ls[1].isdigit():	
						label = ls[1]
						line = ls[0]
					else:
						label = ''
						line = ls[0]
				do_data.append([line_count,line,label])
				fo.write(line+":"+label+":{0:d}\n".format(line_count))
	fo.close()

	#Tratando dos laços com continues numéricos
	print("do com label=================================")
	for do in do_data:
		label = do[2]
		if len(label)>0:
			linha_inicial = do[0]
			linha_final,max_aninhamento = busca_end(linha_inicial+1,label,do_data)
			print(file,linha_inicial,linha_final,int(linha_final)-int(linha_inicial),max_aninhamento,"\n")
		#print(label,len(label),do)
	print("do-enddo=================================")
	#Tratando dos laços com do-enddo
	for do in do_data:
		label = do[2]
		if len(label)==0 and do[1]=="do":
			linha_inicial = do[0]
			linha_final,max_aninhamento = busca_end_2(linha_inicial+1,do_data)
			print(file,linha_inicial,linha_final,int(linha_final)-int(linha_inicial),max_aninhamento,"\n")
