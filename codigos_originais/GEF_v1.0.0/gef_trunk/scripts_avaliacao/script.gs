*************************************************
* Gera mapas das variaveis de regioes do Globo  *
*            Luiz A. Candido                    *
*              Maio/2003                        *
*************************************************
_io=0
**pull argumentos
labi=subwrd(argumentos,1)
labp=subwrd(argumentos,2)
_nreg=subwrd(argumentos,3)
ndias=subwrd(argumentos,4)
nctl_icn=subwrd(argumentos,5)
nctl_fct=subwrd(argumentos,6)
nctl_fctold=subwrd(argumentos,7)
*************************************************
regioes='Pnor Psul Trop SAmr'
*nomectls='gposicn'labi'.ctl gposfct'labp'.ctl gposfct'labi'.ctl'
nomectls=nctl_icn' 'nctl_fctold' 'nctl_fct
**variaveis='pslp tsl(lev=1000) umrl(lev=925) umrl(lev=850) hgt(lev=500) prec'
variaveis='pslp tsl(lev=1000) hgt(lev=500) prec'
output='Pseanlm Tmp1000 Th5a10p Precipt'
factor1='1.0 1.0 100.0 100.0 1.0 1.0'
factor2='+0.0 -273.16 +0.0 +0.0 -zgeo(lev=1000) +0.0'
tempo1='2 6 10 14 18 22 26 30 34 38 42 46'
tempo2='5 9 13 17 21 25 29 33 37 41 45 49'
*'run rgb.gs'
regiao=subwrd(regioes,_nreg)
*
_lonW=-180;_lonE=180
if(_nreg=1)
_latS=30;_latN=90;_proj='nps';_polo='PN'
endif
if(_nreg=2)
_latS=-90;_latN=-30;_proj='sps';_polo='PS'
endif
if(_nreg=3)
_latS=-45;_latN=45;_proj='latlon';_polo=' '
endif
if(_nreg=4)
_lonW='-100';_lonE='-20';_latS='-60';_latN='15';_proj='latlon';_polo=' '
endif
*
nctl=1
while(nctl<=3)
ctl=subwrd(nomectls,nctl)
'open 'ctl
'q files'
nv=1
while(nv<=6)
if(nctl=1 & nv<6)
  nome=subwrd(variaveis,nv)
  nomeout=subwrd(output,nv)
  fac1=subwrd(factor1,nv)
  fac2=subwrd(factor2,nv)
  rotina=labtime(1)
  labf=_lab
  rotina=def(nome,fac1,fac2,var)
  rotina=varsgs(nv,labi,labf,var)
  figura=nomeout%regiao%labf
  rotina=gif(figura)
  'undefine var'
endif
if(nctl=2)
  nv=6
  'set t 2'
  nome=subwrd(variaveis,nv)
  nomeout=subwrd(output,nv)
  fac1=subwrd(factor1,nv)
  fac2=subwrd(factor2,nv)
  rotina=labtime(1)
  labf=_lab
  nome='ave(prec,t=2,t=3)'
  rotina=def(nome,fac1,fac2,var)
  rotina=varsgs(nv+1,labf,labi,var)   
  figura=nomeout%regiao%labi
  rotina=gif(figura)
  'undefine var'
endif 
if(nctl=3)
  nome=subwrd(variaveis,nv)
  nomeout=subwrd(output,nv)
  fac1=subwrd(factor1,nv)
  fac2=subwrd(factor2,nv)  
  k=1
  while(k<=ndias)
  tm1=subwrd(tempo1,k)
  tm2=subwrd(tempo2,k)
  'set t 'tm2
  rotina=labtime(tm2)
  labf=_lab
   if(nv=6)
     if(k<=8)
        nome='ave(prec,t='tm1',t='tm2')'
     else
        nome='(prec(t='tm1')+prec(t='tm2'))*0.5'
     endif
   endif
   rotina=def(nome,fac1,fac2,var)
   rotina=varsgs(nv,labi,labf,var)      
   figura=nomeout%regiao%labf
   rotina=gif(figura)
   'undefine var'
  k=k+1
  endwhile
endif
nv=nv+1
endwhile
'close 1'
nctl=nctl+1
endwhile

'quit'
*****************************************************
function varsgs(nv,labi,labf,var1)
'set display color white';'clear'
rotina=title(nv)
'set gxout '_preench
'set grads off'
'set lon '_lonW' '_lonE
'set lat '_latS' '_latN
'set mproj '_proj
'set mpvals '_lonW' '_lonE' '_latS' '_latN
'set map 1 1 5';'set mpdset mres'
'set clevs '_clvs
'set ccols '_cor
'set clab on'
if (_preench="contour")
'set cthick 6';'set cstyle 1'
endif
'display smth9('var1')'
if (_preench!="contour")
if(_nreg=1 | _nreg=2)
'run cbarn.gs 1.1 0 4.25 1.35'
endif
if(_nreg=3)
'run cbarn.gs 1.0 0 5.5 2.25'
endif
if(_nreg=4)
'run cbarn.gs 1.1 0 4.25 0.8'
endif
'set gxout contour';'set grads off'
'set lon '_lonW' '_lonE
'set lat '_latS' '_latN
'set mproj '_proj
'set mpvals '_lonW' '_lonE' '_latS' '_latN
'set map 1 1 5';'set mpdset mres'
'set clevs '_clvs
'set ccolor 1';'set clab off'
'display smth9('var1')'
endif
rotina=subtitle(labi,labf)
return
*********************************************************
function def(var,mult,sum,var1)
'set lon '_lonW' '_lonE
'set lat '_latS' '_latN
'define 'var1'='mult%'*'%var%sum
return
*********************************************************
function subtitle(labi,labf)
if(_nreg=1 | _nreg=2)
'set strsiz 0.15';'set string 1 bc 6'
'draw string 4.25 9.9 CPTEC/INPE/MCT - GLOBAL MODEL - T213L42'
if (labi=labf)
'draw string 4.25 9.6  ANALYSIS: 'labi
'draw string 4.25 9.3 '_title
else
'draw string 4.25 9.6  FORECAST FROM: 'labi'  VALID FOR: 'labf
'draw string 4.25 9.3 '_title
endif
'set string 1 bl 6'
'draw string 0.7 8.9  '_polo
endif
if(_nreg=3)
'set strsiz 0.15';'set string 1 bc 6'
'draw string 5.5 6.5 CPTEC/INPE/MCT - GLOBAL MODEL - T213L42'
if (labi=labf)
'draw string 5.5 6.2  ANALYSIS: 'labi
'draw string 5.5 5.9 '_title
else
'draw string 5.5 6.2  FORECAST FROM: 'labi'  VALID FOR: 'labf
'draw string 5.5 5.9 '_title
endif
endif
if(_nreg=4)
'query w2xy '%_lonW%' '%_latN
yt=subwrd(result,6)
'set strsiz 0.145';'set string 1 bc 6'
'draw string 4.25 'yt+0.7' CPTEC/INPE/MCT - GLOBAL MODEL - T213L42'
if (labi=labf)
'draw string 4.25 'yt+0.4' ANALYSIS: 'labi
'draw string 4.25 'yt+0.1' '_title
else
'draw string 4.25 'yt+0.4' FORECAST FROM: 'labi' VALID FOR: 'labf
'draw string 4.25 'yt+0.1' '_title
endif
endif
return
***********************************************************
function gif(figura)
_io=_io+1
'printim 'figura'.png png'
*'!/usr/bin/convert -crop 0x0 'figura'.png 'figura'.png'
'set grads on'
return
***********************************************************
function labtime(tin)
'set t 'tin'';'query dims'
ltime=sublin(result,5);time=subwrd(ltime,6);yy=substr(time,9,4);mh=substr(time,6,3)
dd=substr(time,4,2);hh=substr(time,1,2)
frase1='JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC'
frase2='jan feb mar apr may jun jul aug sep oct nov dec'
frase3='01 02 03 04 05 06 07 08 09 10 11 12'
n=1
while(n<=12)
mes1=subwrd(frase1,n);mes2=subwrd(frase2,n)
if (mh=mes1 | mh=mes2)
mm=subwrd(frase3,n)
endif
n=n+1
endwhile
_lab=yy%mm%dd%hh
return
*****************************************************
function title(m)
if(m=1)
_title='MSL PRESSURE (hPa)'
_preench='contour'
_clvs='940 945 950 955 960 965 970 975 980 985 990'
_clvs=_clvs%' 994 996 998 1000 1002 1004 1006 1008 1010 1012 1014 1016 1018 1020 1022 1024 1026'
_clvs=_clvs%' 1030 1035 1040 1045 1050 1055 1060'
_cor='16 17 18 19 20 21 22 23 24 25 26'
_cor=_cor%' 27 28 29 1 30 31 32 33 34 35 36 37 38 39 40 41 42'
_cor=_cor%' 43 44 45 46 47 48 49'
endif
if(m=2)
_title='TEMPERATURE at 1000 hPa (`aO`nC)'
_clvs='-45 -30 -15 -10 -6 -2 0 2 4 6 8 10 12 14 16 18 20 22 24 26 28'
_cor='64 63 62 61 60 59 58 57 56 55 54 53 52 51 50 0 65 66 67 68 69 70'
if(_nreg = 3 | _nreg = 4)
_clvs='0 2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36'
_cor='59 58 57 56 55 54 53 52 51 50 65 66 67 68 69 70 71 72 73 74'
endif
_preench='shaded'
endif
if(m=3)
_title='RELATIVE HUMIDITY at 925 hPa (%)'
_clvs='5 10 20 30 40 60 70 80 90 95'
_cor='96 94 93 91 90 89 88 87 86 85 83'
*teste
_title='RELATIVE HUMIDITY at 925 hPa (%)'
_clvs='10 20 30 40 60 70 80 90'
_cor='69 67 65 17 18 16 50 55 58'
*_cor='58 55 50 18 16 17 68 70 72'
endif
if(m=4)
_title='RELATIVE HUMIDITY at 850 hPa (%)'
_clvs='5 10 20 30 40 60 70 80 90 95'
_cor='96 94 93 91 90 89 88 87 86 85 83'
*teste
_title='RELATIVE HUMIDITY at 850 hPa (%)'
_clvs='10 20 30 40 60 70 80 90'
_cor='69 67 65 17 18 16 50 55 58'
*_cor='58 55 50 18 16 17 68 70 72'
endif
if(m=5)
_title='500-1000 hPa THICKNESS (gpm)'
_clvs='4960 5080 5200 5320 5380 5440 5500 5560 5590 5620 5650 5680 5710'
_cor='0 50 51 52 53 54 55 0 56 57 58 59 60 61'
if(_nreg = 3 | _nreg = 4)
_clvs='5320 5380 5440 5500 5560 5590 5620 5650 5680 5710 5740 5770 5800'
_cor='51 52 53 54 55 56 57 58 59 60 61 62 63 64'
endif
endif
if(m=6)
_title='TOTAL PREC (mm/day - Ac. last 24h)'
_clvs='3 5 7 10 15 20 30 40 50 60 70 80'
_cor='0 14 4 11 5 13 3 10 7 12 8 2 6'
endif
if(m=7)
_title='TOTAL PREC (mm/day - Ac. last 12h)'
_clvs='3 5 7 10 15 20 30 40 50 60 70 80'
_cor='0 14 4 11 5 13 3 10 7 12 8 2 6'
endif
return
