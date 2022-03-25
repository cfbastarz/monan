#************ glob acc prec, mm/day, GLOBAL-CPTEC *******************************
'open /stornext/online7/pnt/oper/tempo/oens_MCGA/TQ0126L028/201702/2700/pos/ctrl/GPOSNMC2017022700.ctl'

'c'

'set clevs 2 3 4 5 6 7 8 9 10 11'
'set ccols 0 14 4 11 5 13 3 7 12 8 2'

'set grads off'
'set gxout shaded'
'd ave(prec,t=1,t=40)'
'cbarn'
'draw title Acc. prec, mm/day, 27 FEB 2017, GLOBAL T126'
'printim prec-feb2017-global.png x1000 y800 white'


#************ glob acc prec, mm/day, GEF *********************************
'reinit'
'open /scratchout/grupos/eta/home/reta/tempo/GEF_tempo/GLOBRUN/2017022700/globrun.ctl'

'c'

'set clevs 2 3 4 5 6 7 8 9 10 11'
'set ccols 0 14 4 11 5 13 3 7 12 8 2'

'set gxout shaded'
'set grads off'
'd ave(acprec*1000,t=1,t=40)'
'cbarn'
'draw title Acc. prec, mm/day, 27 FEB 2017, GEF'
'printim prec-feb2017-GEF.png x1000 y800 white'
'quit'

#************ glob acc prec, mm/day, CFSR *******************************
'reinit'
'open /stornext/online1/eta/reta/Precipitacao/prec.ctl'

'set clevs 2 3 4 5 6 7 8 9 10 11'
'set ccols 0 14 4 11 5 13 3 7 12 8 2'

'set grads off'
'set gxout shaded'
'd ave(prec,t=7,t=16)*4'
'cbarn'
'draw title Acc. prec, mm/day, FEB 2017, CFSR'
'printim NEW_acc-prec-feb17-CFSR.png x1000 y800 white'
'quit'

#**************************************************************************
