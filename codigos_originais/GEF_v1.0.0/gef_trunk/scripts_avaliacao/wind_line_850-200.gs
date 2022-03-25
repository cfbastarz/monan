'open /stornext/online7/pnt/oper/tempo/oens_MCGA/TQ0126L028/201702/2700/pos/ctrl/GPOSNMC2017022700.ctl'
'open /scratchout/grupos/eta/home/reta/tempo/GEF_tempo/GLOBRUN/2017022700/globrun.ctl'
'open /stornext/online1/eta/reta/Vento_Temp_2/temp_vento.ctl'

#************850mb************************

'c'

'set dfile 1'
'set time 00z27feb2017 18z08mar2017'
'set lat -90 90'
'set lon 50'
'set lev 850'
'set vrange 5 10'
'set grads off'
'set cthick 6'
'set ccolor 2'
'set cmark 3'
'd tloop(aave(mag(uvel,vvel),g))'


'set dfile 2'
'set time 00z27feb2017 18z08mar2017'
'set lat -90 90'
'set lon 50'
'set lev 850'
'set grads off'
'set cthick 6'
'set ccolor 3'
'set cmark 3'
'd tloop(aave(mag(usp,vsp),g))'


'set dfile 3'
'set time 00z27feb2017 18z08mar2017'
'set lat -90 90'
'set lon 50'
'set lev 850'
'set grads off'
'set cthick 6'
'set ccolor 4'
'set cmark 3'
'd tloop(aave(mag(ugrdprs,vgrdprs),g))'


'run cbar_line -x 8.2 -y 2.5 -c 2 3 4 -l 1 1 1 -m 3 3 3 -t  "GLOBAL" "GEF" "CFSR"' 
'draw title Wind 850mb, FEB 2017'
'printim wind-850mb-feb2017.png x1000 y800 white'


#************200mb************************
'reinit'
'open /stornext/online7/pnt/oper/tempo/oens_MCGA/TQ0126L028/201702/2700/pos/ctrl/GPOSNMC2017022700.ctl'
'open /scratchout/grupos/eta/home/reta/tempo/GEF_tempo/GLOBRUN/2017022700/globrun.ctl'
'open /stornext/online1/eta/reta/Vento_Temp_2/temp_vento.ctl'

'c'

'set dfile 1'
'set time 00z27feb2017 18z08mar2017'
'set lat -90 90'
'set lon 50'
'set lev 200'
'set vrange 15 25'
'set grads off'
'set cthick 6'
'set ccolor 2'
'set cmark 3'
'd tloop(aave(mag(uvel,vvel),g))'


'set dfile 2'
'set time 00z27feb2017 18z08mar2017'
'set lat -90 90'
'set lon 50'
'set lev 200'
'set grads off'
'set cthick 6'
'set ccolor 3'
'set cmark 3'
'd tloop(aave(mag(usp,vsp),g))'


'set dfile 3'
'set time 00z27feb2017 18z08mar2017'
'set lat -90 90'
'set lon 50'
'set lev 200'
'set grads off'
'set cthick 6'
'set ccolor 4'
'set cmark 3'
'd tloop(aave(mag(ugrdprs,vgrdprs),g))'


'run cbar_line -x 8.2 -y 2.5 -c 2 3 4 -l 1 1 1 -m 3 3 3 -t  "GLOBAL" "GEF" "CFSR"' 
'draw title Wind 200mb, FEB 2017'
'printim wind-200mb-feb2017.png x1000 y800 white'

'quit'
