FUNCTION ConvBln (Bln) as String  

IF Bln = 1 THEN 
	NmBulan = 'Januari'
ENDIF 
IF Bln = 2 THEN 
	NmBulan = 'Pebruari'
ENDIF 
IF Bln = 3 THEN 
	NmBulan = 'Maret'
ENDIF 
IF Bln = 4 THEN 
	NmBulan = 'April'
ENDIF 
IF Bln = 5 THEN 
	NmBulan = 'M e i'
ENDIF 
IF Bln = 6 THEN 
	NmBulan = 'Juni'
ENDIF 
IF Bln = 7 THEN 
	NmBulan = 'Juli'
ENDIF 
IF Bln = 8 THEN 
	NmBulan = 'Agustus'
ENDIF 
IF Bln = 9 THEN 
	NmBulan = 'September'
ENDIF 
IF Bln = 10 THEN  
	NmBulan = 'Oktober'
ENDIF 
IF Bln = 11 THEN 
	NmBulan = 'Nopember'
ENDIF 
IF Bln = 12 THEN 
	NmBulan = 'Desember'
ENDIF 

RETURN NmBulan	