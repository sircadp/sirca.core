Func Bulan(Bln,Ln)
*--------------------------------------------------*
* Bln : Nilai Bulan yang diconversi                *
* Ln  : 0-Nama Bulan Panjang, 1-Nama Bulan Pendek  *
*  		2-Konversi Jadi Anggka Romawi 			   *
*--------------------------------------------------* 
cBulan=Spac(9)
DIMENSION B1(12)
DIMENSION B2(12)
DIMENSION B3(12)
B1(1) ='Januari'
B1(2) ='Pebruari'
B1(3) ='Maret'
B1(4) ='April'
B1(5) ='Mei'
B1(6) ='Juni'
B1(7) ='Juli'
B1(8) ='Agustus'
B1(9) ='September'
B1(10)='Oktober'
B1(11)='Nopember'
B1(12)='Desember'
B2(1) ='Jan'
B2(2) ='Peb'
B2(3) ='Mar'
B2(4) ='Apr'
B2(5) ='Mei'
B2(6) ='Jun'
B2(7) ='Jul'
B2(8) ='Agt'
B2(9) ='Sep'
B2(10)='Okt'
B2(11)='Nop'
B2(12)='Des'
B3(1) ='I'
B3(2) ='II'
B3(3) ='III'
B3(4) ='IV'
B3(5) ='V'
B3(6) ='VI'
B3(7) ='VII'
B3(8) ='VIII'
B3(9) ='IX'
B3(10)='X'
B3(11)='XI'
B3(12)='XII'
IF 	Ln = 1
	cBulan = B2[Bln]
ELSE
	IF 	Ln = 2
		cBulan = B3[Bln]
	ELSE
		cBulan = B1[Bln]
	ENDIF
ENDIF
RETURN cBulan