SET TALK OFF
SET STATUS OFF
SET CENTURY on
SET DATE ITALIAN 
SET DELETED ON
SET STATUS BAR OFF
SET HELP OFF

PUBLIC gcpath, ConDB, gcUser, lcPath, xPath, gcDSX, vConect, vSetApp, vUserName, xInfo, cSH,;
	   vKodeEmit, xKodeEmit, vTahun, vBulan, xThn, xBln, xTgl, vOrderBy, vSortBy, vAgenda, _Reg_ID, _DPS_ID,;
	   vShmHdrConflic, vShmHdrIndependent, vPembagi, vJmlShmHadir, vJmlShmKuasa, vNamaKuasa,;
	   vKodeSuara, vNoledger, vKodeReg, vKodeFlag, vBalance, vIsLockConflictMenu, vWifiBarcode,;
	   vBarcode, vJmlBarcode, vJmlAgenda, vSubAgenda, vJmlSubAgenda, vRefAgenda, iNoAgenda, iJmlSubAgenda,;
	   vLockData, vAutoPrint, vNamaSH, vJmlShm, vDirkom, vRePrint, xScreen_X_Awal, xScreen_Y_Awal,;
	   vJudul, vJnsRups, vEmiten, vTglRup, vExcludeNoLedger, vTotalDPS, vDependent, vIndependent, vPembagi_Vote,;
	   xNamaEmiten, xUraian, xTitle, xJamAgenda, xMntAgenda, vJnsRUPS

vKodeSuara     = ' '
vOrderBy 	   = ' '
vSortBy  	   = ' '
vJmlShmKuasa   = 0
vShmHdrConflic = 0
vJmlBarcode	   = 0
vAgenda		   = 0	
vJmlAgenda     = 0
vSubAgenda     = 0
vJmlSubAgenda  = 0
vRefAgenda     = 0
vRePrint	   = 0
vTotalDPS	   = 0
vDependent	   = 0
vIndependent   = 0
vPembagi	   = 0	 
vPembagi_Vote  = 0

clayar = CREATEOBJECT("layar.tlayar")
xScreen_X_Akhir = clayar.ResolusiLayar(0,1)
xScreen_Y_Akhir = clayar.ResolusiLayar(0,2)

gcPath = SYS(2003)

SET DEFAULT TO &gcPath

lcPath = gcPath+"\Forms,"+gcPath+"\Programs,"+gcPath+"\Class,"+gcPath+"\Images,"+;
         gcPath+"\Reports,"+gcPath+"\Data,"
         
SET PATH TO &lcPath

Application.Visible = .F.
DO FORM Login
Read EVENTS
Application.Visible = .T.