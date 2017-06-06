PUBLIC gcpath, lcPath, vKodeSuara, vOrderBy, vSortBy

vAgenda    = ' '
vKodeSuara = ' '
vOrderBy   = ' '
vSortBy    = ' '

&& Path Production
&& gcPath = 'C:\AppRUPS'

&& Path Development
gcPath = 'D:\BACKUP_DATA\APLIKASI\DEVELOPMENT\AppRUPS'

SET DEFAULT TO &gcPath

lcPath = gcPath+"\Forms,"+gcPath+"\Programs,"+gcPath+"\Class,"+gcPath+"\Images,"+;
         gcPath+"\Reports,"+gcPath+"\Data,"+"%Path%"
         
SET PATH TO &lcPath

