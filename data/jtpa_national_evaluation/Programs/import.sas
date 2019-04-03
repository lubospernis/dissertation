***************************************************************
** IMPORT.SAS imports a SAS data set that was written in SAS **
** export format.                                            **
***************************************************************;

libname outdir "device_name:[directory_name]";
libname file1 xport "device_name:[directory_name]export_file_name.xpt";
proc copy in=file1 out=outdir;
run;
