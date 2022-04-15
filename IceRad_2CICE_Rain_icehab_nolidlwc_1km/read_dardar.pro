pro read_dardar,fname,fieldname,data
;+ to read dardar format data
;fname refers to the name of file; number is the number of field in the file
;fname=file_search('/smb/microwave2/dardar/2007/2007_01_01/' ,'*.hdf')

	fid=hdf_sd_start(fname,/read)

	sds_index = HDF_SD_NAMETOINDEX(fid,fieldname)

	nid=hdf_sd_select(fid,sds_index)

	hdf_sd_getdata,nid,data

	hdf_sd_endaccess,nid

	hdf_sd_end,fid

end
