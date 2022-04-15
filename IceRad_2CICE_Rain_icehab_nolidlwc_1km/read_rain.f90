        subroutine read_rain(file_name)!,lwc,rew,lat,lon,&
                 !       height_s)

        use global,only:rwc,NCOL_S,NROW
	implicit none

	! declarations
	integer (kind=4) :: fid,swid,status,astat
	integer (kind=4) :: swopen,swattach,swdetach,swclose
	integer (kind=4) :: swfldinfo,swrdfld,swrdattr
	integer (kind=4) :: rank,dim_sizes(7),datatype,i,j
	character (len=255) :: n_attrs
        integer (kind=4) :: start(32),stride(32),edges(32)

	character (len=120) :: file_name,swathname,fieldname
        ! declarations for fields to read
        integer,parameter :: DFACC_READ=1
        
         allocate(rwc(NCOL_S,NROW))
	!======= choose the file and field to read
	swathname = "2C-RAIN-PROFILE"
	!======= open the file
	fid = swopen(file_name, DFACC_READ)
	if (fid .eq. -1) then
		print *,'CANNOT OPEN CWC FILE',fid
		stop
	endif
!	print *,fid
 
	!======= attach to the swath
	swid = swattach(fid,swathname)
	if (swid .eq. -1) then
		print *,'CANNOT ATTACH TO SWATH',swid
		stop
	endif
 
	!======= get field information
	status = swfldinfo(swid,'precip_liquid_water',rank,&
                dim_sizes,datatype,n_attrs)
	if (status .ne. 0) then
		print *,'CANNOT GET FIELD INFO',status
		stop
	endif

        start(1:2) = 0
	stride(1:2) = 1
	!======= read the field
        
        status = swrdfld(swid,'precip_liquid_water',start,stride,&
                        dim_sizes(1:rank),rwc)
	!======= detach from the swath===========================
	status = swdetach(swid)
	if (status .ne. 0) then
		print *,'CANNOT DETACH FROM SWATH',status
		stop
	endif
        !print *,rwc(99:101,13360) 
	!======= close the file==================================
	status = swclose(fid)
	if (status .ne. 0) then
		print *,'CANNOT CLOSE FILE',status
		stop
	endif

        end
