!*****************************************************************
!       read from CWC-RVOD product
!****************************************************************

        subroutine read_cloudsat(file_name)!,lwc,rew,lat,lon,&
                 !       height_s)
        use global
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
        
!        real(kind=4) :: lon(NROW),lat(NROW)
!        integer (kind=2) :: height_s(NCOL_S,NROW),lwc(NCOL_S,NROW),&
!                        rew(NCOL_S,NROW)
!        real :: TAI_start(1)
         allocate(height_s(NCOL_S,NROW))
         allocate(lwc(NCOL_S,NROW))
         allocate(rew(NCOL_S,NROW))
         allocate(lon(NROW))
         allocate(lat(NROW)) 
	!======= choose the file and field to read
	swathname = "2B-CWC-RVOD"
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
	status = swfldinfo(swid,'RVOD_liq_water_content',rank,&
                dim_sizes,datatype,n_attrs)
	if (status .ne. 0) then
		print *,'CANNOT GET FIELD INFO',status
		stop
	endif

!	print *,'-------------------------------------------------'
!	print *,'datatype: ',datatype
!        print *, 'rank',rank
!        print *,'size',dim_sizes
!        print *,'attribution',n_attrs
        start(1:2) = 0
	stride(1:2) = 1
	!======= read the field
        
        status = swrdfld(swid,'Height',start,stride,dim_sizes(1:rank),&
                        height_s)
	status = swrdfld(swid,'Latitude',start,stride,dim_sizes(2:2),lat)
	status = swrdfld(swid,'Longitude',start,stride,dim_sizes(2:2),lon)
!	status = swrdfld(swid,'Profile_time',start,stride,dim_sizes(2:2),time)
        status = swrdfld(swid,'RVOD_liq_water_content',start,stride,&
                        dim_sizes(1:rank),lwc)
        status = swrdfld(swid,'RVOD_liq_effective_radius',start,stride,&
                        dim_sizes(1:rank),rew)
!        start(1)=0
!        stride(1)=1

!	status = swrdfld(swid,'TAI_start',start,stride,1,TAI_start)
!        print *, status,TAI_start
!        stop
	!======= detach from the swath
	status = swdetach(swid)
	if (status .ne. 0) then
		print *,'CANNOT DETACH FROM SWATH',status
		stop
	endif
 
	!======= close the file
	status = swclose(fid)
	if (status .ne. 0) then
		print *,'CANNOT CLOSE FILE',status
		stop
	endif

        
        end
