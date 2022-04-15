        subroutine read_2cice(file_name)!,wv_sh,pres,temp_t,ozone3)

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
               

!        real (kind=4) :: wv_sh(NCOL_S,NROW),pres(NCOL_S,NROW),&
!                        temp_t(NCOL_S,NROW),ozone3(NCOL_S,NROW)
        allocate(iwc(NCOL_S,NROW))
        allocate(rei(NCOL_S,NROW))
        allocate(vis_od(NROW))

	!======= choose the file and field to read
	swathname = "2C-ICE"

!        print *, file_name
	!======= open the file
	fid = swopen(file_name, DFACC_READ)
	if (fid .eq. -1) then
		print *,'CANNOT OPEN 2cice FILE',fid
		stop
	endif
!	print *,fid
 
	!======= attach to the swath
	swid = swattach(fid,swathname)
	if (swid .eq. -1) then
		print *,'CANNOT ATTACH TO SWATH(2cice)',swid
		stop
	endif
 
	!======= get field information===========================
        status=swfldinfo(swid,'optical_depth',rank,&
                dim_sizes,datatype,n_attrs)
        IF (status .ne. 0) then 
                print *,'cannot get vis_od field in 2cice'
                stop
        ENDIF
        start(1)=0
        stride(1)=1
        status = swrdfld(swid,'optical_depth',start,stride,&
                        dim_sizes(1:rank),vis_od)
	

        status = swfldinfo(swid,'re',rank,&
                dim_sizes,datatype,n_attrs)
	if (status .ne. 0) then
		print *,'CANNOT GET FIELD INFO in 2cice',status
		stop
	endif

        start(1:2) = 0
	stride(1:2) = 1
	!======= read the field
        
        status = swrdfld(swid,'re',start,stride,&
                        dim_sizes(1:rank),rei)
        status = swrdfld(swid,'IWC',start,stride,&
                        dim_sizes(1:rank),iwc)

	!======= detach from the swath
	status = swdetach(swid)
	if (status .ne. 0) then
		print *,'CANNOT DETACH FROM SWATH in 2cice',status
		stop
	endif
!        print *,pres(5,4),temp_t(5,4),wv_sh(5,4) 
	!======= close the file
	status = swclose(fid)
	if (status .ne. 0) then
		print *,'CANNOT CLOSE FILE in 2cice',status
		stop
	endif

        end
