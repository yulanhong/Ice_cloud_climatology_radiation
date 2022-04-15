        subroutine read_ecmwf(file_name)!,wv_sh,pres,temp_t,ozone3)

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
        
        allocate(wv_sh(NCOL_S,NROW))
        allocate(pres(NCOL_S,NROW))
        allocate(temp_t(NCOL_S,NROW))
        allocate(ozone3(NCOL_S,NROW))
        allocate(skin_T(NROW))

        !======= choose the file and field to read
        swathname = "ECMWF-AUX"

       !print *, file_name
        !======= open the file
        fid = swopen(file_name, DFACC_READ)
        if (fid .eq. -1) then
                print *,'CANNOT OPEN ecmwf FILE',fid
                 call system("mail -s 'job' yh12c@my.fsu.edu < &
                        message1")
                stop
        endif
!	print *,fid
 
	!======= attach to the swath
        swid = swattach(fid,swathname)
        if (swid .eq. -1) then
                print *,'CANNOT ATTACH TO SWATH(ECMWF)',swid
                stop
        endif
 
	!======= get field information
	status = swfldinfo(swid,'Temperature',rank,&
                dim_sizes,datatype,n_attrs)
        if (status .ne. 0) then
		print *,'CANNOT GET FIELD INFO in ecmwf',status
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
        status = swrdfld(swid,'Skin_temperature',start,stride,&
                        dim_sizes(2:2),skin_T)
        status = swrdfld(swid,'Temperature',start,stride,&
                        dim_sizes(1:rank),temp_t)
        status = swrdfld(swid,'Pressure',start,stride,&
                        dim_sizes(1:rank),pres)
        status = swrdfld(swid,'Specific_humidity',start,stride, &
                        dim_sizes(1:rank),wv_sh)
        
        status = swrdfld(swid,'Ozone',start,stride, &
                        dim_sizes(1:rank),ozone3)
        start(1)=0
        stride(1)=1
!	status = swrdfld(swid,'TAI_start',start,stride,1,TAI_start)
!        print *, status,TAI_start
!        stop
	!======= detach from the swath
	status = swdetach(swid)
	if (status .ne. 0) then
		print *,'CANNOT DETACH FROM SWATH in ecmwf',status
		stop
	endif
!        print *,pres(5,4),temp_t(5,4),wv_sh(5,4) 
	!======= close the file
	status = swclose(fid)
	if (status .ne. 0) then
		print *,'CANNOT CLOSE FILE in ecmwf',status
		stop
	endif
       
        end
