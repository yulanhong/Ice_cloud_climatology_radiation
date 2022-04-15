        subroutine inquire_info(file_name)

        use global
        implicit none
        integer :: fid,swid,status
        integer :: swopen,swattach,swdetach,swclose,swfldinfo
        integer :: rank,dim_sizes(7),datatype
        character(len=255) :: n_attrs
        character(len=120) :: file_name,swathname
        integer,parameter :: DFACC_READ=1

        swathname = "ECMWF-AUX"

!        print *, file_name
        !======= open the file
        fid = swopen(file_name, DFACC_READ)
        if (fid .eq. -1) then
                print *,'CANNOT OPEN ecmwf FILE',fid
                stop
        endif
!       print *,fid

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

        NCOL_S = dim_sizes(1)
        NROW   = dim_sizes(2)

!	print *,NCOL_S, NROW
	
        status=swdetach(swid)

        status = swclose(fid)

        if (status .ne. 0) then
                print *,'CANNOT CLOSE FILE in ecmwf',status
                stop
        endif

        end 
