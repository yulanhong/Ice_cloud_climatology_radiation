        subroutine read_aerosol(file_name)

        use global, only : NROW,aero_ext,aero_ssa,aero_asy,lidlwc,&
                DN_flag
 
        implicit none

        integer DFACC_READ, DFNT_INT32
        parameter(DFACC_READ = 1, DFNT_INT32 = 24)
 
! Function declaration
        integer sfstart, sfn2index, sfselect, sfginfo, sfrdata
        integer sfendacc, sfend
 
! ************************** Variable declaration
        character*100 sds_name
        character*100 file_name
        integer values(8)  
        integer sd_id, sds_id, sds_index,astat, status
        integer rank, data_type, n_attrs
        integer dim_sizes(32), start(32), edges(32), stride(32)
        integer nrows, ncols, i, j, k

        allocate(aero_ssa(125,NROW))
        allocate(aero_asy(125,NROW))
        allocate(aero_ext(125,NROW))
        allocate(lidlwc(125,NROW))
        allocate(DN_flag(NROW))

! ***************** End of variable declaration******************
! Open the file
        sd_id = sfstart(file_name, DFACC_READ)
        if (sd_id .eq. -1) then
                print *,'CANNOT OPEN AERO FILE'
                 call system("mail -s 'job' yulanh@illinois.edu < &
                        message1")
                stop
        endif
! Get the index of the given data set SDS_NAME
        start(1) = 0  ! index of first row to read 
        edges(1) =NROW ! the number of rows to read
        stride(1) = 1 ! to read entire data

        sds_name="day_night_flag"
        sds_index=sfn2index(sd_id,sds_name)
        sds_id=sfselect(sd_id,sds_index)
        status = sfrdata(sds_id, start, stride, edges,DN_flag)
        status = sfendacc(sds_id)
        !print *,DN_flag(1:10)
        
        sds_name="aero_ssa"
        sds_index=sfn2index(sd_id,sds_name)
        sds_id=sfselect(sd_id,sds_index)
! Get the name, rank, dimension sizes, data type and number of 
! attributes for a data set
      status = sfginfo(sds_id,sds_name,rank,dim_sizes,data_type,n_attrs)

! Define the location, pattern, and size of the data set
        start(1) = 0  ! index of first row to read 
        start(2) = 0  ! index of first column to read
        edges(1) = dim_sizes(1)    ! the number of cols to read
        edges(2) = dim_sizes(2)    ! the number of rows to read
        stride(1) = 1 ! to read entire data
        stride(2) = 1
! Read entire data into data array. The array stride (i.e. step)
! specifies 
! the reading pattern along each dimension. 
! The sfrdata routine reads numeric scientific data and sfrcdata reads 
! character scientific data
        status = sfrdata(sds_id, start, stride, edges,aero_ssa)
        status = sfendacc(sds_id)

        sds_name="aero_asy"
        sds_index=sfn2index(sd_id,sds_name)
        sds_id=sfselect(sd_id,sds_index)
        status = sfrdata(sds_id, start, stride, edges,aero_asy)
        status = sfendacc(sds_id)
       
        sds_name="aero_ext"
        sds_index = sfn2index(sd_id,sds_name)
        sds_id = sfselect(sd_id, sds_index)
        status = sfrdata(sds_id, start, stride, edges, aero_ext)
        status = sfendacc(sds_id)

        sds_name="lwc"
        sds_index = sfn2index(sd_id,sds_name)
        sds_id = sfselect(sd_id, sds_index)
        status = sfrdata(sds_id, start, stride, edges, lidlwc)
        status = sfendacc(sds_id)

		!print *,lidlwc(:,23627)

        status=sfend(sd_id)
        if (status .ne. 0) then 
                print *,'CANNOT CLOSE caliop FILE'
                stop
        endif 
       
        end
