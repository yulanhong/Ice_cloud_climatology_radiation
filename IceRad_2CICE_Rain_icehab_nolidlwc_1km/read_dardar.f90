        subroutine read_dardar(file_name)!,iwc,rei,mask,&
        !        DN_flag,time)

        use global
 
        implicit none

!        integer :: NROW,NCOL_S
!        common/file_info/ NROW,NCOL_S
 
! Parameter declaration
!        integer,parameter :: NCOL_D=436
!        real(kind=4) ::  iwc(NCOL_D,NROW),rei(NCOL_D,NROW),time(NROW)
!        integer(kind=2) :: mask(NCOL_D,NROW),DN_flag(NROW)  



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

        
        allocate(time(NROW))
!        allocate(iwc(NCOL_D,NROW))
!        allocate(rei(NCOL_D,NROW))
        allocate(mask(NCOL_D,NROW))
        allocate(DN_flag(NROW))

! ***************** End of variable declaration******************
! Open the file
        sd_id = sfstart(file_name, DFACC_READ)
        if (sd_id .eq. -1) then
                print *,'CANNOT OPEN DARDAR FILE'
                stop
        endif
! Get the index of the given data set SDS_NAME

        sds_name="day_night_flag"
        start(1)=0
        edges(1)=NROW
        STRIDE(1)=1
        sds_index=sfn2index(sd_id,sds_name)
        sds_id=sfselect(sd_id,sds_index)
        status=sfrdata(sds_id,start,stride,edges,DN_flag)
        status=sfendacc(sds_id)

        sds_name="time"
        sds_index=sfn2index(sd_id,sds_name)
        sds_id=sfselect(sd_id,sds_index)
        status=sfrdata(sds_id,start,stride,edges,time)
        status=sfendacc(sds_id)

        sds_name="iwc"
        sds_index = sfn2index(sd_id,sds_name)
        sds_id = sfselect(sd_id, sds_index)
 
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
!        status = sfrdata(sds_id, start, stride, edges, iwc)
!        print *, status 
!        i=311  ! row index
!        j=174  ! col index
!        print*, 'iwc',iwc(j,i)
 
        status = sfendacc(sds_id)
!************************* read ice effective radius ***************** 
!        sds_name="effective_radius"
!        sds_index = sfn2index(sd_id,sds_name)
!        sds_id = sfselect(sd_id, sds_index)
 
!        status = sfrdata(sds_id, start, stride, edges, rei)
!        status = sfendacc(sds_id)

!      i=311  ! row index
!      j=174  ! col index
!      print*, 're',rei(j,i)
!************************read cloud mask****************************
        sds_name="DARMASK_Simplified_Categorization"  
        sds_index = sfn2index(sd_id,sds_name)
        sds_id = sfselect(sd_id, sds_index)
        status = sfrdata(sds_id, start, stride, edges, mask)
        status = sfendacc(sds_id)
!      i=311  ! row index
!      j=174  ! col index
!      print*, 'mask',mask(j,i)
        status=sfend(sd_id)
        if (status .ne. 0) then 
                print *,'CANNOT CLOSE DARDAR FILE'
                stop
        endif 
        end
