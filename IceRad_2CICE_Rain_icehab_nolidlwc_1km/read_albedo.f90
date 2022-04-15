        subroutine read_albedo()
        implicit none
        
        integer, parameter :: Xlon=720,Ylat=360,Nday=23
        real(kind=4) :: albedo(Xlon,Ylat),albedo_mon(Xlon,Ylat,Nday)        
        character(len=100) :: fname,SDS_NAME
        character(len=3) :: findex(Nday)

        integer :: sd_id, sds_id, sds_index,astat, status
        integer :: rank, data_type, n_attrs
        integer :: dim_sizes(2), start(2), edges(2), stride(2)

        integer :: sfstart, sfn2index, sfselect, sfginfo, sfrdata
        integer :: sfendacc, sfend
        integer,parameter:: DFACC_READ=1,DFNT_FLOAT32=5
        integer :: fi
       
        common /albedo/ albedo_mon
 
        findex=['001','017','033','049','065','081','097','113','129',&
                '145','161','177','193','209','225','241','257','273',&
                '289','305','321','337','353']

        DO fi=1,Nday

        fname="/sat/yhong/albedo/modis/landsea_albedo/albedo_landsea_"//&
                 findex(fi)//".hdf"
       
        sd_id=sfstart(fname,DFACC_READ)
        if (sd_id .eq. -1) then
                print *,'CANNOT OPEN DARDAR FILE'
                stop
        endif

        SDS_NAME="albedo"
        sds_index=sfn2index(sd_id,SDS_name)
        sds_id=sfselect(sd_id,sds_index)

        status = sfginfo(sds_id,sds_name,rank,dim_sizes,data_type,&
                n_attrs)

        start(1) = 0  ! index of first row to read 
        start(2) = 0  ! index of first column to read
        edges(1) = dim_sizes(1)    ! the number of cols to read
        edges(2) = dim_sizes(2)    ! the number of rows to read
        stride(1) = 1 ! to read entire data
        stride(2) = 1
        
        status = sfrdata(sds_id, start, stride, edges, albedo)
!    print *,albedo(1,45:60)

        status=sfendacc(sds_id)
        status=sfend(sd_id)

        albedo_mon(:,:,fi)=albedo

        ENDDO
        
        end subroutine read_albedo
