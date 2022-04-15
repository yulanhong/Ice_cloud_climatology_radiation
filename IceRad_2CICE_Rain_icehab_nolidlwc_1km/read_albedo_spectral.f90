        subroutine read_albedo_spectral()

	use global,only : N_wave
        implicit none
       
        integer, parameter :: Xlon=720,Ylat=360,Nday=23
        integer(kind=2) :: albedo(Xlon,Ylat),&
                        albedo_mon(Xlon,Ylat,Nday,N_wave)        
        character(len=100) :: fname,SDS_NAME
        character(len=3) :: findex(Nday)
        character (len=5) :: wave(N_wave)

        integer :: sd_id, sds_id, sds_index,astat, status
        integer :: rank, data_type, n_attrs
        integer :: dim_sizes(2), start(2), edges(2), stride(2)

        integer :: sfstart, sfn2index, sfselect, sfginfo, sfrdata
        integer :: sfendacc, sfend
        integer,parameter:: DFACC_READ=1,DFNT_FLOAT32=5
        integer :: fi,wi
       
        common /albedo/ albedo_mon
 
        findex=['001','017','033','049','065','081','097','113','129',&
                '145','161','177','193','209','225','241','257','273',&
                '289','305','321','337','353']
        wave=['0.47 ','0.555','0.659','0.858','1.24 ','1.64 ','2.13 ']

        DO wi=1,N_wave
        DO fi=1,Nday

        fname=&
        "/data/gdi/b/yulanh/albedo/albedo/landsea_albedo_"//&
         trim(wave(wi))//&
        "/albedo_landsea_"//trim(wave(wi))//"_"//findex(fi)//".hdf"
        sd_id=sfstart(fname,DFACC_READ)
        if (sd_id .eq. -1) then
                print *,'CANNOT OPEN ALBEDO  FILE',fname
                stop
        endif

        SDS_NAME="albedo_"//trim(wave(wi))
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
!     print *,fname,albedo(1,45:60)
	
        status=sfendacc(sds_id)
        status=sfend(sd_id)
        albedo_mon(:,:,fi,wi)=albedo
       
        ENDDO
        ENDDO 
        end subroutine read_albedo_spectral
