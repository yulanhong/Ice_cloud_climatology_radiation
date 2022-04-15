        subroutine write_hdf (FILE_NAME)

        use global
        
        implicit none
 
!******* Function declaration.**************************************
!
        integer sfstart,sfcreate, sfsnatt, sfwdata, sfendacc, sfend, &
                sfsattr
 
!**** Variable declaration *******************************************
!
        character(len=120) ::  FILE_NAME
        character(len=20) :: SDS_NAME
         

        integer(kind=1) :: cf_hgt(NM_hgt)

        integer  ::     RANK
        integer :: parameter,DFACC_CREATE=4,DFNT_INT16=22,&
                DFNT_FLOAT32=5,DFNT_CHAR8=4,DFNT_FLOAT=5,DFNT_INT8=21
        integer :: fid, sds_id, status
        integer :: start ( 3 ), edges ( 3 ), stride ( 3 ),dim_sizes(3)
        integer :: i, j
        real :: miss_value,offset,factor,datarange(2)
!
        allocate(wr_pres(NM_hgt,NROW))
        allocate(wr_FU_swa(NM_hgt,NROW,N_cloud))
        allocate(wr_FU_lwa(NM_hgt,NROW,N_cloud))
        allocate(wr_FD_swa(NM_hgt,NROW,N_cloud))
        allocate(wr_FD_lwa(NM_hgt,NROW,N_cloud))
        allocate(wr_temp(NM_hgt,NROW))
        allocate(wr_albedo(N_wave,NROW))

!**** End of variable declaration ************************************

        FORALL (i=1:NM_hgt) cf_hgt(i)=(i-1)*0.5
!***************** change datatype *******************************
        wr_albedo=nint(vis_albedo*1000)
        wr_pres=nint(pres_swa*10)
        wr_temp=nint((temp_swa)*10)
        wr_FU_swa=nint(FU_swa*10)
        wr_FD_swa=nint(FD_swa*10)
        wr_FU_lwa=nint(FU_lwa*10)
        wr_FD_lwa=nint(FD_lwa*10)
!        print *,wr_temp(:,182)
!        print *,temp_swa(:,182)
!     Open the file and initialize the SD interface.
        fid = sfstart( FILE_NAME, DFACC_CREATE )
        IF (fid==-1) then
                print *,'process stop at writing file: ',FILE_NAME
                stop
        ENDIF

!***************WRITE GEODATAFIELD************************************     
        RANK=1           
        
        start  ( 1 ) = 0
        edges  ( 1 ) = NROW
        dim_sizes(1) = NROW
        stride ( 1 ) = 1
    
        factor=1.0
        offset=0.0

        SDS_NAME="latitude"
        sds_id = sfcreate(fid,SDS_NAME,DFNT_FLOAT32,RANK,dim_sizes)
        factor=1.0
        offset=0.0
        status=sfwdata( sds_id, start, stride, edges, lat )
        status=sfsattr(sds_id,'Longname',DFNT_CHAR8,8,'latitude')
        status=sfsattr(sds_id,'units',DFNT_CHAR8,6,'degree') 
        status=sfsnatt(sds_id,'offset',DFNT_FLOAT32,1,offset)
        status=sfsnatt(sds_id,'factor',DFNT_FLOAT32,1,factor)
        datarange(1)=-90.0
        datarange(2)=90.0
        status=sfsnatt(sds_id,'range',DFNT_FLOAT32,2,datarange)

        SDS_NAME="longitude"
        sds_id = sfcreate(fid,SDS_NAME,DFNT_FLOAT32,RANK,dim_sizes)
        factor=1.0
        offset=0.0
        status=sfwdata ( sds_id, start, stride, edges, lon )
        status=sfsattr(sds_id,'Longname',DFNT_CHAR8,9,'longitude')
        status=sfsattr(sds_id,'units',DFNT_CHAR8,6,'degree') 
        status=sfsnatt(sds_id,'offset',DFNT_FLOAT32,1,offset)
        status=sfsnatt(sds_id,'factor',DFNT_FLOAT32,1,factor)
        datarange(1)=-180.0
        datarange(2)=180.0
        status=sfsnatt(sds_id,'range',DFNT_FLOAT32,2,datarange)

        SDS_NAME="vis_optical_depth"
        sds_id = sfcreate(fid,SDS_NAME,DFNT_FLOAT32,RANK,dim_sizes)
        factor=1.0
        offset=0.0
        status=sfwdata ( sds_id, start, stride, edges, vis_od )
        status=sfsattr(sds_id,'Longname',DFNT_CHAR8,12,'optial depth')
        status=sfsattr(sds_id,'units',DFNT_CHAR8,1,'1') 
        status=sfsnatt(sds_id,'offset',DFNT_FLOAT32,1,offset)
        status=sfsnatt(sds_id,'factor',DFNT_FLOAT32,1,factor)
        datarange(1)=0.0
        datarange(2)=500.0
        status=sfsnatt(sds_id,'range',DFNT_FLOAT32,2,datarange)

        SDS_NAME="day_night_flag"
        sds_id = sfcreate(fid,SDS_NAME,DFNT_INT8,RANK,dim_sizes)
        status=sfwdata (sds_id, start, stride, edges,DN_flag)
        status=sfsattr(sds_id,'Longname',DFNT_CHAR8,14,'Night(1)Day(0)')

        SDS_NAME="sky_mask"
        sds_id = sfcreate(fid,SDS_NAME,DFNT_INT16,RANK,dim_sizes)
        factor=1.0
        offset=0.0
        status=sfwdata ( sds_id, start, stride, edges, sky_index )
        status=sfsattr(sds_id,'Longname',DFNT_CHAR8,18,&
                'sky categorization')
        status=sfsattr(sds_id,'units',DFNT_CHAR8,1,'1') 
        status=sfsnatt(sds_id,'offset',DFNT_FLOAT32,1,offset)
        status=sfsnatt(sds_id,'factor',DFNT_FLOAT32,1,factor)
        status=sfsattr(sds_id,'clear',DFNT_CHAR8,1,'0') 
        status=sfsnatt(sds_id,'ice only',DFNT_CHAR8,1,'1')
        status=sfsnatt(sds_id,'ice+water',DFNT_CHAR8,1,'2')
        status=sfsnatt(sds_id,'ice+rain',DFNT_CHAR8,1,'3')
        status=sfsnatt(sds_id,'ice+water+rain',DFNT_CHAR8,1,'4')
        status=sfsnatt(sds_id,'water or rain',DFNT_CHAR8,1,'5')

        SDS_NAME="aerosol_flag"
        sds_id = sfcreate(fid,SDS_NAME,DFNT_INT8,RANK,dim_sizes)
        status=sfwdata ( sds_id, start, stride, edges,aero_flag)
        status=sfsattr(sds_id,'aerosol',DFNT_CHAR8,1,'1')

        SDS_NAME="lidlwc_flag"
        sds_id = sfcreate(fid,SDS_NAME,DFNT_INT8,RANK,dim_sizes)
        status=sfwdata ( sds_id, start, stride, edges,lidlwc_flag)
        status=sfsattr(sds_id,'lwc from lidar not in radiation',&
        DFNT_CHAR8,1,'1')

        SDS_NAME="rain_flag"
        sds_id = sfcreate(fid,SDS_NAME,DFNT_INT8,RANK,dim_sizes)
        status=sfwdata ( sds_id, start, stride, edges,rain_flag)
        status=sfsattr(sds_id,'rain_cldousat_flag',DFNT_CHAR8,1,'1')
        status=sfsattr(sds_id,'rain_dardar_flag',DFNT_CHAR8,1,'2')

        !SDS_NAME="Daylength"
        !sds_id = sfcreate(fid,SDS_NAME,DFNT_INT16,RANK,dim_sizes)
        !factor=10
        !offset=0.0
        !status=sfwdata ( sds_id, start, stride, edges, Day_Len )
        !status=sfsattr(sds_id,'Longname',DFNT_CHAR8,18,&
        !        'Hour of daylength')
        !status=sfsattr(sds_id,'units',DFNT_CHAR8,1,'h') 
        !status=sfsnatt(sds_id,'offset',DFNT_FLOAT32,1,offset)
        !status=sfsnatt(sds_id,'factor',DFNT_FLOAT32,1,factor)

        SDS_NAME="height"
        edges(1)= NM_hgt
        dim_sizes(1)=NM_hgt
        sds_id = sfcreate(fid,SDS_NAME,DFNT_INT8,RANK,dim_sizes)
        factor=1.0
        offset=0.0
        status=sfwdata(sds_id, start, stride, edges,cf_hgt)
        status=sfsattr(sds_id,'height',DFNT_CHAR8,20,&
                'altitude of results')
        status=sfsattr(sds_id,'units',DFNT_CHAR8,2,'km') 
        status=sfsnatt(sds_id,'offset',DFNT_FLOAT32,1,offset)
        status=sfsnatt(sds_id,'factor',DFNT_FLOAT32,1,factor)
        datarange(1)=0.0
        datarange(2)=25.0
        status=sfsnatt(sds_id,'range',DFNT_FLOAT32,2,datarange)

!****************write data field **********************************

        RANK=2
        dim_sizes(1)=N_wave
        dim_sizes(2)=NROW
        edges(1)=N_wave
        edges(2)=NROW
        start(1)=0
        start(2)=0
        stride(1)=1
        stride(2)=1

        SDS_NAME="albedo"
        sds_id = sfcreate(fid,SDS_NAME,DFNT_INT16,RANK,dim_sizes)
        factor=1000.0
        offset=0.0
        status=sfwdata ( sds_id, start, stride, edges, wr_albedo )
        status=sfsattr(sds_id,'Longname',DFNT_CHAR8,20,&
                'vis surface albedo')
        status=sfsattr(sds_id,'units',DFNT_CHAR8,1,'/') 
        status=sfsnatt(sds_id,'offset',DFNT_FLOAT32,1,offset)
        status=sfsnatt(sds_id,'factor',DFNT_FLOAT32,1,factor)
        datarange(1)=0
        datarange(2)=1000
        status=sfsnatt(sds_id,'range',DFNT_FLOAT32,2,datarange)

        RANK=2
        dim_sizes(1)=NM_hgt
        dim_sizes(2)=NROW
        edges(1)=NM_hgt
        edges(2)=NROW
        start(1)=0
        start(2)=0
        stride(1)=1
        stride(2)=1

        SDS_NAME="pressure"
        sds_id = sfcreate(fid,SDS_NAME,DFNT_INT16,RANK,dim_sizes)
        factor=10.0
        offset=0.0
        status=sfwdata (sds_id, start, stride, edges,wr_pres)
        status=sfsattr(sds_id,'Longname',DFNT_CHAR8,8,&
                'pressure')
        status=sfsattr(sds_id,'units',DFNT_CHAR8,2,'hPa') 
        status=sfsnatt(sds_id,'offset',DFNT_FLOAT32,1,offset)
        status=sfsnatt(sds_id,'factor',DFNT_FLOAT32,1,factor)

        SDS_NAME="temperature"
        sds_id = sfcreate(fid,SDS_NAME,DFNT_INT16,RANK,dim_sizes)
        factor=10.0
        offset=0.0
        status=sfwdata (sds_id, start, stride, edges,wr_temp)
        status=sfsattr(sds_id,'Longname',DFNT_CHAR8,11,&
                'temperature')
        status=sfsattr(sds_id,'units',DFNT_CHAR8,4,'degC') 
        status=sfsnatt(sds_id,'offset',DFNT_FLOAT32,1,offset)
        status=sfsnatt(sds_id,'factor',DFNT_FLOAT32,1,factor)

        RANK=3
        dim_sizes(1)=NM_hgt
        dim_sizes(2)=NROW
        dim_sizes(3)=N_cloud
        edges(1)=NM_hgt
        edges(2)=NROW
        edges(3)=N_cloud
        start(1)=0
        start(2)=0
        start(3)=0
        stride(1)=1
        stride(2)=1
        stride(3)=1
        
        SDS_NAME="FU_SW"
        sds_id = sfcreate(fid,SDS_NAME,DFNT_INT16,RANK,dim_sizes)
        factor=10.0
        offset=0.0
        miss_value=-99.0
        status=sfwdata (sds_id, start, stride, edges,wr_FU_swa)
        status=sfsattr(sds_id,'Longname',DFNT_CHAR8,120,&
             'Upward flux in SW for sky with ice(1) and without ice(2)&
                clear sky(3) fullnowater(4)')
        status=sfsattr(sds_id,'units',DFNT_CHAR8,4,'W/m2') 
        status=sfsnatt(sds_id,'offset',DFNT_FLOAT32,1,offset)
        status=sfsnatt(sds_id,'factor',DFNT_FLOAT32,1,factor)
        status=sfsnatt(sds_id,'missing_value',DFNT_FLOAT32,1,miss_value)

        SDS_NAME="FD_SW"
        sds_id = sfcreate(fid,SDS_NAME,DFNT_INT16,RANK,dim_sizes)
        factor=10.0
        offset=0.0
        status=sfwdata (sds_id, start, stride, edges,wr_FD_swa)
        status=sfsattr(sds_id,'Longname',DFNT_CHAR8,100,&
          'Downward flux in SW for sky with ice(1) and without ice(2)&
          clear sky(3)')
        status=sfsattr(sds_id,'units',DFNT_CHAR8,4,'W/m2') 
        status=sfsnatt(sds_id,'offset',DFNT_FLOAT32,1,offset)
        status=sfsnatt(sds_id,'factor',DFNT_FLOAT32,1,factor)
        status=sfsnatt(sds_id,'missing_value',DFNT_FLOAT32,1,miss_value)

        SDS_NAME="SW_heat"
        sds_id = sfcreate(fid,SDS_NAME,DFNT_FLOAT,RANK,dim_sizes)
        factor=1.0
        offset=0.0
        status=sfwdata (sds_id, start, stride, edges,heat_swa)
        status=sfsattr(sds_id,'Longname',DFNT_CHAR8,100,&
             'SW heating rate for sky with ice(1) and without ice(2)&
                clear sky(3)')
        status=sfsattr(sds_id,'units',DFNT_CHAR8,5,'K/day') 
        status=sfsnatt(sds_id,'offset',DFNT_FLOAT32,1,offset)
        status=sfsnatt(sds_id,'factor',DFNT_FLOAT32,1,factor)

        SDS_NAME="FU_LW"
        sds_id = sfcreate(fid,SDS_NAME,DFNT_INT16,RANK,dim_sizes)
        factor=10.0
        offset=0.0
        status=sfwdata (sds_id, start, stride, edges,wr_FU_lwa)
        status=sfsattr(sds_id,'Longname',DFNT_CHAR8,100,&
             'Upward flux in LW for sky with ice(1) and without ice(2)&
                clear sky(3)')
        status=sfsattr(sds_id,'units',DFNT_CHAR8,4,'W/m2') 
        status=sfsnatt(sds_id,'offset',DFNT_FLOAT32,1,offset)
        status=sfsnatt(sds_id,'factor',DFNT_FLOAT32,1,factor)
        status=sfsnatt(sds_id,'missing_value',DFNT_FLOAT32,1,miss_value)

        SDS_NAME="FD_LW"
        sds_id = sfcreate(fid,SDS_NAME,DFNT_INT16,RANK,dim_sizes)
        factor=10.0
        offset=0.0
        status=sfwdata(sds_id, start, stride, edges,wr_FD_lwa)
        status=sfsattr(sds_id,'Longname',DFNT_CHAR8,100,&
           'Downward flux in LW for sky with ice(1) and without ice(2)&
                clear sky(3)')
        status=sfsattr(sds_id,'units',DFNT_CHAR8,5,'mW/m2') 
        status=sfsnatt(sds_id,'offset',DFNT_FLOAT32,1,offset)
        status=sfsnatt(sds_id,'factor',DFNT_FLOAT32,1,factor)
        status=sfsnatt(sds_id,'missing_value',DFNT_FLOAT32,1,miss_value)

        SDS_NAME="LW_heat"
        sds_id = sfcreate(fid,SDS_NAME,DFNT_FLOAT,RANK,dim_sizes)
        factor=1.0
        offset=0.0
        status=sfwdata(sds_id, start, stride, edges,heat_lwa)
        status=sfsattr(sds_id,'Longname',DFNT_CHAR8,100,&
             'LW heating rate for sky with ice(1) and without ice(2)&
                clear sky(3)')
        status=sfsattr(sds_id,'units',DFNT_CHAR8,5,'K/day') 
        status=sfsnatt(sds_id,'offset',DFNT_FLOAT32,1,offset)
!*******************************************************************
        status = sfendacc ( sds_id )
!
!        print *,status
!     Terminate access to the SD interface and close the file.
!
        status = sfend ( fid )
!        print *,status
        print *,'finish writing',file_name

        end subroutine write_hdf
