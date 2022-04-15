        program ice_cf

        use global
        
        implicit none
        
        include 'mpif.h'

        character(len=120) :: filename,fname_cwc,&
                              fname_ecmwf,out_fname,fname_cwc1,&
                              fname_2cice,fname_aero,fname_rain        
        character(len=100) :: cwcdir,ecmdir,filestr,&
                        writedir,icedir,aerodir,raindir
	character(len=84), allocatable :: file_mmdd(:)
	character(len=84) :: subfile, ftemp
	character(len=2) :: month_1
        character(len=3) :: day_1
        character(len=4) :: year_1
        character(len=6) :: clddate
        character(len=5) :: swathid,part_flag
        character(len=25) :: icehab
        integer :: day_2,Jul_day,year_2,month
  
        logical flag,DirExis,ecmwf_flag,cwc_flag,c2ice_flag,&
                rain_pro_flag,aero_lid_flag

        integer :: status,Nfname
        integer :: values(8),di,si,hi
       
	!************ initialize for mpi***********************
        integer :: mpi_err, numnodes, myid, Nodes
        integer, parameter :: my_root=0

        call MPI_INIT( mpi_err )
        call MPI_COMM_SIZE( MPI_COMM_WORLD, numnodes, mpi_err )
        call MPI_Comm_rank( MPI_COMM_WORLD, myid, mpi_err )
        !***************************************************
        clddate="200807"
        icehab='column_8elements'
        part_flag=''
        aerodir="/data/gdi/b/yulanh/aerosol/"//clddate//"/"
        cwcdir="/data/gdi/b/yulanh/CWC-RVOD/"//clddate//"/"
        ecmdir="/data/gdi/b/yulanh/ECMWF/"//clddate//"/"
        icedir="/data/gdi/b/yulanh/2CICE/"//clddate//"/"
        raindir="/data/gdi/b/yulanh/2C-RAIN/"//clddate//"/"
        writedir="/data/gdi/b/yulanh/ICE_RAD/"//clddate//"/"            
      
        IF (myid == my_root) THEN  
        call date_and_time(values=values)
        print *,values
        !======= create folder =================================
        inquire(file=trim(writedir),exist=DirExis)
        IF (DirExis) then
                write (*,*) "Directory exists:  ", trim(writedir)
        ELSE
                call system("mkdir "//trim(writedir))
                write (*,*) "Directory is created:  ",trim(writedir)
        ENDIF

        !======================================================
        read(*,*) day_1        
        call system(&
                "ls "//trim(icedir)//"2008"//day_1//"*"//" >&
                filename"//day_1,status)
        call system("cat filename"//day_1//" | wc -l > count.txt"//day_1)

        open(100, file="count.txt"//day_1)
        read(100,*) Nfname
        close(100)

        print *,'total file',Nfname
       
        allocate(file_mmdd(Nfname))
        open(200,file="filename"//day_1)
        read(200,fmt="(a84)",iostat=status) file_mmdd
        close(200)

        EndIf  ! end root==0
       
        call MPI_SCATTER(file_mmdd,84,MPI_CHARACTER,subfile,&
        84,MPI_CHARACTER,my_root,MPI_COMM_WORLD,mpi_err)
 
        !===== to get Julian day, starting at 3.21=====================
        ftemp=subfile
        day_1=ftemp(37:39)
        year_1=ftemp(33:36)
        swathid=ftemp(47:51)
!	print *,ftemp	
!	   	print *, 'myid= ', myid,' ', day_1,' ',year_1,' ',swathid
       
        read(day_1,'(i3)') day_2
        read(year_1,'(i4)') year_2

        call Julday2month(year_2,day_2,month)        
        write(month_1,'(i2.2)') month
        !	print *,year_2,day_2,month,month_1    

        Jul_day=day_2
        fname_2cice=trim(icedir)//trim(ftemp(33:51))//&
                "_CS_2C-ICE_GRANULE_P1_R04_E02.hdf"
        !	print *, 'myid= ',myid, Jul_day,year_2,day_2,fname_2cice
!	print *, fname_2cice
      
        !=========== copy and untar files ==============================

        filestr="_CS_2B-CWC-RVOD_GRANULE_P_R04_E02.hdf"
        fname_cwc=trim(cwcdir)//trim(ftemp(33:51))&
            //trim(filestr)
        filestr="_CS_ECMWF-AUX_GRANULE_P_R05_E02_F00.hdf"
        fname_ecmwf=trim(ecmdir)//trim(ftemp(33:51))&
        //trim(filestr)
        fname_aero=trim(aerodir)//"aerosol_lid_"//&
                  trim(ftemp(33:51))//".hdf"
        filestr="_CS_2C-RAIN-PROFILE_GRANULE_P_R04_E02.hdf"
        fname_rain=trim(raindir)//trim(ftemp(33:51))//&
                  trim(filestr)
        !print *,'myid= ',myid,fname_cwc,fname_ecmwf,fname_aero
        !============inquire file info and define arrays================

        INQUIRE (file=fname_ecmwf,exist=ecmwf_flag)
        INQUIRE (file=fname_cwc,exist=cwc_flag)
        INQUIRE (file=fname_2cice,exist=c2ice_flag)     
        INQUIRE (file=fname_rain,exist=rain_pro_flag)
        INQUIRE (file=fname_aero,exist=aero_lid_flag)
!        print *,'myid= ',myid,ecmwf_flag,cwc_flag,c2ice_flag,&
!		rain_pro_flag,aero_lid_flag
! 	print *,aero_lid_flag,fname_aero
      
        IF (cwc_flag .AND. ecmwf_flag .AND. c2ice_flag) THEN  
     
        call read_albedo_spectral()
        call inquire_info(fname_ecmwf)
        call read_ecmwf(fname_ecmwf)
        call read_cloudsat(fname_cwc)
        call read_2cice(fname_2cice)

        IF (rain_pro_flag) Then
                call read_rain(fname_rain)
        Else
                allocate(rwc(125,NROW))
                rwc=0.0
        EndIf

        IF (aero_lid_flag) Then
              call read_aerosol(fname_aero)
        Else
              allocate(aero_ssa(125,NROW))
              allocate(aero_ext(125,NROW))
              allocate(aero_asy(125,NROW))
              allocate(lidlwc(125,NROW))
              aero_ext=0.0
              aero_ssa=0.0
              aero_asy=0.0
              lidlwc=0.0 
        EndIf
 
        call core_simulate(Jul_day,day_2,icehab,swathid,part_flag)        
       
       out_fname=trim(writedir)//&
                 trim(ftemp(33:51))//&
                        "_2C-ICE_p1_HEAT_FLUX_aero_rain_"//&
                        trim(icehab)//trim(part_flag)//".hdf"
       call write_hdf(out_fname)
                                
       include 'deallocate_array.file'
      END IF ! end if all dataset files exist

      IF (myid == my_root) Then

      deallocate(file_mmdd)
      call date_and_time(values=values)
      print *,values

      ENDIF

      call MPI_FINALIZE(mpi_err)

      endprogram ice_cf
