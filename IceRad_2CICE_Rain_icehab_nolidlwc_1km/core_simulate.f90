        subroutine core_simulate(Julday,day_2,icehab,swathid,part_flag)

        use global

        implicit none

        real :: height_d(NCOL_D)
        integer (kind=2) ::albedo_mon(720,360,23,N_wave),&
                vis_abd(N_wave),write_abd(N_wave),snow_abd(N_wave),&
                sea_abd(N_wave)
		real :: zero_abd
        common /albedo/ albedo_mon
	
!       *********************** local declaration*******************
        integer :: i,ci,k1,k2,si,hi,dj,suni,counthour,icloudy,&
                file_size,water_top,ice_top,wk,Ncloud,rain_top,&
                cld_rain_flag

        real :: Ice_Rain_Info(4), water_mark(NCOL_S),rain_mark(NCOL_D)
 
        integer :: ice_flag,water_flag_s,water_flag_d,lidwat_flag,&
                        no_iwc,no_rain,values(8),rain_flag_s

        integer :: Julday,day_2,lat_scr,lon_scr,mon_scr ! definition for
! suninfo albedo
!======================== aerosol ==============================
        real (kind=4) :: temp_ssa(125),temp_asy(125),&
		temp_wcaero_flag(125),temp_tau(125)

        integer :: aero_season,wi,aero_top
		real :: atm_hgt(50)=(/120.0,115.0,110.0,105.0,100.0,95.0,90.0,&
                85.0,80.0,75.0,70.0,65.0,60.0,55.0,50.0,47.5,45.0,42.5,&
                40.0,37.5,35.0,32.5,30.0,27.5,25.0,24.0,23.0,22.0,21.0,&
                20.0,19.0,18.0,17.0,16.0,15.0,14.0,13.0,12.0,11.0,10.0,&
                9.0,8.0,7.0,6.0,5.0,4.0,3.0,2.0,1.0,0.0/)
!=======================================================================
        integer (kind=2) :: temp_dnflag,hour(N_h)
        real(kind=4) :: iwc_s1(NCOL_S),rei_s1(NCOL_S),lwc_s1(NCOL_S),&
                        rew_s1(NCOL_S),hgt_s(NCOL_S),rwc_s1(NCOL_S)
        real :: hour_sza(N_h),h_index(N_h),min_sza, hour_interval,tphour
        real, allocatable :: write_sza(:)
        real(kind=4) :: rain_hgt,rain_value,rain_size,temp_lat,&
                        temp_lon!,temp_time
        real :: noon_sza,midn_sza,yearlength,daylength
        real :: mean_hgt,mean_iwc,mean_rei,mean_hgt_1,mean_iwc_1,&
                mean_rei_1,ice_lay_1,ice_lay_2,water_lay_1,water_lay_2
        real :: miss_value,pres_mark,pres_surf

        character(len=100):: swout,lwout
        character(len=1) :: f_index
        character(len=25) :: atm_std_file,icehab
        character(len=4) :: wave(N_wave)
        character(len=5) :: swathid,part_flag 
!       ******************** output declaration*********************
        !from read_out
        real :: pres_out(NM_hgt),temp_out(NM_hgt),fd_out(NM_hgt),&
                fu_out(NM_hgt),heat_out(NM_hgt)
        common /res_out/ pres_out,temp_out,fd_out,fu_out,heat_out

!       ****************allocate memory ***************************
         allocate(pres_swa(NM_hgt,NROW))
         allocate(temp_swa(NM_hgt,NROW))
         allocate(FD_swa(NM_hgt,NROW,N_cloud))
         allocate(FU_swa(NM_hgt,NROW,N_cloud))
         allocate(heat_swa(NM_hgt,NROW,N_cloud))
         allocate(vis_albedo(N_wave,NROW))
         allocate(FD_lwa(NM_hgt,NROW,N_cloud))
         allocate(FU_lwa(NM_hgt,NROW,N_cloud))
         allocate(heat_lwa(NM_hgt,NROW,N_cloud))
         allocate(sky_index(NROW))
         allocate(Day_Len(NROW))
         allocate(aero_flag(NROW))
         allocate(rain_flag(NROW))
         allocate(lidlwc_flag(NROW))

        pres_swa=0.0
        
        iwc_s1=0.0
        rei_s1=0.0
        lwc_s1=0.0
        rew_s1=0.0
        hgt_s=0.0
        rain_value=0.15
        rain_size=25

        miss_value=-99.0

        zero_abd=0.0
        wave=['470 ','555 ','659 ','858 ','1240','1640','2130']
        snow_abd=[900,900,900,854,368,52,25]
        sea_abd =[26,74,87,31,0,0,0]

        FORALL (i=1:N_h) hour(i)=i*2  !index for sza
        hour_sza(:)=0.0

        lidlwc_flag=0
        aero_flag=0
        rain_flag=0
 
        DO si=1,Nrow ! for each swath
                !print *,si
!               call date_and_time(values=values)
!               print *,values
        !============= geolocation ============================
                temp_lat=lat(si)
                temp_lon=lon(si)
!                temp_time=time(si)                
        !=============aerosol===================================
                temp_tau=aero_ext(:,si)
                temp_ssa=aero_ssa(:,si)
                temp_asy=aero_asy(:,si)
        !	temp_wcaero_flag=wc_aero_flag(:,si)
	!==============ice part================================
                iwc_s1=iwc(:,si)  !unit, g/m^3
                rei_s1=rei(:,si) ! um
                lwc_s1=float(lwc(:,si))/1000.0 !unit: g/m^3
                rew_s1=float(rew(:,si))/10.0
                hgt_s = height_s(:,si)/1000.0 
                rwc_s1=float(rwc(:,si))/1000.0
        
                ice_flag = 0
                water_flag_s = 0
                water_flag_d = 0
                rain_flag_s = 0
                rain_mark = 0.0
                lidwat_flag=0
                water_top = 0
                ice_top = 0
                rain_top=0
                aero_top=0

                ice_lay_2=0.0
                ice_lay_1=0.0
                water_lay_1=0.0
                water_lay_2=0.0

        !===========inquire water layer=========================
                water_flag_d=count(lwc_s1 > 0.0)!+count(lidlwc(:,si) > 0.0)
                cld_rain_flag=count(rwc_s1 > 0.0)
        
        !=============================================================
        !        IF (ice_flag > 0) then ! if ice cloud is present
        !        both cloudy and clear sky are calculated
        !=============================================================       

                OPEN(200,file='IC.DAT'//swathid//part_flag)

                OPEN(300, file="WATER.DAT"//swathid//part_flag)!,form="unformatted")

                OPEN(600, file="atm_cond.dat"//swathid//part_flag)
        
                OPEN(700, file="aero_ssa.dat"//swathid//part_flag)
                OPEN(750, file="aero_gg.dat"//swathid//part_flag)       
                OPEN(800,file="aero_ext.dat"//swathid//part_flag)     

        !===========start each profiles========================
                ice_lay_1=0.0
                ice_lay_2=0.0
                water_lay_1=0.0
                water_lay_2=0.0
                k1=1
                k2=1

       DO hi=1,NCOL_S  
        !===================begin to write ice file=================
            ! when layer begins
            mean_rei=rei_s1(hi)
            mean_iwc=iwc_s1(hi)
            ice_lay_1=hgt_s(hi) 
            !judge ice layer gap between two layers
 
            IF (mean_iwc > 0.0 .and. mean_rei > 0.0) then

                IF (mean_rei .gt. 90.0) mean_rei=90.0
                IF (mean_rei .lt. 5.0) mean_rei=5.0

                IF (abs(ice_lay_2-ice_lay_1)>0.96 .and. ice_lay_2 /=0)&
                then 
                       write(200,"(x,f6.3,2x,f12.6,2x,f6.2)") &
                        (ice_lay_2+ice_lay_1)/2.0,0.0,0.0
                        ice_flag=ice_flag+1
                ENDIF

                IF (ice_top == 0)&
                        write(200,"(x,f6.3,2x,f12.6,2x,f6.2)") &
                        hgt_s(hi)+0.02,0.0,0.0

                        write(200,"(x,f6.3,2x,f12.6,2x,f6.2)") &
                        hgt_s(hi),mean_iwc,mean_rei
                        ice_flag=ice_flag+1
                        ice_lay_2=ice_lay_1
                        ice_top=ice_top+1
               ENDIF ! end ice cloud

        !================ end writing ice ==========================

        !====================water part=============================
        !=================== write caliop lwc=======================

           water_lay_1=hgt_s(hi)

        !============== write caliop lwc=========================
        !   IF ((lidlwc(hi,si) > 0) .AND. (lwc_s1(hi)<=0.0) &
        !                .AND. (hgt_s(hi) > 0)) Then
        !           if (water_top == 0) &
        !                write(300,"(x,f6.3,2x,f12.6,2x,f6.2)") &
        !                hgt_s(hi)+0.02,0.0,0.0

        !                IF (abs(water_lay_2-water_lay_1)>0.96 .and. &
        !                         water_lay_2 /=0 ) then
        !                write(300,"(x,f6.3,2x,f12.6,2x,f6.2)") &
        !                water_lay_1+0.02,0.0,0.0
        !                ENDIF

        !          write(300,"(x,f6.3,2x,f12.6,2x,f6.2)") &
        !               hgt_s(hi),lidlwc(hi,si),10.0

        !               water_lay_2=water_lay_1
        !               water_flag_s = water_flag_s+1
        !               lidwat_flag=lidwat_flag+1
        !               water_top = water_top+1

        !   Endif ! end write caliop lwc
                
!================= write cwc-rvod lwc==================
            IF (lwc_s1(hi) > 0.0 .AND. lwc_s1(hi) <=15.0 &
                   .AND. rew_s1(hi)>0.0 .AND. rew_s1(hi)<1000.0) then
                        
                        if (rew_s1(hi) > 25) rew_s1(hi)=25

                        if (water_top == 0) &
                        write(300,"(x,f6.3,2x,f12.6,2x,f6.2)") &
                        hgt_s(hi)+0.02,0.0,0.0
 
                        IF (abs(water_lay_2-water_lay_1)>0.96 .and. &
                                 water_lay_2 /=0 ) then 
                        write(300,"(x,f6.3,2x,f12.6,2x,f6.2)") &
                        (water_lay_2+water_lay_1)/2.0,0.0,0.0
                        ENDIF

                        write(300,"(x,f6.3,2x,f12.6,2x,f6.2)") &
                        hgt_s(hi),lwc_s1(hi),rew_s1(hi)

                        water_lay_2 = water_lay_1        
                        water_flag_s = water_flag_s+1
                        water_top = water_top+1
           ENDIF  
        !============= end write cwc-rvod lwc
           mean_hgt=0.0
           mean_hgt_1=0.0
           rain_hgt=0

          !when there is no water cloud 
          !if cwc-rvod and rain-profile have water profiles, use
          !cwc-rvod only, as cwc-rvod does't sperate non-rain and
          !rainy clouds

          IF (water_flag_d == 0) then

                IF (cld_rain_flag >0 .AND. rwc_s1(hi) >0.0) then 

                     if (rain_top ==0) then
                        write (300,"(x,f6.3,2x,f12.6,2x,f6.2)"),&
                                hgt_s(hi)+0.02,0.0,0.0
                                rain_top=rain_top+1
                 endif

                if (abs(water_lay_2-water_lay_1)>=0.96 .and. &
                        water_lay_2 /=0) then
                        write(300,"(x,f6.3,2x,f12.6,2x,f6.2)") &
                        water_lay_1+0.02,0.0,0.0
                endif

                rain_value=rwc_s1(hi)
                write(300,"(x,f6.3,2x,f12.6,2x,f6.2)")&
                    hgt_s(hi),rain_value,rain_size
                    water_lay_2=hgt_s(hi)
                    rain_flag_s=rain_flag_s+1
                    if (rain_flag(si) ==0) rain_flag(si)=1

                ENDIF
               ENDIF

        !============== end rain
        !==============end write water profile======================

        !=================== atmospheric conditions===================
             IF (wv_sh(hi,si) >0 .AND. pres(hi,si) >0 .AND. &
                        temp_t(hi,si) >0 .AND. ozone3(hi,si) >0 &
                        .AND. mod(hi,5)==0 .AND. hi >= 5) then 

                      write(600,"(x,f8.3,2x,f12.6,2x,f12.8,2x,f14.11)")&
                       (pres(hi-2,si)+pres(hi-1,si)+pres(hi,si)+&
                        pres(hi-3,si)+pres(hi-4,si))/500.0,&
                       (temp_t(hi-2,si)+temp_t(hi-1,si)+temp_t(hi,si)+ &
                        temp_t(hi-3,si)+temp_t(hi-4,si))/5.0,&
                       (wv_sh(hi-2,si)+wv_sh(hi-1,si)+wv_sh(hi,si)+ &
                        wv_sh(hi-3,si)+wv_sh(hi-4,si))/5.0,&
                       (ozone3(hi-2,si)+ozone3(hi-1,si)+ ozone3(hi,si)+&
                        ozone3(hi-3,si)+ozone3(hi-4,si))/5.0
                
                        pres_mark=(pres(hi-2,si)+pres(hi-1,si)+&
                        pres(hi,si)+pres(hi-3,si)+pres(hi-4,si))/500.0 
            ENDIF

                !deal with  the boundary
            IF (hi <= 124) THEN
                IF((pres(hi,si)*pres(hi+1,si)) < 0 ) then 

                       pres_surf=pres(hi,si)/100.0

                     IF (pres_surf > (pres_mark)+5.0)&
                     write(600,"(x,f8.3,2x,f12.6,2x,f12.8,2x,f14.11)")&
                        pres(hi,si)/100.0,temp_t(hi,si),&
                       wv_sh(hi,si),ozone3(hi,si)
                ENDIF
            ENDIF

        !================ write aerosols ================================
        !        IF (hi <=31 .AND. temp_ssa > 0.0) THEN 
        !           IF (temp_ext(hi) > 0.0) write(800,"(x,i3,x,f12.8)"),&
        !              31-hi,temp_ext(hi)
        !        ENDIF
        IF (temp_tau(hi) > 0.0 .and. hgt_s(hi) >0.0 .and. &
                temp_ssa(hi) > 0.0) &
           write(800,"(x,f6.3,x,f12.8)") hgt_s(hi),temp_tau(hi)
        IF (temp_ssa(hi) > 0.0 .and. hgt_s(hi) > 0.0) &
           write(700,"(x,f6.3,x,f12.8)") hgt_s(hi),temp_ssa(hi)
        IF (temp_asy(hi) > 0.0 .and. hgt_s(hi) > 0.0) &
           write(750,"(x,f6.3,x,f12.8)") hgt_s(hi),temp_asy(hi)

        IF (temp_tau(hi) > 0.0 .and. temp_ssa(hi) > 0.0 .and. &
           hgt_s(hi) > 0.0) aero_flag(si)=1

        ENDDO !end hi
                
        close(200)
        close(300)
        close(600)
        close(700)
        close(750)
        close(800)

        IF (lidwat_flag > 0) lidlwc_flag(si)=1  

!                print *, rain_flag,ice_flag,water_flag_s,ice_water_lay
 
!******************** get standard atmosphere file********************
                IF (abs(temp_lat) <= 23.5 ) then 
                        atm_std_file="afglt.dat"
                        aero_season=1
                ENDIF
                IF ( 23.5 < temp_lat .AND. temp_lat < 66.5) then
                       if (Julday <180) then 
                                atm_std_file="afglms.dat"
                                aero_season=1
                        else if (Julday >= 180) then 
                                atm_std_file="afglmw.dat"
                                aero_season=2
                      end if
                ELSE IF (66.5 <= temp_lat .AND. temp_lat <=90) then    

                        if (Julday <180) then 
                                atm_std_file="afglss.dat"
                                aero_season=1
                        else if (Julday >= 180) then 
                                atm_std_file="afglsw.dat"
                                aero_season=2
                        end if

                ELSE IF (-66.5 < temp_lat .AND. temp_lat <-23.5) then
                        if (Julday <180) then 
                                atm_std_file="afglmw.dat"
                                aero_season=2
                        else if (Julday >= 180) then 
                                atm_std_file="afglms.dat"
                                aero_season=1
                        end if
                        
                ELSE IF (-90 <= temp_lat .AND. temp_lat < -66.5) then   

                        if (Julday <180) then 
                                atm_std_file="afglsw.dat"
                                aero_season=2
                        else if (Julday >=180) then  
                                atm_std_file="afglss.dat"
                                aero_season=1
                        end if
                END IF

        !*************write sky index******************************
        IF (ice_flag == 0 .and. (water_flag_s+&
                rain_flag_s<2) ) then 
                sky_index(si) = 0
                Ncloud=3
        ELSE IF (ice_flag == 0 .and. (water_flag_s+ &
                rain_flag_s) >= 2) then
                sky_index(si)=5
                Ncloud=2
        ELSE IF (ice_flag > 0 .and. (water_flag_s+ &
                rain_flag_s) <2) then
                sky_index(si) = 1
                Ncloud=1 
        ELSE IF (ice_flag > 0 .and. water_flag_s > 1 .and. &
                rain_flag_s == 0) then
                sky_index(si) = 2
                Ncloud=1
        ELSE IF (ice_flag > 0 .and. water_flag_s == 0 .and. &
                rain_flag_s > 1) then
                sky_index(si) = 3 
                Ncloud=1
        ELSE IF (ice_flag > 0 .and. water_flag_s > 0 .and. &
                rain_flag_s > 0) then
                sky_index(si) = 4 
                Ncloud=1
        ENDIF

       !print *,si,ice_flag,rain_flag_s,&
       !         water_flag_s,water_flag_d,sky_index(si)
        
        !================SW computation===============================
        ! once clouds present, assume them stay there a whole day(day
        ! and night)

        counthour=0
		!tphour=(3101.872+146.24)/3600.
        !call sun_info(temp_lat,temp_lon,Julday,tphour,&
        !                noon_sza,daylength)
		!print *,noon_sza	
	
        DO suni=1,N_h  
                call sun_info(temp_lat,temp_lon,Julday,hour(suni),&
                        noon_sza,daylength)
                hour_sza(suni)=noon_sza
        ENDDO

        counthour=count(hour_sza < 90.0)
        
				!print *,si,counthour,hour_sza
       IF (counthour > 0) THEN
                allocate(write_sza(counthour))
                write_sza(:)=0.0 
                ci=1
                DO suni=1, N_h
                        IF (hour_sza(suni)< 90.0) THEN
                               write_sza(ci)=hour_sza(suni)
                               ci=ci+1
                        ENDIF
                ENDDO
        
        ENDIF
                Day_Len(si) = nint(daylength*10)
        !===============get albedo ================================= 
                lat_scr=nint((90-temp_lat-0.25)/0.5)
                lon_scr=nint((temp_lon+180-0.25)/0.5)
                mon_scr=(day_2/16)+1
                vis_abd=albedo_mon(lon_scr,lat_scr,mon_scr,:)
                vis_albedo(:,si)=float(vis_abd)/1000.0
        
                IF (vis_abd(1) >= 0.0) write_abd=vis_abd
                IF (vis_abd(1) < 0.0 .AND. abs(temp_lat) >= 70) &
                        write_abd=snow_abd
                IF (vis_abd(1) < 0.0 .AND. abs(temp_lat) < 70) &
                        write_abd=sea_abd
                
                OPEN (900, file="sw_albedo.dat"//swathid//part_flag)
                DO wi=1,N_wave
                write(900,fmt="(a6,x,f5.3)"),wave(wi),write_abd(wi)/1000.0
                ENDDO
                write(900,fmt="(a6,x,f5.3)") '100000',zero_abd
                CLOSE(900)
	
        DO icloudy=Ncloud,N_cloud
		!print *,Ncloud,N_cloud,temp_dnflag
                 
        IF (counthour > 0)  THEN  ! when the sun presents

                OPEN (500,file="swinput.inp"//swathid//part_flag)
                        write(500,*) "atmosphere_file &
                           	/sw/libRadtran-2.0.1-gnu/&
                            share/libRadtran/data/atmmod/"//atm_std_file
                   write(500,*) "source solar /sw/libRadtran-2.0.1-gnu/&
                      share/libRadtran/data/solar_flux/kurudz_0.1nm.dat"
                        write(500,*) "day_of_year", Julday
                        write(500,*) "radiosonde atm_cond.dat"&
                        //swathid//part_flag//" H2O MMR O3 MMR"
                        write(500,*) "sur_temperature",skin_T(si)
                        write(500,*) "rte_solver twostr"
                        write(500,*) "wavelength 250.  4000."
                        write(500,*) "mol_abs_param fu"
                        write(500,*) "sza SZA"
                        write(500,*) "albedo_file  sw_albedo.dat"&
                        //swathid//part_flag 
                        IF (icloudy==1 .and. ice_flag > 0) then
                        !write(500,*) "ic_layer"
                        write(500,*) "ic_file 1D IC.DAT"//&
                        swathid//part_flag
                       write(500,*) "ic_properties yang2013 interpolate"
                       write(500,*) "ic_habit_yang2013 ",&
                        trim(icehab)//" severe"
                        swout="swcloudy.out"//swathid//part_flag
                        ELSE IF (icloudy == 2) then
                        swout="swnoice.out"//swathid//part_flag
                        ELSE IF (icloudy == 3) then
                        swout="swclear.out"//swathid//part_flag
                        ELSE IF (icloudy == 4) then
                        if (ice_flag > 0) then
                         !       write(500,*) "ic_layer"
                                write(500,*) "ic_file 1D IC.DAT"&
                                //swathid//part_flag
                       write(500,*) "ic_properties yang2013 interpolate"
                                write(500,*) "ic_habit_yang2013 ",&
                                trim(icehab)//" severe"
                        endif
                        swout="swfullnoice.out"//swathid//part_flag
                        ENDIF

                        IF (aero_flag(si) > 0.0) THEN
                        write(500,*) "aerosol_default "
                        write(500,*) "aerosol_file gg  aero_gg.dat"//&
                        swathid//part_flag
                        write(500,*) "aerosol_file ssa  aero_ssa.dat"//&
                        swathid//part_flag
                        write(500,*) "aerosol_file tau aero_ext.dat"//&
                        swathid//part_flag
                        ENDIF

                        IF (icloudy < 3) THEN
                        !totally clear
                        IF ((water_flag_s+rain_flag_s >=2))&
                                then
                        !        write(500,*) "wc_layer"
                                write(500,*) "wc_file 1D WATER.DAT"//&
                                swathid//part_flag
                                write(500,*) "wc_properties mie"
                        ENDIF

                        ENDIF

                    write(500,*) "zout 0 1 2 3 4 "//&
                     "5 6 7 8 9 10 11 "// &
                    "12 13 14 15 16 17 "//& 
		    "18 19 20 21 22 23 "//&
                    "24 25"
                        write(500,*) "output_user zout sza p T eglo &
                                         eup  heat"
                        write(500,*) "output_process sum"
                        write(500,*) "quiet"
                close(500)
                 
                OPEN (700,file="sw.sh"//swathid//part_flag)
                        write(700,*) "#!/bin/bash"
                        write(700,*) "for sza in", write_sza
                        write(700,*) "do"
                        write(700,*) "sed s/SZA/$sza/ &
                       swinput.inp"//swathid//part_flag//" > uvspec.inp"//swathid//part_flag
                        write(700,*) "uvspec < uvspec.inp"//swathid//part_flag//" >>"//&
                                trim(swout)
                        write(700,*) "rm -f uvspec.inp"//swathid//part_flag
                        write(700,*) "done"
                close(700)
              
                 
                call system("sh sw.sh"//swathid//part_flag)
                 
                call system("rm -f sw.sh"//swathid//part_flag)
                  
                !===deal with results==========
                inquire(file=swout,size=file_size)
              !   print *, file_size
               
                IF (file_size == 0 .or. file_size == -1) then
                        FD_swa(:,si,:)=miss_value
                        FU_swa(:,si,:)=miss_value
                        heat_swa(:,si,:)=miss_value
                        print *,si
                        goto 1000               
                ENDIF ! END NO VALID FILES
                call read_output(swout,counthour)
        
                IF (Ncloud == 3) then ! if clear sky
                        FD_swa(:,si,1)=fd_out
                        FU_swa(:,si,1)=fu_out
                        heat_swa(:,si,1)=heat_out
                        FD_swa(:,si,2)=fd_out
                        FU_swa(:,si,2)=fu_out
                        heat_swa(:,si,2)=heat_out
                        FD_swa(:,si,3)=fd_out
                        FU_swa(:,si,3)=fu_out
                        heat_swa(:,si,3)=heat_out
                        FD_swa(:,si,4)=fd_out
                        FU_swa(:,si,4)=fu_out
                        heat_swa(:,si,4)=heat_out
                ELSE IF (Ncloud ==2) then ! if only water present 
                        FD_swa(:,si,icloudy)=fd_out
                        FU_swa(:,si,icloudy)=fu_out
                        heat_swa(:,si,icloudy)=heat_out
                        FD_swa(:,si,1)=FD_swa(:,si,2)
                        FU_swa(:,si,1)=FU_swa(:,si,2)
                        heat_swa(:,si,1)=heat_swa(:,si,2)
                ELSE IF (Ncloud == 1) then
                       FD_swa(:,si,icloudy)=fd_out
                        FU_swa(:,si,icloudy)=fu_out
                        heat_swa(:,si,icloudy)=heat_out
                ENDIF
!        print *,icloudy,swout,fd_out
               
        ENDIF  ! IF SZA<90
              
        IF (counthour==0.0 .OR. temp_dnflag==1) then
                pres_swa(:,si)=miss_value
                temp_swa(:,si)=miss_value
                FD_swa(:,si,:)=miss_value
                FU_swa(:,si,:)=miss_value
                heat_swa(:,si,:)=miss_value
        ENDIF ! end sw calculat
!=================LW computation==============================   
  1000   OPEN (400,file="lwinput.inp"//swathid//part_flag)
                        write(400,*) "atmosphere_file &
			/sw/libRadtran-2.0.1-gnu/&  
                         share/libRadtran/data/atmmod/"//atm_std_file
                        write(400,*) "radiosonde atm_cond.dat"//&
                        swathid//part_flag//" H2O MMR O3 MMR"
                        write(400,*) "sur_temperature",skin_T(si)
                        write(400,*) "source thermal"
                        write(400,*) "day_of_year", Julday
!                        write(400,*) "nstr 6"
                        write(400,*) "rte_solver twostr"
                        write(400,*) "wavelength 4000.  100000."
                        write(400,*) "mol_abs_param fu"
                        
                        IF (icloudy==1 .AND. ice_flag > 0) then
                        !write(400,*) "ic_layer"
                        write(400,*) "ic_file 1D IC.DAT"&
                        //swathid//part_flag
                       write(400,*) "ic_properties yang2013 interpolate"
                       write(400,*) "ic_habit_yang2013  ",&
                        trim(icehab)//" severe"
                        lwout="lwcloudy.out"//swathid//part_flag
                        ELSE IF (icloudy == 2) then
                        lwout="lwnoice.out"//swathid//part_flag
                        ELSE IF (icloudy == 3) then
                        lwout="lwclear.out"//swathid//part_flag
                        ELSE IF (icloudy ==4) then
                          if (ice_flag > 0) then
                        !     write(400,*) "ic_layer"
                             write(400,*) "ic_file 1D IC.DAT"&
                                //swathid//part_flag
                       write(400,*) "ic_properties yang2013 interpolate"
                       write(400,*) "ic_habit_yang2013 ",trim(icehab)&
                                //" severe"
                          endif
                        lwout="lwfullnoice.out"//swathid//part_flag
                        ENDIF

                !        IF (sum(temp_ssa) > 0.0) THEN
                !       write(400,*) "aerosol_default"
                !        write(400,*) "aerosol_file tau aero_ext.dat"
                !        write(400,*) "aerosol_file ssa  aero_ssa.dat"
                !        write(400,*) "aerosol_file gg aero_gg.dat"
                !        ENDIF

                        IF (icloudy < 3) then
                        IF ((water_flag_s+rain_flag_s >= 2))then
                        !         write(400,*) "wc_layer"
                             write(400,*) "wc_file 1D WATER.DAT"//&
                                swathid//part_flag
                           write(400,*) "wc_properties mie "  
                        ENDIF         
                        ENDIF
                        write(400,*) "output_file  ",trim(lwout)
                  write(400,*) "zout 0 1 2 3 4 5 "//&
                   "6 7 8 9 10 11 "// &
                   "12 13 14 15 16 17 "// &
	           "18 19 20 21 22 "//&
                   "23 24 25"
                        write(400,*) "output_user zout p T eglo &
                                        eup heat"
                        write(400,*) "output_process sum" 
                        write(400,*) "quiet" 
                close(400)
                 
                !********** if model fail**************************               
                call system("uvspec <lwinput.inp"//&
                swathid//part_flag//"> mytest.out"//swathid//part_flag)
                 
                inquire(file=lwout,size=file_size)

                IF (file_size == 0 .or. file_size == -1) then
                FD_lwa(:,si,:)=miss_value
                FU_lwa(:,si,:)=miss_value
                heat_lwa(:,si,:)=miss_value
                print *,si
                goto 2000              

                ENDIF
                
                call read_output(lwout,hour_interval)
                        pres_swa(:,si)=pres_out
                        temp_swa(:,si)=temp_out
		!print *,Ncloud
		!print *,fu_out
               !======== record results========================
                IF (Ncloud == 3) then ! if clear sky
                        FD_lwa(:,si,1)=fd_out
                        FU_lwa(:,si,1)=fu_out
                        heat_lwa(:,si,1)=heat_out
                        FD_lwa(:,si,2)=fd_out
                        FU_lwa(:,si,2)=fu_out
                        heat_lwa(:,si,2)=heat_out
                        FU_lwa(:,si,3)=fu_out
                        FD_lwa(:,si,3)=fd_out
                        heat_lwa(:,si,3)=heat_out
                        FU_lwa(:,si,4)=fu_out
                        FD_lwa(:,si,4)=fd_out
                        heat_lwa(:,si,4)=heat_out
                ELSE IF (Ncloud ==2 ) then ! if only water present 
                        FD_lwa(:,si,icloudy)=fd_out
                        FU_lwa(:,si,icloudy)=fu_out
                        heat_lwa(:,si,icloudy)=heat_out
                        FD_lwa(:,si,1)=FD_lwa(:,si,2)
                        FU_lwa(:,si,1)=FU_lwa(:,si,2)
                        heat_lwa(:,si,1)=heat_lwa(:,si,2)
                ELSE IF (Ncloud == 1) then
                        FD_lwa(:,si,icloudy)=fd_out
                        FU_lwa(:,si,icloudy)=fu_out
                        heat_lwa(:,si,icloudy)=heat_out
                ENDIF
              
           ENDDO ! end clear and cloudy,icloudy
	
  2000     if (counthour >0) deallocate(write_sza)
               call system("rm -f IC.DAT"//swathid//part_flag)
               call system("rm -f WATER.DAT"//swathid//part_flag)
               call system("rm -f atm_cond.dat"//swathid//part_flag)
               call system("rm -f swinput.inp"//swathid//part_flag)
               call system("rm -f lwinput.inp"//swathid//part_flag)
               call system("rm -f sw_albedo.dat"//swathid//part_flag)
               call system("rm -f aero_ext.dat"//swathid//part_flag)
               call system("rm -f aero_gg.dat"//swathid//part_flag)
               call system("rm -f aero_ssa.dat"//swathid//part_flag)
               call system("rm -f swcloudy.out"//swathid//part_flag)
               call system("rm -f swclear.out"//swathid//part_flag)
               call system("rm -f lwcloudy.out"//swathid//part_flag)
               call system("rm -f lwclear.out"//swathid//part_flag)
               call system("rm -f swnoice.out"//swathid//part_flag)
               call system("rm -f lwnoice.out"//swathid//part_flag)
               call system("rm -f swfullnoice.out"//swathid//part_flag)
               call system("rm -f lwfullnoice.out"//swathid//part_flag)
         ENDDO   ! end swath
       
         
        end subroutine core_simulate
                
