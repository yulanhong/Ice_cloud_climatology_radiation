        subroutine core_simulate(Julday,day_2)

        use global

        implicit none

        real :: height_d(NCOL_D)
        integer (kind=2) ::albedo_mon(720,360,23,N_wave),vis_abd(N_wave)
        common /albedo/ albedo_mon

!       *********************** local declaration*******************
        integer :: i,k1,k2,si, hi, dj,icloudy,file_size,water_top,&
                        ice_top,wk,Ncloud,&
                        rain_top,ice_rain_flag,ice_index,rain_index

        integer :: ice_water_lay

        real :: Ice_Rain_Info(4), water_mark(NCOL_S),rain_mark(NCOL_D)
 
        integer :: ice_flag,water_flag_d,water_flag_s,rain_flag,&
                        no_iwc,no_rain,values(8),rain_flag_s

        integer :: Julday,day_2,lat_scr,lon_scr,mon_scr ! definition for
! suninfo albedo
        integer :: aero_season,wi
        integer (kind=2) :: mask_s1(NCOL_D),temp_dnflag
        real(kind=4) :: iwc_s1(NCOL_D),rei_s1(NCOL_D),lwc_s1(NCOL_S),&
                        rew_s1(NCOL_S),hgt_s(NCOL_S)
        real :: hour_sza(N_h),h_index(N_h),min_sza
        real(kind=4) :: rain_hgt,rain_value,rain_size,temp_lat,&
                        temp_lon,temp_time
        real :: noon_sza,midn_sza,yearlength,daylength,hour,hour_2
        real :: mean_hgt,mean_iwc,mean_rei,mean_hgt_1,mean_iwc_1,&
                mean_rei_1,ice_lay_1,ice_lay_2,water_lay_1,water_lay_2
        real :: miss_value,pres_mark,pres_surf

        character(len=100):: swout,lwout
        character (len=1) :: f_index
        character (len=10) :: atm_std_file
        character (len=4) :: wave(N_wave)

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
         allocate(SZA_noon(NROW))
         allocate(sky_index(NROW))

        pres_swa=0.0

        FORALL(i=1:NCOL_D) height_d(i)= 25.08-0.06*(i-1) 
        
        iwc_s1=0.0
        rei_s1=0.0
        lwc_s1=0.0
        rew_s1=0.0
        hgt_s=0.0
        rain_value=0.15
        rain_size=25

        yearlength=365.24
        hour=0.0       

        wave=['470 ','555 ','659 ','858 ','1240','1640','2130']

        FORALL (i=1:N_h) h_index(i)=i  !index for sza
 
        DO  si=1,NROW ! for each swath

!                print *,si
!                call date_and_time(values=values)
!                print *,values
        !============= geolocation ============================
                temp_lat=lat(si)
                temp_lon=lon(si)
                temp_time=time(si)                
                temp_dnflag=DN_flag(si)
        !==============ice part================================

                iwc_s1=iwc(:,si)*1000.0  !unit, g/m^3
                rei_s1=rei(:,si)*1000000.0 ! um
                mask_s1=mask(:,si)
                lwc_s1=float(lwc(:,si))/1000.0 !unit: g/m^3
                rew_s1=float(rew(:,si))/10.0
                hgt_s = height_s(:,si)/1000.0 

                ice_flag = 0
                rain_flag = 0
                water_flag_d = 0
                water_flag_s = 0
                rain_flag_s = 0
                rain_mark = 0.0
                water_top = 0
                ice_top = 0
                rain_top=0
                ice_water_lay=0
                

                ice_rain_flag=0
                ice_index=0
                rain_index=0



                ice_lay_2=0.0
                ice_lay_1=0.0
                water_lay_1=0.0
                water_lay_2=0.0

                !============== inquiry rain and ice===================
                DO i=1,NCOL_S

                        dj=4*i

                        IF (dj < 418) then 

                                Ice_Rain_Info=0.0

                                where ( iwc_s1((dj-3):dj) > 0 .and. &
                                iwc_s1((dj-3):dj) <=10.0) &
                                Ice_Rain_Info=height_d(dj-3:dj)

                                IF (sum(Ice_Rain_Info) > 0.0) then
                                ice_flag=ice_flag+1
                                !ice_rain_flag=ice_rain_flag+1
                                ENDIF
        
                                where( mask_s1((dj-3):dj) == 5) &
                                rain_mark((dj-3):dj)=height_d(dj-3:dj)

                                IF (sum(rain_mark((dj-3):dj)) > 0.0) then
                                rain_flag=rain_flag+1
                   !             ice_rain_flag=ice_rain_flag+1
                                ENDIF
                        ENDIF ! END 418

                !===========inquire water layer=========================
                   IF (lwc_s1(i) > 0.0) water_flag_d=water_flag_d+1

                ENDDO

        !=============================================================
        !        IF (ice_flag > 0) then ! if ice cloud is present
        !        both cloudy and clear sky are calculated
        !=============================================================       

                OPEN (200,file='IC.DAT')

                OPEN(300, file="WATER.DAT")!,form="unformatted")

                OPEN (600, file="atm_cond.dat")
                

        !===========start each profiles========================
                ice_lay_1=0.0
                ice_lay_2=0.0
                water_lay_1=0.0
                water_lay_2=0.0
                k1=1
                k2=1

                DO hi=1,NCOL_S  
                    !interpolate dardar resolution to cloudsat   
       ! average DARDAR
                dj=4*hi
                mean_iwc=0.0
                mean_rei=0.0
                mean_hgt=0.0
                mean_iwc_1=0.0
                mean_rei_1=0.0
                mean_hgt_1=0.0
                no_iwc=0

        !===================begin to write ice file=================
                IF (ice_flag > 0) THEN

                DO i=0,3

                        IF (dj <= 418) THEN
                        IF ( iwc_s1(dj-i) > 0 .AND. &
                         iwc_s1(dj-i) <= 10.0 &
                        .AND. rei_s1(dj-i) <= 108.0) then
                         mean_iwc=mean_iwc+iwc_s1(dj-i)
                         mean_rei=mean_rei+rei_s1(dj-i)
                         mean_hgt=mean_hgt+height_d(dj-i)
                         no_iwc=no_iwc+1 
                        ENDIF
                        ENDIF
                ENDDO !end i
                
                
                IF (no_iwc /= 0) then
                        mean_hgt_1= mean_hgt/no_iwc
                        mean_iwc_1= mean_iwc/no_iwc
                        mean_rei_1=mean_rei/no_iwc
                        ice_lay_1=mean_hgt_1
               ! when layer begins
                if (ice_top == 0 ) &
                write(200,"(x,f6.3,2x,f12.6,2x,f6.2)") &
                       mean_hgt_1+0.12,0.0,0.0
                
                !judge ice layer
                if (abs(ice_lay_2-ice_lay_1)>0.96 .and. ice_lay_2 /=0)&
                then 
                       write(200,"(x,f6.3,2x,f12.6,2x,f6.2)") &
                        (ice_lay_2+ice_lay_1)/2.0,0.0,0.0
                endif

                        write(200,"(x,f6.3,2x,f12.6,2x,f6.2)") &
                        mean_hgt_1,mean_iwc_1,mean_rei_1
                        ice_lay_2=ice_lay_1

                        ice_top=ice_top+1 
                ENDIF !end no_iwc

                ENDIF ! end ice_flag
        !====================water part=============================
!                IF (water_flag_d > 0) THEN

                mean_hgt=0.0
                mean_hgt_1=0.0
                no_rain=0
                rain_hgt=0


                IF (rain_flag > 0 .OR. water_flag_d > 0) THEN 
        !if water cloud present 

                water_lay_1=hgt_s(hi)
                

                IF (lwc_s1(hi) > 0.0 .AND. lwc_s1(hi) <=15.0 &
                   .AND. rew_s1(hi)>0.0 .AND. rew_s1(hi)<1000.0) then
                        
                        if (rew_s1(hi) > 25) rew_s1(hi)=25

                        if (water_top == 0) &
                        write(300,"(x,f6.3,2x,f12.6,2x,f6.2)") &
                        hgt_s(hi)+0.12,0.0,0.0

 
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



                IF (dj <=418) no_rain=count(rain_mark(dj-3:dj)>0.0)

                IF (no_rain >0) then
                mean_hgt=sum(rain_mark(dj-3:dj))/no_rain
                mean_hgt_1=mean_hgt
                ENDIF

                !when there is no water cloud 

                IF (water_flag_d == 0 .and. no_rain>0) then

                       
                        if (rain_top == 0) then
                                water_lay_2=mean_hgt_1+0.48
                               write (300,"(x,f6.3,2x,f12.6,2x,f6.2)"),&
                                mean_hgt_1+0.12,0.0,0.0
                        endif
 
                       rain_top=rain_top+1
 
               ENDIF

!                        print *,mean_hgt_1,water_lay_2,water_lay_1 
                ! rain layer is lower than water layer, and not insert
                ! into water layer
                IF (mean_hgt_1 < water_lay_2 .and. no_rain > 0&
                        .and. (water_lay_2-water_lay_1)>=0.24) then
                        
                        write(300,"(x,f6.3,2x,f12.6,2x,f6.2)")&
                        mean_hgt_1,rain_value,rain_size
                        water_lay_2=mean_hgt_1 
                        rain_flag_s=rain_flag_s+1
                ENDIF
!                ENDIF !end write water profile

                ENDIF ! end rain_flag or water_flag_d 
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

                ENDDO !end hi

!                 print*,ice_flag,ice_rain_flag,&
!                        rain_flag_s,water_flag_d,water_flag_s
                close(200)
                close(300)
                close(600)

!                print *, rain_flag,ice_flag,water_flag_s,ice_water_lay
 
                !******* get standard atmosphere file********************
                IF (abs(temp_lat) <= 23.5 ) atm_std_file="afglt.dat"
                
                IF ( 23.5 < temp_lat .AND. temp_lat < 66.5) then
                       if (Julday <180) then 
                                atm_std_file="afglms.dat"
                        else if (Julday >= 180) then 
                                atm_std_file="afglmw.dat"
                      end if
                ELSE IF (66.5 <= temp_lat .AND. temp_lat <=90) then       

                        if (Julday <180) then 
                                atm_std_file="afglss.dat"
                        else if (Julday >= 180) then 
                                atm_std_file="afglsw.dat"
                        end if

                ELSE IF (-66.5 < temp_lat .AND. temp_lat <-23.5) then
                        if (Julday <180) then 
                                atm_std_file="afglmw.dat"
                        else if (Julday >= 180) then 
                                atm_std_file="afglms.dat"
                        end if
                        
                ELSE IF (-90 <= temp_lat .AND. temp_lat < -66.5) then      

                        if (Julday <180) then 
                                atm_std_file="afglsw.dat"
                        else if (Julday >=180) then  
                                atm_std_file="afglss.dat"
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

!       print *,si,ice_flag,rain_flag_s,&
!                water_flag_s,sky_index(si)

        !================SW computation===============================
        ! once clouds present, assume them stay there a whole day(day
        ! and night)

                call sun_sza(temp_lat,Julday,hour,yearlength,noon_sza,&
                        daylength)

                SZA_noon(si) = noon_sza

!                print *,temp_lat,noon_sza,daylength

                IF (noon_sza < 90 .and. daylength < 24.0) THEN ! not perpetual night

                        hour_sza=90.0-(h_index-0.5)*&
                                ((90.0-noon_sza)/N_h)                                
                ENDIF

                IF (noon_sza < 90 .and. daylength == 24.0) THEN
                        hour_2=12.0
                        call sun_sza(temp_lat,Julday,hour_2,&
                                yearlength,midn_sza,daylength)
                        hour_sza=midn_sza-(h_index-0.5)*&
                                ((midn_sza-noon_sza)/N_h)
                ENDIF

                
                !==== calculate radiation=====================                 
!                IF (ice_flag >0) then
!                call date_and_time(values=values)
!                print *,values
!                endif
                
                lat_scr=nint((90-temp_lat-0.25)/0.5)
                lon_scr=nint((temp_lat+180-0.25)/0.5)
                mon_scr=(day_2/16)+1
                vis_abd=albedo_mon(lon_scr,lat_scr,mon_scr,:)
                vis_albedo(:,si)=float(vis_abd)/1000.0
!                print *,temp_lat,temp_lon

                DO icloudy=Ncloud,3

         
               IF (noon_sza< 88 .AND. vis_abd(1) > 0) THEN  ! when the sun presents

                !****get albedo *************************************

                OPEN (900, file="sw_albedo.dat")

                DO wi=1,N_wave
                        write(900,*),wave(wi),&
                                vis_abd(wi)/1000.0
                ENDDO

                write(900,*) '100000 0.0'
                CLOSE(900)

!               print *, temp_lat,temp_lon,vis_abd,noon_sza
                OPEN (500,file="swinput.inp")
                        write(500,*) "solar_file data/solar_flux/fu"
                        write(500,*) "atmosphere_file /opt/libRadtran/&
                           share/libRadtran/data/atmmod/"//atm_std_file
                        write(500,*) "source solar"
                        write(500,*) "radiosonde atm_cond.dat H2O MMR &
                                        O3 MMR"
                        write(500,*) "rte_solver twostr"
                        write(500,*) "wavelength 250.  4000."
                        write(500,*) "correlated_k fu"
                        write(500,*) "sza SZA"
                        write(500,*) "albedo_file  sw_albedo.dat" 
                        IF (icloudy==1 .AND. ice_flag > 0) then
                        write(500,*) "ic_layer"
                        write(500,*) "ic_file IC.DAT"
                        write(500,*) "ic_properties yang"
                        write(500,*) "ic_habit rough-aggregate"
                        swout="swcloudy.out"
                        ELSE IF (icloudy == 2) then
                        swout="swnoice.out"
                        ELSE IF (icloudy == 3) then
                        swout="swclear.out"
                        ENDIF

                        write(500,*) "aerosol_haze 6"
                        write(500,*)  "aerosol_visibility 50"
                        write(500,*) "aerosol_season 1"
                        write(500,*) "aerosol_vulcan 1"

                        IF (icloudy /= 3 ) THEN
                        !totally clear
                              IF ((water_flag_s+rain_flag_s >=2))&
                                then
                                write(500,*) "wc_layer"
                                write(500,*) "wc_file WATER.DAT"
                                write(500,*) "wc_properties mie"
                        ENDIF

                        ENDIF

                        write(500,*) "zout 0 1 2 3 4 5 6 7 8 9 10 11 &
                          12 13 14 15 16 17 18 19 20 21 22 23 24 25"
                        write(500,*) "output_user zout sza p T eglo &
                                         eup  heat"
                        write(500,*) "output sum"
                        write(500,*) "quiet"
                close(500)
                
                OPEN (700,file="sw.sh")
                        write(700,*) "#!/bin/bash"
                        write(700,*) "for sza in", hour_sza
                        write(700,*) "do"
                        write(700,*) "sed s/SZA/$sza/ swinput.inp &
                                 swinput.inp > uvspec.inp"

                        write(700,*) "uvspec < uvspec.inp >>"//&
                                trim(swout)
                        write(700,*) "rm -f uvspec.inp"
                        write(700,*) "done"
                close(700)
                
                call system("sh sw.sh")
                
!                call system("rm -f sw.sh")
                
                  
                miss_value=-99.0
                !===deal with results==========
                inquire(file=swout,size=file_size)
!                 print *, file_size
                
                IF (file_size == 0 .or. file_size == -1) then
                        FD_swa(:,si,:)=miss_value
                        FU_swa(:,si,:)=miss_value
                        heat_swa(:,si,:)=miss_value
                        print *,si
                        goto 1000               
                ENDIF ! END NO VALID FILES
                call read_output(swout)
                pres_swa(:,si)=pres_out
                temp_swa(:,si)=temp_out

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

                
                ENDIF  ! IF SZA<90
              
                IF (noon_sza >=88) then
                pres_swa(:,si)=miss_value
                temp_swa(:,si)=miss_value
                FD_swa(:,si,icloudy)=miss_value
                FU_swa(:,si,icloudy)=miss_value
                heat_swa(:,si,icloudy)=miss_value
                ENDIF ! end sw calculat
!=================LW computation==============================
  1000          OPEN (400,file="lwinput.inp")
                        write(400,*) "solar_file data/solar_flux/fu"
                        write(400,*) "atmosphere_file /opt/libRadtran/&
                           share/libRadtran/data/atmmod/"//atm_std_file
                        write(400,*) "radiosonde atm_cond.dat H2O MMR &
                                        O3 MMR"
                        write(400,*) "source thermal"
!                        write(400,*) "nstr 6"
                        write(400,*) "rte_solver twostr"
                        write(400,*) "wavelength 4000.  100000."
                        write(400,*) "correlated_k fu"
                        
                        IF (icloudy==1 .AND. ice_flag > 0) then
                        write(400,*) "ic_layer"
                        write(400,*) "ic_file IC.DAT"
                        write(400,*) "ic_properties yang"
                        write(400,*) "ic_habit rough-aggregate"
                        lwout="lwcloudy.out"
                        ELSE IF (icloudy == 2) then
                        lwout="lwnoice.out"
                        ELSE IF (icloudy == 3) then
                        lwout="lwclear.out"
                        ENDIF

                        IF (icloudy /= 3) then
                        IF ((water_flag_s+rain_flag_s >= 2))then
                                write(400,*) "wc_layer"
                                write(400,*) "wc_file WATER.DAT"
                                write(400,*) "wc_properties mie"  
                                ENDIF
                        ENDIF
       
                        write(400,*) "output_file  ",trim(lwout)
                        write(400,*) "zout 0 1 2 3 4 5 6 7 8 9 10 11 &
                            12 13 14 15 16 17 18 19 20 21 22 23 24 25"
                        write(400,*) "output_user zout p T eglo &
                                        eup heat"
                        write(400,*) "output sum"
                        write(400,*) "quiet" 
                close(400)
                
                !********** if model fail**************************               
                call system("uvspec <lwinput.inp> mytest.out")
    
                inquire(file=lwout,size=file_size)

                IF (file_size == 0 .or. file_size == -1) then
                FD_lwa(:,si,:)=miss_value
                FU_lwa(:,si,:)=miss_value
                heat_lwa(:,si,:)=miss_value
                print *,si
                goto 2000              

                ENDIF

                call read_output(lwout)
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
                ELSE IF (Ncloud ==2) then ! if only water present 
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
               
  2000         ENDDO ! end clear and cloudy,icloudy
!                stop
!               call system("rm -f IC.DAT")
!               call system("rm -f WATER.DAT")
!               call system("rm -f swinput.inp")
!               call system("rm -f lwinput.inp")


                 call system("rm -f swcloudy.out")
                 call system("rm -f swclear.out")
                 call system("rm -f lwcloudy.out")
                 call system("rm -f lwclear.out")
                 call system("rm -f swnoice.out")
                 call system("rm -f lwnoice.out")

 
        ENDDO   ! end swath

        
        end subroutine core_simulate
                
        subroutine sun_sza(local_lat,Julday,hour,yearlength,sza,&
                daylength)
        implicit none
        real :: lat,hour,yearlength,sza,solar_dec,delda,h,hour_angle,&
                lamda,daylength
        real(kind=4) :: local_lat        
        integer :: Julday
        real, parameter :: pi=3.1415926

        lat=local_lat*pi/180.0
        lamda=(360.0*(Julday/yearlength))*pi/180.0

        solar_dec=sin(23.5*pi/180.0)*sin(lamda)

        delda=asin(solar_dec)

        h=-tan(lat)*tan(delda)
        hour_angle=(360.0*hour/24.0)*pi/180.0

        sza=sin(lat)*solar_dec+cos(lat)*cos(delda)*cos(hour_angle)

        sza=acos(sza)*180.0/pi

        h=-tan(lat)*tan(delda)
        daylength=24*acos(h)/pi


        IF (sza > 90) then 
                daylength=0.0
        ENDIF

        IF (sza < 90 .and. daylength /= daylength) then
                daylength=24.0
        ENDIF
!        print *,Julday,local_lat,sza
!        stop
        end subroutine sun_sza 
