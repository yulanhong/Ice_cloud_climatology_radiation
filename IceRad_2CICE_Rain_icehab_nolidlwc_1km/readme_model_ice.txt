20150227 
refine core_simulate.f90, rejust surface albedo in high latitudes. If SZA>0 and modis_albedo<0
then adopt assumed values. 
Location: dendrite IceRad_daily_test_dardar

---------------------------------------------done-----------------------------------------

201503-10
Test:
add aerosol from CALIPSO to IceRad_Instant_2c-ICE on dendrite
ssa is interpreted from libRadtran and asymmetric factor is fixed as 0.80

and refine core_instant_simulate.f90, rejust surface albedo in high laitudes. 
Write aerosol_flag

---------------------------- background aerosol effect-------------------------
background aerosols are from libradtran
test swath 04495,2007062 day
with aerosol sw forcing toa: -16.62, surface:-17.52
without aerosol sw forcing toa: -17.44, surface,-18.66
with aerosol lw  forcing toa: 26.53, surface:10.15
without aerosol lw forcing toa:26.49,surface:10.15

----------------------test CALIPSO aerosol----------------------------------
test day:20080101, swath 08992
results are better than using libradtran background aerosols
SW up boa: 
calispso: 51.97 , cldsat: 53.98, libratran:52.76
SW up toa: 262.34, 240.41,280.89
lw up toa, 220.34,220.71,220.66
lw dn boa, 299.17,303.14,298.75
lw up boa,352.65,350.79,352.65
sw dn boa,484.66,513.27,496.92
sw dn toa,796.09,814.87
--------------------------2015/03/11------------------------------------------
1. modify DARDAR_Daily_Test on dendrite
1) cacluate SW radiation in daytime not in nighttime
2) add aerosols from CALIPSO
3) write aero_flag
4) delete writing SZA at noon

----------------------2015/03/12----------------------------------------------
1
 modify 2CICE_Daily_Test on dendrite
---------------------2015/03/14-----------------------------------------------
add asymmetric factor and ssa from OPAC database to DARDAR_Daily_TEST and 
2CICE_DAILY_TEST on dendrite

add asymmetric factor and ssa from OPAC database to 2C-ICE_INSTANT on dendrite

----------------2015/03/15-------------------------------------------------
dendrite runs 2cice-daily 20080101
rosette runs dardar-daily 20080101

----------- 2015/03/16--------------------------------------------------------
1. add asymmetric factor and ssa from OPAC	database to DARDAR_INSTANT on aero
2. copy 2C-ICE instant/DARDAR_DAILY/2C-ICE_DAILcY to aero

3. check daily_dardar and daily_2cice, work well
-------------2015/03/17----------------------------------------------------
correct Ice_INstant_dardar on mizu: add output_files sw

---------------2015/03/18--------------------------------------------------
copy 2C-ICE-DAILY from dendrite to aero
add check input_data files(ecmwf and 2cice) on aero,copy to ice and dendrite 
add check input_data files on crystallus,copy to rosette, cumulus, aqua
copy IceRad_DARDAR_1 from aero to hpc

--------------2015/03/19-------------------------------------------------------
DARDAR_INSTANT on aero has been revised on sza_info inputs: hour is real in core_simuate_instant and in sza_info
Note that DAILY_DARDAR and DAILY_2CICE hour is integer 

-----------2015/03/21----------------------------------------------------
check DARDAR_INSTANT, works well
found aerosols is not included in January ICE_RAD claculations, including daily and instant.Keep this data for uncertainties analysis.

----------------2015/003/26----------------------------------------------
Test daily, DARDAR SW: OK
	    DARDAR LW: OK
Test daily, 2C-ICE LW: OK
	    2C-ICE SW: OK 
---------------2015/03/27-------------------------------------------------
test ice cloud forcing, obs_num is for both day and night,
but sw cloud forcing is only calculated in daytime.
Program for average ice cloud forcing is need to be revised.	    
Also, no need to correct SW downward fluxes
-------------2015/03/28--------------------------------------------------
test daily: DARDAR SW and LW OK
test daily: 2C-ICE SW and LW OK 

--------------2015/04/01-------------------------------------------------
add rain to Instant 2C-ICE, New folder Instant_2c-ice_rain
---------------2015/04/02------------------------------------------------
1.check Instant 2C-ICE RAIN, OK
2.add Rain to Daily 2C-ICE:
1) record whether have cldsat rain
3.add rain to Instant dardar on aero
-------------2015/04/03--------------------------------------------------
1. check 2c-ice daily with rain on aero
1) rain flag is not right--do correction (not finish)
2) check instant_dardar on aero: Ok
---------------2015/04/04------------------------------------------------
1)revise dardar_instant_rain to add noice and clear sky situation
2)no need to repeat daily noice and clear situation in both 2c-ice and dardar
--------------2015/04/05---------------------------------------------------
check revise dardar_instant_rain: OK
1) revise 2C-ICE daily: delete noice and clear sky situation
------------2015/04/06----------------------------------------------------
1. test global mean planetary albedo by adding area weights
2. revice DARDAR_daily, add cloudsat rain
1) change IceRad_2CICE_RAIN N_cloud=1,icloudy=1
---------- 2015/04/08-----------------------------------------------------
check albedo by adding a weighting function (cosine), results are OK;
albedo is too large before is due to high latitude reflection contributes too much!!
----------2015/04/09------------------------------------------------------
1) note that when average dardar iwc, iwc should include zero value, but effect radius should not include zero. Revice on aero DARDAR_Daily_Rain then copy DARDAR_Daily_Rain to mizu, rosette,cumulus,and crystallus

2) copy 2CICE_DAILY_RAIN to dendrite, and ice
----------2015/04/15---------------------------------------------------------
1) check Instant_dARDAR_rain_origin: 
Have no too much difference using the original grid and average grid
2) check DARDAR_Daily_rain_orgin:
Almost no difference using the roginal grid and the average grid
-----------2015/04/16-----------------------------------------------------
revise CWC daily_input on aero
-----------2015/04/17----------------------------------------------------
copy DARAR_daily_rain_orgin to crystallus and rosette
----------2015/04/30------------------------------------------------------
test CWC on hpc: OK

open CWC on aero
---------2015/05/05---------------------------------------------------------
test CWC on hpc:OK
---------2015/05/06---------------------------------------------------------
test CWC on hpc:OK
test CWC on aero and dendrite: OK
-------------2015/05/25/-------------------------------------------------
test 2C-Ice ON HPC: OK
-------------2015/06/12--------------------------------------------------
check again wether 2C-ICE and DARDAR produce the same results without ice situations
test on aero: IceRad_2CICE_Rain_all
almost the same as DARDAR in the clear and in noice situation. There exist 
differences around 0.1 between two datasets, which might be due to approximationfrom float to integer type when writing the results. 
-------------2015/07/05------------------
revice 2C-ICE_rAIN_1 on HPC, test 2008.07.06

------------2016/04/05-----------------
add iceonly to model and can change icehabit
IceRad_DARDR_RAIN_orig_icehabit
IceRad_2CICE_Rain_Icehabit 
also add an interation to get iceonly fluxes 

--------2016/07-------------
update version will used updated 2C-ICE P1 version
and 2B-CWC-RVOD for liquid water clouds

continue to update to include CALIPSO low-level warm clouds?
DARDAR-MASK warm clouds --> no CWC but CALIPSO---> probal water
(yes, done with high level super liquid)

--------2016/08---------------
update version using libRadtran-2.0.1, which include yang2013
using 2B-CWC-RVOD for liquid cloud
correct aerosol input file:
1. rescale to atmosphere grid (on ice server)

-------2016/09----------------
update a version which is easy to test instantanous fluxs for examples

----2016/10------------------
1.update a daily version which include caliop lwc(in cloudsat resolution)
2. if cwc-rvod or lidar exist lwc, rain-profile would not used due to
   cwc-rvod doesn't sperate rain and cloud
3. add cloud top to a layer in the resolution of profile
(IceRad_DARDAR_orig_caliop_vy13) 

---- 2018/04-----------------
1. add parallel computation
2. no DARDAR data anymore; 
   1) delete day-night-flag (DN_flag),using SZA instead
   2) for SW daily average, assume the cloud existing the whole day, no matter cloud occurs at night or daytime
   3) fu  (reptran coarse is too slow)
   4) change aerosols optical properties? (included only in SW and seted ssa, asy same as Alexander V. Matus and Tristan 2015)
3. change surface albedo water from [26,31,42,61,100,60,20] to [26,74,87,31,0,0,0] Bowker No.151 
4. record SW instantaneous fluxes and daily average      
