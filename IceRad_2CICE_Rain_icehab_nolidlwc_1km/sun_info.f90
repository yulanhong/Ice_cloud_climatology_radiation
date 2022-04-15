       subroutine sun_info(local_lat,lon,Julday,hour,sza,daylength)
        implicit none
        real :: lat,sza,solar_dec,delda,h,hour_angle,&
                lamda,daylength,et,eqt
        real(kind=4) :: lon,local_lat
        integer :: Julday
        integer (kind=2) :: hour
  ! 		real :: hour
	     real, parameter :: pi=3.1415926

        lat=local_lat*pi/180.0
        !lamda=(360.0*(Julday/yearlength))*pi/180.0
        lamda=2*pi*Julday/365.0
!        solar_dec=sin(23.5*pi/180.0)*sin(lamda)


        delda=0.006918-0.399912*cos(lamda)-0.006758*cos(2.*lamda)&
                -0.002697*cos(3.*lamda)+0.070257*sin(lamda)+&
                0.000907*sin(2.*lamda)+0.001480*sin(3.*lamda)
        h=-tan(lat)*tan(delda)

        et=2.*pi*Julday/366.
        eqt= 0.0072*cos(et)-0.0528*cos(2.*et)-0.0012*cos(3.*et)&
                -0.1229*sin(et)-0.1565*sin(2.*et)-0.0041*sin(3.*et)
        hour_angle=(360.0/24.0)*(hour+lon/15.0+eqt-12)*pi/180.0

        sza=sin(lat)*sin(delda)+cos(lat)*cos(delda)*cos(hour_angle)

        sza=acos(sza)*180.0/pi

        h=-tan(lat)*tan(delda)
        daylength=24*acos(h)/pi


        IF (sza > 90) then
                daylength=0.0
        ENDIF

        IF (sza < 90 .and. daylength /= daylength) then
                daylength=24.0
        ENDIF

        end
