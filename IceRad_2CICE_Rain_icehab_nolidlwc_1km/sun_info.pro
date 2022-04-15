pro sun_info,local_lat,Juday,hour,yearlength,sza,daylength

;***************solar zenith angle**********************************
	pi=3.1415926
	lat=local_lat*pi/180

	lamda=(360*(Juday/yearlength))*pi/180

	solar_dec=sin(23.5*pi/180.)*sin(lamda)

	delda=asin(solar_dec)

	h=-tan(lat)*tan(delda)
	hour_angle=(360*hour/24.)*pi/180

	sza=sin(lat)*solar_dec+cos(lat)*cos(delda)*cos(hour_angle)
        
	sza=acos(sza)*180/pi

;***************** daylegth****************************************

	h=-tan(lat)*tan(delda)
        daylength=24*acos(h)/pi

	print,sza,daylength
end
