        subroutine read_output(str_name,counthour)

	use global,only: NM_hgt, N_h
        implicit none
        
        character(len=100) :: fname,str_name

        real :: hgt,sza,p,t,fd,fu,heat
        integer :: i,j,k,stat,counthour
!        integer,parameter, NM_hgt=21
        logical :: flag

        real :: hgt_out(NM_hgt),pres_out(NM_hgt),temp_out(NM_hgt),&
                fd_out(NM_hgt),fu_out(NM_hgt),heat_out(NM_hgt)

        common /res_out/ pres_out,temp_out,fd_out,fu_out,heat_out

        !==========================================================
        pres_out=0.0
        temp_out=0.0
        fd_out  =0.0
        fu_out  =0.0
        heat_out=0.0
         
        flag=.true.
        i=1

        fname=trim(str_name)

        IF (fname(1:2) == "lw") then 

        open(800,file=fname,status="old",action="read")
           !     access="direct")
        do while (flag)
                read(800,*,iostat=stat),hgt_out(i),pres_out(i),&
                temp_out(i),fd_out(i),fu_out(i),heat_out(i)     
                i=i+1    
        if (stat /=0 .or. i== NM_hgt+1) exit
        enddo
        close(800)
        
        ELSE

        pres_out=0.0
        temp_out=0.0
        fd_out  =0.0
        fu_out  =0.0
        heat_out=0.0

        i=1

        open(900,file=fname,status="old",action="read")

        do while (flag)
                read(900,*,iostat=stat) hgt,sza,p,t,fd,fu,heat
                j=mod(i,NM_hgt)	
		if (j==0) j=NM_hgt
	!	print *,j,fd
		pres_out(j)= pres_out(j) + p
                temp_out(j)= temp_out(j) + t
                fd_out(j)  = fd_out(j) + fd
                fu_out(j)  = fu_out(j) + fu
                heat_out(j)= heat_out(j)  + heat
                i=i+1
        if (stat /=0 .or. i== counthour*NM_hgt+1) exit
        enddo
               close(900)

               pres_out=pres_out/counthour  
               temp_out=temp_out/counthour
               fd_out  =fd_out/N_h
               fu_out  =fu_out/N_h
               heat_out=heat_out/N_h       
	 
        ENDIF
        !print *,N_h,counthour
        !print *,NM_hgt,counthour,fname,fd_out
        
	end
