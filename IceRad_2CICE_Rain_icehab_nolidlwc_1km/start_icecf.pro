   
   pro start_icecf
   
      year=['2007','2008','2009','2010']
      mon=['01','02','03','04','05','06','07','08','09','10','11','12']
   
      ny=n_elements(year)
      nx=n_elements(mon)
  
      datadir='/data/gdi/b/yulanh/2CICE/'
  
      for yi=1, 1 do begin
      for xi=0, 0 do begin
      for zi=1,30 do begin

	day=(zi+1)/1000.0
	day1=strcompress(string(day),/rem)
	day2=strmid(day1,2,3)

      datadir1=datadir+year[yi]+mon[xi]
  
      fname=file_search(datadir1,'2008'+day2+'*')
      nodes=n_elements(fname)
      nodes=string(nodes)
      nodes=strcompress(nodes,/rem)
      print,year[yi]+mon[xi],' ',nodes

      spawn,'sed s/Nnodes/'+nodes+'/'+' sbatch_submit1.sh > sbatch_submit.sh'
      spawn,'sed s/001/'+day2+'/'+' sbatch_submit.sh > sbatch_submit.sh'
	stop 
     spawn,'sbatch sbatch_submit.sh'
     end 
     end
     end
 end

