        subroutine write_hdf5 ()
        
        implicit none
        include 'para.file'
        integer :: fid,swathid,access,status
        integer(kind=16) :: i,buffer(10,10),j
        character(len=20) ::fname, swathname,fieldname
        
        integer, parameter :: DFACC_CREATE = 4,X_LENGTH=10,Y_LENGTH=10,&
                DFNT_INT16=22,HDFE_NOMERGE=0,DFACC_RDWR=3
        integer ::  start(2),stride(2),edge(2)
        integer swopen,swcreate,swdetach,swclose,swdefdim,swwrfld,&
                swwrattr,swdefdfld,swattach
!
        fname="test.hdf"
        swathname="FORCING"
        fieldname="latitude"

        FORALL(i=1:10,j=1:10) buffer(i,j)=i
        start  ( 1 ) = 0
        start  ( 2 ) = 0
        edge   ( 1 ) =10 
        edge   ( 2 ) = 10
        stride ( 1 ) = 1
        stride ( 2 ) = 1
        fid=swopen(fname,DFACC_CREATE)
        swathid=swcreate(fid,swathname)
        

        status=swdetach(swathid)
        status=swclose(fid)

        fid=swopen(fname,DFACC_RDWR)
        swathid=swattach(fid,swathname)
        status=swdefdim(swathid,'a','b',10,10)
        print *,status
        status= swdefdfld(swathid,"latitude","a,b",&
                DFNT_INT16,HDFE_NOMERGE)
        print *, fid,swathid,status
        status=swwrfld(swathid,"latitude",start,stride,edge,buffer) 
        print *,status
        i=3
        status=swwrattr(swathid,"datatype",DFNT_INT16,1,i)
        print *,status
        status=swdetach(swathid)
        print *,status
        status=swclose(fid)

        print *,status
        
!
        end subroutine write_hdf5
