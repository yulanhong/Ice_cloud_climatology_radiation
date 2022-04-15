        module global

        implicit none

        !===== variable to read in ===========================
        real(kind=4),allocatable :: iwc(:,:),rei(:,:),vis_od(:)
        real(kind=4),allocatable :: lon(:),lat(:)
        integer(kind=1),allocatable :: DN_flag(:)
        integer(kind=2),allocatable :: lwc(:,:),ice_frac(:,:), &
                                rew(:,:),height_s(:,:),rwc(:,:)
        !====aerosol===========================================
        real(kind=4),allocatable :: aero_ext(:,:),aero_ssa(:,:),&
                                aero_asy(:,:),lidlwc(:,:)
        integer(kind=1),allocatable :: aero_flag(:),rain_flag(:),&
                        lidlwc_flag(:)

        real (kind=4),allocatable :: wv_sh(:,:),pres(:,:),&
                                temp_t(:,:),ozone3(:,:),skin_T(:)
      
        !===========variable to write out============================ 
        real,allocatable :: pres_swa(:,:),temp_swa(:,:),FD_swa(:,:,:),&
                FU_swa(:,:,:),heat_swa(:,:,:),vis_albedo(:,:),&
                FD_lwa(:,:,:),FU_lwa(:,:,:),heat_lwa(:,:,:)

        integer(kind=2),allocatable :: sky_index(:),&
                Day_Len(:)

        integer(kind=2),allocatable :: wr_pres(:,:),wr_FU_swa(:,:,:),&
                wr_FU_lwa(:,:,:),wr_FD_swa(:,:,:),&
                wr_FD_lwa(:,:,:),wr_temp(:,:),wr_albedo(:,:)

        integer, parameter :: NM_hgt=26,NCOL_D=436,N_h=12,N_wave=7,&
                N_cloud=4
        integer :: NROW,NCOL_S
        common /file_info/ NROW,NCOL_S

        end module

