
        subroutine Julday2month(year,Julday,month)


        implicit none
        integer :: year,month,Julday

        If (year == 2008) then
        
        select case (Julday)
                case (1:31)
                month=1
                case (32:60)
                month=2        
                case (61:91)
                month=3
                case (92:121)
                month=4        
                case (122:152)
                month=5        
                case (153:182)
                month=6        
                case (183:213)
                month=7        
                case (214:244)
                month=8        
                case (245:274)
                month=9        
                case (275:305)
                month=10        
                case (306:335)
                month=11        
                case (336:366)
                month=12        
        end select

        else  

        select case (Julday)
                case (1:31)
                month=1
                case (32:59)
                month=2        
                case (60:90)
                month=3
                case (91:120)
                month=4        
                case (121:151)
                month=5        
                case (152:181)
                month=6        
                case (182:212)
                month=7        
                case (213:243)
                month=8        
                case (244:273)
                month=9        
                case (274:304)
                month=10        
                case (305:334)
                month=11        
                case (335:365)
                month=12        
        end select

        EndIf

                
        end
