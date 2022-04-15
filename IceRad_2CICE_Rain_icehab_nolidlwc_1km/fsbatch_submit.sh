#!/bin/tcsh
 
#SBATCH --job-name="job001"
#SBATCH -n 20 
#SBATCH --time=144:00:00
#SBATCH --mem-per-cpu=4096
#SBATCH --mail-type=FAIL
#SBATCH --mail-type=END
#SBATCH --mail-user=yulanh@illinois.edu

module load gnu/libRadtran-2.0.1-gnu
 
set RunDir=/data/gdi/b/yulanh/IceRad_2CICE_Rain_Icehabit_Parallel

 
echo "001" | mpirun -n 15 ./ice_cf 
