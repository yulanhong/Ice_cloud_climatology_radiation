#!/bin/tcsh
#SBATCH --job-name="0028droxtal"
#SBATCH -n 14
#SBATCH --time=144:00:00
#SBATCH --mem-per-cpu=4096
#SBATCH --mail-type=FAIL
#SBATCH --mail-type=END
#SBATCH --mail-user=yulanh@illinois.edu
module load gnu/libRadtran-2.0.1-gnu
#RunDir=/data/keeling/a/yulanh/a/IceRad_2CICE_Rain_8element_Parallel0
echo "183" | mpirun -n 1 ./ice_cf
