#!/bin/bash

#PBS -N part2 

#PBS -l nodes=13:ppn=32

#PBS -l walltime=48:00:00

# combine PBS standard output and error files

#PBS -j oe

# mail is sent to you when the job starts and when it terminates or aborts

#PBS -m bea

# specify your email address

#PBS -M yulanh@illinois.edu

#change to the directory where you submitted the job

cd $PBS_O_WORKDIR

#include the full path to the name of your MPI program

aprun -n 414 ./ice_cf2

exit 0
