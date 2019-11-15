#!/bin/bash
#SBATCH -J metahit
#SBATCH --time=2:00:00
#SBATCH --cpus-per-task=4
#SBATCH --array=1-26

module purge
module load default-login

Rscript disbayes-run.r
