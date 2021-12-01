#!/bin/bash
#SBATCH -p academic
#SBATCH --job-name=zclement		# Job name (My harvard ID)
#SBATCH --mail-type=NONE		# Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --cpus-per-task=1		# Number of CPU cores per task
#SBATCH --mem=1GB				# Memory
#SBATCH --time=03:00:00			# Time limit hrs:min:sec
#SBATCH --output=output_%j.log	# Standard output and error log
#SBATCH --error=error_%j.log	# Error output
#SBATCH --output=NONE			# Standard output and error log 
#SBATCH --error=NONE			# Error output
#SBATCH --array=1-10			# how many times you want it to run. 1-2 makes it run 2 times. 

module load R/4.1.0-fasrc01
Rscript replicate_mse.R 50 $SLURM_ARRAY_TASK_ID
