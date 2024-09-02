#!/bin/bash
#Set job requirements (note set time 1.5 to 2x longer than expected)
#SBATCH -J net_49152

#SBATCH -t 50:00:00 
#SBATCH -p genoa
#SBATCH --exclusive 
#SBATCH --nodes=1
#SBATCH --tasks-per-node=1  
#SBATCH --cpus-per-task=192 

#Send email at start en end
#SBATCH --mail-type=BEGIN,END
#SBATCH --mail-user=k.park@uva.nl

#SBATCH --output=log/%x-%j.out                 # where to store the output ( %j is the JOBID )
#SBATCH --error=log/%x-%j.err                  # where to store error messages

#Loading modules
module load 2023
module load R-bundle-CRAN/2023.12-foss-2023a


Rscript $HOME/code/net_49152.R 

cp -r res* $HOME/results/