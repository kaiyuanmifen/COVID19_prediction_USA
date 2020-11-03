#!/bin/bash
#SBATCH -t 0-4:59                         # Runtime in D-HH:MM format
#SBATCH -p short                          # Partition to run in
#SBATCH --mem=20000

module load gcc/6.2.0 
module load R/4.0.1
#Grouthturth  FeatureSelection Smoothing IncludeMechanisticPrediction_ahead_of_time Autoregression
#C:\"Program Files"\R\R-4.0.0\bin\Rscript.exe PredictiveModels.R "covidtracking_new_deaths"  1 0 1 0 "IHME_prediction.csv" "GroundTruthCorrelation"
echo $1
echo $2
echo $3
echo $4
echo $5
echo $6
echo $7
Rscript PredictiveModels.R $1 $2 $3 $4 $5 $6 $7