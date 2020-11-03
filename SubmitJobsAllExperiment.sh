#!/bin/bash


for GroundTruth in "JHK_New_confirmed" "covidtracking_new_deaths"
do 	
	echo $GroundTruth
	for ClusteringMethod in "GroundTruthCorrelation" "ByRegion" "ByParty" "StartingDate"
	do 
	echo $ClusteringMethod

	sbatch RunDifferentExperiment.sh $GroundTruth  1 0 1 0 "IHME_prediction.csv" $ClusteringMethod
	sbatch RunDifferentExperiment.sh $GroundTruth  1 1 1 0 "IHME_prediction.csv" $ClusteringMethod
	sbatch RunDifferentExperiment.sh $GroundTruth  0 0 0 1 "IHME_prediction.csv" $ClusteringMethod
	sbatch RunDifferentExperiment.sh $GroundTruth  0 1 0 1 "IHME_prediction.csv" $ClusteringMethod


	done 
done


