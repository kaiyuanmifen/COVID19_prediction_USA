#!/bin/bash

sbatch RunDifferentExperiment.sh "JHK_New_confirmed"  1 0 1 0
sbatch RunDifferentExperiment.sh "JHK_New_confirmed"  1 1 1 0
sbatch RunDifferentExperiment.sh "JHK_New_confirmed"  0 0 0 1
sbatch RunDifferentExperiment.sh "JHK_New_confirmed"  0 1 0 1

sbatch RunDifferentExperiment.sh "covidtracking_new_deaths"  1 0 1 0
sbatch RunDifferentExperiment.sh "covidtracking_new_deaths"  1 1 1 0
sbatch RunDifferentExperiment.sh "covidtracking_new_deaths"  0 0 0 1
sbatch RunDifferentExperiment.sh "covidtracking_new_deaths"  0 1 0 1