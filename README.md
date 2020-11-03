# State level COVID19 prediction in the USA

This document contains R/Python codes for COVID-19 cases/ incidence death prediction in the U.S. 

DataCuration.sh file contrain commandline to download data from different sources 

DataProcessing*.R filess are used to process raw data from different sources. Among these ,DataProcessing_Leo.R should be used to process data aggregated from different websites

Functions_NCOV.R is the most important file containing functions needed for ARGO based model training and prediction 

PredictiveModels.R run the model training and predicition 

DataAnalysis.R visualize the results 

RunDifferentExperiment.sh and  SubmitJobsAllExperiment.sh run different experiment under various parameter settings 

 NeuralNetworkModels.py,Longer_term_prediction.py and NN_prediction.py traing recurrent artificial neural network based prediciton. 
 
 If you have any questions regarding this repo, please email me at kaiyuanmfien@gmail.com
