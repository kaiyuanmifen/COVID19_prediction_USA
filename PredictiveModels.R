
#Prediction 

#Load functions/packages needed 

packages <- c("ggplot2", "dplyr", "reshape")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
library(ggplot2)
library(reshape)
#library(translateR)
#library("showtext")
library(dplyr)
source("Functions_NCOV.R")

#load data 
AllProvinces=read.csv("../results/OrganizedData_US_countries.csv")
AllProvinces=as.data.frame(AllProvinces)
head(AllProvinces)
AllProvinces$Location=as.character(AllProvinces$Location)
AllProvinces$Date=as.Date(as.character(AllProvinces$Date),origin = "1970-01-01")
max(AllProvinces$Date) 
min(AllProvinces$Date) 
#tail(AllProvinces)


#nrow(AllProvinces)
#names(AllProvinces)


#take arguments into 

args <- commandArgs(trailingOnly = TRUE)
print(args)

#Set modell parameter 
Lags=3#Max lags 
#LagsToInclude=c(1:3,6,9,12,15)#Actual Lags to use 
LagsToInclude=c(1:3)
UseR0_as_predictor=F
B_global_loops=1
Normalization=T
First_index_date=as.Date("2020-08-01",origin = "1970-01-01")#first date to make prediction 




# 
# GroundTruthName="covidtracking_new_deaths"   #Targeted feature for prediction
# FeatureSelection=T
# Smoothing=F
# IncludeMechanisticPrediction_ahead_of_time=T
# Autoregression=F

GroundTruthName="covidtracking_new_deaths"  #Targeted feature for prediction
FeatureSelection=T
Smoothing=T
IncludeMechanisticPrediction_ahead_of_time=T
Autoregression=F
Mechanistic_prediction_file="IHME_prediction.csv"
ClusteringMethod="StartingDate"


GroundTruthName=as.character(args[1])    #Targeted feature for prediction
FeatureSelection=as.logical(as.integer(args[2]))
Smoothing=as.logical(as.integer(args[3]))
IncludeMechanisticPrediction_ahead_of_time=as.logical(as.integer(args[4]))
Autoregression=as.logical(as.integer(args[5]))
Mechanistic_prediction_file=as.character(args[6])
ClusteringMethod=as.character(args[7])



print(paste("groundtruth:",GroundTruthName))
print(paste("FeatureSelection:",as.character(FeatureSelection)))
print(paste("Smoothing:",as.character(Smoothing)))
print(paste("IncludeMechanisticPrediction_ahead_of_time:",as.character(IncludeMechanisticPrediction_ahead_of_time)))
print(paste("Autoregression:",as.character(Autoregression)))
print(paste("Mechanistic prediction file:",as.character(Mechanistic_prediction_file)))
print(paste("clusterting method:",as.character(ClusteringMethod)))

Mechanistic_prediction=read.csv(paste0('../results/',Mechanistic_prediction_file))
#Mechanistic_prediction=Mechanistic_prediction[,2:ncol(Mechanistic_prediction)]



OutlierDataPointRemoval=F


FirstDate_use_data=as.Date("2020-04-20",origin = "1970-01-01")#data usage

Aggregation=7
Binary=F
Clustering=T

if(Autoregression==T){
  Clustering=F 
}
Augmentation=T
IncludeIncrement=T
IncludeWeeklyCumulateive=T

CompareWith="cdc_ili.idea"

daysaheads=c(1,2)
NumberOfSateOutperform=NULL
for (daysahead in daysaheads){

  ExperimentName=GroundTruthName
  if(Smoothing==T){
    ExperimentName=paste0(ExperimentName,"_","smoothed")  
  } else {ExperimentName=paste0(ExperimentName,"_","unsmoothed")  }
  
  if(Autoregression==T){
    ExperimentName=paste0(ExperimentName,"_","autoregression")  
  } 
  
  
  if(IncludeMechanisticPrediction_ahead_of_time==T){
    ExperimentName=paste0(ExperimentName,"_","MechanisticGuided")  
  } 
  
  
  
  ExperimentName=paste0(ExperimentName,"_Agg_",Aggregation,"_ahead_",daysahead,"_",ClusteringMethod) 
  print(paste("Experiment name: ",ExperimentName))

#Filtering data 
# AllProvinces=AllProvinces[AllProvinces$Date>=FirstDate_use_data,]
# Vec=aggregate(AllProvinces[,"JHK_New_confirmed" ],by=list(AllProvinces$Location),sum)
# AllProvinces=AllProvinces[AllProvinces$Location%in%Vec$Group.1[Vec$x>1000],]#remove those very too few numer

#filter by specific target variable
#Vec=aggregate(AllProvinces[,GroundTruthName],by=list(AllProvinces$Location),sum)
#AllProvinces=AllProvinces[AllProvinces$Location%in%Vec$Group.1[Vec$x>(median(Vec$x)/3)],]#remove those very too few numer
#length(unique(AllProvinces$Location))

##Require at least 30 days with >0
# Vec=aggregate(AllProvinces[,GroundTruthName],by=list(AllProvinces$Location),FUN=function(x){sum(x>0)})
# Vec=Vec[Vec$x>=30,]
# AllProvinces=AllProvinces[AllProvinces$Location%in%Vec$Group.1,]
# length(unique(AllProvinces$Location))



#choose features to be included into ARGO
names(AllProvinces)

if (Autoregression==T){Information_to_include=GroundTruthName} else {
  Information_to_include=c("covidtracking_new_deaths","JHK_New_confirmed",
                           "NewHospitalization",
                           "search_intensity" ,
                           "Cuebiq_Mobility", 
                           "Kinsa_ObservedFever","Kinsa_ForecastFever", "Kinsa_AtypicalFeverDelta",
                           "cdc_ili",
                           "Apple_mobility" ,
                           "google_covid" ,"google_covid.symptoms","google_fever"  ,
                           "Twitter_RelatedTweets" )}


MechanisticPredictionsAheadofTime=c( "IHME_allbed_mean","IHME_ICUbed_mean","IHME_InvVen_mean","IHME_admis_mean" ,"IHME_newICU_mean", "IHME_totdea_mean","IHME_deaths_mean")
#Information_to_includeNames=c(Information_to_include,MechanisticPredictionsAheadofTime)
Information_to_includeNames=c(Information_to_include)

  
#names(AllProvinces)[!names(AllProvinces)%in%c("Location","X", "Date" ,"gleammodel_state_name.2","gleammodel_state_name")]
  
  
  
 #Hospitalization, #New death,"deathIncrease",
#c("NewHospitalized" ,"NewICU","deathIncrease" ,"JHK_New_confirmed","hospitalizedCumulative","PositiveRate",
 # "MediaCloudCount","GoogleTrend",
  #"IHME_admis_mean","NEU_prediction")
#New cases 
#c("NewHospitalized" ,"NewICU","JHK_Cumulative_confirmed","deathIncrease" ,"JHK_New_confirmed","death","PositiveRate",
#"MediaCloudCount","GoogleTrend",
#"IHME_totdea_mean","NEU_prediction")


#"Location"                 "positive"                 "negative"                
#"pending"                  "hospitalizedCurrently"    "hospitalizedCumulative"  
# "inIcuCurrently"           "inIcuCumulative"          "onVentilatorCurrently"   
# "onVentilatorCumulative"   "recovered"                "death"                   
# "deathIncrease"            "hospitalizedIncrease"     "negativeIncrease"        
# "positiveIncrease"         "totalTestResultsIncrease" "PositiveRate"            
#                    "MediaCloudCount"          "GoogleTrend:cold"        
# "GoogleTrend:covid"        "GoogleTrend:cough"        "NEU_prediction"          
#"IHME_allbed_mean"         "IHME_ICUbed_mean"         "IHME_InvVen_mean"        
# "IHME_admis_mean"          "IHME_newICU_mean"         "JHK_Cumulative_confirmed"
# "JHK_New_confirmed" "IHME_totdea_mean","NewHospitalized"



DataForRegression=AllProvinces
names(DataForRegression)%in%c(Information_to_include,MechanisticPredictionsAheadofTime,"Location", "Date" )
Mechanistic_prediction_to_use=Mechanistic_prediction


#Starting training

#processing data for additional setting

if(Smoothing==T){
  #Smoothing the targeted variable using weekly data
  for (Location in unique(DataForRegression$Location)){
    Vec=DataForRegression[DataForRegression$Location==Location,GroundTruthName]
    Vec2=Vec
    for(i in 7:length(Vec)){
      Vec[i]=mean(Vec2[(i-6):i])
    }
    DataForRegression[DataForRegression$Location==Location,GroundTruthName]=Vec
    
  }
  
  
}


#include increase at each time step

if(IncludeIncrement==T){
  NewDF=NULL
  #Smoothing the targeted variable 
  for (Location in unique(DataForRegression$Location)){
    Vec=DataForRegression[DataForRegression$Location==Location,Information_to_includeNames]
    Vec=as.data.frame(Vec)
    Vec2=Vec[c(1,1:(nrow(Vec)-1)),]
    Vec=Vec-Vec2
    names(Vec)=paste("Incremental_",names(Vec))
    Vec=cbind(DataForRegression[DataForRegression$Location==Location,],Vec)
    NewDF=rbind(NewDF,Vec)
  }
  
  DataForRegression=NewDF
  Information_to_include=c(Information_to_include,names(Vec)[grepl(x = names(Vec),"Incremental")])
  
  print("including incremental increase")
}

if(IncludeWeeklyCumulateive==T){
  
  DataForRegression$WeeklyCumulativeNewGroundTruth=0
  #Smoothing the targeted variable 
  for (Location in unique(DataForRegression$Location)){
    Vec=DataForRegression[DataForRegression$Location==Location,GroundTruthName]
    
    Vec2=Vec
    for(i in 6:length(Vec)){
      Vec[i]=sum(Vec2[(i-5):i])
    }
    DataForRegression$WeeklyCumulativeNewGroundTruth[DataForRegression$Location==Location]=Vec
    
  }
  Information_to_include=c(Information_to_include,"WeeklyCumulativeNewGroundTruth")
  print("including WeeklyCumulativeNewGroundTruth")
}


#if auto regression only include groudtruth
if (Autoregression==T){Information_to_include=GroundTruthName}

#include lags 

Vec=c(Information_to_include)
Information_to_include=paste0(Information_to_include,'_Lag_0')

if (Lags>0){
  for (Lag in LagsToInclude){
    Information_to_include=c(Information_to_include,paste0(Vec,'_Lag_',Lag)
    )
    
  }
}

#Information_to_include=c(Information_to_include,paste(MechanisticPredictionsAheadofTime,"on_predicted_date"))



All_X=NULL
All_Y=NULL
All_Results=NULL
All_Outputs=NULL
All_VariableImportance=NULL
AllPrediction_in_sample=NULL


for (Loop in 1:B_global_loops){
  print(paste("the last day of data",max(DataForRegression$Date)))  
  
  #Model is retrained at each single index day 
  Output=Run_models(DataForRegression,Lags=Lags,daysahead=daysahead,Information_to_include,
                GroundTruthName=GroundTruthName,UseR0_as_predictor=UseR0_as_predictor,
                First_index_date=First_index_date,Normalization=Normalization,
                FeatureSelection =FeatureSelection ,Aggregation=Aggregation,Binary=F,
                Clustering=Clustering,Augmentation=Augmentation,OutlierDataPointRemoval=OutlierDataPointRemoval,
                IncludeMechanisticPrediction_ahead_of_time=IncludeMechanisticPrediction_ahead_of_time,
                MechanisticPredictionsAheadofTime=MechanisticPredictionsAheadofTime,
                Mechanistic_prediction_to_use=Mechanistic_prediction_to_use,
                ClusteringMethod=ClusteringMethod)
  
  
  #prediction results
  Results=Output$Results
  #range(Results$Y_test,na.rm = T)
  X=Output$X
  Y=Output$Y
  
  
  
  
  #in sample prediction for visualization purpose 
  # library(glmnet)
  # LastModel=Output$LastModel
  # method="lambda.1se"
  # X_vec=X[,grepl(names(X),pattern = paste0(Information_to_include,collapse = "|"))]
  # Prediction_in_sample=predict(LastModel[[1]],type = "class",newx=as.matrix(X_vec),s=c(method))
  # Prediction_in_sample=data.frame(Prediction_in_sample=Prediction_in_sample,
  #                               date_T=X$Date,
  #                               location=X$Location,
  #                               Round=Loop)
  # AllPrediction_in_sample=rbind(AllPrediction_in_sample,Prediction_in_sample)
  
 
  
   #importance of variable 
  VariableImportance=Output$VaraibleImportance
  #VariableImportance=Reduce(VariableImportance,f = cbind)
  #colnames(VariableImportance)=as.character(unique(Output$Results$Dates))
  #write.csv(VariableImportance,paste0("../figures/VariableImportance",".csv"))
  #organize importance weights 
  
  #Vec=Reduce(lapply(VariableImportance,function(x){Reduce(x,f = rbind)}),f = rbind)
  All_VariableImportance[[length(All_VariableImportance)+1]]=VariableImportance
  
  
  #
  # Results$Run=Loop 
  # X$Run=Loop 
  # Y$Run=Loop 
  # #VariableImportance$Run=Loop 
  # 
   
  #the algorithm may run several loop to get rid of effects of stochasticity 
  All_Results=rbind(All_Results ,Results)
  All_Y=rbind(All_Y,Y)
  All_X=rbind(All_X,X)
  All_VariableImportance=rbind(All_VariableImportance,VariableImportance)
  #AllImportance[[length(AllImportance)+1]]=VariableImportance
  #

  
  #look at performance 
  AllOutput=NULL 
        for (Location in unique(Results$Location)){
          print(Location)
          Vec_results=Results[Results$Location==Location,]
        OutputVec=Get_Performance_each_province(Vec_results,X = X[X$Location==Location,],Aggregation = Aggregation,daysahead=daysahead,GroundTruthName,Print = T)
        AllOutput=rbind(AllOutput,OutputVec$Performance)
        }
        sum(AllOutput$RMSE-AllOutput$Baseline_RMSE<0)
        sum(AllOutput$Cor-AllOutput$Baseline_Cor>0,na.rm = T)
        sum(AllOutput$Mape-AllOutput$Baseline_Mape>0,na.rm = T)
        AllOutput$Locations[AllOutput$RMSE-AllOutput$Baseline_RMSE>0]
        
        
        #MeanImportance=Reduce(AllImportance,f = "+")/length(AllImportance)
        #apply(MeanImportance,MARGIN = 1,mean)
        
        AllOutput$Run=Loop 
        
        All_Outputs=rbind(All_Outputs,AllOutput)
        
      
        
#end of loop
  }
  
  
  
  
#save the results 
write.csv(All_Results,paste0("../figures/All_results_",ExperimentName,".csv"))
#write.csv(All_X,paste0("../figures/X_outputs",ExperimentName,".csv"))
All_Y$Location=All_X$Location
write.csv(All_Y,paste0("../figures/Y_outputs",ExperimentName,".csv"))
#write.csv(All_Outputs,file = "../figures/ModelPerformance_For_Figure.csv")
 #write.csv(AllPrediction_in_sample,paste0("../figures/AllPrediction_in_sample_",ExperimentName,".csv"))
  
save(All_VariableImportance,file = paste0("../figures/VariableImportance_",ExperimentName,".Rdata"))  
  
  
#Look at performance of mean prediction across 20 runs 
Vec=All_Results
Vec_pred=aggregate(Vec$Predictions,list(Vec$Dates,Vec$Location),mean)
Vec_sd=aggregate(Vec$Predictions,list(Vec$Dates,Vec$Location),sd)
Vec_test=aggregate(Vec$Y_test,list(Vec$Dates,Vec$Location),mean)
Vec=Vec_pred
names(Vec)= c("Dates","Location","Predictions")
Vec$Y_test=Vec_test$x
Vec$Aggregation=Aggregation
Vec$DaysAhead =daysahead
Vec$prediction_sd =Vec_sd$x

Results=Vec
Results$Location=as.character(Results$Location)

AllOutput=NULL
AllPrediction=NULL
for (Location in unique(Results$Location)){
  print(Location)
  Vec_results=Results[Results$Location==Location,]
  OutputVec=Get_Performance_each_province(Vec_results,X = X[X$Location==Location,],Aggregation = Aggregation,daysahead=daysahead,GroundTruthName,Print = T)
  AllOutput=rbind(AllOutput,OutputVec$Performance)
  AllPrediction=rbind(AllPrediction,OutputVec$Prediction)
}
print(sum((AllOutput$RMSE-AllOutput$Baseline_RMSE)<0))
NumberOfSateOutperform=c(NumberOfSateOutperform,sum((AllOutput$RMSE-AllOutput$Baseline_RMSE)<0))

sum(AllOutput$Cor-AllOutput$Baseline_Cor>0,na.rm = T)
sum(AllOutput$Mape-AllOutput$Baseline_Mape>0,na.rm = T)
AllOutput$Locations[AllOutput$RMSE-AllOutput$Baseline_RMSE>0]

sum(AllOutput$Mechanistic_RMSE-AllOutput$Baseline_RMSE<0)
sum(AllOutput$Mechanistic_Cor-AllOutput$Baseline_Cor>0,na.rm = T)
sum(AllOutput$Mechanistic_Mape-AllOutput$Baseline_Mape>0,na.rm = T)
AllOutput$Locations[AllOutput$Mechanistic_Mape-AllOutput$Baseline_RMSE>0]

sum(AllOutput$Mechanistic_RMSE-AllOutput$RMSE<0)
sum(AllOutput$Mechanistic_Cor-AllOutput$Cor>0,na.rm = T)
sum(AllOutput$Mechanistic_Mape-AllOutput$Mape>0,na.rm = T)
AllOutput$Locations[AllOutput$Mechanistic_Mape-AllOutput$RMSE>0]



}
 
