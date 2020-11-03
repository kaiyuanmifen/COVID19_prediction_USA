#Load functions/packages needed 
library(ggplot2)
library(reshape)
library(translateR)
library("showtext")
library(dplyr)
source("Functions_NCOV.R")

#settings 


Aggregation=2

All_daysahead=c(1,3,5,7)

All_Results=NULL

#JHK_New_confirmed_
TargetedFeature="covidtracking_new_deaths_smoothed_MechanisticGuided"
AutoRegressionBaselineName="covidtracking_new_deaths_smoothed_autoregression"
MechanisticPredictionName="cdc_ili.idea"
ClusteringMethod="StartingDate"
#"GroundTruthCorrelation" "ByRegion" "ByParty" "StartingDate"


  #"gleammodel_infectious_mean_merged"

DaysAheadTovisualize=5#some plot only visualize one time horrizone 

Smoothing=T
GroundTruthName="covidtracking_new_deaths"
#"JHK_New_confirmed" "covidtracking_new_deaths"
CuttoffDate=as.Date("2020-07-01")

#load data 
AllProvinces=read.csv("../results/OrganizedData_US_countries.csv")
AllProvinces=as.data.frame(AllProvinces)
head(AllProvinces)
AllProvinces$Location=as.character(AllProvinces$Location)
AllProvinces$Date=as.Date(as.character(AllProvinces$Date),origin = "1970-01-01")
names(AllProvinces)

DataForRegression=AllProvinces
if(Smoothing==T){
  #Smoothing the targeted variable 
  for (Location in unique(DataForRegression$Location)){
    Vec=DataForRegression[DataForRegression$Location==Location,GroundTruthName]
    Vec2=Vec
    for(i in 6:length(Vec)){
      Vec[i]=mean(Vec2[(i-5):i])
    }
    DataForRegression[DataForRegression$Location==Location,GroundTruthName]=Vec
    
  }
  
  
}

#Also smooth the mechanistic prediction for comparison purpose 

if(Smoothing==T){
  #Smoothing the mechanistic prediction
  for (Location in unique(DataForRegression$Location)){
    Vec=DataForRegression[DataForRegression$Location==Location,MechanisticPredictionName]
    Vec2=Vec
    for(i in 6:length(Vec)){
      Vec[i]=mean(Vec2[(i-5):i])
    }
    DataForRegression[DataForRegression$Location==Location,MechanisticPredictionName]=Vec
    
  }
  
  
}

AllProvinces=DataForRegression



#Index day based visualization


for (daysahead in All_daysahead){
  ExperimentName=paste0(TargetedFeature,"_Agg_",Aggregation,"_ahead_",daysahead,"_",ClusteringMethod)
  
  
  #predictions
  result=read.csv(paste0("../figures/All_results_",ExperimentName,".csv"))
  result$Dates=as.Date(as.character(result$Dates))
  result$IndexDate=result$Dates
  result$PredictionDate=result$IndexDate+result$DaysAhead
  
  All_Results=rbind(All_Results,result)
  
  #Y 
  Y=read.csv(paste0("../figures/Y_outputs",ExperimentName,".csv"))
  Y$Date=as.Date(as.character(Y$Date))
  Y$Date=Y$Date+Y$daysahead#Prediction date
  Y$PredictionDate=Y$Date
  
}






#visualization
pdf(file=paste("../Visualizations/",TargetedFeature,"_",ClusteringMethod,"_visualization.pdf"))
par(mfrow = c(4, 3))
for (Location in as.character(unique(All_Results$Location))){
  
  
  for (index_date in unique(All_Results$IndexDate[All_Results$Location==Location])){
    index_date=as.Date(index_date ,origin = "1970-01-01")
    
    VecPlot=Y[(Y$Location==Location)&(Y$Date<=index_date),]
    names(VecPlot)[2]="Count"
    VecPlot=VecPlot[,c("Count","Date","Location"  )]
    VecPlot$Type="GroundTruth"
    VecPlot=VecPlot[VecPlot$Date<=index_date+max(All_daysahead)*Aggregation,]
    
    
    Predicted=All_Results[(All_Results$IndexDate==index_date)&(All_Results$Location==Location),]
    Predicted=Predicted[,c("Predictions","PredictionDate","Location")]
    names(Predicted)=c("Count","Date","Location"  )
    Predicted$Type="Predicted"
    
    
    Baseline=NULL
    Vec=VecPlot[(VecPlot$Location==Location)&(VecPlot$Date<=index_date)&(VecPlot$Date>=index_date-7),]
    Vec$Count=mean(Vec$Count)
    Vec$Date=max(All_daysahead)*Aggregation+Vec$Date
    Baseline=rbind(Baseline,Vec)
    Baseline$Type="Baseline"
    
    #VecPlot=rbind(VecPlot,Predicted,Baseline)
    
   plot(VecPlot$Count~VecPlot$Date,
        xlim=c(as.Date("2020-06-20"),as.Date("2020-08-01")),
        ylim=c(0,max(max(VecPlot$Count),max(Predicted$Count,na.rm = T))),
        type="l",lwd=2,
        xlab="Date",ylab=TargetedFeature,
        main=paste(Location,index_date))
   
   Predicted=rbind(VecPlot[nrow(VecPlot),], Predicted)#give one more point for visualization
   lines(Predicted$Count~Predicted$Date,col="brown",lwd=2)
   abline(v =index_date,col="grey",lty=2,lwd=2)
  }
  
 
  #par(mfrow = c(1, 1))
 
}
dev.off() 
par(mfrow = c(1, 1))







#Compare prediction vs baseline 

#visualization
# 
# AllResults=NULL
# 
# for (index_date in unique(All_Results$IndexDate)){
# 
#   
# Predicted=All_Results[All_Results$IndexDate==index_date,]
# Predicted=Predicted[,c("Predictions","Dates","Location")]
# names(Predicted)=c("Count","Date","Location"  )
# Predicted$Type="Predicted"
#   
#   
#   
# VecPlot=Y
# names(VecPlot)[2]="Count"
# VecPlot=VecPlot[,c("Count","Date","Location"  )]
# VecPlot$Type="GroundTruth"
# VecPlot=VecPlot[VecPlot$Date<=index_date+max(All_daysahead)*Aggregation,]
# 
# 
# 
# Baseline=NULL
# for (Location in unique(VecPlot$Location)){
#   Vec=VecPlot[(VecPlot$Location==Location)&(VecPlot$Date<=index_date)&(VecPlot$Date>=index_date-7),]
#   Vec$Count=mean(Vec$Count)
#   Vec$Date=max(All_daysahead)*Aggregation+Vec$Date
#   Baseline=rbind(Baseline,Vec)
#   
# }
# 
# Baseline$Type="Baseline"
# 
# 
# GroundTruthName=Y[match(Predicted$Date,Y$Date),]
# 
# Result=Predicted[,c("Count","Date","Location")]
# names(Result)=c("Predicted","Date","Location")
# Result$Baseline=Baseline$Count[match(interaction(Result[,c("Date","Location")]),interaction(Baseline[,c("Date","Location")]))]
# Result$GroundTruth=Y$Y[match(interaction(Result[,c("Date","Location")]),interaction(Y[,c("Date","Location")]))]
# 
# Result$IndexDate=index_date
# 
# AllResults=rbind(AllResults,Result)
# }
# 
# 
# RMSE_model_all=NULL
# RMSE_baseline_all=NULL
# Location_All=NULL
# for (index_date in unique(AllResults$IndexDate)){
#   for (Location in unique(AllResults$Location)){
#    Vec=AllResults[(AllResults$IndexDate==index_date)&(AllResults$Location==Location),]
#    
#    RMSE_model=sqrt(mean((Vec$Predicted-Vec$GroundTruth)^2,na.rm=T))
#    RMSE_model_all=c(RMSE_model_all,RMSE_model)
#    
#    RMSE_baseline=sqrt(mean((Vec$Baseline-Vec$GroundTruth)^2,na.rm=T))
#    RMSE_baseline_all=c(RMSE_baseline_all,RMSE_baseline)
#    
#    Location_All=c(Location_All,Location)
# 
#   
#   }
# }
# 
# AllPerformance=data.frame(RMSE_model=RMSE_model_all,
#                           RMSE_baseline=RMSE_baseline_all,
#                           Location=Location_All)
# 
# AllPerformance=AllPerformance[(!is.na(AllPerformance$RMSE_model))&(!is.na(AllPerformance$RMSE_baseline)),]
# boxplot(AllPerformance)
# sum(AllPerformance$RMSE_model<AllPerformance$RMSE_baseline)
# 
# 
# 






#Prediction based visualization


#Aggregation=2

#TargetedFeature="deathIncrease"
daysahead_all=All_daysahead
InitialData=as.Date("2020-07-01")

pdf(file=paste("../Visualizations/",TargetedFeature,"_",ClusteringMethod,"_visualization_all_gap.pdf"),width = 10,height = 10)
par(mfrow = c(2, 2))
for (Location in as.character(unique(All_Results$Location))){
  
  for (daysahead in daysahead_all){
    # this should be multiplied by aggregation
    ExperimentName=paste0(TargetedFeature,"_Agg_",Aggregation,"_ahead_",daysahead,"_",ClusteringMethod)
    
    #predictions
    result=read.csv(paste0("../figures/All_results_",ExperimentName,".csv"))
    result$Dates=as.Date(as.character(result$Dates))
    result$IndexDate=result$Dates
    result$PredictionDate=result$IndexDate+result$DaysAhead
    
    All_Results=rbind(All_Results,result)
    
    #Y 
    Y=read.csv(paste0("../figures/Y_outputs",ExperimentName,".csv"))
    Y$Date=as.Date(as.character(Y$Date))
    Y$Date=Y$Date+Y$daysahead#Prediction date
    Y$PredictionDate=Y$Date
    
    
    
    #visualization
    #First_index_date=as.Date("2020-05-1",origin = "1970-01-01")
    
    VecPlot=Y[Y$Location==Location,]
    names(VecPlot)[2]="Count"
    VecPlot=VecPlot[,c("Count","Date","Location"  )]
    VecPlot$Type="GroundTruth"
    #VecPlot=VecPlot[VecPlot$Date<=index_date+max(All_daysahead)*Aggregation,]
    
    
    Predicted=result[result$Location==Location,]
    Predicted=Predicted[,c("Predictions","PredictionDate","Location")]
    names(Predicted)=c("Count","Date","Location"  )
    Predicted$Type="Predicted"
    
    
    
    #persistence baseline
    Baseline=NULL
    
    
    for (Location in unique(VecPlot$Location)){
      Vec=Predicted[(Predicted$Location==Location),]
      Vec$Count=Y$Y[match(interaction(Vec$Location,Vec$Date-Aggregation*daysahead),interaction(Y$Location,Y$Date))]
      Baseline=rbind(Baseline,Vec)
      
    }
    
    Baseline$Type="Baseline"
    
    #make sure on the same time horrizon
    
    Predicted=Predicted[Predicted$Date>=CuttoffDate,]
    Baseline=Baseline[Baseline$Date>=min(Predicted$Date),]
    
    
    #mean of one week before as another baseline
    
    One_week_before_mean=NULL
    
    
    for (Location in unique(VecPlot$Location)){
      Vec=Predicted[(Predicted$Location==Location),]
      for (Date in as.Date(unique(Vec$Date))){
        Vec=Predicted[(Predicted$Location==Location)&(Predicted$Date==Date),]
        Date=as.Date(Date,origin = '1970-01-01')
        Vec$Count=mean(Y$Y[match(interaction(Vec$Location,Date-Aggregation*daysahead-c(0:7)),interaction(Y$Location,Y$Date))],na.rm = T)
        One_week_before_mean=rbind(One_week_before_mean,Vec)
      }
     
      
    }
    
    One_week_before_mean$Type="Baseline_one_week_before"
    
    
    
    #Autoregression
    Autoregression=read.csv(paste0("../figures/All_results_",paste0(AutoRegressionBaselineName,"_Agg_",Aggregation,"_ahead_",daysahead,"_",ClusteringMethod),".csv"))
    Autoregression$Dates=as.Date(as.character(Autoregression$Dates))
    Autoregression$IndexDate=Autoregression$Dates
    Autoregression$PredictionDate=Autoregression$IndexDate+result$DaysAhead
    
    Predicted_Autoregression=Autoregression[Autoregression$Location==Location,]
    Predicted_Autoregression=Predicted_Autoregression[,c("Predictions","PredictionDate","Location")]
    names(Predicted_Autoregression)=c("Count","Date","Location"  )
    Predicted_Autoregression$Type="autoregression"
    
    
    #mechanistic prediction
    
    
    
    Predicted_mechanistic=data.frame(Count=AllProvinces[AllProvinces$Location==Location,MechanisticPredictionName],
                                        Date=AllProvinces$Date[AllProvinces$Location==Location],
                                        Location=Location)
    Predicted_mechanistic$Type="mechanistic"
    Predicted_mechanistic=Predicted_mechanistic[Predicted_mechanistic$Date>=InitialData,]
    
    
    Predicted_Autoregression=  Predicted_Autoregression[ Predicted_Autoregression$Date>=min(Predicted$Date),]
    One_week_before_mean=One_week_before_mean[One_week_before_mean$Date>=min(Predicted$Date),]
    Predicted_mechanistic=Predicted_mechanistic[Predicted_mechanistic$Date>=min(Predicted$Date),]
    
    
    #plot
    plot(VecPlot$Count~VecPlot$Date,
         xlim=c(as.Date("2020-06-30"),as.Date("2020-08-01")),
         ylim=c(0,max(max(VecPlot$Count,na.rm = T),max(Predicted$Count,na.rm = T))),
         type="l",lwd=3,
         xlab="Date",ylab=TargetedFeature,
         main=paste(Location,daysahead*Aggregation,"Days ahead of time"))
    
    
    
    #keep all the plot the same starting date
   
    Predicted=Predicted[Predicted$Date>=InitialData,]
    Baseline=Baseline[Baseline$Date>=InitialData,]
    lines(Predicted$Count~Predicted$Date,col="red",lwd=2)
    lines(Baseline$Count~Baseline$Date,col="orange",lwd=2,lty=2)
    lines(Predicted_Autoregression$Count~Predicted_Autoregression$Date,col="blue",lwd=2,lty=2)
    lines(Predicted_mechanistic$Count~Predicted_mechanistic$Date,col="brown",lwd=1,lty=2)
    lines(One_week_before_mean$Count~ One_week_before_mean$Date,col="green",lwd=1,lty=2)
    
    
    abline(v =min(InitialData),col="grey",lty=2,lwd=2)
    legend("topleft", lty=c(1,1,2,2,2,2),
           col = c("black","red","orange","blue",'brown','green'), 
           legend = c("Observed","ARGO","Persistence",'Autoregression',"mechanistic","WeekMean"))
    
 
  
  
  }
}
dev.off()
par(mfrow = c(1, 1))







#Performance of each time horizon 


#Aggregation=2

#TargetedFeature="deathIncrease"
#daysahead_all=c(1,2,3,4,5)
daysahead_all=All_daysahead
All_allMetric=list()
AllCompareRMSE=NULL
AllCompareCOR=NULL

#pdf(file=paste("../figures/",TargetedFeature,"_visualization_performance.pdf"))
par(mfrow = c(2, 2))


  for (daysahead in daysahead_all){
    # this should be multiplied by aggregation
    ExperimentName=paste0(TargetedFeature,"_Agg_",Aggregation,"_ahead_",daysahead,"_",ClusteringMethod)

    #predictions
    result=read.csv(paste0("../figures/All_results_",ExperimentName,".csv"))
    result$Dates=as.Date(as.character(result$Dates))
    result$IndexDate=result$Dates
    result$PredictionDate=result$IndexDate+result$DaysAhead

    All_Results=rbind(All_Results,result)

    #Y
    Y=read.csv(paste0("../figures/Y_outputs",ExperimentName,".csv"))
    Y$Date=as.Date(as.character(Y$Date))
    Y$Date=Y$Date+Y$daysahead#Prediction date
    Y$PredictionDate=Y$Date
  
    
  

    #visualization
    #First_index_date=as.Date("2020-05-1",origin = "1970-01-01")

    VecPlot=Y
    names(VecPlot)[2]="Count"
    VecPlot=VecPlot[,c("Count","Date","Location"  )]
    VecPlot$Type="GroundTruth"
    #VecPlot=VecPlot[VecPlot$Date<=index_date+max(All_daysahead)*Aggregation,]


    Predicted=result
    Predicted=Predicted[,c("Predictions","PredictionDate","Location")]
    names(Predicted)=c("Count","Date","Location"  )
    Predicted$Type="Predicted"


    Baseline=NULL


    for (Location in unique(VecPlot$Location)){
      Vec=Predicted
      Vec$Count=Y$Y[match(interaction(Vec$Location,Vec$Date-Aggregation*daysahead),interaction(Y$Location,Y$Date))]
      Baseline=rbind(Baseline,Vec)

    }

    Baseline$Type="Baseline"

    #make initial predictio date the same 
    #Y=Y[Y$PredictionDate>="2020-05-10",]
    #All_Results=All_Results[All_Results$Predictions>="2020-05-10",]
    #result=result[result$PredictionDate>="2020-05-10",]
    
    
    #make sure on the same time horrizon
    
    Predicted=Predicted[Predicted$Date>=CuttoffDate,]
    Baseline=Baseline[Baseline$Date>=min(Predicted$Date),]
    
    
    
    
    #mean of one week before as another baseline
    
    One_week_before_mean=NULL
    
    
    for (Location in unique(VecPlot$Location)){
      Vec=Predicted[(Predicted$Location==Location),]
      for (Date in as.Date(unique(Vec$Date))){
        Vec=Predicted[(Predicted$Location==Location)&(Predicted$Date==Date),]
        Date=as.Date(Date,origin = '1970-01-01')
        Vec$Count=mean(Y$Y[match(interaction(Vec$Location,Date-Aggregation*daysahead-c(0:7)),interaction(Y$Location,Y$Date))],na.rm = T)
        One_week_before_mean=rbind(One_week_before_mean,Vec)
      }
      
      
    }
    
    One_week_before_mean$Type="Baseline_one_week_before"
    
    
    
    
    
    #Autoregression
    Autoregression=read.csv(paste0("../figures/All_results_",paste0(AutoRegressionBaselineName,"_Agg_",Aggregation,"_ahead_",daysahead,"_",ClusteringMethod),".csv"))
    Autoregression$Dates=as.Date(as.character(Autoregression$Dates))
    Autoregression$IndexDate=Autoregression$Dates
    Autoregression$PredictionDate=Autoregression$IndexDate+result$DaysAhead
    
    Predicted_Autoregression=Autoregression
    Predicted_Autoregression=Predicted_Autoregression[,c("Predictions","PredictionDate","Location")]
    names(Predicted_Autoregression)=c("Count","Date","Location"  )
    Predicted_Autoregression$Type="autoregression"
    
    
    #mechanistic prediction
    Predicted_mechanistic=data.frame(Count=AllProvinces[,MechanisticPredictionName],
                                     Date=AllProvinces$Date,
                                     Location=AllProvinces$Location)
    Predicted_mechanistic$Type="mechanistic"
    
    
    Predicted_Autoregression= Predicted_Autoregression[Predicted_Autoregression$Date>=min(Predicted$Date),]
    One_week_before_mean=One_week_before_mean[One_week_before_mean$Date>=min(Predicted$Date),]
    Predicted_mechanistic=Predicted_mechanistic[Predicted_mechanistic$Date>=min(Predicted$Date),]
    
    
    #performance

    VecPlot=rbind(VecPlot,Predicted,Baseline)

    Performance=Predicted
    names(Performance)=c("Predicted","Date" ,"Location","Type")
    Performance$Groundtruth=Y$Y[match(interaction(Performance$Location,Performance$Date),interaction(Y$Location,Y$Date))]
    Performance$Baseline=Baseline$Count[match(interaction(Performance$Location,Performance$Date),interaction(Baseline$Location,Baseline$Date))]
    Performance$autoregression=Predicted_Autoregression$Count[match(interaction(Performance$Location,Performance$Date),interaction(Predicted_Autoregression$Location,Predicted_Autoregression$Date))]
    Performance$mechanistic=Predicted_mechanistic$Count[match(interaction(Performance$Location,Performance$Date),interaction(Predicted_mechanistic$Location,Predicted_mechanistic$Date))]
    Performance$One_week_before_mean=Predicted_mechanistic$Count[match(interaction(Performance$Location,Performance$Date),interaction(One_week_before_mean$Location,One_week_before_mean$Date))]
    
    
    Performance=Performance[(!is.na(Performance$Baseline))&((!is.na(Performance$Groundtruth))),]


    RMSE_model_all=NULL
    RMSE_baseline_all=NULL
    RMSE_autogr_all=NULL
    RMSE_mechanistic_all=NULL
    RMSE_weekmean_all=NULL
    
    
    Cor_model_all=NULL
    Cor_baseline_all=NULL
    Cor_autogr_all=NULL
    Cor_mechanistic_all=NULL
    Cor_weekmean_all=NULL
    
    Ratio_model_all=NULL
    Ratio_autogr_all=NULL
    Ratio_mechanistic_all=NULL
    Ratio_weekmean_all=NULL
    
    for (Location in unique(Performance$Location)){
      Vec=Performance[Performance$Location==Location,]
      
      RMSE_model=sqrt(mean((Vec$Predicted-Vec$Groundtruth)^2))
      RMSE_baseline=sqrt(mean((Vec$Baseline-Vec$Groundtruth)^2))
      RMSE_autogr=sqrt(mean((Vec$autoregression-Vec$Groundtruth)^2))
      RMSE_mechanistic=sqrt(mean((Vec$mechanistic-Vec$Groundtruth)^2))
      RMSE_weekmean=sqrt(mean((Vec$One_week_before_mean-Vec$Groundtruth)^2))
      
      Cor_model=cor(Vec$Predicted,Vec$Groundtruth)
      Cor_baseline=cor(Vec$Baseline,Vec$Groundtruth)
      Cor_autogr=cor(Vec$autoregression,Vec$Groundtruth)
      Cor_mechanistic=cor(Vec$mechanistic,Vec$Groundtruth)
      Cor_weekmean=cor(Vec$One_week_before_mean,Vec$Groundtruth)
      
      
      
      Ratio_model=RMSE_model/RMSE_baseline
      Ratio_autogr=RMSE_autogr/RMSE_baseline
      Ratio_mechanistic=RMSE_mechanistic/RMSE_baseline
      Ratio_weekmean=RMSE_weekmean/RMSE_baseline
      
      
      #organize for table
      RMSE_model_all=c(RMSE_model_all,RMSE_model)
      RMSE_baseline_all=c(RMSE_baseline_all,RMSE_baseline)
      RMSE_autogr_all=c(RMSE_autogr_all,RMSE_autogr)
      RMSE_mechanistic_all=c(RMSE_mechanistic_all,RMSE_mechanistic)
      RMSE_weekmean_all=c(RMSE_weekmean_all,RMSE_weekmean)
      
      
      Cor_model_all=c(Cor_model_all,Cor_model)
      Cor_baseline_all=c(Cor_baseline_all,Cor_baseline)
      Cor_autogr_all=c(Cor_autogr_all,Cor_autogr)
      Cor_mechanistic_all=c(Cor_mechanistic_all,Cor_mechanistic)
      Cor_weekmean_all=c(Cor_weekmean_all,Cor_weekmean)
      
      
      Ratio_model_all=c(Ratio_model_all,Ratio_model)
      Ratio_autogr_all=c(Ratio_autogr_all,Ratio_autogr)
      Ratio_mechanistic_all=c(Ratio_mechanistic_all,Ratio_mechanistic)
      Ratio_weekmean_all=c( Ratio_weekmean_all,Ratio_weekmean)
    }

    AllMetrics=data.frame(RMSE_model=RMSE_model_all,
                          RMSE_baseline=RMSE_baseline_all,
                          RMSE_autogr=RMSE_autogr_all,
                          RMSE_mechanistic=RMSE_mechanistic_all,
                          RMSE_weekmean= RMSE_weekmean_all,
                          Cor_model=Cor_model_all,
                          Cor_baseline=Cor_baseline_all,
                          Cor_autogr=Cor_autogr_all,
                          Cor_mechanistic= Cor_mechanistic_all,
                          Cor_weekmean=Cor_weekmean_all,
                          Ratio_model=Ratio_model_all,
                          Ratio_autogr=Ratio_autogr_all,
                          Ratio_mechanistic=Ratio_mechanistic_all,
                          Ratio_weekmean=Ratio_weekmean_all)
    AllMetrics$Location=unique(Performance$Location)


    AllMetrics=AllMetrics[order(AllMetrics$Ratio_model,decreasing = F),]
    AllMetrics$Daysahead=daysahead
    
    All_allMetric[[length(All_allMetric)+1]]=AllMetrics

    AllMetrics$Location=factor(AllMetrics$Location,levels = AllMetrics$Location)

    library(ggplot2)
    #A=ggplot(data = AllMetrics,aes(x=Location, y=Ratio))+
     # geom_bar(position = "dodge", stat="identity") + scale_y_log10() + coord_flip()+
      #ggtitle("Model RMSE/ baseline RMSE")


    #ggsave(A,file=paste0("../figures/",TargetedFeature,"_",daysahead,"_Prediction_based_visualization_bar.pdf"),
     #      device="pdf",dpi = 300,width = 40.1,height = 30.1)

    #print(sum(AllMetrics$RMSE_model<AllMetrics$RMSE_autogr))
    print(sum(AllMetrics$RMSE_model<AllMetrics$RMSE_baseline,na.rm = T))
    
    AllCompareRMSE=c(AllCompareRMSE,sum(AllMetrics$RMSE_model<AllMetrics$RMSE_baseline,na.rm = T))
    #AllCompareCOR=c(AllCompareCOR,sum(AllMetrics$<AllMetrics$RMSE_baseline))


  }


AllCompareRMSE=data.frame(Count=AllCompareRMSE,Daysahead=daysahead_all*Aggregation)

write.csv(AllCompareRMSE,file = paste0("../Visualizations/",TargetedFeature,'_',ClusteringMethod,"_different_time_horrizon_performance.csv"))


#show a table indicate how many states which method performance best about in each time horizone 
WhichMmethdMinRMSE=NULL

WhichMmethdMaxCor=NULL

for (i in 1:length(All_allMetric)){
  Vec=All_allMetric[[i]]
  Vec=Vec[,c("RMSE_model", "RMSE_baseline", "RMSE_autogr", "RMSE_mechanistic", "RMSE_weekmean")]
  Vec2=rep(0,length(Vec))
  Vec=apply(Vec,MARGIN = 1,FUN = which.min)
  Vec=table(Vec)
  Vec2[as.integer(names(Vec))]=Vec
  WhichMmethdMinRMSE=rbind(WhichMmethdMinRMSE,Vec2)
  
  Vec=All_allMetric[[i]]
  Vec=Vec[,c("Cor_model", "Cor_baseline", "Cor_autogr", "Cor_mechanistic", "Cor_weekmean")]
  Vec2=rep(0,length(Vec))
  Vec[is.na(Vec)]=-100#remove NA
  Vec=apply(Vec,MARGIN = 1,FUN = which.max)
  Vec=table(Vec)
  Vec2[as.integer(names(Vec))]=Vec
  WhichMmethdMaxCor=rbind(WhichMmethdMaxCor,Vec2)
  
}
WhichMmethdMinRMSE=as.data.frame(WhichMmethdMinRMSE)
names(WhichMmethdMinRMSE)=c("RMSE_model", "RMSE_baseline", "RMSE_autogr", "RMSE_mechanistic", "RMSE_weekmean")
rownames(WhichMmethdMinRMSE)=NULL
WhichMmethdMinRMSE$Daysahead=daysahead_all*Aggregation

WhichMmethdMaxCor=as.data.frame(WhichMmethdMaxCor)
names(WhichMmethdMaxCor)=c("Cor_model", "Cor_baseline", "Cor_autogr", "Cor_mechanistic", "Cor_weekmean")
rownames(WhichMmethdMaxCor)=NULL
WhichMmethdMaxCor$Daysahead=daysahead_all*Aggregation

write.csv(WhichMmethdMinRMSE,file =paste("../Visualizations/",TargetedFeature,"_",ClusteringMethod,"_all_horizion_all_method_RMSE.csv") )

write.csv(WhichMmethdMaxCor,file =paste("../Visualizations/",TargetedFeature,"_",ClusteringMethod,"_all_horizion_all_method_COR.csv") )





#Visualize performance by US state map 

#US map performance 

library(usmap)
library(ggplot2)
library(viridis)

VecPlot= All_allMetric[[2]]
statepop=as.data.frame(statepop)
statepop$location=tolower(statepop$full)
statepop$location=paste0("US-",statepop$location)

VecPlot$ARGO.RMSE_Over_Persistence.RMSE=VecPlot$RMSE_model/VecPlot$RMSE_baseline
VecPlot$state=statepop$full[match(VecPlot$Location,statepop$location)]

VecPlot$ARGO.RMSE_Over_Persistence.RMSE[VecPlot$ARGO.RMSE_Over_Persistence.RMSE>2]=2#for visualization purpose

plot_usmap(data = VecPlot, values = "ARGO.RMSE_Over_Persistence.RMSE", color = "white") + 
  scale_fill_viridis(na.value = "grey", name = "ARGO.RMSE/Persistence.RMSE",breaks=c(0,1,2),
                     label = scales::comma,rescaler = function(x, to = c(0, 1), from = NULL) {
                       ifelse(x<1, 
                              scales::rescale(x,
                                              to = to,
                                              from = c(min(x, na.rm = TRUE), 1)),
                              1)},limits=c(0,2))+
  theme(legend.position = "right")



ggsave(paste0("../Visualizations/",ExperimentName,"_Performance_US_map ",".pdf"),device = "pdf",scale = 2)




#Barplot 
dev.new()
VecPlot= All_allMetric[[2]]
statepop=as.data.frame(statepop)
statepop$location=tolower(statepop$full)
statepop$location=paste0("US-",statepop$location)

VecPlot$state=statepop$full[match(VecPlot$Location,statepop$location)]
VecPlot2=NULL
for (Target in c("Ratio_model", "Ratio_autogr", "Ratio_mechanistic")){
  VecPlot2=rbind(VecPlot2,data.frame(state=VecPlot$state,Ratio=VecPlot[,Target],Type=Target))
}
VecPlot2=as.data.frame(VecPlot2)



VecPlot2$Type= factor(VecPlot2$Type, levels = c("Ratio_model" ,"Ratio_autogr" ,"Ratio_mechanistic"  ))

#column for ordering in plot
Vec=VecPlot2[VecPlot2$Type=="Ratio_model",]
VecPlot2$OrderColumn=Vec$Ratio[match(VecPlot2$state,Vec$state)]
  
ggplot(data=VecPlot2, aes(x=reorder(state,OrderColumn), y=(1/Ratio)-1,fill=Type)) +
  geom_bar(position=position_dodge(width=0.7), stat="identity") +ylab("(Baseline_RMSE / RMSE)-1")+
  xlab("")+
  theme(panel.background = element_blank(),legend.position="right",axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  geom_hline(yintercept=0.5, linetype="dashed", color = "grey", size=1)+
  geom_hline(yintercept=-0.5, linetype="dashed", color = "grey", size=1)+
  geom_hline(yintercept=0, linetype="dashed",  color = "grey", size=1)+
  scale_y_continuous(breaks = seq(-1, 1, len = 5),limits = c(-1,2))

# 
# ggplot(data=VecPlot2, aes(x=reorder(state,Ratio), y=log10(Ratio),fill=Type)) +
#   geom_bar(position="dodge", stat="identity") +ylab("log10(10*RMSE/ARGO_RMSE)")+
#   xlab("")+
#   theme(panel.background = element_blank(),legend.position="right",axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
#   ylim(-max(log10(abs(VecPlot2$Ratio))), max(log10(abs(VecPlot2$Ratio))))
# 


ggsave(paste0("../Visualizations/",ExperimentName,"_Performance_bar ",".pdf"),device = "pdf",height = 7,width = 14)



#boxplot of each method over each time horrizon 



VecPlot=Reduce(All_allMetric,f = rbind)
library(ggplot2)




#Rearrange data for plotting 
names(VecPlot)

VecAll=NULL
for (ColVec in c("RMSE_model","RMSE_baseline" ,"RMSE_autogr" ,"RMSE_mechanistic","RMSE_weekmean")){
  Vec=VecPlot[,c(ColVec,"Location","Daysahead")]
  names(Vec)=c("Value","Location","Daysahead")
  Vec$Method=strsplit(ColVec,split = "[_]")[[1]][2]
  VecAll=rbind(VecAll,Vec)
}
VecPlot=VecAll
#VecPlot$Daysahead=as.factor(VecPlot$Daysahead)

library(car)
unique(VecPlot$Method)


VecPlot$Method=factor(VecPlot$Method,levels=c("model","baseline" ,"autogr" ,"mechanistic" ,"weekmean" ))


AllPlots=list()
for (Location in unique(VecPlot$Location)){
  Vec=VecPlot[VecPlot$Location==Location,]
  
  AllPlots[[length(AllPlots)+1]]=ggplot(Vec, aes(x=Daysahead, y=log(Value), color=Method,shape=Method)) +
    geom_point(size=2)+
    theme(panel.background = element_blank(),legend.position="right",panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                                                                                      colour = "grey"), 
          panel.grid.minor = element_line(size = 0.2, linetype = 'solid',
                                          colour = "grey"))+
    labs(title=Location,
         x ="Daysaheahd of time", y = "log(RMSE)")

  
}

library(gridExtra)

pdf(paste("../Visualizations/",TargetedFeature,"_",ClusteringMethod,"_All_province_RMSE.pdf"), onefile = TRUE)
for (i in seq(length(AllPlots))) {
  print(AllPlots[[i]])  
}
dev.off()


dev.new()

# 
# #heatmap of correlation and clustering 
# pdf(file=paste("../Visualizations/",TargetedFeature,"_cor_mat.pdf"),width = 12,height = 12)
# par(mfrow = c(1, 1))
# 
# library(corrplot)
# 
# daysahead=1
# ExperimentName=paste0(TargetedFeature,"_Agg_",Aggregation,"_ahead_",daysahead)
# result=read.csv(paste0("../figures/All_results_",ExperimentName,".csv"))
# 
# MatCor=NULL
# 
# for (Location in unique(result$Location)){
#   MatCor=cbind(MatCor,result$Y_test[(result$Location==Location)&((result$Dates>="2020-03-01"))])
#   print(Location)
#   print(length(result$Y_test[(result$Location==Location)&((result$Dates>="2020-03-01"))]))
#  
# }
# MatCor=data.frame(MatCor)
# names(MatCor)=unique(result$Location)
# MatCor=MatCor[apply(MatCor,MARGIN = 1,function(x){sum(is.na(x))})==0,]
# 
# CorMat=cor(MatCor)
# par(mfrow=c(1,1))
# VecCor=corrplot(CorMat,order = "hclust",hclust.method = c("average"),outline = T,addrect = T)
# str(VecCor)
# 
# dev.off()


#Visualize clustering by US state map 
# FirstDate_use_data=as.Date("2020-03-01",origin = "1970-01-01")#data usage
# First_index_date=as.Date("2020-04-20",origin = "1970-01-01")#first date to make prediction 
# clusterCut=HierachiClustering(AllProvinces,GroundTruthName = GroundTruthName)
# 
# 
# library(usmap)
# library(ggplot2)
# names(clusterCut)=gsub(".*-","",names(clusterCut))
# VecPlot=statepop
# VecPlot$Cluster=clusterCut[match(tolower(VecPlot$full),names(clusterCut))]
# VecPlot$Cluster=as.factor(VecPlot$Cluster)
# plot_usmap(data = VecPlot, values = "Cluster", color = "white")+
#   theme(legend.position = "None")
# 
# ggsave(paste0("../Visualizations/",ExperimentName,"_cluster_map ",".pdf"),device = "pdf",scale = 2)

