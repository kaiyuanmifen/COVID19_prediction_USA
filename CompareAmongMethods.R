#Load functions/packages needed 
library(ggplot2)
library(reshape)
library(translateR)
library("showtext")
library(dplyr)
source("Functions_NCOV.R")

#settings 


Aggregation=7

All_daysahead=c(1,2)
InitialData=as.Date("2020-08-01")
All_Results=NULL

#JHK_New_confirmed_
TargetedFeature="covidtracking_new_deaths_unsmoothed_MechanisticGuided"
AutoRegressionBaselineName="covidtracking_new_deaths_unsmoothed_autoregression"
MechanisticPredictionName="cdc_ili.idea"
ClusteringMethod="StartingDate"

DaysAheadTovisualize=1#some plot only visualize one time horrizone 

Smoothing=F
GroundTruthName="covidtracking_new_deaths"
#"JHK_New_confirmed" "covidtracking_new_deaths"
CuttoffDate=as.Date("2020-08-01")

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



#Load data from forecast hub 





#compare RMSE and Cor 
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
    
    #data from COIVDhub 
    X=load("../figures/COVID_hub_preds.Rdata")
    COVIDHub=get(X)
    
    Vec=COVIDHub$COVID_hub_baseline
    Performance$COVID_hub_baseline=Vec$value[match(interaction(tolower(Performance$Location),Performance$Date),interaction(tolower(Vec$Location),Vec$target_end_date))]
    Vec=COVIDHub$COVID_hub_ensemble
    Performance$COVID_hub_ensemble=Vec$value[match(interaction(tolower(Performance$Location),Performance$Date),interaction(tolower(Vec$Location),Vec$target_end_date))]
    Vec=COVIDHub$CMU_prediction
    Performance$CMU_prediction=Vec$value[match(interaction(tolower(Performance$Location),Performance$Date),interaction(tolower(Vec$Location),Vec$target_end_date))]
    Vec=COVIDHub$YYG_prediction
    Performance$YYG_prediction=Vec$value[match(interaction(tolower(Performance$Location),Performance$Date),interaction(tolower(Vec$Location),Vec$target_end_date))]
    Vec=COVIDHub$JHU_prediction
    Performance$JHU_prediction=Vec$value[match(interaction(tolower(Performance$Location),Performance$Date),interaction(tolower(Vec$Location),Vec$target_end_date))]
    
    names(Performance)[1]="AugmentedARGO"
    
    
    
    
    Performance=Performance[(!is.na(Performance$Baseline))&((!is.na(Performance$Groundtruth))),]
    Performance[is.na(Performance)]=0
    
    RMSE_all=list()
    Cor_all=list()
    Ratio_all=list()
    AllNames=names(Performance)[c(1,5:ncol(Performance))]
    for (i in 1:length(AllNames)){
      RMSE_all[[i]]=NaN#this placeholder first line will be removed later 
      
      Cor_all[[i]]=NaN#this placeholder first line will be removed later 
     
      Ratio_all[[i]]=NaN#this placeholder first line will be removed later 
      
      }
    names(RMSE_all)=AllNames
    names(Cor_all)=AllNames
    names(Ratio_all)=AllNames
    
    
    
    for (Location in unique(Performance$Location)){
      Vec=Performance[Performance$Location==Location,]
        for (i in 1:length(AllNames)){
        print(AllNames[i])
        RMSE_vec=sqrt(mean((Vec[,AllNames[i]]-Vec$Groundtruth)^2))
        RMSE_all[[i]]=c(RMSE_all[[i]],RMSE_vec)
        
        Cor_vec=cor(Vec[,AllNames[i]],Vec$Groundtruth)
        Cor_all[[i]]=c(Cor_all[[i]],Cor_vec)
        
        
        Ratio_vec=sqrt(mean((Vec[,AllNames[i]]-Vec$Groundtruth)^2))/sqrt(mean((Vec$Baseline-Vec$Groundtruth)^2))
        Ratio_all[[i]]=c(Ratio_all[[i]],Ratio_vec)      
        }
      
    }
    
    RMSE_all=as.data.frame(Reduce(RMSE_all,f = cbind))
    RMSE_all=RMSE_all[2:nrow(RMSE_all),]
    names(RMSE_all)=paste0("RMSE_",AllNames)
    
    Cor_all=as.data.frame(Reduce(Cor_all,f = cbind))
    Cor_all=Cor_all[2:nrow(Cor_all),]
    names(Cor_all)=paste0("Cor_",AllNames)
    
    
    Ratio_all=as.data.frame(Reduce(Ratio_all,f = cbind))
    Ratio_all=Ratio_all[2:nrow(Ratio_all),]
    names(Ratio_all)=paste0("Ratio_",AllNames)
    
    AllMetrics=cbind(RMSE_all,Cor_all,Ratio_all)
    AllMetrics$Location=unique(Performance$Location)
 

    AllMetrics=AllMetrics[order(AllMetrics$Ratio_AugmentedARGO,decreasing = F),]
    AllMetrics=AllMetrics[,!grepl(names(AllMetrics),pattern = "Groundtruth")]#remove ground trueth from the analysis
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
    
    AllCompareRMSE=c(AllCompareRMSE,sum(AllMetrics$RMSE_AugmentedARGO<AllMetrics$RMSE_baseline,na.rm = T))
    #AllCompareCOR=c(AllCompareCOR,sum(AllMetrics$<AllMetrics$RMSE_baseline))


  }


AllCompareRMSE=data.frame(Count=AllCompareRMSE,Daysahead=daysahead_all*Aggregation)

write.csv(AllCompareRMSE,file = paste0("../Visualizations/",TargetedFeature,'_',ClusteringMethod,"_different_time_horrizon_performance_withCOVIDhub.csv"))


#show a table indicate how many states which method performance best about in each time horizone 
WhichMmethdMinRMSE=NULL

WhichMmethdMaxCor=NULL

for (i in 1:length(All_allMetric)){
  Vec=All_allMetric[[i]]
  Vec=Vec[,names(Vec)[grepl(names(Vec),pattern = "RMSE")]]
  Vec2=rep(0,length(Vec))
  Vec=apply(Vec,MARGIN = 1,FUN = which.min)
  Vec=table(Vec)
  Vec2[as.integer(names(Vec))]=Vec
  WhichMmethdMinRMSE=rbind(WhichMmethdMinRMSE,Vec2)
  
  Vec=All_allMetric[[i]]
  Vec=Vec[,names(Vec)[grepl(names(Vec),pattern = "Cor")]]
  Vec2=rep(0,length(Vec))
  Vec[is.na(Vec)]=-100#remove NA
  Vec=apply(Vec,MARGIN = 1,FUN = which.max)
  Vec=table(Vec)
  Vec2[as.integer(names(Vec))]=Vec
  WhichMmethdMaxCor=rbind(WhichMmethdMaxCor,Vec2)
  
}
WhichMmethdMinRMSE=as.data.frame(WhichMmethdMinRMSE)
names(WhichMmethdMinRMSE)=names(All_allMetric[[1]])[grepl(names(All_allMetric[[1]]),pattern = "RMSE")]
rownames(WhichMmethdMinRMSE)=NULL
WhichMmethdMinRMSE$Daysahead=daysahead_all*Aggregation

WhichMmethdMaxCor=as.data.frame(WhichMmethdMaxCor)
names(WhichMmethdMaxCor)=names(All_allMetric[[1]])[grepl(names(All_allMetric[[1]]),pattern = "Cor")]
rownames(WhichMmethdMaxCor)=NULL
WhichMmethdMaxCor$Daysahead=daysahead_all*Aggregation

write.csv(WhichMmethdMinRMSE,file =paste("../Visualizations/",TargetedFeature,"_",ClusteringMethod,"_all_horizion_all_method_RMSE_withCOVID_hub.csv") )

write.csv(WhichMmethdMaxCor,file =paste("../Visualizations/",TargetedFeature,"_",ClusteringMethod,"_all_horizion_all_method_COR_withCOVID_hub.csv") )





#Visualize performance by US state map 

#US map performance 

library(usmap)
library(ggplot2)
library(viridis)

VecPlot= All_allMetric[[2]]
statepop=as.data.frame(statepop)
statepop$location=tolower(statepop$full)
statepop$location=paste0("US-",statepop$location)

VecPlot$ARGO.RMSE_Over_Persistence.RMSE=VecPlot$RMSE_AugmentedARGO/VecPlot$RMSE_Baseline
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



ggsave(paste0("../Visualizations/",ExperimentName,"_Performance_US_map_withCOVID_hub ",".pdf"),device = "pdf",scale = 2)




#Barplot 
dev.new()
VecPlot= All_allMetric[[2]]
statepop=as.data.frame(statepop)
statepop$location=tolower(statepop$full)
statepop$location=paste0("US-",statepop$location)

VecPlot$state=statepop$full[match(VecPlot$Location,statepop$location)]
VecPlot2=NULL
for (Target in c("Ratio_AugmentedARGO", "Ratio_COVID_hub_ensemble", "Ratio_JHU_prediction")){
  VecPlot2=rbind(VecPlot2,data.frame(state=VecPlot$state,Ratio=VecPlot[,Target],Type=Target))
}
VecPlot2=as.data.frame(VecPlot2)



VecPlot2$Type= factor(VecPlot2$Type, levels = c("Ratio_AugmentedARGO", "Ratio_COVID_hub_ensemble", "Ratio_JHU_prediction" ))

#column for ordering in plot
Vec=VecPlot2[VecPlot2$Type=="Ratio_AugmentedARGO",]
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


ggsave(paste0("../Visualizations/",ExperimentName,"_Performance_bar_withCovid_hub",".pdf"),device = "pdf",height = 7,width = 14)








#scatter plot of each method over each time horrizon 

VecPlot=Reduce(All_allMetric,f = rbind)
library(ggplot2)



#Rearrange data for plotting 
names(VecPlot)

VecAll=NULL
for (ColVec in c("RMSE_AugmentedARGO","RMSE_Baseline" ,"RMSE_COVID_hub_ensemble" ,"Ratio_YYG_prediction","Ratio_JHU_prediction" )){
  Vec=VecPlot[,c(ColVec,"Location","Daysahead")]
  names(Vec)=c("Value","Location","Daysahead")
  Vec$Method=strsplit(ColVec,split = "[_]")[[1]][2]
  VecAll=rbind(VecAll,Vec)
}
VecPlot=VecAll
#VecPlot$Daysahead=as.factor(VecPlot$Daysahead)

library(car)
head(VecPlot)
unique(VecPlot$Method)


VecPlot$Method=factor(VecPlot$Method,levels=c("AugmentedARGO","Baseline" ,"COVID" ,"YYG","JHU"))


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

pdf(paste("../Visualizations/",TargetedFeature,"_",ClusteringMethod,"_All_province_RMSE_withCOVIDHub.pdf"), onefile = TRUE)
for (i in seq(length(AllPlots))) {
  print(AllPlots[[i]])  
}
dev.off()


dev.new()








#Prediction based visualization ( original curse )


#Aggregation=2


pdf(file=paste("../Visualizations/",TargetedFeature,"_",ClusteringMethod,"_visualization_all_gap_withCOVID_HUB.pdf"),width = 10,height = 10)
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
    
    
    #COVID_hub enemble 
    #data from COIVDhub 
    X=load("../figures/COVID_hub_preds.Rdata")
    COVIDHub=get(X)
    
    
    Vec=COVIDHub$COVID_hub_baseline
    VecPlot$COVID_hub_baseline=Vec$value[match(interaction(tolower(VecPlot$Location),VecPlot$Date),interaction(tolower(Vec$Location),Vec$target_end_date))]
    Vec=COVIDHub$COVID_hub_ensemble
    VecPlot$COVID_hub_ensemble=Vec$value[match(interaction(tolower(VecPlot$Location),VecPlot$Date),interaction(tolower(Vec$Location),Vec$target_end_date))]
    
    Vec=COVIDHub$CMU_prediction
    VecPlot$CMU_prediction=Vec$value[match(interaction(tolower(VecPlot$Location),VecPlot$Date),interaction(tolower(Vec$Location),Vec$target_end_date))]
    Vec=COVIDHub$YYG_prediction
    VecPlot$YYG_prediction=Vec$value[match(interaction(tolower(VecPlot$Location),VecPlot$Date),interaction(tolower(Vec$Location),Vec$target_end_date))]
    Vec=COVIDHub$JHU_prediction
    VecPlot$JHU_prediction=Vec$value[match(interaction(tolower(VecPlot$Location),VecPlot$Date),interaction(tolower(Vec$Location),Vec$target_end_date))]
    
    #plot
    plot(VecPlot$Count~VecPlot$Date,
         xlim=c(as.Date("2020-06-30"),as.Date("2020-10-01")),
         ylim=c(0,max(max(VecPlot$Count,na.rm = T),max(Predicted$Count,na.rm = T))),
         type="l",lwd=3,
         xlab="Date",ylab=TargetedFeature,
         main=paste(Location,daysahead*Aggregation,"Days ahead of time"))
    
    
    
    #keep all the plot the same starting date
    
    Predicted=Predicted[Predicted$Date>=InitialData,]
    Baseline=Baseline[Baseline$Date>=InitialData,]
    lines(Predicted$Count~Predicted$Date,col="red",lwd=2)
    lines(Baseline$Count~Baseline$Date,col="orange",lwd=2,lty=2)
    
    lines(VecPlot$COVID_hub_ensemble~ VecPlot$Date,col="blue",lwd=1,lty=2)
    lines(VecPlot$JHU_prediction~ VecPlot$Date,col="brown",lwd=1,lty=2)
    lines(VecPlot$YYG_prediction~ VecPlot$Date,col="green",lwd=1,lty=2)
    
  
    
    
    abline(v =min(InitialData),col="grey",lty=2,lwd=2)
    legend("topleft", lty=c(1,1,2,2,2,2),
           col = c("black","red","orange","blue",'brown','green'), 
           legend = c("Observed","ARGO","Persistence",'COVID_hub_ensemble',"JHU","YYG"))
    
    
    
    
  }
}
dev.off()
par(mfrow = c(1, 1))
