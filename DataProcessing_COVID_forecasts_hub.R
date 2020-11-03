#This version use data from Leo 


#Location data 
LocationData='C:/Users/kaiyu/Google Drive/research/Coronavirus/covid-19_us_and_world/data/covid19-forecast-hub-master/data-locations/locations.csv'
LocationFile=read.csv(LocationData)
head(LocationFile)
LocationFile$State=paste0("US-",LocationFile$location_name)


#COVID-hub baseline 


FileDir='C:/Users/kaiyu/Google Drive/research/Coronavirus/covid-19_us_and_world/data/covid19-forecast-hub-master/data-processed/COVIDhub-baseline/'

AllFiles=list.files(FileDir)
AllFiles=AllFiles[grepl(AllFiles,pattern = ".csv")]
AllDates=as.Date(substr(AllFiles,start = 1,stop = 10))

AllData=NULL
for (i in 1:length(AllFiles)){
  Vec=read.csv(paste0(FileDir,AllFiles[i]))
  Vec=Vec[Vec$location!="US",]
  Vec$Location=LocationFile$State[match(as.integer(Vec$location),as.integer(LocationFile$location))]
  Vec=Vec[(Vec$target=="1 wk ahead inc death")&((Vec$type=="point")),]
  
  AllData=rbind(AllData,Vec)
  
}

AllData=AllData[,c("Location","target_end_date",'value')]

COVID_hub_baseline=AllData



#COVID-hub essenble


FileDir='C:/Users/kaiyu/Google Drive/research/Coronavirus/covid-19_us_and_world/data/covid19-forecast-hub-master/data-processed/COVIDhub-ensemble/'

AllFiles=list.files(FileDir)
AllFiles=AllFiles[grepl(AllFiles,pattern = ".csv")]
AllDates=as.Date(substr(AllFiles,start = 1,stop = 10))

AllData=NULL
for (i in 1:length(AllFiles)){
  Vec=read.csv(paste0(FileDir,AllFiles[i]))
  Vec=Vec[Vec$location!="US",]
  Vec$Location=LocationFile$State[match(as.integer(Vec$location),as.integer(LocationFile$location))]
  Vec=Vec[(Vec$target=="1 wk ahead inc death")&((Vec$type=="point")),]
  
  AllData=rbind(AllData,Vec)
  
}

AllData=AllData[,c("Location","target_end_date",'value')]

COVID_hub_ensemble=AllData


#CMU predictions
FileDir='C:/Users/kaiyu/Google Drive/research/Coronavirus/covid-19_us_and_world/data/covid19-forecast-hub-master/data-processed/CMU-TimeSeries/'

AllFiles=list.files(FileDir)
AllFiles=AllFiles[grepl(AllFiles,pattern = ".csv")]
AllDates=as.Date(substr(AllFiles,start = 1,stop = 10))

AllData=NULL
for (i in 1:length(AllFiles)){
  Vec=read.csv(paste0(FileDir,AllFiles[i]))
  Vec$Location=LocationFile$State[match(Vec$location,as.integer(LocationFile$location))]
  Vec=Vec[(Vec$target=="1 wk ahead inc death")&((Vec$type=="point")),]
  AllData=rbind(AllData,Vec)
  
}

AllData=AllData[,c("Location","target_end_date",'value')]

CMU_prediction=AllData



#YYG predictions
FileDir='C:/Users/kaiyu/Google Drive/research/Coronavirus/covid-19_us_and_world/data/covid19-forecast-hub-master/data-processed/YYG-ParamSearch/'

AllFiles=list.files(FileDir)
AllFiles=AllFiles[grepl(AllFiles,pattern = ".csv")]
AllDates=as.Date(substr(AllFiles,start = 1,stop = 10))

AllData=NULL
for (i in 1:length(AllFiles)){
  Vec=read.csv(paste0(FileDir,AllFiles[i]))
  Vec=Vec[Vec$location!="US",]
  Vec$Location=LocationFile$State[match(as.integer(Vec$location),as.integer(LocationFile$location))]
  Vec=Vec[(Vec$target=="1 wk ahead inc death")&((Vec$type=="point")),]

  AllData=rbind(AllData,Vec)
}

AllData=AllData[,c("Location","target_end_date",'value')]

YYG_prediction=AllData



#JHU IDD predictions
FileDir='C:/Users/kaiyu/Google Drive/research/Coronavirus/covid-19_us_and_world/data/covid19-forecast-hub-master/data-processed/JHU_IDD-CovidSP/'

AllFiles=list.files(FileDir)
AllFiles=AllFiles[grepl(AllFiles,pattern = ".csv")]
AllDates=as.Date(substr(AllFiles,start = 1,stop = 10))
AllFiles=AllFiles[AllDates>"2020-07-19"]

AllData=NULL
for (i in 1:length(AllFiles)){
  Vec=read.csv(paste0(FileDir,AllFiles[i]))
  Vec=Vec[Vec$location!="US",]
  Vec$Location=LocationFile$State[match(as.integer(Vec$location),as.integer(LocationFile$location))]
  Vec=Vec[(Vec$target=="1 wk ahead inc death")&((Vec$type=="point")),]
  
  AllData=rbind(AllData,Vec)
}

AllData=AllData[,c("Location","target_end_date",'value')]

JHU_prediction=AllData


#save the data 

AllData=list(COVID_hub_baseline=COVID_hub_baseline,
             COVID_hub_ensemble=COVID_hub_ensemble,
             CMU_prediction=CMU_prediction,
             YYG_prediction=YYG_prediction,
             JHU_prediction=JHU_prediction)

save(AllData,file = '../figures/COVID_hub_preds.Rdata')
