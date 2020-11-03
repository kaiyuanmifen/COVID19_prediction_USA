#This version use data from Leo 


#Data processed by Leo

FileDir='./data/Leo_data/2020-09-21/all_merged/'

#gsub(FileDir,pattern = "\ ",replacement = "/")

AllFiles=list.files(FileDir)
AllFiles=AllFiles[grepl("csv",x = AllFiles)]

AllStates=NULL
for (File in AllFiles){
  print(File)
  Vec=read.csv(paste0(FileDir,File))
  names(Vec)[1]="Date"
  
  Location=tolower(strsplit(File,split = "[.]")[[1]][1])
  Location=paste0("US-",Location)
  
  #New cases 
  Vec$JHK_New_confirmed=Vec$jhk_positiveTests_cumulative-c(0,Vec$jhk_positiveTests_cumulative[1:(nrow(Vec)-1)])
  #New Death
  Vec$covidtracking_new_deaths=Vec$covidtracking_deaths_cumulative-c(0,Vec$covidtracking_deaths_cumulative[1:(nrow(Vec)-1)])
  #New hospitalization
  Vec$NewHospitalization=Vec$covidtracking_hospitalizedCumulative-c(0,Vec$covidtracking_hospitalizedCumulative[1:(nrow(Vec)-1)])
  
  #new infected by gleamn gleam model data are in another folder 
  Vec=Vec[,!grepl(pattern = "gleam",names(Vec))]
  #Vec$gleammodel_increase_Infectious=Vec$gleammodel_total_Infectious_mean-c(0,Vec$gleammodel_total_Infectious_mean[1:(nrow(Vec)-1)])
  
  Vec$Location=Location
  #somehow certain states has few columns than others , need to unify
  if(!is.null(AllStates)){
    ColSelected=intersect(names(AllStates),names(Vec))
    AllStates=AllStates[,ColSelected]
    Vec=Vec[,ColSelected]

  }
  print(nrow(Vec))
  AllStates=rbind(AllStates,Vec)
}

names(AllStates)

# 
# #interventions by differnet states 
# Target=read.csv(paste0(DateDier,"covid-19_us_and_world/data/case/interventions/COVID-19 US state policy database (CUSP) - State policy changes - fully tracked to 5_22_2020.csv"),header = T,skip = 0)
# head(Target)
# Target=Target[1:51,]
# Target$State=paste0("US-",Target$State)
# names(Target)
# 
# unique(Target$State)
# 
# 
# AllDate_Vec=NULL
# for (Date in c(min(COVID_Tracking$Date)+1:as.integer(abs(min(COVID_Tracking$Date)-max(COVID_Tracking$Date))))){
#   Date=as.Date(Date,origin = "1970-01-01")
#   print(Date)
#   Vec_day=Target
#   Vec=Vec_day[,3:48]
#   #change everything to date
#   
#   Vec=apply(Vec,MARGIN = c(1,2),function(x){as.character(x)})
#   Vec[Vec=="1"]="1/1/2020"
#   Vec[Vec=="0"]="1/1/2025"
#   
#   
#   Vec[apply(Vec,MARGIN = c(1,2),function(x){!grepl(x,pattern = "[/]")})]="1/1/2025"
#   
#   Vec=apply(Vec,MARGIN = c(1,2),function(x){as.Date(x,format="%m/%d/%y",origin="1970-01-01")})
#   
#   Vec=apply(Vec,MARGIN = c(1,2),function(x){x<=Date})
#   Vec=apply(Vec,MARGIN = c(1,2),function(x){as.integer(x)})
#   Vec_day$TotalNumberOfInterventios=apply(Vec,FUN = function(x){sum(x,na.rm = T)},1)
#   
#   Vec_day$Date=Date
#   AllDate_Vec=rbind(AllDate_Vec,Vec_day)
# }
# 
# AllDate_Vec[is.na(AllDate_Vec)]=0
# 
# Interventions=AllDate_Vec
# names(Interventions)=paste0("Interventions:",names(Interventions))





AllStates[is.na(AllStates)]=0

print(paste("total number of states:",length(unique(AllStates$Location))))

write.csv(AllStates,file = '../results/OrganizedData_US_countries.csv')





DateDier="C:/Users/kaiyu/Google Drive/research/Coronavirus/"
#UsA infor

FileDir="covid-19_us_and_world/data/case/JohnHopKinsCSSEData/20200525/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"

Target=read.csv(paste0(DateDier,FileDir))

#for US, Only keep states, other country aggregate
library(readxl)
US_state_code=as.data.frame(read_excel(paste0(DateDier,"data/coronavirus_data/US_state_codes.xlsx")))


#IHME prediction ( use the nearly project value)

list.files(DateDier)
IHME_prediction=NULL


Target=read.csv(paste0(DateDier,"covid-19_us_and_world/data/case/MurrayPrediction/20200421/","Hospitalization_all_locs.csv"),header = T)
Target$location_name=as.character(Target$location_name)
Target=Target[Target$location_name%in%US_state_code$State,]
Target$location_name=paste0("US-",tolower(Target$location_name))
unique(Target$location_name)
tail(Target)
Target=Target[,2:ncol(Target)]
Target$date=as.Date(Target$date)
Target$DateMakingPrediction="2020-04-21"
#Target=Target[(Target$date>Target$DateMakingPrediction),]
range(Target$date)



Target=read.csv(paste0(DateDier,"covid-19_us_and_world/data/case/MurrayPrediction/20200416/","Hospitalization_all_locs.csv"),header = T)
Target$location_name=as.character(Target$location_name)
Target=Target[Target$location_name%in%US_state_code$State,]
Target$location_name=paste0("US-",tolower(Target$location_name))
unique(Target$location_name)
tail(Target)
Target=Target[,2:ncol(Target)]
Target$date=as.Date(Target$date)
Target$DateMakingPrediction="2020-04-16"
#Target=Target[(Target$date>Target$DateMakingPrediction),]
range(Target$date)


AllNames=c("location_name","date","allbed_mean","ICUbed_mean","InvVen_mean","admis_mean" ,"newICU_mean", "totdea_mean","deaths_mean","DateMakingPrediction")
Target=Target[,AllNames]
IHME_prediction=rbind(IHME_prediction,Target)

Target=read.csv(paste0(DateDier,"covid-19_us_and_world/data/case/MurrayPrediction/20200423/","Hospitalization_all_locs.csv"),header = T)
Target$location_name=as.character(Target$location_name)
Target=Target[Target$location_name%in%US_state_code$State,]
Target$location_name=paste0("US-",tolower(Target$location_name))
unique(Target$location_name)
tail(Target)
#Target=Target[,2:ncol(Target)]
Target$date=as.Date(Target$date)
Target$DateMakingPrediction="2020-04-23"
#Target=Target[(Target$date>Target$DateMakingPrediction),]
range(Target$date)
Target=Target[,AllNames]
IHME_prediction=rbind(IHME_prediction,Target)


Target=read.csv(paste0(DateDier,"covid-19_us_and_world/data/case/MurrayPrediction/20200428/","Hospitalization_all_locs.csv"),header = T)
Target$location_name=as.character(Target$location_name)
Target=Target[Target$location_name%in%US_state_code$State,]
Target$location_name=paste0("US-",tolower(Target$location_name))
unique(Target$location_name)
tail(Target)
#Target=Target[,2:ncol(Target)]
Target$date=as.Date(Target$date)
Target$DateMakingPrediction="2020-04-23"
#Target=Target[(Target$date>Target$DateMakingPrediction),]
range(Target$date)
Target=Target[,AllNames]
IHME_prediction=rbind(IHME_prediction,Target)


Target=read.csv(paste0(DateDier,"covid-19_us_and_world/data/case/MurrayPrediction/20200503/","Hospitalization_all_locs.csv"),header = T)
Target$location_name=as.character(Target$location_name)
Target=Target[Target$location_name%in%US_state_code$State,]
Target$location_name=paste0("US-",tolower(Target$location_name))
unique(Target$location_name)
tail(Target)
Target=Target[,2:ncol(Target)]
Target$date=as.Date(Target$date)
Target$DateMakingPrediction="2020-05-03"
#Target=Target[(Target$date>Target$DateMakingPrediction),]
range(Target$date)
Target=Target[,AllNames]
IHME_prediction=rbind(IHME_prediction,Target)


Target=read.csv(paste0(DateDier,"covid-19_us_and_world/data/case/MurrayPrediction/20200513/","Hospitalization_all_locs.csv"),header = T)
Target$location_name=as.character(Target$location_name)
Target=Target[Target$location_name%in%US_state_code$State,]
Target$location_name=paste0("US-",tolower(Target$location_name))
unique(Target$location_name)
tail(Target)
Target=Target[,2:ncol(Target)]
Target$date=as.Date(Target$date)
Target$DateMakingPrediction="2020-05-13"
#Target=Target[(Target$date>Target$DateMakingPrediction),]
range(Target$date)
Target=Target[,AllNames]
IHME_prediction=rbind(IHME_prediction,Target)


Target=read.csv(paste0(DateDier,"covid-19_us_and_world/data/case/MurrayPrediction/20200520/","Hospitalization_all_locs.csv"),header = T)
Target$location_name=as.character(Target$location_name)
Target=Target[Target$location_name%in%US_state_code$State,]
Target$location_name=paste0("US-",tolower(Target$location_name))
unique(Target$location_name)
tail(Target)
Target=Target[,2:ncol(Target)]
Target$date=as.Date(Target$date)
Target$DateMakingPrediction="2020-05-20"
#Target=Target[(Target$date>Target$DateMakingPrediction),]
range(Target$date)
Target=Target[,AllNames]
IHME_prediction=rbind(IHME_prediction,Target)

Target=read.csv(paste0(DateDier,"covid-19_us_and_world/data/case/MurrayPrediction/20200525/","Hospitalization_all_locs.csv"),header = T)
Target$location_name=as.character(Target$location_name)
Target=Target[Target$location_name%in%US_state_code$State,]
Target$location_name=paste0("US-",tolower(Target$location_name))
unique(Target$location_name)
tail(Target)
Target=Target[,2:ncol(Target)]
Target$date=as.Date(Target$date)
Target$DateMakingPrediction="2020-05-25"
#Target=Target[(Target$date>Target$DateMakingPrediction),]
range(Target$date)
Target=Target[,AllNames]
IHME_prediction=rbind(IHME_prediction,Target)



Target=read.csv(paste0(DateDier,"covid-19_us_and_world/data/case/MurrayPrediction/20200605/","Hospitalization_all_locs.csv"),header = T)
Target$location_name=as.character(Target$location_name)
Target=Target[Target$location_name%in%US_state_code$State,]
Target$location_name=paste0("US-",tolower(Target$location_name))
unique(Target$location_name)
tail(Target)
Target=Target[,2:ncol(Target)]
Target$date=as.Date(Target$date)
Target$DateMakingPrediction="2020-06-05"
#Target=Target[(Target$date>Target$DateMakingPrediction),]
range(Target$date)
Target=Target[,AllNames]
IHME_prediction=rbind(IHME_prediction,Target)



Target=read.csv(paste0(DateDier,"covid-19_us_and_world/data/case/MurrayPrediction/20200608/","Hospitalization_all_locs.csv"),header = T)
Target$location_name=as.character(Target$location_name)
Target=Target[Target$location_name%in%US_state_code$State,]
Target$location_name=paste0("US-",tolower(Target$location_name))
unique(Target$location_name)
tail(Target)
Target=Target[,2:ncol(Target)]
Target$date=as.Date(Target$date)
Target$DateMakingPrediction="2020-06-08"
#Target=Target[(Target$date>Target$DateMakingPrediction),]
range(Target$date)
Target=Target[,AllNames]
IHME_prediction=rbind(IHME_prediction,Target)




Target=read.csv(paste0(DateDier,"covid-19_us_and_world/data/case/MurrayPrediction/20200613/","Hospitalization_all_locs.csv"),header = T)
Target$location_name=as.character(Target$location_name)
Target=Target[Target$location_name%in%US_state_code$State,]
Target$location_name=paste0("US-",tolower(Target$location_name))
unique(Target$location_name)
tail(Target)
Target=Target[,2:ncol(Target)]
Target$date=as.Date(Target$date)
Target$DateMakingPrediction="2020-06-13"
#Target=Target[(Target$date>Target$DateMakingPrediction),]
range(Target$date)
Target=Target[,AllNames]
IHME_prediction=rbind(IHME_prediction,Target)



Target=read.csv(paste0(DateDier,"covid-19_us_and_world/data/case/MurrayPrediction/20200624/","Reference_hospitalization_all_locs.csv"),header = T)
Target$location_name=as.character(Target$location_name)
Target=Target[Target$location_name%in%US_state_code$State,]
Target$location_name=paste0("US-",tolower(Target$location_name))
unique(Target$location_name)
tail(Target)
Target=Target[,2:ncol(Target)]
Target$date=as.Date(Target$date)
Target$DateMakingPrediction="2020-06-24"
#Target=Target[(Target$date>Target$DateMakingPrediction),]
range(Target$date)
Target=Target[,AllNames]
IHME_prediction=rbind(IHME_prediction,Target)



Target=read.csv(paste0(DateDier,"covid-19_us_and_world/data/case/MurrayPrediction/20200627/","Reference_hospitalization_all_locs.csv"),header = T)
Target$location_name=as.character(Target$location_name)
Target=Target[Target$location_name%in%US_state_code$State,]
Target$location_name=paste0("US-",tolower(Target$location_name))
unique(Target$location_name)
tail(Target)
Target=Target[,2:ncol(Target)]
Target$date=as.Date(Target$date)
Target$DateMakingPrediction="2020-06-27"
#Target=Target[(Target$date>Target$DateMakingPrediction),]
range(Target$date)
Target=Target[,AllNames]
IHME_prediction=rbind(IHME_prediction,Target)



Target=read.csv(paste0(DateDier,"covid-19_us_and_world/data/case/MurrayPrediction/20200704/","Reference_hospitalization_all_locs.csv"),header = T)
Target$location_name=as.character(Target$location_name)
Target=Target[Target$location_name%in%US_state_code$State,]
Target$location_name=paste0("US-",tolower(Target$location_name))
unique(Target$location_name)
tail(Target)
Target=Target[,2:ncol(Target)]
Target$date=as.Date(Target$date)
Target$DateMakingPrediction="2020-07-04"
#Target=Target[(Target$date>Target$DateMakingPrediction),]
range(Target$date)
Target=Target[,AllNames]
IHME_prediction=rbind(IHME_prediction,Target)




Target=read.csv(paste0(DateDier,"covid-19_us_and_world/data/case/MurrayPrediction/20200711/","Reference_hospitalization_all_locs.csv"),header = T)
Target$location_name=as.character(Target$location_name)
Target=Target[Target$location_name%in%US_state_code$State,]
Target$location_name=paste0("US-",tolower(Target$location_name))
unique(Target$location_name)
tail(Target)
Target=Target[,2:ncol(Target)]
Target$date=as.Date(Target$date)
Target$DateMakingPrediction="2020-07-11"
#Target=Target[(Target$date>Target$DateMakingPrediction),]
range(Target$date)
Target=Target[,AllNames]
IHME_prediction=rbind(IHME_prediction,Target)




Target=read.csv(paste0(DateDier,"covid-19_us_and_world/data/case/MurrayPrediction/20200718/","Reference_hospitalization_all_locs.csv"),header = T)
Target$location_name=as.character(Target$location_name)
Target=Target[Target$location_name%in%US_state_code$State,]
Target$location_name=paste0("US-",tolower(Target$location_name))
unique(Target$location_name)
tail(Target)
Target=Target[,2:ncol(Target)]
Target$date=as.Date(Target$date)
Target$DateMakingPrediction="2020-07-18"
#Target=Target[(Target$date>Target$DateMakingPrediction),]
range(Target$date)
Target=Target[,AllNames]
IHME_prediction=rbind(IHME_prediction,Target)


Target=read.csv(paste0(DateDier,"covid-19_us_and_world/data/case/MurrayPrediction/20200718/","Reference_hospitalization_all_locs.csv"),header = T)
Target$location_name=as.character(Target$location_name)
Target=Target[Target$location_name%in%US_state_code$State,]
Target$location_name=paste0("US-",tolower(Target$location_name))
unique(Target$location_name)
tail(Target)
Target=Target[,2:ncol(Target)]
Target$date=as.Date(Target$date)
Target$DateMakingPrediction="2020-08-06"
#Target=Target[(Target$date>Target$DateMakingPrediction),]
range(Target$date)
Target=Target[,AllNames]
IHME_prediction=rbind(IHME_prediction,Target)



Target=read.csv(paste0(DateDier,"covid-19_us_and_world/data/case/MurrayPrediction/20200718/","Reference_hospitalization_all_locs.csv"),header = T)
Target$location_name=as.character(Target$location_name)
Target=Target[Target$location_name%in%US_state_code$State,]
Target$location_name=paste0("US-",tolower(Target$location_name))
unique(Target$location_name)
tail(Target)
Target=Target[,2:ncol(Target)]
Target$date=as.Date(Target$date)
Target$DateMakingPrediction="2020-09-11"
#Target=Target[(Target$date>Target$DateMakingPrediction),]
range(Target$date)
Target=Target[,AllNames]
IHME_prediction=rbind(IHME_prediction,Target)

names(IHME_prediction)[3:9]=paste0("IHME_",names(IHME_prediction[3:9]))
names(IHME_prediction)[1]="Location"

write.csv(IHME_prediction,file = '../results/IHME_prediction.csv')





#Gleam predictions


FileDir='C:/Users/kaiyu/Google Drive/research/Coronavirus/covid-19_us_and_world/data/Leo_data/2020-08-08/all_merged/gleam/'

#gsub(FileDir,pattern = "\ ",replacement = "/")

AllFiles=list.files(FileDir)
AllFiles=AllFiles[grepl("csv",x = AllFiles)]

AllStates=NULL
for (File in AllFiles){
  Vec=read.csv(paste0(FileDir,File))
  names(Vec)[1]="Date"
  
  Location=tolower(strsplit(File,split = "[.]")[[1]][1])
  Location=paste0("US-",Location)
 
  Vec= Vec[,1:(ncol(Vec)-1)]
  VecAllDate=NULL
  for (i in 2:ncol(Vec)){
    Vec2=Vec[,c(1,i)]
    names(Vec2)=c("date","gleam_infectious_mean")
    Vec2$DateMakingPrediction= as.Date(substr(names(Vec)[i],start = 2,stop = nchar(names(Vec)[i])), tryFormats = c("%Y.%m.%d"))
    VecAllDate=rbind(VecAllDate,Vec2)
    }
  VecAllDate$Location=Location
  VecAllDate=VecAllDate[!(is.na(VecAllDate$gleam_infectious_mean)),]
  AllStates=rbind(AllStates,VecAllDate)
}

names(AllStates)
head(AllStates)
write.csv(AllStates,file = '../results/GleamPredictionss.csv')

