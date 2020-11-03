
DateDier="/Users/dianboliu/Google_Drive/research/Coronavirus/"

#JHK data for each location each date 
# 
# FileDir="case/JohnHopKinsCSSEData/20200312_late/csse_covid_19_data/csse_covid_19_daily_reports/"
# AllFiles=list.files(paste0(DateDier,FileDir))
# AllFiles=AllFiles[grepl(pattern = '.csv',x = AllFiles)]
# 
# AllData=NULL
# for (File in AllFiles){
#   DateVec=strsplit(File,split = "[.]")[[1]][1]
#   DateVec=as.character(as.Date(DateVec,format = "%m-%d-%y",origin="1970-01-01"))
#   Target=read.csv(paste0(DateDier,FileDir,File))
#   Target$Location=paste0(Target$Country.Region,"-",Target$Province.State)
#   Target$Date=DateVec
#   Target=Target[,!colnames(Target)%in%c("Latitude","Longitude")]
#   AllData=rbind(AllData,Target)
# }
# 
# #calculate new cases 
# 
# AllData2=NULL
# for (Date in unique(AllData$Date)){
#   Vec=AllData[AllData$Date==Date,]
#   
#   Vec_2=AllData[AllData$Date==(as.Date(Date)-1),]
#   Vec$Confirmed_previous1=Vec_2$Confirmed[match(Vec$Location,Vec_2$Location)]
#   Vec$Deaths_previous1=Vec_2$Deaths[match(Vec$Location,Vec_2$Location)]
#   Vec$Recovered_previous1=Vec_2$Recovered[match(Vec$Location,Vec_2$Location)]
#   
#   Vec$New_confirmed=Vec$Confirmed-Vec$Confirmed_previous1
#   Vec$New_confirmed[is.na(Vec$New_confirmed)]=0
#   Vec$New_death=Vec$Deaths-Vec$Deaths_previous1
#   Vec$New_death[is.na(Vec$New_death)]=0
#   Vec$New_recovered=Vec$Recovered-Vec$Recovered_previous1
#   Vec$New_recovered[is.na(Vec$New_recovered)]=0
#   names(Vec)=c("Province.State","Country.Region","Last.Update" ,"Cumulative_confirmed","Cumulative_death", "Cumulative_recovered" ,"Location" , "Date",            
#               "Confirmed_previous1","Deaths_previous1" ,"Recovered_previous1", "New_confirmed" ,"New_death", "New_recovered")
#   AllData2=rbind(AllData2,Vec[,c("Location" ,"Date" , "Last.Update" ,
#                                  "Cumulative_confirmed","Cumulative_death", "Cumulative_recovered",
#                                  "New_confirmed","New_death","New_recovered")])
# }
#   
# JHK=AllData2

FileDir="covid-19_us_and_world/data/case/JohnHopKinsCSSEData/20200525/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"

Target=read.csv(paste0(DateDier,FileDir))

#for US, Only keep states, other country aggregate
library(readxl)
US_state_code=as.data.frame(read_excel(paste0(DateDier,"data/coronavirus_data/US_state_codes.xlsx")))
Target_US=Target[Target$Country_Region=="US",]
Target_US=Target_US[tolower(Target_US$Province_State)%in%tolower(US_state_code$State),]

Vec_Location=paste0(Target_US$Country_Region,"-",Target_US$Province_State)
Target_US=Target_US[,12:ncol(Target_US)]
Vec=names(Target_US)[1:ncol(Target_US)]
Vec=gsub(Vec,pattern = "X",replacement = "")
Vec=as.character(as.Date(Vec,format = "%m.%d.%y"))
colnames(Target_US)=Vec
Target_US=cbind(Location=Vec_Location,Target_US)

AllTargets=NULL
for (i in 3:ncol(Target_US)){
  Vec=data.frame(Location=Target_US$Location,Count=Target_US[,i],Date=colnames(Target_US)[i])
  Date=Vec$Date

  Vec=aggregate(Vec$Count,by=list(Category=Vec$Location), FUN=sum)
  Vec$Date=unique(Date)
  names(Vec)=c("Location","Count")

  Vec_1=data.frame(Location=Target_US$Location,Count=Target_US[,colnames(Target_US)==as.character(as.Date(colnames(Target_US)[i])-1)],Date=as.Date(colnames(Target_US)[i])-1)

  Vec_1=aggregate(Vec_1$Count,by=list(Category=Vec_1$Location), FUN=sum)
  Vec_1$Date=unique(Date)
  names(Vec_1)=c("Location","Count")
  if(length(Vec_1$Count)>0){
    Vec$New_count=Vec$Count-Vec_1$Count
  }
  if(length(Vec_1$Count)==0){
    Vec$New_count=0
  }
  names(Vec)=c("Location","Count","Date","New_count")
  Vec=Vec[,c("Location","Date","Count","New_count")]
  AllTargets=rbind(AllTargets,Vec)
}

Target_US=AllTargets




# Target_countries=Target[Target$Country.Region!="US",]
# Target_countries=Target_countries[,c(2,5:ncol(Target_countries))]
# Target_countries=aggregate(.~Country.Region,data=Target_countries,FUN=sum)
# Vec=names(Target_countries)[2:ncol(Target_countries)]
# Vec=gsub(Vec,pattern = "X",replacement = "")
# Vec=as.character(as.Date(Vec,format = "%m.%d.%y"))
# colnames(Target_countries)[2:ncol(Target_countries)]=Vec
# names(Target_countries)[1]="Location"
#
# AllTargets=NULL
# for (i in 2:ncol(Target_countries)){
#   Vec=data.frame(Location=Target_countries$Location,Count=Target_countries[,i],Date=colnames(Target_countries)[i])
#
#   Vec_1=data.frame(Location=Target_countries$Location,Count=Target_countries[,colnames(Target_countries)==as.character(as.Date(colnames(Target_countries)[i])-1)],Date=as.Date(colnames(Target_countries)[i])-1)
#   if(length(Vec_1$Count)>0){
#   Vec$New_count=Vec$Count-Vec_1$Count
#   }
#   if(length(Vec_1$Count)==0){
#     Vec$New_count=0
#   }
#   Vec=Vec[,c("Location","Date","Count","New_count")]
#   AllTargets=rbind(AllTargets,Vec)
#   }
# Target_countries=AllTargets

#JHK=rbind(Target_US,Target_countries)
JHK=Target_US
names(JHK)=c("Location" ,"Date" ,"Cumulative_confirmed", "New_confirmed")




#COVID-tracking data 
FileDir="covid-19_us_and_world/data/case/COVID-track/20200525/daily.csv"

Target=read.csv(paste0(DateDier,FileDir))
head(Target)
Target$state=as.character(Target$state)
library(readxl)
US_state_code=as.data.frame(read_excel(paste0(DateDier,"data/coronavirus_data/US_state_codes.xlsx")))
head(US_state_code)
Target$Location=US_state_code$State[match(Target$state,US_state_code$`2_code`)]
head(Target,100)
Target=Target[!is.na(Target$Location),]#overseas distribute are removed 

Target$Location=paste0("US-",Target$Location)
tail(Target)
Date=as.Date(Target$dateChecked,origin="1970-01-01")
Target=Target[,c("Location","positive","negative","pending","hospitalizedCurrently","hospitalizedCumulative",
                 "inIcuCurrently","inIcuCumulative","onVentilatorCurrently","onVentilatorCumulative","recovered","death",
                 "deathIncrease","hospitalizedIncrease","negativeIncrease","positiveIncrease","totalTestResultsIncrease")]

Target$PositiveRate=Target$positive/(Target$positive+Target$negative)

Target$Date=Date
Target=Target[with(Target, order(Location, Date)), ]
#New hospitalized
 Target$NewHospitalized=0
 for (Location in unique(Target$Location)){
   Vec=Target[Target$Location==Location,"hospitalizedCumulative"]
   Vec[1]=max(c(0,Vec[1]),na.rm = T)
   #Impute missing value using previous number 
   for (i in 1:length(Vec)){
     if(is.na(Vec[i])){
       Vec[i]=Vec[i-1]
     }
   }
   Target[Target$Location==Location,"hospitalizedCumulative"]=Vec
   Target$NewHospitalized[Target$Location==Location]=Vec-c(0,Vec[1:(length(Vec)-1)])#there are some -ve values due to inaccuracy of the data 
 }

#Target=Target[Target$Date<"2020-04-15",]#to ensure other dataset has the date 
COVID_Tracking=Target

tail(COVID_Tracking)



#media cloud per province
FileDir="covid-19_us_and_world/data/media_cloud_US_othercountries/2020-05-25/MediaCloud.csv"
Media_cloud_by_location=as.data.frame(read.csv(paste0(DateDier,FileDir)))
head(Media_cloud_by_location)
Media_cloud_by_location$publish_date=as.character(Media_cloud_by_location$publish_date)
Media_cloud_by_location$publish_date= as.Date(substr(Media_cloud_by_location$publish_date,1,10),origin="1970-01-01")
Media_cloud_by_location$Date=Media_cloud_by_location$publish_date
head(Media_cloud_by_location)
Media_cloud_by_location$Locations=Media_cloud_by_location$Location
Media_cloud_by_location$Locations=tolower(as.character(Media_cloud_by_location$Locations))
Media_cloud_by_location=as.data.frame(table(Media_cloud_by_location[,c("Locations","Date")]))
head(Media_cloud_by_location)
names(Media_cloud_by_location)=c("Locations","Date","Count"  )
Media_cloud_by_location$Date=as.Date(as.character(Media_cloud_by_location$Date),origin="1970-01-01")
max(Media_cloud_by_location$Date)


Media_cloud_by_location$Locations=as.character(Media_cloud_by_location$Locations)
Vec=Media_cloud_by_location$Locations[Media_cloud_by_location$Locations%in%tolower(US_state_code$State)]
Vec=as.character(Vec)
Media_cloud_by_location$Locations[Media_cloud_by_location$Locations%in%tolower(US_state_code$State)]=paste0("US-",Vec)
tail(Media_cloud_by_location)
#Google trend 
# 
#  FileDir="covid-19_us_and_world/data/case/GoogleTrend/2020-05-25/"
#  Google_trend_US_states=as.data.frame(read.csv(paste0(DateDier,FileDir,"U.S.csv")))
#  library(readxl)
#  US_state_code=as.data.frame(read_excel(paste0(DateDier,"data/coronavirus_data/US_state_codes.xlsx")))
#  Google_trend_US_states$Location_fullname=US_state_code$State[match(Google_trend_US_states$Location,paste0("US-",US_state_code$`2_code`))]
#  Google_trend_US_states$Location_fullname=paste0("US-",Google_trend_US_states$Location_fullname)
#  names(Google_trend_US_states)[1]="date"
#  Google_trend_US_states=Google_trend_US_states[,c("date","COVID.19.symptoms","how.many.degree","symptoms.of.fever",
#                                                   "isPartial" , "Location", "Location_fullname")]

# FileDir="covid-19_us_and_world/data/case/GoogleTrend/2020-05-25/gtrends/"
# Final_dir=paste0(DateDier,FileDir)
# AllFiles=list.files(Final_dir)
# AllFiles=AllFiles[grepl(x = AllFiles,pattern = "US")]
# #AllFiles=paste0(Final_dir,AllFiles)
# Target=NULL
# for (File in AllFiles){
#   Vec=read.csv(paste0(Final_dir,File))
#   Vec$Location=strsplit(File,split = '[.|-]')[[1]][2]
#   #names(Vec[,3:(ncol(Vec)-1)])[order(apply(Vec[,3:(ncol(Vec)-1)],MARGIN = 2,FUN = sum),decreasing = T)]
#   Vec=Vec[,c("Location","date","cold" ,"covid", "cough" ,"pneumonia")]
#   Target=rbind(Target,Vec)
# }
# 
# library(readxl)
# US_state_code=as.data.frame(read_excel(paste0(DateDier,"data/coronavirus_data/US_state_codes.xlsx")))
# head(US_state_code)
# Target$Location=US_state_code$State[match(Target$Location,US_state_code$`2_code`)]
# head(Target,100)
# Target=Target[!is.na(Target$Location),]#overseas distribute are removed
# 
# Target$Location=paste0("US-",Target$Location)
# head(Target)
# GoogleTrend=Target

# FileDir="data/coronavirus_data/GoogleTrend/gtrends/"
# AllFiles=paste0(DateDier,FileDir,list.files(paste0(DateDier,FileDir)))
# Target=NULL
# 
# for (File in AllFiles){
#   Vec=read.csv(File)
#   Target=rbind(Target,Vec)
# }

# Google_trend_US_states=as.data.frame(read.csv(paste0(DateDier,FileDir,"U.S.csv")))



#Google_trend_all_countries=as.data.frame(read.csv(paste0(DateDier,FileDir,"AllCountries.csv")))
#All_country_code=as.data.frame(read_excel(paste0(DateDier,"COUNTRY-CODE.xlsx")))
#Google_trend_all_countries$Location_fullname=All_country_code$COUNTRY[match(Google_trend_all_countries$Location,All_country_code$`2-CODE`)]
#names(Google_trend_all_countries)[1]="date"

#All_GoogleTrend=rbind(Google_trend_US_states,Google_trend_all_countries)
#All_GoogleTrend=Google_trend_US_states

#names(All_GoogleTrend)[c(2,3,4)]=paste0("GoogleTrend:",names(All_GoogleTrend)[c(2,3,4)])





#Mechanistic predictions from NEU 

Target=read.csv(paste0(DateDier,"data/coronavirus_data/NEU_data/","daily-infections_us-states_gleam.csv"),header = T)
tail(Target)
Mechanistic_pre=Target
Mechanistic_pre$state_name=paste0("US-",Mechanistic_pre$state_name)
#Mechanistic_pre$Province[Mechanistic_pre$Province=="inner_mongolia"]="innermongolia"


#IHME prediction
Target=read.csv(paste0(DateDier,"covid-19_us_and_world/data/case/MurrayPrediction/20200428/","Hospitalization_all_locs.csv"),header = T)
Target$location_name=as.character(Target$location_name)
Target=Target[Target$location_name%in%US_state_code$State,]
Target$location_name=paste0("US-",Target$location_name)

unique(Target$location_name)
tail(Target)
IHME_prediction=Target

#MIT prediction
Target=read.csv(paste0(DateDier,"covid-19_us_and_world/data/case/MIT_prediction/20200428/","2020-04-27-MIT_CovidAnalytics-DELPHI.txt"),header = T)
Target$location_name=as.character(Target$location_name)
Target=Target[Target$location_name%in%US_state_code$State,]
Target$location_name=paste0("US-",Target$location_name)
range(as.Date(Target$target_end_date))
#Target=Target[Target$target=="1 wk ahead cum death",]
unique(Target$target)
unique(Target$location_name)
tail(Target)
MIT_prediction=Target




#excess death
Target=read.csv(paste0(DateDier,"covid-19_us_and_world/data/case/AllDeath/20200525/","Excess_Deaths_Associated_with_COVID-19.csv"),header = T)
Target$Week.Ending.Date=as.Date(as.character(Target$Week.Ending.Date))
Target=Target[Target$Year>=2020,]
Target=Target[Target$Week.Ending.Date>"2020-01-01",]
#Assuming death happen in a linear manner


range(Target$Week.Ending.Date)
Target$State=paste0("US-",Target$State)
Target$Observed.Number=as.integer(Target$Observed.Number)

Target=Target[Target$Type=="Predicted (weighted)",]
Target=Target[Target$Outcome=="All causes",]

# this is a weekly data, make every the as the previous week 
AllDate_Vec=NULL
for (Location in unique(Target$State)){
  for (Date in unique(Target$Week.Ending.Date)){
    Vec=NULL
    for (i in 1:7){
      Vec=rbind(Vec,Target[(Target$State==Location)&(Target$Week.Ending.Date==Date),])
    }
    Vec$Date=Vec$Week.Ending.Date
    Vec$Date=Vec$Date+c(1:7)
    
    AllDate_Vec=rbind(AllDate_Vec,Vec)
    
  }
  
  
}


Excess_death=AllDate_Vec
names(Excess_death)=paste0("ExcessDeath:",names(Excess_death))


# #ILI

Target=read.csv(paste0(DateDier,"covid-19_us_and_world/data/case/ILI/20200525/","ILINet.csv"),header = T,skip = 1)
head(Target)
Target=Target[Target$YEAR>=2019,]
dim(Target)
Target$DateLower=as.Date(paste0(Target$YEAR,"-01-01"))+7*(Target$WEEK-1)
Target$DateUpper=as.Date(paste0(Target$YEAR,"-01-01"))+7*(Target$WEEK)
Target=Target[Target$DateLower>="2020-01-01",]
Target$REGION=paste0("US-",Target$REGION)




# this is a weekly data, make every the as the previous week 
AllDate_Vec=NULL
for (Location in unique(Target$REGION)){
  print(Location)
  for (Date in unique(Target$DateLower)){
    Vec=NULL
    for (i in 1:7){
      Vec=rbind(Vec,Target[(Target$REGION==Location)&(Target$DateLower==Date),])
    }
    Vec$Date=Vec$DateLower
    Vec$Date=Vec$Date+c(1:7)
    
    AllDate_Vec=rbind(AllDate_Vec,Vec)
    
  }
  
  
}

ILI_data=AllDate_Vec
ILI_data$TOTAL.PATIENTS=as.numeric(as.character(ILI_data$TOTAL.PATIENTS))
ILI_data$ILITOTAL=as.numeric(as.character(ILI_data$ILITOTAL))

ILI_data$Percentage=ILI_data$ILITOTAL/ILI_data$TOTAL.PATIENTS
ILI_data$Percentage[is.na(ILI_data$Percentage)]=0



#interventions by differnet states 
Target=read.csv(paste0(DateDier,"covid-19_us_and_world/data/case/interventions/COVID-19 US state policy database (CUSP) - State policy changes - fully tracked to 5_22_2020.csv"),header = T,skip = 0)
head(Target)
Target=Target[1:51,]
Target$State=paste0("US-",Target$State)
names(Target)

unique(Target$State)


AllDate_Vec=NULL
for (Date in c(min(COVID_Tracking$Date)+1:as.integer(abs(min(COVID_Tracking$Date)-max(COVID_Tracking$Date))))){
  Date=as.Date(Date,origin = "1970-01-01")
  print(Date)
  Vec_day=Target
  Vec=Vec_day[,3:48]
  #change everything to date
  
  Vec=apply(Vec,MARGIN = c(1,2),function(x){as.character(x)})
  Vec[Vec=="1"]="1/1/2020"
  Vec[Vec=="0"]="1/1/2025"
  
  
  Vec[apply(Vec,MARGIN = c(1,2),function(x){!grepl(x,pattern = "[/]")})]="1/1/2025"
  
  Vec=apply(Vec,MARGIN = c(1,2),function(x){as.Date(x,format="%m/%d/%y",origin="1970-01-01")})
  
  Vec=apply(Vec,MARGIN = c(1,2),function(x){x<=Date})
  Vec=apply(Vec,MARGIN = c(1,2),function(x){as.integer(x)})
  Vec_day$TotalNumberOfInterventios=apply(Vec,FUN = function(x){sum(x,na.rm = T)},1)
  
  Vec_day$Date=Date
  AllDate_Vec=rbind(AllDate_Vec,Vec_day)
}

AllDate_Vec[is.na(AllDate_Vec)]=0

Interventions=AllDate_Vec
names(Interventions)=paste0("Interventions:",names(Interventions))



#each province 

DateSource=COVID_Tracking

AllX=list()
AllLocations=list()
for (Location in unique(DateSource$Location)){
  print(paste("working on location: ",Location))
  
  Vec_Location=DateSource[DateSource$Location==Location,]
  X=Vec_Location
  #Vec_Location=Vec_Location[!is.na(Vec_Location$New_confirmed),]
  #province level media and baidu  
  #X=Vec_Location[,2:ncol(Vec_Location)]
  Vec_Media_cloud_by_location=Media_cloud_by_location[tolower(Media_cloud_by_location$Locations)==tolower(Location),]
 
  X$MediaCloudCount=Vec_Media_cloud_by_location$Count[match(X$Date,Vec_Media_cloud_by_location$Date)]
  X$MediaCloudCount[is.na(X$MediaCloudCount)]=0#some data not available
  
  
  #3 search key words 
  #Vec=GoogleTrend[tolower(GoogleTrend$Location)==tolower(Location),]
  #Vec$date=as.character(Vec$date)
  #Vec=Vec[match(as.character(X$Date),Vec$date),3:(ncol(Vec)-1)]
  #colnames(Vec)=paste0("GoogleTrend:",colnames(Vec))
  #X=cbind(X,Vec)
  
  #NEU Mechanistic prediction 
  X$NEU_prediction=Mechanistic_pre$total_Infectious_median[match(interaction(X$Location,as.Date(X$Date)),
                                                interaction(Mechanistic_pre$state_name,as.Date(Mechanistic_pre$date)))]
  X$NEU_prediction[is.na(X$Mechanistic_pred)]=0#missing data fill with zeros 
  
  
  #IHME prediction
  head(IHME_prediction)
  Vec=IHME_prediction[match(interaction(X$Location,as.Date(X$Date)),
                                                    interaction(IHME_prediction$location_name,as.Date(IHME_prediction$date))),
                      c("allbed_mean","ICUbed_mean","InvVen_mean","admis_mean","newICU_mean","totdea_mean")]
  names(Vec)=paste0("IHME_",names(Vec))
  
  X=cbind(X,Vec)
  
  # #MIT prediction
  # X$MIT_cumulative_death_prediction=MIT_prediction$value[match(interaction(X$Location,as.Date(X$Date)),
  #                                                                interaction(MIT_prediction$location_name,as.Date(as.character(MIT_prediction$target_end_date))))]
  # X$NEU_prediction[is.na(X$Mechanistic_pred)]=0#missing data fill with zeros 
  # 
  # head(MIT_prediction)
  # Vec=IHME_prediction[match(interaction(X$Location,as.Date(X$Date)),
  #                           interaction(IHME_prediction$location_name,as.Date(IHME_prediction$date))),
  #                     c("allbed_mean","ICUbed_mean","InvVen_mean","admis_mean","newICU_mean")]
  # names(Vec)=paste0("IHME_",names(Vec))
  # 
  
  #ILI
  X$Date=as.character(X$Date)
  X$Date=as.Date(X$Date)
  X$ILI=ILI_data$Percentage[match(interaction(X$Location,as.Date(X$Date)),
                                  interaction(ILI_data$REGION,as.Date(ILI_data$Date)))]

  
  
  #JHK data 
  X$JHK_Cumulative_confirmed=JHK$Cumulative_confirmed[match(interaction(X$Location,as.Date(X$Date)),
                                                 interaction(JHK$Location,as.Date(JHK$Date)))]
  
  X$JHK_New_confirmed=JHK$New_confirmed[match(interaction(X$Location,as.Date(X$Date)),
                                                            interaction(JHK$Location,as.Date(JHK$Date)))]
  
  #Excess death
  
  X$`ExcessDeath:Excess.Lower.Estimate`=Excess_death$`ExcessDeath:Excess.Lower.Estimate`[match(interaction(X$Location,as.Date(X$Date)),
                                                          interaction(Excess_death$`ExcessDeath:State`,as.Date(Excess_death$`ExcessDeath:Date`)))]
  
  X$`ExcessDeath:Excess.Higher.Estimate`=Excess_death$`ExcessDeath:Excess.Higher.Estimate`[match(interaction(X$Location,as.Date(X$Date)),
                                                                                             interaction(Excess_death$`ExcessDeath:State`,Excess_death$`ExcessDeath:Date`))]
  
  
  
  #Intervention
  
  X$`Interventions:TotalNumberOfInterventios`=Interventions$`Interventions:TotalNumberOfInterventios`[match(interaction(X$Location,as.Date(X$Date)),
                                                                                                            interaction(Interventions$`Interventions:State`,Interventions$`Interventions:Date`))]
  
  
  
  
  AllX[[length(AllX)+1]]=X
  AllLocations[[length(AllLocations)+1]]=rep(Location,nrow(X))
}

AllPlaces=Reduce(AllX,f = rbind)
AllPlaces$Location=Reduce(AllLocations,f = c)
names(AllPlaces)
tail(AllPlaces)

OrganizedData=AllPlaces
OrganizedData[is.na(OrganizedData)]=0#all NA put as 0


#Sort by state and date 
OrganizedData=OrganizedData[with(OrganizedData, order(Location, Date)), ]
head(OrganizedData)

OrganizedData$Date=as.character(OrganizedData$Date)

#remove states with < 1000 positive on the last date 
Vec=OrganizedData[OrganizedData$Date==max(OrganizedData$Date),]
#OrganizedData=OrganizedData[OrganizedData$Location%in%Vec$Location[Vec$positive>1000],]

write.csv(OrganizedData,file = '../results/OrganizedData_US_countries.csv')


# 
# #Save for Symbolic AI 
# VecSymbol=OrganizedData[,c("JHK_New_confirmed" ,"deathIncrease","NewHospitalized","GoogleTrend:cold","Date" )]
# VecSymbol$`GoogleTrend:cold`=VecSymbol$`GoogleTrend:cold`/max(VecSymbol$`GoogleTrend:cold`)
# VecSymbol=VecSymbol[VecSymbol$Date<="2020-04-20",]
# VecSymbol$Date=as.numeric(as.Date(VecSymbol$Date)-as.Date("2020-01-01"))
# rownames(VecSymbol)=NULL
# colnames(VecSymbol)=NULL
# for (i in 1:ncol(VecSymbol)){
#   VecSymbol[,i]=as.numeric(VecSymbol[,i])
#   VecSymbol[,i][is.na( VecSymbol[,i])]=0
#   VecSymbol[,i][is.infinite( VecSymbol[,i])]=0
#   VecSymbol[,i]=VecSymbol[,i]+rnorm(mean = 0,sd = 0.01,n =nrow(VecSymbol) )
#   
# }
# 
# #VecSymbol=VecSymbol+rnorm(mean = 0,sd = 0.01,n =2413 )
# write.table(VecSymbol,file = "DataForSymbolic.txt",sep = " ",row.names = F,col.names = F)
# 
