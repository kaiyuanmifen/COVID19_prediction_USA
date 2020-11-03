import mediacloud.api, json, datetime
import pandas as pd
import datetime
from datetime import date

mc = mediacloud.api.MediaCloud('86032bce0acef3fa4b016b753394ced697bbc9f1ed7d7629d0bc31882f99dd4f')

QueryList=["COVID-19","coronavirus","2019-nCoV","pneumonia","fever","cough"]

Locations=["Shanghai" ,"Yunnan" , "Russia" , "Inner Mongolia" , "Canada" , "Beijing"  , "India"  , "Taiwan"  , "Jilin"   ,  "Sichuan"   ,  "Tianjin"  ,   "Ningxia"  ,
               "Anhui" ,    "Nepal" ,  "Shandong"  ,   "Shanxi" ,    "Guangdong"  ,   "Guangxi" ,    "Germany" ,    "Italy" ,  "Siri LanKa" ,"Singapore" ,  "Xinjiag"   ,  "Japan"  ,
               "Cambodia"  , "Belgium" ,  "Jiangsu"   ,  "Jiangxi"    , "Hebei"  ,   "Henan" ,    "France"  ,   "Thailand"  ,   "Zhejiang","Hainan"    , "Hubei" ,    "Hunan" ,
               "Australia" ,"Macau"  ,   "Sweden",     "Gansu" ,    "Fujiang",     "USA" ,    "Finland"  ,   "UK" ,    "he philippines"  , "Spain"  , "Tibet"  ,   "Guizhou" ,
               "Vietnam" ,    "Liaoning" ,    "Chongqing"  ,   "the united arab emirates"  , "Shaanxi"   ,  "Qinghai"   ,  "South Korea"  ,   "Hong Kong"  ,   "Malaysia", "Heilongjiang"]

MediaCollectionID=["34412193"]
34412193
#416841
#Make query list

AllLocations=[]
stories=[]
numdays=30

base1=datetime.date(2020,1,1)
base2=date.today()
#Date_list = [base - datetime.timedelta(days=x) for x in range(numdays)]

for location in Locations:

    Query = "(" + " or ".join(QueryList) + ") AND " + location + " AND tags_id_media:" + MediaCollectionID[0]
   # Query = "(" + " or ".join(QueryList) + ")" " AND tags_id_media:" + MediaCollectionID[0]

    print(Query)

    res = mc.storyList(Query,
                        solr_filter=mc.publish_date_query(base1, base2))

    stories.extend(res)


    AllLocations.extend([location for i in range(len(res))])

PD=pd.DataFrame(stories)
PD["Location"]=AllLocations





MediaCount=PD

import datetime
from datetime import date
datetime.datetime.now()
print(date.today())
import os
Dir='/Users/dianboliu/Google_Drive/research/Coronavirus/data/coronavirus_data/media_cloud/'+str(date.today())+"/"
if not os.path.exists(Dir):
    os.makedirs(Dir)

MediaCount.to_csv(Dir+"MediaCloud.csv")