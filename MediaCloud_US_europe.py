import mediacloud.api, json, datetime
import pandas as pd
import datetime
from datetime import date

mc = mediacloud.api.MediaCloud('3e052cf149bd986e99eef77ed657638cecd01389d8de90efc797d1c61a769dcf')

QueryList=["COVID-19","coronavirus","2019-nCoV","pneumonia","fever","cough"]


#ChineseProvinces=["Shanghai" ,"Yunnan" , "Russia" , "Inner Mongolia" , "Canada" , "Beijing"  , "India"  , "Taiwan"  , "Jilin"   ,  "Sichuan"   ,  "Tianjin"  ,   "Ningxia"  ,
 #              "Anhui" ,  "Shandong"  ,   "Shanxi" ,    "Guangdong"  ,   "Guangxi" ,    "Germany" ,  "Xinjiag"  ,
  #               "Jiangsu"   ,  "Jiangxi"    , "Hebei"  ,   "Henan" ,   "Zhejiang","Hainan"    , "Hubei" ,    "Hunan" ,
   #            "Macau"  ,   "Sweden",     "Gansu" ,    "Fujiang",     "USA" ,    "Finland"  ,   , "Tibet"  ,   "Guizhou" ,
    #          "Liaoning" ,    "Chongqing"  ,    "Shaanxi"   ,  "Qinghai"  ,   "Hong Kong"  ,   "Malaysia", "Heilongjiang"]


US_states=["Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","Florida","Georgia","Hawaii",
"Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi",
"Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York","North Carolina","North Dakota","Ohio",
"Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington",
"West Virginia","Wisconsin","Wyoming"]


Other_countries=["Italy","Iran","South Korea","France","Spain","Germany","US","Norway","Cruise Ship","Switzerland","Japan","Denmark","Sweden",
                 "Netherlands","United Kingdom","Belgium","Austria","Qatar","Bahrain","Singapore","Malaysia","Israel","Australia",
                "Canada","Iceland","Greece","Czechia","Slovenia","United Arab Emirates","Kuwait","India","Iraq","Thailand","San Marino","Egypt",
                "Lebanon","Finland","Portugal","Brazil","Philippines","Romania","Poland",
                "Saudi Arabia","Ireland","Vietnam","Indonesia","Russia","Algeria","Georgia","Albania","Chile","Costa Rica","Pakistan",
                 "Argentina","Luxembourg","Croatia","Serbia","Oman","South Africa","Ecuador","Slovakia","Estonia",
                 "Peru","Hungary","Belarus","Mexico","Bosnia and Herzegovina","Azerbaijan","Panama","Brunei","Latvia"]

Locations=US_states+Other_countries

MediaCollectionID=["186572516"]#US top source 2018
#34412193
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
Dir='/Users/dianboliu/Google_Drive/research/Coronavirus/covid-19_us_and_world/data/media_cloud_US_othercountries/'+str(date.today())+"/"
if not os.path.exists(Dir):
    os.makedirs(Dir)

MediaCount.to_csv(Dir+"MediaCloud.csv")