import pandas as pd
from pytrends.request import TrendReq
import pytrends
#from pytrends.request import get_historical_interest
pytrend = TrendReq(timeout=(10,25),retries=2,backoff_factor=0.1)


#load state and country Code
States_code=pd.read_excel("../../data/coronavirus_data/US_state_codes.xlsx")
States_code=["US-"+x for x in States_code["2_code"].tolist()]

Country_codes=pd.read_excel("../../data/coronavirus_data/COUNTRY-CODE.xlsx")

Country_codes=Country_codes["2-CODE"].tolist()

AllLocations=States_code


#Loop through locations, US
All_DF=pd.DataFrame()

WORD_LIST=["Coronavirus Symptoms",
                                   "body aches +/for icd 10" , "muscle aches","essential oils for /+ body aches",
                                   "fatigue and tiredness","vitamins","excessive tiredness/fatigue",
                                   "Fever Symptoms","icd 10 code for/+ fever","baby/toddler fever",
                                   "sore throat +/for icd 10","essential oil +/for sore throat",
                                   "icd 10 code for/+ cough","essential oils  for/+ cough","toddler/baby cough",
                                   "cough medicine","pineapple juice for/+ cough","dry cough",
                                   "shortness of breath","icd 10 +/for shortness of breath","shortness of breath + fatigue",
                                   "chest pain",
                                   "fever +/and sore throat","cough +/and sore throat","muscle ache +/and fever",
                                   "cough +/and fever","essential oil + various symptoms(cough, fever,sore throat)",
                                   "icd 10 + various symptoms(cough, fever,sore throat)","shortness of breath +/and cough"]
ALL_words=[]
for WORD in WORD_LIST:
    print(WORD)
    for Location in States_code:
        print(str(Location))
        pytrend.build_payload(kw_list=[WORD],geo=Location)
        # Interest by Region
        #df = pytrend.interest_by_region()
        #df.head(10)

        Df_time=pytrend.interest_over_time()
        Df_time["Location"]=Location
        All_DF=pd.concat([All_DF,Df_time])
        ALL_words.append(All_DF)



import datetime
from datetime import date
FileDir="/Users/dianboliu/Google_Drive/research/Coronavirus/covid-19_us_and_world/data/case/GoogleTrend/"+str(date.today())+"/"
import os
if not os.path.exists(FileDir):
    os.makedirs(FileDir)

All_DF.to_csv(FileDir+"U.S.csv")


#
# #Loop through locations, all countries
# All_DF=pd.DataFrame()
#
#
# for Location in Country_codes:
#     print(str(Location))
#     pytrend.build_payload(kw_list=["COVID-19 symptoms", "how many degree" , "symptoms of fever"],geo=Location)
#     # Interest by Region
#     #df = pytrend.interest_by_region()
#     #df.head(10)
#
#     Df_time=pytrend.interest_over_time()
#     Df_time["Location"]=Location
#     All_DF=pd.concat([All_DF,Df_time])
#
#
#
# import datetime
# from datetime import date
# FileDir="../data/coronavirus_data/GoogleTrend/"+str(date.today())+"/"
# import os
# if not os.path.exists(FileDir):
#     os.makedirs(FileDir)
#
# All_DF.to_csv(FileDir+"AllCountries.csv")