
Date="20200525"

#JHK
mkdir /Users/dianboliu/Google_Drive/research/Coronavirus/covid-19_us_and_world/data/case/JohnHopKinsCSSEData/${Date}

git clone https://github.com/CSSEGISandData/2019-nCoV /Users/dianboliu/Google_Drive/research/Coronavirus/covid-19_us_and_world/data/case/JohnHopKinsCSSEData/${Date}/


#DXY data 
mkdir /Users/dianboliu/Google_Drive/research/Coronavirus/covid-19_us_and_world/data/case/blanker_L_dxy/${Date}

git clone https://github.com/BlankerL/DXY-2019-nCoV-Data.git /Users/dianboliu/Google_Drive/research/Coronavirus/covid-19_us_and_world/data/case/blanker_L_dxy/${Date}

#MIDSA
mkdir /Users/dianboliu/Google_Drive/research/Coronavirus/covid-19_us_and_world/data/case/MIDAS/${Date}

git clone https://github.com/midas-network/COVID-19.git /Users/dianboliu/Google_Drive/research/Coronavirus/covid-19_us_and_world/data/case/MIDAS/${Date}/

#COVID-track 

mkdir /Users/dianboliu/Google_Drive/research/Coronavirus/covid-19_us_and_world/data/case/COVID-track/${Date}

wget -O /Users/dianboliu/Google_Drive/research/Coronavirus/covid-19_us_and_world/data/case/COVID-track/${Date}/daily.csv https://covidtracking.com/api/v1/states/daily.csv 


#ILI

#Google mobility 

mkdir /Users/dianboliu/Google_Drive/research/Coronavirus/covid-19_us_and_world/data/case/GoogleMobility/${Date}

wget -O /Users/dianboliu/Google_Drive/research/Coronavirus/covid-19_us_and_world/data/case/GoogleMobility/${Date}/Global_Mobility_Report.csv https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv 



#Apple mobility 


#murray data prediction 
mkdir /Users/dianboliu/Google_Drive/research/Coronavirus/covid-19_us_and_world/data/case/MurrayPrediction/${Date}

wget -O /Users/dianboliu/Google_Drive/research/Coronavirus/covid-19_us_and_world/data/case/MurrayPrediction/${Date}/ihme-covid19.zip https://ihmecovid19storage.blob.core.windows.net/latest/ihme-covid19.zip 
unzip /Users/dianboliu/Google_Drive/research/Coronavirus/covid-19_us_and_world/data/case/MurrayPrediction/${Date}/ihme-covid19.zip /Users/dianboliu/Google_Drive/research/Coronavirus/covid-19_us_and_world/data/case/MurrayPrediction/${Date}/


#MIT 

#mkdir /Users/dianboliu/Google_Drive/research/Coronavirus/covid-19_us_and_world/data/case/JohnHopKinsCSSEData/${Date}

#git clone https://github.com/CSSEGISandData/COVID-19 /Users/dianboliu/Google_Drive/research/Coronavirus/covid-19_us_and_world/data/case/JohnHopKinsCSSEData/${Date}/