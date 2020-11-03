print("use sequantial model to predict longer term in an reccurent manner")


import pandas as pd
import numpy as np
from utilities import *


#Set the experiment to do

GroundTruthName='JHK_New_confirmed'
FirstDate_use_data="2020-03-01"
First_index_date="2020-07-01"
Information_to_include=['JHK_New_confirmed','JHK_Cumulative_confirmed']
#'GoogleTrend:covid','GoogleTrend:cold','JHK_Cumulative_confirmed'
#'NEU_prediction','totalTestResultsIncrease','IHME_admis_mean'
MechanisticModelCompare='NEU_prediction'



#Load data


X_train,Y_train,Y_test ,MechanisticPrediction_test,Locations=DataLoader(GroundTruthName=GroundTruthName,
                                                                        Information_to_include=Information_to_include,
                                                                       FirstDate_use_data=FirstDate_use_data,
                                                                       First_index_date=First_index_date,
                                                                       MechanisticModelCompare=MechanisticModelCompare)




#Padding

import tensorflow as tf
from tensorflow.keras.preprocessing.sequence import pad_sequences
padded_X_train = pad_sequences(X_train,padding='pre',dtype="float")

#padded_X_train=padded_X_train.reshape((padded_X_train.shape[0],padded_X_train.shape[1],1))#reshape to RNN input

padded_Y_train=pad_sequences(Y_train,padding='pre',dtype="float")
padded_Y_train=padded_Y_train.reshape((padded_Y_train.shape[0],padded_Y_train.shape[1],1))

#padded_X=padded_X+np.random.normal(0,0.01,padded_X.shape)#gaussian noise
padded_Y_test = pad_sequences(Y_test,padding='post',dtype="float")

MechanisticPrediction_test=pad_sequences(MechanisticPrediction_test,padding='post',dtype="float")

#padded_X_test =padded_X_test.reshape((padded_X_test.shape[0],padded_X_test.shape[1],1))#reshape to RNN input




#Pad the data to make th

import datetime as dt
FirstDate_use_data=dt.datetime.strptime(FirstDate_use_data, '%Y-%m-%d')
First_index_date=dt.datetime.strptime(First_index_date, '%Y-%m-%d')

AlldatesAvailable=[FirstDate_use_data+dt.timedelta(x) for x in range(padded_X_train.shape[1]+padded_Y_test.shape[1])]


#Data Aggregation
padded_X_train, padded_Y_train,padded_Y_test ,MechanisticPrediction_test,AlldatesAvailable=AggregateData(padded_X_train,padded_Y_train,padded_Y_test,MechanisticPrediction_test,AlldatesAvailable)



#Data normalization
NormalziedData=Normalization(padded_X_train ,padded_Y_train)
padded_X_train ,padded_Y_train=NormalziedData.Nomalize()

#padded_X_train[10,:,4]
#visualze the inputs


import matplotlib
import matplotlib.pyplot as plt


matplotlib.rc('xtick', labelsize=5)
matplotlib.rc('ytick', labelsize=5)
fig, axs = plt.subplots(7, 7,figsize=(16,13))
for i in range(len(axs)):
    for j in range(len(axs[i])):

        #input variables
        for Feature in range(padded_X_train.shape[2]):
            axs[i][j].plot(padded_X_train[i * len(axs) + j, :,Feature].tolist())


            axs[i][j].set_ylim(0,1)
            #plt.xticks(rotation=90)
            #axs[i][j].vlines(First_index_date + dt.timedelta(0), ymin=0, ymax=np.max(Observed[i * len(axs) + j, :]), linestyles='dashed')
            #axs[i, j].xlabel("Date")
            #axs[i, j].ylabel("normalized " + GroundTruthName)
            axs[i][j].set_title(Locations[i*len(axs)+j])
fig.tight_layout()

plt.savefig("../figures/Reccurent_"+GroundTruthName+"_input.png")
plt.close()



AllPredictions=[]#predictions of different number of days ahead of time
#IntervalRange=range(0,1)
Input_lag=1
Interval_ahead=7
#organize data for ML model input

Input_lag=Input_lag
Encoder_Input,Decoder_Output,AllLocations=SubSampleData(padded_X_train ,padded_Y_train,Locations,
                                                        Input_lag=Input_lag,Interval_ahead=Interval_ahead)

#Flatten for none RNN input
Encoder_Input=Encoder_Input.reshape(Encoder_Input.shape[0],Encoder_Input.shape[1]*Encoder_Input.shape[2])

#Break in to foamr of HMM imput
X=[]
lengths=[]
for VecLocations in list(set(Locations)):
    X.append(Encoder_Input[np.asarray(AllLocations)==VecLocations,:])
    lengths.append(np.sum(np.asarray(AllLocations)==VecLocations))

X=np.concatenate(X)

#HMM model
import numpy as np
from hmmlearn import hmm

Model_trained=hmm.GaussianHMM(n_components=10).fit(X, lengths)
Model_trained.monitor_.converged

#Make a model X to Y mapping
from Models import LinearRegression

X_Y_model=LinearRegression(Encoder_Input.shape[1],Encoder_Input,Decoder_Output,N_Epochs=10)



#Make prediction(sampling) each state
Predictions_each_location = []
for VecLocations in list(set(Locations)):
    Model_to_use = Model_trained
    LastZ=Model_to_use.predict(Encoder_Input[np.asarray(AllLocations)==VecLocations,:])[-1]

    Model_to_use.transmat_
    Model_to_use.startprob_=np.zeros(10)
    Model_to_use.startprob_[LastZ]=1#start from the end hidden state of the province
    X_sampled, Z_sampled = Model_to_use.sample(Interval_ahead)
    Predictions_each_location.append(X_Y_model.predict(X_sampled).flatten())

Predictions_each_location=np.stack( Predictions_each_location,0)
AllPredictions.append(Predictions_each_location)








#Predictions
SequantialPrediction=AllPredictions[0]
SequantialPrediction=SequantialPrediction.reshape((SequantialPrediction.shape[0],SequantialPrediction.shape[1]))
SequantialPrediction=SequantialPrediction*NormalziedData.Y_sd[:,0]+NormalziedData.Y_mean.reshape(NormalziedData.Y_mean.shape[0],1)#scale it back




#mechanistic prediction

#MechanisticPrediction_test=MechanisticPrediction_test[:,0]

#Get baseline using one week average before prediction

PersistenceBaseline=padded_Y_train[:,(padded_X_train.shape[1]-7):,0]
PersistenceBaseline=np.mean(PersistenceBaseline,1)
PersistenceBaseline=PersistenceBaseline.reshape((PersistenceBaseline.shape[0],1))
PersistenceBaseline=np.repeat(PersistenceBaseline,SequantialPrediction.shape[1],1)
PersistenceBaseline=PersistenceBaseline*NormalziedData.Y_sd[:,0].reshape(NormalziedData.Y_sd.shape[0],1)+NormalziedData.Y_mean.reshape(NormalziedData.Y_mean.shape[0],1)

#Simply visualize other data input before index date
#padded_X_train_rescaled=np.exp(padded_X_train*NormalziedData.X_max.reshape(NormalziedData.X_max.shape[0],1,NormalziedData.X_max.shape[1]))


#observed ground truth
Observed=np.hstack([padded_Y_train[:,:,0]*NormalziedData.Y_sd[:,0].reshape(NormalziedData.Y_sd.shape[0],1)+NormalziedData.Y_mean.reshape(NormalziedData.Y_mean.shape[0],1),padded_Y_test])


import matplotlib
import matplotlib.pyplot as plt








#Performance of different method for each province
#To dates


TrainingDates=[AlldatesAvailable[i] for i in list(range(padded_X_train.shape[1]))]
PredictionDates=[AlldatesAvailable[i] for i in list(range(padded_X_train.shape[1],padded_X_train.shape[1]+padded_Y_test.shape[1]))[0:Interval_ahead]]
Alldates=AlldatesAvailable#plot grouth trueth for all Dates

i=1
j=2
plt.plot(Alldates, Observed[i  + j, :].tolist())
#plt.plot(PredictionDates,MechanisticPrediction_test[i  + j, :].tolist())

plt.plot(PredictionDates, SequantialPrediction[i+j, :].tolist())
plt.plot(PredictionDates, PersistenceBaseline[i + j, :].tolist())




matplotlib.rc('xtick', labelsize=5)
matplotlib.rc('ytick', labelsize=5)
fig, axs = plt.subplots(7, 7,figsize=(16,13))
for i in range(len(axs)):
    for j in range(len(axs[i])):

        #ground truth
        axs[i][j].plot(Alldates, Observed[i * len(axs) + j, :].tolist())

        #three different types of predictions
        axs[i][j].plot(PredictionDates, SequantialPrediction[i*len(axs)+j, :].tolist())
        axs[i][j].plot(PredictionDates, PersistenceBaseline[i * len(axs) + j, :].tolist())
        #axs[i][j].plot(PredictionDates,MechanisticPrediction_test[i * len(axs) + j, :].tolist())

        #other input before index date
        #axs[i][j].plot([Alldates[i] for i in range(len(Alldates) - SequantialPrediction.shape[1])],
         #    padded_X_train_rescaled[i * len(axs) + j, :, 1].tolist())

        axs[i][j].set_ylim(0,np.max(Observed[i * len(axs) + j, :]))
        #plt.xticks(rotation=90)
        axs[i][j].vlines(First_index_date + dt.timedelta(0), ymin=0, ymax=np.max(Observed[i * len(axs) + j, :]), linestyles='dashed')
        #axs[i, j].xlabel("Date")
        #axs[i, j].ylabel("normalized " + GroundTruthName)
        axs[i][j].set_title(Locations[i*len(axs)+j])
fig.tight_layout()

plt.savefig("../figures/HMM_"+GroundTruthName+".png")
#plt.close()



#Get performance and visualize it



def GetPerformance(Y_test,Predicted):
    RMSE = np.mean((Predicted - Y_test) ** 2, 1) ** 0.5
    from scipy.stats import pearsonr
    PearsonCorrelation=list()
    for i in range(Predicted.shape[0]):

     PearsonCorrelation.append(pearsonr(Predicted[0,:],Y_test[i,:])[0])
    return(RMSE,np.asarray(PearsonCorrelation))



Y_test=padded_Y_test[:,IntervalRange]
Predicted=PersistenceBaseline
RMSE_persistence,Cor_persistence=GetPerformance(Y_test,Predicted)


Predicted=SequantialPrediction
RMSE_seq_model,Cor__seq_model=GetPerformance(Y_test,Predicted)


Predicted=MechanisticPrediction_test
RMSE_Mechanistic,Cor__Mechanistic=GetPerformance(Y_test,Predicted)



x = np.asarray(list(range(len(Locations))))

print(str(sum(RMSE_persistence/RMSE_seq_model>1)))
print(str(sum(RMSE_Mechanistic/RMSE_seq_model>1)))


ax = plt.subplot(111)
ax.bar(x-0.2, RMSE_seq_model/RMSE_seq_model, width=0.2, color='b', align='center')
ax.bar(x, RMSE_persistence/RMSE_seq_model, width=0.2, color='g', align='center')
#ax.bar(x+0.2, RMSE_Mechanistic/RMSE_seq_model, width=0.2, color='r', align='center')
ax.set_ylim(0,5)
plt.xlabel('States', fontweight='bold')
plt.ylabel('RMSE/RMSE by RNN model')
plt.show()

plt.savefig("../figures/HMM_Performance_"+GroundTruthName+".png")
plt.close()



x = np.asarray(list(range(len(Locations))))
ax = plt.subplot(111)
ax.bar(x-0.2, Cor__seq_model/Cor__seq_model, width=0.2, color='b', align='center')
ax.bar(x, Cor__Mechanistic/Cor__seq_model, width=0.2, color='g', align='center')
#ax.bar(x+0.2, RMSE_Mechanistic/RMSE_seq_model, width=0.2, color='r', align='center')
ax.set_ylim(0,5)
plt.xlabel('States', fontweight='bold')
plt.ylabel('Cor/Cor by RNN model')
#plt.legend(loc="upper left")
plt.show()

plt.savefig("figures/HMM_correlation_"+GroundTruthName+".png")
plt.close()
