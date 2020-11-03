print("functions and classes loaded")


import pandas as pd
import numpy as np





#Load data


def DataLoader(GroundTruthName='covidtracking_new_deaths',
                Information_to_include=['covidtracking_deaths_cumulative','covidtracking_new_deaths','cdc_ili'],
               FirstDate_use_data="2020-04-01",
               First_index_date="2020-04-20",
               MechanisticModelCompare='covidtracking_new_deaths'):

    Data=pd.read_csv("../results/OrganizedData_US_countries.csv")
    Data.columns
    import datetime as dt

    #Data=Data.loc[(pd.to_datetime(Data["Date"]) < dt.datetime.strptime("2020-04-30", '%Y-%m-%d')),]#some data are missing after April 30


    #for training

    Data_train = Data.loc[(pd.to_datetime(Data["Date"]) >= dt.datetime.strptime(FirstDate_use_data, '%Y-%m-%d'))*(pd.to_datetime(Data["Date"]) < dt.datetime.strptime(First_index_date, '%Y-%m-%d')),].copy()

    Targets = []
    for Location in Data['Location'].unique().tolist():
        Vec = np.asarray(Data_train[Information_to_include].loc[Data_train['Location'] == Location])
        Targets.append(Vec.tolist())
    X_train = Targets

    Targets = []
    for Location in Data['Location'].unique().tolist():
        Vec = np.asarray(Data_train[GroundTruthName].loc[Data_train['Location'] == Location].tolist())
        Vec = Vec
        Targets.append(Vec.tolist())
    Y_train=Targets




    #for testing
    Data_test = Data.loc[pd.to_datetime(Data["Date"]) >= dt.datetime.strptime(First_index_date, '%Y-%m-%d'),].copy()

    Targets = []
    for i in range(Data['Location'].unique().shape[0]):
        Location=Data['Location'].unique()[i]
        Vec = np.asarray(Data_test[GroundTruthName].loc[Data_test['Location'] == Location].tolist())
        Targets.append(Vec.tolist())
    Y_test = Targets



    # mechanistic prediction in test period
    Data_test = Data.loc[pd.to_datetime(Data["Date"]) >= dt.datetime.strptime(First_index_date, '%Y-%m-%d'),].copy()
    Targets = []
    for i in range(Data['Location'].unique().shape[0]):
        Location = Data['Location'].unique()[i]
        Vec = np.asarray(Data_test[MechanisticModelCompare].loc[Data_test['Location'] == Location].tolist())

        Targets.append(Vec.tolist())
    MechanisticPrediction_test = Targets


    Locations=Data['Location'].unique().tolist()

    return(X_train,Y_train,Y_test ,MechanisticPrediction_test,Locations)




def AggregateData(padded_X_train,padded_Y_train,padded_Y_test,MechanisticPrediction_test,AlldatesAvailable):
    Vec_padded_X_train = padded_X_train
    Vec_padded_Y_train = padded_Y_train
    Vec_padded_Y_test=padded_Y_test
    Vec_MechanisticPrediction_test=MechanisticPrediction_test

    DatesTrain=[]
    for i in range(padded_X_train.shape[1])[1:padded_X_train.shape[1]:2]:
        Vec_padded_X_train[:,i,:]=Vec_padded_X_train[:,i,:]+Vec_padded_X_train[:,i-1,:]
        Vec_padded_Y_train[:,i,:]=Vec_padded_Y_train[:,i,:]+Vec_padded_Y_train[:,i-1,:]
        DatesTrain.append(AlldatesAvailable[i])

    Vec_padded_X_train = Vec_padded_X_train[:, range(padded_X_train.shape[1])[1:padded_X_train.shape[1]:2], :]
    Vec_padded_Y_train = Vec_padded_Y_train[:, range(padded_Y_train.shape[1])[1:padded_Y_train.shape[1]:2], :]

    DatesPrediction=[]
    for i in range(Vec_padded_Y_test.shape[1])[1:Vec_padded_Y_test.shape[1]:2]:
        Vec_padded_Y_test[:, i] = Vec_padded_Y_test[:, i] + Vec_padded_Y_test[:, i - 1]
        Vec_MechanisticPrediction_test[:, i] = Vec_MechanisticPrediction_test[:, i] + Vec_MechanisticPrediction_test[:, i - 1]
        DatesPrediction.append(AlldatesAvailable[padded_X_train.shape[1]+i])

    Vec_padded_Y_test =Vec_padded_Y_test[:, range(Vec_padded_Y_test.shape[1])[1:Vec_padded_Y_test.shape[1]:2]]
    Vec_MechanisticPrediction_test = Vec_MechanisticPrediction_test[:, range(Vec_MechanisticPrediction_test.shape[1])[1:Vec_MechanisticPrediction_test.shape[1]:2]]

    Alldates=DatesTrain+DatesPrediction

    return(Vec_padded_X_train, Vec_padded_Y_train , Vec_padded_Y_test,Vec_MechanisticPrediction_test,Alldates)





#
# class Normalization():
#     def __init__(self,padded_X_train ,padded_Y_train):
#         #As the increase almost follows an expotential distribution at the begining, use log transformation
#         padded_X_train[padded_X_train<=0]=1
#         padded_X_train[np.isnan(padded_X_train)]=1
#
#         padded_Y_train[padded_Y_train<=0]=1
#         padded_Y_train[np.isnan(padded_Y_train)] = 1
#
#         self.padded_X_train=np.log(padded_X_train)
#         self.padded_Y_train =np.log(padded_Y_train)
#
#         self.X_max = np.max(self.padded_X_train, 1)
#         self.X_max=self.X_max.reshape((self.X_max.shape[0],1,self.X_max.shape[1]))
#
#         self.X_min = np.min(self.padded_X_train, 1)
#         self.X_min =self.X_min.reshape((self.X_min.shape[0], 1, self.X_min.shape[1]))
#
#
#         self.Y_max = np.max(self.padded_Y_train,1)
#         self.Y_max=self.Y_max.reshape((self.Y_max.shape[0], 1, 1))
#         self.Y_min = np.min(self.padded_Y_train,1)
#         self.Y_min=self.Y_min.reshape((self.Y_min.shape[0], 1, 1))
#
#     def Nomalize(self):
#
#
#         padded_Y_train_normalzied= (self.padded_Y_train-self.Y_min)/(self.Y_max-self.Y_min)
#
#         padded_X_train_normalzied= (self.padded_X_train-self.X_min)/(self.X_max-self.X_min)
#         padded_X_train_normalzied[np.isnan(padded_X_train_normalzied)]=0#some data are aeros in certain features certain provinces
#
#         return(padded_X_train_normalzied,padded_Y_train_normalzied)
#
#     def Rescale(self,padded_X_train_normalzied,padded_Y_train_normalzied):
#         padded_X_train_rescaled=padded_X_train_normalzied*self.X_max.reshape((self.X_max.shape[0],1,self.X_max.shape[1]))
#         padded_X_train_rescaled=np.exp(padded_X_train_rescaled)
#         padded_Y_train_rescaled=padded_Y_train_normalzied * self.Y_max.reshape((self.Y_max.shape[0],1,1))
#         padded_Y_train_rescaled=np.exp(padded_Y_train_rescaled)
#         return(padded_X_train_rescaled,padded_Y_train_rescaled)



class Normalization():
    def __init__(self,padded_X_train ,padded_Y_train):
        #Z score
        padded_X_train[padded_X_train<=0]=1
        padded_X_train[np.isnan(padded_X_train)]=1

        padded_Y_train[padded_Y_train<=0]=1
        padded_Y_train[np.isnan(padded_Y_train)] = 1

        self.padded_X_train = padded_X_train
        self.padded_Y_train = padded_Y_train

        self.X_mean=np.mean(padded_X_train,1)
        self.X_mean=self.X_mean.reshape(self.X_mean.shape[0],1,self.X_mean.shape[1])
        self.X_sd=np.std(padded_X_train,1)
        self.X_sd = self.X_sd.reshape(self.X_sd.shape[0], 1, self.X_sd.shape[1])

        self.Y_mean=np.mean(padded_Y_train,1)
        self.Y_mean = self.Y_mean.reshape(self.Y_mean.shape[0], 1, self.Y_mean.shape[1])
        self.Y_sd=np.std(padded_Y_train,1)
        self.Y_sd = self.Y_sd.reshape(self.Y_sd.shape[0], 1, self.Y_sd.shape[1])


    def Nomalize(self):


        padded_Y_train_normalzied= (self.padded_Y_train-self.Y_mean)/(self.Y_sd)

        padded_X_train_normalzied= (self.padded_X_train-self.X_mean)/(self.X_sd)
        padded_X_train_normalzied[np.isnan(padded_X_train_normalzied)]=0#some data are aeros in certain features certain provinces

        return(padded_X_train_normalzied,padded_Y_train_normalzied)

    def Rescale(self,padded_X_train_normalzied,padded_Y_train_normalzied):
        padded_X_train_rescaled=padded_X_train_normalzied*self.X_max.reshape((self.X_max.shape[0],1,self.X_max.shape[1]))
        padded_X_train_rescaled=np.exp(padded_X_train_rescaled)
        padded_Y_train_rescaled=padded_Y_train_normalzied * self.Y_max.reshape((self.Y_max.shape[0],1,1))
        padded_Y_train_rescaled=np.exp(padded_Y_train_rescaled)
        return(padded_X_train_rescaled,padded_Y_train_rescaled)

#Generate training data set ( by subsampling the data in training set)
def SubSampleData(padded_X_train ,padded_Y_train,Locations,Input_lag=7,Interval_ahead=3):
    Encoder_Input=[]
    Decoder_Input=[]
    Decoder_Output=[]
    AllLocations=[]
    for Divides in range(Input_lag,padded_X_train.shape[1]-Interval_ahead):
        #Decoder got the groud truth with one time step off
        Encoder_Input.append(padded_X_train[:,(Divides-Input_lag):Divides ,:])

        #one time step off
        #Decoder_Input.append(np.hstack([padded_Y_train[:, (Divides-1):(padded_Y_train.shape[1]-1), :],
         #                         np.zeros((padded_Y_train.shape[0], Divides, padded_Y_train.shape[2]))]))

        Decoder_Output.append(padded_Y_train[:, Divides+Interval_ahead-1, :])
        AllLocations.append(np.asarray(Locations))


    Encoder_Input=np.concatenate(Encoder_Input)#input into encoder
    #Decoder_Input=np.concatenate(Decoder_Input) #input indecoder
    Decoder_Output=np.concatenate(Decoder_Output)# output from decoder
    Decoder_Output=Decoder_Output.reshape((Decoder_Output.shape[0],Decoder_Output.shape[1]))
    AllLocations=np.concatenate(AllLocations)

    #Shuffle the data

    Indexes=np.asarray(range(Encoder_Input.shape[0]))
    np.random.shuffle(Indexes)
    Encoder_Input=Encoder_Input[Indexes,:,:]
    #Decoder_Input=Decoder_Input[Indexes,:,:]
    Decoder_Output=Decoder_Output[Indexes,:]
    AllLocations=AllLocations[Indexes]



    return Encoder_Input,Decoder_Output,AllLocations





#NN MODEL
from keras.layers import SimpleRNN,Dense,LSTM,Embedding,Masking,Input,GRU,Dropout,Masking,Flatten,Activation,concatenate,RepeatVector,merge,Multiply,Add,Lambda
import keras.backend as K
from keras.models import Model
from keras import Sequential
import keras


def repeat_vector(args):
    layer_to_repeat = args[0]
    sequence_layer = args[1]
    return RepeatVector(K.shape(sequence_layer)[1])(layer_to_repeat)


class RNN_model():

    def GetModel(self,N_features_source,Max_step_Source,N_features_target,hidden_size,OutputSize, dropout=0.5,learning_rate=1e-3):

        #encoding stage
        #SourceInput = Input(shape=(Max_step_Source,N_features_source))
        #Mask1= Masking(mask_value=0.,)(SourceInput)

        #output_encoder, state_h= GRU(hidden_size, return_state=True,return_sequences=False, name='encoder')(SourceInput)
        #encoder_state = [state_h]

        #output, state_h, state_c = LSTM(hidden_size, return_state=True, name='encoder')(SourceInput)
        #encoder_state = [state_h, state_c]

        SourceInput = Input(shape=(N_features_source,))
        output_encoder = Dense(hidden_size,activation="relu")(SourceInput)

        #HiddenState=SimpleRNN(hidden_size,return_sequences=False)(SourceInput)


        #Simple attention
        #attention = Dense(1, activation='tanh')(HiddenStates)
        #attention = Flatten()(attention)
        #attention = Activation('softmax')(attention)


        #decoding stage
        #TargetInput = Input(shape=(Max_step_Target,N_features_target))
        #Mask2 =Masking(mask_value=0.)(TargetInput)
        #Decoder_RNN = SimpleRNN(hidden_size, return_sequences=True)(TargetInput,initial_state=HiddenState)

        #decoder_output = SimpleRNN(hidden_size,return_sequences=True, name='decoder')(TargetInput, initial_state=encoder_state )
        #decoder_output =  LSTM(hidden_size, return_sequences=True)(decoder_output)




        #Hidden=Dense(hidden_size,activation="relu")(Decoder_RNN)

        #Flatten()(HiddenState)

        ## Bahdanau attention
        #encoder_gru = GRU(hidden_size, return_sequences=True, return_state=True, name='encoder_gru')
        #encoder_out, encoder_state = encoder_gru(SourceInput)

        # Set up the decoder GRU, using `encoder_states` as initial state.
        #decoder_gru = GRU(hidden_size, return_sequences=True, return_state=True, name='decoder_gru')
        #decoder_out, decoder_state = decoder_gru(TargetInput , initial_state=encoder_state)

        # Attention layer
        #attn_layer = AttentionLayer(name='attention_layer')
        #attn_out, attn_states = attn_layer([encoder_out, decoder_out])

        # attention = Dense(1, activation='tanh')(HiddenState)
        # # attention = Flatten()(attention)
        # attention = Activation('softmax')(attention)
        # # attention=RepeatVector(HiddenState.shape[2])(attention)
        # sent_representation = Multiply()([HiddenState, attention])
        # sent_representation = Lambda(lambda x: K.sum(x, axis=1), output_shape=lambda s: (s[0], s[2]))(
        #     sent_representation)
        #
        # HiddenState = RepeatVector(Max_step_Source)(HiddenState)
        #
        # #TargetInput_repeated=RepeatVector(Max_step_Source)
        # InputToG=concatenate([Decoder_RNN,HiddenState,TargetInput])


        #combine output from each step of decoder RNN and encoder state
        #HiddenState=RepeatVector(Decoder_RNN.shape[1])(HiddenState)
        #reconstructed_HiddenState= Lambda(repeat_vector, output_shape=(None, N_features_target))([HiddenState, Decoder_RNN])
        #Combined=Multiply()([Decoder_RNN ,HiddenState])
        #Combined = Add()([Decoder_RNN, HiddenState])
        #Combined = concatenate([Decoder_RNN, reconstructed_HiddenState])
        #np.concatenate([Decoder_RNN, HiddenState],axis=1)
        decoder_output=Dropout(dropout)(output_encoder)
        decoder_output = Dense(hidden_size,activation="relu")(decoder_output)
        output = Dense(OutputSize)(decoder_output)
        #DecoderOutput= Dense(OutputSize)(Decoder_RNN)

        model = Model(inputs=SourceInput, outputs=output)
        print(model.summary())
        model.compile(optimizer=keras.optimizers.Adam(learning_rate=learning_rate,clipnorm=5),loss="mse")
        self.model=model

    def train_model(self, X,Y,N_Epochs=30,batch_size=2):
        self.history=self.model.fit(x=X, y=Y, epochs=N_Epochs, batch_size=batch_size)






class DNN_model():

    def GetModel(self,N_features_source,hidden_size,OutputSize, dropout=0.0,learning_rate=1e-3):



        SourceInput = Input(shape=(N_features_source,))
        output_encoder = Dense(hidden_size,activation="relu")(SourceInput)


        decoder_output=Dropout(dropout)(output_encoder)
        decoder_output = Dense(hidden_size,activation="relu")(decoder_output)
        output = Dense(OutputSize)(decoder_output)
        #DecoderOutput= Dense(OutputSize)(Decoder_RNN)

        model = Model(inputs=SourceInput, outputs=output)
        print(model.summary())
        model.compile(optimizer=keras.optimizers.Adam(learning_rate=learning_rate,clipnorm=5),loss="mse")
        self.model=model

    def train_model(self, X,Y,N_Epochs=30,batch_size=2):
        self.history=self.model.fit(x=X, y=Y, epochs=N_Epochs, batch_size=batch_size)
