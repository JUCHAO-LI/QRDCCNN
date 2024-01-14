import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
plt.rcParams['font.sans-serif'] = 'SimHei'
plt.rcParams['axes.unicode_minus'] = False
from sklearn.preprocessing import MinMaxScaler
from sklearn.model_selection import train_test_split
import tensorflow as tf
from keras import Sequential, layers, utils
from keras.layers import Input, Dense, LSTM, MaxPooling1D, Conv1D, Flatten, BatchNormalization, ReLU
from keras.models import Model
from sklearn.metrics import r2_score
from math import sqrt
from sklearn.metrics import mean_absolute_error
from sklearn.metrics import mean_squared_error
import copy
from sklearn.model_selection import GridSearchCV
from sklearn.ensemble import RandomForestRegressor
from sklearn.decomposition import PCA
from tensorflow.keras import backend as K
from keras.regularizers import l1_l2



NUM_UNROLLINGS = 128
NUM_EPOCHS = 100
BATCH_SIZE =64

def quantile_loss(q,y_true,y_pred):
    """
    q -- quantile level
    y_true -- true values
    y_pred -- predicted values
    """
    diff = (y_true - y_pred)
    mask = y_true >= y_pred
    mask_ = y_true < y_pred
    loss = (q * K.sum(tf.boolean_mask(diff, mask), axis=-1) - (1 - q) * K.sum(tf.boolean_mask(diff, mask_), axis=-1))
    return loss



def model(trainX, trainY,valX,valY):
    """
    trainX -- input values; shape: [number of samples, NUM_UNROLLINGS, 1]
    trainY -- output values (inputs shifted by 1); shape: [number of samples, NUM_UNROLLINGS, 1]
    """
    varlevel=0.95

    filters = 32
    kernel_size = 2
    reg = l1_l2(l1=0.01, l2=0.01)
    
    input_ts = Input(shape=(trainX.shape[1],1))
    x = Conv1D(filters, kernel_size, activation="relu", kernel_regularizer=reg)(input_ts)
    x = Conv1D(filters, kernel_size, activation="relu", kernel_regularizer=reg)(x)
    x = Conv1D(filters, kernel_size, activation="relu", kernel_regularizer=reg)(x)
    x = Conv1D(filters, kernel_size, activation="relu", kernel_regularizer=reg)(x)
    x = Conv1D(filters, kernel_size, activation="relu", kernel_regularizer=reg)(x)
    x = Conv1D(filters, kernel_size, activation="relu", kernel_regularizer=reg)(x)
#     x = MaxPooling1D(pool_size=2, strides = 1)(x)
    next = Conv1D(1, 1, activation="linear")(x)
    cnn = Model(input_ts, next)
#     x = Flatten()(x)
#     next = Dense(1,activation='linear',use_bias=True)(x)
#     cnn = Model(input_ts, next)
    cnn.compile(optimizer="adadelta", loss=lambda y_true,y_pred: quantile_loss(varlevel,y_true,y_pred))
    history =cnn.fit(trainX, trainY,epochs=NUM_EPOCHS,batch_size=BATCH_SIZE,shuffle=True,validation_data=(valX,valY))
    return cnn,history


