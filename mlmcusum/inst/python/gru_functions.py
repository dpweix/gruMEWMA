import os 
import numpy as np

from tensorflow import keras
from tensorflow.keras import layers

def train_gru(X, Y, lags):
  p = X.shape[1]
  h = 1*p # number of hidden nodes per layer
  
  opt = keras.optimizers.Adam(learning_rate=0.01)
  los = keras.losses.MeanSquaredError()
  
  num_epochs = 100
  num_batch_size = 128

  fit_gru = keras.Sequential()
  fit_gru.add(layers.GRU(h, activation = 'tanh', input_shape = (p, 1)))
  fit_gru.add(layers.Dense(Y.shape[1]))
  
  fit_gru.summary()
  
  fit_gru.compile(optimizer = opt, loss = los)
  
  fit_gru.fit(X, Y,
              epochs = num_epochs,
              batch_size = num_batch_size,
              verbose = 0)
  return fit_gru

def pred_gru(model, X):
  return model.predict(X)
