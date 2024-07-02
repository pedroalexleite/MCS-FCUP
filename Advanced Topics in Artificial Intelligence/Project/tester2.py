import pandas as pd
import numpy as np
import tensorflow as tf
from tensorflow import keras
from sklearn.model_selection import train_test_split

df = pd.read_csv('train.csv')

train_X = df[['Subject Focus','Eyes', 'Face', 'Near', 'Action', 'Accessory', 'Group', 'Collage', 'Human', 'Occlusion', 'Info', 'Blur']].to_numpy()
train_Y = df['Pawpularity'].to_numpy()

#print(train_Y)

df2 = pd.read_csv('test.csv')

test_X = df2[['Subject Focus','Eyes', 'Face', 'Near', 'Action', 'Accessory', 'Group', 'Collage', 'Human', 'Occlusion', 'Info', 'Blur']].to_numpy()

#print(test_X)

model=keras.models.Sequential()

model.add(keras.layers.Dense(128,input_dim = 12, activation=tf.nn.tanh))
model.add(tf.keras.layers.Dense(128,activation=tf.nn.tanh))
model.add(tf.keras.layers.Dense(128,activation=tf.nn.tanh))
model.add(tf.keras.layers.Dense(1,activation=tf.nn.softmax))
model.compile(optimizer='adam', loss='mae', metrics=['accuracy'])
model.fit(train_X,train_Y,epochs=5) # load the model
