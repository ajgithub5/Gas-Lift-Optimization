# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

import pandas as pd
import numpy as np
from math import sqrt
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import train_test_split
from scipy.optimize import linprog
import os
import datetime as dt
import seaborn as sns
import matplotlib.pyplot as plt
%matplotlib inline

os.chdir("C://Users//AJAY//Documents//Gas lift optimisation")
data = pd.read_csv('Gaslift.csv')
data.head(10)

#mydate = dt.datetime(data['Date'])

#Grouping Data with groups

dataA = data[data['Well']=='A']
dataB = data[data['Well']=='B']
dataC = data[data['Well']=='C']
dataD = data[data['Well']=='D']
dataE = data[data['Well']=='E']
dataF = data[data['Well']=='F']
dataG = data[data['Well']=='G']
dataH = data[data['Well']=='H']

corr_var = ['OilDay', 'Injgas', 'AnnulusA','AnnulusB', 'WHP', 'WaterDay', 'Choke', 'TotLiq']

cormatA = dataA[corr_var].corr()
cormatB = dataB[corr_var].corr()
cormatC = dataC[corr_var].corr()
cormatD = dataD[corr_var].corr()
cormatE = dataE[corr_var].corr()
cormatF = dataF[corr_var].corr()
cormatG = dataG[corr_var].corr()
cormatH = dataH[corr_var].corr()



# plot the heatmap for Well A

def heatmap_corr(cormat):
    sns.heatmap(cormat, 
                xticklabels=cormat.columns,
                yticklabels=cormat.columns,
                cmap=sns.diverging_palette(220, 10, as_cmap=True),center=0)
    
heatmap_corr(cormatA)
heatmap_corr(cormatB)
heatmap_corr(cormatC)
heatmap_corr(cormatD)
heatmap_corr(cormatE)
heatmap_corr(cormatF)
heatmap_corr(cormatG)
heatmap_corr(cormatH)    

dep_tags = ['OilDay','WaterDay','TotLiq']
Indep_tags = ['Injgas', 'AnnulusA','AnnulusB', 'WHP','Choke']
xtrainA,xtestA,ytrainA,ytestA = train_test_split(dataA[Indep_tags],dataA[dep_tags[0]],test_size=0.3,random_state=42)

#Oil
#Random forest model
def rf_oil(df):
    xtrain,xtest,ytrain,ytest = train_test_split(df[Indep_tags],df[dep_tags[0]],test_size=0.3,random_state=42)
    model = RandomForestRegressor(n_estimators=400,max_depth=6, min_samples_leaf=100,n_jobs=4,oob_score=True)
    model.fit(df[Indep_tags],df[dep_tags[0]])
    predictions = model.predict(df[Indep_tags])
    df['predicted_oil'] = predictions
    return(df['predicted_oil'])

def mape(y_true, y_pred): 
    y_true, y_pred = np.array(y_true), np.array(y_pred)
    return np.mean(np.abs((y_true - y_pred) / y_true))*100

def avsp_plot_oil(df):
    plt.plot(df['Date'],df['predicted_oil'],df['Date'],df['OilDay'])
    plt.xlabel('Date')
    plt.ylabel('Volume')
    plt.legend(['y=predicted_oil','y=actual_oil'],loc='upper right')
    plt.show()
    
rf_oil(dataG)
oil_errorG = mape(dataG[dep_tags[0]],dataG['predicted_oil'])
avsp_plot_oil(dataG)


#Water
#Random forest model
def rf_water(df):
    model = RandomForestRegressor(n_estimators=400,max_depth=6, min_samples_leaf=100,n_jobs=4,oob_score=True)
    model.fit(df[Indep_tags],df[dep_tags[1]])
    predictions = model.predict(df[Indep_tags])
    df['predicted_water'] = predictions
    return(df['predicted_water'])
    
def mape(y_true, y_pred):    
    y_true, y_pred = np.array(y_true), np.array(y_pred)
    return np.mean(np.abs((y_true - y_pred) / y_true)) * 100

def avsp_plot_water(df):
    plt.plot(df['Date'],df['predicted_water'],df['Date'],df['WaterDay'])
    plt.xlabel('Date')
    plt.ylabel('Volume')
    plt.legend(['y=predicted_water','y=actual_water'],loc='upper right')
    plt.show()

rf_water(dataH)
water_errorH = mape(dataH[dep_tags[1]],dataH['predicted_water'])
water_errorH
avsp_plot_water(dataH)

#Tot Liq
#Random forest model
def rf_tot(df):
    model = RandomForestRegressor(n_estimators=400,max_depth=6, min_samples_leaf=100,n_jobs=4,oob_score=True)
    model.fit(df[Indep_tags],df[dep_tags[2]])
    predictions = model.predict(df[Indep_tags])
    df['predicted_tot'] = predictions
    return(df['predicted_tot'])
    
def mape(y_true, y_pred):    
    y_true, y_pred = np.array(y_true), np.array(y_pred)
    return np.mean(np.abs((y_true - y_pred) / y_true)) * 100

def avsp_plot_tot(df):
    plt.plot(df['Date'],df['predicted_tot'],df['Date'],df['TotLiq'])
    plt.xlabel('Date')
    plt.ylabel('Volume')
    plt.legend(['y=predicted_tot','y=actual_tot'],loc='upper right')
    plt.show()

rf_tot(dataH)
tot_errorH = mape(dataH[dep_tags[2]],dataH['predicted_tot'])
tot_errorH
avsp_plot_tot(dataH)

    
'''output_columns = ['PRIMARY_TAG','Date','Well','Actual_Oil','Predicted_oil','Error%','Injgas']
output_df = pd.DataFrame(columns = output_columns)

ytrain.iloc[2]
predictions[2]
xtrain['Injgas'][0]'''

'''#Linear programming
c = [1, -1]
A = [[-3, 1], [1, 2]]
b = [6, 4]
x0_bnds = (None, None)
x1_bnds = (-3, None)
res = linprog(c, A, b, bounds=(x0_bnds, x1_bnds))'''
    
lower=int(input("Injgas lower limit>>>"))
upper=int(input("Injgas upper limit>>>"))
injgas_seq = np.arange(lower,upper+100,100)

def newdf(df):
    ndf = pd.DataFrame(index=range(0,len(injgas_seq)),columns=['Injgas', 'AnnulusA','AnnulusB', 'WHP','Choke','OilDay','WaterDay'])
    for i in range(0,len(injgas_seq)):
        ndf['Injgas'][i] = injgas_seq[i]
        ndf['AnnulusA'][i]= df['AnnulusA'].mean()
        ndf['AnnulusB'][i]= df['AnnulusB'].mean()
        ndf['WHP'][i]=df['WHP'].mean()
        ndf['Choke'][i]=df['Choke'].mean()
    return(ndf)

def rf_oil_opt(df,ndf):
    model = RandomForestRegressor(n_estimators=400,max_depth=6, min_samples_leaf=100,n_jobs=4,oob_score=True)
    model.fit(df[Indep_tags],df[dep_tags[0]])
    predictions = model.predict(ndf[Indep_tags])
    ndf['OilDay'] = predictions
    return(ndf['OilDay'])
    
def rf_water_opt(df,ndf):
    model = RandomForestRegressor(n_estimators=400,max_depth=6, min_samples_leaf=100,n_jobs=4,oob_score=True)
    model.fit(df[Indep_tags],df[dep_tags[1]])
    predictions = model.predict(ndf[Indep_tags])
    ndf['WaterDay'] = predictions
    return(ndf['WaterDay'])

def ivso_plot_oil(df):
    plt.plot(df['Injgas'],df['OilDay'])
    plt.xlabel('Injgas')
    plt.ylabel('OilDay')
    plt.show()
    
def ivso_plot_water(df):
    plt.plot(df['Injgas'],df['WaterDay'])
    plt.xlabel('Injgas')
    plt.ylabel('WaterDay')
    plt.show()

optH = newdf(dataH)    
rf_oil_opt(dataH,optH)
ivso_plot_oil(optH)        
max(optH['OilDay'])

optA = newdf(dataA)
    
rf_water_opt(dataH,optH)
ivso_plot_water(optH)        
min(optH['WaterDay'])

























