# -*- coding: utf-8 -*-
"""
Created on Mon Mar  6 18:19:20 2023

@author: Kriszti
"""

import pandas as pd
from glob import glob
from datetime import datetime
from scipy.stats import norm
Z = norm.ppf

# Functions

def d_prime_calc(hits, false_alarms, targets, foils):
    # Calculate hit_rate and avoid d' infinity
    hit_rate = hits / targets
    if hit_rate == 1: 
        hit_rate = 0.99999
    if hit_rate == 0: 
        hit_rate = 0.00001
    # Calculate false alarm rate and avoid d' infinity
    fa_rate = false_alarms / foils
    if fa_rate == 1: 
        fa_rate = 0.99999
    if fa_rate == 0: 
        fa_rate = 0.00001
    # Return d'
    d_prime = Z(hit_rate) - Z(fa_rate)
    return(d_prime)

# Procedure

files = input("Path of output files (e.g., C:/outputs/*.csv): ")

long = pd.DataFrame()
long_cols = ['Azonosító', 'blokk', 'Trial', 'tipus', 'betu',
             'response.keys', 'valasz', 'response.corr', 'response.rt']

for file in glob(files):
    df = pd.read_csv(file)
    long = pd.concat(
        [
            long,
            df[pd.notna(df['tipus'])][long_cols]
         ],
        ignore_index = True
        )
long.insert(loc = 2, column = 'back_num', value = long['blokk'].astype(str).str[0].astype(int))
long.rename(columns = {
    "Azonosító" : "ID",
    "blokk" : "block",
    "Trial" : "trial",
    "tipus" : "type",
    "betu" : "letter",
    "response.keys" : "resp",
    "valasz" : "correct_resp",
    "response.corr" : "ACC",
    "response.rt" : "RT"
    },
    inplace = True)

wide = pd.DataFrame()
for name, group in long.groupby(['ID', 'back_num']):
    ID = group['ID'].unique()[0]
    hs = len(group[(group['type'] == 'target') & (group['ACC'] == 1)])
    fa = len(group[((group['type'] == 'lure_min') | (group['type'] == 'lure_plus')) & (group['ACC'] == 0)])
    ts = len(group[group['type'] == 'target'])
    fs = len(group[(group['type'] == 'lure_min') | (group['type'] == 'lure_plus')])
    wide.loc[ID, 'nback_' + str(name[1]) + '_dprime'] = d_prime_calc(hits = hs,
                                                                     false_alarms = fa,
                                                                     targets = ts,
                                                                     foils = fs)
wide['nback_1_2_mean_dprime'] = (wide['nback_1_dprime'] + wide['nback_2_dprime']) / 2
wide['nback_1_2_3_mean_dprime'] = (wide['nback_1_dprime'] + wide['nback_2_dprime'] + wide['nback_3_dprime']) / 3

path = input("Path of result files: ")    
long.to_csv(path + "/Verbal_n_back_long_" + datetime.today().strftime("%Y%m%d") + ".csv",
            index = False)
wide.to_csv(path + "/Verbal_n_back_wide_" + datetime.today().strftime("%Y%m%d") + ".csv")
