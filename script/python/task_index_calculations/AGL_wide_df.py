# -*- coding: utf-8 -*-
"""
Created on Wed Jul  8 17:11:15 2020

@author: Kriszti
"""

# =============================================================================
# 
# =============================================================================

import pandas as pd
import sys
from datetime import datetime
from scipy.stats import norm
from scipy.stats import fisher_exact
Z = norm.ppf

# =============================================================================
# 1. Define functions
# =============================================================================

def read_online():
    global online
    try:
        online_path = input("Path of online preprocessed .csv file: ")
        online = pd.read_csv(online_path)
    except:
        read_online()

def read_2AFC():
    global twoAFC
    try:
        twoAFC_path = input("Path of 2AFC preprocessed .csv file: ")
        twoAFC = pd.read_csv(twoAFC_path)
    except:
        read_2AFC()

def read_prod():
    global prod
    try:
        prod_path = input("Path of production preprocessed .csv file: ")
        prod = pd.read_csv(prod_path)
    except:
        read_prod()

def check_IDs(first, second, third):
    global IDs
    if (first == second) and (second == third):
        IDs = first
    else:
        resp = input("Different IDs in the three tables, first set of IDs will be used. Would you like to continue? (y/n) ")
        if resp == "y":
            IDs = first
        elif resp == "n":
            sys.exit()
        else:
            check_IDs(first, second, third)

def export_wide(table, exp):
    out_path = input("Output folder (e.g., C:/experiment): ")
    table.to_csv(out_path + '/AGL_KS_mod4_wide_' + exp + "_" + datetime.today().strftime('%Y%m%d') + '.csv',
                 index = False)

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


# =============================================================================
# 2. Data processing and analysis
# =============================================================================

# 2.1. Get neccessary data

target = input("Target: ")

read_online()
online['ID'] = online['ID'].astype(str)
read_2AFC()
twoAFC['ID'] = twoAFC['ID'].astype(str)
read_prod()
prod['ID'] = prod['ID'].astype(str)

# 2.2. Actual data processing

online_IDs = list(online['ID'].unique())
twoAFC_IDs = list(twoAFC['ID'].unique())
prod_IDs = list(prod['ID'].unique())

check_IDs(online_IDs, twoAFC_IDs, prod_IDs)
subjects = online_IDs

del online_IDs, twoAFC_IDs, prod_IDs

AGL = pd.DataFrame()

# d-prime filtering

d_prime_data = pd. DataFrame()
crosstabs = {}

for ID in subjects:
    try:
        trgt = online[(online['ID'] == ID) & (online['syl'] == online['target'])]
        foil = online[(online['ID'] == ID) & (online['syl'] != online['target'])]
        d_prime = d_prime_calc(hits = len(trgt[pd.notna(trgt['RT'])]),
                               false_alarms = len(foil[pd.notna(foil['RT'])]),
                               targets = len(trgt),
                               foils = len(foil))
        crosstab = pd.DataFrame(columns = ['trgts', 'foils'],
                                index = ['resp', 'no_resp'])
        crosstab.at['resp', 'trgts'] = len(trgt[pd.notna(trgt['RT'])])
        crosstab.at['no_resp', 'trgts'] = len(trgt[pd.isna(trgt['RT'])])
        crosstab.at['resp', 'foils'] = len(foil[pd.notna(foil['RT'])])
        crosstab.at['no_resp', 'foils'] = len(foil[pd.isna(foil['RT'])])
        odds_ratio, p_value = fisher_exact(crosstab)
        d_prime_data.at[ID, 'trgt_resp_num'] = len(trgt[pd.notna(trgt['RT'])])
        d_prime_data.at[ID, 'foil_resp_num'] = len(foil[pd.notna(foil['RT'])])
        d_prime_data.at[ID, 'hits'] = d_prime_data.at[ID, 'trgt_resp_num'] / len(trgt) # trgt_number
        d_prime_data.at[ID, 'false_alarms'] = d_prime_data.at[ID, 'foil_resp_num'] / len(foil) # foil_number
        d_prime_data.at[ID, 'd_prime'] = d_prime
        d_prime_data.at[ID, 'odds_ratio'] = odds_ratio
        d_prime_data.at[ID, 'p_value'] = p_value
        crosstabs[ID] = crosstab
        if d_prime_data.at[ID, 'p_value'] <= 0.05 and d_prime_data.at[ID, 'd_prime'] >= 0:
            d_prime_data.at[ID, 'OK'] = 1
        else:
            d_prime_data.at[ID, 'OK'] = 0
    except:
        d_prime_data.at[ID, 'OK'] = 9999

excluded_subjects = list(d_prime_data[d_prime_data['OK'] != 1].index)
filtered_subjects = [x for x in subjects if x not in excluded_subjects]

for ID in filtered_subjects:
    AGL.at[ID, 'ID'] = ID
    # Online:
    block_medRT = {}
    block_mednormRT = {}
    block_ACC = {}
    for block in range(1, 6):
        this = online[(online['ID'] == ID) &
                      (online['block_num'] == block) &
                      (online['syl'] == target)]
        block_medRT[block] = this['RT'].median()
        block_mednormRT[block] = this['norm_RT'].median()
        block_ACC[block] = this['ACC'].mean()
    # medRT
    AGL.at[ID, 'medRT_block1'] = block_medRT[1]
    AGL.at[ID, 'medRT_block2'] = block_medRT[2]
    AGL.at[ID, 'medRT_block3'] = block_medRT[3]
    AGL.at[ID, 'medRT_block4'] = block_medRT[4]
    AGL.at[ID, 'medRT_block5'] = block_medRT[5]
    AGL.at[ID, 'medRT_train'] = block_medRT[1] - block_medRT[3]
    AGL.at[ID, 'medRT_block_4_3'] = block_medRT[4] - block_medRT[3]
    AGL.at[ID, 'medRT_block_4_5'] = block_medRT[4] - block_medRT[5]
    AGL.at[ID, 'medRT_diff'] = ((block_medRT[4] - block_medRT[3]) + (block_medRT[4] - block_medRT[5])) / 2
    # normalized medRT
    AGL.at[ID, 'mednormRT_block1'] = block_mednormRT[1]
    AGL.at[ID, 'mednormRT_block2'] = block_mednormRT[2]
    AGL.at[ID, 'mednormRT_block3'] = block_mednormRT[3]
    AGL.at[ID, 'mednormRT_block4'] = block_mednormRT[4]
    AGL.at[ID, 'mednormRT_block5'] = block_mednormRT[5]
    AGL.at[ID, 'mednormRT_train'] = block_mednormRT[1] - block_mednormRT[3]
    AGL.at[ID, 'mednormRT_block_4_3'] = block_mednormRT[4] - block_mednormRT[3]
    AGL.at[ID, 'mednormRT_block_4_5'] = block_mednormRT[4] - block_mednormRT[5]
    AGL.at[ID, 'mednormRT_diff'] = ((block_mednormRT[4] - block_mednormRT[3]) + (block_mednormRT[4] - block_mednormRT[5])) / 2
    # ACC
    AGL.at[ID, 'ACC_block1'] = block_ACC[1]
    AGL.at[ID, 'ACC_block2'] = block_ACC[2]
    AGL.at[ID, 'ACC_block3'] = block_ACC[3]
    AGL.at[ID, 'ACC_block4'] = block_ACC[4]
    AGL.at[ID, 'ACC_block5'] = block_ACC[5]
    AGL.at[ID, 'ACC_train'] = block_ACC[3] - block_ACC[1]
    AGL.at[ID, 'ACC_block_4_3'] = block_ACC[3] - block_ACC[4]
    AGL.at[ID, 'ACC_block_4_5'] = block_ACC[5] - block_ACC[4]
    AGL.at[ID, 'ACC_diff'] = ((block_ACC[3] - block_ACC[4]) + (block_ACC[5] - block_ACC[4])) / 2
    del block, this, block_medRT, block_ACC
    # 2AFC:
    try:
        detail = twoAFC[twoAFC['ID'] == ID].groupby(['Trial_type', 'Violation_type']).agg({'choice_resp.corr': 'mean'})
        trial = twoAFC[twoAFC['ID'] == ID].groupby(['Trial_type']).agg({'choice_resp.corr': 'mean'})
        AGL.at[ID, '2AFC_all'] = twoAFC[twoAFC['ID'] == ID]['choice_resp.corr'].mean()
        AGL.at[ID, '2AFC_phr'] = trial.at['phrase', 'choice_resp.corr']
        AGL.at[ID, '2AFC_sent'] = trial.at['string', 'choice_resp.corr']
        AGL.at[ID, '2AFC_phr_freq'] = detail.at[('phrase', 'low vs high frequency'), 'choice_resp.corr']
        AGL.at[ID, '2AFC_phr_0TP'] = detail.at[('phrase', 'zero TP'), 'choice_resp.corr']
        AGL.at[ID, '2AFC_sent_ch'] = detail.at[('string', 'item_change'), 'choice_resp.corr']
        AGL.at[ID, '2AFC_sent_repl'] = detail.at[('string', 'item_replacement'), 'choice_resp.corr'] 
        del detail, trial
    except:
        pass
    # Prod:
    try:
        this = prod[prod['ID'] == ID]
        AGL.at[ID, 'prod'] = this['production_resp.corr'].mean()
        # Post hoc overall:
        AGL.at[ID, 'offline_overall'] = ((AGL.at[ID, '2AFC_all'] * 38) + (AGL.at[ID, 'prod'] * 24)) / 62
    except:
        pass

exp = input("Experiment name: ")

export_wide(AGL, exp)

# =============================================================================
# online = ot.copy()
# twoAFC = prep_2AFC.copy()
# prod = prep_prod.copy()
# =============================================================================
