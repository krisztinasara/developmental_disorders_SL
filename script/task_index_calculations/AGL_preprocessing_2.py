# -*- coding: utf-8 -*-
"""
Created on Mon Jul  6 13:40:59 2020

@author: Kriszti
"""

# =============================================================================
# This script is for the second step for preprocessing raw data from the AGL
# task registered in the 202005onlineAGL testing session.
# -> INPUT: online data table as the result of step 1 preprocessing
# -> OUTPUT: online data with RTs to each syllable
# !!! IMPORTANT: it works only if there was one session with one participant, so
# !!! it is important to make a preliminary filtering.
# =============================================================================

import pandas as pd
from datetime import datetime

# =============================================================================
# 1. Define functions:
# =============================================================================

def start_info():
    global target
    global item_dur
    global window
    target = input('Target: ')
    item_dur = float(input('Item duration in seconds: '))
    window = float(input('Time window in seconds: '))

def import_table():
    global ot
    try:
        table_path = input('Input table: ')
        ot = pd.read_csv(table_path)
    except:
        import_table()

def in_time(RT, RT_from):
    global window
    if RT - RT_from <= window:
        return True
    else:
        return False

def lookup_RT(table, i, step):
    try:
        if table.at[i - step, 'syl'] == target:
            if pd.isna(table.at[i - step, 'RT']):
                if in_time(table.at[i, 'RT_abs'], table.at[i - step, 'start']):
                    table.at[i - step, 'RT'] = table.at[i, 'RT_abs'] - table.at[i - step, 'start']
                    table.at[i - step, 'ACC'] = 1
                    table.at[i, 'ACC'] = 1
                # If it's not in time:
                else:
                    table.at[i, 'RT'] = table.at[i, 'RT_abs'] - table.at[i, 'start']
                    table.at[i, 'ACC'] = 0
            # If there was a response before:
            else:
                table.at[i, 'RT'] = table.at[i, 'RT_abs'] - table.at[i, 'start']
                table.at[i, 'ACC'] = 0
        # If it's not a target:
        else:
            # This condition needs some polishing here...
            if step * item_dur <= window:
                step = step + 1
                lookup_RT(table, i, step)    
    except:
        table.at[i, 'RT'] = table.at[i, 'RT_abs'] - table.at[i, 'start']
        table.at[i, 'ACC'] = 0
    return table

def sentence_RTs(table):
    # RTs:
    for index, r in table.iterrows():
    # If there was an RT:
        if pd.notna(table.at[index, 'RT_abs']):
            lookup_RT(table = table, i = index, step = 0)
    # ACCs:
    for i, r in table.iterrows():
        if pd.isna(table.at[i, 'ACC']):
            if table.at[i, 'syl'] == target:
                if pd.notna(table.at[i, 'RT']):
                    table.at[i, 'ACC'] = 1
                else:
                    table.at[i, 'ACC'] = 0
            elif table.at[i, 'syl'] != target:
                if pd.notna(table.at[i, 'RT']):
                    table.at[i, 'ACC'] = 0
                else:
                    table.at[i, 'ACC'] = 1
    return table

def RT_subjlev_outl(table):
    q1 = table['RT'].quantile(0.25)
    q3 = table['RT'].quantile(0.75)
    iqr = q3 - q1
    k = 1.5
    for i, r in table.iterrows():
        if table.at[i, 'RT'] < (q1 - (k * iqr)) or table.at[i, 'RT'] > (q3 + (k * iqr)):
            table.at[i, 'RT_filt'] = float('nan')
        else:
            table.at[i, 'RT_filt'] = table.at[i, 'RT']
    return table

def sentence_ACCs(table):
    # ACCs:
    for i, r in table.iterrows():
        if pd.isna(table.at[i, 'ACC']):
            if table.at[i, 'syl'] == target:
                if pd.notna(table.at[i, 'RT_filt']):
                    table.at[i, 'ACC'] = 1
                else:
                    table.at[i, 'ACC'] = 0
            elif table.at[i, 'syl'] != target:
                if pd.notna(table.at[i, 'RT_filt']):
                    table.at[i, 'ACC'] = 0
                else:
                    table.at[i, 'ACC'] = 1
    return table

def outl_filt():
    resp = input("Perform outlier filtering on the subject level? [y/n] ")
    if resp in ["y", "n"]:
        return resp
    else:
        resp = outl_filt()
    return resp
    

def export_table(table, exp):
    out_path = input("Output folder (e.g., C:/experiment): ")
    table.to_csv(out_path + '/AGL_KS_mod4_online_processed_' + exp + "_" + datetime.today().strftime('%Y%m%d') + '.csv',
                 index = False)

def standardize(df, RT_col, norm_RT_col):
    mean = df[RT_col].mean(skipna = True)
    std = df[RT_col].std(skipna = True)
    for i, v in df.iterrows():
        if pd.notna(df.at[i, RT_col]):
            # Z = (score - mean) / std
            df.at[i, norm_RT_col] = (df.at[i, RT_col] - mean) / std
    return df

# =============================================================================
# 2. Data analysis:
# =============================================================================

# =============================================================================
# 2.1. Ask for necessary data
# =============================================================================
start_info()
import_table()
RT_filtering = outl_filt()
exp = input("Experiment name: ")

# =============================================================================
# 2.2. Execute actual data analysis
# =============================================================================
ot['RT'] = float('nan')
ot['ACC'] = float('nan')

if RT_filtering == "y":
    ot.insert(11, 'RT_filt', float('nan'))
for name, group in ot.groupby(['ID', 'block_num', 'sent_num']):
    print("ID: " + str(name[0]))
    ot[(ot.ID == name[0]) & (ot.block_num == name[1]) & (ot.sent_num == name[2])] =\
    sentence_RTs(ot[(ot.ID == name[0]) & (ot.block_num == name[1]) & (ot.sent_num == name[2])])
    if RT_filtering == "y":
        ot[ot.ID == name[0]] = RT_subjlev_outl(ot[ot.ID == name[0]])
    ot[(ot.ID == name[0]) & (ot.block_num == name[1]) & (ot.sent_num == name[2])] =\
    sentence_ACCs(ot[(ot.ID == name[0]) & (ot.block_num == name[1]) & (ot.sent_num == name[2])])
if RT_filtering == "y":
    ot.rename(columns = {'RT' : 'RT_orig',
                         'RT_filt' : 'RT'}, inplace = True)
ot['norm_RT'] = float('nan')
for ID in ot['ID'].unique():
    ot[ot['ID'] == ID] = standardize(
        ot[ot['ID'] == ID],
        RT_col = 'RT', norm_RT_col = 'norm_RT'
        )

# =============================================================================
# 2.3. Export data frame
# =============================================================================
export_table(ot, exp)
