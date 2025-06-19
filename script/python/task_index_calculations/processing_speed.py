# -*- coding: utf-8 -*-
"""
Created on Fri Jun  5 15:24:02 2020

@author: Kriszti
"""
# =============================================================================
# This is a script for processing processing spee data
# Experiment: PROCSPEED_YA_2020
# Input: .csv output files from online experiment
# Output:
#   1. long files with data from 1) visual, 2) acoustic, 3) visual choice PS
#   2. wide file with overall data from the three tasks
# Comments:
#   1. Need for more precise analysis: ignoring first n items
#   2. Only calculate visual decision scores above chance level?
#   3. Shifting cound be computed from the visual decision subtask?
#   4. How to deal with duplicate tests?
# =============================================================================

import pandas as pd
import glob, random
from datetime import datetime

# =============================================================================
# Define functions
# =============================================================================

def uniform_date(orig):
    strd = str(orig)
    form = {}
    date = strd.replace("/", ".").replace(" ", ".").split(".")
    date = list(filter(lambda d: (d != "") and (d != " "), date))
    date = [d.strip() for d in date]
    try:
        # Date formats
        # YYYY.MM.DD
        if len(date[0]) == 4:
            form['year'] = date[0]
            form['month'] = date[1]
            form['day'] = date[2]
        # DD.MM.YYYY
        elif len(date[0]) == 2 and len(date[1]) == 2 and (int(date[0]) > 12 or int(date[1]) < 13):
            form['year'] = date[2]
            form['month'] = date[1]
            form['day'] = date[0]
        # MM.DD.YYYY
        elif len(date[0]) == 2 and len(date[1]) == 2 and (int(date[0]) < 13 or int(date[1]) > 12):
            form['year'] = date[2]
            form['month'] = date[0]
            form['day'] = date[1]
        formatted_date = '.'.join([form['year'], form['month'], form['day']])
    except:
        formatted_date = ""
    return formatted_date

# =============================================================================
# Analysis
# =============================================================================

vis_rt_cols = ['date',
               'trials_visRT.thisIndex',
               'vis_time',
               'visRT_key_resp.rt']
ac_rt_cols = ['date',
              'trials_acRT.thisIndex',
              'ac_time',
              'acRT_key_resp.rt']
vis_dec_cols = ['date',
                'trials_vischoice.thisIndex',
                'ori_1',
                'ori_2',
                'corr_ans_vis',
                'trial_vischoice_key_resp.keys',
                'trial_vischoice_key_resp.corr',
                'trial_vischoice_key_resp.rt']
ac_dec_cols = ['date',
               'trials_acchoice.thisIndex',
               'tone_1',
               'tone_2',
               'corr_ans_ac',
               'trial_acchoice_key_resp.keys',
               'trial_acchoice_key_resp.corr',
               'trial_acchoice_key_resp.rt']

# Create data frames to collect data
vis_rt_data = pd.DataFrame()
ac_rt_data = pd.DataFrame()
vis_dec_data = pd.DataFrame()
ac_dec_data = pd.DataFrame()
all_data = pd.DataFrame()

# Setting path of output files
path = input("Path of output files (e.g., experiments/outputs/*.csv): ")

# Setting counter for overall data frame (all_data) row indexing
num = 0

# Reading and calculating actual data
for file in glob.glob(path):
    try:
        actual = pd.read_csv(file)
        ID = str(actual['Azonosító'].unique()[0])
        random_ID = random.randint(1111111111,9999999999)
        birth_date = uniform_date(actual['Születési dátum (ÉÉÉÉ.HH.NN)'].unique()[0])
        #date = str(actual['date'].unique())[2:-2]
        
        vis_rt = actual.loc[actual['visRT_key_resp.keys'].notna()][vis_rt_cols]
        vis_rt.insert(loc = 0, column = 'ID', value = ID)
        vis_rt_data = pd.concat([vis_rt_data, vis_rt])
        
        ac_rt = actual.loc[actual['acRT_key_resp.keys'].notna()][ac_rt_cols]
        ac_rt.insert(loc = 0, column = 'ID', value = ID)
        ac_rt_data = pd.concat([ac_rt_data, ac_rt])
        
        vis_dec = actual.loc[actual['trial_vischoice_key_resp.keys'].notna()][vis_dec_cols]
        vis_dec.insert(loc = 0, column = 'ID', value = ID)
        ori_diff = abs(vis_dec['ori_1'] - vis_dec['ori_2'])
        vis_dec.insert(loc = 5, column = 'ori_diff', value = ori_diff)
        vis_dec_data = pd.concat([vis_dec_data, vis_dec])
    
        ac_dec = actual.loc[actual['trial_acchoice_key_resp.keys'].notna()][ac_dec_cols]
        ac_dec.insert(loc = 0, column = 'ID', value = ID)
        tone_diff = abs(ac_dec['tone_1'] - ac_dec['tone_2'])
        ac_dec.insert(loc = 5, column = 'tone_diff', value = tone_diff)
        ac_dec_data = pd.concat([ac_dec_data, ac_dec])
        
        all_data.at[num, 'random_ID'] = random_ID
        all_data.at[num, 'ID'] = ID
        all_data.at[num, 'birth_date'] = birth_date
        all_data.at[num, 'vis_RT_med'] = vis_rt['visRT_key_resp.rt'].median()
        all_data.at[num, 'ac_RT_med'] = ac_rt['acRT_key_resp.rt'].median()
        all_data.at[num, 'vis_dec_RT_med'] = vis_dec.loc[vis_dec['trial_vischoice_key_resp.corr'] == 1]['trial_vischoice_key_resp.rt'].median()
        all_data.at[num, 'vis_dec_ACC'] = vis_dec['trial_vischoice_key_resp.corr'].mean()
        all_data.at[num, 'vis_dec_score'] = 1 - ((all_data.at[num, 'vis_dec_RT_med']) * (1 - (all_data.at[num, 'vis_dec_ACC'])))
        all_data.at[num, 'ac_dec_RT_med'] = ac_dec.loc[ac_dec['trial_acchoice_key_resp.corr'] == 1]['trial_acchoice_key_resp.rt'].median()
        all_data.at[num, 'ac_dec_ACC'] = ac_dec['trial_acchoice_key_resp.corr'].mean()
        all_data.at[num, 'ac_dec_score'] = 1 - ((all_data.at[num, 'ac_dec_RT_med']) * (1 - (all_data.at[num, 'ac_dec_ACC'])))
        
        num = num + 1
    except:
        pass

for var in ['vis_RT_med', 'ac_RT_med', 'vis_dec_RT_med', 'ac_dec_RT_med']:
    q1 = all_data[var].quantile(0.25)
    q3 = all_data[var].quantile(0.75)
    iqr = q3 - q1
    all_data[var] = all_data[var].apply(lambda x: float("nan") if ((x < q1 - (2.5 * iqr)) | (x > q3 + (2.5 * iqr))) else x)

criterion = 0.75
excl_part_vis = list(all_data[all_data['vis_dec_ACC'] < criterion]['ID'])
excl_part_ac = list(all_data[all_data['ac_dec_ACC'] < criterion]['ID'])
vis_dec_data = vis_dec_data[vis_dec_data['ID'].isin(excl_part_vis) == False]
ac_dec_data = ac_dec_data[ac_dec_data['ID'].isin(excl_part_ac) == False]
all_data.loc[all_data['ID'].isin(excl_part_vis), ['vis_dec_RT_med', 'vis_dec_ACC', 'vis_dec_score']] = float('nan')
all_data.loc[all_data['ID'].isin(excl_part_ac), ['ac_dec_RT_med', 'ac_dec_ACC', 'ac_dec_score']] = float('nan')


#all_data = all_data[(all_data['vis_dec_ACC'] >= 0.7) & (all_data['ac_dec_ACC'] >= 0.7)]

results_path = input("Path of result files: ")

vis_rt_data.to_csv(results_path + '/PROC_SPEED_v2_vis_RT_' + datetime.today().strftime('%Y%m%d') + '.csv',
                   index = False)
ac_rt_data.to_csv(results_path + '/PROC_SPEED_v2_ac_RT_' + datetime.today().strftime('%Y%m%d') + '.csv',
                  index = False)
vis_dec_data.to_csv(results_path + '/PROC_SPEED_v2_vis_dec_' + datetime.today().strftime('%Y%m%d') + '.csv',
                    index = False)
ac_dec_data.to_csv(results_path + '/PROC_SPEED_v2_ac_dec_' + datetime.today().strftime('%Y%m%d') + '.csv',
                   index = False)
all_data.to_csv(results_path + '/PROC_SPEED_v2_' + datetime.today().strftime('%Y%m%d') + '.csv',
                index = False)
