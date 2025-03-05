# -*- coding: utf-8 -*-
"""
Created on Wed May 20 13:38:57 2020

@author: Kriszti
"""

import pandas as pd
import numpy as np
import glob
from datetime import datetime

output_path = input("Output path (e.g., C:/experiments/data/*.csv): ")

# DataFrame for all online preprocessed data:
prep_online = pd.DataFrame()
prep_2AFC = pd.DataFrame()
prep_prod = pd.DataFrame()

col_2AFC = ['expName',
            'forced_choice_trials.thisIndex',
            'gramm1',
            'gramm2',
            'gramm3',
            'gramm4',
            'gramm5',
            'gramm6',
            'gramm7',
            'ungramm1',
            'ungramm2',
            'ungramm3',
            'ungramm4',
            'ungramm5',
            'ungramm6',
            'ungramm7',
            'Second',
            'Trial_type',
            'Violation_type',
            'Length',
            'start_viol',
            'TP_viol',
            'difficulty',
            'Correct',
            'choice_resp.keys',
            'choice_resp.corr',
            'choice_resp.rt']
col_prod = ['expName',
            'production_trials.thisIndex',
            'Sentence',
            'word1',
            'word2',
            'word3',
            'word4',
            'word5',
            'word6',
            'word7',
            'resp1',
            'resp2',
            'resp3',
            'Target',
            'Target_pos',
            'Distr_1_pos',
            'Distr_2_pos',
            'production_resp.keys',
            'production_resp.corr',
            'production_resp.rt']

for file in glob.glob(output_path):
    this_table = pd.read_csv(file) #, error_bad_lines = False)
    exp = this_table['expName'].unique()[0]
    #if pd.isna(this_table['Subject'][0]) or type(this_table['Subject'][0]) == str:
    #    continue
    # First, create an empty dataframe for the results per participant:
    o_t = pd.DataFrame(columns = ['ID',
                                  'number',
                                  'sent',
                                  'sent_num',
                                  'block',
                                  'block_num',
                                  'target',
                                  'syl',
                                  'start',
                                  'RT_abs'])
    i = 0
    block_num = 1
    # Extracting data from the output file:
    for index, row in this_table.iterrows():
        time = 0.05
        num = 0
        if type(this_table.at[index, 'Feltetel']) == str:
            try:
                rts = [float(item) for item in this_table.at[index, 'online_space_resp.rt'][1:-1].split(",")]
            except:
                rts = []
            last = False
            #for column in ['word1', 'word2', 'word3', 'word4', 'word5', 'word6', 'word7']:
            for column in ['elso', 'masodik', 'harmadik', 'negyedik', 'otodik', 'hatodik', 'hetedik']:
                #print(this_table.at[index, column])
                if this_table.at[index, column] == "--":
                    last = True
                    continue
                o_t.at[i, 'ID'] = this_table.at[0, 'Azonosító']
                o_t.at[i, 'number'] = num
                o_t.at[i, 'sent'] = this_table.at[index, 'Sztring']
                o_t.at[i, 'sent_num'] = int(this_table.at[index, 'Sztring'].split("_")[-1].split(".")[0])
                o_t.at[i, 'block'] = "BLOCK" + str(block_num)
                o_t.at[i, 'block_num'] = block_num
                o_t.at[i, 'target'] = "pef"
                o_t.at[i, 'syl'] = this_table.at[index, column]
                o_t.at[i, 'start'] = time
                for rt in rts:
                    if last == False:
                        if rt >= time and rt < round(time + 0.45, 3):
                            o_t.at[i, 'RT_abs'] = rt
                    elif last == True:
                        if rt >= time:
                            o_t.at[i, 'RT_abs'] = rt
                time = round(time + 0.45, 3)
                num = num + 1
                i = i + 1
        else:
            block_num = block_num + 1
    prep_online = pd.concat([prep_online, o_t])
    
    # 2AFC task
    try:
        AGL_2AFC = this_table.loc[this_table['Length'].notna()][col_2AFC]
        AGL_2AFC.insert(0, 'ID', this_table.at[0, 'Azonosító'])
        prep_2AFC = pd.concat([prep_2AFC, AGL_2AFC])
    except:
        pass
    
    # Production task
    try:
        AGL_prod = this_table.loc[this_table['Sentence'].notna()][col_prod]
        AGL_prod.insert(0, 'ID', this_table.at[0, 'Azonosító'])
        prep_prod = pd.concat([prep_prod, AGL_prod])
    except:
        pass

r_path = input("Path of result files: ")

prep_online.to_csv(r_path + '/AGL_KS_mod4_online_' + exp + "_" + datetime.today().strftime('%Y%m%d') + '.csv',
                    index = False)
prep_2AFC.to_csv(r_path + '/AGL_KS_mod4_2AFC_' + exp + "_" + datetime.today().strftime('%Y%m%d') + '.csv',
                 index = False)
prep_prod.to_csv(r_path + '/AGL_KS_mod4_prod_' + exp + "_" + datetime.today().strftime('%Y%m%d') + '.csv',
                 index = False)
