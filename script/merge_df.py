# -*- coding: utf-8 -*-
"""
Created on Mon Sep 16 19:18:45 2024

@author: krisztinasara
"""

import pandas as pd

cols = ['ID', 'birth_date', 'test_date', 'sex', 'IQ']

TD = pd.read_excel("C:/Users/krisztinasara/github/developmental_disorders_SL/data/dem/participants_master.xlsx", sheet_name = "TD")[cols]
DLD = pd.read_excel("C:/Users/krisztinasara/github/developmental_disorders_SL/data/dem/participants_master.xlsx", sheet_name = "DLD")[cols]
ADHD = pd.read_excel("C:/Users/krisztinasara/github/developmental_disorders_SL/data/dem/participants_master.xlsx", sheet_name = "ADHD")[cols]
ASD = pd.read_excel("C:/Users/krisztinasara/github/developmental_disorders_SL/data/dem/participants_master.xlsx", sheet_name = "ASD")[cols]

TD['group'] = "TD"
DLD['group'] = "DLD"
ADHD['group'] = "ADHD"
ASD['group'] = "ASD"

dem = pd.concat([TD, DLD, ADHD, ASD])

segm1 = pd.read_csv("C:/Users/krisztinasara/github/developmental_disorders_SL/data/tasks/normRTs_SEGM_AL_children_wide_20220215_FINAL_SAMPLE_dprime_filtered.csv")
segm2 = pd.read_csv("C:/Users/krisztinasara/github/developmental_disorders_SL/data/tasks/SEGM_AL_children_450ms_2keys_wide_20240912.csv")
agl = pd.read_csv("C:/Users/krisztinasara/github/developmental_disorders_SL/data/tasks/AGL_KS_mod4_wide_AGL_KS_20240912.csv")
nad = pd.read_csv("C:/Users/krisztinasara/github/developmental_disorders_SL/data/tasks/AGL_NAD_v2_letterID_wide_20231004.csv")
ps1 = pd.read_csv("C:/Users/krisztinasara/github/developmental_disorders_SL/data/tasks/PROC_SPEED_v2_20240913.csv")
ps2 = pd.read_csv("C:/Users/krisztinasara/github/developmental_disorders_SL/data/tasks/PROC_SPEED_YA_20231026_FINAL_SAMPLE.csv")
ds = pd.read_csv("C:/Users/krisztinasara/github/developmental_disorders_SL/data/tasks/DigitSpan_20240912.csv")
nback = pd.read_csv("C:/Users/krisztinasara/github/developmental_disorders_SL/data/tasks/Verbal_n_back_wide_20240913.csv")
stroop = pd.read_csv("C:/Users/krisztinasara/github/developmental_disorders_SL/data/tasks/stroop_wide_20240913.csv")
simon = pd.read_csv("C:/Users/krisztinasara/github/developmental_disorders_SL/data/tasks/Simon_nyilas_wide_20240913.csv")

segm = pd.concat([segm1, segm2])
ps = pd.concat([ps1, ps2])

dem['ID'] = dem['ID'].astype(str)
segm['ID'] = segm['ID'].astype(str)
agl['ID'] = agl['ID'].astype(str)
nad['ID'] = nad['ID'].astype(str)
ps['ID'] = ps['ID'].astype(str)
ds['ID'] = ds['ID'].astype(str)
nback['ID'] = nback['ID'].astype(str)
stroop['ID'] = stroop['ID'].astype(str)
simon['ID'] = simon['ID'].astype(str)

stroop = stroop.rename(columns = {'RT_score' : 'stroop_RT', 'ACC_score' : 'stroop_ACC'})
simon = simon.rename(columns = {'RT_score' : 'simon_RT', 'ACC_score' : 'simon_ACC'})

df = dem.merge(
     segm[[
         'ID',
         'medRT_TRN3_RND4',
         'medRT_RND4_REC5',
         '2AFC_n_p_bigram',
         '2AFC_n_p_trigram',
         '2AFC_n_w_bigram',
         '2AFC_n_w_trigram',
         '2AFC_p_n_bigram',
         '2AFC_p_n_trigram',
         '2AFC_p_w_bigram',
         '2AFC_p_w_trigram',
         '2AFC_w_n_bigram',
         '2AFC_w_n_trigram',
         '2AFC_w_p_bigram',
         '2AFC_w_p_trigram',
         'SEGM_prod_data'
         ]], on = 'ID', how = "left"
     )
df = df.merge(
     agl[[
         'ID',
         'medRT_block_4_3',
         'medRT_block_4_5',
         '2AFC_phr_freq',
         '2AFC_phr_0TP',
         '2AFC_sent_ch',
         '2AFC_sent_repl',
         'prod'
         ]], on = 'ID', how = "left"
    )
df = df.merge(
    nad[[
        'ID',
        'medRT_TRN4',
        'medRT_RND5',
        'medRT_REC6',
        'twoAFC_dep',
        'twoAFC_pos',
        'prod_ACC'
        ]], on = 'ID', how = "left"
    )
df = df.merge(
    ps[[
        'ID',
        'vis_RT_med',
        'ac_RT_med',
        'vis_dec_RT_med',
        'ac_dec_RT_med'
        ]], on = 'ID', how = "left"
    )
df = df.merge(
    ds[[
        'ID',
        'forward_span',
        'backward_span'
        ]], on = 'ID', how = "left"
    )
df = df.merge(
    nback[[
        'ID',
        'nback_1_dprime',
        'nback_2_dprime',
        'nback_3_dprime'
        ]], on = 'ID', how = "left"
    )
df = df.merge(
    stroop, on = 'ID', how = "left"
    )
df = df.merge(
    simon, on = 'ID', how = "left"
    )

df.to_csv("C:/Users/krisztinasara/github/developmental_disorders_SL/data/df.csv", index = False)
