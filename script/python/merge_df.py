# -*- coding: utf-8 -*-
"""
Created on Mon Sep 16 19:18:45 2024

@author: krisztinasara
"""

import pandas as pd

source = pd.concat(
    [
     pd.read_excel("C:/Users/krisztinasara/github/developmental_disorders_SL/data/dem/participants_master.xlsx", sheet_name = "TD"),
     pd.read_excel("C:/Users/krisztinasara/github/developmental_disorders_SL/data/dem/participants_master.xlsx", sheet_name = "DLD"),
     pd.read_excel("C:/Users/krisztinasara/github/developmental_disorders_SL/data/dem/participants_master.xlsx", sheet_name = "ADHD"),
     pd.read_excel("C:/Users/krisztinasara/github/developmental_disorders_SL/data/dem/participants_master.xlsx", sheet_name = "ASD")
     ]
    )

dem = pd.read_excel("C:/Users/krisztinasara/github/developmental_disorders_SL/data/dem/participants_master.xlsx", sheet_name = "matched_sample")

agl = pd.read_csv("C:/Users/krisztinasara/github/developmental_disorders_SL/data/tasks/AGL_KS_mod4_wide_AGL_KS_20240912.csv")
ps1 = pd.read_csv("C:/Users/krisztinasara/github/developmental_disorders_SL/data/tasks/PROC_SPEED_v2_20240913.csv")
ps2 = pd.read_csv("C:/Users/krisztinasara/github/developmental_disorders_SL/data/tasks/PROC_SPEED_YA_20231026_FINAL_SAMPLE.csv")
ds = pd.read_csv("C:/Users/krisztinasara/github/developmental_disorders_SL/data/tasks/DigitSpan_20240912.csv")
nback = pd.read_csv("C:/Users/krisztinasara/github/developmental_disorders_SL/data/tasks/Verbal_n_back_wide_20240913.csv")
stroop = pd.read_csv("C:/Users/krisztinasara/github/developmental_disorders_SL/data/tasks/stroop_wide_20240913.csv")
simon = pd.read_csv("C:/Users/krisztinasara/github/developmental_disorders_SL/data/tasks/Simon_nyilas_wide_20240913.csv")

ps = pd.concat([ps1, ps2])

dem['ID'] = dem['ID'].astype(str)
source['ID'] = source['ID'].astype(str)
agl['ID'] = agl['ID'].astype(str)
ps['ID'] = ps['ID'].astype(str)
ds['ID'] = ds['ID'].astype(str)
nback['ID'] = nback['ID'].astype(str)
stroop['ID'] = stroop['ID'].astype(str)
simon['ID'] = simon['ID'].astype(str)

agl = agl.rename(
    columns = {
        'medRT_train' : 'AGL_medRT_train',
        'medRT_diff' : 'AGL_medRT_diff',
        '2AFC_phr' : 'AGL_2AFC_phr',
        '2AFC_sent' : 'AGL_2AFC_sent',
        'prod' : 'AGL_prod'
        }
    )
ps = ps.rename(
    columns = {
        'vis_RT_med' : 'PS_vis_RT_med',
        'ac_RT_med' : 'PS_ac_RT_med',
        'vis_dec_RT_med' : 'PS_vis_dec_RT_med',
        'ac_dec_RT_med' : 'PS_ac_dec_RT_med'
        }
    )
stroop = stroop.rename(columns = {'RT_score' : 'stroop_RT'})
simon = simon.rename(columns = {'RT_score' : 'simon_RT'})

df = dem.merge(
    source[['ID', 'sex']], on = 'ID', how = "left"
    )
df = df.merge(
     agl[[
         'ID',
         'AGL_medRT_train',
         'AGL_medRT_diff',
         'AGL_2AFC_phr',
         'AGL_2AFC_sent',
         'AGL_prod'
         ]], on = 'ID', how = "left"
    )
df = df.merge(
    ps[[
        'ID',
        'PS_vis_RT_med',
        'PS_ac_RT_med',
        'PS_vis_dec_RT_med',
        'PS_ac_dec_RT_med'
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
    stroop[['ID', 'stroop_RT']], on = 'ID', how = "left"
    )
df = df.merge(
    simon[['ID', 'simon_RT']], on = 'ID', how = "left"
    )

df.to_csv("C:/Users/krisztinasara/github/developmental_disorders_SL/data/df.csv", index = False)
