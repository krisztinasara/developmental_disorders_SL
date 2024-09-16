# -*- coding: utf-8 -*-
"""
Created on Mon Sep 16 19:18:45 2024

@author: krisztinasara
"""

import pandas as pd

cols = ['ID', 'birth_date', 'test_date', 'sex', 'IQ']

TD = pd.read_excel("C:/Users/krisztinasara/github/developmental_disorders_SL/participants_master.xlsx", sheet_name = "TD")[cols]
DLD = pd.read_excel("C:/Users/krisztinasara/github/developmental_disorders_SL/participants_master.xlsx", sheet_name = "DLD")[cols]
ADHD = pd.read_excel("C:/Users/krisztinasara/github/developmental_disorders_SL/participants_master.xlsx", sheet_name = "ADHD")[cols]
ASD = pd.read_excel("C:/Users/krisztinasara/github/developmental_disorders_SL/participants_master.xlsx", sheet_name = "ASD")[cols]

TD['group'] = "TD"
DLD['group'] = "DLD"
ADHD['group'] = "ADHD"
ASD['group'] = "ASD"

dem = pd.concat([TD, DLD, ADHD, ASD])

