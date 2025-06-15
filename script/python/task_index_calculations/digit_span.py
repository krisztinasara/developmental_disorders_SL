# -*- coding: utf-8 -*-
"""
Created on Thu Jun 11 08:10:19 2020

@author: Kriszti
"""

# =============================================================================
# Processing of outputs of the Digit Span task
# Output: forward and backward digit span scores
# =============================================================================

import pandas as pd
from datetime import datetime
import glob, random

# =============================================================================
# Define functions
# =============================================================================

def response_recode(response):
    resp_items = response[2:-2].split('","')
    index = 0
    while index < len(resp_items):
        if resp_items[index] == 'backspace':
            del resp_items[index]
            if index > 0:
                del resp_items[index - 1]
            index = 0
        else: index = index + 1
    resp_items = '-'.join(resp_items)
    return(resp_items)

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

op_path = input("Path of output files (e.g., experiments/data/*.csv): ")
digit_span = pd.DataFrame()

i = 0
for file in glob.glob(op_path):
    actual = pd.read_csv(file, encoding = "utf-8-sig")
    random_ID = random.randint(1111111111,9999999999)
    ID = str(actual['Azonosító'].unique()[0])
    birth_date = uniform_date(actual['Születési dátum'].unique()[0])
    actual_forw = actual[actual['Irany'] == "forward"]
    f_length = 2
    for length in actual_forw['Hossz'].unique():
        f = 0
        forw = actual_forw[actual_forw['Hossz'] == length]
        for index, row in forw.iterrows():
            try:
                if response_recode(forw.at[index, 'key_resp.keys']) == forw.at[index, 'CorrAns']:
                    f = f + 1
            except: pass
        if f < 2: break
        f_length = length
    actual_back = actual[actual['Irany'] == "backward"]
    b_length = 2
    for length in actual_back['Hossz'].unique():
        b = 0
        back = actual_back[actual_back['Hossz'] == length]
        for index, row in back.iterrows():
            try:
                if response_recode(back.at[index, 'key_resp.keys']) == back.at[index, 'CorrAns']:
                    b = b + 1
            except: pass
        if b < 2: break
        b_length = length
    digit_span.at[i, 'random_ID'] = random_ID
    digit_span.at[i, 'ID'] = ID
    digit_span.at[i, 'birth_date'] = birth_date
    digit_span.at[i, 'forward_span'] = f_length
    digit_span.at[i, 'backward_span'] = b_length
    del f_length, b_length
    i = i + 1

#del forw, back, f, b, i, index, row, ID, actual, actual_back, actual_forw, b_length, f_length, length

r_path = input("Path of result files: ")
digit_span.to_csv(r_path + '/DigitSpan_' + datetime.today().strftime('%Y%m%d') + '.csv',
                  index = False)
