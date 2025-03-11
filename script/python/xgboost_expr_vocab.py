###########
# setup
###########

import pandas as pd
import numpy as np
from xgboost import XGBRegressor 
from sklearn.metrics import mean_squared_error
from sklearn.model_selection import GridSearchCV
import json
import os

# load data
data = pd.read_csv('~/Github/developmental_disorders_SL/data/df.csv')

# list unique values of data col group
data.group.unique()

data.dtypes

# dummy code group, drop "TD"
data = pd.get_dummies(data, columns=['group'])
data = data.drop(columns=['group_TD'])

# drop id col
data = data.drop(columns=['ID','age_years','IQ','prod','AFC_phr','AFC_sent'])

# set up XGBoost regressor (using 'reg:squarederror' for regression)
xgbr = XGBRegressor(objective='reg:squarederror', missing=np.nan)

# o3 mini suggestions for small correlated dataset
param_grid = {
    'n_estimators': [50, 100],           # fewer boosting rounds to avoid overfitting on small data
    'learning_rate': [0.01, 0.05, 0.1],    # smaller learning rates for stable convergence
    'max_depth': [2, 3],                 # shallow trees help reduce complexity and overfitting
    'subsample': [0.8, 1.0],             # use most or all data per tree to retain signal
    'colsample_bytree': [0.2, 0.5, 0.8, 1.0],      # limit feature sampling to lessen the impact of correlated predictors
    'min_child_weight': [1, 3, 5]        # higher values require more instances per leaf, helping regularization
}

# mse: a classic scoring metric!
scoring_metric = 'neg_mean_squared_error'

###########
# sent rep
###########

# drop sent_rep col
edata = data.drop(columns=['sent_rep'])

# find rows where expr_vocab is na
edata[edata['expr_vocab'].isna()]

# drop rows where expr_vocab is missing
edata = edata.dropna(subset=['expr_vocab'])

# sy is "expr_vocab"
expr_vocab_target = edata['expr_vocab']
# sX is rest of edata
expr_vocab_features = edata.drop(columns=['expr_vocab'])

# set up grid search for sent rep using XGBRegressor
grid_search_expr_vocab = GridSearchCV(
    estimator=xgbr,
    param_grid=param_grid,
    cv=10,
    n_jobs=-1,
    verbose=2,
    scoring=scoring_metric
)

# fit grid search to training data
grid_search_expr_vocab.fit(expr_vocab_features, expr_vocab_target)

# best hyperparameters
best_params_expr_vocab = grid_search_expr_vocab.best_params_

# best model
best_grid_expr_vocab = grid_search_expr_vocab.best_estimator_

# best score (note: best_score_ will be negative mse, so higher is better)
best_score = grid_search_expr_vocab.best_score_

# get r2 for best model

best_grid_expr_vocab.score(expr_vocab_features, expr_vocab_target)

# predict best model on sent rep data
expr_vocab_pred = best_grid_expr_vocab.predict(expr_vocab_features)

# save best params and score
results_expr_vocab = {
    'best_params': best_params_expr_vocab,
    'neg_mean_squared_error': best_score
}

results_path = os.path.expanduser('~/Github/developmental_disorders_SL/data/results_expr_vocab.json')

with open(results_path, 'w') as f:
    json.dump(results_expr_vocab, f, indent=4)

# Feature importances
feature_importances_expr_vocab = best_grid_expr_vocab.feature_importances_
feature_names_expr_vocab = expr_vocab_features.columns

# Combine feature names and importances into a DataFrame
feature_importance_expr_vocab_df = pd.DataFrame({
    'Feature': feature_names_expr_vocab,
    'Importance': feature_importances_expr_vocab
})
feature_importance_expr_vocab_df = feature_importance_expr_vocab_df.sort_values(by='Importance', ascending=False)

# save feature importances
feature_importance_expr_vocab_df.to_csv('~/Github/developmental_disorders_SL/data/feature_importances_expr_vocab.csv', index=False)

# add predictions to data and save it as data frame

edata['expr_vocab_pred'] = expr_vocab_pred
edata.to_csv('~/Github/developmental_disorders_SL/data/predictions_expr_vocab.csv', index=False)