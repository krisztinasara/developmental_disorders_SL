###########
# setup
###########

import pandas as pd
import numpy as np
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import GridSearchCV, train_test_split
from sklearn.metrics import r2_score, mean_squared_error, make_scorer
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
data = data.drop(columns=['ID'])

# set up random forest regressor
rf = RandomForestRegressor()

# set up grid of hyperparameters (suggestions for few data)
param_grid = {
    'n_estimators': [10, 25, 50, 100, 500],
    'max_depth': [None, 1, 5, 10, 15],
    'min_samples_split': [2, 5, 10],
    'min_samples_leaf': [1, 2, 4, 10],
    'max_features': [1, 2, 4],
    'max_samples': [0.5, 0.75, 1.0]  # Fraction of samples to draw
}

# set up scorer
r2_scorer = make_scorer(r2_score)

###########
# sent rep
###########

# drop expr_vocab col
sdata = data.drop(columns=['expr_vocab'])

# drop rows where sent_rep is missing
sdata = sdata.dropna(subset=['sent_rep'])

# sy is "sent_rep"
sent_rep_target = sdata['sent_rep']
# sX is rest of sdata
sent_rep_features = sdata.drop(columns=['sent_rep'])

# split sdata into training and test sets
sX_train, sX_test, sy_train, sy_test = train_test_split(sent_rep_features, sent_rep_target, test_size=0.2)

# set up grid search for sent rep
grid_search_sent_rep = GridSearchCV(estimator=rf, param_grid=param_grid, cv=10, n_jobs=-1, verbose=2, scoring=r2_scorer)

# fit grid search to training data
grid_search_sent_rep.fit(sX_train, sy_train)

# best hyperparameters
best_params_sent_rep = grid_search_sent_rep.best_params_

# best model
best_grid_sent_rep = grid_search_sent_rep.best_estimator_

# best model r2
r2_sent_rep = best_grid_sent_rep.score(sX_test, sy_test)

# predict on test set
sy_pred = best_grid_sent_rep.predict(sX_test)

# mse
mse_sent_rep = mean_squared_error(sy_test, sy_pred)

# save best params, r2 and mse
results_sent_rep = {
    'best_params': best_params_sent_rep,
    'r2': r2_sent_rep,
    'mse': mse_sent_rep
}

results_path = os.path.expanduser('~/Github/developmental_disorders_SL/data/results_sent_rep.json')

with open(results_path, 'w') as f:
    json.dump(results_sent_rep, f, indent=4)

# Feature importances
feature_importances_sent_rep = best_grid_sent_rep.feature_importances_
feature_names_sent_rep = sent_rep_features.columns

# Combine feature names and importances into a DataFrame
feature_importance_sent_rep_df = pd.DataFrame({'Feature': feature_names_sent_rep, 'Importance': feature_importances_sent_rep})
feature_importance_sent_rep_df = feature_importance_sent_rep_df.sort_values(by='Importance', ascending=False)

# save feature importances
feature_importance_sent_rep_df.to_csv('~/Github/developmental_disorders_SL/data/feature_importances_sent_rep.csv', index=False)

###########
# expr vocab
###########

# drop sent_rep col
edata = data.drop(columns=['sent_rep'])

# drop rows where expr_vocab is missing
edata = edata.dropna(subset=['expr_vocab'])

# ey is "expr_vocab"
expr_vocab_target = edata['expr_vocab']
# eX is rest of edata
expr_vocab_features = edata.drop(columns=['expr_vocab'])

# split sdata into training and test sets
eX_train, eX_test, ey_train, ey_test = train_test_split(expr_vocab_features, expr_vocab_target, test_size=0.2)

# set up grid search for expr vocab
grid_search_expr_vocab = GridSearchCV(estimator=rf, param_grid=param_grid, cv=10, n_jobs=-1, verbose=2, scoring=r2_scorer)

# fit grid search to training data
grid_search_expr_vocab.fit(eX_train, ey_train)

# best hyperparameters
best_params_expr_vocab = grid_search_expr_vocab.best_params_

# best model
best_grid_expr_vocab = grid_search_expr_vocab.best_estimator_

# best model r2
r2_expr_vocab = best_grid_expr_vocab.score(eX_test, ey_test)

# predict on test set
ey_pred = best_grid_expr_vocab.predict(eX_test)

# mse
mse_expr_vocab = mean_squared_error(ey_test, ey_pred)

# save best params, r2 and mse
results_expr_vocab = {
    'best_params': best_params_expr_vocab,
    'r2': r2_expr_vocab,
    'mse': mse_expr_vocab
}

results_path_expr_vocab = os.path.expanduser('~/Github/developmental_disorders_SL/data/results_expr_vocab.json')

with open(results_path_expr_vocab, 'w') as f:
    json.dump(results_expr_vocab, f, indent=4)


# Feature importances
feature_importances_expr_vocab = best_grid_expr_vocab.feature_importances_
feature_names_expr_vocab = expr_vocab_features.columns

# Combine feature names and importances into a DataFrame
feature_importance_expr_vocab_df = pd.DataFrame({'Feature': feature_names_expr_vocab, 'Importance': feature_importances_expr_vocab})
feature_importance_expr_vocab_df = feature_importance_expr_vocab_df.sort_values(by='Importance', ascending=False)

# save feature importances
feature_importance_expr_vocab_df.to_csv('~/Github/developmental_disorders_SL/data/feature_importances_expr_vocab.csv', index=False)