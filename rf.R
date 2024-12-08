# rf on data

# -- head -- #

setwd('~/Github/developmental_disorders_SL/')
library(tidyverse)
library(naniar)
library(h2o)

# -- read -- #

d = read_csv('data/df.csv')

# -- rummage -- #

d2 = d |> 
  mutate(value = 1) |> 
  pivot_wider(names_from = group, values_from = value, values_fill = 0) |>
  select(-TD)

names(d2)
# outcome variables?
# sent rep, expr vocab
# predictor variables?
# AGL* 
# digit_span*
# n_back
# PS*
# IQ
# group
# age

#vis_miss(d)
#miss_var_summary(d)
#miss_case_summary(d)

# Specify the response and predictor variables
response1 = "sent_rep"
response2 = "expr_vocab"
predictors = c("AGL_offline", "AGL_medRT_diff", "IQ", "DLD", "ADHD", "ASD", 
               "digit_span_forward", "digit_span_backward", 
               "PS_vis_RT_med", "PS_ac_RT_med", "n_back_2_mean_score")

#d |> na.omit() |> nrow() # hm

# -- model -- #

h2o.init(max_mem_size = "16g")

# Convert the dataframe to an H2O frame
d_h2o = as.h2o(d2) # d2!

# Train the Lasso regression model
# fit1 = h2o.glm(
#   x = predictors,
#   y = response1,
#   training_frame = d_h2o,
#   family = "gaussian",
#   alpha = 1,  # alpha = 1 for Lasso regression
#   lambda_search = TRUE  # Enable lambda search to find the best lambda
# )
# only 71 complete observations
# needs tweaking, it shrinks all to 0

## first go ##

fit0 = h2o.randomForest(
  x = predictors,
  y = response1,
  training_frame = d_h2o,
  ntrees = 100,          # Number of trees
  max_depth = 20,        # Maximum depth of each tree
  min_rows = 1,          # Minimum number of rows to split an internal node
  seed = 1234            # Seed for reproducibility
)

print(fit0)
h2o.varimp(fit0)

## hyperparameter search grid ##

# set hyperparameters for tuning a rf
hyper_params = list(
  ntrees = c(50, 100, 150),
  mtries = c(3,4),
  max_depth = c(10,20,30),
  min_rows = c(1,3),
  nbins = c(20,30),
  sample_rate = c(0.55, 0.632, 0.75)
  )

## resp 1 ##

# Run the grid search
h2o.grid(algorithm = "randomForest",
         grid_id = "rf_grid_1",
         x = predictors, 
         y = response1,
         seed = 29, 
         nfolds = 10, 
         training_frame = d_h2o,
         hyper_params = hyper_params,
         search_criteria = list(strategy = "Cartesian"))

# Get the grid search results, sorted by rmse
grid_results1 = h2o.getGrid(grid_id = "rf_grid_1", sort_by = "rmse", decreasing = FALSE)

# Print the grid search results
print(grid_results1)

# Get the best model from the grid search
best_model1 = h2o.getModel(grid_results1@model_ids[[1]])
print(best_model1)
h2o.saveModel(object = best_model1, path = 'models')
h2o.varimp(best_model1)

## resp 2 ##

# Run the grid search
h2o.grid(algorithm = "randomForest",
         grid_id = "rf_grid_2",
         x = predictors, 
         y = response2,
         seed = 29, 
         nfolds = 10, 
         training_frame = d_h2o,
         hyper_params = hyper_params,
         search_criteria = list(strategy = "Cartesian"))

# Get the grid search results, sorted by rmse
grid_results2 = h2o.getGrid(grid_id = "rf_grid_2", sort_by = "rmse", decreasing = FALSE)

# Print the grid search results
print(grid_results2)

# Get the best model from the grid search
best_model2 = h2o.getModel(grid_results2@model_ids[[1]])
print(best_model2)
h2o.saveModel(object = best_model2, path = 'models')

## add pred ##

predict_1 = as.data.frame(h2o.predict(best_model1, newdata = d_h2o))
predict_2 = as.data.frame(h2o.predict(best_model2, newdata = d_h2o))

# response1 = "sent_rep"
# response2 = "expr_vocab"

d = d |> 
  mutate(
    sent_rep_pred_rf = predict_1$predict,
    expr_vocab_pred_rf = predict_2$predict
  )

## checks ##

h2o.performance(best_model1)
h2o.performance(best_model2)
varimp1 = as_tibble(h2o.varimp(best_model1))
varimp2 = as_tibble(h2o.varimp(best_model2))

# tree1 = h2o.getModelTree(best_model1, 1)

h2o.shutdown(prompt = FALSE)

## save ##

write_tsv(varimp1, 'varimp1.tsv')
write_tsv(varimp2, 'varimp2.tsv')
write_tsv(d, 'data/df_pred.tsv')
